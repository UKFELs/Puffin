#!/usr/bin/env bash
# Run the scans behind the figures in this directory, and draw them.
#
#   bash make_figures.sh SCANS [python]
#
# SCANS is a scratch directory for the runs (a few GB) and their results.
# Every scan is started at once, one MPI rank each - 17 at a time, so give it
# a machine with that many cores, or expect it to take proportionally longer
# (about 25 min on an 18-core Apple M5 Pro, Debug build).  Rerunning with the
# results already in SCANS only redraws: delete SCANS/*.json to rerun a scan.
# PUFFIN picks the binary, as for run_scan.py.  The plotting python needs
# numpy, h5py and matplotlib; the scans themselves only the standard library.
set -euo pipefail
SCANS=$(mkdir -p "${1:?usage: make_figures.sh SCANS [python]}" && cd "$1" && pwd)
PY=${2:-python3}
HERE=$(cd "$(dirname "$0")" && pwd)
TOP=$(cd "$HERE/../../.." && pwd)
RS="$HERE/../run_scan.py"
export OMP_NUM_THREADS=1
STEPS1="10 12 14 16 20 24 30 40 60"

scan() {            # scan NAME [VAR=value ...] -- run_scan.py arguments
  local name=$1; shift
  local envs=()
  while [ "$1" != "--" ]; do envs+=("$1"); shift; done; shift
  [ -s "$SCANS/$name.json" ] && return
  env "${envs[@]}" SCAN_RUNS="$SCANS/runs/$name" SCAN_JSON="$SCANS/$name.json" \
    python3 "$RS" "$@" > "$SCANS/$name.log" 2>&1 &
}

for o in 1 2 3; do
  scan pp_n12_o$o SCAN_ORDER=$o -- planepole 240 $STEPS1
  scan hel_n12_o$o SCAN_ORDER=$o -- helical 240 $STEPS1
  scan pp_n24_o$o SCAN_ORDER=$o SCAN_NODES=24 -- planepole 240 12 16 20 24 30 40 60
  scan 3d_o$o SCAN_ORDER=$o SCAN_WRITE=1 \
    SCAN_DECK="$TOP/test/inputs/3D/clara_test.in" SCAN_INPUTS="$TOP/test" \
    -- planepole 240 12 16 20 24 30 40
done
# 3D with the z2 and x/y degrees differing
for c in 31 21 13; do
  scan 3d_o${c:0:1}xy${c:1:1} SCAN_ORDER=${c:0:1} SCAN_ORDER_XY=${c:1:1} SCAN_WRITE=1 \
    SCAN_DECK="$TOP/test/inputs/3D/clara_test.in" SCAN_INPUTS="$TOP/test" \
    -- planepole 240 12 16 20 24 30 40
done
# with a write every period, for the overlays
scan ov_lin SCAN_ORDER=1 SCAN_WRITE=1 -- planepole 240 12 30
scan ov_cub SCAN_ORDER=3 SCAN_WRITE=1 -- planepole 240 12 30
wait

DECK="1D (no diffraction), seeded, noise-free: benchmark/convergence/scan.in. Planar undulator unless stated, aw = 1.012, λw = 2.75 cm, γr = 456.4, ρ = 0.005, λr = 100 nm, 60 periods, no undulator ends; flat-top beam 50 z̄2 long, 400 A, σγ/γ = 0.001, 4 macroparticles per λr; flat-top seed at ωr, |A| = 0.01, x-polarised."
ERR="Error is the relative L2 difference of the whole final field, real and imaginary parts, from a reference run with the same kernel at 240 steps/period. The references themselves agree with 480-step ones to a few times 1e-7 (README, 'How the error is measured')."

$PY "$HERE/../plot_convergence.py" "$HERE/convergence.png" --scans "$SCANS" \
  --title "Step-size convergence by deposition kernel (iShapeOrder): linear, quadratic and cubic B-splines" \
  --scenario "$DECK 3D: the 3D e2e deck test/inputs/3D/clara_test.in - 85x85 transverse, 21 nodes/λr, a periodic single-wavelength slice, diffraction on, beam read from HDF5 so the realisation is fixed, 5 periods with undulator ends. Dashed in the 3D panel: the degree along z2 (iShapeOrder) and across x, y (iShapeOrderXY) set differently - cubic along z2 alone keeps nearly all of the full cubic kernel's gain.

$ERR The kernel can only remove the error from macroparticles crossing mesh nodes, so it is worth most where they cross many: in 3D at this mesh density, and on a planar undulator, whose figure-of-eight drags particles back and forth in z2. At the 1D default (12 nodes/λr) the quiver dominates the step error instead, and on a helical undulator, with no figure-of-eight, the kernel is worth nothing in 1D." \
  --command "bash benchmark/convergence/figures/make_figures.sh SCANS  (run_scan.py, 240-step references; steps $STEPS1)"

OV="$DECK Every panel compares a coarse step against the same kernel's 240 step/period reference, so the gap between the curves is step-size error and nothing else. Solver times are left out: these runs write every period and ran alongside the others; the README's timings are from a Release build, best of three."
R="$SCANS/runs"

$PY "$TOP/benchmark/averaged/plot_compare_latt.py" "$HERE/overlay_linear.png" \
  --unavg "$R/ov_lin/planepole_ref_o1" --avg "$R/ov_lin/planepole_30_o1" \
  --before "$R/ov_lin/planepole_12_o1" --no-times \
  --labels "linear, 240 steps/period (reference)|linear, 30 steps/period (shipped default)|linear, 12 steps/period" \
  --ratio "30 steps / 240 steps" --ratio-before "12 steps / 240 steps" --modules "0:60:undulator, 60 periods" \
  --title "Linear deposition - the shipped default - at 30 and 12 steps per period" \
  --scenario "$OV" \
  --command "SCAN_ORDER=1 SCAN_WRITE=1 python3 run_scan.py planepole 240 12 30"

$PY "$TOP/benchmark/averaged/plot_compare_latt.py" "$HERE/overlay_cubic.png" \
  --unavg "$R/ov_cub/planepole_ref_o3" --avg "$R/ov_cub/planepole_30_o3" \
  --before "$R/ov_cub/planepole_12_o3" --no-times \
  --labels "cubic, 240 steps/period (reference)|cubic, 30 steps/period|cubic, 12 steps/period" \
  --ratio "30 steps / 240 steps" --ratio-before "12 steps / 240 steps" --modules "0:60:undulator, 60 periods" \
  --title "Cubic deposition (iShapeOrder = 3) at 30 and 12 steps per period" \
  --scenario "$OV At this mesh density the step error is mostly the quiver's, not the deposition's, so the cubic kernel does little better than the linear one in the companion figure." \
  --command "SCAN_ORDER=3 SCAN_WRITE=1 python3 run_scan.py planepole 240 12 30"

$PY "$TOP/benchmark/averaged/plot_compare_latt.py" "$HERE/overlay_kernels.png" \
  --unavg "$R/ov_lin/planepole_ref_o1" --avg "$R/ov_cub/planepole_ref_o3" --no-times \
  --labels "linear, 240 steps/period|cubic, 240 steps/period" \
  --ratio "cubic / linear" --modules "0:60:undulator, 60 periods" \
  --title "What the smoother kernel changes at a converged step: linear against cubic, both at 240 steps per period" \
  --scenario "$DECK Both runs are converged in the step - each kernel's 60-step run is already within 3e-4 of its 240-step one, against a 5% difference in the field here - so the difference is the kernel's own: a B-spline filters the source in space, weakening the coupling at a fixed mesh by a form factor that falls as the square of the cell size. It is a different discretisation, not an error - README, section 5." \
  --command "SCAN_ORDER=1|3 SCAN_WRITE=1 python3 run_scan.py planepole 240 ..."
