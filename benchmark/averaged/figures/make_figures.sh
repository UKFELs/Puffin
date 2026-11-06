#!/usr/bin/env bash
# Regenerate the averaged-vs-unaveraged lattice figures in this directory.
#
#   bash make_figures.sh RUNS [python]
#
# RUNS holds one directory per case, each with the run directories that
# run_compare_latt.py wrote (its "Runs:" line, at the foot of each figure),
# copied or linked as
#   RUNS/A_detuned_f0.9/{unavg,avg}
#   RUNS/B_ownres_f0.75/{unavg,avg,avg_prefix}   avg_prefix: averaged, built before 93847a8
#   RUNS/C_taper_-0.05/{unavg,avg}
#   RUNS/3d_A_detuned_f0.9/{unavg,avg}
#   RUNS/3d_B_ownres_f0.75/{unavg,avg}
#   RUNS/3d_C_taper_-0.05/{unavg,avg}
# The legend's solver times come from each run's stdout.txt.  The 1D runs were
# timed one at a time on 1 rank; the 3D ones ran alongside
# each other (see the 3D captions).
set -euo pipefail
RUNS=${1:?usage: make_figures.sh RUNS [python]}
PY=${2:-python3}
HERE=$(cd "$(dirname "$0")" && pwd)
PLOT="$PY $HERE/../plot_compare_latt.py"

COMMON="Common to all three: 1D (no diffraction), planar undulator, aw = 1.012, λw = 2.75 cm, γr = 456.4, ρ = 0.005, λr = 100 nm; flat-top beam 50 z̄2 long, σγ/γ = 0.001; noise-free, seeded. Averaged runs use 2 steps per period. Blue solid: unaveraged. Orange dashed: averaged. Legend times are Puffin's own solver time (start-up excluded), one run at a time on 1 MPI rank of an 18-core Apple M5 Pro carrying light other load, so treat the speed-ups as ±30%."

$PLOT "$HERE/latt_A_detuned_f0.9.png" --unavg "$RUNS/A_detuned_f0.9/unavg" --avg "$RUNS/A_detuned_f0.9/avg" --fold 1 \
  --modules "0:60:module 1, α = 1;62:122:module 2, resonant at 0.9 ωr" \
  --title "A. Detuned second module acting on carrier radiation (seeded at the carrier only)" \
  --scenario "Lattice: 60 periods at α = 1, a 2-period drift, then 60 periods at α = 1.152, which makes a reference electron resonant at 0.9 ωr. Seed: flat-top at ωr only, |A| = 0.01, x-polarised; nothing is seeded at 0.9 ωr. Module 2 therefore drives the existing carrier field 10% off its own resonance, which is where the averaged mesh holds content. Averaged at 1 λr/cell against unaveraged at 96 nodes/λr, 240 steps/period. Ends at z = 3.36 m (z̄ = 7.67).

$COMMON" \
  --command 'python3 run_compare_latt.py planepole --latt "UN 60 1.0 0.0; DR 2; UN 60 f0.9 0.0" --plain 96:240 --avg 1:2'

$PLOT "$HERE/latt_B_ownres_f0.75.png" --unavg "$RUNS/B_ownres_f0.75/unavg" --avg "$RUNS/B_ownres_f0.75/avg" \
  --before "$RUNS/B_ownres_f0.75/avg_prefix" --fold 0.75 --specx=-1,1.3 \
  --modules "0:60:module resonant at 0.75 ωr" \
  --title "B. Radiation at a detuned module's own resonance, 25% below the carrier (the Bessel-factor fix)" \
  --scenario "Lattice: one 60-period module at α = 1.409, which makes a reference electron resonant at 0.75 ωr. Seed: flat-top at 0.75 ωr only, |A| = 0.01, x-polarised; nothing at ωr. The whole interaction is 25% off the averaged mode's carrier: this tests the band, and is the case the PR's Bessel-factor fix corrects (grey dotted: the same averaged run before the fix). Averaged at 0.125 λr/cell against unaveraged at 48 nodes/λr, 120 steps/period. Ends at z = 1.65 m (z̄ = 3.77).

Caveat: the averaged spectrum also holds a mirror image of the 0.75 line at −0.75 ωr, ~9.5% of the field energy, which no real field has. It is counted in the averaged power and energy curves, and is why the averaged power ends above the unaveraged. Power in the 0.75 colour alone is 0.967 of unaveraged (0.758 before the fix), the figure the PR's tables report.

$COMMON" \
  --command 'python3 run_compare_latt.py planepole --latt "UN 60 f0.75 0.0" --seed 0,0 --seed2 0.75:0.01,0.0 --z2len 72 --plain 48:120 --avg 0.125:2'

$PLOT "$HERE/latt_C_taper_-0.05.png" --unavg "$RUNS/C_taper_-0.05/unavg" --avg "$RUNS/C_taper_-0.05/avg" --fold 1 \
  --modules "0:105:module 1, α = 1;105:255:module 2, tapered α 1 → 0.53" \
  --title "C. Very strong taper after saturation: power is right, the bunching of detrapped electrons is not" \
  --scenario "Lattice: 105 periods at α = 1, taking the beam to saturation at z̄ ≈ 6.6, then 150 periods tapered at dα/dz̄ = −0.05, so α falls from 1 to 0.53. Seed: flat-top at ωr, |A| = 0.01, x-polarised. The taper is far too strong to hold the beam: every electron is detrapped and slips ~1.5 rad of ponderomotive phase per period, so the field stops growing while the bunching keeps evolving. Averaged at 1 λr/cell against unaveraged at 96 nodes/λr, 240 steps/period. Ends at z = 7.01 m (z̄ = 16.0).

$COMMON" \
  --command 'python3 run_compare_latt.py planepole --latt "UN 105 1.0 0.0; UN 150 1.0 -0.05" --z2len 80 --plain 96:240 --avg 1:2'

COMMON3D="Common to all three 3D cases: the 3D deck (deck3d.in): diffraction on, planar undulator, aw = 1.012, λw = 2.75 cm, γr = 456.4, ρ = 0.005, λr = 100 nm, Ipk = 4 kA (ten times the 1D deck's, to reach saturation in a few metres), beam matched to the first module's natural focusing, which for a planar undulator is in y only, so the beam slowly grows in x. Flat-top beam about three slippage lengths long, so the middle 30% is in steady state as in 1D; σγ/γ = 0.001; noise-free, seeded at |A| = 0.001 with the seed matched to the beam. Transverse mesh 41 × 41. The comparison is like-for-like: both solvers get the same beam, seed, mesh and lattice. Legend times are Puffin's solver time on the same number of MPI ranks for both solvers; the 3D runs shared an 18-core Apple M5 Pro with each other, so treat the speed-ups as ±30%."

$PLOT "$HERE/latt3d_A_detuned_f0.9.png" --unavg "$RUNS/3d_A_detuned_f0.9/unavg" --avg "$RUNS/3d_A_detuned_f0.9/avg" --fold 1 \
  --modules "0:60:module 1, α = 1;62:122:module 2, resonant at 0.9 ωr" \
  --title "3D A. Detuned second module acting on carrier radiation, with diffraction" \
  --scenario "The 3D counterpart of 1D case A. Lattice: 60 periods at α = 1, a 2-period drift, then 60 periods at α = 1.152, resonant at 0.9 ωr. Seed: flat-top at ωr only, x-polarised; nothing at 0.9 ωr. Beam 24 z̄2 long. Averaged at 1 λr/cell against unaveraged at 48 nodes/λr, 120 steps/period. Ends at z = 3.36 m.

$COMMON3D" \
  --command 'python3 run_compare_latt.py planepole --3d --np 4 --latt "UN 60 1.0 0.0; DR 2; UN 60 f0.9 0.0" --z2len 34 --blen 24 --plain 48:120 --avg 1:2'

$PLOT "$HERE/latt3d_B_ownres_f0.75.png" --unavg "$RUNS/3d_B_ownres_f0.75/unavg" --avg "$RUNS/3d_B_ownres_f0.75/avg" --fold 0.75 --specx=-1,1.3 \
  --modules "0:60:module resonant at 0.75 ωr" \
  --title "3D B. Radiation at a detuned module's own resonance, 25% below the carrier, with diffraction" \
  --scenario "The 3D counterpart of 1D case B. Lattice: one 60-period module at α = 1.409, resonant at 0.75 ωr (the beam is matched to α = 1, so it is slightly mismatched to this module, the same for both solvers). Seed: flat-top at 0.75 ωr only, x-polarised. Beam 15 z̄2 long. Averaged at 0.125 λr/cell, which a colour 25% off the carrier needs, against unaveraged at 48 nodes/λr, 120 steps/period: the averaged mesh is then 8 cells per wavelength, which is why the speed-up here is small. Ends at z = 1.65 m.

Caveat, as in 1D: the averaged spectrum holds a mirror image of the 0.75 line at −0.75 ωr, 8.7% of the field energy, which is counted in the averaged power and energy curves and is why they end ~10% above the unaveraged. Power in the 0.75 colour alone agrees to about 1%.

$COMMON3D" \
  --command 'python3 run_compare_latt.py planepole --3d --np 4 --latt "UN 60 f0.75 0.0" --seed 0,0 --seed2 0.75:0.001,0.0 --z2len 22 --blen 15 --plain 48:120 --avg 0.125:2'

$PLOT "$HERE/latt3d_C_taper_-0.05.png" --unavg "$RUNS/3d_C_taper_-0.05/unavg" --avg "$RUNS/3d_C_taper_-0.05/avg" --fold 1 \
  --modules "0:150:module 1, α = 1;150:250:module 2, tapered α 1 → 0.69" \
  --title "3D C. Strong taper after saturation, with diffraction" \
  --scenario "The 3D counterpart of 1D case C, at the same taper rate. In 3D the beam saturates later, so: 150 periods at α = 1, to saturation at z̄ ≈ 9.4, then 100 periods tapered at dα/dz̄ = −0.05, so α falls from 1 to 0.69. Seed: flat-top at ωr, x-polarised. Beam 47 z̄2 long; transverse mesh widened to 61 nodes over 1.5× the width in x, because without focusing in x the beam outgrows the standard mesh. Unlike 1D the taper first extracts more power (about 1.7×) before the beam detraps and the bunching collapses to ~0.01; at that level the end bunching ratio is noise. Averaged at 1 λr/cell against unaveraged at 24 nodes/λr, 60 steps/period (48 would have taken ~2 h on 8 ranks). Ends at z = 6.88 m.

$COMMON3D" \
  --command 'python3 run_compare_latt.py planepole --3d --np 8 --latt "UN 150 1.0 0.0; UN 100 1.0 -0.05" --z2len 65 --blen 47 --set sFModelLengthX=12.0 --set iNumNodesX=61 --set iRedNodesX=31 --plain 24:60 --avg 1:2'
