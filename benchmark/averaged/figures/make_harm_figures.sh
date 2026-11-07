#!/usr/bin/env bash
# Regenerate the harmonic-band figures in this directory (harm_*.png).
#
#   bash make_harm_figures.sh RUNS [python]
#
# RUNS holds one directory per case, each with three run directories that
# run_compare_latt.py wrote (its commands are at the foot of each figure),
# copied or linked as RUNS/<case>/{unavg,avg,avg_nobands}: the unaveraged run,
# the averaged run with avgHarmonics = 3, 5, and the same averaged run without
# bands.  Cases:
#   planar, elliptical, planar_mpw4          the 60-period deck, no lattice
#   A_detuned_f0.9, B_ownres_f0.75, C_taper_-0.05             1D lattices
#   3d_A_detuned_f0.9, 3d_B_ownres_f0.75, 3d_C_taper_-0.05    3D lattices
# The lattice cases are #149's (make_figures.sh), at finer beam sampling.
# No solver times: these runs dump the field and beam every 5 periods and ran
# alongside each other, so the times say nothing about the solvers.
set -euo pipefail
RUNS=${1:?usage: make_harm_figures.sh RUNS [python]}
PY=${2:-python3}
HERE=$(cd "$(dirname "$0")" && pwd)

plot() {        # plot OUT CASE FOLD MODULES TITLE SCENARIO COMMAND [extra args]
  local out=$1 case=$2 fold=$3 mods=$4 title=$5 scen=$6 cmd=$7
  shift 7
  $PY "$HERE/../plot_compare_latt.py" "$HERE/$out" \
    --unavg "$RUNS/$case/unavg" --avg "$RUNS/$case/avg" --before "$RUNS/$case/avg_nobands" \
    --before-label "no harmonic bands" --harmonics 3,5 --no-times --fold "$fold" \
    --modules "$mods" --title "$title" --scenario "$scen" --command "$cmd" "$@"
}

BANDS="Blue solid: unaveraged. Orange dashed: averaged with avgHarmonics = 3, 5, so bands 1, 3 and 5. Grey dotted: the same averaged run without bands. Bottom row: the power in each harmonic, taken from the spectrum of the middle 30% of the field within half a carrier frequency of the harmonic, as a share of the whole spectrum times the mid-pulse power; an averaged band's share comes from its own envelope only. Early on the unaveraged harmonics sit on a numerical floor (~1e-11 of the fundamental in /power units) and the averaged ones start from zero, so compare the curves once they have grown out of it."
COMMON="1D (no diffraction), aw = 1.012, λw = 2.75 cm, γr = 456.4, ρ = 0.005, λr = 100 nm; flat-top beam 50 z̄2 long, σγ/γ = 0.001; noise-free, seeded; 16 macroparticles per wavelength (iMPsZ2PerWave), more than twice the 5th harmonic, so neither solver aliases the bands. Averaged runs use 2 steps per period. Solver times are left out: field and beam were written every 5 periods and the runs shared the machine."
COMMON4="${COMMON/16 macroparticles per wavelength (iMPsZ2PerWave), more than twice the 5th harmonic, so neither solver aliases the bands/4 macroparticles per wavelength (iMPsZ2PerWave), the default}"
COMMON3D="3D deck (deck3d.in) as #149's figures: diffraction, planar undulator, aw = 1.012, Ipk = 4 kA, beam matched to the first module, transverse mesh 41 × 41, seeded at |A| = 0.001, noise-free, but with 12 macroparticles per wavelength instead of the deck's 8, so neither solver aliases the 5th harmonic (8 aliases it onto the 3rd). The unaveraged field is written at the start and end only (a dump is ~0.7 GB), so the harmonic panels have one point each.

Before saturation (cases A and B) the 3D harmonic comparison is inconclusive, because the unaveraged 3D reference does not look physical: in A its spectrum (bottom right) has a 2nd harmonic at ~2e-3 of the field energy, 15x the 1D one, and 5th and 6th harmonics above the 3rd, which in 1D is 200x the 5th. A planar field does radiate even harmonics off axis, but this ordering suggests a numerical source in the unaveraged 3D field, for instance electrons' transverse wiggle across the deposition mesh. Until the unaveraged 3D harmonics are shown to converge, read those band panels as the averaged mode's prediction, not as a test of it. At saturation (case C) the unaveraged odd harmonics dominate as they should. Solver times are left out: the runs shared the machine."

plot harm_planar.png planar 1 "0:60:planar undulator" \
  "Harmonic bands 1. Planar, one undulator: the 3rd harmonic agrees, the 5th is 17% low" \
  "The 60-period seeded deck, planar undulator, seed |A| = 0.01 along x, against the unaveraged solver at 192 nodes/λr, 480 steps/period, which has converged for the 3rd harmonic and to a few per cent for the 5th (README, Harmonic bands). Averaged at 1 λr/cell. The bands add 0.09% to the total power and change the fundamental by -6e-6, so the orange and grey curves overlie each other in the top rows; the remaining ~1.7% gap in the fundamental is the planar remainder and the dropped A-term, not harmonic content. Even harmonics, which a planar field has off axis and which no band carries, show at 2 and 4 in the wide spectrum at ~1e-4 of the peak.

$BANDS

$COMMON" \
  'python3 run_compare_latt.py planepole --mpw 16 --fieldevery 5 --plain 24:60 48:120 96:240 192:480 --avg 1:2 1:2:3,5'

plot harm_elliptical.png elliptical 1 "0:60:elliptical undulator, (ux, uy) = (0.5, 1)" \
  "Harmonic bands 2. Elliptical undulator, (ux, uy) = (0.5, 1)" \
  "The 60-period seeded deck with the variably polarised undulator at ux = 0.5, uy = 1, seeded with equal x and y amplitudes, |A| = 0.01 each. Against the unaveraged solver at 48 nodes/λr, 120 steps/period; at the end the 3rd harmonic is 5% high and the 5th 17% low, against 3% high and 9% low for planar at the same mesh, so the gap on the 5th is larger here. The fundamental's y fraction is unchanged by the bands.

$BANDS

$COMMON" \
  'python3 run_compare_latt.py 0.5,1 --mpw 16 --fieldevery 5 --plain 48:120 --avg 1:2 1:2:3,5'

plot harm_planar_mpw4.png planar_mpw4 1 "0:60:planar undulator" \
  "Harmonic bands 3. Planar at 4 macroparticles per wavelength: both solvers alias the harmonics" \
  "As figure 1 but at the deck's default iMPsZ2PerWave = 4 and against 48 nodes/λr. With 4 macroparticles per wavelength the beam cannot represent bunching at the 3rd or 5th harmonic: harmonic h aliases onto h mod 4, so the bunching the 3rd and 5th harmonics see is the fundamental's, and their power is ~20x what it is at 16 per wavelength (3rd 1.7% of the fundamental against 0.09%). The averaged bands reproduce the aliased harmonics (the averaged run warns that its beam is too coarse for the bands) and, through them, the change they make to the fundamental: total power goes from 0.971 to 0.989 of unaveraged with the bands. This is an artefact of the sampling in both solvers; #149's lattice figures were made at this sampling.

$BANDS

$COMMON4" \
  'python3 run_compare_latt.py planepole --fieldevery 5 --plain 48:120 --avg 1:2 1:2:3,5'

plot harm_A_detuned_f0.9.png A_detuned_f0.9 1 "0:60:module 1, α = 1;62:122:module 2, resonant at 0.9 ωr" \
  "Harmonic bands 4. #149's case A: detuned second module, with bands" \
  "#149's case A (latt_A_detuned_f0.9.png) at 16 macroparticles per wavelength. Lattice: 60 periods at α = 1, a 2-period drift, then 60 periods resonant at 0.9 ωr; seeded at ωr only, |A| = 0.01 along x. Against the unaveraged solver at 96 nodes/λr, 240 steps/period. The harmonics come from module 1's bunching and sit at 3 and 5 ωr; module 2 adds little at its own 2.7 and 4.5. The 3rd harmonic ends 7% low and the 5th, at ~4e-6 of the field energy, 44% low, against 0% and 15% low for the same deck without the lattice at this mesh: the detuned module degrades the 5th harmonic's agreement, for a reason not traced.

$BANDS

$COMMON" \
  'python3 run_compare_latt.py planepole --mpw 16 --fieldevery 5 --latt "UN 60 1.0 0.0; DR 2; UN 60 f0.9 0.0" --plain 96:240 --avg 1:2 1:2:3,5'

plot harm_B_ownres_f0.75.png B_ownres_f0.75 0.75 "0:60:module resonant at 0.75 ωr" \
  "Harmonic bands 5. #149's case B: at 0.125 λr/cell the bands overlap, and count radiation twice" \
  "#149's case B (latt_B_ownres_f0.75.png) at 16 macroparticles per wavelength: one 60-period module resonant at 0.75 ωr, seeded there only, |A| = 0.01 along x. Averaged at 0.125 λr/cell against unaveraged at 48 nodes/λr, 120 steps/period. Harmonic panels at 2.25 and 3.75 ωr.

Not a valid use of the bands. At 0.125 λr/cell every band's envelope mesh holds ±4 ωr about its own harmonic, so the bands overlap: band 3 radiates at the 0.75 ωr colour and its mirror image at −0.75 (~2.9% of the field energy), band 5 an image near 8.75 ωr, and the fundamental band already carries the 2.25 ωr harmonic itself. Each band couples to all of it with its own harmonic's factor, so the bands add ~3.7% to the averaged power that no real field has. The bands are only meaningful while each band's mesh stays within its own harmonic, λr/cell ≥ 0.5.

$BANDS

$COMMON" \
  'python3 run_compare_latt.py planepole --mpw 16 --fieldevery 5 --latt "UN 60 f0.75 0.0" --seed 0,0 --seed2 0.75:0.01,0.0 --z2len 72 --plain 48:120 --avg 0.125:2 0.125:2:3,5' \
  --specx=-1,1.3

plot harm_C_taper_-0.05.png C_taper_-0.05 1 "0:105:module 1, α = 1;105:255:module 2, tapered α 1 → 0.53" \
  "Harmonic bands 6. #149's case C: strong taper after saturation, with bands" \
  "#149's case C (latt_C_taper_-0.05.png) at 16 macroparticles per wavelength: 105 periods at α = 1 to saturation, then 150 periods tapered at dα/dz̄ = −0.05; seeded at ωr, |A| = 0.01 along x. Against the unaveraged solver at 96 nodes/λr, 240 steps/period. As α falls to 0.53 the module's resonance moves up by 32%, inside the fundamental band (±50% at 1 λr/cell), so Puffin prints no band warning; but it moves h times as far relative to band h, out of bands 3 and 5, which Puffin's lattice check does not test. The harmonics agree regardless (3rd 2% low, 5th 2% high at the end), because they are radiated before the taper has moved far. At this sampling the bands do not change the end bunching (0.941 without, 0.938 with); at #149's 4 macroparticles per wavelength they took it from 0.880 to 1.003, which was the aliased harmonics acting back on the beam.

$BANDS

$COMMON" \
  'python3 run_compare_latt.py planepole --mpw 16 --fieldevery 5 --latt "UN 105 1.0 0.0; UN 150 1.0 -0.05" --z2len 80 --plain 96:240 --avg 1:2 1:2:3,5'

plot harm3d_A_detuned_f0.9.png 3d_A_detuned_f0.9 1 "0:60:module 1, α = 1;62:122:module 2, resonant at 0.9 ωr" \
  "Harmonic bands 7. #149's 3D case A, with bands" \
  "#149's 3D case A (latt3d_A_detuned_f0.9.png): 60 periods at α = 1, a 2-period drift, 60 periods resonant at 0.9 ωr, seeded at ωr only; beam 24 z̄2 long. Averaged at 1 λr/cell against unaveraged at 48 nodes/λr, 120 steps/period, 4 MPI ranks each. The bands change the fundamental by 0.04%. The averaged 3rd harmonic is 3.7e-4 of the field energy, half its 1D value, which is the expected direction; the unaveraged 3rd is 6e-5 (see below).

$BANDS

$COMMON3D" \
  'python3 run_compare_latt.py planepole --3d --np 4 --mpw 12 --latt "UN 60 1.0 0.0; DR 2; UN 60 f0.9 0.0" --z2len 34 --blen 24 --plain 48:120 --avg 1:2 1:2:3,5'

plot harm3d_B_ownres_f0.75.png 3d_B_ownres_f0.75 0.75 "0:60:module resonant at 0.75 ωr" \
  "Harmonic bands 8. #149's 3D case B: overlapping bands at 0.125 λr/cell, as in 1D" \
  "#149's 3D case B (latt3d_B_ownres_f0.75.png): one 60-period module resonant at 0.75 ωr, seeded there only; beam 15 z̄2 long. Averaged at 0.125 λr/cell against unaveraged at 48 nodes/λr, 120 steps/period, 4 ranks each. Not a valid use of the bands, for the reason in figure 5: the bands overlap at this cell size and add ~5% to the averaged power at the 0.75 colour, its image and an image near 8.75 ωr.

$BANDS

$COMMON3D" \
  'python3 run_compare_latt.py planepole --3d --np 4 --mpw 12 --latt "UN 60 f0.75 0.0" --seed 0,0 --seed2 0.75:0.001,0.0 --z2len 22 --blen 15 --plain 48:120 --avg 0.125:2 0.125:2:3,5' \
  --specx=-1,1.3

plot harm3d_C_taper_-0.05.png 3d_C_taper_-0.05 1 "0:150:module 1, α = 1;150:250:module 2, tapered α 1 → 0.69" \
  "Harmonic bands 9. #149's 3D case C: strong taper after saturation, with bands" \
  "#149's 3D case C (latt3d_C_taper_-0.05.png): 150 periods at α = 1 to saturation, then 100 periods tapered at dα/dz̄ = −0.05; beam 47 z̄2 long; transverse mesh widened to 61 nodes over 1.5x the width in x. Averaged at 1 λr/cell against unaveraged at 24 nodes/λr, 60 steps/period, 6 ranks each. At the end the 3rd harmonic agrees to 11%; 24 nodes/λr is under 5 nodes per 5th-harmonic wavelength, so the unaveraged 5th harmonic here is not converged and its 1.7x is not a test. The bands move the fundamental's power by ~1%.

$BANDS

$COMMON3D" \
  'python3 run_compare_latt.py planepole --3d --np 6 --mpw 12 --latt "UN 150 1.0 0.0; UN 100 1.0 -0.05" --z2len 65 --blen 47 --set sFModelLengthX=12.0 --set iNumNodesX=61 --set iRedNodesX=31 --plain 24:60 --avg 1:2 1:2:3,5'
