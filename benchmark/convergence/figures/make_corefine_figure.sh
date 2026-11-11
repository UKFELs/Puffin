#!/usr/bin/env bash
# Run the 1D co-refinement study and draw figures/corefine.png.
#
#   bash make_corefine_figure.sh OUT [python]
#
# OUT is a scratch directory for the 42 runs (planepole and helical, three
# kernels, levels 1-8 and three one-factor variations).  Runs already finished
# in OUT are kept.  About 25 min with 14 at once on an 18-core Apple M5 Pro in
# a Release build; the finest runs take ~17 min each.  PUFFIN picks the binary.
# The analysis and plot need numpy, h5py and matplotlib.
set -euo pipefail
OUT=$(mkdir -p "${1:?usage: make_corefine_figure.sh OUT [python]}" && cd "$1" && pwd)
PY=${2:-python3}
HERE=$(cd "$(dirname "$0")" && pwd)
CORE_JOBS=${CORE_JOBS:-14} python3 "$HERE/../run_corefine.py" "$OUT" planepole helical
$PY "$HERE/../analyse_corefine.py" "$OUT" --json "$HERE/corefine_results.json"
$PY "$HERE/../plot_corefine.py" "$HERE/corefine.png" --json "$HERE/corefine_results.json" \
  --title "Co-refinement in 1D: z2 cells, macroparticles and steps refined together, every kernel" \
  --scenario "Seeded 1D deck (benchmark/convergence/scan.in), 60 periods. Level r: 12r cells per resonant wavelength (nodesPerLambdar = 12r+1, since Puffin lays nodesPerLambdar-1 cells per wavelength), iMPsZ2PerWave = 4r (19 sub-particles each, so 6.3 macroparticles per cell), 30r steps per period (0.4 cells crossed per step) - every ratio fixed. Every error is against a Richardson extrapolation from levels 2, 4 and 8 - point by point for the field envelope and the energy history - and the three kernels' limits agree to 5e-5 (2e-6 for the envelope). Dashed in the second panel: the drift in total (field + beam) energy, as a fraction of the energy exchanged; it measures only time-integration error, since the transposed gather and scatter conserve energy at any mesh.

Every kernel converges to the same answer, at second order. Doubling only the cells reproduces the full refinement; doubling only the macroparticles or only the steps changes nothing - at these ratios the z2 mesh carries all the error. At a given mesh the error is in the ratio 1 : 1.5 : 2 for linear, quadratic and cubic, the ratio of the B-splines' variances: the leading error is the deposition kernel's own spatial smoothing, and the smoother kernels make it larger." \
  --command "bash benchmark/convergence/figures/make_corefine_figure.sh OUT"
