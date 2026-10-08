#!/usr/bin/env bash
# Build Puffin with the slow and big tests switched on, run them, and write the
# evidence a pull request into master needs (UKFELs/Puffin#116). See README.md
# in this directory.
#
#   run_hpc_tests.sh [-b BUILD_DIR] [-o OUT_DIR] [-j JOBS] [-n NOTES] [-- CMAKE_ARGS...]
#
#   -b  build directory (default: build-hpc in the source tree). An existing
#       one is reused, but its old HDF5 outputs are deleted first, since a
#       stale dump can let a broken build pass.
#   -o  where to write the evidence (default: hpc-evidence-<sha> in the source
#       tree)
#   -j  parallel build jobs (default: all cores)
#   -n  free text added to the PR comment, e.g. the machine or job id
#
# Anything after -- goes to cmake, e.g. where pFUnit is installed, or the MPI
# launcher on a machine that wants srun rather than mpiexec:
#
#   run_hpc_tests.sh -- -DCMAKE_PREFIX_PATH=$HOME/pfunit-install \
#                       -DMPIEXEC_EXECUTABLE=$(which srun)
#
# It must run from a clean checkout of the commit under review: the evidence
# records the commit, and is rejected if the tree had local changes. The exit
# status is 0 only if every required test passed.

set -euo pipefail

src=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
build=$src/build-hpc
out=
jobs=$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)
notes=

while getopts "b:o:j:n:h" opt; do
  case $opt in
    b) build=$(mkdir -p "$OPTARG" && cd "$OPTARG" && pwd) ;;
    o) out=$OPTARG ;;
    j) jobs=$OPTARG ;;
    n) notes=$OPTARG ;;
    h) sed -n '2,24p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) exit 2 ;;
  esac
done
shift $((OPTIND - 1))
[ "${1:-}" = "--" ] && shift

cd "$src"
sha=$(git rev-parse HEAD)
if [ -n "$(git status --porcelain --untracked-files=no)" ]; then
  echo "error: $src has local changes. Run this from a clean checkout of the" >&2
  echo "commit under review; evidence from a modified tree is rejected." >&2
  git status --short --untracked-files=no >&2
  exit 1
fi
out=${out:-$src/hpc-evidence-${sha:0:12}}
mkdir -p "$out"
out=$(cd "$out" && pwd)

echo "== Puffin HPC tests at $sha"
echo "   build:    $build"
echo "   evidence: $out"

# Threading is a net loss with the default libgomp runtime, and the golden
# references were all made single-threaded.
export OMP_NUM_THREADS=1

cmake -S "$src" -B "$build" \
  -DCMAKE_BUILD_TYPE=Release \
  -DENABLE_PARALLEL=ON \
  -DENABLE_TESTING=ON \
  -DPUFFIN_SLOW_TESTS=ON \
  -DPUFFIN_BIG_TESTS=ON \
  "$@" 2>&1 | tee "$out/configure.log"

cmake --build "$build" -j "$jobs" 2>&1 | tee "$out/build.log"

find "$build/test/inputs" -name '*.h5' -delete 2>/dev/null || true

# -L slow selects both suites: the big test carries the slow label too. -V
# keeps the output of passing tests, which is where the big test prints the
# reductions the evidence is checked on.
set +e
ctest --test-dir "$build" -L slow -V --output-junit "$out/junit.xml" 2>&1 | tee "$out/ctest.log"
status=${PIPESTATUS[0]}
set -e

python3 "$src/scripts/hpc-tests/hpc_evidence.py" collect "$build" "$src" "$out" \
  --junit "$out/junit.xml" --log "$out/ctest.log" --ctest-exit-code "$status" \
  ${notes:+--notes "$notes"}
