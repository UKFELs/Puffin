# Puffin performance benchmarks

These are timing runs, not correctness tests. They exist so that refactoring
work — in particular the migration from module-level globals to the
`tSimulationContext` derived types — can be shown not to have cost anything at
runtime, and so that future changes can be checked the same way.

Correctness is covered by the pFUnit suites in `../test`; nothing here asserts
on physics output.

## Requirements

`run_benchmarks.py` needs Python 3.8 or newer (standard library only) and an
`mpirun` on `PATH`. `compare_outputs.py` additionally shells out to `h5dump`,
from the HDF5 tools — the same HDF5 install Puffin is built against already
provides it.

Recorded results live in `results/`, and write-ups alongside them —
see [RESULTS-2026-08-07-globals-refactor.md](RESULTS-2026-08-07-globals-refactor.md)
for the pre- vs post-refactor comparison.

## The cases

| Case | Ranks | What it exercises |
|---|---:|---|
| `1d_serial` | 1 | 1D flat-top CSE integration, no MPI traffic. The most sensitive case for per-step call overhead — argument passing, derived-type indirection, inlining. |
| `1d_mpi` | 2 | Same deck on 2 ranks: adds the parallel field/beam exchange in `para_field.f90`. |
| `3d_single` | 2 | 3D CLARA, one undulator module, 85×85 transverse mesh with diffraction. Dominated by the FFT diffraction step and the 3D interpolation in `system_interpolation.f90`. |
| `3d_lattice` | 2 | 3D CLARA, full 17-module lattice. Adds quads, drifts, chicanes and the undulator match-in/match-out paths in `acc_lattice.f90`, plus repeated element setup. |

Decks live in `inputs/`. They are scaled-up copies of the decks the integration
tests use (`../test/inputs`), sized so each case runs for a few seconds to tens
of seconds — long enough for the timing to mean something. The beam and seed
files, and the pre-generated HDF5 beam that keeps the 3D runs deterministic, are
reused from `../test/inputs` rather than duplicated; `run_benchmarks.py` stages
both trees into a scratch directory before running.

`3d_lattice` uses `../test/inputs/3D/clara_full.in` unmodified — that deck was
already long enough.

## Metrics

Each repetition records two numbers:

- **`undulator_s`** — the sum of the `Finished undulator module in X seconds`
  values Puffin prints. This is the integration loop only, and is the primary
  regression signal: no process startup, no MPI bootstrap.
- **`wall_s`** — wall-clock time for the whole `mpirun` invocation: setup, beam
  generation, integration and HDF5 output. Catches regressions outside the
  integration loop.

The **minimum** over repetitions is reported. Interference from other processes
can only make a run slower, so the minimum is the least noisy estimate.

## Running

```bash
cmake -B build -DCMAKE_BUILD_TYPE=Release .
cmake --build build
./benchmark/run_benchmarks.py --puffin build/puffin/puffin --reps 5 -o after.json
```

To compare two builds — e.g. before and after a change:

```bash
./benchmark/compare_benchmarks.py before.json after.json --markdown
```

## Checking the numbers didn't move

Timing is only half the question. `compare_outputs.py` compares the HDF5 output
of two runs dump by dump, matching on the `istep` attribute:

```bash
./benchmark/compare_outputs.py runA/inputs/3D runB/inputs/3D bench_3d
```

Use it rather than `h5diff -p`. `h5diff -p` computes |a−b|/|a| per element,
which on a field mesh spanning 1e-17 to 1e-3 is dominated by near-zero nodes
outside the beam, where a "relative difference" of 1e-3 corresponds to an
absolute difference of 1e-20. It will report tens of thousands of differences
for a run that is identical everywhere that carries signal.
`compare_outputs.py` reports against the dataset's own scale instead — see the
measurement-trap note in
[RESULTS-2026-08-07-globals-refactor.md](RESULTS-2026-08-07-globals-refactor.md).

Add `--threshold 5` to make the comparison exit non-zero if any case's
integration time regressed by more than 5%.

`--list` shows the cases, and `--case NAME` (repeatable) runs a subset.

### Via CTest

```bash
cmake -B build -DCMAKE_BUILD_TYPE=Release -DENABLE_BENCHMARKS=ON .
cmake --build build
ctest --test-dir build -L benchmark --output-on-failure
```

The benchmarks carry the `benchmark` label and are marked `RUN_SERIAL`, so they
never run as part of a plain `ctest` and never run concurrently with each other.
`cmake --build build --target benchmarks` runs every case in one go and writes a
single `benchmarks.json`.

## Getting numbers you can trust

- **`OMP_NUM_THREADS` is forced to 1** by the runner. Puffin is built with
  OpenMP; left unset, the OpenMP runtime starts one thread per core *per MPI
  rank*. On an 8-core machine running 2 ranks that is 16 threads fighting over 8
  cores. It is not a small effect — on an Apple Silicon laptop the 3D case ran
  **7× slower** oversubscribed than pinned, and the run-to-run scatter was far
  worse. Do not remove this.
- Build both sides of a comparison with the same compiler, the same flags and
  `-DCMAKE_BUILD_TYPE=Release`.
- Close other work before running, and prefer more repetitions to one long run.
- Absolute times are machine-specific and not worth comparing across hosts. Only
  the before/after ratio on one machine means anything. The `toolchain` block in
  the JSON records the host so mismatches are visible.

## Running on another machine

The tooling is portable — plain Python 3 standard library, `mpirun`, and
`h5dump`. There is nothing macOS-specific in the scripts, the decks or the
CMake glue, and no absolute paths. It should run anywhere Puffin builds.

The *recorded numbers* in `results/` are not portable, and this is the one thing
to get right. Absolute timings mean nothing across machines: a different CPU,
memory system, MPI or compiler moves them arbitrarily. Only a before/after ratio
measured on one machine, back to back, says anything.

So on a new machine, do not compare against the committed JSONs. Generate your
own baseline:

```bash
# build the revision you want as your reference, then:
./benchmark/run_benchmarks.py --puffin /path/to/reference/puffin \
    --reps 5 --label reference -o reference.json
# build your change, then:
./benchmark/run_benchmarks.py --puffin build/puffin/puffin \
    --reps 5 --label mine -o mine.json
./benchmark/compare_benchmarks.py reference.json mine.json --markdown
```

`compare_benchmarks.py` warns loudly if the two files disagree on host,
platform, architecture or core count, so a cross-machine comparison is hard to
make by accident. The committed JSONs are a record of what was measured here,
not a reference to measure against.

Two things to sanity-check on unfamiliar hardware:

- The case sizes were chosen so a run takes roughly 5–35 s on a 2026 laptop.
  On much slower hardware `3d_lattice` in particular will drag; use `--case` to
  select a subset rather than editing the decks, so results stay comparable.
- `1d_mpi`, `3d_single` and `3d_lattice` use 2 ranks. On a single-core machine
  or inside a 1-CPU container these will oversubscribe and the timings will be
  noise. Run `--case 1d_serial` there.

## Comparing against an older revision

`git worktree` is the tidy way to get a second build without disturbing the
working tree:

```bash
git worktree add --detach /tmp/puffin-baseline <old-sha>
cmake -B /tmp/puffin-baseline-build -S /tmp/puffin-baseline -DCMAKE_BUILD_TYPE=Release
cmake --build /tmp/puffin-baseline-build -j8
# run the *current* benchmark scripts against the *old* binary
./benchmark/run_benchmarks.py --puffin /tmp/puffin-baseline-build/puffin/puffin \
    --reps 5 --label baseline -o before.json
```

The decks are plain namelist input read at runtime, so the current benchmark
decks work against older Puffin binaries as long as the namelist keys they use
still existed at that revision.
