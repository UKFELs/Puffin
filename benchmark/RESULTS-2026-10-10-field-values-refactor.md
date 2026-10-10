# Performance: the `tFieldValues` refactor and two polarisation components

Measured 2026-10-10, on the branch behind UKFELs/Puffin#139.

Question: did folding the parallel field arrays into `tFieldValues`, moving the
RK4 scratch into a `tRK4Workspace`, and giving the field mesh arrays a component
index cost anything at runtime?

Short answer: **no.** All four cases came out faster, three of them by less than
run-to-run scatter and one (`3d_lattice`) by more, and the output is bit-for-bit
identical to `dev` on every case.

## What was compared

| | Commit | Subject |
|---|---|---|
| Before | `d2c7511` | `dev` head — merge of UKFELs/Puffin#138 |
| After | `cd82415` | branch's last code commit — "Wrap the long lines the lint gate rejects" |

`cd82415` rather than the branch tip because the tip is the commit that adds
this write-up, and it touches no code.

Two fresh out-of-tree builds, configured identically and built from clean
trees: `-DCMAKE_BUILD_TYPE=Release` (`-O3`), `-DENABLE_PARALLEL=ON`,
GNU Fortran 14.3.0, Open MPI 5.0.10, FFTW 3, parallel HDF5 1.14.6,
`OMP_NUM_THREADS=1`.
Host: `tyrell`, Linux 7.0.0, Intel Core i5-4690K (4 cores, no SMT), idle.

The two builds' `flags.make` were diffed and are identical, so the only
difference between them is the source. This matters for this pair in
particular: `dev` gained "Stop passing C OpenMP flags to gfortran" (`3a4cbcb`)
between the two runs recorded here, so both sides are compiled with `-fopenmp`
rather than one of them with `-fopenmp=libomp`.

3 repetitions per case; the minimum is reported. Cases were run one per
invocation, **alternating the two builds**, so that any thermal or load drift
over the half hour the suite takes hits both builds alike rather than only the
one measured second.

These numbers are from a different machine than the August 2026 sets in
`results/`, so they are not comparable with those. Compare the two files from
this run with each other:

```bash
./benchmark/compare_benchmarks.py \
    benchmark/results/2026-10-10_d2c7511_dev-baseline.json \
    benchmark/results/2026-10-10_cd82415_field-values-refactor.json --markdown
```

## Results

"Integration" is the time Puffin reports for the undulator modules themselves;
"Wall" is the whole `mpirun` invocation including setup and HDF5 output.

| Case | Ranks | Integration before (s) | Integration after (s) | Δ | Wall before (s) | Wall after (s) | Δ |
|---|---:|---:|---:|---:|---:|---:|---:|
| `1d_serial` | 1 | 71.967 | 71.706 | -0.4% | 74.117 | 73.879 | -0.3% |
| `1d_mpi` | 2 | 38.374 | 38.111 | -0.7% | 40.164 | 39.907 | -0.6% |
| `3d_single` | 2 | 23.434 | 23.304 | -0.6% | 24.874 | 24.742 | -0.5% |
| `3d_lattice` | 2 | 74.361 | 73.112 | **-1.7%** | 78.479 | 77.204 | -1.6% |

`compare_benchmarks.py --threshold 5` exits 0. Wall-clock tracks integration to
within 0.1% on every case, so nothing moved in setup or I/O either.

### Only `3d_lattice` is outside the scatter

The minimum is the right estimator in general — interference can only make a
run slower — but a sub-1% difference should not be read off it without looking
at the spread. Integration time, all three repetitions:

| Case | | rep 1 | rep 2 | rep 3 |
|---|---|---:|---:|---:|
| `1d_serial` | `d2c7511` | 72.379 | 71.978 | 71.967 |
| | `cd82415` | 71.890 | 72.300 | 71.706 |
| `1d_mpi` | `d2c7511` | 38.377 | 38.468 | 38.374 |
| | `cd82415` | 38.327 | 38.111 | 38.546 |
| `3d_single` | `d2c7511` | 23.773 | 23.434 | 23.616 |
| | `cd82415` | 23.304 | 23.442 | 23.507 |
| `3d_lattice` | `d2c7511` | 75.124 | 75.366 | 74.361 |
| | `cd82415` | 73.815 | 73.112 | 73.502 |

The first three cases' ranges overlap, so -0.4% to -0.7% is scatter and those
cases are level. `3d_lattice` does not overlap — its slowest branch run beats
the baseline's fastest — so that -1.7% is a real difference on this deck. No
cause was established and none is claimed.

### Measured twice

An earlier run of the same suite compared `386a594` (the then `dev` head)
against the pre-rebase branch, and reached the same verdict by a different
route: `1d_mpi` -3.0%, `3d_single` -1.6%, `3d_lattice` -1.5%, `1d_serial`
+0.3%. Only `3d_lattice` reproduced its magnitude across the two runs, which is
the same conclusion the spread above gives. The `+0.3%` on `1d_serial` came out
-0.4% here, confirming it was noise rather than a cost.

Those JSONs are not kept: the branch was rebased onto `d2c7511`, so the commit
they name as "after" is no longer reachable from this history and could not be
checked out to reproduce them.

### Threaded spot check

The default build has OpenMP on and the refactor touched the `WORKSHARE`
regions in the RK4, so the same comparison was run with four threads
(`OMP_WAIT_POLICY=active`, see the OpenMP note in the project memo).
`1d_serial` only: it is a single rank, so no rank-to-thread mapping has to be
got right for the number to mean anything — which is the trap that makes
multi-rank threaded timings on this suite worthless unless `--map-by
slot:PE=$OMP_NUM_THREADS --bind-to core` is passed.

| | Integration (s) | Δ |
|---|---:|---:|
| `d2c7511`, 4 threads | 72.140 | |
| `cd82415`, 4 threads | 71.853 | -0.4% |

Note what this *also* shows: four threads buy nothing on this deck — if
anything it is marginally slower than the 71.967 s the same build takes at one
thread. See the caveat below.

## The numbers did not move

Timing is half the question. `compare_outputs.py` was run over the dumps the
timing runs had already written, baseline against branch, matching on `istep`:

| Case | Dumps matched | Worst L2rel | Differing bits |
|---|---:|---:|---:|
| `1d_serial` | 10 | 0.000e+00 | 0 / 222817 |
| `1d_mpi` | 4 | 0.000e+00 | 0 / 222817 |
| `3d_single` | 6 | 0.000e+00 | 0 / 28000 |
| `3d_lattice` | 6 | 0.000e+00 | 0 / 28000 |

Bit-for-bit identical. A refactor that moves the field arrays into a derived
type and adds a component dimension to them reproduces `dev` exactly on these
decks, which is the stronger statement than the e2e suite's 1e-10.

One trap worth recording: the 3D decks write into `inputs/3D`, not `inputs`.
Pointed at the wrong directory `compare_outputs.py` matches no dumps at all and
still prints `worst L2rel over all matched dumps: 0.000e+00`, which reads as a
pass. Check the per-dump lines are there, not just the summary.

## What this does not show

- **Nothing about memory traffic at production sizes.** `bench_1d.in` is
  ~31.8k macroparticles over 5748 field nodes, so its working set is
  cache-resident, and the 3D decks are only a little better. Adding a component
  dimension to the field arrays is exactly a memory-layout change, so the
  results above should not be assumed to hold on a deck that does not fit in
  L3. Scaling `iMPsZ2PerWave` by 8 and `nPeriods` by 1/8 grows the working set
  at constant runtime and is the way to test that.
- **Nothing about OpenMP scaling.** The threaded check is a valid
  before-and-after comparison, but at four threads the deck ran no faster than
  at one, so it did not exercise threading in any meaningful way.
- **Nothing about other machines.** One host, 4 cores, no SMT. Only the ratios
  mean anything, and only on hardware like this.
- **Nothing about the averaged mode's own cost.** These decks are all
  unaveraged, so they measure the refactor, not the two-component averaged
  path. `benchmark/averaged/` covers that comparison, for accuracy rather than
  speed.
