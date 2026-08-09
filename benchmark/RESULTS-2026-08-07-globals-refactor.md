# Performance: pre- vs post-globals-refactor

Investigated 2026-08-07; resolved 2026-08-08 (commit `d944eb2`).

Question: did the migration from module-level globals to the `tSimulationContext`
derived types (Phases 0–12, plus follow-ups) cost anything at runtime?

Short answer: **the context migration itself cost nothing.** A single follow-up
commit, `b947d93`, made the 3D path ~48% slower; 1D was unaffected. The cause
turned out to be per-iteration array-descriptor reloads across the `!$OMP
ATOMIC` barriers in `getSource_3D`, not anything about the refactor's design.
It is now fixed, and 3D is faster than it was before the refactor — see
[Resolution](#resolution-2026-08-08-commit-d944eb2).

The sections below are kept in investigation order, including the two
hypotheses that were disproved along the way, because the eliminations are the
useful part.

## What was compared

| | Commit | Subject |
|---|---|---|
| Before | `43929e2` | `Merge branch 'UKFELs:master' into dev-test` — last commit before Phase 2 |
| After | `ce37790` | `Merge remote-tracking branch 'origin/dev-test'` — current `dev-test` head |

Both built from clean trees with the same toolchain and flags:
GNU Fortran 16.1.0, Open MPI 5.0.9, FFTW 3, parallel HDF5,
`-DCMAKE_BUILD_TYPE=Release`, `OMP_NUM_THREADS=1`.
Host: Apple Silicon MacBook Pro (macOS 15.6, 8 cores).

5 repetitions per case; the minimum is reported. Run-to-run scatter was under
1% on every case, so the differences below are far outside the noise.

The table below is a historical record of the regression. Only two result sets
are kept in `results/` — the pre-refactor baseline and the final fixed state —
since the intermediate ones documented a problem that is now solved. To see
where things stand today:

```bash
./benchmark/compare_benchmarks.py \
    benchmark/results/2026-08-07_43929e2_pre-refactor.json \
    benchmark/results/2026-08-08_getSource3D-kernel-split.json --markdown
```

Note that all these numbers were taken on one machine (below) and only the
ratios mean anything. To benchmark elsewhere, generate your own baseline — see
[README.md](README.md).

## Results

"Integration" is the time Puffin reports for the undulator modules themselves;
"Wall" is the whole `mpirun` invocation including setup and HDF5 output.

| Case | Ranks | Integration before (s) | Integration after (s) | Δ | Wall before (s) | Wall after (s) | Δ |
|---|---:|---:|---:|---:|---:|---:|---:|
| `1d_serial` | 1 | 10.750 | 11.230 | **+4.5%** | 11.348 | 11.857 | +4.5% |
| `1d_mpi` | 2 | 8.144 | 8.186 | **+0.5%** | 8.931 | 8.973 | +0.5% |
| `3d_single` | 2 | 5.251 | 7.772 | **+48.0%** | 5.483 | 8.010 | +46.1% |
| `3d_lattice` | 2 | 28.698 | 33.510 | **+16.8%** | 29.731 | 34.556 | +16.2% |

`3d_lattice` shows a smaller percentage than `3d_single` because a large part of
its run time is spent in drifts, chicanes and quads rather than in the undulator
integration loop where the regression lives.

## Where the regression came from

The four refactor phases were benchmarked individually on `3d_single`
(2 repetitions each, same build settings):

| Commit | Phase | Integration (s) |
|---|---|---:|
| `43929e2` | baseline | 5.251 |
| `436361c` | Phase 2 — `tIntegrationState` | 5.261 |
| `c2c1ad2` | Phase 6 — `tSimulationFlags` / `tOutputConfig` | 5.268 |
| `9a3ce16` | Phases 7b & 8 — types lifted to `puffin_main` | 5.212 |
| `de034b3` | Phase 9 — types through the RK4 chain | 5.287 |
| `3e9e99d` | Phase 10 Step 1 | *does not compile — skipped* |
| `a060ba3` | Phase 10 Step 2 — ctx through HDF5/write path | 5.254 |
| `cfe9493` | Phase 10 Step 3 — flags through para_field/interpolation | 5.263 |
| `90ac689` | Phase 10 Step 4 — ctx populated by `init()` | 5.274 |
| `7d45aa0` | Phase 10 Step 5 — remove dead globals | 5.266 |
| `5ce9db7` | Phase 11 — `ctx%frame` through init/runtime | 5.242 |
| `c23c66b` | Phase 12 — remove Cluster D globals | 5.240 |
| `b947d93` | **Remove `n2col`/`n2col0`/`undgrad`/`sz0` globals** | **7.752** |

Everything is flat at ~5.25 s for twelve consecutive commits — the entire
`tSimulationContext` migration is free. The whole regression appears at
`b947d93`.

Note in passing: `3e9e99d` (Phase 10 Step 1) does not compile —
`Type mismatch in argument 'ctx' ... passed TYPE(tundulator) to
TYPE(tsimulationcontext)` in `undulator.f90`. It is fixed by the next commit, so
only `git bisect` and archaeology are affected.

## What `b947d93` appears to do

Profiles (macOS `sample`, 3D deck, single rank) before and after that commit,
as a share of in-Puffin samples:

| Symbol | before | after |
|---|---:|---:|
| `getSource_3D` | 54.6% | 76.4% |
| `getFFelecs_3D` | 7.1% | 4.0% |
| `getInterps_3D` | 6.7% | 4.0% |
| everything else | 31.6% | 15.6% |

Combined with the 1.48× total slowdown, that puts `getSource_3D`
(`interaction/system_interpolation.f90`) at roughly **2× its former cost**, with
everything else unchanged. `getSource_3D` is the `!$OMP ATOMIC` scatter of the
electron source term into the field arrays.

The awkward part: `b947d93` does not touch `system_interpolation.f90`, and it
does not touch `bfields.f90` or `puffin_equations.f90` either. Its changes are
in `acc_lattice.f90`, `init_conds.f90`, `setup_calcs.f90`, `undulator.f90`,
`adapter_globals.f90`, `deriv_globals.f90` and `simple_electron_gen.f90` — it
replaces the module globals `n2col`, `n2col0`, `undgrad` and `sz0` with
`und%…` components and explicit scalar arguments.

## What has been ruled out

Each of these was measured, not assumed. Please don't repeat them.

**It is not a code-generation effect.** This was the leading hypothesis and it
is now refuted. Both commits were rebuilt with `-fopt-info-all` and the
optimiser reports compared for every hot file. After normalising GCC's internal
pointer addresses and the build path embedded in runtime error strings, the
reports for `system_interpolation.f90`, `rhs.f90`, `puffin_equations.f90`,
`bfields.f90`, `para_field.f90`, `puffin_mpi_RK4.f90` and `diffraction.f90` are
**identical, line for line**. GCC made exactly the same inlining,
vectorisation and hoisting decisions on both sides.

Stronger still, comparing the emitted machine code object by object across the
whole library, only **7** translation units differ — precisely the 7 files the
commit edits (`acc_lattice`, `adapter_globals`, `simple_electron_gen`, `setup`,
`setup_calcs`, `undulator`, `init_conds`), none of which is in a per-step path.
`system_interpolation.f90` — which contains `getSource_3D`, `getInterps_3D` and
`getFFelecs_3D`, i.e. the entire measured regression — assembles to
byte-identical instructions.

**It is not extra work.** Same step count, same output dumps.

**It is not a physics change.** See the numerical fidelity section below.

**It is not denormals.** Neither field output contains a single denormal value.

**It is not MPI load imbalance.** `maxEl = maxval(procelectrons_G)` bounds the
loops in all three affected routines, so a skewed decomposition would inflate
them on every rank. But the regression is *worse* on one rank than on two:

| ranks | `c23c66b` | `b947d93` | Δ |
|---:|---:|---:|---:|
| 1 | 6.390 s | 10.672 s | +67% |
| 2 | 5.164 s | 7.597 s | +47% |

At one rank `maxEl` is the full electron count in both builds, so this is not it.

**It is not array sizing.** Peak resident set size is 95.27 MB before and
95.57 MB after — 0.3% apart. The local field window, buffers and mesh are the
same size.

**It is not runtime state that develops during the run.** Scanning run length
at one rank, the overhead is a flat multiplier present from the very first
steps, not something that grows as electrons spread or the field builds:

| steps | before (ns/step) | after (ns/step) | ratio |
|---:|---:|---:|---:|
| 750 | 668 | 1147 | 1.72 |
| 1500 | 682 | 1154 | 1.69 |
| 3000 | 685 | 1161 | 1.70 |
| 6000 | 703 | 1171 | 1.67 |
| 9000 | 716 | 1188 | 1.66 |

## Resolution (2026-08-08, commit `d944eb2`)

The evidence above pointed at data placement, and a first guess was cache-set
conflicts or 4K aliasing. **That guess was wrong too**, and the follow-up work
disproved it before finding the real answer — padding sweeps from 0 to 512
bytes and code-alignment sweeps are both dead flat, and every hot array is
page-aligned in both builds.

Hardware counters gave the decisive clue: the two builds **retire the same
instructions to within 0.09% while cycles rise 67%**. Pure stalls, not extra
work — exactly consistent with the byte-identical machine code above.

The mechanism: on AArch64 the 16 `!$OMP ATOMIC` updates in `getSource_3D` are
acquire/release sequences, i.e. memory barriers, so GCC cannot keep module
state in registers across them. The loop reloads the array descriptors for
`s_chi_bar_G`, `sp2`, `p_nodes` and `lis_GR`, plus the scalar `dV3`, from memory
on *every iteration* — twice over, for the real and imaginary parts. That puts
their placement on the per-iteration critical path. `b947d93` shifted the
`globals` module down 16 bytes, landing the `s_chi_bar_G` descriptor at
`…3ff8`, so the 16-byte `ldp` that reads it straddles a page boundary.

So placement *was* the trigger, but the vulnerability was the per-iteration
reload. Padding could not fix it because padding only moves the descriptor to
some other arbitrary address.

The fix keeps `getSource_3D`'s signature and makes it a thin wrapper handing the
module data to `getSource_3D_kernel` as `contiguous, intent(in)` dummy
arguments, whose base addresses are loop-invariant by construction. The routine
is no longer sensitive to where the linker places module symbols, so an
unrelated commit cannot shift it back. `getSource_1D` got the same treatment.

Final state, against the pre-refactor baseline:

| Case | Ranks | `43929e2` pre-refactor (s) | fixed (s) | Δ |
|---|---:|---:|---:|---:|
| `1d_serial` | 1 | 10.750 | 11.034 | +2.6% |
| `1d_mpi` | 2 | 8.144 | 8.190 | +0.6% |
| `3d_single` | 2 | 5.251 | 4.924 | **−6.2%** |
| `3d_lattice` | 2 | 28.698 | 28.205 | **−1.7%** |

Both 3D cases now beat the pre-refactor code. 3D output is unchanged to
round-off (worst L2 relative 2.4e-14 over 9000 steps); 1D output is
bit-identical. Raw data in `results/2026-08-08_getSource3D-kernel-split.json`.

**The transferable lesson:** any loop in this codebase that mixes `!$OMP ATOMIC`
with module-level allocatable arrays is exposed to the same trap. The barriers
force descriptor reloads, and the cost then depends on where the linker happened
to put things — so it can appear and disappear from unrelated commits. Passing
the arrays in as `contiguous, intent(in)` dummies is the durable fix.

## Numerical fidelity: did the refactor change the answers?

Separate question from performance, and worth its own numbers. Compared
`43929e2` against `ce37790` over full-length runs, dump by dump, matching on the
`istep` attribute. Reproduce with `compare_outputs.py`:

```bash
./benchmark/compare_outputs.py runA/inputs runB/inputs bench_1d
./benchmark/compare_outputs.py runA/inputs/3D runB/inputs/3D bench_3d
```

**1D (`bench_1d`, 6000 steps): bit-identical.** Every field value and every
electron coordinate, at all 12 dumps, in both builds. Zero differing bits.

**3D (`bench_3d`, 9000 steps): round-off only, and it does not amplify.** The
field grows from zero to |A| = 0.557 over the run — about five orders of
magnitude of gain — which is exactly the regime where a seeded difference would
blow up if there were one. It doesn't:

| istep | max\|A\| | max abs diff / max\|A\| | L2 relative | max rel diff where \|A\| > 1e-3·max |
|---:|---:|---:|---:|---:|
| 1200 | 5.29e-03 | 1.97e-15 | 2.85e-15 | 5.08e-13 |
| 3600 | 1.42e-02 | 2.82e-15 | 2.88e-15 | 4.36e-13 |
| 6000 | 5.56e-02 | 1.50e-15 | 2.52e-15 | 5.78e-13 |
| 9001 | 5.57e-01 | 5.81e-15 | 3.86e-15 | 4.93e-13 |

The divergence sits at ~3e-15 — a couple of units in the last place of a double
— and stays there while the field gains five orders of magnitude. It creeps up
roughly linearly (2.5e-15 → 3.9e-15), not exponentially. Electrons track the
same story: bit-identical at first, then slowly diverging, reaching 2.1e-14 of
scale at the final dump.

### A measurement trap worth recording

An earlier pass of this analysis used `h5diff -p` and concluded the field
"differed by 1e-9 to 1e-3 relative". That was wrong, and the reason is worth
knowing. `h5diff -p` computes |a−b|/|a| element by element. The 3D field mesh
spans 6.2e-17 to 1.25e-3, and the overwhelming majority of its 303450 nodes sit
near zero, far outside the beam. A node holding 1e-17 that differs in its last
bit reports a "relative difference" of 1e-3 while the absolute difference is
1e-20 — physically nothing. Roughly 50000 nodes tripped the 1e-9 threshold on
exactly this basis.

Measured against the field's own scale, the same data gives 2.9e-15. Use
`compare_outputs.py` rather than `h5diff -p` for this; it exists because of
this mistake.

The other apparent paradox — electrons bit-identical while the field differed —
was real but benign: over a short 150-step run the field had accumulated ~1e-15
of relative difference while the electron coordinates had not yet moved by even
one unit in the last place. Over the full 9000-step run the electrons do
diverge, at the same round-off level.

## Method notes for the next investigation

What actually worked here, in the order that paid off:

1. **Benchmark every commit in the range, not just the endpoints.** Twelve flat
   commits and one 48% jump is a far more useful result than "the refactor cost
   48%", and it took about ten minutes of unattended builds.
2. **Compare emitted machine code before theorising about the compiler.**
   `objdump -d` on each object, with addresses and embedded build paths
   normalised out, settled in one step what a day of reading optimiser reports
   would not have.
3. **Count instructions before blaming memory.** Same instructions retired with
   more cycles means stalls; it immediately rules out every "it's doing more
   work" hypothesis. `/usr/bin/time -l` reports both on macOS and
   `perf stat -e instructions,cycles` on Linux, so this needs no special
   tooling.
4. **Scan run length.** A constant ratio across run lengths says the cause is
   static; a growing one says runtime state diverges. That single plot killed
   several candidate explanations at once.

And two traps worth remembering: `h5diff -p` is the wrong tool for judging field
output (see above), and `OMP_NUM_THREADS` must be pinned or nothing is
measurable at all.

# Resolution (2026-08-08)

**Found and fixed.** The caveat above was right about the shape of the problem —
it *was* luck, and the pre-refactor build was the lucky one — but wrong about
the mechanism. It is not cache-set conflicts or 4K aliasing on the field
arrays. It is that `getSource_3D` was **reloading its module array descriptors
from memory on every loop iteration**, and `b947d93` moved one of those
descriptors onto a page boundary.

## Ruling out the placement hypotheses

Both suggested placement probes were run, and both are negative.

**Data placement (step 1) — refuted.** Sweeping unused `real(kind=wp)` padding
at the exact site of the four removed globals, 3d_single at one rank, three
reps each (symbols verified present in the binary with `nm`):

| padding | 0 | 1 | 2 | 4 | 8 | 16 | 32 | 64 |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| min integration (s) | 10.82 | 10.80 | 10.84 | 10.87 | 10.76 | 10.80 | 10.89 | 10.80 |

Dead flat across 0–512 bytes. Nothing anywhere near the 6.4 s baseline; the
whole spread is 1.2%, i.e. run-to-run noise. The earlier "32 bytes recovers
5%" probe was noise. (The reason it cannot work: the arrays `getSource_3D`
touches are `allocatable`, so they live on the heap. Padding module scalars
repacks `.bss`, which is not where the data is.)

**Code placement — also refuted.** Shifting the whole hot text region by
inserting a dummy routine ahead of `derivs` moved `getSource_3D` across
mod-128 alignments 96 → 0 → 32; timings stayed at 10.80–10.90 s. Notably, at
`mod 128 = 32` — the *exact* alignment the fast build has — the slow build is
still slow. Function alignment is not it.

**Heap addresses — measured, effectively identical.** Dumping `loc()` of
`sDADzr`, `sDADzi`, `sAr`, `p_nodes`, `lis_GR`, `sp2`, `s_chi_bar_G`, `spr`,
`sgam` at several calls: every one is page-aligned in *both* builds, and
`sDADzr`/`sDADzi`/`sAr`/`lis_GR` sit at identical offsets mod 128 KB. There is
also a general argument against any heap-placement theory here: physical page
assignment varies from run to run, so a placement effect would show run-to-run
variance. Both builds are stable to under 1%.

## What it actually is

Hardware counters (`/usr/bin/time -l`, 3d_single, one rank) localise it
precisely:

| | instructions retired | cycles elapsed | IPC |
|---|---:|---:|---:|
| `c23c66b` | 171,533,446,650 | 29,801,350,665 | 5.76 |
| `b947d93` | 171,691,679,621 | 49,772,223,921 | 3.45 |

**+0.09% instructions, +67% cycles.** Identical work, pure stalls.

Cumulative region timers inside `getrhs` (identical instrumentation in both
builds, totals at 36000 calls) confirm the profile attribution was correct and
show the regression is entirely in one routine:

| region | `c23c66b` | `b947d93` |
|---|---:|---:|
| allocation | 0.006 s | 0.005 s |
| `p_nodes` | 0.152 s | 0.150 s |
| `getInterps_3D` | 0.308 s | 0.304 s |
| `getFFelecs_3D` | 0.313 s | 0.305 s |
| **`getSource_3D`** | **2.105 s** | **6.140 s** |
| electron equations | 0.324 s | 0.325 s |
| total | 3.245 s | 7.258 s |

Everything matches to three digits except `getSource_3D`, which is 2.9× — and
that accounts for the whole +4.0 s.

The mechanism is visible in the disassembly. `getSource_3D` does 16 `!$OMP
ATOMIC` updates per iteration, and on AArch64 those are acquire/release
sequences — memory barriers. GCC therefore cannot keep module state in
registers across them, so the loop body **reloads the array descriptors for
`s_chi_bar_G`, `sp2`, `p_nodes`, `lis_GR` and the scalar `dV3` from memory on
every iteration**, twice over (real and imaginary parts). Their placement is
consequently on the per-iteration critical path.

`b947d93` shifted the `globals` module symbols down by 16 bytes, which put the
`s_chi_bar_G` descriptor at `0x1000c3ff8` instead of `0x1000c4008`:

```
before:  ldp x21, x3, [x17]   x17 = 0x1000c4008   -> one 64-byte line
after:   ldp x21, x3, [x17]   x17 = 0x1000c3ff8   -> straddles 0x1000c4000
```

The `ldp` reads 16 bytes, so at `0x1000c3ff8` it crosses both a cache line and
a 4 KB page boundary — twice per iteration, 1.44e8 iterations. That is the
1.67×. It also explains every negative result above: the sensitive address is a
*module symbol* read inside the loop, not the heap arrays everyone was
looking at, and not the code.

## The fix

`puffin/lib/interaction/system_interpolation.f90`: `getSource_3D` keeps its
signature and becomes a thin wrapper that passes the module data it needs into
a new `getSource_3D_kernel` as dummy arguments (`contiguous, intent(in)`). The
loop then addresses everything through dummy-argument descriptors, which are
local and provably loop-invariant, so the barriers no longer force reloads. No
alignment directives, no padding, and nothing about the refactor reverted.

This is deliberately the robust fix rather than an alignment tweak: the routine
is no longer sensitive to where the linker puts module symbols, so an unrelated
commit cannot shift it back.

| Case | Ranks | unfixed `ce37790` | fixed | Δ |
|---|---:|---:|---:|---:|
| `3d_single` | 2 | 7.648 s | 4.924 s | **−35.6%** |
| `3d_lattice` | 2 | 33.597 s | 28.205 s | **−16.0%** |

Against the pre-refactor baseline `43929e2`, i.e. the original target of
parity — it is now *faster* than before the refactor:

| Case | Ranks | `43929e2` | fixed | Δ |
|---|---:|---:|---:|---:|
| `1d_serial` | 1 | 10.750 s | 11.034 s | +2.6% |
| `1d_mpi` | 2 | 8.144 s | 8.190 s | +0.6% |
| `3d_single` | 2 | 5.251 s | **4.924 s** | **−6.2%** |
| `3d_lattice` | 2 | 28.698 s | **28.205 s** | **−1.7%** |

Raw data: `results/2026-08-08_getSource3D-kernel-split.json`, comparable
directly against `results/2026-08-07_43929e2_pre-refactor.json`.

Numerics are unchanged to round-off. `compare_outputs.py` over the full
9000-step 3D run, unfixed vs fixed, worst L2 relative difference **2.4e-14**
across all dumps while the field gains five orders of magnitude — the same
character as the round-off already documented above. `puffin_basic_tests` and
`puffin_e2e_tests_3d` pass. (`puffin_e2e_tests` fails identically on the
unmodified head in a fresh worktree — a missing `inputs/f1main_electrons_2.h5`
fixture, unrelated.)

## `getSource_1D`, same treatment

`getSource_1D` in `system_interpolation_1D.f90` had the same shape — an
`!$OMP ATOMIC` scatter reading `dadz_w`, `p_nodes` and `lis_GR` straight from
their modules — with 4 atomics per iteration rather than 16, and with the
expensive arithmetic already hoisted into a `!$OMP WORKSHARE` above the loop.
It has been split the same way: the WORKSHARE stays in the wrapper (its
implicit barrier still orders the write to `dadz_w` before the loop reads it),
and the loop moves into `getSource_1D_kernel` taking the module data as
`contiguous, intent(in)` dummy arguments.

There was no acute regression here to recover, and the measured gain is
correspondingly small — same-session A/B, 5 reps:

| Case | Ranks | before | after | Δ |
|---|---:|---:|---:|---:|
| `1d_serial` | 1 | 11.038 s | 10.934 s | −0.9% |
| `1d_mpi` | 2 | 8.350 s | 8.283 s | −0.8% |

That is close to the noise floor, though consistent in direction across both
cases and across both the integration and wall numbers. The point of the change
is hardening, not speed: 1D is no longer exposed to the same class of accident.

Unlike the 3D change, this one is **bit-identical** — `compare_outputs.py` over
the full 6000-step `bench_1d` run reports zero differing bits at all 12 dumps,
for both the field and the electrons. The arithmetic in the loop body is
untouched, so there is no FMA-contraction change of the kind that produced the
3D round-off.

## One loose end

- **The 1D numbers in the pre-refactor comparison were taken on a different day** to the `43929e2`
  baseline JSON. The 3D numbers are corroborated same-session (the unfixed head
  reproduced 7.648 s against the 7.772 s recorded on 2026-08-07), so the 3D
  comparison is sound; treat the ±2% on the 1D rows as cross-session drift
  rather than signal.
