# 1D lattice-driven linear-taper e2e test — parked

Parked, not deleted: it passes on macOS/ARM and fails in CI on Linux/x86,
and the reason the two disagree is **not yet understood**. Everything
needed to pick the investigation back up is in this directory.

Cut down from `inputs/simple/1D/osc-linear-taper/shorter`. It covers ground
the other 1D e2e test does not: five undulator modules driven from a lattice
file, undulator ends, a linear taper with initial detuning, drift/chicane/quad
elements between modules, and 4350 integration steps rather than 30, over
which the field grows from identically zero to a peak amplitude of ~7.8e-3.

## How to run it

The deck uses CWD-relative paths, so run from this directory:

```
mpiexec -n 2 <build>/puffin/puffin inputs/1D/osc_taper.in
```

That writes `inputs/1D/osc_taper_{aperp,electrons,integrated}_{0,1,2}.h5`.
Index 2 is the final state (step 4350). Compare against `expected/1D/`,
which was generated on macOS/ARM with gfortran 16.1.0 at 2 MPI ranks:

```
h5diff -d 1e-10 inputs/1D/osc_taper_aperp_2.h5 expected/1D/osc_taper_expected_field.h5
```

To put it back in the suite, move `testMPIIntegration1DTaper.pf` up to
`test/`, move `inputs/1D` and `expected/1D` back alongside the other test
data, and restore the `configure_file` blocks and the
`add_pfunit_ctest(puffin_e2e_tests_1d_taper ...)` target in
`test/CMakeLists.txt`. See commit history for the exact block.

## The open question

Comparing a Linux/x86 CI run against the macOS/ARM reference, with the
**same** input beam read from `inputs/1D/osc_taper_beam.h5`:

| quantity | max deviation | notes |
| --- | --- | --- |
| `power`, `powerSI`, `Intensity` | 1.284e-9 rel | all three identical |
| field array | 7.87e-12 abs | on a peak of 7.785e-3, so ~1.0e-9 rel |
| `bunching2ndHarmonic` | 3.52e-11 rel | |
| `bunchingFundamental` | 1.29e-11 rel | |
| electron z2 / gamma | 1.98e-12 / 2.66e-15 abs | ~12-14 ulp |
| `beamCurrent` | 9.85e-13 rel | |
| `meanGamma` | 6.66e-16 rel | ~3 ulp |
| `Slice Charge` | 0.0 | bit-identical |

For contrast, on **one** machine with only `OMP_NUM_THREADS` varied (1 vs 4),
the field agrees to better than 1e-15 absolute.

So the field is ~4 orders of magnitude further apart across platforms than it
is across thread counts on one machine, while everything computed from the
macroparticles stays at ulp level. That asymmetry is the thing that needs
explaining. It may be legitimate accumulation over 4350 steps, but it has not
been demonstrated, and until it is the size of the divergence is reason enough
not to pin tolerances around it.

Quantities derived from the field (`power`, `powerSI`, `Intensity`) simply
inherit the field's error — they land on the same number — so there is one
discrepancy here, not four.

### Already ruled out

- **Shot-noise RNG.** Puffin draws shot noise from the intrinsic
  `RANDOM_NUMBER`, whose stream is compiler- and version-specific;
  `iRandSeed` makes a run repeatable on one toolchain but not portable. The
  deck now reads a committed beam (`dtype = 'h5'`) so the initial
  macroparticles are byte-identical on both platforms. This was a real cause
  of an earlier CI failure and is fixed; it is not the current one.
- **Macroparticle file ordering.** The test sorts each component before
  comparing, so it no longer assumes rows land in the same order. The
  electron comparison passes on Linux either way.
- **FFTW.** Not involved: in 1D both the plan creation and the transform call
  are commented out in `puffin/lib/diffraction/transforms.f90`
  (`getTransformPlans4FEL` and `Transform`), so no transform runs.

### Not yet investigated

- Whether the divergence grows smoothly with step count (run 1 module instead
  of 5 and compare) — that would separate steady accumulation from something
  that appears at a particular point.
- Whether it is x86-vs-ARM or gfortran-16-vs-gcc-11; two Linux builds at
  different `-O` levels, or a macOS build at `-O0`, would separate compiler
  from architecture.
- Whether `-ffp-contract=off` on both sides closes the gap, which would point
  at FMA contraction.

## Tolerances

Left as they were, deliberately, rather than widened to make the test pass:
1e-10 absolute on the field and macroparticles, 1e-10 relative on the
integrated datasets. Note these are not the same standard — 1e-10 absolute
against a 7.785e-3 peak is ~1.3e-8 relative, so the integrated check is
roughly 128x stricter than the field one, which is why `power` fails while
the field it is derived from passes.

The test prints the observed margin for every comparison before asserting, so
a run states its own numbers. On the machine that generated the reference they
are all exactly zero.
