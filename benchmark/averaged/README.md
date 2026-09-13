# Averaged vs unaveraged

`run_compare.py` runs the period-averaged mode (`qAveraged`, see
`puffin/lib/undulator/averaging.f90` and the "Period-Averaged Mode" section of
`doc/manual.tex`) and the ordinary unaveraged solver on the same 1D deck, and
compares them.

```sh
python3 run_compare.py helical                        # defaults: --plain 12:30 24:60 48:120 --avg 1:2
python3 run_compare.py planepole --plain 48:120 96:240
python3 run_compare.py planepole --aw 0.5
python3 compare.py <plain_dir> <avg_dir> 0.005 helical   # full envelope and power tables
python3 compare_avg.py <avg_ref_dir> <avg_dir> ...       # averaged against averaged
```

The unaveraged field is demodulated onto the averaged envelope's normalisation
(the `exp(-i z2/2rho)` component, averaged over exactly one resonant wavelength)
and compared node by node; power is compared as the flat-top mean of `/power`.
Note that `/power` sums every frequency the mesh resolves, so for a planar
undulator it includes harmonic radiation that the averaged mode cannot carry -
see the harmonic decomposition below. The one-wavelength box used for the
demodulation has exact nulls at every harmonic, so `demod` isolates the
fundamental band.

The deck is the CLARA-like 1D one used for `benchmark/convergence` (rho =
0.005, aw = 1.012, gamma_r = 456.4), seeded, noise-free, 60 periods - about
3.8 gain lengths, still in the exponential regime. Needs `mpirun` and
`h5dump`; standard library Python only.

## What the comparison showed (2026-09-11)

**The averaged mode converges at one step per period.** Measured against itself
at 30 steps per period on an 11-cells-per-wavelength mesh:

| helical, steps per period, at 1 wavelength/cell | 1 | 2 | 4 |
| --- | --- | --- | --- |
| envelope L2 | 7.8755e-3 | 7.8748e-3 | 7.8750e-3 |
| runtime | 1.5 s | 1.9 s | 2.8 s |

| helical, wavelengths per cell, at 1 step/period | 1 | 2 | 4 |
| --- | --- | --- | --- |
| envelope L2 | 7.9e-3 | 1.1e-2 | 2.4e-2 |
| power / fine averaged | 0.9979 | 0.9978 | 0.9980 |

The step changes the answer by ~1e-6, so one per period is already converged,
and at 1.5 s that is 9.3x faster than the unaveraged solver on the deck's own
settings (13.8 s). What is left, 0.2%, is the cell size.

Planepole is step-insensitive in the same way, which matters because it is the
polarisation carrying the residual discussed below. At 1 wavelength per cell,
1 and 2 steps per period agree with each other to ~3e-7 relative, sit at the
same 0.99779 of the fine averaged run, and leave the same gap to the converged
unaveraged run - 0.9581 at 56 periods either way. One step per period runs in
1.57 s against 13.9 s unaveraged. So the step is not a candidate explanation for
that residual.

Coarser cells hold their power to 0.25%, but **this deck cannot really
discriminate them**: seeded, narrow band and flat top, so the envelope is
nearly uniform along z2. The envelope error does triple between 1 and 4
wavelengths per cell. Treat 1-2 wavelengths per cell as what is demonstrated
here. A case with real longitudinal structure - SASE, sharp current gradients,
a short bunch - will be far more sensitive, and 4 wavelengths per cell also caps
the representable bandwidth at roughly +/-12% of the resonant frequency.

**The unaveraged solver's default mesh is the less accurate of the two.**
Helical, power at 60 periods, averaged (2 steps/period, 1 wavelength/cell;
1 step/period reproduces these) over unaveraged:

| unaveraged nodesPerLambdar / stepsPerPeriod | runtime | P_avg / P_unavg | envelope L2 |
| --- | --- | --- | --- |
| 12 / 30 | 13.8 s | 1.086 | 0.28 |
| 24 / 60 | 26.4 s | 1.013 | 0.13 |
| 48 / 120 | 53.9 s | 0.997 | 0.064 |
| 96 / 240 | 110 s | 0.994 | 0.034 |

The unaveraged result converges at second order in the mesh, towards a limit
0.7% above the averaged one. At nodesPerLambdar = 12 it is ~9% low: linear
deposition and linear interpolation each attenuate a carrier sampled at 11
cells per wavelength by sinc^2(pi/11), so the coupling is ~5% weak. The step
scans in `benchmark/convergence` could not see this - they held the mesh fixed
and measured against a finer step on the same mesh. The averaged mode stores a
smooth envelope, so it has no such loss.

Every unaveraged run here scales the step with the mesh, holding 0.4 cells
crossed per step, so in principle those sequences could be measuring step error
rather than mesh error. They are not. Halving the step at fixed mesh, for
planepole, changes the power by 4e-6 at nodesPerLambdar = 48 (120 to 240 steps
per period) and by under 1e-6 at 96 (240 to 480), while refining the mesh at
fixed cells per step moves it by 0.45% from 48 to 96. So the sequences are
mesh-limited, and the averaged-to-unaveraged ratios below are unchanged when
measured against the 480-step run.

That is not in conflict with the ~1e-3 error at 0.4 cells per step reported in
`benchmark/convergence`: that figure is the L2 error of the complex field
against a same-mesh, finer-step reference, which is a different measure from
mid-band power at a fixed distance.

**What is left is the model.** The remaining helical 0.63% is the term the
averaged mode drops: the radiation-driven transverse momentum
(`eta p2 A / kappa^2` in `dppdz`), whose cross term with the quiver moves p2
and so the phase. With that term zeroed in the unaveraged solver the two agree
to ~0.07%.

Planepole, power at 56 periods:

| unaveraged nodesPerLambdar | 12 | 24 | 48 | 96 | 48, A-term zeroed |
| --- | --- | --- | --- | --- | --- |
| P_avg / P_unavg | 1.065 | 0.981 | 0.962 | 0.958 | 0.968 |

Those are *total* power, which for a planar undulator is not a like-for-like
comparison: the unaveraged run radiates at the odd harmonics, and the averaged
mode carries one band, at the fundamental. Decomposing the final field of the
n96 run band by band:

| harmonic | 1 | 2 | 3 | 4 | 5 |
| --- | --- | --- | --- | --- | --- |
| % of total power, aw = 1.012 | 97.87 | 0.0024 | 1.72 | 0.0002 | 0.29 |
| % of total power, aw = 0.5 | 99.87 | 0.0023 | 0.12 | 0.0003 | 0.002 |

Odd harmonics only, as expected on axis in 1D; the even ones sit at leakage
level. That also rules out a quiet-start artifact: 4 macroparticles per
wavelength bunch the beam perfectly at the 4th harmonic, but the 4th does not
couple and no power appears there. Helical shows no harmonic content at any
mesh, which is why its total- and fundamental-power comparisons agree.

Comparing the fundamental band alone, at n96, final dump:

| | gap | A-term share | remainder | 3rd + 5th harmonic |
| --- | --- | --- | --- | --- |
| helical, aw = 1.012 | -0.63% | 0.56% | ~-0.07% | 0.00% |
| planepole, aw = 1.012 | -1.67% | 0.60% | -1.07% | 2.01% |
| planepole, aw = 0.5 | -1.46% | 0.52% | -0.95% | 0.12% |

Helical is then fully accounted for by the dropped A-term. Planepole keeps ~1%
that helical does not, and that remainder is flat in aw while the harmonic
content changes by 17x between the two rows. So it is not harmonic emission
being counted on one side of the comparison only, and probably not back-action
from the harmonic fields either, since the harmonic field amplitude falls ~4x
at aw = 0.5 and the remainder does not move. (The A-term share is also roughly
aw-independent, ~0.55%, not the (1 + aw^2/2)/aw^2 scaling one might guess.)

Two caveats on that reasoning. The averaged model omits the harmonics' drive on
the electron energy equation, not only their emission, and there is no clean way
to test that until the mode can carry more than one band - an additional
envelope mesh for the 3rd harmonic, another for the 5th, and so on
(UKFELs/Puffin#129). And the
measure itself is still mesh-drifting: the planar remainder is 0.68% at n48 and
1.07% at n96, with helical drifting by a similar 0.36 percentage points, so
these are limits approached rather than converged values.

The leading candidate for the remaining ~1% is planar-specific and O(rho). The
planar source has a counter-rotating component at the carrier wavenumber that
oscillates at twice the undulator phase; it drives a field ripple of relative
size ~rho which beats back against the quiver in the energy equation, leaving a
slow term. A helical source has no such component - its projection onto the
carrier is purely slow. That predicts the remainder scales linearly with rho,
halving at rho = 0.0025, which is the next diagnostic (UKFELs/Puffin#130).
