# Averaged vs unaveraged


`run_compare.py` runs the period-averaged mode (`qAveraged`, see
`puffin/lib/undulator/averaging.f90` and the "Period-Averaged Mode" section of
`doc/manual.tex`) and the ordinary unaveraged solver on the same 1D deck, and
compares them.

```sh
python3 run_compare.py helical                        # defaults: --plain 12:30 24:60 48:120 --avg 1:2
python3 run_compare.py planepole --plain 48:120 96:240
python3 run_compare.py planepole --aw 0.5
python3 run_compare.py planepole --mpw 16 --every 5 --avg 1:2 1:2:3,5 --plain 24:60 48:120
                                                      # harmonic bands 3 and 5, see below
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
fundamental band - or, demodulating at `exp(+i h z2/2rho)`, band h, which is
how an averaged run with `avgHarmonics` is compared band by band.

The deck is the CLARA-like 1D one used for `benchmark/convergence` (rho =
0.005, aw = 1.012, gamma_r = 456.4), seeded, noise-free, 60 periods - about
3.8 gain lengths, still in the exponential regime. Needs `mpirun` and
`h5dump`; standard library Python only.

## SASE

`run_compare_sase.py` and `compare_sase.py` are the same comparison started
from shot noise instead of a seed, on `sase.in` / `sase_beam.in`:

```sh
python3 run_compare_sase.py                             # defaults: --np 4 --avg 1:2 --plain 12:30 24:60 48:120
python3 run_compare_sase.py --und helical --plain 12:30 # the other polarisation
python3 run_compare_sase.py --avg 1:1 1:2 2:2 --plain 12:30
python3 run_compare_sase.py --seeds 1,2,3,4,5 --plain 12:30 --reuse    # ensemble
python3 run_compare_sase.py --sigej 0.01 --plain 12:30  # sharper bunch ends
python3 compare_sase.py <plain_dir> <avg_dir> 0.005 helical   # full tables
```

**Run both polarisations.** `--und` defaults to `planepole`, the deck's own
undulator, and for a long time that was the only one this comparison ever ran.
A planar undulator never drives `B_y`, so it is blind to anything that goes
wrong with the second polarisation component: the averaged helical power was
once reported at exactly twice the unaveraged value while this comparison, run
planar, agreed to ~1%. Its seeded sibling `run_compare.py` has taken `helical`
all along — nobody passed it. A change to averaged mode is not checked until
both have been run, and, since it drives `B_x` and `B_y` with different
couplings, an elliptical one (`run_compare.py 0.5,1`).

The deck is `deck.in`'s beam and undulator - 1D, planar, rho = 0.005,
aw = 1.012, gamma_r = 456.4 - with `q_noise = .true.`, no seed file, and 340
periods, which is zbar 21.4 or about 24 gain lengths: enough for the field to
grow seven orders of magnitude out of the noise and saturate. The z2 sampling
is 8 macroparticles per resonant wavelength rather than `deck.in`'s 4, so the
beam's own Nyquist is above the third harmonic the planar undulator radiates.
`run_compare_sase.py` is slower than its seeded sibling: at 4 ranks the
averaged runs take ~20 s each and the unaveraged ones 3.4, 6.7 and 12.6 minutes
at `nodesPerLambdar` 12, 24 and 48.

**Two things have to be got right for this to be a comparison at all.**

*A common shot-noise realisation.* `iRandSeed` in `&MDATA` fixes it; the
default, -1, seeds from the system clock and every run is a fresh draw.
`init_random_seed` (`macroparticle_generation/random.f90`) folds
`tProcInfo_G%rank` into the seed and the `sElectronThreshold` cull is per rank,
so a fixed `iRandSeed` only reproduces a realisation at a fixed rank count -
`run_compare_sase.py` runs both modes at the same `--np` and
`compare_sase.noise_match` checks the initial macroparticle dumps before
anything else is reported. This is the SASE analogue of `compare3d.py`'s
"power ratio at the first write must be 1": with no seed the field starts at
zero in both runs, so what has to be identical is the beam.

The two dumps are not bit-identical even so, and neither reason is a noise
difference. The macroparticles are redistributed to follow each mode's own
field decomposition, so on more than one rank the gathered dumps hold the same
particles in a different order. And `shuntBeam`
(`macroparticle_generation/simple_electron_gen.f90`) slides the whole beam so
its leading macroparticle sits a tenth of a *field* cell above the mesh start -
a cell that differs between the modes by the mesh ratio, leaving the same
realisation translated by 0.0909 of a resonant wavelength for
`lambdarPerCell = 1` against `nodesPerLambdar = 12`. On a temporal mesh with no
seed nothing in the run picks out a z2 origin, so that is invisible to the
physics. `noise_match` therefore sorts on z2 and allows one common offset, and
requires everything else to agree to the last bit. It does on this deck, at 1
rank and at 4.

*Rounded bunch ends.* `sase_beam.in` keeps `qRndEj_G = .true.` with
`sSigEj_G = 0.1`, which rounds the 50-long flat top over a 10%-to-90% rise of
three resonant wavelengths. That is the point of it, not cosmetic: a sharp
current edge radiates coherent spontaneous emission, and the period-averaged
model discards CSE entirely, so with square ends the two solvers would not be
comparable. At `sSigEj_G = 0.1` the edge is already smooth on the carrier
scale - `k_r sigma ~ 10`, so CSE at the fundamental is suppressed by many
orders of magnitude - and `--sigej` scans it to confirm nothing measured here
depends on it.

**What is compared.** Not the fields node by node, which is what `compare.py`
does for the seeded deck and is the wrong expectation here: SASE amplifies by
seven orders of magnitude, so anything the two solvers disagree about is
amplified through the gain and the two fields become different shots with the
same statistics. `compare_sase.py` compares the statistics of the
amplification - power against zbar, the gain length fitted over the window
where the power is between 1e-4 and 1e-2 of saturation, the saturation power
and the zbar it first reaches 95% of it, the spectrum's centre and RMS width,
bunching, and the band-resolved field energy at each `/aperp` write. It also
reports the normalised overlap of the two envelopes, which measures directly
how far up the gain curve the two runs are still the same shot.

Spectra follow the W5 convention: unaveraged, the transform of the resolved
field, bin `m` at `w/wr = |4 pi rho f_m|`; averaged, the transform of the
envelope, whose bins are offsets about the carrier, `w/wr = 1 - 4 pi rho f_m`.
The band an envelope mesh can hold is `1 +/- 1/(2 lambdarPerCell)`, and the
`band` columns restrict both runs to it, which is the like-for-like power
comparison - total power counts the harmonics and the broadband spontaneous
emission the averaged mode is not carrying and never claimed to.

`sBeta = 0` in `sase.in`, but unlike the 3D deck that is not working around the
`NBZ2_G` node count: in 1D `qDiffraction = .false.`, and both the absorber and
`qFilter` live inside the diffraction step, so neither does anything either
way. One consequence worth knowing: the unaveraged run is unfiltered, and
carries its full resolved spectrum.

### What the SASE comparison showed (2026-10-09)

**Averaged and unaveraged agree, and the mesh-refinement series is what makes
that statement mean anything.** Planar, 340 periods to zbar 21.4 (about 24 gain
lengths), 4 ranks, one shot-noise realisation (`iRandSeed = 1`). The field
grows from zero to 1.7 in `sum(/power) * dz2`, seven orders of magnitude, and
saturates at zbar ~ 19.

| run | runtime | gain length, zbar | P_sat | zbar_sat | spectrum centre | RMS width |
| --- | --- | --- | --- | --- | --- | --- |
| averaged 1 λr/cell, 1 step | **17.1 s** | 0.8856 | 1.6890 | 18.85 | 0.995890 | 5.063e-3 |
| averaged 1 λr/cell, 2 steps | 22.6 s | 0.8858 | 1.6901 | 18.85 | 0.995900 | 5.070e-3 |
| unaveraged n = 12, 30 steps | 206 s | 0.9022 | 1.6791 | 19.10 | 0.996485 | 4.751e-3 |
| unaveraged n = 24, 60 steps | 402 s | 0.8874 | 1.7007 | 18.85 | 0.995990 | 5.024e-3 |
| unaveraged n = 48, 120 steps | 756 s | 0.8842 | 1.7089 | 18.85 | 0.995878 | 5.089e-3 |

Gain length is fitted over the window where the power is between 1e-4 and 1e-2
of saturation. Saturation position is the zbar at which the power first reaches
95% of its peak, because past saturation the power oscillates by a few per cent
and which write holds the maximum is noise.

Against the unaveraged solver's own default mesh the averaged gain length is
1.8% short, which compounds over thirteen gain lengths into an in-band energy
ratio of 1.39 - easily read as a model error. It is not. Refine the mesh and
every observable converges onto the averaged one:

| averaged against | gain length ratio | P_sat ratio | in-band ratio at saturation | peak bunching deviation |
| --- | --- | --- | --- | --- |
| n = 12, 30 steps | 0.9816 | 1.0059 | 0.9615 | +16.4% |
| n = 24, 60 steps | 0.9980 | 0.9931 | 0.9922 | +1.7% |
| n = 48, 120 steps | 1.0016 | 0.9883 | 0.9946 | -1.6% |

The unaveraged gain length runs 0.9022, 0.8874, 0.8842 against the averaged
0.8856; the differences fall 4.6x per halving, so Richardson puts the converged
value near 0.883 and the averaged mode's gain length about 0.3% long. The
spectral centre converges 0.996485, 0.995990, 0.995878 onto the averaged
0.995890, and the width 4.751e-3, 5.024e-3, 5.089e-3 onto 5.063e-3. The
bunching ratio brackets: 1.7% high against n = 24 and 1.6% low against n = 48.
A single-mesh comparison here would have charged the unaveraged solver's own
discretisation error to the averaged model, in every observable at once.

What is left at the finest mesh, and it is the honest residual: gain length
0.16% (~0.3% against the extrapolated limit), saturation power 1.2% low,
saturation position identical, in-band field energy 0.5% low at saturation and
within 2% through the whole exponential regime, spectral centre 1e-5 in w/wr,
spectral width 0.5% narrow, bunching within 1.6%. At 44x the speed of the mesh
the unaveraged solver needs to get there, and 12x its own default.

**The two runs stay the same shot.** This was the surprise, and it is why no
statistical argument was needed in the end. With identical shot noise the
normalised overlap of the averaged envelope with the demodulated unaveraged
field, against n = 48, is 0.9958 at zbar 1.3, 0.9990 by 3.8, and 0.9999 or
1.0000 at every write from zbar 7.5 to saturation, falling only to 0.9997 at
the last. The relative L2 with the global phase removed runs 0.09, 0.03, 0.014,
0.023 - one to two per cent through the exponential regime. SASE is chaotic and the
expectation going in was that the two solvers would decorrelate into two
different shots with the same statistics; they do not. The averaged solver
reproduces the unaveraged one spike by spike.

Read that L2 from `compare_sase.py` and not from a direct difference.
`shuntBeam` leaves the two beams 0.0909 of a wavelength apart, 33 degrees of
carrier phase and no physical difference, which alone puts 0.58 into an L2
whose real content is 0.09. `compare.py` can difference the seeded runs'
envelopes directly only because there the seed pins the phase; with no seed
nothing does.

**Early on, total power is meaningless, and how meaningless depends on the
unaveraged mesh.** The share of the unaveraged field inside the envelope band
at zbar 1.3 is 0.87 at n = 12, 0.70 at n = 24 and 0.41 at n = 48 - a finer mesh
resolves more of the broadband spontaneous emission, so the total-power ratio
there reads 0.91, 0.70, 0.41 while the in-band ratio reads 1.05, 1.005, 0.996.
The out-of-band share is the unaveraged mesh's resolved bandwidth, not a
property of the physics. By zbar 7.5 the fundamental has run away and the band
holds over 99% in every case. At saturation the third harmonic holds 0.93% of
the unaveraged n = 48 total, the fifth 0.02%, the even ones at leakage level -
the same odd-only pattern the seeded planar deck shows.

**An ensemble confirms the bias is far below the shot-to-shot spread.** Five
realisations, `iRandSeed` 1 to 5, averaged at 1 λr/cell and 1 step against
unaveraged n = 24:

| | gain length | P_sat | zbar_sat | spectrum centre | RMS width |
| --- | --- | --- | --- | --- | --- |
| unaveraged, mean | 0.8714 | 1.6302 | 18.45 | 0.995902 | 5.358e-3 |
| unaveraged, sd | 0.0122 | 0.1046 | 0.52 | 3.83e-4 | 4.40e-4 |
| averaged, mean | 0.8697 | 1.6185 | 18.35 | 0.995854 | 5.375e-3 |
| averaged, sd | 0.0121 | 0.1017 | 0.47 | 3.59e-4 | 4.35e-4 |

The shot-to-shot spread in gain length is 1.4%, seven times the 0.2% mean
difference between the modes, and the two modes' spreads agree to 1%. The
difference is nonetheless systematic and the same every shot - the averaged
gain length is 0.0013 to 0.0019 shorter, and the saturation power 0.6% to 1.0%
lower, in all five - which is what one wants from a paired comparison: a small
reproducible bias, not scatter.

**Coherent spontaneous emission is not the problem, and the averaged mode
carries more of it than expected.** `sSigEj_G` sets the Gaussian rounding of
the bunch ends; at 0.1 the 10%-to-90% rise is three resonant wavelengths and
CSE at the fundamental should be dead. Scanning it 40x, against unaveraged
n = 24:

| sSigEj_G | in-band energy at zbar 1.3, unavg | averaged | ratio | gain length ratio | P_sat ratio |
| --- | --- | --- | --- | --- | --- |
| 0.01 | 5.919e-5 | 5.947e-5 | 1.0047 | 0.9984 | 0.9922 |
| 0.1 | 1.879e-6 | 1.889e-6 | 1.0052 | 0.9980 | 0.9931 |
| 0.4 | 2.023e-6 | 2.020e-6 | 0.9985 | 0.9981 | 0.9934 |

Two findings. The discrepancy does not track `sSigEj_G` at all: the gain-length
and saturation-power ratios are flat to the fourth decimal across a 40x change
in the bunch-end length, so CSE contributes nothing to the residual above. And
sharpening the ends to `sSigEj_G = 0.01` raises the in-band startup energy 31x,
which *is* coherent spontaneous emission at the resonant wavelength - and the
averaged mode reproduces it to 0.5%. So the premise that "averaged mode
discards CSE entirely" is too strong: what a single envelope cannot carry is
the *out-of-band* part of the edge radiation, which at `sSigEj_G = 0.01` is 2%
of the total. The in-band coherent start is the fundamental bunching, and the
envelope equation has it. A square-ended bunch would still be a harder test
than this, and has not been run; what is shown is that over the range where the
unaveraged run's own CSE grows 31-fold, the two solvers track it together.

**One step per undulator period is converged**, as for the seeded deck: 1 and 2
steps per period agree to 2e-4 in gain length and 7e-4 in saturation power.

**Envelope cells can be coarser than the seeded deck could justify, but not by
as much as the gain length suggests.** Against unaveraged n = 24:

| averaged | gain length | ratio | P_sat ratio | in-band ratio at saturation | RMS width |
| --- | --- | --- | --- | --- | --- |
| 1 λr/cell | 0.8856 | 0.9980 | 0.9931 | 0.9922 | 5.063e-3 |
| 2 λr/cell | 0.8857 | 0.9981 | 0.9934 | 0.9905 | 5.054e-3 |
| 4 λr/cell | 0.8855 | 0.9979 | 0.9921 | 0.9794 | 4.992e-3 |
| 8 λr/cell | 0.8846 | 0.9969 | 0.9861 | 0.9418 | 4.788e-3 |

The gain length holds to 0.3% out to 8 wavelengths per cell, because SASE's
bandwidth is only a few rho - the measured RMS width is 5e-3 of the resonant
frequency, so even an 8 λr/cell band of +/-6.25% is twelve standard deviations
wide - and its longitudinal structure is coarse on the carrier scale, spikes of
order 200 wavelengths. But the saturated in-band field energy does not hold:
0.8% low at 1 λr/cell, 1.0% at 2, 2.1% at 4, 5.8% at 8. Nor does the spectral
width, which falls 5.063e-3, 5.054e-3, 4.992e-3, 4.788e-3 across the four -
0.5%, 0.7%, 1.9% and 5.9% below the converged unaveraged 5.089e-3. The loss is
past saturation, where the spectrum broadens and the envelope grows structure a
coarse mesh smooths away. So the seeded deck's caution that "a case with real
longitudinal structure will be far more sensitive" is not borne out for the
growth rate - the envelope mesh is not what limits the exponential regime of a
1D SASE run - but it is right about the saturated field. 1-2 λr/cell remains the
recommendation, 4 is tolerable before saturation, 8 is not.

### What this does not show

- **One rho, one polarisation, one band.** Everything above is planar at
  rho = 0.005. The ~1% planar remainder the seeded study is chasing (#130) is
  the same size as the residuals here, and nothing here separates them.
- **1D.** No diffraction, no focusing, no transverse structure, so none of the
  3D deck's concerns are retested.
- **Square bunch ends are still untested**, as is a short bunch. The
  `sSigEj_G` scan shows the residual does not track the edge length over 40x,
  not that an edge sharp on the carrier scale would be carried.
- **The unaveraged reference is itself still drifting.** At n = 48 the gain
  length is still moving by 0.4% per mesh halving, so the residuals quoted
  against it are limits approached, not converged values - the same caveat the
  seeded study's planar remainder carries.

## 3D

`run_compare3d.py` is the 3D counterpart of `run_compare.py`, on `deck3d.in` /
`beam_file3d.in` / `seed_file3d.in`:

```sh
python3 run_compare3d.py helical                 # defaults: --plain 12:30 24:60 --avg 1:2
python3 run_compare3d.py helical --plain 12:30 24:60 48:120
python3 run_compare3d.py 0.5,1 --plain 12:30 24:60 48:120     # elliptical, 'puffin' type
python3 run_compare3d.py 1,0 --seed 0.0,0.001                 # linear along y, y seed
python3 compare3d.py <plain_dir> <avg_dir>       # full tables
```

It compares mesh-independent observables - total power, current-weighted bunching, the
field's transverse RMS and profile, and the beam envelope - rather than differencing
fields node by node, because the two runs' z2 meshes differ by an order of magnitude.
The deck runs 120 periods to reach clear exponential growth (power x29), which means
the last write is beginning to roll over towards saturation; read the ratio partway up
the table as well as at the end, since averaged and unaveraged are expected to diverge
once saturation sets in. `sBeta = 0` there on purpose - see the comment in `deck3d.in`.

`compare3d.py` refuses to compare two runs that ended at different `zbar`: Puffin exits
with status 0 when it gives up rearranging its parallel field, so a run that stopped
early otherwise looks finished. It also reports the power ratio at the first write and
warns if it is not 1 - any deviation there means the two runs did not start from the
same field, and nothing after it is a comparison of the physics.

That used to happen whenever the seed's polarisation did not match the undulator, when
averaged mode carried a single envelope and so only the resonant helicity. It now carries
both Cartesian envelopes and lays any seed polarisation exactly. `run_compare3d.py` still
defaults to the polarisation that gains (the resonant helicity for helical and `UX,UY`
undulators, linear along x otherwise), since a component the undulator does not drive
only propagates; `--seed AX,AY` overrides it.

Note neither `compare3d.py` nor the tables below filter power to the fundamental band, as
the 1D `compare.py`'s `demod` does. For a planar undulator that makes total power a
like-for-unlike comparison, since the unaveraged run radiates harmonics the averaged mode
cannot carry; the 3D deck is helical, which has no harmonic content. Run planar or
elliptical, compare bunching as well as power.

### What the 3D comparison showed (2026-09-12)

Helical, averaged at `lambdarPerCell = 1` and 2 steps per period, on 4 ranks:

| unaveraged | runtime | P ratio | bunching ratio | field sigma_x | beam sigma_x |
| --- | --- | --- | --- | --- | --- |
| n = 12, 30 steps | 245 s | 1.0698 | 0.9743 | 1.0055 | 1.0007 |
| n = 24, 60 steps | 613 s | 0.9976 | 0.9904 | 1.0030 | 1.0007 |
| n = 48, 120 steps | 1795 s | 0.9830 | 0.9947 | 1.0022 | 1.0007 |
| averaged | 14.2 s | | | | |

As in 1D, the gap at the unaveraged solver's default mesh is the unaveraged solver's:
bunching converges monotonically towards 1 as its mesh refines. The transverse
observables hardly move - the beam envelope is 1.0007 at every mesh, and the field's
transverse profile agrees to an L2 of 7e-3 in x, 1.3e-2 in y.

Against n48, the power ratio holds 0.999 through the exponential phase and only drifts
to 0.983 past zbar ~ 4.5 as the run rolls over towards saturation, which is why the
series table matters as much as the final row.

These runs predate the two-envelope coupling, but a helical undulator is bit-identical
under it (W3b in `AVERAGED_MODE_ROADMAP.md`), so they stand.

### Other polarisations in 3D (2026-10-25)

After averaged mode gained independent x and y envelopes, the same deck on 4 ranks, four
cases at once on an 18-core laptop. Averaged runs take 8 s; unaveraged 63, 177 and 420 s
at n12, n24 and n48. Ratios are averaged / unaveraged at the last write:

| undulator (ux, uy) | P ratio n12 / n24 / n48 | bunching at n48 | field sigma_x, y at n48 | beam sigma_x, y at n48 |
| --- | --- | --- | --- | --- |
| elliptical (0.5, 1) | 1.1080 / 1.0052 / 0.9836 | 0.9933 | 1.0055, 1.0067 | 1.0006, 1.0013 |
| elliptical (1, 0.6) | 1.0820 / 0.9880 / 0.9686 | 0.9969 | 0.9977, 1.0027 | 1.0007, 1.0019 |
| planar, `planepole` (0, 1) | 1.1152 / 0.9983 / 0.9709 | 0.9932 | 1.0077, 1.0068 | 1.0005, 1.0006 |
| linear along y (1, 0) | 1.0643 / 0.9541 / 0.9290 | 0.9831 | 1.0000, 1.0034 | 1.0006, 1.0017 |

`'puffin'` at (0, 1) gives exactly the `planepole` numbers here, so the planar row is also
the `'puffin'` type's own.

The elliptical and planar cases follow the helical pattern: the power gap at n12 is the
unaveraged solver's and closes as it refines, bunching is within 0.7% at n48, and the
transverse sizes within 0.8%. Through the exponential phase against n48 the power ratio
is 0.995 at zbar 3.8 for (0.5, 1) and 0.996 for (1, 0.6), and drifts down as the runs roll
over. Planar's falls from 1.000 to 0.977 already by zbar 3.8, before there is much gain:
that is the unaveraged run's harmonic radiation, which `/power` here does not filter out.
Its bunching ratio, 0.997 at the same point, is the cleaner measure.

**Linear along y is the outlier, and the evidence puts it in the unaveraged solver.** It is
the mirror of planar, and the two solvers' mean transverse momenta show where they part.
For planar (x wiggle) the slow `<p_x>` and `<p_y>` agree between solvers to about 1e-5
throughout.
For linear along y (y wiggle) `<p_x>` agrees just as well, but the unaveraged `<p_y>` drifts
steadily, from -3.72e-3 to -2.86e-3 over the run, while the averaged one stays at -3.75e-3.
A pure `b_x` undulator has no y dependence and should put no net force on the centroid in
y, which is what the averaged run shows; the unaveraged one keeps its centroid nearer the
axis (`<y>` -0.027 against -0.043 at the end) and so overlaps the field better. Together with
the 2% x/y asymmetry the unaveraged solver shows in 1D, where averaged mode gives planar and
linear-along-y identical power, this points at the unaveraged y-wiggle path. It is not
traced to a cause.


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

**Correction (2026-10-25): the 3rd and 5th harmonic rows above are mostly a
beam-sampling artefact.** `deck.in`'s beam has 4 macroparticles per wavelength,
and a beam laid that way aliases harmonic h onto h mod 4: the 3rd reads the
conjugate of the fundamental's bunching and the 5th the fundamental's own,
from the first step. Rerun at 16 per wavelength (converged: 32 changes the
band powers by under 0.3%), the 3rd harmonic is 0.09% of the fundamental's
power on this deck, not 1.7%, and the 5th 0.0002%, not 0.3%. The argument above
only shows the 4th has no coupling to alias into. So the "3rd + 5th harmonic"
column below is ~20x too large, and the conclusion drawn from it - that the
planar remainder is not harmonic - is right for a stronger reason: there is
almost no harmonic power to account for. See "Harmonic bands" at the end.

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


## Harmonic bands (2026-10-25)

`avgHarmonics` lets the averaged mode carry odd harmonics as extra bands
(`averaging.f90`, and the Harmonics subsection of the manual's Period-Averaged
Mode section). `compare.harmonic_power` compares them with the unaveraged field
demodulated at `exp(+i h z2/2rho)` over one fundamental wavelength, which nulls
every other harmonic, at every `/aperp` write the two runs share:

```sh
python3 run_compare.py planepole --mpw 16 --every 5 --avg 1:2 1:2:3,5 --plain 24:60 48:120 96:240
```

All runs are the seeded 60-period deck, planar, `aw = 1.012`, Release build, 1
rank, with `iMPsZ2PerWave = 16`, more than twice the 5th harmonic (see the
correction above; the averaged run warns below that). Averaged at
`lambdarPerCell = 1`, 2 steps per period. Mid-bunch band power at the last
write, zbar 3.77, as averaged / unaveraged:

| unaveraged nodesPerLambdar | 24 | 48 | 96 | 192 |
| --- | --- | --- | --- | --- |
| total `/power`, fundamental only | 1.0030 | 0.9857 | 0.9817 | 0.9807 |
| total `/power`, with bands 3, 5 | 1.0039 | 0.9866 | 0.9826 | 0.9816 |
| h = 3 | 1.1450 | 1.0284 | 1.0015 | 0.9952 |
| h = 5 | 1.0305 | 0.9184 | 0.8513 | 0.8381 |
| unaveraged h = 5 power | 1.345e-7 | 1.509e-7 | 1.628e-7 | 1.653e-7 |

(The fundamental-only row at n96 and n192 is the with-bands ratio divided by
1.0009: the bands change the averaged fundamental by -6e-6, so the two averaged
runs differ in total power by exactly the harmonic power.) Averaged h = 5
power is 1.386e-7 throughout.

**The 3rd harmonic agrees.** The unaveraged solver converges onto the averaged
answer, 14.5% high at n24 and 0.5% at n192, while the averaged side is already
converged: 4 steps per period changes it in the fifth digit, and
`lambdarPerCell = 0.5` by 0.15%.

**The 5th harmonic does not: it is 16% low against a converged reference.**
The unaveraged h = 5 power moves 1.5% between n96 and n192, so the reference is
settled to a few per cent, and the averaged one moves 0.4% at
`lambdarPerCell = 0.5`, not at all at 4 steps per period, and 0.2% at 32
macroparticles per wavelength. The gap is not there from the start: it opens as
the field grows, 0.93 at zbar 2.2 (n192) falling steadily to 0.84 at the end.

What is known about the cause:

- *The dropped A-term is about a fifth of it.* With the radiation-driven
  quiver (`eta p2 A / kappa^2` in `dppdz`) zeroed in the unaveraged solver, the
  h = 5 ratio at n96 is 0.877 instead of 0.851, and h = 3 1.016 instead of
  1.002; the fundamental moves from 0.983 to 0.989, as before.
- *Late on the harmonics are nonlinear, so they inherit the fundamental's
  error with a lever.* Between those two runs ln(P_5) moved 4.9 times as far as
  ln(P_1), and ln(P_3) 2.4 times, which is what harmonic generation from
  fundamental bunching (P_h ~ P_1^h) predicts. The averaged fundamental is
  1.8% low at n192 - the planar remainder plus the A-term, documented above -
  and with those levers that alone is worth about 9% on h = 5 and 4% on
  h = 3. Taken out, h = 5 would be ~8% low and h = 3 ~4% high, so h = 3
  agreeing to 0.5% is partly a cancellation, not only a property of the band.
  The levers come from one pair of runs and are an estimate.
- *Not the single-particle coupling.* The brute-force tests in
  `test/testAveraging.pf` check q^(3) and q^(5) against an electron's exact
  orbit to ~1e-4 of the band's own coupling, for every polarisation.

What is left once the fundamental's lever is taken out, ~8% low on the 5th
harmonic and ~4% high on the 3rd, is not identified. Candidates not yet tested are terms of second
order in the figure-of-eight, which are h^2 larger for the 5th than the
fundamental, and coupling of the fundamental field to the 5th harmonic through
the even-harmonic bunching that the averaged model drops along with the
A-term.

At the default 4 macroparticles per wavelength the comparison is with the
aliased harmonics, and the two solvers alias the same way: at n48 the
unaveraged 3rd harmonic is 1.73% of the fundamental and the 5th 0.28%, and
the averaged bands reproduce them to 3.2% and 12%. The averaged run warns that
its beam is too coarse for the bands it was asked for.

**What the bands do to the fundamental comparison: almost nothing, because
there is almost nothing there.** Total `/power` moves 0.09% towards the
unaveraged solver (0.9857 to 0.9866 at n48). The ~1.8% planar gap is not
harmonic content, and with the bands carried it is still there.

Elliptical, `(ux, uy) = (0.5, 1)` with a circular seed, n48 only: total
`/power` 0.9902 without bands and 0.9921 with; h = 3 1.059 and h = 5 0.835 at
the last write, the same pattern as planar at the same mesh; the y fraction of
the fundamental 0.2359 unaveraged, 0.2372 averaged, unchanged by the bands. The
3rd harmonic is 0.18% of the fundamental there.

Cost: the bands' gather, deposit and Bessel functions make each about as
expensive as the fundamental. With output off, the averaged integration takes
5.6 s fundamental-only and 16.3 s with bands 3 and 5; with this harness's
integrated write every period, 33 s and 45 s. The unaveraged runs took 66, 106,
218 and 364 s at n24 to n192 (some were sharing the machine with each other).

