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

## Lattices: detuned modules and tapers

`run_compare_latt.py` is `run_compare.py` with a lattice, a taper and a second
seed:

```sh
python3 run_compare_latt.py planepole --latt "UN 60 1.0 0.0; DR 2; UN 60 f0.9 0.0"
python3 run_compare_latt.py planepole --latt "UN 60 f0.75 0.0" --seed 0,0 \
        --seed2 0.75:0.01,0.0 --z2len 72 --avg 1:2 0.5:2 0.25:2 0.125:2
python3 run_compare_latt.py planepole --latt "UN 105 1.0 0.0; UN 150 1.0 -0.02" --z2len 80
```

An undulator module's `ALPHA` can be given as `fF`, the frequency its
reference-energy electron is resonant with relative to the carrier, and alpha is
solved for (`getAvgResonanceShift` in `averaging.f90` is the same relation).
The undulator period is fixed, so a module is detuned through alpha only:
`f1.1` is alpha 0.855 and `f0.6` is alpha 1.723 on the planar deck. Power, bunching
and mean energy are compared write by write on the flat top, and the field at
the last write is Fourier transformed over the same region for the spectral
centre and, with `--seed2`, the power in each colour. `compare.py`'s
one-wavelength demodulation is not used, since it attenuates content at a
frequency offset df by sinc(df).

A red-detuned module slips the beam 1/f wavelengths per period, so long or
strongly red lattices need `--z2len` above the deck's 60, or the run stops with
"Z2 coord is too large". Everything below is 1D, seeded, noise-free, rho =
0.005, averaged at 2 steps per period, against the unaveraged solver at
`nodesPerLambdar` 24, 48 and 96. Raw tables were kept out of the repository.

### What it showed (2026-10-26)

**A detuned module with only the carrier seeded is modelled as well as an
undetuned one.** Module 1 is alpha = 1 for 60 periods, then a 2-period drift,
then 60 periods detuned to f. With no seed or noise at the module's own
resonance, it acts on the carrier field off resonance, which is exactly
where the averaged mesh holds content. Power ratio avg/unavg at the end of the
lattice (zbar 7.67), `lambdarPerCell` 1, against n = 96:

| f | 0.6 | 0.9 | 0.97 | 0.995 | 1 (none) | 1.005 | 1.03 | 1.1 |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| planar | 0.965 | 0.955 | 0.980 | 0.985 | 0.988 | 1.001 | 0.987 | 0.974 |
| helical | | 0.993 | 1.002 | | 0.978 | | | 0.993 |

The planar row was measured before the Bessel-factor fix described below. Rerun
with it, f = 0.6, 0.9 and 1.1 give 0.965, 0.958 and 0.975. Helical is not
affected by the fix.

The planar end of module 1 is 0.960-0.962 on every row, the known planar
remainder at zbar 3.8. Planar bunching agrees to 1.1% except at f = 0.6 (+6%),
and helical to 1.1% except at f = 0.9 and 1.1 (-3.6%, +3.5%), all three where it
has fallen to 0.07-0.11. Mean energy agrees to 5e-5 and the spectral centre to
1e-5. n = 48 to 96 moves the ratios by 0.2-0.4%. Helical shows nothing that
grows with detuning. Planar f = 0.9 is 3% below the undetuned row, which is the
one planar entry here that may be detuning-dependent; it is not explained.

**Radiation at a detuned module's own resonance is where the band matters, and
it exposed a second, larger error in the planar coupling, now fixed.** One module detuned to f and
seeded only at f (`--seed 0,0 --seed2 f:...`), the cleanest test of the band.
Power ratio avg/unavg at the module end against n = 48, which agrees with n = 24
to ~1%:

| f | 1.4 | 1.25 | 1.1 | 0.9 | 0.75 | 0.6 |
| --- | --- | --- | --- | --- | --- | --- |
| helical, `lambdarPerCell` 1 | 0.249 | 0.504 | 0.894 | 0.903 | 0.568 | 0.249 |
| 0.5 | 0.663 | 0.843 | 0.972 | 0.972 | 0.884 | 0.825 |
| 0.25 | 0.904 | 0.960 | 0.992 | 0.988 | 0.960 | 0.942 |
| 0.125 | 0.973 | 0.984 | 0.990 | 0.980 | 0.955 | 0.915 |
| planar, `lambdarPerCell` 1 | 0.793 | 0.610 | 0.883 | 0.871 | 0.448 | 0.126 |
| 0.5 | 0.895 | 0.865 | 0.962 | 0.956 | 0.810 | 0.598 |
| 0.25 | 0.962 | 0.957 | 0.984 | 0.979 | 0.937 | 0.869 |
| 0.125 | 0.982 | 0.979 | 0.984 | 0.983 | 0.967 | 0.950 |
| planar before the fix, 1 | 0.794 | 0.621 | 0.905 | 0.822 | 0.354 | 0.070 |
| 0.5 | 0.898 | 0.886 | 0.986 | 0.902 | 0.636 | 0.307 |
| 0.25 | 0.967 | 0.982 | 1.008 | 0.923 | 0.736 | 0.446 |
| 0.125 | 0.987 | 1.004 | 1.009 | 0.927 | 0.758 | 0.488 |
| elliptical (0.5, 1), 0.125 | | 0.983 | | 0.984 | 0.966 | 0.939 |
| before the fix, 0.125 | | 0.995 | | 0.968 | 0.912 | 0.840 |

Helical is the band and nothing else. Linear deposition and linear
interpolation each scale a mode at delta = |f - 1| `lambdarPerCell` cycles per
cell by sinc^2(pi delta), so the growth rate goes as sinc^(4/3) and the error
grows with the module's gain: at the band edge it is large long before the
nominal +/- 1/(2 `lambdarPerCell`) is reached. f = 0.6 at 1 wavelength per cell
is 0.4 of the way to the Nyquist edge, and the module amplifies a quarter as
much as it should. Refining the cells takes every row to within a few per cent.
The helical red rows fall again slightly at 0.125, where the beam crosses ~7
cells per step; that was not chased.

Before the fix, planar converged on the blue side and **stalled on the red
side**: f = 0.75 sat at 0.74-0.76 and f = 0.6 at 0.45-0.49 from 0.25
wavelengths per cell down. Neither the step nor the cell size was the cause: 8
steps per period, or 0.125 wavelengths per cell, moved the two-colour f = 0.75
case from 0.717 to 0.724 and 0.699. The cause was the figure-of-eight Bessel
factor. `xi` was evaluated against the carrier, but the ripple is in z2, so
radiation at frequency f sees it as a phase ripple f times larger, and its
coupling needs JJ(f xi). xi grows with alpha^2, so the error was large only on
the red (alpha > 1) side, where JJ(xi)/JJ(f xi) is 0.985, 0.938 and 0.838 at
f = 0.9, 0.75 and 0.6. On the blue side it over-coupled by under 1%, which is
why the blue rows sat 1-2% above their helical counterparts.

The fix (`avgJJ` in `averaging.f90`) evaluates xi per macroparticle at the
frequency that particle is resonant with, `xi_j / p2_j`. That is the content
it exchanges energy with on the slow scale. It is exact for that content, and
it leaves trapped electrons in a taper, which stay at p2 = 1, on the carrier
value. Carrier radiation seen by an off-resonant electron now gets JJ(xi/p2)
where JJ(xi) would be exact. That coupling is off resonance and weak, and the
single-seed detuned rows above move by under 0.3%. A scratch build that scaled
`xiCoef` by the module's resonant frequency instead gave the same red-side
numbers, to 0.5%. It was rejected because it would give the trapped electrons
of a tapered module the module's frequency, not the carrier's. With the fix,
planar converges as helical does, and elliptical, whose xi is smaller but
nonzero, is recovered the same way. `testDetunedMatchesBruteForce` in
`test/testAveraging.pf` pins it against a brute-force period average of a
detuned planar, elliptical and linear-y quiver.

On resonance the fix is O(rho), and it moves the resonant comparisons by about
0.1%, towards the unaveraged solver:

| | before | after | unaveraged reference |
| --- | --- | --- | --- |
| planar, P ratio at zbar 3.77 | 0.9624 | 0.9639 | n = 96 |
| elliptical (0.5, 1), P ratio at zbar 3.77 | 0.9835 | 0.9841 | n = 96 |
| SASE planar, gain length | 0.8858 | 0.8855 | 0.8874 at n = 24 |
| SASE planar, saturation power | 1.6901 | 1.6916 | 1.7007 at n = 24 |

The averaged planar and elliptical e2e goldens moved, by up to 2e-4 relative in
the field and 4e-4 in power, through the undulator end ramps, where alpha and so
p2 leave 1. They were regenerated. Helical and every unaveraged golden are
unchanged.

**Two colours in one lattice** - module 1 at the carrier, module 2 detuned to f,
seeds at both. Second-colour power avg/unavg at the end, against n = 48,
planar:

| f | 1.1 | 0.97 | 0.9 | 0.75 | 0.6 |
| --- | --- | --- | --- | --- | --- |
| `lambdarPerCell` 1, before the fix | 0.927 | 0.982 | 0.824 | 0.348 | 0.070 |
| 0.5, before | 0.996 | 0.997 | 0.910 | 0.626 | 0.281 |
| 0.25, before | 1.002 | 0.985 | 0.925 | 0.717 | 0.413 |
| 1, with the fix | | | 0.871 | 0.438 | 0.117 |
| 0.25, with the fix | | | 0.978 | 0.920 | 0.849 |

The carrier colour agrees to 2-3% in every case. The helical version of this
lattice is a poor test, because module 1 saturates the beam enough that module 2
barely amplifies the second colour: it carries 1-7% of the power, and its ratio
is not resolvable from the carrier's.

**Tapers: power and mean energy agree, and the bunching of detrapped electrons
is what goes.** Planar, untapered to zbar 5.65 (gentle) or 6.6 (saturation),
then 150 periods tapered at d(alpha)/d(zbar) as below: 240 periods (zbar 15.1)
for the gentle taper, 255 (zbar 16.0) for the rest. `--z2len 80`. Against
n = 96, `lambdarPerCell` 1:

| taper from | dalpha/dzbar | alpha at end | P ratio, range over the taper | P ratio at end | b ratio at end | <gamma> diff at end | detrapped phase advance per period |
| --- | --- | --- | --- | --- | --- | --- | --- |
| none | 0 | 1 | 0.92-1.00 | 0.966 | 0.990 | 4e-5 | median 0.03 rad |
| 5.65 | -0.005 | 0.953 | 0.976-0.992 | 0.986 | 0.995 | 1.8e-4 | median 0.04 rad |
| 6.6 | -0.02 | 0.812 | 0.983-0.997 | 0.987 | 0.945 | 1.9e-4 | 0.6 rad, 76% of the beam |
| 6.6 | -0.05 | 0.529 | 0.984-0.998 | 0.996 | 0.830 | 3.5e-5 | 1.46 rad, all of it |

These are before the Bessel-factor fix. With it, the end ratios are 0.962,
0.986, 0.983 and 0.995 for power and 0.989, 0.994, 0.954 and 0.825 for
bunching: changes of at most 0.4% in power and 0.9% in bunching.

The phase advance is 2 pi (1 - p2_j) per period at the last averaged dump. The
gentle taper keeps the beam trapped, it gives ~3x the untapered saturation
power, and the
two modes agree as well as they do untapered. The strong taper detraps three
quarters of the beam, and the very strong one all of it, so the field stops
growing: power is then easy to get right, since the detrapped electrons barely
exchange energy. Their bunching is not. At 0.6 rad per period, a tenth of a
cycle, it is 5% low averaged; at 1.46 rad, a quarter of a cycle, it is 17% low,
and the unaveraged bunching is converging (n = 24, 48 and 96 agree to 4%).
That fits the averaging assumption failing for those electrons: the period
average treats the ponderomotive phase as fixed over a period, and theirs moves
a noticeable fraction of 2 pi. Power is not where a strong taper shows it. The
untapered run's post-saturation synchrotron oscillation drifts to 0.92-0.97 at
the deepest minima, as the 1D seeded study found before.

None of these tapers moves a reference-energy electron's resonance outside the
band (the very strong one reaches 1.32 of the carrier), so none of them sets off
the setup warning. In a taper the trapped electrons keep radiating at the
carrier, and only the detrapped ones go off resonance.

### What this does not show

- **1D, seeded, one rho.** No diffraction, which is frequency dependent and
  which the averaged mode offsets about the carrier, and no SASE in a detuned
  module.
- **The planar f = 0.9 single-seed gap**, the 2-5% left in the red rows at
  fine cells (helical, and planar and elliptical after the fix), and the
  bunching gap under a strong taper are measured, not explained.
- **The Bessel-factor fix is right for the content each electron is resonant
  with.** A field holding two well-separated colours, both strong, gives each
  electron the factor for its own colour only. The two-colour rows above are
  the test of that, and they converge with the cells as the single-colour ones
  do.

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
