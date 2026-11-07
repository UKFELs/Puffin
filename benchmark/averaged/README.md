# Averaged vs unaveraged

> **The averaged mode is experimental.** The comparisons below are the cases it
> has been validated on; outside them, confirm its results against an
> unaveraged run. Its inputs, outputs and defaults may change between versions.


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
factor. `xi` was evaluated against the carrier, but the jitter is in z2, so
radiation at frequency f sees it as a phase jitter f times larger, and its
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

### Figures

`figures/latt_*.png` overlay the two solvers for one case from each of the
first, second and taper studies above: peak power, pulse energy, mean beam
energy and bunching along the undulator, and at the end the power profile, the
spectrum and the longitudinal phase space. Each figure states its lattice, seed
and resolutions, and gives the `run_compare_latt.py` command at its foot.
`plot_compare_latt.py` draws one figure from a pair of run directories, and
`figures/make_figures.sh` redraws all three. The averaged runs in them were
made after the Bessel-factor fix.

Two things they show that the tables do not:

- **An image at -f.** At fine cells the averaged field picks up a mirror image
  of off-carrier radiation at -f: 9.5% of the field energy in the f = 0.75
  figure at 0.125 wavelengths per cell. The image is absent from the seed and
  builds up during the run. It also appears in helical runs, and it is
  2-11% at 0.125 wavelengths per cell and up to ~4.5% at 0.25 across the
  own-resonance runs above. No real field has content there, so it inflates
  the averaged `/power` (1.068 of unaveraged in that figure, against 0.967 in
  the 0.75 colour). The tables measure the colour from the spectrum and so
  exclude it. Its cause has not been traced.
- **Narrower spectral wings.** In the f = 0.9 figure, below ~1e-3 of the
  peak, the unaveraged spectrum has wings out to +/-2.5% that the averaged one
  lacks. This has not been investigated.

`figures/latt3d_*.png` are the same three cases in 3D, on `deck3d.in`
(diffraction, a matched beam, Ipk = 4 kA), made with `run_compare_latt.py --3d`.
Each beam is about three slippage lengths long so that the middle 30% is in
steady state, as in 1D. The 3D beam saturates later (~150 periods), so the 3D
taper case runs 150 periods untapered and then 100 tapered at the same rate;
its x mesh is widened (`--set`), because a planar undulator gives no focusing
in x and the beam outgrows the standard mesh by ~170 periods. Averaged /
unaveraged at the end:

| case | unaveraged | peak power | pulse energy | bunching |
| --- | --- | --- | --- | --- |
| 3D A, f = 0.9, carrier seeded | n48 | 0.951 | 0.951 | 0.995 |
| 3D B, f = 0.75 own resonance | n48 | 1.107 (0.75 colour ~1.01) | 1.106 | 1.024 |
| 3D C, taper -0.05 after saturation | n24 | 0.994 | 0.999 | 1.008, at a level of ~0.01 |

They repeat the 1D picture: the -f image appears in 3D too (8.7% of the field
energy in 3D B), and the 0.75 colour alone agrees to ~1%. Unlike 1D, the 3D
taper first extracts ~1.7x more power before the beam detraps; both solvers
follow that to 0.6%.

**Solver time.** Each figure's legend gives Puffin's solver time (start-up
excluded) and MPI ranks for both runs, same ranks for both:

| case | ranks | unaveraged | averaged | speed-up |
| --- | --- | --- | --- | --- |
| 1D A (n96 / 1 λr per cell) | 1 | 243 s | 6.1 s | 40x |
| 1D B (n48 / 0.125) | 1 | 58 s | 2.8 s | 21x |
| 1D C (n96 / 1) | 1 | 510 s | 13.0 s | 39x |
| 3D A (n48 / 1) | 4 | 2052 s | 49 s | 42x |
| 3D B (n48 / 0.125) | 4 | 596 s | 114 s | 5x |
| 3D C (n24 / 1) | 8 | 2537 s | 117 s | 22x |

The 1D runs were timed one at a time, the 3D ones alongside each other, on an
18-core Apple M5 Pro carrying other load; treat them as +/-30%. The speed-up
falls with the cells per wavelength the averaged run needs: a colour 25% off
the carrier needs 0.125 wavelengths per cell, 8 cells per wavelength, which
in 3D, where each cell carries a transverse mesh, leaves only 5x over n48.
Against n24 the 3D A speed-up is 14x (707 s).

**Helical.** `figures/hel_*.png` and `figures/hel3d_*.png` repeat all six with a
helical undulator, which radiates no on-axis harmonics, so the unaveraged field
holds nothing the averaged mode cannot carry. The seed is the amplified
helicity. Helical couples more strongly and saturates sooner (~75 periods in
1D, ~105 in 3D), so C runs untapered to there and then applies the same taper
as its planar counterpart. 3D C keeps the standard mesh (focusing in both
planes) and is at n48. Averaged / unaveraged at the end, with planar alongside:

| case | peak power | pulse energy | bunching | speed-up | planar peak power |
| --- | --- | --- | --- | --- | --- |
| 1D A | 0.992 | 0.993 | 0.952 | 52x | 0.956 |
| 1D B | 1.068 (0.75 colour 0.955) | 1.067 | 1.000 | 23x | 1.070 (0.967) |
| 1D C | 0.996 | 0.996 | 0.999 | 38x | 0.998 |
| 3D A | 0.983 | 0.993 | 0.994 | 53x | 0.951 |
| 3D B | 1.182 (0.75 colour 1.055) | 1.178 | 1.038 | 9x | 1.107 (1.01) |
| 3D C | 0.999 | 1.008 | 1.24, at a level of ~0.015 | 53x | 0.994 |

Without harmonics, A closes from 4-5% to under 2% in both 1D and 3D: most of the
planar A gap was harmonic power the averaged mode does not carry. B does not
change that way. The -f image is there at the same size (8.8% of the
averaged field energy in 1D, 7.9% in 3D), and the 0.75 colour is 0.955 in 1D,
close to planar's 0.967, so that shortfall is not harmonic either. 3D helical B
is the case that agrees least well, 5.5% high in the colour, and it is not
traced. The helical runs ran alongside each other (1D one at a time, 3D three
at once), so the same +/-30% applies to the speed-ups.

### What this does not show

- **Seeded, one rho, mostly 1D.** Only the three 3D figures include
  diffraction, which is frequency dependent and which the averaged mode
  offsets about the carrier, and there is no SASE in a detuned module.
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
python3 run_compare.py planepole --mpw 16 --every 5 --avg 1:2 1:2:3,5 --plain 24:60 48:120 96:240 192:480
```

All runs are the seeded 60-period deck, planar, `aw = 1.012`, Release build, 1
rank, with `iMPsZ2PerWave = 16`, more than twice the 5th harmonic (see the
correction above; the averaged run warns below that). Averaged at
`lambdarPerCell = 1`, 2 steps per period. Mid-bunch band power at the last
write, zbar 3.77, as averaged / unaveraged. This table was rerun after #149,
which takes the Bessel factor at each electron's own resonance; that moved
the averaged band powers by under 0.5% and the fundamental's total power up
by 0.14%. The supporting runs further down (step and cell-size convergence,
32 macroparticles per wavelength, the A-term, 4 per wavelength, elliptical)
were measured before #149 and have not been repeated:

| unaveraged nodesPerLambdar | 24 | 48 | 96 | 192 |
| --- | --- | --- | --- | --- |
| total `/power`, fundamental only | 1.0044 | 0.9871 | 0.9832 | 0.9821 |
| total `/power`, with bands 3, 5 | 1.0054 | 0.9880 | 0.9840 | 0.9830 |
| h = 3 | 1.1433 | 1.0269 | 1.0001 | 0.9937 |
| h = 5 | 1.0256 | 0.9140 | 0.8472 | 0.8341 |
| unaveraged h = 5 power | 1.345e-7 | 1.509e-7 | 1.628e-7 | 1.653e-7 |

(The bands change the averaged fundamental by -6e-6, so the two averaged runs
differ in total power by the harmonic power, 0.09%.) Averaged h = 5 power is
1.379e-7 throughout.

**The 3rd harmonic agrees.** The unaveraged solver converges onto the averaged
answer, 14% high at n24 and 0.6% at n192, while the averaged side is already
converged: 4 steps per period changes it in the fifth digit, and
`lambdarPerCell = 0.5` by 0.15%.

**The 5th harmonic does not: it is 17% low against a converged reference.**
The unaveraged h = 5 power moves 1.5% between n96 and n192, so the reference is
settled to a few per cent, and the averaged one moves 0.4% at
`lambdarPerCell = 0.5`, not at all at 4 steps per period, and 0.2% at 32
macroparticles per wavelength. The gap is not there from the start: it opens as
the field grows, 0.92 at zbar 2.5 (n192) falling steadily to 0.83 at the end.

What is known about the cause:

- *The dropped A-term is about a fifth of it.* With the radiation-driven
  quiver (`eta p2 A / kappa^2` in `dppdz`) zeroed in the unaveraged solver, the
  h = 5 ratio at n96 is 0.877 instead of 0.851, and h = 3 1.016 instead of
  1.002; the fundamental moves from 0.983 to 0.989, as before.
- *Late on the harmonics are nonlinear, so they inherit the fundamental's
  error with a lever.* Between those two runs ln(P_5) moved 4.9 times as far as
  ln(P_1), and ln(P_3) 2.4 times, which is what harmonic generation from
  fundamental bunching (P_h ~ P_1^h) predicts. The averaged fundamental is
  1.7% low at n192 - the planar remainder plus the A-term, documented above -
  and with those levers that alone is worth about 9% on h = 5 and 4% on
  h = 3. Taken out, h = 5 would be ~9% low and h = 3 ~4% high, so h = 3
  agreeing to 0.6% is partly a cancellation, not only a property of the band.
  The levers come from one pair of runs and are an estimate.
- *Not the single-particle coupling.* The brute-force tests in
  `test/testAveraging.pf` check q^(3) and q^(5) against an electron's exact
  orbit to ~1e-4 of the band's own coupling, for every polarisation.

What is left once the fundamental's lever is taken out, ~9% low on the 5th
harmonic and ~4% high on the 3rd, is not identified. Candidates not yet tested
are terms of second order in the figure-of-eight, which are h^2 larger for the 5th than the
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
unaveraged solver (0.9871 to 0.9880 at n48). The ~1.7% planar gap is not
harmonic content, and with the bands carried it is still there.

Elliptical, `(ux, uy) = (0.5, 1)` with a circular seed, n48 only, rerun after
#149: total `/power` 0.9908 without bands and 0.9926 with; h = 3 1.054 and
h = 5 0.827 at the last write, both further off than planar at the same mesh
(1.027 and 0.914). Before #149 the y fraction of the fundamental was 0.2359
unaveraged and 0.2372 averaged, unchanged by the bands. The 3rd harmonic is
0.18% of the fundamental there.

Cost: the bands' gather, deposit and Bessel functions make each about as
expensive as the fundamental. With output off, the averaged integration takes
5.6 s fundamental-only and 16.3 s with bands 3 and 5; with this harness's
integrated write every period, 33 s and 44 s. The unaveraged runs took 64,
100, 169 and 315 s at n24 to n192.


### The bands on #149's lattice cases (2026-11-07)

`run_compare_latt.py` takes the bands too: an `--avg` run's third field is its
`avgHarmonics` (`--avg 1:2:3,5`), `--mpw N` sets `iMPsZ2PerWave`, and
`--fieldevery K` writes `/aperp` every K periods for the harmonic table and
the harmonic panels of `plot_compare_latt.py --harmonics 3,5`. All six planar
lattice cases of #149 (A, B, C in 1D and 3D, above) were rerun with
`avgHarmonics = 3, 5`.

**At #149's own settings the bands appear to change the planar results, but
that is beam-sampling aliasing.** Against #149's unaveraged runs, averaged /
unaveraged at the end, peak power, pulse energy and bunching, without bands
-> with:

| case | peak power | pulse energy | bunching |
| --- | --- | --- | --- |
| 1D A | 0.956 -> 0.979 | 0.962 -> 0.981 | 0.985 -> 1.005 |
| 1D B | 1.070 -> 1.109 | 1.068 -> 1.107 | 1.003 -> 1.010 |
| 1D C | 0.998 -> 0.989 | 0.997 -> 0.998 | 0.880 -> 1.003 |
| 3D A | 0.951 -> 0.951 | 0.951 -> 0.951 | 0.995 -> 0.995 |
| 3D B | 1.107 -> 1.167 | 1.106 -> 1.162 | 1.024 -> 1.041 |
| 3D C | 0.994 -> 1.003 | 0.999 -> 1.010 | 1.008 -> 1.018 |

#149 ran at the beam files' 4 macroparticles per wavelength in 1D and 8 in 3D.
At 4 the 3rd and 5th harmonics alias onto the fundamental (h mod 4), so both
solvers carry ~20x the real harmonic power, and those harmonics act back on
the beam; the bands reproduce that, which is why 1D A and C move towards the
unaveraged solver. At 8 the 5th aliases onto the 3rd. So the cases were rerun
at 16 per wavelength in 1D and 12 in 3D, for both solvers, at #149's meshes:

| case | peak power | pulse energy | bunching | h = 3 | h = 5 |
| --- | --- | --- | --- | --- | --- |
| 1D A, n96 | 0.974 -> 0.975 | 0.981 -> 0.982 | 1.005 -> 1.007 | 0.929 | 0.556 |
| 1D B, n48, 0.125 cells | 1.073 -> 1.113 | 1.071 -> 1.110 | 1.005 -> 1.012 | 1.000 | 0.913 |
| 1D C, n96 | 0.991 -> 0.993 | 0.998 -> 0.999 | 0.941 -> 0.938 | 0.980 | 1.024 |
| 3D A, n48 | 0.976 -> 0.976 | 0.977 -> 0.977 | 0.995 -> 0.995 | (5.7) | (0.20) |
| 3D B, n48, 0.125 cells | 1.171 -> 1.231 | 1.160 -> 1.217 | 1.028 -> 1.044 | (1.9) | (7.5) |
| 3D C, n24 | 1.007 -> 1.016 | 1.002 -> 1.012 | 1.096 -> 1.069 | 1.113 | (1.72) |

(h = 3 and h = 5: the band's power at the end, from the spectrum of the
middle 30% within half a carrier frequency of the harmonic (of 0.75 h for B),
averaged / unaveraged. Bracketed values are not a test of the band; see
below.)

What that shows:

- **Where the bands are used as intended (1 wavelength per cell) they leave
  the fundamental alone**, as on the plain deck: 0.1% or less in power and
  bunching in 1D A and C and 3D A, and ~1% in 3D C. The planar A gap #149 put down to harmonic
  power is mostly sampling: at 16 per wavelength it is 2.6% without bands, not
  4.4%, and the bands close 0.1% of it.
- **1D C's bunching gap is not harmonic.** It is 0.941 at 16 per wavelength,
  with or without bands; at 4 it was 0.880, and the bands' 1.003 there came
  from the aliased harmonics. The harmonics themselves agree to 2%.
- **The bands break down at fine cells (B).** At 0.125 wavelengths per cell
  each band's envelope mesh holds +/-4 carrier frequencies about its own
  harmonic, so the bands overlap: band 3 radiates at the 0.75 colour and its
  -0.75 image (~2.9% of the field energy in 1D), band 5 an image near 8.75,
  and the fundamental band already holds the 2.25 harmonic. Each band couples
  to all of it with its own harmonic's factor, so the bands add 3.7% (1D) and
  5% (3D) to the averaged power that no real field has. Bands need each mesh
  to stay inside its own harmonic, `lambdarPerCell >= 0.5`; `chkAveraged`
  now warns below that.
- **Puffin's lattice band check ignored the harmonics.** 1D C's taper moves
  the resonance 32% (inside the fundamental's +/-50%), and so ~1 and ~1.6
  carrier frequencies relative to bands 3 and 5, with no warning. The
  harmonics agreed anyway, because they were radiated before the taper moved
  far. `chkAvgLattice` now scales the shift by the highest band and warns
  here, saying no `lambdarPerCell` holds band 5 without the bands
  overlapping.
- **In 3D the harmonics are checked only at saturation.** In 3D C the
  unaveraged field's odd harmonics dominate as they should (3rd 8.4e-3 of the
  field energy, 2nd 7.7e-3, 5th 3.0e-4), and the averaged 3rd agrees to 11%.
  Its 5th is 1.7x, against an unaveraged run at n24, under 5 nodes per
  5th-harmonic wavelength, so not a reference. Before saturation, in 3D A,
  the unaveraged spectrum has a 2nd harmonic at ~2e-3 of the field energy
  (15x 1D) and 5th and 6th harmonics above the 3rd (in 1D the 3rd is 200x the
  5th). That ordering looks numerical, for instance electrons' transverse
  wiggle across the deposition mesh, and until it is shown to converge with
  the transverse mesh it is not a reference either. The averaged 3D 3rd
  harmonic there, 3.7e-4 of the field energy, is half its 1D value. In 3D C the
  bands move the fundamental by ~1% (bunching, at a level of ~0.01, by 2.7%).
- **The detuned module costs the 5th harmonic's agreement.** 1D A's 5th is
  44% low (at ~4e-6 of the field energy) against 15% low on the plain deck at
  the same mesh; not traced.
- 3D B without bands moves from 1.107 at 8 per wavelength to 1.171 at 12:
  that case is sensitive to the sampling of the fundamental too, which is
  outside this comparison.

**Figures.** `figures/harm_*.png` and `figures/harm3d_*.png`, drawn by
`figures/make_harm_figures.sh`, are #149's layout with a third row: the power
in bands 3 and 5 along the undulator and the spectrum out to the 5th harmonic.
Blue is unaveraged, orange averaged with bands, grey dotted averaged without.
`harm_planar`, `harm_elliptical` and `harm_planar_mpw4` are the plain-deck
runs of the section above (192, 48 and 48 nodes per wavelength), the rest the
lattice cases in the second table. They give no solver times: these runs wrote
the field and beam every 5 periods and shared the machine.
