# Period-Averaged Mode — Programme of Work

## Overview

The period-averaged (SVEA) solver mode, `qAveraged`, stores the radiation as a slowly
varying envelope about the resonant carrier and averages the electron equations over an
undulator period. It exists to make runs cheap where averaging is valid: on the 1D
validation deck it is 9.3x faster than the unaveraged solver at its own settings, and on
the 3D one 17x — rising to 126x against the mesh the unaveraged solver actually needs to
converge. In both cases it is also closer to the converged answer than the unaveraged
solver is at its own defaults.

**Current state:** 1D and 3D, helical or linearly polarised undulators only, single band.
The 1D mode is on `feature/period-averaged` and in review as UKFELs/Puffin#131; 3D and the
compatibility guards are on `feature/averaged-3d`, branched from it. The default
(unaveraged) path is byte-identical to `dev` throughout.

**Governing constraint:** this is a *mode*, never a replacement. The unaveraged solver is
Puffin's reason to exist; every change here must leave it bit-exact.

Design notes live in `puffin/lib/undulator/averaging.f90` and the "Period-Averaged Mode"
section of `doc/manual.tex`. The accuracy map and its harness are in `benchmark/averaged/`.

---

## Status summary

| # | Workstream | Status | Depends on | Issue |
|---|------------|--------|-----------|-------|
| W1 | Land the 1D mode on `dev` | **In review** — PR #131 open against `dev` | — | #131 |
| W2 | 3D | **Done** — validated against the unaveraged solver | — | — |
| W3 | Multiple field arrays (elliptical + harmonics) | Not started | #107 (ideally) | #129 |
| W4 | Compatibility: periodic mesh, lattices, restart | **Done** — guards in, one `dev` bug left open | — | — |
| W5 | Diagnostics, dump metadata and viz tools | **Done** | — | — |
| W6 | Finish the accuracy map | Partly done | W3 for harmonics | #130 |
| W7 | Housekeeping: buffer bug, benchmarks, defaults | Partly done — on hold | — | #128 |

Raised by this work and not yet filed:

| what | where | who owns it |
|---|---|---|
| The `qUnique = .false.` duplicated-mesh path is broken, in both solver modes — includes a live heap overflow in `UpdateGlobalPow` | W4 | `dev` — pre-existing |
| `NBZ2_G` sizes the z2 absorbing boundary in nodes, not length | W2, W7 | with W7 |
| A failed field rearrangement exits with status 0 | W7 | with W7 |

---

## Invariants

Anything below must preserve these, and they are cheap to check:

1. **Default path bit-exact.** Verified by running a deck against a pristine `dev` build and
   comparing `/aperp` and `/power` byte for byte, plus all four `ctest` suites.
2. **Energy conservation by construction.** The coupling is one per-particle `ptilde` array
   shared between `getSource_*` and `dgamdz_f`; `test/testAveraging.pf` checks the balance
   through the real `getrhs` to 1e-13. Any new band or component must be added the same way.
3. **z2 stays the macroparticle coordinate**, with the ponderomotive phase computed on the
   fly, so the parallel decomposition, buffers, HDF5 chain and diagnostics keep working.
4. **Opt-in, and loudly rejected where invalid** rather than silently wrong.

---

## W1 — Land the 1D mode

**Submitted, and open for review: UKFELs/Puffin#131 against `dev`.** The decision recorded
here — submit now rather than hold for the ~1% planar residual — was taken that way, on the
grounds that the diff was already 22 files and that later work should build on a merged base.
The residual stays documented and tracked (#130, #129) rather than unknown.

Everything else it needed is in place: opt-in, default bit-exact, unit and e2e suites passing,
manual and benchmark documentation written.

W2 and W4 are built on the PR branch rather than on `dev`, so they will need a rebase if #131
picks up review changes.

## W2 — 3D

Done. `chkAveraged` no longer rejects 3D, and the two pieces of physics 3D needs are in
`averaging.f90` alongside the rest of the model.

- **Averaged natural focusing** — `getAvgFocusCoef` / `getAvgFocusing`. It survives averaging
  only through products of two fast quantities: the `bz` beat against the quiver, plus, for a
  curved-pole undulator, the off-axis structure of `bx`/`by` sampled along the quiver
  excursion (focusing in x, defocusing in y — what canting the poles is for). The result
  reproduces the `sKBetaX_G` / `sKBetaY_G` the unaveraged solver derives from the same
  undulator field, in all three geometries, which `test/testAveraging.pf` asserts. The
  strong-focusing channel (`qFocussing`) is already slow and carries through unchanged, with
  one factor of alpha as in `dppdz_r_f`.
- **Diffraction carrier offset** — `getAvgCarrierKz2`, threaded into `multiplyexp` and
  `AbsorptionStep`. The physical wavenumber is `kz2_envelope - 1/2rho`; offsetting rather
  than freezing it at the carrier keeps diffraction frequency-dependent across the band, and
  the envelope's own `kz2 = 0` mode diffracts exactly as the carrier did on a resolved mesh.
- **Filter semantics** — the `sFiltFrac` cutoff is applied to the physical wavenumber, which
  makes it inert in averaged mode: every representable mode sits within ~`1/(2 lambdarPerCell)`
  of the carrier, far above any sensible cutoff. That is the right answer rather than a
  special case — a cut at envelope `kz2 = 0` would delete the carrier. Documented in
  `doc/manual.tex`.
- **Tests** — `testAveraging.pf` gained the `kbeta` checks above, the focusing signs, the
  carrier offset, and the `getrhs` energy balance through the 3D path (`getInterps_3D`,
  `getFFelecs_3D`, `getSource_3D`) for all three undulator types, and the W4 guards. 28 unit
  tests; all four ctest suites still pass and the default path is untouched.
- **Harness** — `benchmark/averaged/run_compare3d.py` and `compare3d.py`, on `deck3d.in` /
  `beam_file3d.in` / `seed_file3d.in`, so every number below is reproducible. It compares
  mesh-independent observables rather than differencing fields node by node as the 1D
  `compare.py` does, because the two runs' z2 meshes differ by an order of magnitude.

### Validated against the unaveraged solver

First, the two new pieces of physics in isolation. These were measured by hand on a variant
of `deck3d.in` — the same mesh and beam, but with the coupling or the diffraction switched
off to leave one effect at a time, which `run_compare3d.py` does not do for you. Averaged at
`lambdarPerCell = 1` and `stepsPerPeriod = 2` against `nodesPerLambdar = 12` and 30 steps:

| what | measure | agreement |
| --- | --- | --- |
| Natural focusing, helical | beam `sigmaXbar`, `sigmaYbar` over 20 periods | 2e-5 |
| Natural + strong focusing, plane-pole | beam `sigmaXbar`, `sigmaYbar` over 20 periods | 4e-7 |
| Diffraction | field transverse RMS growth, 20 periods, seed only | 1.2% |

The diffraction test is sensitive: with the carrier offset removed as a control, the field
is annihilated (`sum|A|^2` from 1.4e2 to 5e-20) because the high-pass filter then sees every
envelope mode as sub-cutoff.

Then the full comparison, `run_compare3d.py helical --plain 12:30 24:60 48:120`: a seeded
helical deck run 120 periods to power x29, averaged at `lambdarPerCell = 1` and 2 steps per
period against the unaveraged solver at three meshes, on 4 ranks.

| unaveraged | runtime | P ratio | bunching ratio | field sigma_x | beam sigma_x |
| --- | --- | --- | --- | --- | --- |
| n = 12, 30 steps | 245 s | 1.0698 | 0.9743 | 1.0055 | 1.0007 |
| n = 24, 60 steps | 613 s | 0.9976 | 0.9904 | 1.0030 | 1.0007 |
| n = 48, 120 steps | 1795 s | 0.9830 | 0.9947 | 1.0022 | 1.0007 |
| averaged | **14.2 s** | | | | |

**The gap at the unaveraged solver's own default mesh is the unaveraged solver's.** Bunching
converges monotonically towards 1 as its mesh refines - 0.974, 0.990, 0.995 - exactly the
signature the 1D study found, where linear deposition and linear interpolation each attenuate
a carrier sampled at 11 cells per wavelength. The transverse observables barely move at all:
the beam envelope holds 1.0007 at every mesh, and the field's transverse profile agrees to an
L2 of 7e-3 in x and 1.3e-2 in y.

Read the ratio partway up the run as well as at the end. Against n48, power holds 0.999
through the whole exponential phase and only drifts to 0.983 past zbar ~ 4.5, as the run
begins to roll over towards saturation - averaged and unaveraged are expected to part company
there, and the deck is deliberately run far enough to show it.

### Known interaction: node-count boundaries on a coarse mesh

`NBZ2_G = 37` (`setup_calcs.f90`) sizes the z2 absorbing boundary in **nodes**, not length.
That is 1.7% of an unaveraged z2 mesh and ~18% of a `lambdarPerCell = 1` envelope mesh of the
same physical length, so with `sBeta > 0` the averaged run absorbs far more of the field —
about 6% over 20 periods on the deck above, against 0.45% unaveraged. Same class of problem
as the `calcBuff` rounding in W7 (a node count that does not survive mesh coarsening), so it
is parked with W7 rather than fixed here. Until then, 3D averaged runs should set
`sBeta = 0`, or accept a wider absorber. `NBX_G` / `NBY_G` are unaffected: the transverse
mesh does not coarsen.

## W3 — Multiple field arrays

Elliptical undulators and harmonic bands are the same structural problem: more than one
envelope. Doing them as one generalisation is much cheaper than twice.

- **General elliptical.** A single envelope is exact only for helical (`u+ = 0`) or linear
  (`|u+| = |u-|`); elliptical needs the two helicity components as independent envelopes,
  since their coupling ratios differ. Currently rejected at setup and in `UndSection`.
- **Harmonic bands (#129).** An extra envelope per odd harmonic, carrier `exp(-i h z2/2rho)`,
  coupling `JJ_h = J_((h-1)/2)(h xi) - J_((h+1)/2)(h xi)`, reducing to today's `J0 - J1` at
  `h = 1`. Recovers both the missing harmonic output (~2% of radiated power at aw ~ 1) and
  the harmonics' drive on `dGamma/dz`.
- **Sequencing:** both want the field storage to be an object rather than loose module
  arrays, which is exactly #107 (`tFieldValues`). Doing #107 first will make this much less
  invasive.
- **Acceptance:** energy balance summed over components; elliptical reproduces helical and
  planar as limiting cases; harmonic power compares like-for-like against an unaveraged run.

## W4 — Compatibility

Done. Guards are in and unit tested, and the three compatibility questions the roadmap
raised — periodic meshes, lattices, restart and HDF5 input — are each answered below. One
finding is left open, and it is not ours: a `dev` bug in the parallel field decomposition
that averaged mode happens to reach sooner than the unaveraged solver does.

### Done

- **Restart and HDF5 input — rejected, per "rejection first".** `chkAveraged` now refuses
  `qResume`, `iReadH5Field` (field_file), and HDF5/MASP macroparticle input, each with a
  message saying why. These are the silently-wrong cases: a resolved field read into an
  envelope array, or a `pperp` that still carries the quiver, both look plausible. Converting
  instead of rejecting needs dump metadata saying which mode wrote the file, which is the
  W5 item.
- **Far-from-resonance decks.** The envelope mesh holds roughly `1 +/- 1/(2 lambdarPerCell)`
  of the resonant frequency. A seed `freqf` outside that band is now a hard error; a beam
  energy oscillation (`mag`) or energy spread wider than the band warns. `testAveraging.pf`
  pins every branch, including that none of it fires with the mode off.
- **Periodic mesh** (`meshType = 1`) **works.** On a 6-wavelength periodic 1D mesh, averaged
  against unaveraged: total power to 0.28%, mean energy to 5e-7. It works wherever the mesh
  still decomposes across the ranks in use — see the `qUnique` finding below for where it
  does not, which turns out not to be an averaged-mode problem.
- **Lattices: modules and drifts are equivalent, as the roadmap assumed.** `sZ` reaching
  `getrhs` is global zbar (`und%z_taper_start + szl`), so the averaged ponderomotive phase
  and the unaveraged quiver phase are the same quantity across a module boundary. Measured on
  three helical modules against the same total length as one:

  | lattice | power ratio avg/unavg |
  | --- | --- |
  | single module, 60 periods | 1.0495 |
  | 3 modules back to back | 1.0496 |
  | 3 modules + 2 drifts | 1.0537 |

### Chicanes: a half-integer slip diverges, because it suppresses the FEL

Adding chicanes to that lattice moved the power ratio to 0.895, flat in `lambdarPerCell`
(0.8952, 0.8957, 0.8976 at 1.0, 0.5, 0.25), so not a mesh effect. Scanning the slip, with
dispersion off:

| `chic_slip` (λr) | P unaveraged | P ratio | bunching ratio |
| --- | --- | --- | --- |
| 0.0 | 4.141e-1 | 1.0537 | 1.0231 |
| 0.5 | **2.931e-2** | **0.5409** | 1.0189 |
| 1.0 | 4.138e-1 | 1.0537 | 1.0231 |
| 1.5 | **2.934e-2** | **0.5415** | 1.0189 |
| 2.0 | 4.136e-1 | 1.0537 | 1.0231 |

`R56` on its own is clean too: 1.0545 at `chic_slip = 0`, `R56 = 0.02`.

**The response is exactly periodic in one radiation wavelength**, so what matters is the
slip modulo λr - a π phase flip - and not whether the slip is a whole number. That
distinction matters because the *total* slippage is never a whole number here anyway: see
the drift note below. An earlier version of this section called the effect "fractional
versus integer slip", which was the wrong axis.

The second column says why the half-integer cases diverge. Half a wavelength reverses the
sign of the coupling, which is what a phase shifter is for, and it knocks the **unaveraged**
run's own power down 14x. What is left is a small residual of large cancelling terms, and the
~2% coupling difference between the two solvers is amplified into a factor of two there.
Bunching still agrees to 1.9% in every row, so the beam dynamics are fine; it is the radiated
field that is a difference of near-cancelling contributions.

So: not a bug, but a real limitation worth stating. **Any regime that works by near
cancellation - a phase shifter, a deliberately de-phased section - amplifies the model
difference, and averaged mode should not be trusted quantitatively there.** The same caution
applies wherever gain is suppressed rather than exponential.

### Drifts do not slip a whole number of radiation wavelengths

Worth recording because it is easy to assume otherwise, and because it is what makes the
"modulo λr" framing above the right one. A `DR` element is specified in **undulator**
periods. Inside the undulator, resonance makes one undulator period exactly one radiation
wavelength of slippage - that is how Puffin's scaling is built, `p2 = 1` at resonance and
both `zbar` and `z2` advance by 4πρ. In a drift there is no quiver, so `1 - beta_z` falls
from `(1 + aw^2)/2gamma^2` to `1/2gamma^2` and the same `zbar` buys a factor `1/(1 + aw^2)`
as much `z2`.

Measured by differencing two lattices that differ only by the drift, with field coupling off
so the advance is purely kinematic:

| | slippage from `DR 4.0` |
| --- | --- |
| unaveraged | 1.976581 λr |
| averaged | 1.976581 λr |
| predicted, 4/(1 + aw^2) at aw = 1.0122 | 1.9758 λr |

0.04% from the analytic value, and the two solver modes agree to seven significant figures -
so the drift slippage itself is handled identically, which is the part that had to be checked.
At `aw ~ 1` a drift slips about half a wavelength per undulator period, so a lattice that
wants the beam back in phase after a drift has to put the remainder in deliberately. The
CLARA lattice in `test/inputs/1D/osc_taper.latt` does exactly that, with a dispersionless
`CH` used as a phase shifter after each drift.

### Open, and pre-existing on `dev`: the duplicated-mesh path

When the active field region is too small to give every rank a slab, `getFStEnd` sets
`qUnique = .false.` and the whole region is meant to be duplicated on every rank with the
particles still distributed. **That path does not work, in either solver mode.** It is not
about nodes per wavelength: holding the mesh fixed and varying only the rank count,

| | 1 rank | 2 | 3 | 4 | 6 | 7 | 8 |
|---|---|---|---|---|---|---|---|
| averaged, nz2 = 7 | ok | ok | ok | **fail** | **fail** | | |
| unaveraged, nz2 = 12 | ok | | | ok | ok | **fail** | **fail** |

`qUnique` is false exactly when `n_act_g < 2*nprocs`, which is the boundary in both rows. The
unaveraged solver reaches it too, just at more ranks, because it never coarsens. The failure
is `getInterps` finding particles past `bz2`, three failed emergency redistributes, then
`stop` — **with exit status 0**, the silent failure already noted in W7.

**Root defect.** `getFStEnd`'s field-based branch (`para_field.f90` ~2241), which is the
branch a periodic mesh takes, sets

```fortran
fz2_act = 1_ip
ez2_act = NZ2_G      ! the active region is the whole mesh - correct
fz2_GGG = 1
ez2_GGG = 1          ! should be ez2_act
```

The electron-based branch immediately above sets `fz2_GGG = fz2_act` and `ez2_GGG = ez2_act`.
Here the first line happens to be right and the second is not.

**Why it is invisible.** `fz2_GGG`/`ez2_GGG` are read in only two places, and both are
written so that `ez2_GGG = 1` masks a second defect rather than exposing it:

- The non-unique branch takes `fz2 = fz2_GGG`, `ez2 = ez2_GGG`, `mainlen = n_act_g`. So the
  rank is told it owns node 1 only, while `mainlen` says it owns the whole mesh.
- `UpdateGlobalPow` sizes `powi` as `ez2_GGG - fz2_GGG + 1` and, on the non-unique path,
  does `powi = A_local` where `A_local` has `mainlen` elements. With `ez2_GGG = 1` that
  copies `mainlen` doubles into a one-element allocation — a live heap overflow today,
  every time `/power` is written while `qUnique` is false.

Several nearby gather counts are also written `(fz2_GGG - ez2_GGG + 1)` rather than the other
way round — `para_field.f90:615` and `:725`, against `:622` and `:697` which have it right.
While `fz2_GGG == ez2_GGG == 1` the two spellings are numerically identical, so the reversal
cannot bite. `:615` is in `UpdateGlobalField`, which is neither public nor called from
anywhere — dead code.

**What was measured, and what was not.** Instrumenting the bound check gives, on a 7-node
periodic mesh at 4 ranks, `fz2 = 1, ez2 = 5, bz2 = 5, nz2_G = 7` with particles sitting at
node 6 — legitimately inside a periodic mesh, flagged out of bounds because the region stops
short. Every retry re-derives the same region, so the three emergency rearrangements cannot
help, and it stops. Setting `ez2_GGG = ez2_act` changes the failure to a segfault inside
`free()`, which says that value really does drive the behaviour, **but the site of that
corruption was not isolated** — it is not `:615`, which is dead, and `UpdateGlobalPow`'s
overflow gets *better* with the fix, not worse. Something further downstream is also wrong.

So: at least one root defect, one live heap overflow it masks, a family of reversed counts
waiting behind it, and an unidentified third problem. The path has evidently never run with
`ez2_GGG /= 1`. That is a piece of work on the parallel decomposition, not a one-liner, and
it belongs to `dev` rather than to this branch — it is what stands between averaged mode and
a single-cycle periodic run on more than a few ranks. Until then, periodic averaged runs need
`nz2 >= 2 * nprocs`. Left here rather than fixed because it changes shared parallel code the
unaveraged e2e goldens depend on.

## W5 — Diagnostics, metadata and viz

Done.

### Dump metadata

`writeRunAtts` (`io/hdf5PuffLow.f90`) writes four attributes into `/runInfo` of every dump —
field, electron and integrated — in **both** solver modes, so a reader never has to branch on
their presence:

| attribute | meaning |
| --- | --- |
| `qAveraged` | 0 or 1, the solver mode that wrote the file |
| `kz2Carrier` | carrier the stored field is an envelope about, so `A_perp = aperp * exp(i kz2Carrier z2)` holds in both modes; 0 unaveraged, where applying it is a no-op |
| `lambdarPerCell` | z2 cell size in resonant wavelengths, **measured from the mesh**, not copied from the input of the same name |
| `pperpMeaning` | whether electron `px`, `py` carry the undulator quiver or only their slow (betatron) part |

`lambdarPerCell` is measured rather than copied deliberately. The input is meaningless
unaveraged — writing it there would have claimed 1 where the mesh is really
`1/(nodesPerLambdar - 1)` — and even in averaged mode the mesh is rounded to a whole number
of nodes, so a deck asking for 2.0 can get 1.99776. The attribute is always true about the
file it is in.

This is also what would let W4's blanket `qResume` rejection become a conversion: the
rejection exists only because nothing in a dump said which mode wrote it. That is no longer
the case for dumps written from here on, though a conversion still has to be written, and
old dumps still carry no marker.

### Viz tools

`utilities/pyPlotting/puffin_viz_data.py` gains `is_averaged`, `carrier_kz2`,
`lambdar_per_cell`, `mode_label`, `band_edges`, `spectrum` and `reconstruct_resolved`, all
falling back sensibly on files written before the metadata existed. The viewers use them:

- **Spectra are about the carrier.** Unaveraged, the two field components each have their own
  real spectrum. Averaged, they are the halves of one complex envelope, so the transform is
  the complex one and its frequencies are offsets: `w/wr = 1 - 4 pi rho f_env`. The sign
  follows from `A = Atilde exp(-i z2/2rho)` — a positive envelope wavenumber is a *lower*
  physical frequency — and is checked against the physics independently rather than against
  the same formula.
- **The representable band is drawn.** Beyond `1 +/- 1/(2 lambdarPerCell)` an averaged
  spectrum is showing aliases, so the viewers mark the edges.
- **Titles say what is plotted** — "envelope, 2 λr/cell" against "resolved field", `|Ã⊥|`
  against `|A⊥|`. Power and intensity are numerically unchanged, because the envelope is
  normalised so `|Atilde|^2` is the period average of `|A_perp|^2`; only the symbol moves.
- **`reconstruct_resolved`** rebuilds `A_perp` from the envelope on an upsampled mesh, for
  showing someone what the field looks like. It is documented as a view rather than data:
  the envelope is interpolated linearly, so it is exact only insofar as the envelope is slow.

**Verified on real dumps.** `environment.yml` declared `python=3.11` but none of the packages
the plotting utilities actually import, and the `puffin` environment had drifted to having no
Python at all — so the viz code could not be run. `numpy`, `h5py` (pinned to the
`mpi_openmpi` build, so it resolves against the solver's own parallel HDF5) and `bokeh` are
now declared there and in `BUILD.md`. Installing them left `hdf5`, `openmpi`, `fftw` and
`gfortran` untouched and all four ctest suites still pass, which matters because the e2e
goldens are bit-exact.

With that, both viewers run to completion on averaged and unaveraged output, and the numbers
check out on real files:

| | z2 mesh | spectrum range in w/wr | peak |
| --- | --- | --- | --- |
| averaged, 1 λr/cell | 956 | 0.501 to 1.500 — exactly the band | 1.0000 |
| unaveraged, n = 12 | 10506 | 0.000 to 5.501 — Nyquist at 11 cells/λr | 1.0001 |

Both peak on resonance, which a resonantly-seeded deck requires; all of the averaged run's
spectral power falls inside the band; and `reconstruct_resolved` takes 956 envelope nodes to
15281, preserving peak `|A|` while going from 5 zero crossings to 1909 — the carrier
appearing, which is the point of it.

## W6 — Finish the accuracy map

- **rho scan (#130)** — deferred; characterises the ~1% planar remainder.
- **Harmonic back-action** — needs W3 before it can be separated from the above.
- **Model the dropped A-term.** The radiation-driven transverse momentum accounts for ~0.55%
  in both polarisations. Its slow effect on p2 can be added analytically, removing a known
  error rather than documenting it.
- **A deck that can discriminate cell size.** The current deck is seeded, narrow band and flat
  top, so it cannot justify cells coarser than 1-2 wavelengths. SASE, or sharp current
  gradients, would also exercise what averaging gives up (coherent spontaneous emission).

## W7 — Housekeeping

**On hold** — deliberately not picked up, pending a decision on the `calcBuff` item. W2 and
W4 each turned up another member of the same family, so they are collected here rather than
fixed in passing.

- **#128** — `calcBuff` rounds the field buffer short; fixed in averaged mode only, because
  fixing it generally shifts e2e goldens at the 1e-10 level.
- **`NBZ2_G`** — the z2 absorbing boundary is sized in *nodes*, not length, so it covers
  ~10x more of a coarse envelope mesh than of an unaveraged one. Measured under W2: ~6% of
  the field absorbed over 20 periods against 0.45% unaveraged. Same family as #128 — a node
  count that does not survive mesh coarsening. Until it is settled, 3D averaged runs want
  `sBeta = 0`, which is what `deck3d.in` does.
- **A failed rearrangement exits 0**, so a half-finished run is indistinguishable from a
  finished one except by its last write's `zbarTotal` — which is how one bogus comparison
  got made during W4 before it was caught. `benchmark/averaged/compare3d.py` now refuses to
  compare runs that ended at different zbar; the real fix is to exit non-zero.
- **The `qUnique = .false.` path** — see W4. Pre-existing, affects both solver modes, and
  bigger than the rest of this list; probably its own issue against `dev`.
- **Benchmarks and defaults.** Add an averaged case to the benchmark suite. The cheapest
  converged setting on the 1D validation deck is `stepsPerPeriod = 1`, `lambdarPerCell = 1`;
  the 3D deck converges at `stepsPerPeriod = 2`, `lambdarPerCell = 1`.

---

## Where things stand

Done, in order of landing:

1. **W1** — 1D mode written, validated and submitted as #131. In review.
2. **W2** — 3D. Averaged natural focusing and the diffraction carrier offset, validated
   against the unaveraged solver on focusing (2e-5), diffraction (1.2%) and a full mesh-
   refinement scan showing the residual converging away. 17x faster than the unaveraged
   solver at its own default mesh, 126x against the mesh it needs to converge.
3. **W4** — compatibility. Guards against the silently-wrong inputs; periodic meshes,
   multi-module lattices, drifts and chicanes each checked against the unaveraged solver.
4. **W5** — dump metadata saying which mode wrote a file and what its arrays mean, and viz
   tools that read it instead of guessing.

Not done, in suggested order:

5. **W7** — housekeeping. On hold pending a decision on the `calcBuff` rounding; the
   `NBZ2_G` and exit-status items found during W2 and W4 are parked with it.
6. **#107, then W3** — multiple field arrays: elliptical first (simpler, two components),
   then harmonic bands.
7. **W6** — close out the accuracy map, once W3 makes the harmonic question answerable.

Separately, and not part of this programme: **the `qUnique = .false.` duplicated-mesh path
is broken on `dev`** (see W4). It affects the unaveraged solver too, and it is what stands
between averaged mode and a single-cycle periodic run on more than a few ranks. Best raised
as its own issue against `dev` rather than carried here, since fixing it means changing
shared parallel code that the unaveraged e2e goldens depend on.

## What would change the picture

Honest limits of what has been shown, rather than open tasks:

- **Every 3D validation is helical or plane-pole, seeded, noise-free, and flat-top.** The
  1D README already warns that such a deck cannot discriminate cell sizes; the same applies
  to the 3D one. SASE, sharp current gradients or a short bunch would be far more demanding,
  and would exercise the coherent spontaneous emission that averaging discards.
- **Near-cancellation regimes amplify the model difference.** The half-integer chicane slip
  is the clean example: where the unaveraged run's own power is suppressed 14x, a 2% coupling
  difference becomes a factor of two. Anywhere gain is suppressed rather than exponential
  deserves the same suspicion.
- **Undulator ends are untested in 3D and across modules.** `getAvgEnvelope` uses the `by`
  ramp for both components, while the helical `bx` ramp has a different shape — a phase
  difference that would recur at every module boundary with `qUndEnds` on, rather than once.
- **Saturation is not validated, and should not be expected to agree.** The 3D deck is run
  far enough to start rolling over precisely so the divergence is visible in the series.
