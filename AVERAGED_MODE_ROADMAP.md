# Period-Averaged Mode — Programme of Work

## Overview

The period-averaged (SVEA) solver mode, `qAveraged`, stores the radiation as a slowly
varying envelope about the resonant carrier and averages the electron equations over an
undulator period. It exists to make runs cheap where averaging is valid: on the 1D
validation deck it is 9.3x faster than the unaveraged solver at its own settings, and
closer to the converged answer.

**Current state:** 1D only, helical or linearly polarised undulators only, single band.
Implemented and validated on `feature/period-averaged`. The default (unaveraged) path is
byte-identical to `dev`.

**Governing constraint:** this is a *mode*, never a replacement. The unaveraged solver is
Puffin's reason to exist; every change here must leave it bit-exact.

Design notes live in `puffin/lib/undulator/averaging.f90` and the "Period-Averaged Mode"
section of `doc/manual.tex`. The accuracy map and its harness are in `benchmark/averaged/`.

---

## Status summary

| # | Workstream | Status | Depends on | Issue |
|---|------------|--------|-----------|-------|
| W1 | Land the 1D mode on `dev` | Ready to submit — decision needed | — | — |
| W2 | 3D | Implemented — validated against the unaveraged solver | — | — |
| W3 | Multiple field arrays (elliptical + harmonics) | Not started | #107 (ideally) | #129 |
| W4 | Compatibility: periodic mesh, lattices, restart | Done, bar one `dev` bug | — | — |
| W5 | Diagnostics, dump metadata and viz tools | Not started | W2 for 3D views | — |
| W6 | Finish the accuracy map | Partly done | W3 for harmonics | #130 |
| W7 | Housekeeping: buffer bug, benchmarks, defaults | Partly done | — | #128 |

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

Everything for a PR against `dev` is in place: opt-in, default bit-exact, unit and e2e suites
passing, manual and benchmark documentation written. The open ~1% planar residual is
documented and tracked (#130, #129) rather than unknown.

**Decision:** submit now and treat 3D and the rest as follow-ups, or hold the PR until the
residual is understood. Submitting now keeps the diff reviewable — it is already 22 files —
and lets 3D build on a merged base.

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
  `getFFelecs_3D`, `getSource_3D`) for all three undulator types. 27 unit tests; all four
  ctest suites still pass and the default path is untouched.
- **Still to do:** a committed 3D comparison deck and harness. `benchmark/averaged/compare.py`
  is 1D-only; the numbers below came from an ad-hoc 3D deck driven by hand. Turning that into
  a `run_compare3d.py` alongside the 1D one — integrated power, bunching, and a transverse
  profile — is what would make these reproducible, and is the natural next step here.

### Validated against the unaveraged solver

A 3D seeded deck, rho = 0.005, aw = 1.012, helical and plane-pole, 41x41x(2.0 -> 12.8)
mesh, matched beam, averaged at `lambdarPerCell = 1` and `stepsPerPeriod = 2` against the
unaveraged solver at `nodesPerLambdar = 12` and `stepsPerPeriod = 30`:

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

Guards are in and unit tested; two findings are open, one of them not ours.

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

### Chicanes: only a *fractional* slip diverges, and only because it suppresses the FEL

Adding chicanes to that lattice moved the power ratio to 0.895, flat in `lambdarPerCell`
(0.8952, 0.8957, 0.8976 at 1.0, 0.5, 0.25), so not a mesh effect. Separating the chicane's
three knobs, on the same three-module lattice:

| chicane | P ratio | bunching ratio | P unaveraged |
| --- | --- | --- | --- |
| slip 0, no dispersion | 1.0537 | 1.0231 | 4.141e-1 |
| slip 0, `R56` = 0.02 | 1.0545 | 1.0232 | 4.221e-1 |
| slip 1.0 (a whole wavelength) | 1.0537 | 1.0231 | 4.138e-1 |
| slip 0.5 (fractional) | **0.541** | 1.0189 | **2.931e-2** |

Dispersion and integer slips are exact to the same ~5% as the rest of the lattice. Only the
fractional slip diverges - and the last column says why. A fractional slip is a phase
shifter: it de-phases the beam against the radiation on purpose, and the unaveraged run's own
power drops 14x when it is switched on. What is left is a small residual of large cancelling
terms, and the ~2% coupling difference between the two solvers is amplified into a factor of
two there. Bunching still agrees to 1.9%, so the beam dynamics are fine; it is the radiated
field that is a difference of near-cancelling contributions.

So: not a bug, but a real limitation worth stating. **Any regime that works by near
cancellation - a phase shifter, a deliberately de-phased section - amplifies the model
difference, and averaged mode should not be trusted quantitatively there.** The same caution
applies wherever gain is suppressed rather than exponential.

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

Two concrete defects found while instrumenting it, neither sufficient alone:

1. `getFStEnd`, field-based branch (`para_field.f90` ~2245): `fz2_GGG = 1` and `ez2_GGG = 1`,
   where `ez2_act = NZ2_G`. The electron-based branch just above correctly sets
   `ez2_GGG = ez2_act`. Since `fz2`/`ez2` are taken straight from these in the non-unique
   branch, the duplicated region covers one node instead of the mesh.
2. Setting `ez2_GGG = ez2_act` then segfaults, because `(fz2_GGG - ez2_GGG + 1)` at
   `para_field.f90:615` and `:725` has its operands reversed — negative gather counts. Line
   622 has the same expression the right way round.

Together these say the path has never executed with `ez2_GGG /= 1`. Fixing it is a piece of
work on the parallel decomposition rather than a one-liner, and it belongs to `dev` rather
than to this branch; it is what stands between averaged mode and a single-cycle periodic run
on more than a few ranks. Until then, periodic averaged runs need
`nz2 >= 2 * nprocs`. Filed here rather than fixed because it changes shared parallel code
that the unaveraged e2e goldens depend on.

## W5 — Diagnostics, metadata and viz

- **Write averaged-mode metadata into the dumps** (`qAveraged`, `lambdarPerCell`, carrier
  wavenumber). Cheap, and it is what lets every downstream tool adapt instead of guessing.
  Worth doing early, independently of the rest.
- **Viz tools** (`utilities/pyPlotting/`: `puffin_viz_data.py`, `viewField1D_bokeh.py`,
  `viewField3D_bokeh.py`, player and theme). In averaged mode the field arrays hold an
  envelope: there is no carrier oscillation to plot, spectra are about the carrier rather
  than absolute, and a "reconstruct the resolved field" view may be worth offering. Power is
  already consistent, because the envelope is normalised so `|Atilde|^2` is the period
  average of `|A_perp|^2`.
- **Electron dumps.** `pperp` holds only its slow part in averaged mode — correct, and
  arguably more useful (it is the betatron momentum), but it must be labelled.

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

- **#128** — `calcBuff` rounds the field buffer short; fixed in averaged mode only, because
  fixing it generally shifts e2e goldens at the 1e-10 level. Also: a failed rearrangement
  exits with status 0.
- **`NBZ2_G`** — the z2 absorbing boundary is a fixed node count, so it covers ~10x more of
  the field on a coarse envelope mesh. See the note under W2 for the measurement.
- **A failed rearrangement exits 0**, so a half-finished run is indistinguishable from a
  finished one except by its last write's `zbarTotal`. `benchmark/averaged/compare3d.py`
  refuses to compare runs that ended at different zbar for exactly this reason; the fix is
  to exit non-zero.
- **The `qUnique = .false.` path** — see W4. Pre-existing, affects both solver modes.
- **Benchmarks and defaults.** Add an averaged case to the benchmark suite. The cheapest
  converged setting on the validation deck is `stepsPerPeriod = 1`, `lambdarPerCell = 1`.

---

## Suggested order

1. **W1** — land 1D, so later work builds on a merged base.
2. **W7 + the metadata item from W5** — cheap, and they unblock coarse meshes and the viz work.
3. **W2** — 3D. *(done)*
4. **W4** *(guards done; two findings open)* — compatibility and guards; mostly tests, and they protect users from silent wrongness.
5. **#107, then W3** — multiple field arrays: elliptical first (simpler, two components), then
   harmonic bands.
6. **W6** — close out the accuracy map, once W3 makes the harmonic question answerable.
