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
| W4 | Compatibility: periodic mesh, lattices, restart | Not started | — | — |
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
| Full coupling + diffraction | `bunchingFundamental` over 60 periods, growing 70x | 2.7% |
| Full coupling + diffraction | `/power` over 60 periods | 0.1% |

The 60-period run takes 10.7 s averaged against 158 s unaveraged, **15x**. Its bunching ratio
is flat from about a third of the way in, so the 2.7% is a coupling offset rather than a
growth-rate error — comfortably inside the ~9% the 1D study showed the unaveraged solver
itself carries at `nodesPerLambdar = 12`, from deposition and interpolation attenuating a
carrier sampled at 11 cells per wavelength.

The diffraction test is sensitive: with the carrier offset removed as a control, the field
is annihilated (`sum|A|^2` from 1.4e2 to 5e-20) because the high-pass filter then sees every
envelope mode as sub-cutoff.

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

Each of these is mostly a test, and each may turn up a guard that needs writing.

- **Periodic mesh** (`meshType = 1`). Untested in averaged mode. Watch the interaction
  between the periodic wrap (`bz2PB`) and the averaged mode's exact buffer bound in
  `calcBuff`. Testbeds: `test/inputs/1D/osc_taper.in`, `inputs/simple/3D/CLARA/single-slice/`.
- **Lattices and multiple modules.** Drifts, chicanes, quads and modulations all act on z2 and
  the slow momentum, so they should be correct as-is — but the ponderomotive phase restarts
  with each module's local zbar, exactly as the unaveraged quiver does, and that equivalence
  should be demonstrated on a multi-module deck rather than assumed.
- **Restart and HDF5 input.** `qResume`, `iReadH5Field`, MASP and HDF5 beam input are
  currently unguarded: a resolved field read into an envelope array, or a `pperp` that still
  contains the quiver, would be silently wrong. Either convert on read (demodulate the field,
  subtract the quiver) or reject with a clear message. Rejection first, conversion later.
- **Far-from-resonance decks.** Two-colour and strongly tapered cases violate the premise that
  `dtheta/dzbar = (1 - p2)/2rho` is slow; they need either multiple carriers or a detuning
  check that warns. Note `osc_taper` is both periodic and tapered.

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
- **Benchmarks and defaults.** Add an averaged case to the benchmark suite. The cheapest
  converged setting on the validation deck is `stepsPerPeriod = 1`, `lambdarPerCell = 1`.

---

## Suggested order

1. **W1** — land 1D, so later work builds on a merged base.
2. **W7 + the metadata item from W5** — cheap, and they unblock coarse meshes and the viz work.
3. **W2** — 3D. *(done)*
4. **W4** — compatibility and guards; mostly tests, and they protect users from silent wrongness.
5. **#107, then W3** — multiple field arrays: elliptical first (simpler, two components), then
   harmonic bands.
6. **W6** — close out the accuracy map, once W3 makes the harmonic question answerable.
