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
| W3a | Polarisation field grids, planar + helical only | **Done** — nComp = 2, second component slaved | #107 ✅ | #129 |
| W3b | General polarisation coupling (elliptical + harmonics) | Not started — needs the per-component couplings derived | W3a ✅ | #129 |
| W4 | Compatibility: periodic mesh, lattices, restart | **Done** — guards in, one `dev` bug left open | — | — |
| W5 | Diagnostics, dump metadata and viz tools | **Done** | — | — |
| W6 | Finish the accuracy map | Partly done — 1D SASE validated; rho scan and harmonics left | W3 for harmonics | #130 |
| W7 | Housekeeping: buffer bug, benchmarks, defaults | Partly done — on hold | — | #128 |

Raised by this work and not yet filed:

| what | where | who owns it |
|---|---|---|
| Periodic meshes with under 2 active nodes per rank fail, and corrupt `/power`, in both solver modes; temporal meshes are exact | W4 | `dev` — filed as #132 |
| `NBZ2_G` sizes the z2 absorbing boundary in nodes, not length | W2, W7 | with W7 |
| A failed field rearrangement exits with status 0 | W7 | with W7 |
| `shuntBeam` places the beam a tenth of a *field* cell from the mesh start, so the same beam sits at a different z2 origin in the two modes | W6 | benign; documented in `compare_sase.py` |

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

Split in two, because the storage and the physics are independent and the
storage unblocks viz work while the coupling derivation is outstanding.

### W3a — the polarisation grids. Done.

Averaged mode carries one envelope per linear polarisation: `nComp = 2`,
component 1 = x, component 2 = y. Unaveraged stays at 1, where a complex pair
already means `(A_x, -A_y)`.

**No new physics was needed**, which is why this could land first. For both
undulator types averaged mode supports, the y envelope is fixed by the x one
exactly and at every `zbar`:

| undulator | relation | why |
|---|---|---|
| planar | `Atilde_y = 0` | `A_perp = sqrt(2) Re[Atilde e]` is purely real — linear along x |
| helical | `Atilde_y = i Atilde_x` | `A_perp = Atilde e` unconstrained, so `A_y = -Im[Atilde e]` |

`setAvgSlavedComp` derives component 2 from component 1 before each dump. Only
component 1 is integrated. The `aperp` dataset's 4th dimension is `2*nComp`,
and `/runInfo` carries `nFieldComp` and `fieldCompMeaning`.

Verified by running the 1D averaged deck planar and helical on 2 ranks and
reading the dumps back — component 2 exact in both cases. Worth knowing:
**no e2e suite enables averaged mode**, so the four ctest suites only prove the
unaveraged path is untouched. `testAveraging.pf::testSlavedComponent` is the
only automated guard on the slaving.

### W3b — the general coupling. Not started.

What remains is the physics: per-component couplings so component 2 evolves
independently, which is what elliptical needs. The constraints any derivation
has to satisfy, two of them already pinned:

- **Planar**: `ptilde_y = 0` and `ptilde_x = -alpha (u_y/2)(J0 - J1) e^{-i theta}`.
  Derived by matching the shipped coupling through the field normalisation, not
  assumed — this is where the familiar difference of Bessel functions lives once
  the basis is Cartesian.
- **Helical**: `xi = 0`, `Atilde_y = i Atilde_x`, and
  `ptilde_x - i ptilde_y = -alpha u_y e^{-i theta}`.
- **Against the circular basis**: under `Atilde_mp = (Atilde_x mp i Atilde_y)/2`
  the combination must reproduce today's `alpha (u_- J0 - u_+ J1)`. Note that a
  naive symmetry guess does *not* — see the design note; the two carriers and the
  conjugation on the `+` component mean the coupling transform is not the plain
  linear map the field transform is.
- **Energy**: balance summed over components, 1e-13, through the real `getrhs`.

Then: `sField4ElecReal/Imag` gain a component dimension, `dgamdz_f` sums over
components, the loop goes inside `getrhs` (shared interpolation weights), and
`chkAveraged`/`UndSection` lift the elliptical rejection. Harmonics extend the
same component axis with carrier `exp(-i h z2/2rho)` and `JJ_h`.

### Original notes

Elliptical undulators and harmonic bands are the same structural problem: more than one
envelope. Doing them as one generalisation is much cheaper than twice.

- **General elliptical.** A single envelope is exact only for helical (`u+ = 0`) or linear
  (`|u+| = |u-|`); elliptical needs the two helicity components as independent envelopes,
  since their coupling ratios differ. Currently rejected at setup and in `UndSection`.
- **Harmonic bands (#129).** An extra envelope per odd harmonic, carrier `exp(-i h z2/2rho)`,
  coupling `JJ_h = J_((h-1)/2)(h xi) - J_((h+1)/2)(h xi)`, reducing to today's `J0 - J1` at
  `h = 1`. Recovers both the missing harmonic output (~2% of radiated power at aw ~ 1) and
  the harmonics' drive on `dGamma/dz`.
- **Sequencing:** both wanted the field storage to be an object rather than loose module
  arrays, which was #107. **That is now done**, so the storage a second envelope needs
  already exists in both places it has to:
  - `ctx%field` (`tFieldValues`) holds the mesh-resident arrays and the decomposition.
  - `work%env(:)` (`tRK4Workspace`) holds the integrator's stage arrays and the inner-mesh
    field, already indexed by envelope. `nEnv` is set in one place, `allact_rk4_arrs`.
- **Where the envelope loop goes:** inside `getrhs`, not around it. Two reasons, and both
  are load-bearing. Locating each macroparticle's nodes and computing its interpolation
  weights is identical for every envelope, so one pass scattering to N envelopes is strictly
  cheaper than N passes. And invariant 2 needs every envelope's contribution summed into
  `dGamma/dz` through the same per-particle `ptilde`, which `testAveraging.pf` checks to
  1e-13 — looping outside `getrhs` would break that balance.
- **The signature work this implies:** `derivs` and `getrhs` still take the stage arrays
  positionally, 18 arguments each, which is why they cannot currently loop. Collapsing them
  to `(sz, istage, work, ctx)` is the enabling change and belongs with this workstream
  rather than with #107, which had no second envelope to justify it.
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

  | lattice | power ratio avg/unavg | bunching ratio |
  | --- | --- | --- |
  | single module, 60 periods | 1.0811 | 1.0139 |
  | 3 modules back to back | 1.0812 | 1.0139 |
  | 3 modules + 2 drifts | 1.0754 | 1.0113 |

  (Re-measured with the resonant-helicity seed; the first numbers recorded here were a few
  per cent off for the reason given under chicanes below.)

### Chicanes work, at any slip

An earlier version of this section reported that a half-integer chicane slip made the power
ratio collapse to 0.54, and explained it as near-cancellation amplifying the model
difference. **That was an artefact of the test, not a property of the mode.** The test deck's
seed (`benchmark/averaged/seed_file.in`) is linearly polarised, and the lattice used a
*helical* undulator. One envelope pins the ratio of the field's two helicity components at
`u+/u-`, which is zero for helical, so the opposite helicity cannot be represented at all and
half a linear seed's power is dropped - correctly, it does not couple to the beam, but
silently. The harness for the 1D study sets `sA0_Y` to match the undulator for exactly this
reason; the ad-hoc lattice runs did not.

The giveaway was in the data and went unnoticed: the power ratio was already exactly 0.5000
at zbar = 0, before either run had done anything. With the resonant-helicity seed:

| `chic_slip` (λr) | P unaveraged | P avg | P ratio | bunching ratio | ratio at zbar = 0 |
| --- | --- | --- | --- | --- | --- |
| 0.0 | 1.168e+0 | 1.256e+0 | 1.0754 | 1.0113 | 1.0000 |
| 0.5 | 6.075e-2 | 6.016e-2 | **0.9905** | 1.0163 | 1.0000 |
| 1.0 | 1.167e+0 | 1.255e+0 | 1.0754 | 1.0113 | 1.0000 |

The half-wavelength slip agrees to **1%** - better than the in-phase cases do. It looked
catastrophic before because that configuration de-phases the beam on purpose and keeps the
field near its seeded level, so the dropped half of the seed dominated the total; in the
in-phase cases the field grew ~30x and swamped it, which is why those rows looked fine.
Dispersion is likewise clean.

**So there is no chicane limitation, and the claim that near-cancellation regimes amplify the
model difference is withdrawn - it was never supported by anything but this artefact.** The
arithmetic never worked either: a 2% coupling difference propagating through a 3.8x amplitude
suppression gives of order 10%, not a factor of two.

Two guards now exist so this cannot recur silently:

- `getSeed` warns when the seed's polarisation is not one a single envelope can hold, naming
  the fraction of the seed power it actually reproduces. A linear seed on a helical undulator
  reports 50%; a linear seed on a planar one is exact and stays quiet, because there
  `u+ = u-` and the envelope carries both helicities.
- `compare3d.py` reports the power ratio at the first write and warns if it is not 1, since
  any deviation there means the two runs did not start from the same field and nothing
  downstream is a comparison of the physics. `run_compare3d.py` sets the seed polarisation
  from the undulator type, as the 1D harness does.

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

### Open, and pre-existing on `dev`: periodic meshes on the duplicated-mesh path (#132)

When the active field region is too small to give every rank a slab, `getFStEnd` sets
`qUnique = .false.` and the region is duplicated on every rank with the particles still
distributed. **That path is correct on a temporal mesh and broken on a periodic one.** It
takes both conditions, and neither alone.

**Condition 1 - fewer than two active-region nodes per rank.** `qUnique` is false exactly
when `n_act_g < 2 * nprocs`. Mapped by varying mesh size and rank count independently, on a
periodic mesh at `lambdarPerCell = 1`:

| nz2, down / ranks, across | 1 | 2 | 3 | 4 | 5 | 6 |
|---|---|---|---|---|---|---|
| 2 | ok | fail | fail | fail | fail | fail |
| 3 | ok | fail | fail | fail | fail | fail |
| 4 | ok | ok | fail | fail | fail | fail |
| 5 | ok | ok | fail | fail | fail | fail |
| 6 | ok | ok | ok | fail | fail | fail |
| 7 | ok | ok | ok | fail | fail | fail |
| 8 | ok | ok | ok | ok | fail | fail |
| 9 | ok | ok | ok | ok | fail | fail |
| 10 | ok | ok | ok | ok | ok | fail |
| 12 | ok | ok | ok | ok | ok | ok |

Exactly `nz2 >= 2 * nprocs`, boundary inclusive. One rank never fails. The unaveraged solver
reaches the same line, just at more ranks, because it never coarsens: on the same periodic
deck it needs 7 ranks at `nodesPerLambdar = 12` where averaged mode needs 4 at `nz2 = 7`.

**Condition 2 - a periodic mesh.** On a temporal mesh the duplicated path is not merely
non-fatal, it is exact. A 1 to 4 lambda_r beam on a 60 lambda_r mesh gives `n_act_g = 2`, so
`qUnique` is false from 2 ranks up, and `/power` at 2, 3, 4 and 6 ranks is **bit-identical**
to the single-rank run in every case.

**Why.** The one defect is `ez2_GGG = 1` in `getFStEnd`'s field-based branch, where
`ez2_act = NZ2_G`; the electron-based branch above it correctly writes `ez2_GGG = ez2_act`.
The non-unique branch then takes `ez2 = ez2_GGG`, and what happens next depends on the mesh,
because `calcBuff` treats the two differently:

- **Periodic:** the last rank does `bz2 = ez2 + bz2PB`, so the buffer is derived *from* the
  bad `ez2` and stays short. Measured on a 7-node mesh at 4 ranks: `fz2 = 1, ez2 = 5,
  bz2 = 5` with particles legitimately at node 6, flagged out of bounds. Every retry
  re-derives the same region, the three emergency rearrangements fail, and it stops - with
  exit status 0. The output is wrong too: the initial `/power` has 3 of 7 nodes nonzero and
  sums to 43% of the 2-rank value.
- **Temporal:** that block is skipped. `bz2` keeps its particle-derived value, and the
  `.not. qUnique` block then sets `ez2 = bz2`, repairing the bad value before anything reads
  it. Hence bit-exact.

So `ez2_GGG = 1` is a genuine defect with a narrow blast radius: periodic meshes, at fewer
than two nodes per rank. It is also not the whole story - setting `ez2_GGG = ez2_act` turns
the clean stop into a segfault inside `free()`, and that site was not isolated.

**What it costs.** A single-cycle periodic run is `nz2 = 2`, so it works on one rank and
fails on any more. That is the case this most obviously blocks. Until it is fixed, periodic
runs need `nz2 >= 2 * nprocs` - use fewer ranks, a smaller `lambdarPerCell`, or more
`sperwaves`. Temporal meshes are unaffected at any rank count.

Fixing it is work on the parallel decomposition rather than a one-liner, and it belongs to
`dev` rather than to this branch, since it changes shared code the unaveraged e2e goldens
depend on. Filed upstream as UKFELs/Puffin#132, with a reproduction on the committed
`inputs/simple/3D/CLARA/single-slice` deck: `nz2 = 21` there, so it runs at 10 ranks and
fails at 11.

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
- **A deck that can discriminate cell size.** Done in 1D — `benchmark/averaged/sase.in`,
  `run_compare_sase.py`, `compare_sase.py`. See below.

### SASE, 1D. Done

The one case every earlier validation was missing: shot noise instead of a seed, run 340
periods to zbar 21.4 (~24 gain lengths) through seven orders of magnitude of growth and
past saturation. Planar, rho = 0.005, the `deck.in` beam with the z2 sampling raised to 8
macroparticles per wavelength and the bunch ends rounded over three resonant wavelengths.
Numbers and tables in `benchmark/averaged/README.md`.

**The averaged mode reproduces SASE.** Against the unaveraged solver at `nodesPerLambdar =
48`, 120 steps: gain length 0.16% (~0.3% against the Richardson limit), saturation power
1.2% low, saturation position identical, in-band field energy 0.5% low at saturation and
within 2% through the exponential regime, spectral centre 1e-5 in `w/wr`, spectral width
0.5% narrow, bunching within 1.6%. 17 s against 756 s — 44x, and 12x against the
unaveraged solver's own default mesh.

**The mesh-refinement series is what makes that a statement about the model.** At the
unaveraged solver's default mesh the averaged gain length is 1.8% short, which compounds
over thirteen gain lengths into an in-band energy ratio of 1.39. Refining the mesh
collapses it: 0.9816, 0.9980, 1.0016 in gain-length ratio over n = 12, 24, 48, with the
spectral centre and width and the bunching converging onto the averaged values in step. A
single-mesh comparison would have charged the unaveraged solver's own discretisation error
to the averaged model, in every observable at once — exactly the trap W2 and the 1D study
already found for power, now confirmed to apply to the growth rate.

**The two runs stay the same shot.** The surprise. With identical shot noise the
normalised overlap of the averaged envelope with the demodulated unaveraged field is
0.9958 at zbar 1.3 and 0.9999 or 1.0000 at every write from zbar 3.8 to saturation. SASE
is chaotic and the expectation was that any difference in the coupling would be amplified
through the gain until the two were different shots with the same statistics. They are
not: the averaged solver reproduces the unaveraged one spike by spike, so the comparison
is pointwise after all and the five-shot ensemble (below) only confirmed it.

**The ensemble puts the bias in context.** Five realisations: the shot-to-shot spread in
gain length is 1.4%, seven times the 0.2% mean difference between the modes, and the two
modes' spreads agree to 1%. The difference is systematic and the same sign every shot,
which is what a paired comparison should give — a small reproducible bias, not scatter.

**Coherent spontaneous emission is not the residual, and averaging keeps more of it than
this programme assumed.** Scanning `sSigEj_G` 40x moves the gain-length and
saturation-power ratios not at all (0.9984/0.9980/0.9981 and 0.9922/0.9931/0.9934 at 0.01,
0.1, 0.4). Sharpening the ends to `sSigEj_G = 0.01` raises the in-band startup energy 31x
— that is CSE at the resonant wavelength — and the averaged mode reproduces it to 0.5%.
The claim "averaged mode discards CSE" needs narrowing: the in-band coherent start is the
fundamental bunching and the envelope equation carries it. What one envelope cannot carry
is the out-of-band part of the edge radiation, 2% of the total at `sSigEj_G = 0.01`. A
square-ended bunch is still untested.

**Early total power is meaningless, and how meaningless depends on the unaveraged mesh.**
The unaveraged field's in-band share at zbar 1.3 is 0.87, 0.70, 0.41 at n = 12, 24, 48 —
a finer mesh resolves more broadband spontaneous emission — so the total-power ratio reads
0.91, 0.70, 0.41 where the in-band ratio reads 1.05, 1.005, 0.996. The out-of-band share
is the unaveraged mesh's resolved bandwidth, not physics. Any SASE comparison has to be
band-limited, which is what `compare_sase.py` does.

**Cell size: the growth rate is insensitive, the saturated field is not.** Gain length
holds to 0.3% out to 8 wavelengths per cell, because the SASE bandwidth is only a few rho
and the spikes are ~200 wavelengths long. But the saturated in-band energy falls 0.8%,
1.0%, 2.1%, 5.8% at 1, 2, 4, 8 λr/cell and the spectral width 0.5%, 0.7%, 1.9%, 5.9%, the
loss being past saturation where the spectrum broadens. So 1-2 λr/cell stays the
recommendation; 4 is tolerable before saturation; 8 is not. One step per period is
converged, as for the seeded deck.

**Two mode-dependent beam differences found and characterised**, both benign and both
previously undocumented. `shuntBeam` (`macroparticle_generation/simple_electron_gen.f90`)
slides the beam to a tenth of a *field* cell above the mesh start, so the two modes put
the same realisation at z2 origins a fixed fraction of a wavelength apart — 0.0909 for
`lambdarPerCell = 1` against `nodesPerLambdar = 12`. Invisible to a seedless temporal-mesh
run, but it is 33 degrees of carrier phase, which alone puts 0.58 into a node-by-node
relative L2 whose real content is 0.09; `compare_sase.py` removes the global phase. And
the macroparticles are reordered to follow each mode's own field decomposition, so on more
than one rank the gathered dumps hold the same particles in a different order. Everything
else — count, weights, energies, relative z2 — is bit-identical at equal `iRandSeed` and
equal rank count, verified at 1 and 4 ranks.

Still open: 3D SASE, a second rho, helical SASE, square bunch ends.

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
- **The `qUnique = .false.` path on a periodic mesh** — see W4. Pre-existing, affects both
  solver modes, and bigger than the rest of this list. Filed as UKFELs/Puffin#132.
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
5. **W6, in part** — 1D SASE, the case every earlier validation was missing. Validated from
   shot noise past saturation against a mesh-refinement series: gain length to 0.16%,
   saturation power to 1.2%, spectral centre to 1e-5, bunching to 1.6%, at 44x the speed.
   The rho scan and the harmonic question are still open and still waiting on W3.

Not done, in suggested order:

6. **W7** — housekeeping. On hold pending a decision on the `calcBuff` rounding; the
   `NBZ2_G` and exit-status items found during W2 and W4 are parked with it.
7. **W3** — multiple field arrays: elliptical first (simpler, two components), then harmonic
   bands. #107 is done, so the storage exists; what remains is the physics and the `getrhs`
   signature change that lets it loop over envelopes.
8. **W6** — close out the accuracy map: the rho scan, the A-term, and the harmonic question
   once W3 makes it answerable. 3D SASE and helical SASE are now the cheapest additions,
   since the 1D harness and its guard rails exist.

Separately, and not part of this programme: **periodic meshes with fewer than two z2 nodes
per rank fail on `dev`** (see W4) — filed as UKFELs/Puffin#132. It affects the unaveraged
solver too, and it is what stands between averaged mode and a single-cycle periodic run on
more than one rank. Left to `dev` rather than carried here, since fixing it means changing
shared parallel code that the unaveraged e2e goldens depend on.

## What would change the picture

Honest limits of what has been shown, rather than open tasks:

- **SASE is validated in 1D, and that changed the picture rather than confirming it.**
  Planar, from shot noise, past saturation: gain length to 0.16%, saturation power to 1.2%,
  spectral centre to 1e-5, bunching to 1.6% against the unaveraged solver at
  `nodesPerLambdar = 48` — see W6. Three things in this list were wrong. The two solvers do
  not merely agree statistically, they stay the *same shot* (overlap 0.9999 through the
  exponential regime), so the ensemble was a check rather than the measurement. Averaging
  does not discard coherent spontaneous emission wholesale: the in-band coherent start is
  the fundamental bunching and the envelope equation has it, verified where sharpening the
  bunch ends raised the startup energy 31x and the two modes tracked it to 0.5%. And SASE
  is *less* cell-size sensitive than the seeded deck, not more — its bandwidth is a few rho
  and its spikes are hundreds of wavelengths long, so the growth rate holds to 0.3% even at
  8 wavelengths per cell. What SASE did expose is that the saturated field, not the growth
  rate, is what a coarse envelope mesh loses.
- **Every 3D validation is helical or plane-pole, seeded, noise-free, and flat-top**, and so
  is every 1D validation but the SASE one. 3D SASE has not been run, nor has a second rho,
  nor helical SASE, nor a bunch with ends sharp on the carrier scale. The `sSigEj_G` scan
  shows the residual does not track the edge length over a 40x range, not that a square end
  would be carried.
- **The unaveraged reference is itself still drifting** wherever these residuals are quoted.
  At `nodesPerLambdar = 48` the SASE gain length still moves 0.4% per mesh halving, so the
  0.16% is a limit approached, not a converged value — the same caveat the seeded study's
  ~1% planar remainder carries, and the two are the same size.
- **The seed has to be a polarisation one envelope can hold.** Not a limit so much as a trap,
  and the one that produced the only wrong finding in this programme: a linear seed on a
  helical undulator silently loses half its power, because `u+ = 0` there and the opposite
  helicity cannot be represented. `getSeed` warns now, and `compare3d.py` checks the ratio at
  zbar = 0. A comparison that does not start at 1.0 is not a comparison.
- **Undulator ends are untested in 3D and across modules.** `getAvgEnvelope` uses the `by`
  ramp for both components, while the helical `bx` ramp has a different shape — a phase
  difference that would recur at every module boundary with `qUndEnds` on, rather than once.
- **Saturation is not validated, and should not be expected to agree.** The 3D deck is run
  far enough to start rolling over precisely so the divergence is visible in the series.
