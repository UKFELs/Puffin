# Puffin Global Variables → Derived Types Migration Roadmap

## Overview

This document outlines the refactoring of global variables in `EDerivGlobals.f90` into
organized Fortran derived types. The goal is to improve code maintainability, reduce global
namespace pollution, and make data dependencies explicit.

**Current State:** Phases 0–10 complete. `tSimulationContext` adopted; ctx threaded through RK4
chain, HDF5/write chain, and init path; element-counter and flag globals removed.
**Target State:** All remaining globals eliminated (init-only physics globals, HDF5 timing globals).

---

## Executive Summary — Phase Status

| Phase | Status | Scope | Key Deliverable |
|-------|--------|-------|-----------------|
| 0 | ✅ DONE | Type definitions | `GlobalTypes.f90` with 8 derived types |
| 1 | ✅ DONE | Adapter layer | `AdapterGlobals.f90` with populate/update functions |
| 2 | ✅ DONE | Integration state | `tIntegrationState` in `UndSection` |
| 3 | ✅ DONE | Lattice elements | `tLatticeElements` in `UndSection` |
| 4 | ✅ DONE | Field mesh | `tFieldMesh` in `UndSection` |
| 5 | ✅ DONE | FEL physics | `tFELPhysics` in `UndSection` |
| 6 | ✅ DONE | Flags & IO | `tSimulationFlags` & `tOutputConfig` in `UndSection` |
| 7 | ✅ DONE | Final cleanup | `qResume_G`→local, `tInitData_G`→local; remaining globals documented |
| 7b | ✅ DONE | Split tFELPhysics | Replace with `tFELFrame` + `tUndulator` |
| 8 | ✅ DONE | Architectural lift | Types owned by `puffin_main`, passed to all element routines |
| 9 | ✅ DONE | RK4 chain threading | `tUndulator`/`tFELFrame`/`tSimulationFlags` threaded through full RK4 chain; dead `m2col` removed |
| 10 | ✅ DONE | tSimulationContext + init path | ctx adopted; threaded through write chain, flags chain, init; dead globals removed |
| 11 | 🔜 NEXT | Remove remaining globals | Remove `iCsteps`, `igwr`, `sZi_G`, physics globals (`sRho_G` etc.) |

---

## Architectural Insight: Phases 2–7 Hit the Wrong Scope Boundary

Phases 2–7 proved that the adapter pattern works and successfully scoped six derived types
into `UndSection`. However, this approach has reached a structural ceiling.

**Root cause:** `UndSection` is not the main loop — it is one of five element handlers
called by `puffin_main`. Types scoped *inside* `UndSection` cannot be passed to HDF5
writers or sibling element subroutines (`disperse`, `driftSection`, `Quad`, `BModulation`).
This is why many globals "cannot be removed" — they are blocked because the types live at
the wrong level.

```
puffin_main (puffin_module.f90)
  do iL = 1, modNum
    iUnd  → UndSection(iL, sZ)        ← only one of five handlers
    iChic → disperse(iL, sZ)
    iDrift → driftSection(iL, sZ)
    iQuad → Quad(iL)
    iMod  → BModulation(iL)
  end do
```

**Corrected architecture:** simulation-lifetime types are created and owned by `puffin_main`,
then passed as arguments to every element subroutine. This eliminates the populate/update
round-trip and allows globals to be removed because callees receive the type directly.

---

## Phase 7b: Split tFELPhysics → tFELFrame + tUndulator

### Objective
Replace the monolithic `tFELPhysics` type with two correctly-scoped types before the
architectural lift in Phase 8.

### tFELFrame — simulation scaling frame (immutable for the entire run)

| Field | Global | Note |
|-------|--------|------|
| `rho` | `sRho_G` | Pierce parameter |
| `aw` | `sAw_G` | Undulator strength (reference) |
| `gamma_ref` | `sGammaR_G` | Reference Lorentz factor |
| `eta` | `sEta_G` | Detuning parameter |
| `kappa` | `sKappa_G` | Coupling strength |
| `lambda_w` | `lam_w_G` | Wiggler period |
| `lambda_r` | `lam_r_G` | Resonant wavelength |
| `gain_length` | `lg_G` | e-folding gain length |
| `cooperation_length` | `lc_G` | Cooperation/slippage length |
| `coefficient_1` | `cf1_G` | Setup scaling coefficient |

**Scope:** `puffin_main` (populated once after `init()` in Phase 8).

### tUndulator — properties of the current undulator element (reset per element)

| Field | Global | Note |
|-------|--------|------|
| `undulator_type` | `zUndType_G` | Per-element |
| `kx_undulator`, `ky_undulator` | `kx_und_G`, `ky_und_G` | Off-axis wavenumbers |
| `k_beta_x_sf`, `k_beta_y_sf` | `sKBetaXSF_G`, `sKBetaYSF_G` | Scaled beta (per-element) |
| `k_beta`, `k_beta_x`, `k_beta_y` | `sKBeta_G`, `sKBetaX_G`, `sKBetaY_G` | Kept for now |
| `focus_factor`, `focus_factor_saved` | `sFocusfactor_G` | Natural focusing |
| `fx`, `fy` | `fx_G`, `fy_G` | Polarisation focusing |
| `beta_absorption` | `sBeta_G` | Per-element absorption |
| `model_undulator_ends` | `qUndEnds_G` | Entrance/exit corrections |
| `z_start_undulator`, `z_end_undulator` | `sZFS`, `sZFE` | Ends geometry |
| `undulator_position` | `iUndPlace_G` | Position marker |
| `n2col_initial` | `n2col0` | Field strength at entry |
| `n2col` | `n2col` | Current field strength (evolves via taper) |
| `undulator_gradient` | `undgrad` | Taper rate |
| `z_taper_start` | `sz0` | z reference for taper |
**Scope:** local to `UndSection` — re-populated by `initUndulator()` on each call.

Note: `m2col` was removed in Phase 9 as dead code (never read or written in any computation).

### Files to change
- `puffin/lib/global_types.f90` — replace `tFELPhysics` with `tFELFrame` + `tUndulator`
- `puffin/lib/adapter_globals.f90` — replace `FELPhysics` adapters with adapters for both new types
- `puffin/lib/undulator.f90` — rename `physics` local to `und` of `type(tUndulator)`

---

## Phase 8: Lift Types to puffin_module Scope

### Objective
Create all simulation-lifetime types in `puffin_main`, initialize once, pass as arguments
to every element subroutine. This eliminates the populate/update round-trip for those types.

### Types that are simulation-lifetime (belong in puffin_main)

| Type | Reason |
|------|--------|
| `tFieldMesh` | Grid geometry set once at init; read by all elements + HDF5 writers |
| `tFELFrame` | Scaling frame set once; read by all elements and callees |
| `tSimulationFlags` | Control switches set once; checked everywhere |
| `tOutputConfig` | Filenames/frequencies set once; used by write infrastructure |
| `tLatticeElements` | Master element list; each element indexes into it |

### Types that are element-local (stay local to UndSection)

| Type | Reason |
|------|--------|
| `tIntegrationState` | Per-undulator step counter; re-initialized for each UndSection call |
| `tUndulator` | Per-element undulator properties; re-initialized by `initUndulator()` |

### Steps

1. **Add type declarations to `puffin_module.f90`**
   - Declare `mesh`, `frame`, `flags`, `output`, `lattice` after `use` statements
   - Call `PopulateXxxFromGlobals(xxx)` once, immediately after `init()` returns
   - Pass all types as `intent(inout)` arguments into the element dispatch loop

2. **Update element subroutine signatures**
   - `UndSection(iM, sZ, mesh, frame, flags, output, lattice)` — receive simulation-lifetime types; `tUndulator` and `tIntegrationState` remain local
   - `disperse(iL, sZ, lattice, frame, flags)` — currently reads all as globals
   - `driftSection(iL, sZ, lattice, frame, flags)` — same
   - `Quad(iL, lattice, frame, flags)` — same
   - `BModulation(iL, lattice)` — same

3. **Remove populate/update boilerplate from UndSection**
   - Delete populate calls for simulation-lifetime types; keep for `tIntegrationState` and `tUndulator`
   - Replace re-sync hacks with direct type mutations

4. **Thread types through callees that currently read globals**
   - `writeIM` / `wr_cho` receive `output` → removes global reads of `qhdf5_G` etc.
   - `diffractIM` receives `flags` or `mesh` → removes `qDiffraction_G`, `NX_G` reads

5. **Update `UpdateGlobalsFromXxx` calls**
   - Simulation-lifetime types no longer need Update calls from UndSection
   - Only `UpdateGlobalsFromUndulator` and `UpdateGlobalsFromIntegrationState` remain in UndSection

### Key files to modify
- `puffin/lib/puffin_module.f90` — primary: declare & own types, pass to dispatch
- `puffin/lib/undulator.f90` — remove local declares for simulation-lifetime types
- `puffin/lib/acc_lattice.f90` — update signatures for element subroutines
- `puffin/lib/adapter_globals.f90` — populate calls move to puffin_module; update calls removed for simulation-lifetime types

---

## Phase 9: Thread Types Through RK4 Integration Chain ✅ DONE

### Objective
Thread `tUndulator`, `tFELFrame`, and `tSimulationFlags` through the full RK4 call chain
so the integration loop no longer reads undulator/frame globals directly.

### Call chain threaded

```
UndSection
  └─ rk4par(und inout, frame in, flags inout)       [puffin_mpi_RK4.f90]
       └─ derivs(und inout, frame in, flags inout)   [derivative.f90]
            └─ getrhs(und inout, frame in)           [rhs.f90]
                 ├─ rhs_tmsavers(und in, frame in)   — replaces sRho_G, sAw_G, sEta_G, fx_G, fy_G etc.
                 ├─ getAlpha(sZ, und)                [wiggler_taper.f90] — writes und%n2col, und%n2col_initial
                 ├─ adjUndPlace(sZ, und)             [puffin_equations.f90] — writes und%undulator_position
                 ├─ getBFields(..., und, frame)       [bfields.f90] — reads all und/frame fields
                 └─ dppdz_r/i_f, dgamdz_f,
                    dxdz_f, dydz_f(..., und, frame)  [puffin_equations.f90]
```

### Key design decisions

- `qPArrOK_G` / `qInnerXYOK_G` are still written by `system_interpolation.f90` and
  `para_field.f90` (not modified in Phase 9). `derivs` syncs `flags%xxx = qXxx_G` after
  each `getrhs` call, does the allreduce on the flags fields, and writes back to both
  `flags%xxx` and the globals (keeping them in sync for the unthreaded callees).
- `und intent(inout)` is required in `getrhs` because `getAlpha` and `adjUndPlace`
  mutate `und%n2col`, `und%n2col_initial`, and `und%undulator_position`.
- Post-loop sync hacks in `undulator.f90` (`und%n2col = n2col` etc.) removed — values
  are now updated in-place on `und` throughout the loop.
- Dead field `m2col` removed from `tUndulator`, both adapters, and `deriv_globals.f90`.

### Globals removed / partially decoupled

| Global | Replaced by | Status |
|--------|-------------|--------|
| `n2col` | `und%n2col` (updated by `getAlpha`) | ✅ Removed from integration chain |
| `n2col0` | `und%n2col_initial` | ✅ Removed from integration chain |
| `iUndPlace_G` | `und%undulator_position` | ✅ Removed from integration chain |
| `undgrad` | `und%undulator_gradient` | ✅ Removed from integration chain |
| `sz0` | `und%z_taper_start` | ✅ Removed from integration chain |
| `sZFS`, `sZFE` | `und%z_start_undulator`, `und%z_end_undulator` | ✅ Removed from integration chain |
| `qUndEnds_G` | `und%model_undulator_ends` | ✅ Removed from integration chain |
| `zUndType_G` | `und%undulator_type` | ✅ Removed from integration chain |
| `kx_und_G`, `ky_und_G` | `und%kx_undulator`, `und%ky_undulator` | ✅ Removed from integration chain |
| `fx_G`, `fy_G` | `und%fx`, `und%fy` | ✅ Removed from integration chain |
| `sKBetaXSF_G`, `sKBetaYSF_G` | `und%k_beta_x_sf`, `und%k_beta_y_sf` | ✅ Removed from integration chain |
| `sRho_G` | `frame%rho` | ✅ Removed from integration chain |
| `sAw_G` | `frame%aw` | ✅ Removed from integration chain |
| `sGammaR_G` | `frame%gamma_ref` | ✅ Removed from integration chain |
| `sEta_G` | `frame%eta` | ✅ Removed from integration chain |
| `sKappa_G` | `frame%kappa` | ✅ Removed from integration chain |
| `qPArrOK_G` | `flags%parallel_arrays_ok` | ✅ Deleted in Phase 10 |
| `qInnerXYOK_G` | `flags%inner_xy_ok` | ✅ Deleted in Phase 10 |
| `m2col` | — | ✅ Deleted (dead code) |

Note: globals listed as "removed from integration chain" still exist in `deriv_globals.f90`
because initialization code (`acc_lattice.f90`, `init_conds.f90`, `setup_calcs.f90`) still
writes them, and `PopulateUndulatorFromGlobals` / `UpdateGlobalsFromUndulator` bridge the
gap. Full removal requires threading types through the initialization path.

---

## Phase 10: Adopt tSimulationContext + Thread Init Path ✅ DONE

### Objective
Bundle all 8 simulation types into a single `tSimulationContext ctx` owned by `puffin_main`,
thread it through the HDF5/write chain and the init path, and remove the now-dead element-counter
and flag globals.

### Steps completed

**Step 0 — Add element counters and z_inter to existing types**
- Added `current_xxx_index` fields to `tLatticeElements` (chic, drift, quad, modulation)
- Added `z_inter` to `tIntegrationState`
- Updated adapters accordingly

**Step 1 — Adopt ctx in puffin_main and element subroutine signatures**
- `puffin_main` now declares a single `type(tSimulationContext) :: ctx`
- All 5 element subroutines (`UndSection`, `Quad`, `disperse`, `driftSection`, `BModulation`)
  take `ctx` as their sole simulation-data argument
- `rk4par`, `derivs`, `getrhs` updated to take `ctx` (replaces separate `und/frame/flags` args)

**Step 2 — Thread ctx through the HDF5/write path**
- `writeIM` / `wr_cho` / `writeCommonAtts` / `writeRunAtts` receive `ctx`
- Replaced ~25 global reads in the HDF5 writers with `ctx%xxx` field accesses
  (including `iCsteps`, `igwr`, `sZi_G`, `iUnd_cr`, all five element counters,
  and the `sRho_G`, `sAw_G`, ... physics frame fields)

**Step 3 — Thread flags through para_field and system_interpolation**
- `getLocalFieldIndices(sdz, flags)` and `getInNode(flags)` take explicit `flags` arg
- `getInterps_1D(sz2, flags)` and `getInterps_3D(sx, sy, sz2, flags)` write directly to
  `flags%parallel_arrays_ok` / `flags%inner_xy_ok` — no global→flags sync needed in `derivs`

**Step 4 — Thread init functions to write directly to ctx**
- All `PopulateXxxFromGlobals` calls moved inside `init()` (unconditional, covers both resume
  and non-resume paths); `puffin_main` no longer calls any Populate functions after `init()`

**Step 5 — Remove dead globals (Clusters A and E)**
- **Cluster E** (`qPArrOK_G`, `qInnerXYOK_G`): removed all "keep in sync" writes and
  declarations; `PopulateSimulationFlagsFromGlobals` now initialises both to `.true.`
- **Cluster A** (`iUnd_cr`, `iChic_cr`, `iDrift_cr`, `iQuad_cr`, `iModulation_cr`):
  removed module-variable declarations and initialisations from `acc_lattice.f90`; removed
  four sync writes in element routines; `PopulateLatticeElementsFromGlobals` hardcodes
  counters to 1; resume override added after Populate block in `setup.f90`

### Globals removed / partially decoupled in Phase 10

| Global | Replaced by | Status |
|--------|-------------|--------|
| `qPArrOK_G` | `ctx%flags%parallel_arrays_ok` | ✅ Deleted |
| `qInnerXYOK_G` | `ctx%flags%inner_xy_ok` | ✅ Deleted |
| `iUnd_cr` | `ctx%lattice%current_und_index` | ✅ Deleted |
| `iChic_cr` | `ctx%lattice%current_chic_index` | ✅ Deleted |
| `iDrift_cr` | `ctx%lattice%current_drift_index` | ✅ Deleted |
| `iQuad_cr` | `ctx%lattice%current_quad_index` | ✅ Deleted |
| `iModulation_cr` | `ctx%lattice%current_modulation_index` | ✅ Deleted |
| `iCsteps` | `ctx%lattice%cumulative_steps` | ⚠️ HDF5 writers use ctx; global still set for compat |
| `igwr` | `ctx%mesh%highpass_filter_gr` | ⚠️ HDF5 writers use ctx; global still set for compat |
| `sZi_G` | `ctx%integration%z_inter` | ⚠️ HDF5 writers use ctx; global still set for compat |
| `sRho_G`, `sAw_G`, ... | `ctx%frame%rho`, `ctx%frame%aw`, ... | ⚠️ HDF5 writers use ctx; globals still written by init code |

---

## Phase 11: Remove Remaining Globals

### Remaining globals that cannot be removed yet

| Variable | Why it remains | Blocker |
|----------|----------------|---------|
| `iCsteps` | Still written in `acc_lattice.f90:setupMods` and sync'd in `undulator.f90`; HDF5 writers now use ctx | Need to remove sync writes |
| `igwr` | Set in `undulator.f90` resume path; HDF5 writers now use ctx | Need to remove sync writes |
| `sZi_G` | Set in `undulator.f90` resume path; HDF5 writers now use ctx | Need to remove sync writes |
| `sRho_G`, `sAw_G`, `sGammaR_G`, `sEta_G`, `sKappa_G` | Written by `calcScaling`/`setup_calcs.f90`; read by `init_conds.f90`, `gen_macros.f90`, `MPfDists.f90` | Thread ctx through init/generation path |
| `lam_w_G`, `lam_r_G`, `lg_G`, `lc_G`, `cf1_G` | Written by `calcScaling`; read throughout init | Thread ctx through init |
| `end_time`, `start_time` | Timing globals read by `diffraction.f90` | Minor; move to local vars |

---

## Verification (every phase)

```
make -C build -j4
ctest --output-on-failure
```

Both `puffin_basic_tests` (unit) and `puffin_e2e_tests` (MPI, 2 processes) must pass.
The e2e test verifies numerical results to 1e-10 tolerance.

---

## Key Files Reference

| File | Role |
|------|------|
| `puffin/lib/global_types.f90` | All derived type definitions |
| `puffin/lib/adapter_globals.f90` | Populate/Update adapters |
| `puffin/lib/undulator.f90` | Main integration loop (UndSection) |
| `puffin/lib/puffin_module.f90` | Top-level puffin_main loop |
| `puffin/lib/acc_lattice.f90` | Element subroutines (disperse, drift, quad, BModulation) |
| `puffin/lib/deriv_globals.f90` | All global variables |

---

## Remaining Globals Audit (after Phase 10)

| Variable | Why it remains | Phase that removes it |
|----------|----------------|-----------------------|
| `iCsteps` | written by `setupMods`; sync write in `undulator.f90`; HDF5 writers already use ctx | 11 |
| `igwr` | set in `undulator.f90` resume path; HDF5 writers already use ctx | 11 |
| `sZi_G` | set in `undulator.f90` resume path; HDF5 writers already use ctx | 11 |
| `end_time`, `start_time` | timing globals read by `diffraction.f90` | 11 |
| `sRho_G`, `sAw_G`, `sGammaR_G`, `sEta_G`, `sKappa_G` | written by `calcScaling`; read by `init_conds.f90`, `gen_macros.f90`, `MPfDists.f90` | 11 |
| `lam_w_G`, `lam_r_G`, `lg_G`, `lc_G`, `cf1_G` | written by `calcScaling`; read throughout init | 11 |
| `qPArrOK_G`, `qInnerXYOK_G` | ✅ Deleted in Phase 10 | — |
| `iUnd_cr`, `iChic_cr`, `iDrift_cr`, `iQuad_cr`, `iModulation_cr` | ✅ Deleted in Phase 10 | — |
