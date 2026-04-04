# Puffin Global Variables → Derived Types Migration Roadmap

## Overview

This document outlines the refactoring of global variables in `EDerivGlobals.f90` into
organized Fortran derived types. The goal is to improve code maintainability, reduce global
namespace pollution, and make data dependencies explicit.

**Current State:** Phases 0–7 complete. Derived types exist and adapters work.
**Target State:** Simulation-lifetime types owned by `puffin_main`; passed as arguments to
all element subroutines; globals eliminated.

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
| 7b | 🔜 NEXT | Split tFELPhysics | Replace with `tFELFrame` + `tUndulator` |
| 8 | 🔜 | Architectural lift | Types owned by `puffin_main`, passed to all element routines |
| 9 | 🔜 | Global removal | Remove globals unlocked by Phase 8 |
| 10 | 🔜 | Naming cleanup | Optional: rename fields to domain-meaningful names |

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
| `m2col` | `m2col` | Legacy field |

**Scope:** local to `UndSection` — re-populated by `initUndulator()` on each call.

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

## Phase 9: Remove Remaining Globals (Unlocked by Phase 8)

Once types are at `puffin_main` scope and callees receive types as arguments, globals that
were "blocked" in the Phase 7 audit can be removed:

- `iStep`, `iCsteps` — thread via `tIntegrationState` / `tLatticeElements`
- `igwr`, `sZi_G` — now in `tFieldMesh` / `tIntegrationState`
- `iUnd_cr` — indexable via `tLatticeElements`
- `n2col`, `n2col0` — in `tUndulator`, mutated in place
- `qPArrOK_G`, `qInnerXYOK_G` — in `tSimulationFlags`, mutated in place
- HDF5-writer globals: `qhdf5_G`, `qSeparateStepFiles_G`, `zFileName_G`, etc.

---

## Phase 10: Naming Cleanup (Optional, Low Risk)

Current type field names were chosen to match the global names they replaced. Now that types
are properly scoped, rename fields to domain-meaningful names (e.g.
`integration%redistribution_length` → `integration%redistrib_len`).

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

## Remaining Globals Audit (from Phase 7)

These globals remain in `UndSection` and cannot be removed until Phase 8 threads types
through the relevant callees:

| Variable | Why it remains | Phase that removes it |
|----------|----------------|-----------------------|
| `iStep` | read by HDF5 writers for output filenames | 8/9 |
| `iCsteps` | read by HDF5 writers for dataset writes | 8/9 |
| `igwr` | read by HDF5 writers | 8/9 |
| `sZi_G` | read by HDF5 writers for z-coordinate output | 8/9 |
| `iUnd_cr` | read by HDF5 writers for undulator index | 8/9 |
| `end_time`, `start_time` | read by diffraction.f90 | 8/9 |
| `n2col`, `n2col0` | modified by wiggler_taper mid-loop | 9 |
| `qPArrOK_G`, `qInnerXYOK_G` | set by rk4par callees mid-loop | 9 |
