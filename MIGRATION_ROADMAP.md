# Puffin Global Variables → Derived Types Migration Roadmap

## Overview

This document outlines a systematic approach to refactor the global variables in `EDerivGlobals.f90` into organized Fortran derived types. The goal is to improve code maintainability, reduce global namespace pollution, and make data dependencies explicit.

**Current State:** 100+ global variables scattered across Globals module
**Target State:** 8 organized derived types bundled into tSimulationContext

---

## Executive Summary - Phase Breakdown

| Phase | Duration | Risk | Scope | Key Deliverable |
|-------|----------|------|-------|-----------------|
| 0 | ✅ DONE | Low | Create GlobalTypes.f90 | Type definitions module |
| 1 | ~3-4 days | Low | Field mesh & helper code | Adapter functions & first migrated modules |
| 2 | ~4-5 days | Low-Medium | Lattice elements & integration | Element management refactored |
| 3 | ~5-7 days | Medium | Electron phase space | Core physics code updated |
| 4 | ~3-4 days | Medium | Physics parameters | FEL math preserved but typed |
| 5 | ~2-3 days | Low | Flags and IO | Remaining scattered variables |
| 6 | ~2-3 days | High | Final migration | Complete removal of untyped globals |
| 7 | ~1-2 days | Medium | Testing & validation | Full test suite passes |

**Total Effort:** ~3-4 weeks of development work

---

## Phase 0: Foundation (COMPLETED)

### Deliverables
- ✅ Created `GlobalTypes.f90` with 8 derived types:
  - `tFieldMesh` - Grid dimensions, wavenumbers, element sizes
  - `tElectronCloud` - 6D particle coordinates and metadata
  - `tFELPhysics` - Undulator/wiggler parameters and derived quantities
  - `tLatticeElements` - All lattice element arrays organized by type
  - `tIntegrationState` - Integration loop control variables
  - `tOutputConfig` - IO configuration and file handling
  - `tSimulationFlags` - Boolean control flags
  - `tSimulationContext` - Master type bundling all above

### What to do next
- Build the code with GlobalTypes included
- Ensure no compilation errors before proceeding to Phase 1

---

## Phase 1: Create Adapter/Wrapper Layer (LOW RISK)

### Objective
Establish a safe migration path by creating adapter functions that convert between globals and types without changing existing code.

### Approach
1. **Create AdapterGlobals module** with read/write subroutines:
   ```fortran
   ! File: AdapterGlobals.f90 (new)
   module AdapterGlobals
   ! Helper subroutines to populate types from globals and vice versa
   contains
       subroutine PopulateFieldMeshFromGlobals(mesh)
       subroutine PopulateElectronCloudFromGlobals(electrons)
       subroutine UpdateGlobalsFromFieldMesh(mesh)
       subroutine UpdateGlobalsFromElectronCloud(electrons)
   end module
   ```

2. **Key adapter subroutines for Phase 1:**

   ```fortran
   subroutine PopulateFieldMeshFromGlobals(mesh)
       type(tFieldMesh), intent(inout) :: mesh
       mesh%nx = NX_G
       mesh%ny = NY_G
       mesh%nz2 = NZ2_G
       mesh%nbx = NBX_G
       mesh%nby = NBY_G
       mesh%nbz2 = NBZ2_G
       if (allocated(mesh%kx)) deallocate(mesh%kx)
       allocate(mesh%kx(size(kx_G)))
       mesh%kx = kx_G
       ! ... etc for all mesh fields
   end subroutine
   ```

3. **Identify module entry points** - Find initialization points where globals are first set:
   - `Msetup.f90` - Allocates and initializes field mesh
   - `FreadData.f90` - Reads configuration from input files
   - `simple_electron_gen.f90` - Generates electron distribution

### Candidate Modules for Immediate Wrapping (Leaf Modules)
These have few dependencies and are good candidates to be wrapped first:

1. **ArrayFunctions.f90** - Utility functions (very low risk)
   - No user-facing globals, mostly self-contained
   - Action: Minimal changes, add dependency on GlobalTypes

2. **puffin_kinds.f90** - Type definitions (zero risk)
   - No changes needed, just ensure GlobalTypes imports this

3. **puffin_constants.f90** - Mathematical/physical constants (zero risk)
   - No changes needed, ensure GlobalTypes can use it

### Files to Create
- `AdapterGlobals.f90` - Adapter/wrapper module with conversion functions
  - PopulateFieldMeshFromGlobals()
  - PopulateElectronCloudFromGlobals()
  - PopulateIntegrationStateFromGlobals()
  - UpdateGlobalsFromFieldMesh()
  - UpdateGlobalsFromIntegrationState()

### Testing Strategy
- Compile with GlobalTypes and AdapterGlobals
- No behavioral changes expected
- Run existing tests to ensure baseline works

### Files to Modify
- Update `CMakeLists.txt` or build system to include GlobalTypes.f90 and AdapterGlobals.f90
- Update all module dependencies to `use GlobalTypes`

---

## Phase 2: Migrate Integration & Stepping (LOW-MEDIUM RISK)

### Objective
Migrate integration control variables from globals to `tIntegrationState` type.

### Why Start Here
- Integration state is **self-contained** (doesn't feed back into many modules)
- Used primarily in:
  - `undulator.f90` - Main integration loop
  - `MRK4.f90` - RK4 stepping
  - `choWrite.f90` - Decides when to write output

### Migration Steps

1. **Identify all integration variable uses:**
   ```
   iStep, nSteps, start_step, sStep, sStepSize, iCount
   start_time, end_time, time1, time2
   diffStep, sRedistLen_G, iRedistStp_G, totUndLineLength
   ```

2. **Create IntegrationState initialization in Msetup.f90:**
   ```fortran
   type(tIntegrationState) :: integration
   ! Initialize from globals at program start
   call PopulateIntegrationStateFromGlobals(integration)
   ```

3. **Update function signatures in order:**
   - `undulator.f90`: Main integration loop
     ```fortran
     ! OLD: function rk4par(sZl, sStepSize, qDiffrctd)
     ! NEW: function rk4par(sZl, integration, qDiffrctd)
     ```
   - `MRK4.f90`: RK4 kernel
     ```fortran
     ! Pass integration%step_size instead of reading global sStepSize
     ```
   - `choWrite.f90`: Write decision logic
     ```fortran
     ! Pass integration%write_nth_steps instead of global iWriteNthSteps
     ```

4. **Create wrapper versions before full migration:**
   - Keep old globals active
   - Add optional integration argument to functions
   - Auto-populate from globals if not provided (for backwards compatibility)

### Key Files to Modify
1. `Msetup.f90` - Create and initialize integration state
2. `undulator.f90` - Main loop, pass to callees
3. `MRK4.f90` - Accept integration parameter
4. `choWrite.f90` - Accept integration parameter
5. `Jdatawrite.f90` - Any timing/step info handling

### Testing
- Compile with modified files
- Run simulation with various step counts
- Verify output matches previous runs (numerical results should be identical)
- Check timing information is correct

### Rollback Plan
If issues arise, these modules are isolated - can revert just Phase 2 changes and continue with other phases.

---

## Phase 3: Migrate Lattice Elements (LOW-MEDIUM RISK)

### Objective
Migrate all lattice element arrays into `tLatticeElements` type.

### Why This Phase
- **Clear data organization:** Each element type has its own array set
- **Limited coupling:** Lattice is mostly read during setup, used in element selection
- **Reduces global array proliferation:** Currently have 15+ allocatable arrays

### Migration Steps

1. **Audit lattice array usage:**
   - Find all references to: zMod, mf, delmz, tapers, ux_arr, uy_arr, kbnx_arr, kbny_arr
   - Find references to: chic_zbar, chic_slip, chic_disp
   - Find references to: drift_zbar, enmod_wavenum, enmod_mag, quad_fx, quad_fy

2. **Create LatticeElements initialization in Lattice.f90:**
   ```fortran
   type(tLatticeElements) :: lattice
   ! Read from input files into lattice type instead of globals
   ```

3. **Update function signatures:**
   - `Lattice.f90`: Element initialization routines
   - `SetupLattice()` - Initialize lattice structure
   - `initUndulator()` - Read undulator array data into lattice%und_* fields
   - `initChicane()` - Read chicane data into lattice%chic_* fields
   - `Element selection routines` - Use lattice%current_module index

4. **Scaling transformations:**
   - In `Lattice.f90::prepLattice()` or similar:
     ```fortran
     ! OLD:
     kbnx_arr = kbnx_arr * lg_G
     kbny_arr = kbny_arr * lg_G

     ! NEW:
     lattice%und_kbx = lattice%und_kbx * physics%gain_length
     lattice%und_kby = lattice%und_kby * physics%gain_length
     ```

### Key Files to Modify
1. `Lattice.f90` - Core lattice management (allocate, initialize, access)
2. `undulator.f90` - Switch element types using lattice
3. `Jsetupcalcs.f90` - Apply scaling to lattice arrays
4. `simple_electron_gen.f90` - Read beam parameters (if lattice-dependent)

### Testing
- Parse input file with multiple lattice elements
- Verify correct element selection during integration
- Check field values match pre-migration values

---

## Phase 4: Migrate Field Mesh (MEDIUM RISK)

### Objective
Migrate field grid variables from globals to `tFieldMesh` type.

### Why This Phase (not earlier)
- **More widespread usage:** Used in FFT routines, field access patterns
- **Benefits from Phase 1-3:** Reduces field indices when steps/lattice migrate
- **Depends on Phase 2:** Integration variables may be needed for dimensions

### Migration Steps

1. **Create FieldMesh initialization in Msetup.f90:**
   ```fortran
   type(tFieldMesh) :: mesh
   mesh%nx = NX_G
   mesh%ny = NY_G
   mesh%nz2 = NZ2_G
   allocate(mesh%kx(size(kx_G)))
   mesh%kx = kx_G
   ! etc.
   ```

2. **Update field-heavy modules in order:**
   - `JFiElec.f90` - Electron-field interpolation (HIGH USE)
     ```fortran
     ! OLD: Uses NX_G, NY_G, NZ2_G directly
     ! NEW: function electron_field_coupling(electrons, mesh, ...)
     ```
   - `para_field.f90` - Parallel field communication
     ```fortran
     ! Pass mesh for dimensions in MPI calls
     ```
   - `Ltransforms.f90` - Coordinate transforms
     - Uses kx_G, ky_G, kz2_loc_G
     - Pass mesh instead
   - `GEquations.f90` - Field equation RHS
   - `hdf5_puff.f90` - HDF5 data writing

3. **Update FFT wrapper calls:**
   - FFTW calls reference NX_G, NY_G, NZ2_G
   - Create FFT plan with mesh dimensions instead

4. **Field axis arrays:**
   - x_ax_G, y_ax_G used for integration/plotting
   - Store in mesh%x_axis, mesh%y_axis

### Dependency Graph for This Phase
```
Msetup.f90 (initialize mesh) →
    ├─ JFiElec.f90 (most critical)
    ├─ para_field.f90
    ├─ Ltransforms.f90
    ├─ GEquations.f90
    └─ hdf5_puff.f90
```

### Key Files to Modify
1. `Msetup.f90` - Populate mesh structure from input file
2. `JFiElec.f90` - Pass mesh to electron coupling
3. `para_field.f90` - Use mesh for MPI distribution
4. `FFT wrapper` - Use mesh dimensions
5. `GEquations.f90` - Use mesh in coefficient calculations
6. `hdf5_puff.f90` - Use mesh for array sizing

### Testing
- Verify electron-field interpolation produces same results
- Check parallel field gathering/scattering works
- Validate FFT output matches previous runs
- Check HDF5 file dimensions

---

## Phase 5: Migrate Physics Parameters (MEDIUM RISK)

### Objective
Migrate FEL physics parameters to `tFELPhysics` type.

### Why This Phase (order matters)
- **Must follow Phase 4:** Field mesh needs to be migrated first
- **Must follow Phase 2:** Integration stepping doesn't depend on physics parameters, but good to clear that first
- **Physics parameters used throughout:** Once migrated, many equations simplify

### Critical Operations on Physics Parameters
Analyzed from `Jsetupcalcs.f90`:

```
sEta_G = (1.0 - sbetaz) / sbetaz
sKappa_G = aw / 2 / srho / sgamr
cf1_G = sEta_G / sKappa_G^2
sKBetaX_G = aw / sqrt(2*sEta_G) / sGammaR_G * kx_und_G
lam_r_G = slam_w * sEta_G
lg_G = lam_r_G / (2.0 * pi * srho)
lc_G = lam_r_G / (2.0 * sqrt(2.0 * srho))
```

### Migration Steps

1. **Create physics initialization routine in Jsetupcalcs.f90:**
   ```fortran
   subroutine InitializePhysics(physics, input_data)
       type(tFELPhysics), intent(inout) :: physics
       ! Read sRho_G, sAw_G, sGammaR_G
       physics%rho = sRho_G
       physics%aw = sAw_G
       physics%gamma_ref = sGammaR_G
       ! Compute derived quantities
       physics%eta = (1.0 - sbetaz) / sbetaz
       physics%kappa = physics%aw / (2 * physics%rho * physics%gamma_ref)
       ! etc.
   end subroutine
   ```

2. **Audit all files using physics parameters:**
   - Search for: sRho_G, sAw_G, sEta_G, sKappa_G, sKBeta_G
   - Main users:
     - `GEquations.f90` - Uses in algebraic field equations
     - `Jrhs.f90` - Right-hand side derivatives
     - `KDerivative.f90` - Orchestrates derivatives
     - `CinitConds.f90` - Initial conditions using sRho_G, sAw_G, sGammaR_G
     - `simple_electron_gen.f90` - Beam energy scaled by sGammaR_G

3. **Update signatures (from users → Jsetupcalcs.f90):**
   ```fortran
   ! OLD in GEquations.f90:
   function GetFieldCoefficient() result(coeff)
       coeff = 1.0 / (sRho_G * sKappa_G)
   end function

   ! NEW:
   function GetFieldCoefficient(physics) result(coeff)
       type(tFELPhysics), intent(in) :: physics
       coeff = 1.0 / (physics%rho * physics%kappa)
   end function
   ```

4. **Update lattice/physics connections:**
   - Currently, tapering modifies field settings mid-run
   - See `Lattice.f90` where n2col, undgrad are used
   - These should update physics%n2col, physics%undulator_gradient

### Key Files to Modify
1. `Jsetupcalcs.f90` - Initialize physics parameters
2. `GEquations.f90` - Pass physics to coefficient functions
3. `Jrhs.f90` - Pass physics to RHS calculation
4. `KDerivative.f90` - Coordinate physics parameter passing
5. `CinitConds.f90` - Use physics for initial conditions
6. `simple_electron_gen.f90` - Use physics%gamma_ref for beam energy
7. `Lattice.f90` - Update physics%n2col and tapering parameters

### Testing
- Verify initial FEL parameter values calculated correctly
- Check derived quantities (eta, kappa, etc.) match hand calculations
- Run simulation with tapering enabled
- Verify gain length and cooperation length still correct
- Check against benchmarks with known FEL physics

---

## Phase 6: Migrate Electron Data (HIGH RISK - Do Carefully)

### Objective
Migrate `sElX_G, sElY_G, sElZ2_G, sElPX_G, sElPY_G, sElGam_G` and electron metadata to `tElectronCloud` type.

### Why This Phase (near end)
- **High coupling:** Electron data appears everywhere (RK4, coupling, output)
- **Benefits from prior phases:** Field mesh and physics migrated means cleaner electron function signatures
- **Change is deep:** Touches nearly every file in dynamics code

### Migration Steps

1. **Create electron initialization in simple_electron_gen.f90:**
   ```fortran
   subroutine GenerateElectrons(electrons, physics)
       type(tElectronCloud), intent(inout) :: electrons
       type(tFELPhysics), intent(in) :: physics
       ! Generate sElX_G, sElY_G, etc. → electrons%x, electrons%y, etc.
   end subroutine
   ```

2. **Create electron metadata structure:**
   ```fortran
   electrons%num_electrons = iNumberElectrons_G
   electrons%num_electrons_global = iGloNumElectrons_G
   allocate(electrons%electrons_per_proc(mpi_size))
   electrons%electrons_per_proc = procelectrons_G
   ```

3. **Update RK4 integrator:**
   - Current: Reads sElX_G(i), sElY_G(i), etc.
   - New: Reads electrons%x(i), electrons%y(i), etc.
   ```fortran
   ! OLD in MRK4.f90:
   subroutine rk4par(sZl, sStepSize, qDiffrctd)
       real(kind=wp), intent(inout) :: sElX_G(:), sElY_G(:)

   ! NEW:
   subroutine rk4par(electrons, integration, physics, mesh, qDiffrctd)
       type(tElectronCloud), intent(inout) :: electrons
       type(tIntegrationState), intent(in) :: integration
   ```

4. **Update electron-field coupling:**
   ```fortran
   ! OLD in JFiElec.f90:
   call get_local_field_index(sElX_G(iel), sElY_G(iel), NX_G, NY_G, ix, iy)

   ! NEW:
   call get_local_field_index(electrons%x(iel), electrons%y(iel), &
                              mesh%nx, mesh%ny, ix, iy)
   ```

5. **Update output/writing:**
   - Instead of reading global sElX_G in write routines
   - Pass electrons structure to write functions

### Dependency Chain
```
MRK4.f90 (RK4 and derivatives) ←
    ↑
    ├─ Jrhs.f90 (right-hand sides) ←
    │   ├─ GEquations.f90 (field equations)
    │   └─ JFiElec.f90 (electron-field coupling) ← mesh
    │
    ├─ JFiElec.f90
    │
    └─ choWrite.f90 (output) ← electrons
```

### Key Files to Modify (IN ORDER)
1. `simple_electron_gen.f90` - Generate electrons structure
2. `MRK4.f90` - Modify RK4 to use electrons structure
3. `Jrhs.f90` - Pass electrons to RHS calculation
4. `GEquations.f90` - Adapt to receive electrons%px, electrons%py, etc.
5. `JFiElec.f90` - Use electrons%x, electrons%y, electrons%z2
6. `KDerivative.f90` - Coordinate electron parameter passing
7. `choWrite.f90` - Write electrons structure to output
8. `remove_low_weights.f90` - Reference electrons%num_electrons
9. `undulator.f90` - Main loop orchestration

### Special Considerations
- **Temporary RK4 arrays:** Both old code and new need intermediate q arrays
  - These could go into integration state OR stay as temporaries
  - Keep as allocatable temp arrays within RK4.f90 for now
- **MPI communication:** procelectrons_G used in parallel communication
  - Transition electrons%electrons_per_proc carefully
  - Test parallel runs thoroughly

### Testing
- Run single electron simulation (check one value matches)
- Run multi-electron simulation with different distributions
- Test with MPI (2+ processes)
- Verify output file format and values match
- **Very important:** Numerical results must be bit-identical

---

## Phase 7: Migrate Flags & Output (LOW RISK)

### Objective
Migrate remaining boolean flags and output configuration to `tSimulationFlags` and `tOutputConfig`.

### Flags Migration
```fortran
! tSimulationFlags should contain:
qElectronsEvolve_G → electrons_evolve
qFieldEvolve_G → field_evolve
qElectronFieldCoupling_G → electron_field_coupling
qDiffraction_G → diffraction
qFocussing_G → focusing
qFilter → highpass_filter
qDump_G → dump_on_crash
qResume → resume_from_dump
qWrite → write_output
qOneD_G → one_dimensional
qMod_G → using_modules
qFMesh_G → fixed_mesh
qFixCharge_G → fixed_charge
qUseEmit_G → use_emittance
qscaled_G → scaled_coordinates
qInitWrLat_G → initial_write_lattice
qDumpEnd_G → dump_at_end
qPArrOK_G → parallel_arrays_ok
qInnerXYOK_G → inner_xy_ok
```

### Output Configuration Migration
```fortran
! tOutputConfig should contain:
tArrayE(:) → array_electron(:)
tArrayA(:) → array_field(:)
tArrayZ → array_z
iWriteNthSteps → write_nth_steps
iIntWriteNthSteps → write_nth_steps_intermediate
zFileName_G → main_filename
zBFile_G → beam_filename
zSFile_G → seed_filename
qhdf5_G → write_hdf5
qsdds_G → write_sdds
ioutInfo_G → output_info_level
frecvs → field_recv_counts
fdispls → field_displacements
cmd_call_G → command_call
```

### Migration Steps
1. **Update FreadData.f90** - Populate flags and output config from input
2. **Update Msetup.f90** - Initialize before main loop
3. **Update choWrite.f90** - Use output config in write decisions
4. **Update all modules using flags** - Pass flags structure instead of reading globals

### Key Files to Modify
1. `FreadData.f90` - Read into tSimulationFlags and tOutputConfig
2. `Msetup.f90` - Initialize structures
3. `choWrite.f90` - Use output config
4. `undulator.f90` - Check flags in main loop
5. Any module checking qDiffraction_G, qFocussing_G, etc.

### Testing
- Verify all flags still control behavior correctly
- Test with various flag combinations
- Check output files written to correct names

---

## Phase 8: Final Integration & Removal (HIGHEST RISK)

### Objective
- Assemble all structures into `tSimulationContext`
- Remove old global variables from `EDerivGlobals.f90`
- Full codebase using derived types

### Steps

1. **Create SimulationContext initialization:**
   ```fortran
   type(tSimulationContext) :: sim
   call InitializeContext(sim, input_file)
   call MainIntegrationLoop(sim)
   ```

2. **Update all module signatures** to accept context:
   ```fortran
   ! OLD:
   subroutine undulator()
       Use Globals
       ! ... uses 50+ globals

   ! NEW:
   subroutine undulator(sim)
       type(tSimulationContext), intent(inout) :: sim
       ! ... uses sim%mesh, sim%electrons, sim%integration, etc.
   ```

3. **Remove globals from EDerivGlobals.f90** (or keep as deprecated/wrapped):
   - Option A: Delete all unreferenced globals
   - Option B: Keep stubs that point to internal context (for debugging)

4. **Update main program:**
   ```fortran
   program puffin
       type(tSimulationContext) :: sim
       call ReadInput(sim)
       call InitializeMesh(sim%mesh)
       call InitializePhysics(sim%physics)
       call InitializeElectrons(sim%electrons, sim%physics)
       call SetupLattice(sim%lattice)
       call MainIntegrationLoop(sim)
   end program puffin
   ```

5. **Comprehensive testing:**
   - Full regression test suite
   - Compare output files bit-by-bit with baseline
   - Performance benchmarking
   - Parallel runs with various MPI configurations
   - HDF5 and SDDS output validation

### Risk Mitigation
- Keep old Globals module intact through Phase 7
- Run both versions in parallel during Phase 8
- Have clear rollback points at each major transformation
- Test incrementally, don't migrate all at once

---

## Files to Create

1. **GlobalTypes.f90** ✅ DONE
   - All derived type definitions
   - Location: `/src/puffin/lib/GlobalTypes.f90`
   - Dependencies: puffin_kinds, puffin_constants, ArrayFunctions, initDataType

2. **AdapterGlobals.f90** (Phase 1)
   - Wrapper functions to read/write globals from/to types
   - Location: `/src/puffin/lib/AdapterGlobals.f90`
   - Dependencies: GlobalTypes, Globals

3. **SimulationContext.f90** (Phase 8, optional)
   - Advanced module that orchestrates type initialization
   - Location: `/src/puffin/lib/SimulationContext.f90`
   - Dependencies: All other type modules

---

## Files to Modify (Summary)

### Always Safe (Early Phases)
- CMakeLists.txt or build system (add new files)
- puffin_kinds.f90, puffin_constants.f90 (just dependencies)

### Phase 1-3 (Low Risk)
- Msetup.f90 - Initialize structures
- Lattice.f90 - Lattice management
- undulator.f90 - Main loop
- MRK4.f90 - RK4 stepping
- choWrite.f90 - Output decisions

### Phase 4-5 (Medium Risk)
- Jsetupcalcs.f90 - Physics initialization
- JFiElec.f90 - Electron-field coupling
- GEquations.f90 - Field equations
- Jrhs.f90 - Right-hand sides
- para_field.f90 - Parallel field code
- KDerivative.f90 - Integration coordinator

### Phase 6 (High Risk - Most Extensive)
- simple_electron_gen.f90 - Electron generation
- MRK4.f90 - RK4 (already noted)
- Jrhs.f90 - RHS (already noted)
- JFiElec.f90 - Coupling (already noted)
- remove_low_weights.f90 - Electron filtering
- CinitConds.f90 - Initial conditions
- Jdatawrite.f90 - Data writing
- Any domain-specific physics modules

### Phase 7-8 (Cleanup)
- FreadData.f90 - Reading all configuration
- EDerivGlobals.f90 - Removal/cleanup
- Main program files
- All files that transitioned in earlier phases

---

## Compilation & Testing Strategy

### Build System Changes
```cmake
# Add to CMakeLists.txt:
add_library(puffin_types
    src/puffin/lib/GlobalTypes.f90
    src/puffin/lib/AdapterGlobals.f90
)

# Update main library:
target_link_libraries(puffin_lib PRIVATE puffin_types)
```

### Testing at Each Phase

| Phase | Compilation Check | Behavioral Test | Output Validation |
|-------|------------------|-----------------|-------------------|
| 1 | ✓ Builds cleanly | Run existing test suite | No changes expected |
| 2 | ✓ | Single step in loop | Step counters match |
| 3 | ✓ | Multi-element lattice | Element selection correct |
| 4 | ✓ | Field operations | FFT results match |
| 5 | ✓ | FEL physics setup | Calculated parameters match |
| 6 | ✓ | Full electron integration | Electron positions match |
| 7 | ✓ | Flag/output operations | Files written correctly |
| 8 | ✓ | Full simulation | Bit-identical output or acceptable tolerance |

### Regression Testing
- Keep baseline output from current code
- After each phase, compare against baseline
- Numerical tolerance: 1e-10 for field values
- Use diff/comparison script for HDF5 files

### Performance Testing
- Time each phase before/after
- Watch for unexpected slowdowns (type overhead)
- Profile hotspot functions

---

## Rollback Strategy

If major issues arise during a phase:

1. **Phase-level rollback:**
   - Commit work from previous phase
   - Revert current phase changes
   - Identify issue
   - Try alternative approach

2. **Git branches:**
   - Main development branch: `global-refactor-main`
   - Phase branches: `phase-1-integration`, `phase-2-lattice`, etc.
   - Each phase has a clean commit history
   - Can cherry-pick successful phases if needed

3. **Global wrapper option:**
   - Keep Globals module intact but deprecated
   - Add compilation flag to choose old vs new
   - Allows parallel comparison builds

---

## Documentation & Code Comments

For each phase, add comments:
```fortran
! REFACTORING: This module has been migrated to use tFieldMesh
! Old globals: NX_G, NY_G, NZ2_G → mesh%nx, mesh%ny, mesh%nz2
! Migration completed in Phase 4
```

Keep a REFACTORING.md file updated:
```markdown
# Refactoring Progress

## Completed Phases
- [x] Phase 0: GlobalTypes.f90 created
- [ ] Phase 1: Adapter layer & integration state

## Current Phase
- [ ] Phase 2: Lattice elements

## Blocked/In Progress
(none)
```

---

## Success Criteria

Project is successful when:

1. ✅ **No global variables in Globals module** (except deprecated wrappers)
2. ✅ **All functions have explicit data dependencies** (passed as arguments)
3. ✅ **Code compiles cleanly** with no warnings about global variable access
4. ✅ **Test suite passes** with acceptable numerical agreement
5. ✅ **Documentation updated** with new type usage patterns
6. ✅ **Performance maintained or improved** (no overhead from types)
7. ✅ **Parallel code verified** to work with explicit data passing
8. ✅ **Code review approved** by maintainers

---

## Timeline Estimate

With one dedicated developer:
- **Phase 0:** ✅ Day 1 (Design complete)
- **Phase 1:** Days 2-3 (Adapter layer)
- **Phase 2:** Days 4-5 (Integration state)
- **Phase 3:** Days 6-7 (Lattice)
- **Phase 4:** Days 8-10 (Field mesh)
- **Phase 5:** Days 11-13 (Physics parameters)
- **Phase 6:** Days 14-18 (Electron migration - longest)
- **Phase 7:** Days 19-20 (Flags & output)
- **Phase 8:** Days 21-24 (Final assembly & testing)

**Total: ~3-4 weeks**

With parallel development (multiple developers):
- Phases can overlap where dependencies allow
- Phase 2-3 can progress in parallel (little coupling)
- Phase 4-5 have some dependencies but could partially overlap
- Estimated time: **2 weeks**

---

## Notes & Caveats

1. **MPI Communication:** Special care needed when passing parallel arrays
   - Currently: procelectrons_G defines electron distribution
   - New: electrons%electrons_per_proc serves same role
   - Test multi-process runs thoroughly

2. **Memory Footprint:** Structs may have slight padding overhead
   - Fortran compilers optimize this well
   - Not expected to be significant

3. **FFTW Plans:** FFT plans reference field dimensions
   - May need to cache FFT plans in tFieldMesh
   - Or recreate them as-needed with mesh%nx, mesh%ny, etc.

4. **Checkpoint/Restart:** tInitData already defined
   - Ensure checkpoint code reads/writes new type layouts
   - Consider versioning checkpoint format

5. **Legacy Code:** Some routines may have circular dependencies
   - Identify these early in Phase 1
   - Break cycles before full migration

---

## Contact & Questions

For questions about this roadmap:
- Review the analysis document (separate file)
- Check GlobalTypes.f90 for type member documentation
- Reference specific file locations in roadmap
