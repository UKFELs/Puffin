! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Module providing adapter functions to convert between old global variables
!> and new derived types. This enables gradual migration from globals to types.
!>
!> MIGRATION NOTE: This module is a temporary bridge during the refactoring.
!> Functions here should be phased out as corresponding modules are updated
!> to use the new type structures directly.

module AdapterGlobals

use puffin_kinds
use puffin_constants
use GlobalTypes
use Globals

implicit none

private

! Public subroutines for adapting globals to types and vice versa
public :: PopulateFieldMeshFromGlobals, UpdateGlobalsFromFieldMesh
public :: PopulateElectronCloudFromGlobals, UpdateGlobalsFromElectronCloud
public :: PopulateIntegrationStateFromGlobals, UpdateGlobalsFromIntegrationState
public :: PopulateUndulatorFromGlobals, UpdateGlobalsFromUndulator
public :: PopulateLatticeElementsFromGlobals, UpdateGlobalsFromLatticeElements
public :: PopulateOutputConfigFromGlobals, UpdateGlobalsFromOutputConfig
public :: PopulateSimulationFlagsFromGlobals, UpdateGlobalsFromSimulationFlags

contains

! ============================================================================
! FIELD MESH ADAPTERS
! ============================================================================

!> Populate tFieldMesh type from global variables
!!
!! Copies all field mesh-related globals into the mesh type structure.
!! This is a one-way operation - globals are the source of truth.
subroutine PopulateFieldMeshFromGlobals(mesh)
    type(tFieldMesh), intent(inout) :: mesh

    ! Grid dimensions
    mesh%nx = NX_G
    mesh%ny = NY_G
    mesh%nz2 = NZ2_G
    mesh%nbx = NBX_G
    mesh%nby = NBY_G
    mesh%nbz2 = NBZ2_G

    ! Element sizes
    mesh%dx = sLengthOfElmX_G
    mesh%dy = sLengthOfElmY_G
    mesh%dz2 = sLengthOfElmZ2_G
    mesh%delta = delta_G

    ! Wavenumbers - allocate and copy
    if (allocated(mesh%kx)) deallocate(mesh%kx)
    if (allocated(mesh%ky)) deallocate(mesh%ky)
    if (allocated(mesh%kz2_loc)) deallocate(mesh%kz2_loc)

    if (allocated(kx_G)) then
        allocate(mesh%kx(size(kx_G)))
        mesh%kx = kx_G
    end if
    if (allocated(ky_G)) then
        allocate(mesh%ky(size(ky_G)))
        mesh%ky = ky_G
    end if
    if (allocated(kz2_loc_G)) then
        allocate(mesh%kz2_loc(size(kz2_loc_G)))
        mesh%kz2_loc = kz2_loc_G
    end if

    ! Axis arrays
    if (allocated(mesh%x_axis)) deallocate(mesh%x_axis)
    if (allocated(mesh%y_axis)) deallocate(mesh%y_axis)

    if (allocated(x_ax_G)) then
        allocate(mesh%x_axis(size(x_ax_G)))
        mesh%x_axis = x_ax_G
    end if

    if (allocated(y_ax_G)) then
        allocate(mesh%y_axis(size(y_ax_G)))
        mesh%y_axis = y_ax_G
    end if

    ! Mesh properties
    mesh%nodes_per_element = iNodesPerElement_G
    mesh%mesh_type = fieldMesh
    mesh%period_waves = sperwaves_G

    ! FFT helper variables
    mesh%nspin_dx = nspinDX
    mesh%nspin_dy = nspinDY
    mesh%reduced_nodes_x = iRedNodesX_G
    mesh%reduced_nodes_y = iRedNodesY_G
    mesh%output_node_x = outnodex_G
    mesh%output_node_y = outnodey_G

    ! Filtering
    mesh%filter_cutoff_freq = sfilt
    mesh%highpass_filter_gr = -1_ip

    ! Flags
    mesh%is_1d = qOneD_G
    mesh%equal_xy_spacing = qEquiXY_G

end subroutine PopulateFieldMeshFromGlobals

!> Update global variables from tFieldMesh type
!!
!! Copies mesh data back to globals. Use when mesh is modified.
subroutine UpdateGlobalsFromFieldMesh(mesh)
    type(tFieldMesh), intent(in) :: mesh

    ! Grid dimensions
    NX_G = mesh%nx
    NY_G = mesh%ny
    NZ2_G = mesh%nz2
    NBX_G = mesh%nbx
    NBY_G = mesh%nby
    NBZ2_G = mesh%nbz2

    ! Element sizes
    sLengthOfElmX_G = mesh%dx
    sLengthOfElmY_G = mesh%dy
    sLengthOfElmZ2_G = mesh%dz2
    delta_G = mesh%delta

    ! Wavenumbers - copy back if allocated
    if (allocated(mesh%kx) .and. allocated(kx_G)) then
        if (size(mesh%kx) == size(kx_G)) then
            kx_G = mesh%kx
        end if
    end if

    if (allocated(mesh%ky) .and. allocated(ky_G)) then
        if (size(mesh%ky) == size(ky_G)) then
            ky_G = mesh%ky
        end if
    end if

    if (allocated(mesh%kz2_loc) .and. allocated(kz2_loc_G)) then
        if (size(mesh%kz2_loc) == size(kz2_loc_G)) then
            kz2_loc_G = mesh%kz2_loc
        end if
    end if

    ! Mesh properties
    iNodesPerElement_G = mesh%nodes_per_element
    fieldMesh = mesh%mesh_type
    sperwaves_G = mesh%period_waves

    ! FFT helper variables
    nspinDX = mesh%nspin_dx
    nspinDY = mesh%nspin_dy
    iRedNodesX_G = mesh%reduced_nodes_x
    iRedNodesY_G = mesh%reduced_nodes_y
    outnodex_G = mesh%output_node_x
    outnodey_G = mesh%output_node_y

    ! Filtering
    sfilt = mesh%filter_cutoff_freq

    ! Flags
    qOneD_G = mesh%is_1d
    qEquiXY_G = mesh%equal_xy_spacing

end subroutine UpdateGlobalsFromFieldMesh

! ============================================================================
! ELECTRON CLOUD ADAPTERS
! ============================================================================

!> Populate tElectronCloud type from global variables
subroutine PopulateElectronCloudFromGlobals(electrons)
    type(tElectronCloud), intent(inout) :: electrons

    ! Electron counts
    electrons%num_electrons = iNumberElectrons_G
    electrons%num_electrons_global = iGloNumElectrons_G

    ! Allocate and copy electron distribution
    if (allocated(electrons%electrons_per_proc)) deallocate(electrons%electrons_per_proc)
    if (allocated(procelectrons_G)) then
        allocate(electrons%electrons_per_proc(size(procelectrons_G)))
        electrons%electrons_per_proc = procelectrons_G
    end if

    ! Phase space coordinates - allocate and copy
    if (allocated(electrons%x)) deallocate(electrons%x)
    if (allocated(electrons%y)) deallocate(electrons%y)
    if (allocated(electrons%z2)) deallocate(electrons%z2)
    if (allocated(electrons%px)) deallocate(electrons%px)
    if (allocated(electrons%py)) deallocate(electrons%py)
    if (allocated(electrons%gamma)) deallocate(electrons%gamma)

    if (allocated(sElX_G)) then
        allocate(electrons%x(size(sElX_G)))
        electrons%x = sElX_G
    end if

    if (allocated(sElY_G)) then
        allocate(electrons%y(size(sElY_G)))
        electrons%y = sElY_G
    end if

    if (allocated(sElZ2_G)) then
        allocate(electrons%z2(size(sElZ2_G)))
        electrons%z2 = sElZ2_G
    end if

    if (allocated(sElPX_G)) then
        allocate(electrons%px(size(sElPX_G)))
        electrons%px = sElPX_G
    end if

    if (allocated(sElPY_G)) then
        allocate(electrons%py(size(sElPY_G)))
        electrons%py = sElPY_G
    end if

    if (allocated(sElGam_G)) then
        allocate(electrons%gamma(size(sElGam_G)))
        electrons%gamma = sElGam_G
    end if

    ! Derived quantities
    electrons%peak_density = npk_bar_G
    electrons%fill_factor = fillFact_G
    electrons%ata = ata_G

    ! Tracking positions
    electrons%z_interaction = 0.0_wp
    electrons%z_last_step = sZlSt_G

end subroutine PopulateElectronCloudFromGlobals

!> Update global variables from tElectronCloud type
subroutine UpdateGlobalsFromElectronCloud(electrons)
    type(tElectronCloud), intent(in) :: electrons

    ! Electron counts
    iNumberElectrons_G = electrons%num_electrons
    iGloNumElectrons_G = electrons%num_electrons_global

    ! Copy electron distribution back
    if (allocated(electrons%electrons_per_proc) .and. allocated(procelectrons_G)) then
        if (size(electrons%electrons_per_proc) == size(procelectrons_G)) then
            procelectrons_G = electrons%electrons_per_proc
        end if
    end if

    ! Copy phase space coordinates back
    if (allocated(electrons%x) .and. allocated(sElX_G)) then
        if (size(electrons%x) == size(sElX_G)) sElX_G = electrons%x
    end if

    if (allocated(electrons%y) .and. allocated(sElY_G)) then
        if (size(electrons%y) == size(sElY_G)) sElY_G = electrons%y
    end if

    if (allocated(electrons%z2) .and. allocated(sElZ2_G)) then
        if (size(electrons%z2) == size(sElZ2_G)) sElZ2_G = electrons%z2
    end if

    if (allocated(electrons%px) .and. allocated(sElPX_G)) then
        if (size(electrons%px) == size(sElPX_G)) sElPX_G = electrons%px
    end if

    if (allocated(electrons%py) .and. allocated(sElPY_G)) then
        if (size(electrons%py) == size(sElPY_G)) sElPY_G = electrons%py
    end if

    if (allocated(electrons%gamma) .and. allocated(sElGam_G)) then
        if (size(electrons%gamma) == size(sElGam_G)) sElGam_G = electrons%gamma
    end if

    ! Derived quantities
    npk_bar_G = electrons%peak_density
    fillFact_G = electrons%fill_factor
    ata_G = electrons%ata

    ! Tracking positions
    sZlSt_G = electrons%z_last_step

end subroutine UpdateGlobalsFromElectronCloud

! ============================================================================
! INTEGRATION STATE ADAPTERS
! ============================================================================

!> Populate tIntegrationState type from global variables
subroutine PopulateIntegrationStateFromGlobals(integration)
    type(tIntegrationState), intent(inout) :: integration

    ! Step counters
    integration%current_step = iStep
    integration%total_steps = nSteps
    integration%start_step = start_step
    integration%count = iCount

    ! Step sizes
    integration%step_parameter = sStep
    integration%step_size = sStepSize

    ! Diffraction and redistribution
    integration%diffraction_step_size = diffStep
    integration%redistribution_length = sRedistLen_G
    integration%redistribution_step = iRedistStp_G

    ! Intermediate z tracker
    integration%z_inter = 0.0_wp

    ! Timing (time_start is set by puffin_main after init; time_end is local to UndSection)
    integration%time_start = 0.0_wp
    integration%time_end = 0.0_wp
    integration%time_debug1 = time1
    integration%time_debug2 = time2

end subroutine PopulateIntegrationStateFromGlobals

!> Update global variables from tIntegrationState type
subroutine UpdateGlobalsFromIntegrationState(integration)
    type(tIntegrationState), intent(in) :: integration

    ! Step counters
    iStep = integration%current_step
    nSteps = integration%total_steps
    start_step = integration%start_step
    iCount = integration%count

    ! Step sizes
    sStep = integration%step_parameter
    sStepSize = integration%step_size

    ! Diffraction and redistribution
    diffStep = integration%diffraction_step_size
    sRedistLen_G = integration%redistribution_length
    iRedistStp_G = integration%redistribution_step

    ! Timing
    time1 = integration%time_debug1
    time2 = integration%time_debug2

end subroutine UpdateGlobalsFromIntegrationState

! ============================================================================
! FEL FRAME ADAPTERS  (simulation-lifetime scaling frame)
! ============================================================================

! ============================================================================
! UNDULATOR ADAPTERS  (per-element undulator properties)
! ============================================================================

!> Populate tUndulator type from global variables
!!
!! Called after initUndulator() sets up the globals for a new element.
subroutine PopulateUndulatorFromGlobals(und)
    type(tUndulator), intent(inout) :: und

    ! Undulator field shape
    und%undulator_type = zUndType_G
    und%kx_undulator = kx_und_G
    und%ky_undulator = ky_und_G

    ! Beta-function wavenumbers
    und%k_beta_x_sf = sKBetaXSF_G
    und%k_beta_y_sf = sKBetaYSF_G
    und%k_beta = sKBeta_G
    und%k_beta_x = sKBetaX_G
    und%k_beta_y = sKBetaY_G

    ! Focusing
    und%focus_factor = sFocusfactor_G
    und%focus_factor_saved = sFocusfactor_save_G
    und%fx = fx_G
    und%fy = fy_G

    ! Absorption
    und%beta_absorption = sBeta_G

    ! Undulator ends
    und%model_undulator_ends = qUndEnds_G
    und%z_start_undulator = sZFS
    und%z_end_undulator = sZFE
    und%undulator_position = iUndPlace_G

    ! Tapering (field-dependent, may evolve mid-loop)
    und%n2col = n2col
    und%n2col_initial = n2col0
    und%undulator_gradient = undgrad
    und%z_taper_start = sz0

end subroutine PopulateUndulatorFromGlobals

!> Update global variables from tUndulator type
subroutine UpdateGlobalsFromUndulator(und)
    type(tUndulator), intent(in) :: und

    ! Undulator field shape
    zUndType_G = und%undulator_type
    kx_und_G = und%kx_undulator
    ky_und_G = und%ky_undulator

    ! Beta-function wavenumbers
    sKBetaXSF_G = und%k_beta_x_sf
    sKBetaYSF_G = und%k_beta_y_sf
    sKBeta_G = und%k_beta
    sKBetaX_G = und%k_beta_x
    sKBetaY_G = und%k_beta_y

    ! Focusing
    sFocusfactor_G = und%focus_factor
    sFocusfactor_save_G = und%focus_factor_saved
    fx_G = und%fx
    fy_G = und%fy

    ! Absorption
    sBeta_G = und%beta_absorption

    ! Undulator ends
    qUndEnds_G = und%model_undulator_ends
    sZFS = und%z_start_undulator
    sZFE = und%z_end_undulator
    iUndPlace_G = und%undulator_position

    ! Tapering
    n2col = und%n2col
    n2col0 = und%n2col_initial
    undgrad = und%undulator_gradient
    sz0 = und%z_taper_start

end subroutine UpdateGlobalsFromUndulator

! ============================================================================
! LATTICE ELEMENTS ADAPTERS
! ============================================================================

!> Populate tLatticeElements type from global variables
subroutine PopulateLatticeElementsFromGlobals(lattice)
    type(tLatticeElements), intent(inout) :: lattice

    ! Counters
    lattice%num_undulators = numOfUnds
    lattice%num_chicanes = numOfChics
    lattice%num_drifts = numOfDrifts
    lattice%num_modulations = numOfModulations
    lattice%num_quadrupoles = numOfQuads

    ! Overall tracking (cumulative_steps starts at 0; resume path overrides in setup.f90)
    lattice%cumulative_steps = 0_ip
    lattice%num_modules = ModNum
    lattice%module_count = ModCount

    ! Per-element-type index counters (start at 1; resume path overrides in setup.f90)
    lattice%current_und_index = 1_ip
    lattice%current_chic_index = 1_ip
    lattice%current_drift_index = 1_ip
    lattice%current_quad_index = 1_ip
    lattice%current_modulation_index = 1_ip

    ! Allocate and copy undulator arrays
    if (allocated(lattice%und_z_mod)) deallocate(lattice%und_z_mod)
    if (allocated(lattice%und_field)) deallocate(lattice%und_field)
    if (allocated(lattice%und_delta_z)) deallocate(lattice%und_delta_z)
    if (allocated(lattice%und_taper)) deallocate(lattice%und_taper)
    if (allocated(lattice%und_ux)) deallocate(lattice%und_ux)
    if (allocated(lattice%und_uy)) deallocate(lattice%und_uy)
    if (allocated(lattice%und_kbx)) deallocate(lattice%und_kbx)
    if (allocated(lattice%und_kby)) deallocate(lattice%und_kby)
    if (allocated(lattice%und_type)) deallocate(lattice%und_type)
    if (allocated(lattice%und_nsteps)) deallocate(lattice%und_nsteps)

    if (allocated(zMod)) then
        allocate(lattice%und_z_mod(size(zMod)))
        lattice%und_z_mod = zMod
    end if
    if (allocated(mf)) then
        allocate(lattice%und_field(size(mf)))
        lattice%und_field = mf
    end if
    if (allocated(delmz)) then
        allocate(lattice%und_delta_z(size(delmz)))
        lattice%und_delta_z = delmz
    end if
    if (allocated(tapers)) then
        allocate(lattice%und_taper(size(tapers)))
        lattice%und_taper = tapers
    end if
    if (allocated(ux_arr)) then
        allocate(lattice%und_ux(size(ux_arr)))
        lattice%und_ux = ux_arr
    end if
    if (allocated(uy_arr)) then
        allocate(lattice%und_uy(size(uy_arr)))
        lattice%und_uy = uy_arr
    end if
    if (allocated(kbnx_arr)) then
        allocate(lattice%und_kbx(size(kbnx_arr)))
        lattice%und_kbx = kbnx_arr
    end if
    if (allocated(kbny_arr)) then
        allocate(lattice%und_kby(size(kbny_arr)))
        lattice%und_kby = kbny_arr
    end if
    if (allocated(zundtype_arr)) then
        allocate(lattice%und_type(size(zundtype_arr)))
        lattice%und_type = zundtype_arr
    end if
    if (allocated(nSteps_arr)) then
        allocate(lattice%und_nsteps(size(nSteps_arr)))
        lattice%und_nsteps = nSteps_arr
    end if

    ! Chicane arrays
    if (allocated(lattice%chic_zbar)) deallocate(lattice%chic_zbar)
    if (allocated(lattice%chic_slip)) deallocate(lattice%chic_slip)
    if (allocated(lattice%chic_disp)) deallocate(lattice%chic_disp)

    if (allocated(chic_zbar)) then
        allocate(lattice%chic_zbar(size(chic_zbar)))
        lattice%chic_zbar = chic_zbar
    end if
    if (allocated(chic_slip)) then
        allocate(lattice%chic_slip(size(chic_slip)))
        lattice%chic_slip = chic_slip
    end if
    if (allocated(chic_disp)) then
        allocate(lattice%chic_disp(size(chic_disp)))
        lattice%chic_disp = chic_disp
    end if

    ! Drift arrays
    if (allocated(lattice%drift_zbar)) deallocate(lattice%drift_zbar)
    if (allocated(drift_zbar)) then
        allocate(lattice%drift_zbar(size(drift_zbar)))
        lattice%drift_zbar = drift_zbar
    end if

    ! Modulation arrays
    if (allocated(lattice%enmod_wavenum)) deallocate(lattice%enmod_wavenum)
    if (allocated(lattice%enmod_mag)) deallocate(lattice%enmod_mag)

    if (allocated(enmod_wavenum)) then
        allocate(lattice%enmod_wavenum(size(enmod_wavenum)))
        lattice%enmod_wavenum = enmod_wavenum
    end if
    if (allocated(enmod_mag)) then
        allocate(lattice%enmod_mag(size(enmod_mag)))
        lattice%enmod_mag = enmod_mag
    end if

    ! Quadrupole arrays
    if (allocated(lattice%quad_fx)) deallocate(lattice%quad_fx)
    if (allocated(lattice%quad_fy)) deallocate(lattice%quad_fy)

    if (allocated(quad_fx)) then
        allocate(lattice%quad_fx(size(quad_fx)))
        lattice%quad_fx = quad_fx
    end if
    if (allocated(quad_fy)) then
        allocate(lattice%quad_fy(size(quad_fy)))
        lattice%quad_fy = quad_fy
    end if

    ! Total line length
    lattice%total_line_length = totUndLineLength

end subroutine PopulateLatticeElementsFromGlobals

!> Update global variables from tLatticeElements type
subroutine UpdateGlobalsFromLatticeElements(lattice)
    type(tLatticeElements), intent(in) :: lattice

    ! Counters
    numOfUnds = lattice%num_undulators
    numOfChics = lattice%num_chicanes
    numOfDrifts = lattice%num_drifts
    numOfModulations = lattice%num_modulations
    numOfQuads = lattice%num_quadrupoles

    ! Overall tracking
    ModNum = lattice%num_modules
    ModCount = lattice%module_count

    ! Copy undulator arrays back
    if (allocated(lattice%und_z_mod) .and. allocated(zMod)) then
        if (size(lattice%und_z_mod) == size(zMod)) zMod = lattice%und_z_mod
    end if
    if (allocated(lattice%und_field) .and. allocated(mf)) then
        if (size(lattice%und_field) == size(mf)) mf = lattice%und_field
    end if
    if (allocated(lattice%und_delta_z) .and. allocated(delmz)) then
        if (size(lattice%und_delta_z) == size(delmz)) delmz = lattice%und_delta_z
    end if
    if (allocated(lattice%und_taper) .and. allocated(tapers)) then
        if (size(lattice%und_taper) == size(tapers)) tapers = lattice%und_taper
    end if
    if (allocated(lattice%und_ux) .and. allocated(ux_arr)) then
        if (size(lattice%und_ux) == size(ux_arr)) ux_arr = lattice%und_ux
    end if
    if (allocated(lattice%und_uy) .and. allocated(uy_arr)) then
        if (size(lattice%und_uy) == size(uy_arr)) uy_arr = lattice%und_uy
    end if
    if (allocated(lattice%und_kbx) .and. allocated(kbnx_arr)) then
        if (size(lattice%und_kbx) == size(kbnx_arr)) kbnx_arr = lattice%und_kbx
    end if
    if (allocated(lattice%und_kby) .and. allocated(kbny_arr)) then
        if (size(lattice%und_kby) == size(kbny_arr)) kbny_arr = lattice%und_kby
    end if
    if (allocated(lattice%und_type) .and. allocated(zundtype_arr)) then
        if (size(lattice%und_type) == size(zundtype_arr)) zundtype_arr = lattice%und_type
    end if
    if (allocated(lattice%und_nsteps) .and. allocated(nSteps_arr)) then
        if (size(lattice%und_nsteps) == size(nSteps_arr)) nSteps_arr = lattice%und_nsteps
    end if

    ! Copy chicane arrays back
    if (allocated(lattice%chic_zbar) .and. allocated(chic_zbar)) then
        if (size(lattice%chic_zbar) == size(chic_zbar)) chic_zbar = lattice%chic_zbar
    end if
    if (allocated(lattice%chic_slip) .and. allocated(chic_slip)) then
        if (size(lattice%chic_slip) == size(chic_slip)) chic_slip = lattice%chic_slip
    end if
    if (allocated(lattice%chic_disp) .and. allocated(chic_disp)) then
        if (size(lattice%chic_disp) == size(chic_disp)) chic_disp = lattice%chic_disp
    end if

    ! Copy drift arrays back
    if (allocated(lattice%drift_zbar) .and. allocated(drift_zbar)) then
        if (size(lattice%drift_zbar) == size(drift_zbar)) drift_zbar = lattice%drift_zbar
    end if

    ! Copy modulation arrays back
    if (allocated(lattice%enmod_wavenum) .and. allocated(enmod_wavenum)) then
        if (size(lattice%enmod_wavenum) == size(enmod_wavenum)) &
            enmod_wavenum = lattice%enmod_wavenum
    end if
    if (allocated(lattice%enmod_mag) .and. allocated(enmod_mag)) then
        if (size(lattice%enmod_mag) == size(enmod_mag)) enmod_mag = lattice%enmod_mag
    end if

    ! Copy quadrupole arrays back
    if (allocated(lattice%quad_fx) .and. allocated(quad_fx)) then
        if (size(lattice%quad_fx) == size(quad_fx)) quad_fx = lattice%quad_fx
    end if
    if (allocated(lattice%quad_fy) .and. allocated(quad_fy)) then
        if (size(lattice%quad_fy) == size(quad_fy)) quad_fy = lattice%quad_fy
    end if

    ! Total line length
    totUndLineLength = lattice%total_line_length

end subroutine UpdateGlobalsFromLatticeElements

! ============================================================================
! OUTPUT CONFIG ADAPTERS
! ============================================================================

!> Populate tOutputConfig type from global variables
subroutine PopulateOutputConfigFromGlobals(output)
    type(tOutputConfig), intent(inout) :: output

    ! Write frequency
    output%write_nth_steps = iWriteNthSteps
    output%write_nth_steps_intermediate = iIntWriteNthSteps

    ! File names
    output%main_filename = zFileName_G
    output%beam_filename = zBFile_G
    output%seed_filename = zSFile_G

    ! Output flags
    output%write_hdf5 = qhdf5_G
    output%write_sdds = qsdds_G
    output%output_info_level = ioutInfo_G
    output%separate_step_files = qSeparateStepFiles_G

    ! Command call for reproducibility
    output%command_call = cmd_call_G

    ! MPI displacement info
    if (allocated(output%field_recv_counts)) deallocate(output%field_recv_counts)
    if (allocated(output%field_displacements)) deallocate(output%field_displacements)

    if (allocated(frecvs)) then
        allocate(output%field_recv_counts(size(frecvs)))
        output%field_recv_counts = frecvs
    end if

    if (allocated(fdispls)) then
        allocate(output%field_displacements(size(fdispls)))
        output%field_displacements = fdispls
    end if

end subroutine PopulateOutputConfigFromGlobals

!> Update global variables from tOutputConfig type
subroutine UpdateGlobalsFromOutputConfig(output)
    type(tOutputConfig), intent(in) :: output

    ! Write frequency
    iWriteNthSteps = output%write_nth_steps
    iIntWriteNthSteps = output%write_nth_steps_intermediate

    ! File names
    zFileName_G = output%main_filename
    zBFile_G = output%beam_filename
    zSFile_G = output%seed_filename

    ! Output flags
    qhdf5_G = output%write_hdf5
    qsdds_G = output%write_sdds
    ioutInfo_G = output%output_info_level
    qSeparateStepFiles_G = output%separate_step_files

    ! Command call for reproducibility
    cmd_call_G = output%command_call

    ! MPI displacement info
    if (allocated(output%field_recv_counts) .and. allocated(frecvs)) then
        if (size(output%field_recv_counts) == size(frecvs)) &
            frecvs = output%field_recv_counts
    end if

    if (allocated(output%field_displacements) .and. allocated(fdispls)) then
        if (size(output%field_displacements) == size(fdispls)) &
            fdispls = output%field_displacements
    end if

end subroutine UpdateGlobalsFromOutputConfig

! ============================================================================
! SIMULATION FLAGS ADAPTERS
! ============================================================================

!> Populate tSimulationFlags type from global variables
subroutine PopulateSimulationFlagsFromGlobals(flags)
    type(tSimulationFlags), intent(inout) :: flags

    ! Physics switches
    flags%electrons_evolve = qElectronsEvolve_G
    flags%field_evolve = qFieldEvolve_G
    flags%electron_field_coupling = qElectronFieldCoupling_G
    flags%diffraction = qDiffraction_G
    flags%focusing = qFocussing_G
    flags%highpass_filter = qFilter

    ! Data management
    flags%dump_on_crash = qDump_G
    flags%resume_from_dump = qResume
    flags%write_output = qWrite

    ! Simulation type
    flags%one_dimensional = qOneD_G
    flags%using_modules = qMod_G

    ! Mesh properties
    flags%fixed_mesh = qFMesh_G
    flags%fixed_charge = qFixCharge_G
    flags%use_emittance = qUseEmit_G
    flags%scaled_coordinates = qscaled_G
    flags%initial_write_lattice = qInitWrLat_G
    flags%dump_at_end = qDumpEnd_G

    ! Particle arrays status (initialize to .true.; getInterps will set .false. on out-of-bounds)
    flags%parallel_arrays_ok = .true.
    flags%inner_xy_ok = .true.

    ! Seed properties
    if (allocated(flags%field_round_edges)) deallocate(flags%field_round_edges)
    if (allocated(flags%field_edge_sigma)) deallocate(flags%field_edge_sigma)
    if (allocated(flags%match_seed)) deallocate(flags%match_seed)

    if (allocated(qRndFj_G)) then
        allocate(flags%field_round_edges(size(qRndFj_G)))
        flags%field_round_edges = qRndFj_G
    end if
    if (allocated(sSigFj_G)) then
        allocate(flags%field_edge_sigma(size(sSigFj_G)))
        flags%field_edge_sigma = sSigFj_G
    end if
    if (allocated(qMatchS_G)) then
        allocate(flags%match_seed(size(qMatchS_G)))
        flags%match_seed = qMatchS_G
    end if

    ! Sequencing
    flags%num_sequential_parts = nseqparts_G

    ! Field seed type
    flags%field_seed_type = iFieldSeedType_G

    ! Electron input type
    flags%electron_input_type = iInputType_G

    ! Tracking load method
    flags%tracking_load_method = TrLdMeth_G

    ! Current mesh calculation
    flags%current_mesh_pts = npts_I_G
    flags%current_mesh_dz2 = dz2_I_G

    ! Seed rounding
    if (allocated(flags%seed_sigma)) deallocate(flags%seed_sigma)
    if (allocated(flags%seed_round_edges)) deallocate(flags%seed_round_edges)

    if (allocated(sSigEj_G)) then
        allocate(flags%seed_sigma(size(sSigEj_G)))
        flags%seed_sigma = sSigEj_G
    end if
    if (allocated(qRndEj_G)) then
        allocate(flags%seed_round_edges(size(qRndEj_G)))
        flags%seed_round_edges = qRndEj_G
    end if

end subroutine PopulateSimulationFlagsFromGlobals

!> Update global variables from tSimulationFlags type
subroutine UpdateGlobalsFromSimulationFlags(flags)
    type(tSimulationFlags), intent(in) :: flags

    ! Physics switches
    qElectronsEvolve_G = flags%electrons_evolve
    qFieldEvolve_G = flags%field_evolve
    qElectronFieldCoupling_G = flags%electron_field_coupling
    qDiffraction_G = flags%diffraction
    qFocussing_G = flags%focusing
    qFilter = flags%highpass_filter

    ! Data management
    qDump_G = flags%dump_on_crash
    qResume = flags%resume_from_dump
    qWrite = flags%write_output

    ! Simulation type
    qOneD_G = flags%one_dimensional
    qMod_G = flags%using_modules

    ! Mesh properties
    qFMesh_G = flags%fixed_mesh
    qFixCharge_G = flags%fixed_charge
    qUseEmit_G = flags%use_emittance
    qscaled_G = flags%scaled_coordinates
    qInitWrLat_G = flags%initial_write_lattice
    qDumpEnd_G = flags%dump_at_end

    ! Seed properties
    if (allocated(flags%field_round_edges) .and. allocated(qRndFj_G)) then
        if (size(flags%field_round_edges) == size(qRndFj_G)) &
            qRndFj_G = flags%field_round_edges
    end if
    if (allocated(flags%field_edge_sigma) .and. allocated(sSigFj_G)) then
        if (size(flags%field_edge_sigma) == size(sSigFj_G)) &
            sSigFj_G = flags%field_edge_sigma
    end if
    if (allocated(flags%match_seed) .and. allocated(qMatchS_G)) then
        if (size(flags%match_seed) == size(qMatchS_G)) &
            qMatchS_G = flags%match_seed
    end if

    ! Sequencing
    nseqparts_G = flags%num_sequential_parts

    ! Field seed type
    iFieldSeedType_G = flags%field_seed_type

    ! Electron input type
    iInputType_G = flags%electron_input_type

    ! Tracking load method
    TrLdMeth_G = flags%tracking_load_method

    ! Current mesh calculation
    npts_I_G = flags%current_mesh_pts
    dz2_I_G = flags%current_mesh_dz2

    ! Seed rounding
    if (allocated(flags%seed_sigma) .and. allocated(sSigEj_G)) then
        if (size(flags%seed_sigma) == size(sSigEj_G)) sSigEj_G = flags%seed_sigma
    end if
    if (allocated(flags%seed_round_edges) .and. allocated(qRndEj_G)) then
        if (size(flags%seed_round_edges) == size(qRndEj_G)) &
            qRndEj_G = flags%seed_round_edges
    end if

end subroutine UpdateGlobalsFromSimulationFlags

end module AdapterGlobals
