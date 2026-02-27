! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Module defining derived types to replace global variables in Puffin
!> This replaces the scatter of globals in EDerivGlobals.f90 with structured types

module GlobalTypes

use puffin_kinds
use puffin_constants
use ArrayFunctions
use initDataType

implicit none

! ============================================================================
! 1. FIELD MESH TYPE - Encapsulates spatial grid and field mesh properties
! ============================================================================
type :: tFieldMesh
    ! Grid dimensions
    integer(kind=ip) :: nx, ny, nz2
    integer(kind=ip) :: nbx, nby, nbz2        ! Ghost/boundary cells

    ! Wavenumbers (allocatable arrays)
    real(kind=wp), allocatable :: kx(:), ky(:), kz2_loc(:)

    ! Element sizes
    real(kind=wp) :: dx, dy, dz2
    real(kind=wp) :: delta        ! Volume element (dx * dy * dz2)

    ! Axis arrays for integration
    real(kind=wp), allocatable :: x_axis(:), y_axis(:)

    ! Mesh properties
    integer(kind=ip) :: nodes_per_element
    integer(kind=ip) :: mesh_type  ! iTemporal or iPeriodic
    real(kind=wp) :: period_waves

    ! State/helper variables for FFT operations
    integer(kind=ip) :: nspin_dx, nspin_dy  ! Derived from dimensions
    integer(kind=ip) :: reduced_nodes_x, reduced_nodes_y
    integer(kind=ip) :: output_node_x, output_node_y

    ! Filtering
    real(kind=wp) :: filter_cutoff_freq    ! sfilt
    integer(kind=ip) :: highpass_filter_gr ! igwr

    ! 1D vs 3D flag
    logical :: is_1d
    logical :: equal_xy_spacing
end type tFieldMesh

! ============================================================================
! 2. ELECTRON PHASE SPACE TYPE - 6D particle data and metadata
! ============================================================================
type :: tElectronCloud
    ! Phase space coordinates (allocatable for each electron)
    real(kind=wp), allocatable :: x(:), y(:), z2(:)      ! Positions
    real(kind=wp), allocatable :: px(:), py(:)           ! Transverse momenta
    real(kind=wp), allocatable :: gamma(:)               ! Lorentz factor

    ! Electron metadata
    integer(kind=ipl) :: num_electrons                   ! iNumberElectrons_G
    integer(kind=ipl) :: num_electrons_global            ! iGloNumElectrons_G
    integer(kind=ipl), allocatable :: electrons_per_proc(:)  ! procelectrons_G

    ! Derived quantities
    real(kind=wp) :: peak_density                        ! npk_bar_G
    real(kind=wp) :: fill_factor, ata                    ! fillFact_G, ata_G

    ! Tracking
    real(kind=wp) :: z_interaction, z_last_step         ! sZi_G, sZlSt_G
end type tElectronCloud

! ============================================================================
! 3. FEL PHYSICS PARAMETERS TYPE - Wiggler & undulator physics
! ============================================================================
type :: tFELPhysics
    ! Core undulator parameters (usually set once at initialization)
    real(kind=wp) :: rho                    ! sRho_G - Pierce parameter
    real(kind=wp) :: aw                     ! sAw_G - undulator strength
    real(kind=wp) :: gamma_ref              ! sGammaR_G - reference electron energy

    ! Derived parameters (computed from above in Jsetupcalcs)
    real(kind=wp) :: eta                    ! sEta_G - detuning parameter
    real(kind=wp) :: kappa                  ! sKappa_G
    real(kind=wp) :: k_beta                 ! sKBeta_G
    real(kind=wp) :: k_beta_x, k_beta_y    ! sKBetaX_G, sKBetaY_G
    real(kind=wp) :: k_beta_x_sf, k_beta_y_sf  ! sKBetaXSF_G, sKBetaYSF_G

    ! Wavelengths and characteristic lengths
    real(kind=wp) :: lambda_w               ! sLam_w_G - wiggler period
    real(kind=wp) :: lambda_r               ! sLam_r_G - resonant radiation wavelength
    real(kind=wp) :: gain_length            ! lg_G
    real(kind=wp) :: cooperation_length     ! lc_G

    ! Undulator field shape
    character(32_IP) :: undulator_type      ! zUndType_G
    real(kind=wp) :: kx_undulator, ky_undulator  ! 3D undulator B-field wavenumbers

    ! Focusing
    real(kind=wp) :: focus_factor           ! sFocusfactor_G
    real(kind=wp) :: focus_factor_saved     ! For backup
    real(kind=wp) :: fx, fy                 ! Focusing in x, y

    ! Absorption
    real(kind=wp) :: beta_absorption        ! sBeta_G

    ! Undulator ends modeling
    logical :: model_undulator_ends         ! qUndEnds_G
    real(kind=wp) :: z_start_undulator, z_end_undulator  ! Markers
    integer(kind=ip) :: undulator_position  ! iUndPlace_G (start/middle/end)

    ! Field-dependent parameters (updated during tapering)
    real(kind=wp) :: n2col                  ! Fractional change in aw
    real(kind=wp) :: n2col_initial          ! n2col0 - at start of module
    real(kind=wp) :: undulator_gradient     ! undgrad - d/dz of n2col
    real(kind=wp) :: z_taper_start          ! sz0
    real(kind=wp) :: m2col                  ! Fractional change in eta (redundant)

    ! Scaling factor (from cf1_G calculation)
    real(kind=wp) :: coefficient_1
end type tFELPhysics

! ============================================================================
! 4. LATTICE ELEMENTS TYPE - All element types in one unified structure
! ============================================================================
type :: tLatticeElements
    ! Counters for each element type
    integer(kind=ip) :: num_undulators, num_chicanes, num_drifts
    integer(kind=ip) :: num_modulations, num_quadrupoles

    ! Overall tracking
    integer(kind=ip) :: cumulative_steps    ! iCsteps - sum across all modules
    integer(kind=ip) :: num_modules
    integer(kind=ip) :: module_count
    integer(kind=ip) :: current_module

    ! -------- UNDULATOR ELEMENTS --------
    real(kind=wp), allocatable :: und_z_mod(:)        ! zMod - position modulators
    real(kind=wp), allocatable :: und_field(:)        ! mf - field strength
    real(kind=wp), allocatable :: und_delta_z(:)      ! delmz - step size per module
    real(kind=wp), allocatable :: und_taper(:)        ! tapers - field taper
    real(kind=wp), allocatable :: und_ux(:), und_uy(:)  ! ux_arr, uy_arr - orientation
    real(kind=wp), allocatable :: und_kbx(:), und_kby(:)  ! kbnx_arr, kbny_arr
    character(32_ip), allocatable :: und_type(:)      ! zundtype_arr
    integer(kind=ip), allocatable :: und_nsteps(:)    ! nSteps_arr - steps per module

    ! -------- CHICANE ELEMENTS --------
    real(kind=wp), allocatable :: chic_zbar(:)        ! zbar position
    real(kind=wp), allocatable :: chic_slip(:)        ! Slip
    real(kind=wp), allocatable :: chic_disp(:)        ! Dispersion

    ! -------- DRIFT ELEMENTS --------
    real(kind=wp), allocatable :: drift_zbar(:)       ! zbar position

    ! -------- MODULATION ELEMENTS --------
    real(kind=wp), allocatable :: enmod_wavenum(:)    ! enmod_wavenum
    real(kind=wp), allocatable :: enmod_mag(:)        ! enmod_mag

    ! -------- QUADRUPOLE ELEMENTS --------
    real(kind=wp), allocatable :: quad_fx(:)
    real(kind=wp), allocatable :: quad_fy(:)

    ! Total undulator line length
    real(kind=wp) :: total_line_length
end type tLatticeElements

! ============================================================================
! 5. INTEGRATION STATE TYPE - Loop control and convergence tracking
! ============================================================================
type :: tIntegrationState
    ! Step counters
    integer(kind=ip) :: current_step        ! iStep
    integer(kind=ip) :: total_steps         ! nSteps
    integer(kind=ip) :: start_step          ! start_step (for resuming)
    integer(kind=ip) :: count               ! iCount (for output cycling)

    ! Step sizes and resolution
    real(kind=wp) :: step_parameter         ! sStep (may be normalized parameter)
    real(kind=wp) :: step_size              ! sStepSize (physical step size)

    ! Diffraction support
    real(kind=wp) :: diffraction_step_size  ! diffStep
    integer(kind=ip) :: steps_per_diffraction  ! iSteps4Diff

    ! Redistribution
    real(kind=wp) :: redistribution_length  ! sRedistLen_G
    integer(kind=ip) :: redistribution_step ! iRedistStp_G

    ! Current z position
    real(kind=wp) :: z_current              ! sZl (if tracked separately)

    ! Timing
    real(kind=wp) :: time_start, time_end
    real(kind=wp) :: time_debug1, time_debug2
end type tIntegrationState

! ============================================================================
! 6. OUTPUT CONFIGURATION TYPE - IO and data writing
! ============================================================================
type :: tOutputConfig
    ! Output descriptors for different data types
    TYPE(cArraySegment), allocatable :: array_electron(:)  ! tArrayE
    TYPE(cArraySegment), allocatable :: array_field(:)     ! tArrayA
    TYPE(cArraySegment) :: array_z                         ! tArrayZ for z position

    ! Write frequency
    integer(kind=ip) :: write_nth_steps         ! iWriteNthSteps
    integer(kind=ip) :: write_nth_steps_intermediate  ! iIntWriteNthSteps

    ! File names and paths
    character(1024_ip) :: main_filename         ! zFileName_G
    character(1024_ip) :: beam_filename         ! zBFile_G
    character(1024_ip) :: seed_filename         ! zSFile_G

    ! Output control flags
    logical :: separate_step_files              ! qSeparateStepFiles_G
    logical :: write_hdf5                       ! qhdf5_G
    logical :: write_sdds                       ! qsdds_G
    integer(kind=ip) :: output_info_level       ! ioutInfo_G

    ! MPI displacement info (for ALLGATHERV)
    integer(kind=ip), allocatable :: field_recv_counts(:)   ! frecvs
    integer(kind=ip), allocatable :: field_displacements(:) ! fdispls

    ! Command line call (for reproducibility)
    character(132_ip) :: command_call           ! cmd_call_G
end type tOutputConfig

! ============================================================================
! 7. SIMULATION FLAGS TYPE - Boolean control flags
! ============================================================================
type :: tSimulationFlags
    ! Physics switches
    logical :: electrons_evolve              ! qElectronsEvolve_G
    logical :: field_evolve                  ! qFieldEvolve_G
    logical :: electron_field_coupling       ! qElectronFieldCoupling_G
    logical :: diffraction                   ! qDiffraction_G
    logical :: focusing                      ! qFocussing_G
    logical :: highpass_filter               ! qFilter

    ! Data management
    logical :: dump_on_crash                 ! qDump_G
    logical :: resume_from_dump              ! qResume
    logical :: write_output                  ! qWrite

    ! Simulation type
    logical :: one_dimensional               ! qOneD_G
    logical :: using_modules                 ! qMod_G

    ! Mesh properties
    logical :: fixed_mesh                    ! qFMesh_G
    logical :: fixed_charge                  ! qFixCharge_G
    logical :: use_emittance                 ! qUseEmit_G
    logical :: scaled_coordinates            ! qscaled_G
    logical :: initial_write_lattice         ! qInitWrLat_G
    logical :: dump_at_end                   ! qDumpEnd_G

    ! Particle arrays status
    logical :: parallel_arrays_ok             ! qPArrOK_G
    logical :: inner_xy_ok                    ! qInnerXYOK_G

    ! Seed field properties
    logical, allocatable :: field_round_edges(:)       ! qRndFj_G
    real(kind=wp), allocatable :: field_edge_sigma(:)  ! sSigFj_G
    logical, allocatable :: match_seed(:)              ! qMatchS_G

    ! Sequencing
    integer(kind=ip) :: num_sequential_parts ! nseqparts_G

    ! Field seed type
    integer(kind=ip) :: field_seed_type

    ! Electron input type
    integer(kind=ip) :: electron_input_type

    ! Tracking load method
    integer(kind=ip) :: tracking_load_method ! TrLdMeth_G

    ! Current mesh calculation
    integer(kind=ip) :: current_mesh_pts     ! npts_I_G
    real(kind=wp) :: current_mesh_dz2        ! dz2_I_G

    ! Seed rounding
    real(kind=wp), allocatable :: seed_sigma(:)  ! sSigEj_G
    logical, allocatable :: seed_round_edges(:)  ! qRndEj_G
end type tSimulationFlags

! ============================================================================
! 8. GLOBAL CONTEXT TYPE - Bundles physics, mesh, and state (optional master type)
! ============================================================================
type :: tSimulationContext
    ! Major data objects
    type(tFieldMesh) :: mesh
    type(tElectronCloud) :: electrons
    type(tFELPhysics) :: physics
    type(tLatticeElements) :: lattice
    type(tIntegrationState) :: integration
    type(tOutputConfig) :: output
    type(tSimulationFlags) :: flags

    ! Initialization data (for restarts)
    type(cInitData) :: init_data

    ! Temporary arrays for RK4 intermediates
    real(kind=wp), allocatable :: dadz_w(:)  ! Temporary for derivative calculations

    ! Fourier transform scaling
    real(kind=wp) :: fourier_scale_factor   ! ffact
end type tSimulationContext

end module GlobalTypes
