!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Module defining shared (global) variables used in Puffin

module Globals

use paratype
use typesAndConstants
use ArrayFunctions
use initDataType

implicit none


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! field vars

integer(kind=ip) :: NX_G, NBX_G
integer(kind=ip) :: NY_G, NBY_G
integer(kind=ip) :: NZ2_G, NBZ2_G
integer(kind=ip) :: ntrnds_G, ntrndsi_G

integer(kind=ip) :: nspinDX, nspinDY

real(kind=wp)    :: sLengthOfElmX_G 
real(kind=wp)    :: sLengthOfElmY_G 
real(kind=wp)    :: sLengthOfElmZ2_G



integer(kind=ip) :: iRedNodesX_G
integer(kind=ip) :: iRedNodesY_G
integer(kind=ip) :: outnodex_G,outnodey_G

integer(kind=ip) :: iNodesPerElement_G

integer(kind=ipn) :: iNumberNodes_G

integer(kind=ip) :: seedend

real(kind=wp), allocatable :: kx_G(:)
real(kind=wp), allocatable :: ky_G(:)
real(kind=wp), allocatable :: kz2_loc_G(:)

real(kind=wp) :: sBeta_G    ! Absorption coefficient

real(kind=wp)  :: sfilt   ! Frequency cutoff for high pass filter, in units 
                          ! of f_z2 = Lenz2 * ffrac / lamda_rz2

real(kind=wp) :: delta_G  ! Volume of field element (dx * dy * dz2)



real(kind=wp) :: sMNum_G


real(kind=wp), allocatable :: x_ax_G(:), y_ax_G(:) ! x and y axis for field integration

!   ---   For rounded edge seed field   ---   !

logical, allocatable :: qRndFj_G(:)  ! Rounding edges of flat top seed?

real(kind=wp), allocatable :: sSigFj_G(:) ! sigma of gaussian tail off
                                          ! for flat top seed

logical, allocatable :: qMatchS_G(:)
logical :: qFMesh_G

integer(kind=ip) :: nseqparts_G
logical :: qEquiXY_G
logical :: qFixCharge_G


integer(kind=ip) :: npts_I_G    !  Specifying mesh 4 current calculation

real(kind=wp) :: dz2_I_G

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! electron vars

real(kind=wp), allocatable     :: s_chi_bar_G(:)
real(kind=wp), allocatable     :: s_Normalised_chi_G(:)

integer(kind=ipl), allocatable  :: procelectrons_G(:)

integer(kind=ipl) :: iNumberElectrons_G
integer(kind=ipl) :: iGloNumElectrons_G

real(kind=wp) :: npk_bar_G  ! peak electron number density
                            ! in the scaled xbar, ybar and z2bar
                            ! dimensions

!   ---   For rounded edge beam   ---   !

logical, allocatable :: qRndEj_G(:)
real(kind=wp), allocatable :: sSigEj_G(:) 
real(kind=wp), parameter :: gExtEj_G = 7.5_wp

!  --- Read particle set algorithms ---
integer(kind=ip) :: iInputType_G
integer(kind=ip), parameter :: iGenHom_G = 1_ip
integer(kind=ip), parameter :: iReadDist_G = 2_ip
integer(kind=ip), parameter :: iReadMASP_G = 3_ip
integer(kind=ip), parameter :: iReadH5_G = 4_ip

! --- Reading field algorithms
integer(kind=ip) :: iFieldSeedType_G
integer(kind=ip), parameter :: iSimpleSeed_G = 1_ip
integer(kind=ip), parameter :: iReadH5Field_G = 2_ip

! Electron macroparticle phase space coordinates 

real(kind=wp), allocatable     :: sElX_G(:)
real(kind=wp), allocatable     :: sElY_G(:)
real(kind=wp), allocatable     :: sElZ2_G(:)
real(kind=wp), allocatable     :: sElPX_G(:)
real(kind=wp), allocatable     :: sElPY_G(:)
real(kind=wp), allocatable     :: sElGam_G(:)




real(kind=wp), allocatable     :: dadz_w(:)




! For recording the INTERACTION zbar only - the distance
! with no drifts

real(kind=wp) :: sZi_G, sZlSt_G


! For restarting from a previous run


type(cInitData) :: tInitData_G

! Temporary intermediate arrays for RK4

! *t is 'temp', for intermediate stages of RK4
! d*t and d*m are temp intermediate d/dz of each variable 


! allocate with size iNumberElectrons_G



!real(kind=wp), allocatable :: dxm(:), dxt(:), xt(:)    
!real(kind=wp), allocatable :: dym(:), dyt(:), yt(:)
!real(kind=wp), allocatable :: dpxm(:), dpxt(:), pxt(:)
!real(kind=wp), allocatable :: dpym(:), dpyt(:), pyt(:)
!real(kind=wp), allocatable :: dz2m(:), dz2t(:), z2t(:)
!real(kind=wp), allocatable :: dpz2m(:), dpz2t(:), pz2t(:) 



!real(kind=wp), allocatable :: dAm(:), dAt(:), A_localt(:) 



!real(kind=wp), allocatable :: dxdz(:), dydz(:), dz2dz(:), dpxdz(:), dpydz(:), dpz2dz(:)
!real(kind=wp), allocatable :: dAdz(:)




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Wiggler vars



real(kind=wp)    :: sRho_G,sAw_G,sGammaR_G
real(kind=wp)    :: sEta_G,sKBeta_G, sKappa_G
real(kind=wp)    :: sFocusfactor_G
real(kind=wp)    :: sFocusfactor_save_G
real(kind=wp)    :: fx_G, fy_G


real(kind=wp) :: lam_w_G, lam_r_G   ! wiggler period, resonant wavelength

real(kind=wp) :: lg_G, lc_G  ! gain length, cooperation length



character(32_IP) :: zUndType_G     ! Selects undulator type

real(kind=wp) :: kx_und_G, ky_und_G    ! kx and ky for 3D undulator B-field variation

real(kind=wp) :: sKBetaX_G, sKBetaY_G

real(kind=wp) :: sKBetaXSF_G, sKBetaYSF_G

real(kind=wp)       :: Dfact  ! Dispersion strength factor for chicane
                              ! (=1 for 'normal' dispersion, =0 for 
                              ! isochronous chicanes)




! ****************************************************
! ****************************************************
!     For lattice element type 'undulator'

real(kind=wp), allocatable    :: zMod(:), mf(:), delmz(:), tapers(:), &
                                 ux_arr(:), uy_arr(:), &
                                 kbnx_arr(:), kbny_arr(:) 

                                 
character(32_ip), allocatable :: zundtype_arr(:)

integer(kind=ip), allocatable :: nSteps_arr(:)



! ****************************************************
! ****************************************************
!     For lattice element type 'chicane'

real(kind=wp), allocatable    :: chic_zbar(:), chic_slip(:), &
                                 chic_disp(:)


! ****************************************************
! ****************************************************
!     For lattice element type 'drift'


real(kind=wp), allocatable    :: drift_zbar(:) 



! ****************************************************
! ****************************************************
!     For lattice element type 'modulation'


real(kind=wp), allocatable    :: enmod_wavenum(:), enmod_mag(:)



! ****************************************************
! ****************************************************
!     For lattice element type 'quadrupole'


real(kind=wp), allocatable    :: quad_fx(:), quad_fy(:) 


!     End module specific array definitions
! ****************************************************
! ****************************************************





integer(kind=ip) :: numOfUnds, numOfChics, numOfDrifts, numOfModulations, numOfQuads



integer(kind=ip) :: iCsteps  ! Cumulative steps across all modules

integer(kind=ip)    :: ModNum, ModCount   !  Number of modules and module counter




real(kind=wp) :: n2col ! alpha, fractional change in aw (see 
	                     ! LT Campbell, BWJ McNeil and S Reiche, 
	                     ! New Journal of Physics 16 (2014) 103019)

! The following are used for linear magnetic field tapering
! where n2col = n2col0 + (undgrad * (sz-sz0)) 

real(kind=wp) :: sz0     ! zbar used for beginning of taper i.e.
                         ! (usually, the start of the current undulator module)

real(kind=wp) :: undgrad ! d/dzbar of alpha (n2col)

real(kind=wp) :: n2col0  ! Initial alpha in the current undulator module

real(kind=wp) :: m2col   ! Fractional change in eta due to change in aw
                         ! (redundant) 

logical :: qUndEnds_G     ! If modelling undulator ends

logical :: qhdf5_G, qsdds_G  ! Switches for data output file formats

real(kind=wp) :: sZFS, sZFE  ! Markers for wiggler ends

! Indicates which part of the wiggler we're in -
! i.e. start, middle or end

integer(kind=ip) :: iUndPlace_G
integer(kind=ip), parameter :: iUndStart_G = 1_ip, &
                               iUndEnd_G = 2_ip, &
                               iUndMain_G = 0_ip

real(kind=wp)  :: diffStep ! Stepsize in zbar used for diffraction 

real(kind=wp)  :: ffact    ! Scaling factor for fourier transforms
                           ! (= nnodesX * nnodesY * nnodesz2)




real(kind=wp) :: cf1_G


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Integration

integer(kind=ip) :: iCount, iStep, start_step
real(kind=wp)    :: sStep, sStepSize
integer(kind=ip) :: nSteps

real(kind=wp)   :: start_time,end_time
real(kind=wp)   :: time1, time2 !!!FOR DEBUGGING!!!


real(kind=wp) :: sRedistLen_G
integer(kind=ip) :: iRedistStp_G

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Data writing



character(1024_IP) :: zDataFileName ! filename extension for 
                                  ! data files

! Type arrays describing electron, field and z output files,
! respectively

TYPE(cArraySegment), save :: tArrayE(nElectronEquations_CG)
TYPE(cArraySegment), save :: tArrayA(nFieldEquations_CG)
TYPE(cArraySegment), save :: tArrayZ


TYPE(cFileType), save :: tPowF   ! Type array describing the power file
	                       ! output



integer(kind=ip) :: iWriteNthSteps, iDumpNthSteps, iIntWriteNthSteps


character(132_ip) :: cmd_call_G
character(1024_ip) :: zFileName_G, zBFile_G, zSFile_G


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Parallel Vars

! These describe the displacement of data across MPI processes (see 
! MPI dcumentation, e.g. inputs of MPI_ALLGATHERV)...



! ...for the full field, when split according to FFTW...

integer(kind=ip), allocatable :: frecvs(:), fdispls(:)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Flags



logical   ::  qElectronsEvolve_G ! Integrate electron equations?

logical   ::  qFieldEvolve_G     ! Integrate field equations?

logical   ::  qElectronFieldCoupling_G  ! Electron-field coupling?

logical   ::  qDiffraction_G     ! Model diffraction?

logical   ::  qFocussing_G       ! Provide Focusing for electron beam?

logical   ::  qFilter            ! High pass filter for radiation field
                                 ! during diffraction? If not, the frequencies
                                 ! below the cutoff are simply not diffracted.


logical   ::  qDump_G            ! Dump data in case of crash?

logical   ::  qResume_G          ! Reading from previously crashed runs dump files? (REDUNDANT)

logical   ::  qSeparateStepFiles_G  ! Make seperate sdds files for each phase space coordinate? 


logical   ::  qMod_G  ! Using undulator modules and chicanes?

logical   ::  qResume            ! Reading from previously crashed runs dump files? (ACTUALLY IN USE!!)

logical   ::  qWrite             ! Write data?

logical   ::  qOneD_G

logical   ::  qPArrOK_G

logical   ::  qInnerXYOK_G

logical   ::  qscaled_G

logical   ::  qInitWrLat_G



End Module Globals
