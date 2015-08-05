!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

module Globals

! Module defining shared (global) variables used in Puffin

use paratype
use typesAndConstants
use ArrayFunctions

implicit none


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! field vars

integer(kind=ip) :: NX_G, NBX_G
integer(kind=ip) :: NY_G, NBY_G
integer(kind=ip) :: NZ2_G, NBZ2_G

real(kind=wp)    :: sLengthOfElmX_G 
real(kind=wp)    :: sLengthOfElmY_G 
real(kind=wp)    :: sLengthOfElmZ2_G



integer(kind=ip) :: ReducedNX_G
integer(kind=ip) :: ReducedNY_G
integer(kind=ip) :: outnodex_G,outnodey_G


integer(kind=ip), allocatable  :: iGloNumA_G(:)
integer(kind=ip), allocatable  :: iNodCodA_G(:,:,:)

integer(kind=ip) :: iNodesPerElement_G

integer(kind=ip) :: iNumberNodes_G

integer(kind=ip) :: seedend

real(kind=wp), allocatable :: kx_G(:)
real(kind=wp), allocatable :: ky_G(:)
real(kind=wp), allocatable :: kz2_loc_G(:)

real(kind=wp) :: sBeta_G    ! Absorption coefficient

real(kind=wp)  :: sfilt   ! Frequency cutoff for high pass filter, in units 
                          ! of f_z2 = Lenz2 * ffrac / lamda_rz2

real(kind=wp) :: delta_G  ! Volume of field element (dx * dy * dz2)


real(kind=wp), allocatable :: x_ax_G(:), y_ax_G(:) ! x and y axis for field integration

!   ---   For rounded edge seed field   ---   !

logical, allocatable :: qRndFj_G(:)  ! Rounding edges of flat top seed?

real(kind=wp), allocatable :: sSigFj_G(:) ! sigma of gaussian tail off
                                          ! for flat top seed









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


! Electron macroparticle phase space coordinates 

real(kind=wp), allocatable     :: sElX_G(:)
real(kind=wp), allocatable     :: sElY_G(:)
real(kind=wp), allocatable     :: sElZ2_G(:)
real(kind=wp), allocatable     :: sElPX_G(:)
real(kind=wp), allocatable     :: sElPY_G(:)
real(kind=wp), allocatable     :: sElPZ2_G(:)




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Wiggler vars



real(kind=wp)    :: sRho_save_G,sAw_save_G
real(kind=wp)    :: sRho_G,sAw_G,sGammaR_G
real(kind=wp)    :: sEta_G,sKBeta_G
real(kind=wp)    :: sFocusfactor_G
real(kind=wp)    :: sFocusfactor_save_G
real(kind=wp)    :: fx_G, fy_G


real(kind=wp) :: lam_w_G, lam_r_G   ! wiggler period, resonant wavelength

real(kind=wp) :: lg_G, lc_G  ! gain length, cooperation length



character(32_IP) :: zUndType_G     ! Selects undulator type

real(kind=wp) :: kx_und_G, ky_und_G    ! kx and ky for 3D undulator B-field variation


real(kind=wp)       :: Dfact  ! Dispersion strength factor for chicane
                              ! (=1 for 'normal' dispersion, =0 for 
                              ! isochronous chicanes)

real(kind=wp), allocatable    :: D(:), delta(:), zMod(:), &
                                 mf(:), delmz(:), tapers(:)

integer(kind=ip)    :: ModNum, ModCount








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






real(kind=wp)  :: diffStep ! Stepsize in zbar used for diffraction 

real(kind=wp)  :: ffact    ! Scaling factor for fourier transforms
                           ! (= nnodesX * nnodesY * nnodesz2)





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Integration

integer(kind=ip) :: iCount, iStep, start_step
real(kind=wp)    :: sStep, sStepSize
integer(kind=ip) :: nSteps

real(kind=wp)   :: start_time,end_time
real(kind=wp)   :: time1, time2 !!!FOR DEBUGGING!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Data writing



character(32_IP) :: zDataFileName ! filename extension for 
                                  ! data files

! Type arrays describing electron, field and z output files,
! respectively

TYPE(cArraySegment), save :: tArrayE(nElectronEquations_CG)
TYPE(cArraySegment), save :: tArrayA(nFieldEquations_CG)
TYPE(cArraySegment), save :: tArrayZ


TYPE(cFileType), save :: tPowF   ! Type array describing the power file
	                       ! output



integer(kind=ip) :: iWriteNthSteps, iDumpNthSteps, iIntWriteNthSteps




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Parallel Vars

! These describe the displacement of data across MPI processes (see 
! MPI dcumentation, e.g. inputs of MPI_ALLGATHERV)...



! ...for the full field, when split according to FFTW...

integer(kind=ip), allocatable :: frecvs(:), fdispls(:)

! ...for the full (or 'large') field array, when spread as 
! evenly as possible across processes...

integer(kind=ip), allocatable :: lrecvs(:), ldispls(:)

! ...and for the reduced or active field nodes, split 
! evenly across processes.

integer(kind=ip), allocatable :: mrecvs(:), mdispls(:)



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








End Module Globals
