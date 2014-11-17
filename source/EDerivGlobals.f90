!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

Module Globals

! Module containing shared variables used in Puffin

USE paratype
USE typesAndConstants
USE ArrayFunctions

IMPLICIT NONE

!********************************************************
! Define Global variables
!
! NX_G            - number of nodes in X direction
! NY_G            - number of nodes in Y direction
! NZ2_G           - number of nodes in Z2 direction
! sLengthOfElmX_G - Length of ONE element in x direction
! sLengthOfElmY_G - Length of ONE element in y direction
! sLengthOfElmZ2_G - Length of ONE element in z2 direction
!
! qElectronsEvolve_G - If letting electrons evolve
! qFieldEvolve_G     - If letting field evolve
! qElectronFieldCoupling_G - If allowing field coupling in
!                            electron equations
! qDiffraction_G     - If allowing for diffraction
! qFocussing_G    -  If natural focussing is included in
!                    the transverse plane
!
! iOrderA_G       - Order of square matrix A
! NNZA_G          - Number of non zero entries of A
! iLenA_G         - Length of array holding sparse matrix A
! iColA_G         - Col Coordinate of A
! iRowA_G         - Row Coordinate of A
! sA_G            - Sparse matrix stored in coordinate
!                   storage format
! iGloNumA_G - AN ARRAY STORING A GLOBAL NUMBER CORRESPONDS
!              TO EACH NODE
! iNodCodA_G - co-ordinates of the nodes
!
! iNodesPerElement_G - Number of nodes per element
!
! sEl_X0Position_G     - Initial electron X position
! sEl_YP0osition_G     - Initial electron Y position
! sEl_Z20Position_G    - Initial electron Z2 position
! rho                - Pierce parameter, describe the
!                      strength of the field
! aw                 - Wiggler parameter
! epsilon            - (1-betaz)/betaz                              
! gamma_r            - Mean electron velocity at resonance
! sFocusfactor       - Focussing factor - sqrt(2) for
!                      natural helical wiggler
!
! iNumberElectrons_G  - Number of electrons (total)
! iNumberNodes_G      - Number of nodes (total)

INTEGER(KIND=IP) :: NX_G, NBX_G
INTEGER(KIND=IP) :: NY_G, NBY_G
INTEGER(KIND=IP) :: NZ2_G, NBZ2_G

REAL(KIND=WP)    :: sLengthOfElmX_G 
REAL(KIND=WP)    :: sLengthOfElmY_G 
REAL(KIND=WP)    :: sLengthOfElmZ2_G
!
LOGICAL          :: qElectronsEvolve_G
LOGICAL          :: qFieldEvolve_G
LOGICAL          :: qElectronFieldCoupling_G
LOGICAL          :: qDiffraction_G
LOGICAL          :: qFocussing_G
LOGICAL          :: qFilter
logical   ::  qDump_G, qResume_G

!
INTEGER(KIND=IP) :: iOrderA_G
INTEGER(KIND=IP) :: NNZA_G      
INTEGER(KIND=IP) :: iLenA_G
INTEGER(KIND=IP) :: ReducedNX_G
INTEGER(KIND=IP) :: ReducedNY_G
INTEGER(KIND=IP) :: outnodex_G,outnodey_G
REAL(KIND=WP), ALLOCATABLE     :: sA_G(:)  
!
INTEGER(KIND=IP), ALLOCATABLE  :: iGloNumA_G(:)
INTEGER(KIND=IP), ALLOCATABLE  :: iNodCodA_G(:,:,:)
!	  
INTEGER(KIND=IP) :: iNodesPerElement_G
!
INTEGER(KIND=IPL), ALLOCATABLE  :: procelectrons_G(:)

REAL(KIND=WP), ALLOCATABLE     :: s_chi_bar_G(:)
REAL(KIND=WP), ALLOCATABLE     :: s_Normalised_chi_G(:)

REAL(KIND=WP)    :: sRho_save_G,sAw_save_G
REAL(KIND=WP)    :: sRho_G,sAw_G,sGammaR_G
REAL(KIND=WP)    :: sEta_G,sKBeta_G
REAL(KIND=WP)    :: sFocusfactor_G
REAL(KIND=WP)    :: sFocusfactor_save_G

!

INTEGER(KIND=IPL) :: iNumberElectrons_G
INTEGER(KIND=IPL) :: iGloNumElectrons_G
INTEGER(KIND=IP) :: iNumberNodes_G,seedend

REAL(KIND=WP)    :: fx_G, fy_G

REAL(KIND=WP), ALLOCATABLE :: kx_G(:)
REAL(KIND=WP), ALLOCATABLE :: ky_G(:)
REAL(KIND=WP), ALLOCATABLE :: kz2_loc_G(:)

REAL(KIND=WP) :: sBeta_G    ! Absorption coefficient

REAL(KIND=WP)  :: sfilt, n2col, m2col, sz0, undgrad, n2col0
  
REAL(KIND=WP) :: delta_G, npk_bar_G
!!!!!
!!!!! NEW FOR NMAIN

LOGICAL          :: qSeparateStepFiles_G

LOGICAL             :: qMod_G

LOGICAL :: qResume, qWrite

REAL(KIND=WP)       :: Dfact
REAL(KIND=WP), ALLOCATABLE    :: D(:), delta(:), zMod(:), &
                                 mf(:), delmz(:), tapers(:)
INTEGER(KIND=IP)    :: ModNum, ModCount

INTEGER(KIND=IP), ALLOCATABLE :: frecvs(:),fdispls(:),&
     lrecvs(:),ldispls(:),mrecvs(:),mdispls(:)
INTEGER(KIND=IP) :: iCount, iStep, start_step
REAL(KIND=WP) :: sStep, sStepSize

INTEGER(KIND=IP) :: nSteps

REAL(KIND=WP)       :: ffact
REAL(KIND=WP)  :: diffStep


real(kind=wp) :: lam_w_G, lam_r_G   ! wiggler period, resonant wavelength

real(kind=wp) :: lg_G, lc_G  ! gain length, cooperation length









!!!!!!    FOR DATA WRITING

CHARACTER(32_IP) :: zDataFileName

TYPE(cArraySegment) :: tArrayE(nElectronEquations_CG)
TYPE(cArraySegment) :: tArrayA(nFieldEquations_CG)
TYPE(cArraySegment) :: tArrayZ


TYPE(cFileType) :: tPowF



INTEGER(KIND=IP) :: iWriteNthSteps, iDumpNthSteps, iIntWriteNthSteps

!!!!!!









REAL(KIND=WP)   :: start_time,end_time
REAL(KIND=WP)   :: time1, time2 !!!FOR DEBUGGING!!!


real(kind=wp), allocatable :: x_ax_G(:), y_ax_G(:) ! x and y axis for field integration

character(32_IP) :: zUndType_G         ! Selects undulator type
real(kind=wp) :: kx_und_G, ky_und_G    ! kx and ky for 3D undulator B-field variation

REAL(KIND=WP), ALLOCATABLE     :: sEl_X0Position_G(:)
REAL(KIND=WP), ALLOCATABLE     :: sEl_Y0Position_G(:)
REAL(KIND=WP), ALLOCATABLE     :: sEl_Z20Position_G(:)
REAL(KIND=WP), ALLOCATABLE     :: sEl_PX0Position_G(:)
REAL(KIND=WP), ALLOCATABLE     :: sEl_PY0Position_G(:)
REAL(KIND=WP), ALLOCATABLE     :: sEl_PZ20Position_G(:)





!    New for frequency oscillation

real(kind=wp) :: t_mag_G 
real(kind=wp) :: t_fr_G  



End Module Globals
