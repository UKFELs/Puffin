! This module sets up types for variables
! Also sets up physical constants

MODULE typesAndConstants

      USE paratype
      IMPLICIT NONE

!============== Define physical constants ===============================================

      REAL(KIND=WP),    PARAMETER :: pi  = 3.14159265358979323846
      REAL(KIND=WP),    PARAMETER :: c   = 2.99792458e8
      REAL(KIND=WP),    PARAMETER :: e_0 = 8.854187817e-12
      REAL(KIND=WP),    PARAMETER :: m_e = 9.1093826e-31
      REAL(KIND=WP),    PARAMETER :: q_e = 1.60217653e-19

      COMPLEX(KIND=WP), PARAMETER :: ci  = (0.0_WP,1.0_WP)
!
!====================================================================
! Define Global parameters
!
! nSpaceDimensions_CG       - Number of space dimensions
! nPDimensions_CG           - Number of p dimensions
! iX_CG                     - pointer to X direction values in arrays
! iY_CG                     - pointer to Y direction values in arrays
! iZ2_CG                    - pointer to Z2 direction values in arrays
!
! nSwitches_CG              - Number of switch pointers
! iFieldEvolve_CG	    - pointer to if letting field evolve
! iElectronsEvolve_CG	    - pointer to if letting electrons evolve
! iElectronFieldCoupling_CG - pointer to if allowing elctrona and field coupling
! iDiffraction_CG	    - pointer to if allowing diffration
! iInitialGaussField_CG     - pointer to if having a gauss shaped initial field
! iFocussing_CG             - pointer to if natural focussing is included in the transverse plane
! iMatchedBeam_CG           - pointer to if matched electron beam is used in the transverse plane
!
!=====================================================================
!
      INTEGER(KIND=IP), PARAMETER    :: nSpaceDimensions_CG      = 3_IP
      INTEGER(KIND=IP), PARAMETER    :: nPDimensions_CG          = 3_IP
!
      INTEGER(KIND=IP), PARAMETER    :: iX_CG   = 1_IP
      INTEGER(KIND=IP), PARAMETER    :: iY_CG   = 2_IP
      INTEGER(KIND=IP), PARAMETER    :: iZ2_CG  = 3_IP
      INTEGER(KIND=IP), PARAMETER    :: iPX_CG  = 4_IP
      INTEGER(KIND=IP), PARAMETER    :: iPY_CG  = 5_IP
      INTEGER(KIND=IP), PARAMETER    :: iPZ2_CG = 6_IP
!
      INTEGER(KIND=IP), PARAMETER    :: nSwitches_CG              = 11_IP
      INTEGER(KIND=IP), PARAMETER    :: iFieldEvolve_CG           = 1_IP
      INTEGER(KIND=IP), PARAMETER    :: iElectronsEvolve_CG       = 2_IP
      INTEGER(KIND=IP), PARAMETER    :: iElectronFieldCoupling_CG = 3_IP
      INTEGER(KIND=IP), PARAMETER    :: iDiffraction_CG           = 4_IP
      INTEGER(KIND=IP), PARAMETER    :: iFocussing_CG             = 5_IP
      INTEGER(KIND=IP), PARAMETER    :: iMatchedBeam_CG           = 6_IP
      INTEGER(KIND=IP), PARAMETER    :: iOneD_CG                  = 7_IP
      INTEGER(KIND=IP), PARAMETER    :: iFilter_CG                = 8_IP
      INTEGER(KIND=IP), PARAMETER    :: iNoise_CG                 = 9_IP
      INTEGER(KIND=IP), PARAMETER    :: iDump_CG                  = 10_IP
      INTEGER(KIND=IP), PARAMETER    :: iResume_CG                = 11_IP

END MODULE typesAndConstants
