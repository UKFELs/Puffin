! ###############################################
! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause
! ###############################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> This module contains physical constants, and identifiers for array indices
!> for size arrays in Puffin.

module puffin_constants

      use puffin_kinds, only: ip, wp
      implicit none

!============== Define physical constants ===========================

      real(kind=wp),    parameter :: pi  = 3.14159265358979323846
      real(kind=wp),    parameter :: c   = 2.99792458e8
      real(kind=wp),    parameter :: e_0 = 8.854187817e-12
      real(kind=wp),    parameter :: m_e = 9.1093826e-31
      real(kind=wp),    parameter :: q_e = 1.60217653e-19

      complex(kind=wp), parameter :: ci  = (0.0_WP,1.0_WP)
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
      integer(kind=ip), parameter    :: nSpaceDimensions_CG      = 3_IP
      integer(kind=ip), parameter    :: nPDimensions_CG          = 3_IP
!
      integer(kind=ip), parameter    :: iX_CG   = 1_IP
      integer(kind=ip), parameter    :: iY_CG   = 2_IP
      integer(kind=ip), parameter    :: iZ2_CG  = 3_IP
      integer(kind=ip), parameter    :: iPX_CG  = 4_IP
      integer(kind=ip), parameter    :: iPY_CG  = 5_IP
      integer(kind=ip), parameter    :: iGam_CG = 6_IP
!
      integer(kind=ip), parameter    :: nSwitches_CG              = 11_IP
      integer(kind=ip), parameter    :: iFieldEvolve_CG           = 1_IP
      integer(kind=ip), parameter    :: iElectronsEvolve_CG       = 2_IP
      integer(kind=ip), parameter    :: iElectronFieldCoupling_CG = 3_IP
      integer(kind=ip), parameter    :: iDiffraction_CG           = 4_IP
      integer(kind=ip), parameter    :: iFocussing_CG             = 5_IP
      integer(kind=ip), parameter    :: iMatchedBeam_CG           = 6_IP
      integer(kind=ip), parameter    :: iOneD_CG                  = 7_IP
      integer(kind=ip), parameter    :: iFilter_CG                = 8_IP
      integer(kind=ip), parameter    :: iNoise_CG                 = 9_IP
      integer(kind=ip), parameter    :: iDump_CG                  = 10_IP
      integer(kind=ip), parameter    :: iResume_CG                = 11_IP

END MODULE puffin_constants
