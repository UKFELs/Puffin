! ###############################################
! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause
! ###############################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> This module contains the type to describe the drift sections in Puffin.

module typeBModulate

  use paratype
  use typeLattElm

  implicit none

  private

  type, extends(lelm), public :: fBModulate

!     These describe the physical element:

    real(kind=wp) :: wavenum = 1.0_wp  ! Scaled focusing factors for x and y
    real(kind=wp) :: mag = 0.0_wp

  contains
    
    procedure :: prop => bModulation

  end type fBModulate

  contains

! ##############################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Apply a simple energy modulation to the beam in Puffin.
!> @param[in] tBMod Beam energy modulation described by Fortran type
!> @param[in] sz2 Electron z2 coordinates
!> @param[inout] sgam Electron scaled energy coordinates

  subroutine bModulation(self, sX, sY, sZ2, sPr, sPi, sGam, sAperp, tFMesh, &
                          tScale, sZ)

    use typeFMesh
    use typeScale

    class(fBModulate), intent(in) :: self
    type(fFMesh), intent(in) :: tFMesh
    type(fScale), intent(in) :: tScale
    real(kind=wp), contiguous, intent(inout) :: sX(:), sY(:), sZ2(:)
    real(kind=wp), contiguous, intent(inout) :: sPr(:), sPi(:), sGam(:)
    real(kind=wp), contiguous, intent(inout) :: sAperp(:)
    real(kind=wp), intent(inout) :: sZ

    sGam = sGam + ( self%mag &
               * cos(self%wavenum * sZ2) )

  end subroutine bModulation

end module typeBModulate


