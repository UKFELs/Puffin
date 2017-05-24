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

  implicit none

  type fBModulate

!     These describe the physical element:

    real(kind=wp) :: wavenum = 1.0_wp  ! Scaled focusing factors for x and y
    real(kind=wp) :: mag = 0.0_wp

  end type fBModulate

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

  subroutine bModulation(tBMod, sZ2, sGam)

    type(tBMod), intent(in) :: tBMod
    real(kind=wp), contiguous, intent(in) :: sZ2(:)
    real(kind=wp), contiguous, intent(inout) :: sGam(:)

    sGam = sGam + ( tBMod%mag &
               * cos(tBMod%wavenum * sZ2) )

  end subroutine bModulation

end module typeBModulate


