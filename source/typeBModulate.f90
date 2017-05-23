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

end module typeBModulate


