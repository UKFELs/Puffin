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

module typeDrift

  use paratype

  implicit none

  type fDrift

!     These describe the physical element:

    real(kind=wp) :: zbar = 0.0_wp  ! Scaled focusing factors for x and y

  end type fDrift

end module typeDrift
