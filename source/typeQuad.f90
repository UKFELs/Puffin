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
!> This module contains the type to describe the focusing quads in Puffin

module typeQuad

  use paratype

  implicit none

  type fQuad

!     These describe the physical element:

    real(kind=wp) :: qfx  ! Scaled focusing factors for x and y
    real(kind=wp) :: qfy

  end type fQuad

end module typeQuad
