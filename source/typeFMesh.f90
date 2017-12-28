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
!> This module contains the type definition to hold the constants
!> pertaining to the numerical integration.

module typeFMesh

  use paratype

  implicit none

  type fFmesh

!     These describe the field mesh:

    real(kind=wp) :: dx
    real(kind=wp) :: dy
    real(kind=wp) :: dz2
    integer(kind=ip) :: nx
    integer(kind=ip) :: ny
    integer(kind=ip) :: nz2

    integer(kind=ip) :: nbx
    integer(kind=ip) :: nby
    integer(kind=ip) :: nbz2

    integer(kind=ip) :: ntrnds

    integer(kind=ip) :: v3d  ! formerly 'delta_g'
    
    logical qDiff

  end type fFmesh

end module typeFMesh
