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
!> This module contains the type to describe a generic lattice element.
!> Useable types should extend this type.

module typeLattElm

use paratype

implicit none

  type lelm

      real(kind=wp) :: distance

    contains

      procedure :: prop

  end type lelm  

  contains


!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> In general, the propagation through this element should effect
!> the beam and the field.
!> @param[in] tDrift Drift described by Fortran type
!> @param[inout] sx Electron scaled x coords
!> @param[inout] sy Electron scaled y coords
!> @param[in] sz2 Electron z2 coordinates
!> @param[in] sPr Electron scaled momenta in x, real(p_perp) = px
!> @param[in] sPi Electron scaled momenta in y, imag(p_perp) = -py
!> @param[in] sgam Electron scaled energy coordinates
!> @param[inout] saperp Scaled field
!> @param[inout] sZ Scaled distance through the machine



  subroutine prop(self, sX, sY, sZ2, sPr, sPi, sGam, sAperp, tFMesh, &
                          tScale, sZ)

    use gtop2
    use typeFMesh
    use typeScale
    
    class(lelm), intent(in) :: self
    type(fFMesh), intent(in) :: tFMesh
    type(fScale), intent(in) :: tScale
    real(kind=wp), contiguous, intent(inout) :: sX(:), sY(:), sZ2(:)
    real(kind=wp), contiguous, intent(inout) :: sPr(:), sPi(:), sGam(:)
    real(kind=wp), contiguous, intent(inout) :: sAperp(:)
    real(kind=wp), intent(inout) :: sZ
    
    
    
  end subroutine prop

  
end module typeLattElm