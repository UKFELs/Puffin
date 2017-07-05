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
  use typeLattElm

  implicit none

  private

  type, extends(lelm), public :: fQuad

!     These describe the physical element:

    real(kind=wp) :: qfx  ! Scaled focusing factors for x and y
    real(kind=wp) :: qfy
    
  contains
  
    procedure :: prop => quad
    
  end type fQuad

  contains

! ##############################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Subroutine to model the quad element in Puffin. This is implemented as a
!> simple point transform.
!> @param[in] this Quad described by Fortran type
!> @param[in] sx Electron scaled x coords
!> @param[in] sy Electron scaled y coords
!> @param[in] sz2 Electron z2 coordinates
!> @param[inout] sPr Electron scaled momenta in x, real(p_perp) = px
!> @param[inout] sPi Electron scaled momenta in y, imag(p_perp) = -py
!> @param[in] sgam Electron scaled energy coordinates

  subroutine Quad(self, sX, sY, sZ2, sPr, sPi, sGam, sAperp, tFMesh, &
                          tScale, sZ)

    use typeScale
    use typeFMesh

    class(fQuad), intent(in) :: self
    type(fFMesh), intent(in) :: tFMesh
    type(fScale), intent(in) :: tScale
    real(kind=wp), contiguous, intent(inout) :: sX(:), sY(:), sZ2(:)
    real(kind=wp), contiguous, intent(inout) :: sPr(:), sPi(:), sGam(:)
    real(kind=wp), contiguous, intent(inout) :: sAperp(:)

    real(kind=wp), intent(inout) :: sZ

    real(kind=wp), allocatable :: sp2(:)
    integer(kind=ip) :: iNMPs

    iNMPs = size(sX)

    allocate(sp2(iNMPs))

    call tScale%getP2(sp2, sGam, sPr, sPi)

!    Apply quad transform (point transform)


    if (.not. tScale%qOneD) then

      sPr = sPr + sqrt(tScale%eta) / &
                  (2 * tScale%rho * tScale%kappa) * sX &
                   / self%qfx

      sPi = sPi - sqrt(tScale%eta) / &
                  (2 * tScale%rho * tScale%kappa) * sY &
                  / self%qfy

    end if


    deallocate(sp2)
    

  end subroutine Quad



end module typeQuad
