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

! ##############################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Subroutine to model the quad element in Puffin. This is implemented as a
!> simple point transform.
!> @param[in] tQuad Quad described by Fortran type
!> @param[in] sx Electron scaled x coords
!> @param[in] sy Electron scaled y coords
!> @param[in] sz2 Electron z2 coordinates
!> @param[inout] sPr Electron scaled momenta in x, real(p_perp) = px
!> @param[inout] sPi Electron scaled momenta in y, imag(p_perp) = -py
!> @param[in] sgam Electron scaled energy coordinates

  subroutine Quad(tQuad, sX, sY, sZ2, sPr, sPi, sGam)
 
    type(fQuad), intent(in) :: tQuad
    real(kind=wp), contiguous, intent(inout) :: sPr(:), sPi(:)
    real(kind=wp), contiguous, intent(in) :: sX(:), sY(:), sZ2(:), sGam(:)

    real(kind=wp), allocatable :: sp2(:)
    integer(kind=ip) :: iNMPs

    iNMPs = size(sX)

    allocate(sp2(iNMPs))

    call getP2(sp2, sGam, sPr, sPi, sEta_G, sGammaR_G, saw_G)

!    Apply quad transform (point transform)


    if (.not. qOneD_G) then

      sPr = sPr + sqrt(sEta_G) / &
                  (2 * sRho_G * sKappa_G) * sX &
                   / tQuad%qfx

      sPi = sPi - sqrt(sEta_G) / &
                  (2 * sRho_G * sKappa_G) * sY &
                  / tQuad%qfy

    end if


    deallocate(sp2)
    

  end subroutine Quad



end module typeQuad
