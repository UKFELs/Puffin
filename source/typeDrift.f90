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

  contains


! ##############################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Subroutine to model the electron drift between other lattice elements.
!> @param[in] tDrift Drift described by Fortran type
!> @param[inout] sx Electron scaled x coords
!> @param[inout] sy Electron scaled y coords
!> @param[in] sz2 Electron z2 coordinates
!> @param[in] sPr Electron scaled momenta in x, real(p_perp) = px
!> @param[in] sPi Electron scaled momenta in y, imag(p_perp) = -py
!> @param[in] sgam Electron scaled energy coordinates
!> @param[inout] saperp Scaled field
!> @param[inout] sZ Scaled distance through the machine

  subroutine driftSection(tDrift, sX, sY, sZ2, sPr, sPi, sGam, sAperp, tFMesh, &
                          tScale, sZ)
 
    use gtop2
    use typeFMesh
    use typeScale
    use pDiff

    type(fDrift), intent(in) :: tDrift
    type(fFMesh), intent(in) :: tFMesh
    type(fScale), intent(in) :: tScale
    real(kind=wp), contiguous, intent(inout) :: sX(:), sY(:), sZ2(:)
    real(kind=wp), contiguous, intent(in) :: sPr(:), sPi(:), sGam(:)
    real(kind=wp), contiguous, intent(inout) :: sAperp(:)

    real(kind=wp), intent(inout) :: sZ

    real(kind=wp) :: del_dr_z

    real(kind=wp), allocatable :: sp2(:)
    integer(kind=ip) :: iNMPs
    logical :: qDummy, qOKL


    del_dr_z = tDrift%zbar

    iNMPs = size(sX)

    allocate(sp2(iNMPs))

    call getP2(sp2, sGam, sPr, sPi, tScale%eta, tScale%gamma_r, tScale%aw)

    sZ2 = sZ2 + del_dr_z * sp2

    if (.not. tScale%qOneD) then

      ! drift in x and y...

      sX = sX + (2 * tScale%rho * tScale%kappa / sqrt(tScale%eta) * &
            (1 + tScale%eta * sp2) / sGam *  &
            sPr) * del_dr_z

      sY = sY - (2 * tScale%rho * tScale%kappa / sqrt(tScale%eta) * &
            (1 + tScale%eta * sp2) / sGam *  &
            sPi) * del_dr_z

    end if

    if (tFMesh%qDiff) call diffractIM(del_dr_z, qDummy, qOKL)

    deallocate(sp2)

    sZ = sZ + del_dr_z

  end subroutine driftSection




end module typeDrift
