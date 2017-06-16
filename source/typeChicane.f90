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
!> This module contains the type to describe the chicanes in Puffin

module typeChicane

  use paratype

  implicit none

  type fChicane

!     These describe the physical element:

! all electrons recieve same drift! Need to use non-zero disp to create 'real' chicane

    real(kind=wp) :: zbar  ! Physical length of chicane
    real(kind=wp) :: slip  ! Total delay in chicane
    real(kind=wp) :: disp  ! Dispersion in chicane (if = zero then isochronous)

  end type fChicane

  contains


!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Subroutine for modelling the chicane element in Puffin. This is achieved
!> only through a simple point tranform.
!> @param[in] tChicane Chicane described by Fortran type
!> @param[inout] sz2 Electron z2 coordinates
!> @param[in] sgam Electron scaled energy coordinates
!> @param[inout] saperp Scaled field
!> @param[inout] sZ Scaled distance through the machine

  subroutine disperse(tChicane, sz2, sgam, saperp,  tFMesh, sZ)

    use typeFMesh
    use pDiff

    implicit none

    type(fchicane), intent(in) :: tChicane
    type(fFMesh), intent(in) :: tFMesh
    real(kind=wp), contiguous, intent(inout) :: sz2(:)
    real(kind=wp), contiguous, intent(in) :: sgam(:)
    real(kind=wp), contiguous, intent(inout) :: saperp(:)
    real(kind=wp), intent(inout) :: sZ

    real(kind=wp) :: szbar4d
    logical :: qDummy

    logical :: qOKL

    szbar4d = tChicane%zbar

!     Propagate through chicane

    sz2 = sz2 - 2.0_WP * tChicane%disp *  &
                 (sgam - 1_wp) &
                 + tChicane%slip

    if (tFMesh%qDiff) then

!  Convert slippage in z2bar to spatial length for diffraction

        if (szbar4d > 0.0_wp) call diffractIM(szbar4d, qDummy, qOKL)

    end if

    sZ = sZ + szbar4d

  end subroutine disperse



end module typeChicane
