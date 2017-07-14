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
  use typeLattElm

  implicit none

  private

  type, extends(lelm), public :: fChicane

!     These describe the physical element:

! all electrons recieve same drift! Need to use non-zero disp to create 'real' chicane

!    real(kind=wp) :: zbar  ! Physical length of chicane
    real(kind=wp) :: slip  ! Total delay in chicane
    real(kind=wp) :: disp  ! Dispersion in chicane (if = zero then isochronous)

  contains
    
    procedure :: prop => disperse

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

  subroutine disperse(self, sX, sY, sZ2, sPr, sPi, sGam, sAperp, tFMesh, &
                          tScale, sZ)

    use typeFMesh
    use typeScale

    class(fChicane), intent(in) :: self
    type(fFMesh), intent(in) :: tFMesh
    type(fScale), intent(in) :: tScale
    real(kind=wp), contiguous, intent(inout) :: sX(:), sY(:), sZ2(:)
    real(kind=wp), contiguous, intent(inout) :: sPr(:), sPi(:), sGam(:)
    real(kind=wp), contiguous, intent(inout) :: sAperp(:)
    real(kind=wp), intent(inout) :: sZ

    real(kind=wp) :: szbar4d
    logical :: qDummy

    logical :: qOKL

    szbar4d = self%zbar

!     Propagate through chicane

    sz2 = sz2 - 2.0_WP * self%disp *  &
                 (sgam - 1_wp) &
                 + self%slip

    if (tFMesh%qDiff) then

!  Convert slippage in z2bar to spatial length for diffraction

        if (szbar4d > 0.0_wp) call diffractIM(szbar4d, qDummy, qOKL)

    end if

    sZ = sZ + szbar4d

  end subroutine disperse



end module typeChicane
