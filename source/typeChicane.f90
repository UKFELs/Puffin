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

end module typeChicane
