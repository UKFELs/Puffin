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
!> This module contains the type definition for the undulator module

module typeUndMod

  use paratype

  implicit none

  type fUndMod

!     These describe the physical module:

    real(kind=wp) :: alpha ! scaled tuning alpha = aw / aw0
    real(kind=wp) :: ux, uy ! undulator polarization
    real(kind=wp) :: taper
    real(kind=wp) :: kbx, kby  ! Scaled betatron wavenumbers in x and y from
                               ! in-undulator 'strong' focusing
    integer(kind=ip) :: Nw     ! Number of periods
    logical :: qUndEnds   ! If simulating undulator ends
    character(32_ip) :: zundtype_arr

!          Describe numerical integration: (may spin out to seperate type...)

    real(kind=wp) :: delmz           ! Integration step size
    integer(kind=ip) :: isteps4diff  ! Steps per diffraction step

  end type fUndMod

end module typeUndMod
