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

    real(kind=wp) :: kux, kuy  ! undulator field x and y variation (from curved 
                               ! poles, if used)

    real(kind=wp) :: kbx, kby  ! Scaled betatron wavenumbers in x and y from
                               ! in-undulator 'strong' focusing
    integer(kind=ip) :: Nw     ! Number of periods
    logical :: qUndEnds   ! If simulating undulator ends
    character(32_ip) :: zundtype

!        These describe numerical integration: (may spin out to seperate type...)

    real(kind=wp) :: delmz           ! Integration step size
    integer(kind=ip) :: isteps4diff  ! Steps per diffraction step
    integer(kind=ip) :: nsteps  ! Total number of steps

    real(kind=wp) :: sZFS, sZFE ! Start and end of the 'main' undulator,
                                ! when ignoring the ends (local - so with
                                ! no ends, sZFS = 0, and sZFE = module length).
  end type fUndMod

end module typeUndMod
