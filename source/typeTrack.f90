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
!> This module contains the type for tracking the absolute, interaction
!> and local distances through the machine.

module typeTrack

  use paratype

  implicit none

  type fTrack

    real(kind=wp) :: szi  ! 'Interaction' distance (of this run) (undulator only distance)
    real(kind=wp) :: szt  ! 'Total', or absolute, distance (including drifts etc)
    real(kind=wp) :: szl  ! 'Local' distance

  end type fTrack

end module typeTrack
