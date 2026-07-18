! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

module wigglerVar

use globals
use puffin_kinds
use puffin_mpiInfo
use lattice
use GlobalTypes, only: tUndulator

implicit none

contains



subroutine getAlpha(sZ, und)

! Calculates linear taper based on und%undulator_gradient.
! Reads and writes und%n2col and und%n2col_initial directly;
! no longer touches globals n2col, n2col0, undgrad, sz0, sZFS, sZFE.

  real(kind=wp), intent(in) :: sZ
  type(tUndulator), intent(inout) :: und

  if ((sZ >= und%z_start_undulator) .and. (sZ <= und%z_end_undulator)) then

    und%n2col = und%n2col_initial + und%undulator_gradient*(sZ - und%z_start_undulator)

  else if (sZ > und%z_end_undulator) then

    und%n2col_initial = und%n2col

  end if



end subroutine getAlpha




end module wigglerVar
