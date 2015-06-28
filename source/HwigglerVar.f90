module wigglerVar

use globals
use paratype
use ParallelInfoType

contains 



subroutine getAlpha(sZ)

! Calculates linear taper based on
! global variable undGrad

  real(kind=wp), intent(in) :: sZ

! Variables n2col, c2col0, undgrad and sz0 are
! global variables, and are defined elsewhere.

  n2col = n2col0  + undgrad*(sz - sz0)

end subroutine getAlpha


end module wigglerVar
