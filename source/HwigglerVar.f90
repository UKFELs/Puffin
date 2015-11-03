module wigglerVar

use globals
use paratype
use ParallelInfoType
use lattice

contains 



subroutine getAlpha(sZ)

! Calculates linear taper based on
! global variable undGrad

  real(kind=wp), intent(in) :: sZ

! Variables n2col, c2col0, undgrad and sz0 are
! global variables, and are defined elsewhere.

  if (sZ <= sZFS) then   ! Work out wiggler ends and bounds???
                          ! e.g. ...    (sZ <= sZFS)  (sZ >= sZFE)
 

    n2col = n2col0 * sin( (sZ - sZ0) / (4_wp * sRho_G) )**2_wp

  else if (sZ >= sZFE) then

    n2col = n2col0 * cos( (sZ - sZFE) / (4_wp * sRho_G) )**2_wp

  else 

    n2col = n2col0  + undgrad*(sz - sz0)

  end if



end subroutine getAlpha



! subroutine getAlpha_end(sZ)

! ! Calculates linear taper based on
! ! global variable undGrad

!   real(kind=wp), intent(in) :: sZ

! ! Variables n2col, c2col0, undgrad and sz0 are
! ! global variables, and are defined elsewhere.

!   real(kind=wp) :: Nwt

!   Nwt = 2.0_wp

!   if (endType == iFront) then
!     n2col = n2col0 * sin((sz-sz0) / 8.0_wp / sRho_G / Nwt)**2.0_wp
!   else if (endType == iBack)
!     n2col = n2col0 * sin((sz-sz0) / 8.0_wp / sRho_G / Nwt)**2.0_wp
!   end if


! end subroutine getAlpha_end




end module wigglerVar
