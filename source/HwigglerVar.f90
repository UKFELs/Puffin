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

!  if (qFMod_G) then
!    !n2col = 1_wp + t_mag_G*sin(t_fr_G * sZ)  
!    !n2col = 1_wp / n2col
!    n2col = 1_wp
!  else
!  	n2col = sqrt(  ( ( 1 + sAw_G**2.0_wp ) / (sAw_G**2.0_wp * (1 + t_mag_G*sin(t_fr_G * sz))) ) - (1.0_wp /sAw_G**2.0_wp) )
!  end if

!  if (sZ <= sZFS) then   ! Work out wiggler ends and bounds???
                          ! e.g. ...    (sZ <= sZFS)  (sZ >= sZFE)
 

!    n2col = n2col0 * (sin( (sZ - sZ0) / (16_wp * sRho_G) ))**2_wp

!  else if (sZ >= sZFE) then

!    n2col = n2col0 * cos( (sZ - sZFE) / (16_wp * sRho_G) )**2_wp

!  else 


  if ((sZ >= sZFS) .and. (sZ <= sZFE)) then

    if (qFMod_G) then

      n2col = 1_wp

    else

      n2col = sqrt(  ( ( 1 + sAw_G**2.0_wp ) / (sAw_G**2.0_wp * &
                           (1 + t_mag_G*sin(t_fr_G * (sz - sZFS)   ))) ) &
                  - (1.0_wp /sAw_G**2.0_wp) )

!    else

!      n2col = n2col0  + undgrad*(sz - sZFS)  ! linear taper

    end if

  else if (sZ > sZFE) then

    n2col0 = n2col

  end if

!  end if


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
