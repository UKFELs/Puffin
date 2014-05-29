!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE addNoise

use paratype
use randomGauss
use parallelInfoType
use macrosGen

implicit none

contains


subroutine applyNoise(x,dx,gam,dgam,Nks)

! This routine adds noise to the macroparticle 
! weight and phase space coordinate in each
! dimension.

  implicit none

!            ARGUMENTS

  real(kind=wp), intent(inout) :: x(:), gam(:), Nks(:)
  real(kind=wp), intent(in) :: dx, dgam

!            LOCAL ARGS

  real(kind=wp) :: u
  integer(kind=ipl) :: k, Nm


  Nm = size(x)

  call init_random_seed()

  do k = 1,Nm

    Nks(k) = random_Poisson(Nks(k), .TRUE.)

    x(k)   = addxDev(x(k), dx, Nks(k), u)

    gam(k) = addxDev(gam(k), dgam, Nks(k), u)

  end do

end subroutine applyNoise



function addxDev(x,dx,Nk,u)

! This function adds on a random deviation to 
! the coordinate x, based on Poisson stats and
! it's weight Nk (which is the number of 
! macroparticles it represents).

  implicit none

!           ARGUMENTS 

  real(kind=wp), intent(in)    ::  x, dx, Nk  
  real(kind=wp), intent(out)   ::  u
  real(kind=wp) :: addxDev

!          LOCAL ARGS

  real(kind=wp) :: randomNum

  randomNum =   RandomNoGenerator(u)
  addxDev   =   x    +    (randomNum - 0.5_WP)  *  dx / SQRT(Nk)

end function addxDev



!!!!!!!!!!!!!!!!!!!!!!

END MODULE addNoise