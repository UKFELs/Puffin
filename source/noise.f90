! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Module to apply noise to macroparticle positions in one dimension.

MODULE addNoise

use paratype
use randomGauss
use typempicomm
use macrosGen

implicit none

contains


subroutine applyNoise(x,dx,Nks)

! This routine adds noise to the macroparticle 
! weight and phase space coordinate in each
! dimension.

  implicit none

!            ARGUMENTS

  real(kind=wp), intent(inout) :: x(:), Nks(:)
  real(kind=wp), intent(in) :: dx

!            LOCAL ARGS

  real(kind=wp) :: u
  real(kind=wp) :: sv
  integer(kind=ipl) :: k, Nm


  Nm = size(x)

  call init_random_seed()

  do k = 1,Nm

    sv = Nks(k)
    Nks(k) = random_Poisson(Nks(k), .TRUE.)
    if (Nks(k) <= 0.0_wp) then
      Nks(k) = sv
    else
      x(k)   = addxDev(x(k), dx, Nks(k), u)
    end if

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