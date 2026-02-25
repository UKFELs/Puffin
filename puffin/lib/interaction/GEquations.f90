! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

module Equations


use puffin_kinds
use ArrayFunctions
use Globals
use rhs_vars

implicit none

contains

  subroutine dppdz_r_f(sx, sy, sz2, spr, spi, sgam, &
                       sZ, sdpr)

  	implicit none


    real(kind=wp), contiguous, intent(in) :: sx(:), sy(:), sz2(:), spr(:), &
                                             spi(:), sgam(:)
    real(kind=wp), intent(in) :: sZ
    real(kind=wp), contiguous, intent(out) :: sdpr(:)

    real(kind=wp) :: szt

!$OMP WORKSHARE
    sdpr = sInv2rho * ( n2col * byu  & 
                        - sEta_G * sp2 / sKappa_G**2 *    &
                        sField4ElecReal ) & 
           + sKappa_G * spi / sgam * (1 + sEta_G * sp2) &
               * n2col * bzu
!$OMP END WORKSHARE

  end subroutine dppdz_r_f


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






  subroutine dppdz_i_f(sx, sy, sz2, spr, spi, sgam, sZ, &
                       sdpi)

    implicit none

    real(kind=wp), contiguous, intent(in) :: sx(:), sy(:), sz2(:), spr(:), &
                                             spi(:), sgam(:) 
    real(kind=wp), intent(in) :: sZ
    real(kind=wp), contiguous, intent(out) :: sdpi(:)

    real(kind=wp) :: szt

!$OMP WORKSHARE
    sdpi = sInv2rho * (  n2col * bxu  & 
           - sEta_G * sp2 / sKappa_G**2 * &
                        sField4ElecImag ) & 
           - sKappa_G * spr / sgam * (1 + sEta_G * sp2) &
               * n2col * bzu
!$OMP END WORKSHARE

  end subroutine dppdz_i_f

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dgamdz_f(sx, sy, sz2, spr, spi, sgam, &
                      sdgam)

    implicit none


    real(kind=wp), contiguous, intent(in) :: sx(:), sy(:), sz2(:), spr(:), &
                                             spi(:), sgam(:)

    real(kind=wp), contiguous, intent(out) :: sdgam(:)


!$OMP WORKSHARE

    sdgam = -sRho_G * ( 1 + sEta_G * sp2 ) / sgam * 2_wp *   &
           ( spr * sField4ElecReal + spi * sField4ElecImag ) 

!$OMP END WORKSHARE

  end subroutine dgamdz_f







  subroutine dxdz_f(sx, sy, sz2, spr, spi, sgam, &
                    sdx)

    implicit none

!   Calculate dx/dz
!
!              Arguments:


    real(kind=wp), contiguous,  intent(in) :: sx(:), sy(:), sz2(:), spr(:), &
                                              spi(:), sgam(:)
    real(kind=wp), contiguous, intent(out) :: sdx(:)

!$OMP WORKSHARE

    sdx = 2 * sRho_G * sKappa_G / sqrt(sEta_G) * &
          (1 + sEta_G * sp2) / sgam *  &
          spr

!$OMP END WORKSHARE

  end subroutine dxdz_f





  subroutine dydz_f(sx, sy, sz2, spr, spi, sgam, &
                    sdy)

    implicit none

!   Calculate dy/dz
!
!              Arguments:


    real(kind=wp), contiguous, intent(in) :: sx(:), sy(:), sz2(:), spr(:), &
                                             spi(:), sgam(:)
    
    real(kind=wp), contiguous, intent(out) :: sdy(:)

!$OMP WORKSHARE

    sdy = - 2 * sRho_G * sKappa_G / sqrt(sEta_G) * &
          (1 + sEta_G * sp2) / sgam *  &
          spi

!$OMP END WORKSHARE

  end subroutine dydz_f




  subroutine dz2dz_f(sx, sy, sz2, spr, spi, sgam, &
                     sdz2)

    implicit none

!   Calculate dz2/dz
!
!              Arguments:


    real(kind=wp), contiguous, intent(in) :: sx(:), sy(:), sz2(:), spr(:), &
                                             spi(:), sgam(:)

    real(kind=wp), contiguous, intent(out) :: sdz2(:)

!$OMP WORKSHARE

    sdz2 = sp2

!$OMP END WORKSHARE

  end subroutine dz2dz_f
  






  subroutine alct_e_srtcts(ar_sz)

    implicit none

! Allocate the arrays used in the calculation of
! the electron eqns  

    integer(kind=ip), intent(in) :: ar_sz

    allocate(sp2(ar_sz), sField4ElecReal(ar_sz), &
             sField4ElecImag(ar_sz))! , Lj(ar_sz))

    allocate(bxu(ar_sz), byu(ar_sz), bzu(ar_sz))

  end subroutine alct_e_srtcts



  subroutine dalct_e_srtcts()

    implicit none

! Allocate the arrays used in the calculation of
! the electron eqns  

    deallocate(sp2, sField4ElecReal, &
             sField4ElecImag)! , Lj(ar_sz))

    deallocate(bxu, byu, bzu)

  end subroutine dalct_e_srtcts



  subroutine adjUndPlace(szl)

    real(kind=wp) :: szl

      if (qUndEnds_G) then

        if (szl < 0) then

          print*, 'undulator section not recognised, sz < 0!!'
          stop

        else if (sZl <= sZFS) then 

          iUndPlace_G = iUndStart_G

        else if (sZl >= sZFE) then
 
          iUndPlace_G = iUndEnd_G

        else if ((sZl > sZFS) .and. (sZl < sZFE)) then

          iUndPlace_G = iUndMain_G

        else 

          print*, 'undulator section not recognised, sz > sZFE!!'
          stop

        end if

      else

        iUndPlace_G = iUndMain_G

      end if

  end subroutine adjUndPlace






end module equations

