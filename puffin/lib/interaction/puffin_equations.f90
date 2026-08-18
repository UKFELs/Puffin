! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

module Equations


use puffin_kinds, only: WP, IP
use Globals, only: iUndStart_G, iUndEnd_G, iUndMain_G
use rhs_vars, only: sInv2rho, sp2, sField4ElecReal, sField4ElecImag, bxu, byu, bzu
use GlobalTypes, only: tUndulator, tFELFrame

implicit none (type, external)
private

public :: adjundplace, alct_e_srtcts, bxu, byu, bzu, dalct_e_srtcts, dgamdz_f, dppdz_i_f, &
           dppdz_r_f, dxdz_f, dydz_f, dz2dz_f, sField4ElecImag, sField4ElecReal, sInv2rho, sp2


contains

  subroutine dppdz_r_f(sx, sy, sz2, spr, spi, sgam, &
                       sZ, sdpr, und, frame)

        implicit none (type, external)


    real(kind=wp), contiguous, intent(in) :: sx(:), sy(:), sz2(:), spr(:), &
                                             spi(:), sgam(:)
    real(kind=wp), intent(in) :: sZ
    real(kind=wp), contiguous, intent(out) :: sdpr(:)
    type(tUndulator), intent(in) :: und
    type(tFELFrame), intent(in) :: frame


!$OMP WORKSHARE
    sdpr = sInv2rho * ( und%n2col * byu  &
                        - frame%eta * sp2 / frame%kappa**2 *    &
                        sField4ElecReal ) &
           + frame%kappa * spi / sgam * (1 + frame%eta * sp2) &
               * und%n2col * bzu
!$OMP END WORKSHARE

  end subroutine dppdz_r_f


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




  subroutine dppdz_i_f(sx, sy, sz2, spr, spi, sgam, sZ, &
                       sdpi, und, frame)

    implicit none (type, external)

    real(kind=wp), contiguous, intent(in) :: sx(:), sy(:), sz2(:), spr(:), &
                                             spi(:), sgam(:)
    real(kind=wp), intent(in) :: sZ
    real(kind=wp), contiguous, intent(out) :: sdpi(:)
    type(tUndulator), intent(in) :: und
    type(tFELFrame), intent(in) :: frame


!$OMP WORKSHARE
    sdpi = sInv2rho * (  und%n2col * bxu  &
           - frame%eta * sp2 / frame%kappa**2 * &
                        sField4ElecImag ) &
           - frame%kappa * spr / sgam * (1 + frame%eta * sp2) &
               * und%n2col * bzu
!$OMP END WORKSHARE

  end subroutine dppdz_i_f

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dgamdz_f(sx, sy, sz2, spr, spi, sgam, &
                      sdgam, frame)

    implicit none (type, external)


    real(kind=wp), contiguous, intent(in) :: sx(:), sy(:), sz2(:), spr(:), &
                                             spi(:), sgam(:)

    real(kind=wp), contiguous, intent(out) :: sdgam(:)
    type(tFELFrame), intent(in) :: frame


!$OMP WORKSHARE

    sdgam = -frame%rho * ( 1 + frame%eta * sp2 ) / sgam * 2_wp *   &
           ( spr * sField4ElecReal + spi * sField4ElecImag )

!$OMP END WORKSHARE

  end subroutine dgamdz_f




  subroutine dxdz_f(sx, sy, sz2, spr, spi, sgam, &
                    sdx, frame)

    implicit none (type, external)

!   Calculate dx/dz
!
!              Arguments:


    real(kind=wp), contiguous,  intent(in) :: sx(:), sy(:), sz2(:), spr(:), &
                                              spi(:), sgam(:)
    real(kind=wp), contiguous, intent(out) :: sdx(:)
    type(tFELFrame), intent(in) :: frame

!$OMP WORKSHARE

    sdx = 2 * frame%rho * frame%kappa / sqrt(frame%eta) * &
          (1 + frame%eta * sp2) / sgam *  &
          spr

!$OMP END WORKSHARE

  end subroutine dxdz_f




  subroutine dydz_f(sx, sy, sz2, spr, spi, sgam, &
                    sdy, frame)

    implicit none (type, external)

!   Calculate dy/dz
!
!              Arguments:


    real(kind=wp), contiguous, intent(in) :: sx(:), sy(:), sz2(:), spr(:), &
                                             spi(:), sgam(:)

    real(kind=wp), contiguous, intent(out) :: sdy(:)
    type(tFELFrame), intent(in) :: frame

!$OMP WORKSHARE

    sdy = - 2 * frame%rho * frame%kappa / sqrt(frame%eta) * &
          (1 + frame%eta * sp2) / sgam *  &
          spi

!$OMP END WORKSHARE

  end subroutine dydz_f




  subroutine dz2dz_f(sx, sy, sz2, spr, spi, sgam, &
                     sdz2)

    implicit none (type, external)

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

    implicit none (type, external)

! Allocate the arrays used in the calculation of
! the electron eqns

    integer(kind=ip), intent(in) :: ar_sz

    allocate(sp2(ar_sz), sField4ElecReal(ar_sz), &
             sField4ElecImag(ar_sz))! , Lj(ar_sz))

    allocate(bxu(ar_sz), byu(ar_sz), bzu(ar_sz))

  end subroutine alct_e_srtcts



  subroutine dalct_e_srtcts()

    implicit none (type, external)

! Allocate the arrays used in the calculation of
! the electron eqns

    deallocate(sp2, sField4ElecReal, &
             sField4ElecImag)! , Lj(ar_sz))

    deallocate(bxu, byu, bzu)

  end subroutine dalct_e_srtcts



  subroutine adjUndPlace(szl, und)

! Sets und%undulator_position based on current z position szl.
! Uses und%model_undulator_ends, und%z_start_undulator, und%z_end_undulator.
! Replaces the global iUndPlace_G; no longer touches any globals.

    real(kind=wp), intent(in) :: szl
    type(tUndulator), intent(inout) :: und

      if (und%model_undulator_ends) then

        if (szl < 0) then

          print*, "undulator section not recognised, sz < 0!!"
          stop

        else if (sZl <= und%z_start_undulator) then

          und%undulator_position = iUndStart_G

        else if (sZl >= und%z_end_undulator) then

          und%undulator_position = iUndEnd_G

        else if ((sZl > und%z_start_undulator) .and. (sZl < und%z_end_undulator)) then

          und%undulator_position = iUndMain_G

        else

          print*, "undulator section not recognised, sz > z_end_undulator!!"
          stop

        end if

      else

        und%undulator_position = iUndMain_G

      end if

  end subroutine adjUndPlace




end module equations
