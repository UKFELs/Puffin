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

public :: adjundplace, bxu, byu, bzu, dgamdz_f, dxdz_f, dydz_f, dz2dz_f, &
           getelecderivs, sField4ElecImag, sField4ElecReal, sInv2rho, sp2


contains

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




!> d/dz of all six electron coordinates, unaveraged mode, in one pass.
!>
!> The z2, x, y and gamma right hand sides are those of dz2dz_f, dxdz_f,
!> dydz_f and dgamdz_f (which averaged mode still calls), written element by
!> element with the same order of operations, so they give the same bits; keep
!> them in step. One loop reads sp2, sgam, the momenta, the gathered field and
!> the undulator field once per macroparticle, where a whole-array statement
!> per equation read them six times over.

  subroutine getElecDerivs(spr, spi, sgam, sZ, &
                           sdx, sdy, sdz2, sdpr, sdpi, sdgam, und, frame)

    implicit none (type, external)

    real(kind=wp), contiguous, intent(in) :: spr(:), spi(:), sgam(:)
    real(kind=wp), intent(in) :: sZ
    real(kind=wp), contiguous, intent(inout) :: sdx(:), sdy(:), sdz2(:), &
                                                sdpr(:), sdpi(:), sdgam(:)
    type(tUndulator), intent(in) :: und
    type(tFELFrame), intent(in) :: frame

    integer(kind=ip) :: i

!$OMP DO
    do i = 1, size(sdz2, kind=ip)

      sdz2(i) = sp2(i)

      sdx(i) = 2 * frame%rho * frame%kappa / sqrt(frame%eta) * &
               (1 + frame%eta * sp2(i)) / sgam(i) *  &
               spr(i)

      sdy(i) = - 2 * frame%rho * frame%kappa / sqrt(frame%eta) * &
               (1 + frame%eta * sp2(i)) / sgam(i) *  &
               spi(i)

      sdpr(i) = sInv2rho * ( und%n2col * byu(i)  &
                          - frame%eta * sp2(i) / frame%kappa**2 *    &
                          sField4ElecReal(i) ) &
                + frame%kappa * spi(i) / sgam(i) * (1 + frame%eta * sp2(i)) &
                    * und%n2col * bzu(i)

      sdpi(i) = sInv2rho * (  und%n2col * bxu(i)  &
                - frame%eta * sp2(i) / frame%kappa**2 * &
                          sField4ElecImag(i) ) &
                - frame%kappa * spr(i) / sgam(i) * (1 + frame%eta * sp2(i)) &
                    * und%n2col * bzu(i)

      sdgam(i) = -frame%rho * ( 1 + frame%eta * sp2(i) ) / sgam(i) * 2_wp *   &
                 ( spr(i) * sField4ElecReal(i) + spi(i) * sField4ElecImag(i) )

    end do
!$OMP END DO

  end subroutine getElecDerivs






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
