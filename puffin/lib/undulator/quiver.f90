! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Closed-form undulator quiver motion, for the split (Lawson-type) integrator.
!>
!> Puffin resolves the undulator oscillation, so RK4 spends most of its accuracy
!> budget tracking a motion we have a formula for.  Splitting
!>
!>     pperp  =  Pw(zbar)  +  u
!>
!> where Pw is the exact solution of the momentum equation driven by the
!> undulator field alone, leaves u varying only on the gain-length and betatron
!> scales.  RK4 then integrates u, and pperp is reconstructed analytically at
!> each stage - so the quiver carries no truncation error at all.
!>
!> In the main body of the undulator every supported undulator type has an
!> on-axis field of the form
!>
!>     bx = cx cos(zbar/2rho),    by = cy sin(zbar/2rho)
!>
!> with (cx, cy) fixed by the undulator type, so
!>
!>     dPwr/dzbar = alpha cy sin(zbar/2rho) / 2rho
!>     dPwi/dzbar = alpha cx cos(zbar/2rho) / 2rho
!>
!> integrates in closed form, including the linear taper alpha(zbar).
!>
!> Only the on-axis field is removed.  For 'planepole', 'helical' and the
!> variably-polarised default, bx and by have no transverse dependence at all,
!> so the removal is exact.  For 'curved', by carries a cosh(kx x) cosh(ky y)
!> factor and the off-axis remainder stays in u, where RK4 still integrates it -
!> correct, but a smaller gain for that undulator type.
!>
!> The undulator end ramps are not split: their field envelope varies over the
!> ramp, and adjUndPlace switches the field description at the section
!> boundaries.  qQuiverStepOK below reports whether a whole RK4 step lies inside
!> the main section, and the caller falls back to plain RK4 when it does not.
!> The transformation is applied and undone within a single step, so mixing
!> split and unsplit steps in one module is safe.

module quiver

use puffin_kinds, only: WP
use GlobalTypes, only: tUndulator, tFELFrame
use wigglerVar, only: getAlpha

implicit none (type, external)
private

public :: getQuiverAmps, getQuiverDrive, getQuiverMomentum, qQuiverStepOK


contains

!> On-axis amplitudes (cx, cy) of the undulator field in the main section.
!> Mirrors the iUndMain_G branches of getBXfield/getBYfield at x = y = 0.

  subroutine getQuiverAmps(und, cx, cy)

    type(tUndulator), intent(in) :: und
    real(kind=wp), intent(out) :: cx, cy

    select case (trim(und%undulator_type))

    case ("curved", "planepole")

!     bx vanishes on axis (sinh(0)), by -> sin(zbar/2rho)

      cx = 0.0_wp
      cy = 1.0_wp

    case ("helical")

      cx = 1.0_wp
      cy = 1.0_wp

    case default

!     'puffin' elliptical undulator - variable polarisation

      cx = und%fx
      cy = und%fy

    end select

  end subroutine getQuiverAmps


!> alpha(zbar) and its gradient, taken from getAlpha itself (on a local copy of
!> und, so nothing is mutated) rather than reproduced here.  tUndulator holds
!> only scalars, so the copy is cheap, and this cannot drift out of step with
!> the taper the b-field routines actually see.

  subroutine getTaper(sZ, und, alpha, alphaGrad)

    real(kind=wp), intent(in) :: sZ
    type(tUndulator), intent(in) :: und
    real(kind=wp), intent(out) :: alpha, alphaGrad

    type(tUndulator) :: undLocal

    undLocal = und
    call getAlpha(sZ, undLocal)

    alpha = undLocal%n2col
    alphaGrad = und%undulator_gradient

  end subroutine getTaper


!> Analytic quiver momentum Pw(zbar) = \int (1/2rho) alpha(zbar) b^axis dzbar.
!>
!> With theta = zbar/2rho and alpha linear in zbar, integrating by parts gives
!>
!>     Pwr = cy ( -alpha cos(theta) + 2 rho alpha' sin(theta) )
!>     Pwi = cx (  alpha sin(theta) + 2 rho alpha' cos(theta) )
!>
!> which differentiates back to getQuiverDrive exactly.  Only differences of Pw
!> between two zbar values are used, so the integration constant is irrelevant.

  subroutine getQuiverMomentum(sZ, und, frame, Pwr, Pwi)

    real(kind=wp), intent(in) :: sZ
    type(tUndulator), intent(in) :: und
    type(tFELFrame), intent(in) :: frame
    real(kind=wp), intent(out) :: Pwr, Pwi

    real(kind=wp) :: cx, cy, theta, alpha, alphaGrad, twoRho

    call getQuiverAmps(und, cx, cy)
    call getTaper(sZ, und, alpha, alphaGrad)

    twoRho = 2.0_wp * frame%rho
    theta = sZ / twoRho

    Pwr = cy * ( -alpha * cos(theta) + twoRho * alphaGrad * sin(theta) )
    Pwi = cx * (  alpha * sin(theta) + twoRho * alphaGrad * cos(theta) )

  end subroutine getQuiverMomentum


!> d Pw / dzbar - the undulator drive removed from the momentum equations when
!> the split integrator is active.  Must be the exact derivative of
!> getQuiverMomentum, or the reconstruction in rk4par will not close.

  subroutine getQuiverDrive(sZ, und, frame, Fwr, Fwi)

    real(kind=wp), intent(in) :: sZ
    type(tUndulator), intent(in) :: und
    type(tFELFrame), intent(in) :: frame
    real(kind=wp), intent(out) :: Fwr, Fwi

    real(kind=wp) :: cx, cy, theta, alpha, alphaGrad, twoRho

    call getQuiverAmps(und, cx, cy)
    call getTaper(sZ, und, alpha, alphaGrad)

    twoRho = 2.0_wp * frame%rho
    theta = sZ / twoRho

    Fwr = cy * alpha * sin(theta) / twoRho
    Fwi = cx * alpha * cos(theta) / twoRho

  end subroutine getQuiverDrive


!> Whether the RK4 step from sZ to sZ + h lies wholly inside the main undulator
!> section, where the closed form above is valid.

  logical function qQuiverStepOK(sZ, h, und)

    real(kind=wp), intent(in) :: sZ, h
    type(tUndulator), intent(in) :: und

    if (.not. und%model_undulator_ends) then

!     No end ramps - adjUndPlace reports iUndMain_G throughout

      qQuiverStepOK = .true.

    else

      qQuiverStepOK = (sZ > und%z_start_undulator) .and. &
                      ((sZ + h) < und%z_end_undulator)

    end if

  end function qQuiverStepOK

end module quiver
