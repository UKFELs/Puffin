! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Period-averaged (SVEA) electron-field coupling, for the opt-in averaged
!> solver mode (qAveraged).
!>
!> Puffin normally resolves the radiation carrier on the z2 mesh and the
!> undulator quiver in the electron equations.  In averaged mode the field
!> arrays hold a slowly varying envelope instead,
!>
!>     A_perp = (u-/sqrt(fp)) Atilde exp(-i z2/2rho)  +  (u+/sqrt(fp)) conj(Atilde) exp(+i z2/2rho)
!>
!> and pperp holds only its slow part: the quiver is carried analytically and
!> averaged over an undulator period.  The carrier sign is fixed by Puffin's
!> own conventions - with bx = cx cos(zbar/2rho), by = cy sin(zbar/2rho) the
!> quiver is
!>
!>     pperp_w = -alpha [ u+ exp(+i zbar/2rho) + u- exp(-i zbar/2rho) ],
!>     u- = (cy + cx)/2,   u+ = (cy - cx)/2,
!>
!> and the resonant field component is the one at exp(-i z2/2rho).  (The
!> seed Puffin builds for sA0_X = sA0_Y is exactly that helicity.)
!>
!> Averaging the coupling over a period turns the quiver into a single
!> effective 'resonant momentum' per macroparticle,
!>
!>     ptilde_j = -alpha sqrt(fp) JJ_j exp(-i theta_j),
!>     theta_j  = (zbar - z2_j) / 2rho,
!>
!> and the averaged system is the unaveraged one with pperp replaced by
!> ptilde in BOTH places it couples to the field - the field source in
!> getSource_* and the energy equation in dgamdz_f.  Using one array for both
!> is what makes the averaged system conserve beam plus field energy exactly:
!> alpha, fp and JJ cannot drift apart because they are only computed here.
!>
!> The normalisation by sqrt(fp), fp = (cx^2 + cy^2)/2, makes |Atilde|^2 the
!> period-averaged |A_perp|^2, so the power diagnostics need no change.
!>
!> JJ comes from the 2 theta_w ripple in |pperp_w|^2 for a planar undulator:
!> p2 ripples, so z2 carries a figure-of-eight excursion of phase amplitude
!>
!>     xi_j = (1 + eta p2_j)^3 aw^2 alpha^2 (cy^2 - cx^2)/2 / (4 eta gamma_r^2 Gamma_j^2),
!>
!> and expanding exp(i xi sin(2 theta_w)) in Bessel functions against the two
!> quiver components leaves JJ = J0(xi) - (u+/u-) J1(xi).  For a helical
!> undulator |pperp_w| is constant, xi = 0 and JJ = 1.  xi is evaluated per
!> macroparticle: it goes as 1/Gamma^2, and taking it at the reference energy
!> alone, as Genesis does, would put an error of ~0.2 (Gamma_j - 1) into the
!> planar coupling.
!>
!> A single envelope is exact only when the two helicity components of the
!> resonant field stay proportional: helical (u+ = 0) or linear (|u+| = |u-|).
!> A general elliptical undulator needs two envelopes, and is rejected.

module averaging

use puffin_kinds, only: WP
use puffin_constants, only: pi
use GlobalTypes, only: tUndulator, tFELFrame
use wigglerVar, only: getAlpha

implicit none (type, external)
private

public :: tAvgCoupling, getAvgUndAmps, getAvgPolarisation, qAvgPolarisationOK, &
          getAvgEnvelope, getAvgCoupling, avgJJ, getResonantMomentum, &
          getAvgSeedFactor, getAvgBufferPqSq


!> The period-averaged coupling at one zbar, common to every macroparticle.
!> The per-particle part (JJ_j and the phase) is added by getResonantMomentum.

type :: tAvgCoupling
  real(kind=wp) :: amp      ! alpha sqrt(fp) - |ptilde| before JJ
  real(kind=wp) :: xiCoef   ! xi_j = xiCoef (1 + eta p2_j)^3 / Gamma_j^2
  real(kind=wp) :: uRatio   ! u+/u-
  real(kind=wp) :: pqSq     ! period average of |pperp_w|^2 = alpha^2 fp, for getP2Avg
  real(kind=wp) :: jjRef    ! JJ for the reference particle, for diagnostics
end type tAvgCoupling


contains

!> On-axis amplitudes (cx, cy) of the undulator field in the main section,
!> bx = cx cos(zbar/2rho), by = cy sin(zbar/2rho).  Mirrors the iUndMain_G
!> branches of getBXfield/getBYfield at x = y = 0.  Takes the type and the
!> polarisation scalars rather than a tUndulator so the seed setup, which runs
!> before the lattice is built, can use it too.

  subroutine getAvgUndAmps(undType, fx, fy, cx, cy)

    character(*), intent(in) :: undType
    real(kind=wp), intent(in) :: fx, fy
    real(kind=wp), intent(out) :: cx, cy

    select case (trim(undType))

    case ("curved", "planepole")

      cx = 0.0_wp
      cy = 1.0_wp

    case ("helical")

      cx = 1.0_wp
      cy = 1.0_wp

    case default

!     'puffin' elliptical undulator - variable polarisation

      cx = fx
      cy = fy

    end select

  end subroutine getAvgUndAmps


!> Helicity weights of the quiver and the polarisation factor
!> fp = (cx^2 + cy^2)/2 = u-^2 + u+^2, the period average of |pperp_w|^2 / alpha^2.

  subroutine getAvgPolarisation(cx, cy, uMinus, uPlus, fp)

    real(kind=wp), intent(in) :: cx, cy
    real(kind=wp), intent(out) :: uMinus, uPlus, fp

    uMinus = 0.5_wp * (cy + cx)
    uPlus = 0.5_wp * (cy - cx)
    fp = 0.5_wp * (cx**2 + cy**2)

  end subroutine getAvgPolarisation


!> Whether a single envelope represents the resonant field exactly - helical
!> (u+ = 0) or linearly polarised (|u+| = |u-|).  u- = 0 is the opposite
!> helicity, resonant with the exp(+i z2/2rho) carrier instead.

  logical function qAvgPolarisationOK(cx, cy)

    real(kind=wp), intent(in) :: cx, cy

    real(kind=wp) :: uMinus, uPlus, fp, tol

    call getAvgPolarisation(cx, cy, uMinus, uPlus, fp)

    tol = 1.0e-12_wp * max(abs(uMinus), abs(uPlus), tiny(1.0_wp))

    qAvgPolarisationOK = (abs(uMinus) > tol) .and. &
                         ((abs(uPlus) <= tol) .or. &
                          (abs(abs(uPlus) - abs(uMinus)) <= tol))

  end function qAvgPolarisationOK


!> Period-averaged envelope of the undulator field, 0 to 1.  In the main
!> section it is 1; over the end ramps it follows the linear ramp of the by
!> field in bfields.f90, which rises over 2 periods (8 pi rho).  The helical
!> bx ramp has a slightly different shape - averaged mode uses the by ramp
!> for both components, which is only a difference in how the coupling is
!> switched on over those two periods.

  real(kind=wp) function getAvgEnvelope(sZ, und, frame)

    real(kind=wp), intent(in) :: sZ
    type(tUndulator), intent(in) :: und
    type(tFELFrame), intent(in) :: frame

    real(kind=wp) :: rampLen

    getAvgEnvelope = 1.0_wp

    if (.not. und%model_undulator_ends) return

    rampLen = 8.0_wp * pi * frame%rho

    if (sZ <= und%z_start_undulator) then
      getAvgEnvelope = sZ / rampLen
    else if (sZ >= und%z_end_undulator) then
      getAvgEnvelope = 1.0_wp - (sZ - und%z_end_undulator) / rampLen
    end if

    getAvgEnvelope = min(max(getAvgEnvelope, 0.0_wp), 1.0_wp)

  end function getAvgEnvelope


!> The period-averaged coupling at zbar = sZ.  alpha includes the taper (via
!> getAlpha, on a local copy so nothing is mutated) and the end-ramp envelope.

  subroutine getAvgCoupling(sZ, und, frame, cpl)

    real(kind=wp), intent(in) :: sZ
    type(tUndulator), intent(in) :: und
    type(tFELFrame), intent(in) :: frame
    type(tAvgCoupling), intent(out) :: cpl

    type(tUndulator) :: undLocal
    real(kind=wp) :: cx, cy, uMinus, uPlus, fp, alphaEff, onePlusEtaP2

    undLocal = und
    call getAlpha(sZ, undLocal)

    alphaEff = undLocal%n2col * getAvgEnvelope(sZ, und, frame)

    call getAvgUndAmps(und%undulator_type, und%fx, und%fy, cx, cy)
    call getAvgPolarisation(cx, cy, uMinus, uPlus, fp)

    cpl%amp = alphaEff * sqrt(fp)
    cpl%uRatio = uPlus / uMinus
    cpl%pqSq = alphaEff**2 * fp

    cpl%xiCoef = frame%aw**2 * alphaEff**2 * 0.5_wp * (cy**2 - cx**2) &
                 / (4.0_wp * frame%eta * frame%gamma_ref**2)

!   Reference particle: Gamma = 1, no slow transverse momentum

    onePlusEtaP2 = 1.0_wp / sqrt(1.0_wp - (1.0_wp + frame%aw**2 * cpl%pqSq) &
                                          / frame%gamma_ref**2)

    cpl%jjRef = avgJJ(cpl, onePlusEtaP2, 1.0_wp)

  end subroutine getAvgCoupling


!> JJ for a macroparticle with the given (1 + eta p2) and Gamma.

  elemental real(kind=wp) function avgJJ(cpl, onePlusEtaP2, gam)

    type(tAvgCoupling), intent(in) :: cpl
    real(kind=wp), intent(in) :: onePlusEtaP2, gam

    real(kind=wp) :: xi

    xi = cpl%xiCoef * onePlusEtaP2**3 / gam**2

    avgJJ = bessel_j0(xi) - cpl%uRatio * bessel_j1(xi)

  end function avgJJ


!> ptilde_j = -alpha sqrt(fp) JJ_j exp(-i theta_j), theta_j = (zbar - z2_j)/2rho,
!> split into real and imaginary parts.  These stand in for spr, spi wherever
!> the electrons couple to the field.  sp2 must already hold the averaged p2
!> (getP2Avg).  Called from inside the !$OMP PARALLEL region in getrhs, so it
!> is an orphaned !$OMP DO over private scalars, as getP2 is.  A helical
!> undulator has xi = 0, JJ = 1, and skips the Bessel functions.

  subroutine getResonantMomentum(sZ, sz2, sgam, sp2, eta, rho, cpl, sprRes, spiRes)

    real(kind=wp), intent(in) :: sZ, eta, rho
    real(kind=wp), contiguous, intent(in) :: sz2(:), sgam(:), sp2(:)
    type(tAvgCoupling), intent(in) :: cpl
    real(kind=wp), contiguous, intent(out) :: sprRes(:), spiRes(:)

    integer :: i
    real(kind=wp) :: inv2rho, theta, mag

    inv2rho = 1.0_wp / (2.0_wp * rho)

    if (cpl%xiCoef == 0.0_wp) then

!$OMP DO PRIVATE(theta)
      do i = 1, size(sz2)
        theta = (sZ - sz2(i)) * inv2rho
        sprRes(i) = -cpl%amp * cos(theta)
        spiRes(i) = cpl%amp * sin(theta)
      end do
!$OMP END DO

    else

!$OMP DO PRIVATE(theta, mag)
      do i = 1, size(sz2)
        theta = (sZ - sz2(i)) * inv2rho
        mag = cpl%amp * avgJJ(cpl, 1.0_wp + eta * sp2(i), sgam(i))
        sprRes(i) = -mag * cos(theta)
        spiRes(i) = mag * sin(theta)
      end do
!$OMP END DO

    end if

  end subroutine getResonantMomentum


!> Upper bound over the module on the period-averaged quiver |pperp|^2, for
!> sizing the parallel field buffers (calcBuff).  p2 grows with |pperp|^2, so
!> taking the larger end of the linear taper, and ignoring the end ramps that
!> only reduce it, over-estimates the slippage - which is the safe side.

  real(kind=wp) function getAvgBufferPqSq(und)

    type(tUndulator), intent(in) :: und

    real(kind=wp) :: cx, cy, uMinus, uPlus, fp, alpha0, alpha1

    call getAvgUndAmps(und%undulator_type, und%fx, und%fy, cx, cy)
    call getAvgPolarisation(cx, cy, uMinus, uPlus, fp)

    alpha0 = und%n2col_initial
    alpha1 = und%n2col_initial + und%undulator_gradient * &
                                 (und%z_end_undulator - und%z_start_undulator)

    getAvgBufferPqSq = fp * max(alpha0**2, alpha1**2)

  end function getAvgBufferPqSq


!> Factor taking the exp(-i z2/2rho) component of a seed field to the stored
!> envelope: Atilde = sqrt(fp)/u- * A+.

  real(kind=wp) function getAvgSeedFactor(undType, fx, fy)

    character(*), intent(in) :: undType
    real(kind=wp), intent(in) :: fx, fy

    real(kind=wp) :: cx, cy, uMinus, uPlus, fp

    call getAvgUndAmps(undType, fx, fy, cx, cy)
    call getAvgPolarisation(cx, cy, uMinus, uPlus, fp)

    getAvgSeedFactor = sqrt(fp) / uMinus

  end function getAvgSeedFactor

end module averaging
