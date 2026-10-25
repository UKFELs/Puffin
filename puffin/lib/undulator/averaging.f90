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
!> arrays hold slowly varying envelopes instead, one per Cartesian component,
!>
!>     A_x = sqrt(2) Re[ B_x exp(-i z2/2rho) ],    A_y = sqrt(2) Re[ B_y exp(-i z2/2rho) ],
!>
!> component 1 = B_x, component 2 = B_y, and pperp holds only its slow part:
!> the quiver is carried analytically and averaged over an undulator period.
!> The sqrt(2) makes |B_c|^2 the period average of A_c^2, so |B_x|^2 + |B_y|^2
!> is the period-averaged |A_perp|^2 and the power diagnostics need no factor.
!> Both helicities of the resonant field live at the same carrier in this
!> basis - A_perp = A_x - i A_y holds (B_x - i B_y)/sqrt(2) at exp(-i z2/2rho)
!> and the conjugate of (B_x + i B_y)/sqrt(2) at exp(+i z2/2rho) - so any
!> undulator and any seed polarisation is represented.
!>
!> With bx = cx cos(theta_w), by = cy sin(theta_w), theta_w = zbar/2rho, the
!> quiver is
!>
!>     px = -alpha cy cos(theta_w),    py = -alpha cx sin(theta_w),
!>
!> and the electrons couple to the field only through Re[pperp conj(A_perp)]
!> = px A_x + py A_y, which does not mix x and y.  That is why the Cartesian
!> basis is the one to integrate in: each component has its own coupling.
!>
!> Averaging over a period, including the figure-of-eight ripple of the phase
!> theta_j = (zbar - z2_j)/2rho = thetabar_j - xi_j sin(2 theta_w), with
!>
!>     xi_j = (1 + eta p2_j)^3 aw^2 alpha^2 (cy^2 - cx^2)/2 / (4 eta gamma_r^2 Gamma_j^2)
!>
!> (signed: it follows cy^2 - cx^2), turns the quiver into one effective
!> resonant momentum per macroparticle and component,
!>
!>     q_x = -(alpha cy / sqrt(2)) [J0(xi) - J1(xi)] exp(-i thetabar),
!>     q_y = -i (alpha cx / sqrt(2)) [J0(xi) + J1(xi)] exp(-i thetabar),
!>
!> and <p . A> = Re sum_c q_c conj(B_c).  The averaged system is the
!> unaveraged one with pperp replaced by q_c and A_perp by B_c, component by
!> component, in BOTH places they couple - the field source in getSource_*
!> and the energy equation in dgamdz_f.  The period average of the source
!> p_c exp(+i psi) projects to the same q_c, so one array per component serves
!> both, which is what makes the averaged system conserve beam plus field
!> energy exactly.
!>
!> In the helicity basis the same couplings are q_- = -alpha (u- J0 - u+ J1)
!> and q_+ = -alpha (u+ J0 - u- J1), u-+ = (cy +- cx)/2: J0 and J1 swap
!> partners between the two, so the helicities are driven in a ratio that is
!> u+/u- only for helical (u+ = 0) or linear (|u+| = |u-|) undulators.  Those
!> were the two polarisations a single envelope could carry.  For them this
!> reduces exactly to the single-envelope coupling, alpha sqrt(fp) JJ with
!> JJ = J0 - (u+/u-) J1: planar is bit-identical, helical has B_x = Atilde/sqrt(2)
!> and B_y = i B_x.  The derivation is in doc/manual.tex.
!>
!> xi is evaluated per macroparticle: it goes as 1/Gamma^2, and taking it at
!> the reference energy alone, as Genesis does, would put an error of
!> ~0.2 (Gamma_j - 1) into the planar coupling.
!>
!> Odd harmonics.  The same figure-of-eight couples the beam to the odd
!> harmonics of the resonant frequency, and averaged mode can carry them as
!> further bands on the component axis (avgHarmonics in the input file).
!> Band h, h odd, holds
!>
!>     A_c^(h) = sqrt(2) Re[ B_c^(h) exp(-i h z2/2rho) ],
!>
!> and its drive is the period average of sqrt(2) w p_c exp(+i h z2/2rho).
!> With z2/2rho = theta_w - thetabar + xi sin(2 theta_w), Jacobi-Anger gives
!> exp(i h xi sin(2 theta_w)) = sum_n J_n(h xi) exp(2 i n theta_w), and the
!> exp(+-i theta_w) of the quiver leaves a slow term only for n = -(h+-1)/2.
!> With m = (h-1)/2 and J_(-n) = (-1)^n J_n,
!>
!>     q_x^(h) = -(alpha cy / sqrt(2)) (-1)^m [J_m(h xi) - J_(m+1)(h xi)] exp(-i h thetabar),
!>     q_y^(h) = -i (alpha cx / sqrt(2)) (-1)^m [J_m(h xi) + J_(m+1)(h xi)] exp(-i h thetabar),
!>
!> which are q_x, q_y above at h = 1.  Even h has no slow term, because the
!> quiver's exp(+-i theta_w) needs h +- 1 even; that holds on axis, and the
!> off-axis harmonic coupling a 3D beam has is not carried either.  A helical
!> undulator has xi = 0 and J_m(0) = 0 for m >= 1, so no on-axis harmonic
!> coupling at all.  Products of two bands at different carriers average to
!> zero, so each band couples on its own: one q^(h) per component serves the
!> band's source and its share of dGamma/dz, energy is conserved summed over
!> every component, and |A_perp|^2 averages to the sum of |B|^2 over all of
!> them.  Each band diffracts at its own physical wavenumber, its envelope's
!> kz2 less h/2rho (getAvgCarrierKz2).
!>
!> The mesh is shared, so every band has the same envelope band in absolute
!> frequency, h +- 1/(2 lambdarPerCell) in units of the resonant frequency.
!> A detuning that moves the fundamental by d moves harmonic h by h d, so the
!> harmonics leave the band first: an energy spread or offset e is in band for
!> harmonic h while 2 h |e| < 1/(2 lambdarPerCell).
!>
!> Against the unaveraged solver on the 1D seeded planar deck the 3rd harmonic
!> agrees to 0.5%, and the 5th is 16% low, for reasons only partly understood;
!> see W3c in AVERAGED_MODE_ROADMAP.md.

module averaging

use puffin_kinds, only: WP, IP
use puffin_constants, only: pi
use GlobalTypes, only: tUndulator, tFELFrame
use wigglerVar, only: getAlpha

implicit none (type, external)
private

public :: tAvgCoupling, getAvgUndAmps, getAvgPolarisation, &
          getAvgEnvelope, getAvgCoupling, avgJJ, avgJJh, getResonantMomentum, &
          getAvgBufferPqSq, getAvgAlpha, getAvgBands, &
          tAvgFocusing, getAvgFocusCoef, getAvgFocusing, getAvgCarrierKz2


!> The period-averaged coupling at one zbar, common to every macroparticle.
!> The per-particle part (the Bessel factors and the phase) is added by
!> getResonantMomentum.

type :: tAvgCoupling
  real(kind=wp) :: ampX     ! alpha cy / sqrt(2) - |q_x| before its Bessel factor
  real(kind=wp) :: ampY     ! alpha cx / sqrt(2) - |q_y| before its Bessel factor
  real(kind=wp) :: xiCoef   ! xi_j = xiCoef (1 + eta p2_j)^3 / Gamma_j^2
  real(kind=wp) :: pqSq     ! period average of |pperp_w|^2 = alpha^2 fp, for getP2Avg
  real(kind=wp) :: jjRef    ! J0 - J1 (the x factor) for the reference particle, for diagnostics
end type tAvgCoupling


!> The period-averaged transverse focusing at one zbar, common to every
!> macroparticle.  The per-particle (1 + eta p2)/Gamma is applied by
!> getAvgFocusing.  Zero in 1D, where there is no transverse dynamics.

type :: tAvgFocusing
  real(kind=wp) :: cNatX, cNatY   ! natural focusing, scaled by (1 + eta p2)/Gamma
  real(kind=wp) :: cSFX, cSFY     ! strong-focusing channel, energy-independent
end type tAvgFocusing


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


!> The effective alpha at zbar = sZ: the taper (via getAlpha, on a local copy
!> so nothing is mutated) times the end-ramp envelope.  This is the alpha that
!> multiplies the undulator field everywhere in the averaged equations.

  real(kind=wp) function getAvgAlpha(sZ, und, frame)

    real(kind=wp), intent(in) :: sZ
    type(tUndulator), intent(in) :: und
    type(tFELFrame), intent(in) :: frame

    type(tUndulator) :: undLocal

    undLocal = und
    call getAlpha(sZ, undLocal)

    getAvgAlpha = undLocal%n2col * getAvgEnvelope(sZ, und, frame)

  end function getAvgAlpha


!> The period-averaged coupling at zbar = sZ.  alpha includes the taper (via
!> getAlpha, on a local copy so nothing is mutated) and the end-ramp envelope.

  subroutine getAvgCoupling(sZ, und, frame, cpl)

    real(kind=wp), intent(in) :: sZ
    type(tUndulator), intent(in) :: und
    type(tFELFrame), intent(in) :: frame
    type(tAvgCoupling), intent(out) :: cpl

    real(kind=wp) :: cx, cy, uMinus, uPlus, fp, alphaEff, onePlusEtaP2, jjY

    alphaEff = getAvgAlpha(sZ, und, frame)

    call getAvgUndAmps(und%undulator_type, und%fx, und%fy, cx, cy)
    call getAvgPolarisation(cx, cy, uMinus, uPlus, fp)

    cpl%ampX = alphaEff * cy * sqrt(0.5_wp)
    cpl%ampY = alphaEff * cx * sqrt(0.5_wp)
    cpl%pqSq = alphaEff**2 * fp

    cpl%xiCoef = frame%aw**2 * alphaEff**2 * 0.5_wp * (cy**2 - cx**2) &
                 / (4.0_wp * frame%eta * frame%gamma_ref**2)

!   Reference particle: Gamma = 1, no slow transverse momentum

    onePlusEtaP2 = 1.0_wp / sqrt(1.0_wp - (1.0_wp + frame%aw**2 * cpl%pqSq) &
                                          / frame%gamma_ref**2)

    call avgJJ(cpl, onePlusEtaP2, 1.0_wp, cpl%jjRef, jjY)

  end subroutine getAvgCoupling


!> The Bessel factors of the two Cartesian couplings for a macroparticle with
!> the given (1 + eta p2) and Gamma: jjX = J0(xi) - J1(xi), jjY = J0(xi) + J1(xi).
!> xi is signed, so a linear undulator along either axis gets J0(|xi|) - J1(|xi|)
!> on the component it drives.

  elemental subroutine avgJJ(cpl, onePlusEtaP2, gam, jjX, jjY)

    type(tAvgCoupling), intent(in) :: cpl
    real(kind=wp), intent(in) :: onePlusEtaP2, gam
    real(kind=wp), intent(out) :: jjX, jjY

    real(kind=wp) :: xi, j0, j1

    xi = cpl%xiCoef * onePlusEtaP2**3 / gam**2

    j0 = bessel_j0(xi)
    j1 = bessel_j1(xi)

    jjX = j0 - j1
    jjY = j0 + j1

  end subroutine avgJJ


!> The Bessel factors of harmonic h's couplings, h odd and m = (h-1)/2:
!> jjX = (-1)^m [J_m(h xi) - J_(m+1)(h xi)], jjY = (-1)^m [J_m(h xi) + J_(m+1)(h xi)].
!> At h = 1 these are avgJJ's factors.  The fundamental band keeps avgJJ, whose
!> bessel_j0/bessel_j1 need not agree with bessel_jn to the last bit, so that
!> path is unchanged by the harmonics.

  elemental subroutine avgJJh(cpl, h, onePlusEtaP2, gam, jjX, jjY)

    type(tAvgCoupling), intent(in) :: cpl
    integer(kind=ip), intent(in) :: h
    real(kind=wp), intent(in) :: onePlusEtaP2, gam
    real(kind=wp), intent(out) :: jjX, jjY

    real(kind=wp) :: hxi, jm, jm1
    integer :: m

    hxi = real(h, kind=wp) * cpl%xiCoef * onePlusEtaP2**3 / gam**2
    m = int((h - 1_ip) / 2_ip)

    jm = bessel_jn(m, hxi)
    jm1 = bessel_jn(m + 1, hxi)

    jjX = jm - jm1
    jjY = jm + jm1

    if (mod(m, 2) == 1) then
      jjX = -jjX
      jjY = -jjY
    end if

  end subroutine avgJJh


!> The resonant momenta q_x, q_y of every macroparticle, theta_j = (zbar - z2_j)/2rho,
!>
!>     q_x = -ampX jjX exp(-i theta),     q_y = -i ampY jjY exp(-i theta),
!>
!> split into real (column 1 = x, 2 = y) and imaginary parts.  Each column
!> stands in for spr, spi wherever the electrons couple to that field
!> component.  sp2 must already hold the averaged p2 (getP2Avg).  Called from
!> inside the !$OMP PARALLEL region in getrhs, so it is an orphaned !$OMP DO
!> over private scalars, as getP2 is.  A helical undulator has xi = 0, both
!> Bessel factors 1, and skips the Bessel functions.
!>
!> With bandHarm (the field's bandHarm), band b >= 2 fills columns 2b-1 and 2b
!> the same way for its harmonic h, with theta -> h theta and avgJJh's Bessel
!> factors.  Without it, or with the fundamental alone, only columns 1 and 2
!> are touched.

  subroutine getResonantMomentum(sZ, sz2, sgam, sp2, eta, rho, cpl, sprRes, spiRes, bandHarm)

    real(kind=wp), intent(in) :: sZ, eta, rho
    real(kind=wp), contiguous, intent(in) :: sz2(:), sgam(:), sp2(:)
    type(tAvgCoupling), intent(in) :: cpl
    real(kind=wp), contiguous, intent(out) :: sprRes(:,:), spiRes(:,:)
    integer(kind=ip), intent(in), optional :: bandHarm(:)

    integer :: i, ib, nBand
    real(kind=wp) :: inv2rho, theta, cth, sth, jjX, jjY, magX, magY, hth

    inv2rho = 1.0_wp / (2.0_wp * rho)

    if (cpl%xiCoef == 0.0_wp) then

!$OMP DO PRIVATE(theta, cth, sth)
      do i = 1, size(sz2)
        theta = (sZ - sz2(i)) * inv2rho
        cth = cos(theta)
        sth = sin(theta)
        sprRes(i,1) = -cpl%ampX * cth
        spiRes(i,1) = cpl%ampX * sth
        sprRes(i,2) = -cpl%ampY * sth
        spiRes(i,2) = -cpl%ampY * cth
      end do
!$OMP END DO

    else

!$OMP DO PRIVATE(theta, cth, sth, jjX, jjY, magX, magY)
      do i = 1, size(sz2)
        theta = (sZ - sz2(i)) * inv2rho
        cth = cos(theta)
        sth = sin(theta)
        call avgJJ(cpl, 1.0_wp + eta * sp2(i), sgam(i), jjX, jjY)
        magX = cpl%ampX * jjX
        magY = cpl%ampY * jjY
        sprRes(i,1) = -magX * cth
        spiRes(i,1) = magX * sth
        sprRes(i,2) = -magY * sth
        spiRes(i,2) = -magY * cth
      end do
!$OMP END DO

    end if

    nBand = 1
    if (present(bandHarm)) nBand = size(bandHarm)

    if (nBand > 1) then

!$OMP DO PRIVATE(ib, theta, hth, cth, sth, jjX, jjY, magX, magY)
      do i = 1, size(sz2)
        theta = (sZ - sz2(i)) * inv2rho
        do ib = 2, nBand
          hth = real(bandHarm(ib), kind=wp) * theta
          cth = cos(hth)
          sth = sin(hth)
          call avgJJh(cpl, bandHarm(ib), 1.0_wp + eta * sp2(i), sgam(i), jjX, jjY)
          magX = cpl%ampX * jjX
          magY = cpl%ampY * jjY
          sprRes(i,2*ib-1) = -magX * cth
          spiRes(i,2*ib-1) = magX * sth
          sprRes(i,2*ib) = -magY * sth
          spiRes(i,2*ib) = -magY * cth
        end do
      end do
!$OMP END DO

    end if

  end subroutine getResonantMomentum


!> Period-averaged transverse focusing coefficients at zbar = sZ.
!>
!> In 1D bz = 0 and the slow transverse momentum is constant, so averaged mode
!> leaves dpperp/dz at zero.  In 3D the natural focusing has to be put back by
!> hand, because it lives entirely in products of two fast quantities, which is
!> exactly what averaging removes:
!>
!>   1. The bz beat.  bz oscillates at theta_w and, to first order off axis,
!>      bz = sqrt(eta)/(2rho) (-cx x sin theta_w + cy y cos theta_w) for every
!>      undulator type here.  The -i kappa (1+eta p2)/Gamma alpha bz p term in
!>      dpperp/dz has <bz p_slow> = 0, but beating bz against the quiver
!>      pperp_w = -alpha [u+ exp(i theta_w) + u- exp(-i theta_w)] leaves
!>
!>          <bz pperp_w> = -alpha sqrt(eta)/(4rho) (cy^2 y + i cx^2 x),
!>
!>      using u- + u+ = cy and u- - u+ = cx.
!>
!>   2. The off-axis structure of bx, by seen along the quiver excursion.  Only
!>      the curved-pole undulator has any: by = cosh(kx x) cosh(ky y) sin theta_w
!>      and bx = (kx/ky) sinh(kx x) sinh(ky y) sin theta_w.  Neither has a term
!>      linear in x or y, so their own period average vanishes; what survives is
!>      the field sampled along the quiver excursion, x_w = -2 rho C alpha cy
!>      sin theta_w with C = 2 rho kappa (1 + eta p2) / (sqrt(eta) Gamma), the
!>      coefficient in dx/dz.  That gives <d(by)/dx x_w> = -rho C alpha cy^2 kx^2 x
!>      and <bx> = -rho C alpha cy kx^2 y: focusing in x, defocusing in y, which
!>      is what canting the poles is for.  kx_undulator is zero for every other
!>      undulator type, so the term switches itself off.
!>
!> Writing the result as d2x/dz2 = -kbx^2 x, d2y/dz2 = -kby^2 y reproduces the
!> sKBetaX_G / sKBetaY_G that calcScaling sets for the reference particle -
!> kappa/sqrt(2) for helical in both planes, 0 and kappa/sqrt(2) for planepole,
!> kappa/2 and kappa/2 for curved - and testAveraging.pf checks all three.
!>
!> The non-physical strong-focusing channel (qFocussing) is already slow: it
!> adds static terms to bx and by, and survives averaging as it stands, with
!> one factor of alpha as in dppdz_r_f/dppdz_i_f.
!>
!> Dropped, as in the unaveraged-to-averaged step generally, is the radiation
!> term eta p2 A / kappa^2: it rotates against the carrier and averages away.

  subroutine getAvgFocusCoef(sZ, und, frame, qFocusing, foc)

    real(kind=wp), intent(in) :: sZ
    type(tUndulator), intent(in) :: und
    type(tFELFrame), intent(in) :: frame
    logical, intent(in) :: qFocusing
    type(tAvgFocusing), intent(out) :: foc

    real(kind=wp) :: cx, cy, alphaEff, sqrtEta, kxuSq

    alphaEff = getAvgAlpha(sZ, und, frame)

    call getAvgUndAmps(und%undulator_type, und%fx, und%fy, cx, cy)

    sqrtEta = sqrt(frame%eta)

!   Only the curved-pole undulator has off-axis structure in bx, by.  Keyed on
!   the type rather than on kx_undulator, which calcScaling leaves untouched
!   for the helical and elliptical types.  kx_undulator and ky_undulator are
!   always set equal ("giving equal focusing for now"), which is what lets the
!   bz beat keep its on-axis form here.

    kxuSq = 0.0_wp
    if (trim(und%undulator_type) == "curved") kxuSq = und%kx_undulator**2

!   The two parts come from different places, which is why only the first
!   transposes between the planes.
!
!   The 1/4rho terms are each plane's own quiver beating against the on-axis
!   gradient of bz.  With pperp_w = (-cy cos psi, -cx sin psi) and
!   bz = sqrt(eta)/2rho (-fx x sin psi + fy y cos psi), the x force picks up
!   <sin^2> = 1/2 off cx fx and the y force <cos^2> = 1/2 off cy fy, giving
!   cx^2 in x and cy^2 in y.  Genuinely symmetric under x <-> y.
!
!   The kxuSq terms come from the curvature of the pole faces, which is
!   geometry and not a polarisation amplitude, so there is no cx or cy in them
!   to transpose - and a curved pole is not an x <-> y symmetric configuration
!   anyway: the electron wiggles in x alone and both planes are focused by that
!   one quiver, hence the sign difference rather than a swap.  They used to
!   carry cy^2 and cy respectively, which was decoration: kxuSq is nonzero only
!   for "curved", where getAvgUndAmps gives cx = 0, cy = 1 and the field
!   expressions in bfields.f90 contain no fx or fy at all, so cy^2, cy and 1
!   could not be told apart.  Should kx_undulator ever reach another type, or
!   calcScaling stop setting kx == ky, this term needs deriving from the period
!   average of dppdz_r_f/dppdz_i_f rather than trusting either form - see the
!   focusing note in the W3b section of AVERAGED_MODE_ROADMAP.md.

    foc%cNatX = frame%kappa * alphaEff**2 * &
                ( sqrtEta * cx**2 / (4.0_wp * frame%rho) &
                  + frame%rho * kxuSq / sqrtEta )

    foc%cNatY = frame%kappa * alphaEff**2 * &
                ( sqrtEta * cy**2 / (4.0_wp * frame%rho) &
                  - frame%rho * kxuSq / sqrtEta )

    foc%cSFX = 0.0_wp
    foc%cSFY = 0.0_wp

    if (qFocusing) then

      foc%cSFX = alphaEff * sqrtEta * und%k_beta_x_sf**2 / &
                 (2.0_wp * frame%rho * frame%kappa)
      foc%cSFY = alphaEff * sqrtEta * und%k_beta_y_sf**2 / &
                 (2.0_wp * frame%rho * frame%kappa)

    end if

  end subroutine getAvgFocusCoef


!> d/dz of the slow transverse momentum in averaged mode, from getAvgFocusCoef.
!> sdpi carries the opposite sign because spi is -py and dy/dz goes as -spi, so
!> both planes come out focusing for positive coefficients.  sp2 must already
!> hold the averaged p2 (getP2Avg).  Called from inside the !$OMP PARALLEL
!> region in getrhs, so it is an orphaned !$OMP DO, as getResonantMomentum is.

  subroutine getAvgFocusing(sx, sy, sgam, sp2, eta, foc, sdpr, sdpi)

    real(kind=wp), contiguous, intent(in) :: sx(:), sy(:), sgam(:), sp2(:)
    real(kind=wp), intent(in) :: eta
    type(tAvgFocusing), intent(in) :: foc
    real(kind=wp), contiguous, intent(out) :: sdpr(:), sdpi(:)

    integer :: i
    real(kind=wp) :: gam_fac

!$OMP DO PRIVATE(gam_fac)
    do i = 1, size(sx)
      gam_fac = (1.0_wp + eta * sp2(i)) / sgam(i)
      sdpr(i) = -(foc%cNatX * gam_fac + foc%cSFX) * sx(i)
      sdpi(i) = (foc%cNatY * gam_fac + foc%cSFY) * sy(i)
    end do
!$OMP END DO

  end subroutine getAvgFocusing


!> Offset from the stored envelope's z2 wavenumber to the physical one, for
!> the band at harmonic h (1 for the fundamental).  The field is
!> A_c = sqrt(2) Re[B_c exp(-i h z2 / 2rho)], so an envelope Fourier mode at
!> kz2 is physically at kz2 - h/2rho.  Diffraction, and the absorbing
!> boundary, are frequency dependent and must use the physical wavenumber;
!> the envelope's own kz2 = 0 mode is the carrier.  Not applied outside
!> averaged mode, where the mesh already resolves the carrier.

  real(kind=wp) function getAvgCarrierKz2(rho, h)

    real(kind=wp), intent(in) :: rho
    integer(kind=ip), intent(in) :: h

    getAvgCarrierKz2 = -real(h, kind=wp) / (2.0_wp * rho)

  end function getAvgCarrierKz2


!> The harmonic of each averaged-mode field band, from the avgHarmonics input:
!> the fundamental first, then the nonzero entries in ascending order.
!> chkAveraged has already rejected anything but distinct odd harmonics >= 3.

  subroutine getAvgBands(harmIn, bandHarm)

    integer(kind=ip), intent(in) :: harmIn(:)
    integer(kind=ip), allocatable, intent(out) :: bandHarm(:)

    integer(kind=ip), allocatable :: extra(:)
    integer(kind=ip) :: t
    integer :: i, j

    extra = pack(harmIn, harmIn /= 0_ip)

!   Insertion sort - there are at most a handful

    do i = 2, size(extra)
      t = extra(i)
      j = i - 1
      do while (j >= 1)
        if (extra(j) <= t) exit
        extra(j+1) = extra(j)
        j = j - 1
      end do
      extra(j+1) = t
    end do

    allocate(bandHarm(size(extra) + 1))
    bandHarm(1) = 1_ip
    bandHarm(2:) = extra

  end subroutine getAvgBands


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


end module averaging
