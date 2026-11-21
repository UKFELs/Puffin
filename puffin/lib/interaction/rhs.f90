! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Module to calculate d/dz of the field values and electron macroparticle
!> coordinates.

module rhs

use puffin_kinds, only: WP, IP
use ArrayFunctions, only: tErrorLog_G, log_error
use Globals, only: NX_G, NY_G, NZ2_G, ntrndsi_G, nspinDX, nspinDY, sLengthOfElmX_G, &
  sLengthOfElmY_G, sLengthOfElmZ2_G, procelectrons_G, iNumberElectrons_G, qElectronsEvolve_G, &
  qFieldEvolve_G, qElectronFieldCoupling_G
use Equations, only: dppdz_r_f, dppdz_i_f, dgamdz_f, dxdz_f, dydz_f, dz2dz_f, &
  adjundplace, sInv2rho, sp2, sField4ElecReal, sField4ElecImag, bxu, byu, bzu
use rhs_vars, only: p_nodes, lis_GR
use wigglerVar, only: getalpha
use FiElec1D, only: getinterps_1d, getffelecs_1d, getsource_1d
use FiElec, only: getinterps_3d, getffelecs_3d, getsource_3d
use gtop2, only: getp2, getp2avg
use averaging, only: tAvgCoupling, getAvgCoupling, getResonantMomentum, &
  tAvgFocusing, getAvgFocusCoef, getAvgFocusing
use ParaField, only: tTransInfo_G
use bfields, only: getbfields
use GlobalTypes, only: tUndulator, tFELFrame, tSimulationContext


use Functions, only: tProcInfo_G
implicit none (type, external)
private

public :: getrhs, releaseRhsWork, IP, log_error, sp2, tErrorLog_G, tProcInfo_G, WP

! Averaged mode only - see averaging.f90. sprRes, spiRes hold the resonant
! momenta q_x, q_y of every band (column per field component, x and y of the
! fundamental first, then each harmonic band), which replace spr, spi wherever
! the electrons couple to that component. One array for both the source and
! the energy equation is what keeps the averaged system energy-conserving.
! sdgamC is one component's share of dGamma/dz.
!
! They are work arrays of getrhs, kept here rather than as locals so that they
! persist between calls, as the rhs_vars arrays do. See ensureRhsWork.

real(kind=wp), allocatable :: sprRes(:,:), spiRes(:,:), sdgamC(:)


contains

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Calculate d/dz of radiation field and electron macroparticle coordinates in
!> 6D phase space
!> @param[in] sz zbar
!> @param[in] sAr Real (x) component of A_perp
!> @param[in] sAi Imaginary (-y) component of A_perp
!> @param[in] sx scaled electron x coordinates
!> @param[in] sy scaled electron y coordinates
!> @param[in] sz2 scaled electron z2 coordinates
!> @param[in] spr scaled real (px) p_perp electron coordinates
!> @param[in] spi scaled real (-py) p_perp electron coordinates
!> @param[in] sgam scaled energy (gamma) electron coordinates
!> @param[out] sdx d/dz of scaled electron x coordinates
!> @param[out] sdy d/dz of scaled electron y coordinates
!> @param[out] sdz2 d/dz of scaled electron z2 coordinates
!> @param[out] sdpr d/dz of scaled real (px) p_perp electron coordinates
!> @param[out] sdpi d/dz of scaled real (-py) p_perp electron coordinates
!> @param[out] sdgam d/dz of scaled energy (gamma) electron coordinates
!> @param[out] sDADzr d/dz of real (x) component of A_perp
!> @param[out] sDADzi d/dz of real (-y) component of A_perp
!> @param[out] qOK Error flag
!> @param[in] und Undulator parameters (replaces undulator globals in chain)
!> @param[in] frame FEL frame parameters (replaces physics globals in chain)

  subroutine getrhs(sz, &
                    sAr, sAi, &
                    sx, sy, sz2, &
                    spr, spi, sgam, &
                    sdx, sdy, sdz2, &
                    sdpr, sdpi, sdgam, &
                    sDADzr, sDADzi, &
                    qOK, ctx)

  use rhs_vars, only: halfx, halfy, dx, dy, dz2

  implicit none (type, external)

!> Inputs %%%
!
! sZ - Propagation distance
! sA - current radiation field vals
! sy - current electron coordinates in all dimensions
!
! Output
! sb  - d/dz of electron phase space positions
! sDADz - RHS of field source term

  real(kind=wp), intent(in) :: sz
  real(kind=wp), contiguous, intent(in) :: sAr(:,:), sAi(:,:)      ! (node, envelope)
  real(kind=wp), contiguous, intent(in)  :: sx(:), sy(:), sz2(:), &
                                            spr(:), spi(:), sgam(:)


  real(kind=wp), contiguous, intent(inout)  :: sdx(:), sdy(:), sdz2(:), &
                                   sdpr(:), sdpi(:), sdgam(:)

  real(kind=wp), contiguous,  intent(inout) :: sDADzr(:,:), sDADzi(:,:)  ! (node, envelope)
  logical, intent(inout) :: qOK
  type(tSimulationContext), intent(inout) :: ctx

  logical :: qOKL

  type(tAvgCoupling) :: cpl
  type(tAvgFocusing) :: foc
  integer(kind=ip) :: ie, nWts

!     Begin

  qOK = .false.
  qOKL = .false.

!     SETUP AND INITIALISE THE PARTICLE'S POSITION
!     The per-macroparticle work arrays persist between calls, and are only
!     (re)allocated here when their shape has changed.

  if (tTransInfo_G%qOneD) then
    nWts = 2_ip
  else
    nWts = 8_ip
  end if

  call ensureRhsWork(iNumberElectrons_G, nWts, ctx%field%nEnv, &
                     size(sdgam, kind=ip), ctx%flags%period_averaged)

!     Initialise right hand side to zero

  sField4ElecReal = 0.0_WP
  sField4ElecImag = 0.0_WP

  call rhs_tmsavers(sz, ctx%und, ctx%frame)  ! This can be moved later...

!     Adjust undulator tuning

  call getAlpha(sZ, ctx%und)
  call adjUndPlace(sZ, ctx%und)

  if (ctx%flags%period_averaged) then
    call getAvgCoupling(sZ, ctx%und, ctx%frame, cpl)
    call getAvgFocusCoef(sZ, ctx%und, ctx%frame, ctx%flags%focusing, foc)
  end if


!$OMP PARALLEL

  if (ctx%flags%period_averaged) then
    call getP2Avg(sp2, sgam, spr, spi, ctx%frame%eta, ctx%frame%gamma_ref, &
                  ctx%frame%aw, cpl%pqSq)
    call getResonantMomentum(sZ, sz2, sgam, sp2, ctx%frame%eta, ctx%frame%rho, cpl, &
                             sprRes, spiRes, ctx%field%bandHarm)
  else
    call getP2(sp2, sgam, spr, spi, ctx%frame%eta, ctx%frame%gamma_ref, ctx%frame%aw)
  end if




  if (tTransInfo_G%qOneD) then



!$OMP WORKSHARE

    p_nodes = int(sz2 / dz2, kind=ip) + 1_IP - (ctx%field%fz2-1)

!$OMP END WORKSHARE

  else

!$OMP WORKSHARE

!    p_nodes = (floor( (sx+halfx)  / dx)  + 1_IP) + &
!              (floor( (sy+halfy)  / dy) * ReducedNX_G )  + &   !  y 'slices' before primary node
!              (ReducedNX_G * ReducedNY_G * &
!                              floor(sz2  / dz2) ) - &
!                              (fz2-1)*ntrnds_G  ! transverse slices before primary node

    p_nodes = (int( (sx+halfx)  / dx, kind=ip)  + 1_IP) + &
              (int( (sy+halfy)  / dy, kind=ip) * nspinDX )  + &   !  y 'slices' before primary node
              (nspinDX * nspinDY * &
                              int(sz2  / dz2, kind=ip) ) - &
                              (ctx%field%fz2-1)*ntrndsi_G  ! transverse slices before primary node

!$OMP END WORKSHARE

  end if




! The field arrays carry an envelope index, and getInterps_* has already done
! the part that is shared between envelopes - locating each macroparticle's
! nodes and computing its interpolation weights into rhs_vars. Only the gather
! and scatter are per-envelope, which is why the envelope loop belongs here
! rather than around getrhs.

  if (tTransInfo_G%qOneD) then
    call getInterps_1D(sz2, ctx%flags, ctx%field)
  else
    call getInterps_3D(sx, sy, sz2, ctx%flags, ctx%field)
  end if

  if (ctx%flags%period_averaged) then

!   Averaged mode drives both Cartesian components of every band, each
!   through its own q_c. sField4ElecReal/Imag hold one component's field at a
!   time, so the energy exchange is summed here, component by component,
!   rather than in the electron block below. Skipped with the coupling off, as
!   zeroing the gathered field does on the unaveraged path.

!$OMP WORKSHARE
    sdgam = 0.0_wp
!$OMP END WORKSHARE

    do ie = 1, ctx%field%nEnv

      if (ie > 1) then
!$OMP WORKSHARE
        sField4ElecReal = 0.0_wp
        sField4ElecImag = 0.0_wp
!$OMP END WORKSHARE
      end if

      if (tTransInfo_G%qOneD) then
        if (ctx%flags%parallel_arrays_ok) then
          call getFFelecs_1D(sAr(:,ie), sAi(:,ie))
          call getSource_1D(sDADzr(:,ie), sDADzi(:,ie), sprRes(:,ie), spiRes(:,ie), &
                            sgam, ctx%frame%eta)
        end if
      else
        if ((ctx%flags%parallel_arrays_ok) .and. (ctx%flags%inner_xy_ok)) then
          call getFFelecs_3D(sAr(:,ie), sAi(:,ie))
          call getSource_3D(sDADzr(:,ie), sDADzi(:,ie), sprRes(:,ie), spiRes(:,ie), &
                            sgam, ctx%frame%eta)
        end if
      end if

      if (qElectronFieldCoupling_G) then
        call dgamdz_f(sx, sy, sz2, sprRes(:,ie), spiRes(:,ie), sgam, &
                      sdgamC, ctx%frame)
!$OMP WORKSHARE
        sdgam = sdgam + sdgamC
!$OMP END WORKSHARE
      end if

    end do

  else

!   Unaveraged: one envelope, the resolved A_perp.

    if (tTransInfo_G%qOneD) then
      if (ctx%flags%parallel_arrays_ok) then
        call getFFelecs_1D(sAr(:,1), sAi(:,1))
        call getSource_1D(sDADzr(:,1), sDADzi(:,1), spr, spi, sgam, ctx%frame%eta)
      end if
    else
      if ((ctx%flags%parallel_arrays_ok) .and. (ctx%flags%inner_xy_ok)) then
        call getFFelecs_3D(sAr(:,1), sAi(:,1))
        call getSource_3D(sDADzr(:,1), sDADzi(:,1), spr, spi, sgam, ctx%frame%eta)
      end if
    end if

  end if



!    IF (ioutside>0) THEN
!       Print*, 'WARNING: ',ioutside,&
!            ' electrons are outside the inner driving core'
!    END IF


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      Calculate electron d/dz of electron equations - if needed

  if (.not. qElectronFieldCoupling_G) then
    sField4ElecReal = 0.0_WP
    sField4ElecImag = 0.0_WP
  end if


    if (qElectronsEvolve_G) then

        if (.not. ctx%flags%period_averaged) then
          call getBFields(sx, sy, sz, &
                          bxu, byu, bzu, ctx%und, ctx%frame)
        end if

!     z2

        CALL dz2dz_f(sx, sy, sz2, spr, spi, sgam, &
                     sdz2)

!     X

        call dxdz_f(sx, sy, sz2, spr, spi, sgam, &
                    sdx, ctx%frame)

!     Y

        call dydz_f(sx, sy, sz2, spr, spi, sgam, &
                    sdy, ctx%frame)


        if (ctx%flags%period_averaged) then

!     Averaged mode: pperp holds only its slow part. The undulator quiver is
!     carried analytically, and the radiation-driven quiver (the A term in
!     dppdz) rotates against the carrier and averages out. What is left is
!     focusing - zero in 1D, where bz = 0 and there is no transverse dynamics,
!     and in 3D the period average of the bz beat against the quiver, plus the
!     strong-focusing channel. See getAvgFocusCoef.

          if (tTransInfo_G%qOneD) then

!$OMP WORKSHARE
            sdpr = 0.0_wp
            sdpi = 0.0_wp
!$OMP END WORKSHARE

          else

            call getAvgFocusing(sx, sy, sgam, sp2, ctx%frame%eta, foc, sdpr, sdpi)

          end if

!     The energy exchange was summed over the field components above.

        else

!     PX (Real pperp)

          call dppdz_r_f(sx, sy, sz2, spr, spi, sgam, sZ, &
                         sdpr, ctx%und, ctx%frame)

!     -PY (Imaginary pperp)

          call dppdz_i_f(sx, sy, sz2, spr, spi, sgam, sz, &
                         sdpi, ctx%und, ctx%frame)

!     P2

          call dgamdz_f(sx, sy, sz2, spr, spi, sgam, &
                       sdgam, ctx%frame)

        end if

    end if



!$OMP END PARALLEL



!    if (qFieldEvolve_G) then

!     Sum dadz from different MPI processes together

!        call sum2RootArr(sDADz,ReducedNX_G*ReducedNY_G*NZ2_G*2,0)

!     Boundary condition dadz = 0 at head of field

!        if (tProcInfo_G%qRoot) sDADz(1:ReducedNX_G*ReducedNY_G) = 0.0_WP
!        if (tProcInfo_G%qRoot) sDADz(ReducedNX_G*ReducedNY_G*NZ2_G + 1: &
!                                     ReducedNX_G*ReducedNY_G*NZ2_G + &
!                                     ReducedNX_G*ReducedNY_G) = 0.0_WP

        !if (tTransInfo_G%qOneD) then
        !  if (tProcInfo_G%qRoot) sDADz=sDADz !sDADz=6.0_WP*sDADz
        !else
        !   if (tProcInfo_G%qRoot) sDADz=sDADz !216.0_WP/8.0_WP*sDADz
        !end if

!    end if

!     Switch field off

    if (.not. qFieldEvolve_G) then
       sDADzr = 0.0_WP
       sDADzi = 0.0_WP
    end if

!     if electrons not allowed to evolve then

    if (.not. qElectronsEvolve_G) then
       sdpr = 0.0_wp
       sdpi = 0.0_wp
       sdgam = 0.0_wp
       sdx   = 0.0_wp
       sdy   = 0.0_wp
       sdz2 = 0.0_wp
    end if

!     The work arrays are kept for the next call - see ensureRhsWork.

    ! Set the error flag and exit

    qOK = .true.

    goto 2000

    call log_error("Error in rhs:getrhs",tErrorLog_G)
    print*,"Error in rhs:getrhs"
2000 continue

  end subroutine getrhs



!        #########################################


!> @brief
!> Make sure the per-macroparticle work arrays of getrhs exist and have the
!> shape this call needs, allocating them only if they do not.
!>
!> getrhs is called four times per RK4 step. Allocating these on entry and
!> freeing them on exit, as it used to, had the allocator hand the memory back
!> to the kernel and fault it in again on every call, which cost 20-26% of the
!> wall time of a 1D run (UKFELs/Puffin#156). They are now allocated on the
!> first call and reused until the number of local macroparticles changes;
!> deallact_rk4_arrs releases them along with the RK4 workspace.
!>
!> Every array is fully written before it is read within a call, bar
!> sField4ElecReal/Imag, which getFFelecs_* accumulates into and which getrhs
!> therefore still zeroes on each call.
!> @param[in] nElec Number of local macroparticles
!> @param[in] nWts Interpolation weights per macroparticle (2 in 1D, 8 in 3D)
!> @param[in] nEnv Number of field envelopes (averaged mode only)
!> @param[in] nGam Size of the caller's dGamma/dz array (averaged mode only)
!> @param[in] qAveraged Whether the averaged-mode arrays are needed

subroutine ensureRhsWork(nElec, nWts, nEnv, nGam, qAveraged)

  integer(kind=ip), intent(in) :: nElec, nWts, nEnv, nGam
  logical, intent(in) :: qAveraged

  if (allocated(p_nodes)) then
    if ((size(p_nodes) /= nElec) .or. (size(lis_GR, 1) /= nWts)) call releaseRhsWork()
  end if

  if (.not. allocated(p_nodes)) then
    allocate(p_nodes(nElec), lis_GR(nWts, nElec))
    allocate(sp2(nElec), sField4ElecReal(nElec), sField4ElecImag(nElec))
    allocate(bxu(nElec), byu(nElec), bzu(nElec))
  end if

  if (qAveraged) then

    if (allocated(sprRes)) then
      if ((size(sprRes, 1) /= nElec) .or. (size(sprRes, 2) /= nEnv) .or. &
          (size(sdgamC) /= nGam)) deallocate(sprRes, spiRes, sdgamC)
    end if

    if (.not. allocated(sprRes)) then
      allocate(sprRes(nElec, nEnv), spiRes(nElec, nEnv), sdgamC(nGam))
    end if

  end if

end subroutine ensureRhsWork


!> @brief
!> Free the work arrays of getrhs. Safe to call when they are not allocated.
!> The next getrhs call allocates them again.

subroutine releaseRhsWork()

  if (allocated(p_nodes)) then
    deallocate(p_nodes, lis_GR)
    deallocate(sp2, sField4ElecReal, sField4ElecImag)
    deallocate(bxu, byu, bzu)
  end if

  if (allocated(sprRes)) deallocate(sprRes, spiRes, sdgamC)

end subroutine releaseRhsWork


!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Initialize data used in the calculation of d/dz of electron beam + radiation
!> field quantities
!> @param[in] sz zbar
!> @param[in] und Undulator parameters
!> @param[in] frame FEL frame parameters

subroutine rhs_tmsavers(sz, und, frame)

use rhs_vars, only: iOutside, retim, ntrans, halfx, halfy, nc, nb, ZOver2rho, salphaSq, &
  sInv2rho, econst, un, dV3, dx, dy, dz2, qoutside, WP

real(kind=wp), intent(in) :: sz
type(tUndulator), intent(in) :: und
type(tFELFrame), intent(in) :: frame

  ioutside=0


!     Define the size of each element

  dx = sLengthOfElmX_G
  dy = sLengthOfElmY_G
  dz2 = sLengthOfElmZ2_G

  dV3 = sLengthOfElmX_G*sLengthOfElmY_G*sLengthOfElmZ2_G


!     Time savers

  sInv2rho    = 1.0_WP/(2.0_WP * frame%rho)

  ZOver2rho   = sz * sInv2rho
  salphaSq    = (2.0_WP * frame%gamma_ref * frame%rho / frame%aw)**2

  un = sqrt(und%fx**2.0_WP + und%fy**2.0_WP)


!     number of transverse nodes

  ntrans = NX_G * NY_G

!     Diff between real and imaginary nodes in the reduced system

  retim = nspinDX*nspinDY*nZ2_G

  econst = frame%aw/(frame%rho*sqrt(2.0_WP*(und%fx**2.0_WP+und%fy**2.0_WP)))

  nc = 2.0_WP*frame%aw**2/(und%fx**2.0_WP + und%fy**2.0_WP)

  nb = 2.0_WP * frame%rho / ((und%fx**2.0_WP+und%fy**2.0_WP)*frame%eta)

  qoutside=.FALSE.
  iOutside=0_IP

!  halfx = ((ReducedNX_G-1) / 2.0_WP) * sLengthOfElmX_G
!  halfy = ((ReducedNY_G-1) / 2.0_WP) * sLengthOfElmY_G

  halfx = ((nspinDX-1) / 2.0_WP) * sLengthOfElmX_G
  halfy = ((nspinDY-1) / 2.0_WP) * sLengthOfElmY_G


end subroutine rhs_tmsavers

end module rhs
