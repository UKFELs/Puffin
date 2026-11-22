! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Module containing routines dealing with the interpolation of the
!> macroparticles to the 3D field mesh.
!>
!> The trilinear interpolation of each macroparticle onto its 8 surrounding
!> field nodes is done in a single pass: locate the nodes, form the 8
!> weights, gather the field at the macroparticle and scatter its source term
!> back onto the same nodes. Doing the three in one loop keeps the weights in
!> registers, where they used to be written to an (8, nElectrons) array and
!> read back twice - at 400k macroparticles that was 25 MB streamed three
!> times per RHS evaluation, plus the allocation of it and of a node index
!> array on every call.


module FiElec

use puffin_kinds, only: WP, IPL, IP
use globals, only: NZ2_G, ntrndsi_G, nspinDX, nspinDY, fieldMesh, iTemporal, s_chi_bar_G, &
  procelectrons_G
use GlobalTypes, only: tSimulationFlags, tFieldValues

implicit none (type, external)
private

public :: getinteract_3d


contains


!> Interpolate the field to the macroparticles and their source term back to
!> the field mesh, for one field envelope.
!>
!> On return sFr/sFi hold the field at each macroparticle and sDADzr/sDADzi
!> have had the macroparticles' source term added to them. Both use the same
!> trilinear weights, and every floating point operation, and the order in
!> which contributions are added into each node and each macroparticle, is the
!> one the separate getInterps/getFFelecs/getSource passes used, so the result
!> is bit-for-bit unchanged.
!>
!> A macroparticle outside the inner transverse mesh, or past the end of the
!> local field, clears flags%inner_xy_ok / flags%parallel_arrays_ok as before
!> and is skipped: the caller discards the whole derivative and redistributes
!> when either flag is down, so the remaining macroparticles' contributions
!> are never used, and skipping keeps the out-of-range one from indexing
!> outside the arrays.

subroutine getInteract_3D(sx, sy, sz2, sAr, sAi, sDADzr, sDADzi, &
                          spr, spi, sgam, seta, flags, field)

use rhs_vars, only: halfx, halfy, dx, dy, dz2, dV3, sp2, sField4ElecReal, &
  sField4ElecImag

real(kind=wp), contiguous, intent(in) :: sx(:), sy(:), sz2(:)
real(kind=wp), contiguous, intent(in) :: sAr(:), sAi(:)
real(kind=wp), contiguous, intent(inout) :: sDADzr(:), sDADzi(:)
real(kind=wp), contiguous, intent(in) :: spr(:), spi(:), sgam(:)
real(kind=wp), intent(in) :: seta
type(tSimulationFlags), intent(inout) :: flags
type(tFieldValues), intent(in) :: field

  call getInteract_3D_kernel(sx, sy, sz2, sAr, sAi, sDADzr, sDADzi, &
                             sField4ElecReal, sField4ElecImag, &
                             spr, spi, sgam, seta, s_chi_bar_G, sp2, &
                             halfx, halfy, dx, dy, dz2, dV3, &
                             nspinDX, nspinDY, ntrndsi_G, NZ2_G, &
                             field%fz2, field%bz2, (fieldMesh == iTemporal), &
                             procelectrons_G(1), flags)

end subroutine getInteract_3D



!> The loop of getInteract_3D. Module data comes in as dummy arguments so
!> that its base addresses are loop invariant by construction - otherwise
!> the !$OMP ATOMIC updates, which act as memory barriers, force the array
!> descriptors to be reloaded every iteration (see
!> benchmark/RESULTS-2026-08-07-globals-refactor.md).
!>
!> The scatter needs !$OMP ATOMIC only when several threads share the loop.
!> An atomic update compiles to a locked compare-and-swap whatever the team
!> size, and with 16 per macroparticle it used to dominate the 3D step even
!> on one thread, so a team of one takes plain updates instead. Both branches
!> add the same terms in the same order.

subroutine getInteract_3D_kernel(sx, sy, sz2, sAr, sAi, sDADzr, sDADzi, &
                                 sFr, sFi, spr, spi, sgam, seta, &
                                 s_chi_bar, sp2, halfx, halfy, dx, dy, dz2, dV3, &
                                 nspinDX, nspinDY, ntrndsi, nz2, fz2, bz2, &
                                 qTemporal, nLocalElecs, flags)

!$ use omp_lib, only: omp_get_num_threads

real(kind=wp), contiguous, intent(in) :: sx(:), sy(:), sz2(:)
real(kind=wp), contiguous, intent(in) :: sAr(:), sAi(:)
real(kind=wp), contiguous, intent(inout) :: sDADzr(:), sDADzi(:)
real(kind=wp), contiguous, intent(inout) :: sFr(:), sFi(:)
real(kind=wp), contiguous, intent(in) :: spr(:), spi(:), sgam(:)
real(kind=wp), intent(in) :: seta
real(kind=wp), contiguous, intent(in) :: s_chi_bar(:), sp2(:)
real(kind=wp), intent(in) :: halfx, halfy, dx, dy, dz2, dV3
integer(kind=ip), intent(in) :: nspinDX, nspinDY, ntrndsi, nz2, fz2, bz2
logical, intent(in) :: qTemporal
integer(kind=ipl), intent(in) :: nLocalElecs
type(tSimulationFlags), intent(inout) :: flags

integer(kind=ipl) :: i
integer(kind=ip) :: xnode, ynode, z2node, p, p2, p3, p4, p5, p6, p7, p8
real(kind=wp) :: locx, locy, locz2, x_in1, x_in2, y_in1, y_in2, z2_in1, z2_in2
real(kind=wp) :: w1, w2, w3, w4, w5, w6, w7, w8
real(kind=wp) :: fr, fi, dadzRInst, dadzIInst
logical :: qAtomic

  qAtomic = .false.
!$ qAtomic = (omp_get_num_threads() > 1)

!$OMP DO PRIVATE(xnode, ynode, z2node, p, p2, p3, p4, p5, p6, p7, p8, &
!$OMP locx, locy, locz2, x_in1, x_in2, y_in1, y_in2, z2_in1, z2_in2, &
!$OMP w1, w2, w3, w4, w5, w6, w7, w8, fr, fi, dadzRInst, dadzIInst)
  do i = 1, nLocalElecs

!                  Get surrounding nodes

    xnode = floor( (sx(i) + halfx ) / dx)  + 1_IP
    locx = sx(i) + halfx - real(xnode  - 1_IP, kind=wp) * dx
    x_in2 = locx / dx
    x_in1 = (1.0_wp - x_in2)

    ynode = floor( (sy(i) + halfy )  / dy)  + 1_IP
    locy = sy(i) + halfy - real(ynode  - 1_IP, kind=wp) * dy
    y_in2 = locy / dy
    y_in1 = (1.0_wp - y_in2)

    z2node = floor(sz2(i)  / dz2)  + 1_IP
    locz2 = sz2(i) - real(z2node  - 1_IP, kind=wp) * dz2
    z2_in2 = locz2 / dz2
    z2_in1 = (1.0_wp - z2_in2)

    if (qTemporal) then
      if (z2node >= nz2) then
        print*, "Z2 coord is too large!! with node:", z2node, &
                " and pos ", sz2(i)
        STOP
      end if
    end if

    if ((xnode >= nspinDX) .or. (xnode < 1) .or. &
        (ynode >= nspinDY) .or. (ynode < 1)) then
      flags%inner_xy_ok = .false.
      flags%parallel_arrays_ok = .false.
      cycle
    end if

    if (z2node >= bz2) then
      flags%parallel_arrays_ok = .false.
      cycle
    end if

!                  Weights for the interpolant

    w1 = x_in1 * y_in1 * z2_in1
    w2 = x_in2 * y_in1 * z2_in1
    w3 = x_in1 * y_in2 * z2_in1
    w4 = x_in2 * y_in2 * z2_in1
    w5 = x_in1 * y_in1 * z2_in2
    w6 = x_in2 * y_in1 * z2_in2
    w7 = x_in1 * y_in2 * z2_in2
    w8 = x_in2 * y_in2 * z2_in2

!                  Node indices. The primary node is found with int() rather
!                  than floor(), as it always has been - the two agree for
!                  every macroparticle that passes the checks above.

    p = (int( (sx(i)+halfx)  / dx, kind=ip)  + 1_IP) + &
        (int( (sy(i)+halfy)  / dy, kind=ip) * nspinDX )  + &
        (nspinDX * nspinDY * int(sz2(i)  / dz2, kind=ip) ) - &
        (fz2-1)*ntrndsi
    p2 = p + 1_ip
    p3 = p + nspinDX
    p4 = p + nspinDX + 1_ip
    p5 = p + ntrndsi
    p6 = p + ntrndsi + 1_ip
    p7 = p + ntrndsi + nspinDX
    p8 = p + ntrndsi + nspinDX + 1_ip

!                  Field at the macroparticle

    fr = 0.0_wp
    fr = w1 * sAr(p)  + fr
    fr = w2 * sAr(p2) + fr
    fr = w3 * sAr(p3) + fr
    fr = w4 * sAr(p4) + fr
    fr = w5 * sAr(p5) + fr
    fr = w6 * sAr(p6) + fr
    fr = w7 * sAr(p7) + fr
    fr = w8 * sAr(p8) + fr
    sFr(i) = fr

    fi = 0.0_wp
    fi = w1 * sAi(p)  + fi
    fi = w2 * sAi(p2) + fi
    fi = w3 * sAi(p3) + fi
    fi = w4 * sAi(p4) + fi
    fi = w5 * sAi(p5) + fi
    fi = w6 * sAi(p6) + fi
    fi = w7 * sAi(p7) + fi
    fi = w8 * sAi(p8) + fi
    sFi(i) = fi

!                  'Instantaneous' dA/dz from this macroparticle

    dadzRInst = ((s_chi_bar(i)/dV3) * (1 + seta * sp2(i) ) &
                      * spr(i) / sgam(i) )

    dadzIInst = ((s_chi_bar(i)/dV3) * (1 + seta * sp2(i) ) &
                      * spi(i) / sgam(i) )

    if (qAtomic) then

      !$OMP ATOMIC
      sDADzr(p)  = w1 * dadzRInst + sDADzr(p)
      !$OMP ATOMIC
      sDADzr(p2) = w2 * dadzRInst + sDADzr(p2)
      !$OMP ATOMIC
      sDADzr(p3) = w3 * dadzRInst + sDADzr(p3)
      !$OMP ATOMIC
      sDADzr(p4) = w4 * dadzRInst + sDADzr(p4)
      !$OMP ATOMIC
      sDADzr(p5) = w5 * dadzRInst + sDADzr(p5)
      !$OMP ATOMIC
      sDADzr(p6) = w6 * dadzRInst + sDADzr(p6)
      !$OMP ATOMIC
      sDADzr(p7) = w7 * dadzRInst + sDADzr(p7)
      !$OMP ATOMIC
      sDADzr(p8) = w8 * dadzRInst + sDADzr(p8)

      !$OMP ATOMIC
      sDADzi(p)  = w1 * dadzIInst + sDADzi(p)
      !$OMP ATOMIC
      sDADzi(p2) = w2 * dadzIInst + sDADzi(p2)
      !$OMP ATOMIC
      sDADzi(p3) = w3 * dadzIInst + sDADzi(p3)
      !$OMP ATOMIC
      sDADzi(p4) = w4 * dadzIInst + sDADzi(p4)
      !$OMP ATOMIC
      sDADzi(p5) = w5 * dadzIInst + sDADzi(p5)
      !$OMP ATOMIC
      sDADzi(p6) = w6 * dadzIInst + sDADzi(p6)
      !$OMP ATOMIC
      sDADzi(p7) = w7 * dadzIInst + sDADzi(p7)
      !$OMP ATOMIC
      sDADzi(p8) = w8 * dadzIInst + sDADzi(p8)

    else

      sDADzr(p)  = w1 * dadzRInst + sDADzr(p)
      sDADzr(p2) = w2 * dadzRInst + sDADzr(p2)
      sDADzr(p3) = w3 * dadzRInst + sDADzr(p3)
      sDADzr(p4) = w4 * dadzRInst + sDADzr(p4)
      sDADzr(p5) = w5 * dadzRInst + sDADzr(p5)
      sDADzr(p6) = w6 * dadzRInst + sDADzr(p6)
      sDADzr(p7) = w7 * dadzRInst + sDADzr(p7)
      sDADzr(p8) = w8 * dadzRInst + sDADzr(p8)

      sDADzi(p)  = w1 * dadzIInst + sDADzi(p)
      sDADzi(p2) = w2 * dadzIInst + sDADzi(p2)
      sDADzi(p3) = w3 * dadzIInst + sDADzi(p3)
      sDADzi(p4) = w4 * dadzIInst + sDADzi(p4)
      sDADzi(p5) = w5 * dadzIInst + sDADzi(p5)
      sDADzi(p6) = w6 * dadzIInst + sDADzi(p6)
      sDADzi(p7) = w7 * dadzIInst + sDADzi(p7)
      sDADzi(p8) = w8 * dadzIInst + sDADzi(p8)

    end if

  end do
!$OMP END DO

end subroutine getInteract_3D_kernel

end module FiElec
