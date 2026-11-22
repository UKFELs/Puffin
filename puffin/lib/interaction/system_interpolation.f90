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
!> field nodes is done in one sweep over the macroparticles: locate the
!> nodes, form the 8 weights, gather the field at the macroparticle and
!> scatter its source term back onto the same nodes. The weights used to be
!> written to an (8, nElectrons) array and read back twice - at 400k
!> macroparticles that was 25 MB streamed three times per RHS evaluation,
!> plus the allocation of it and of a node index array on every call. They
!> now go through a buffer of one block of macroparticles, small enough to
!> stay in L1 (see getInteract_3D_kernel).


module FiElec

use puffin_kinds, only: WP, IPL, IP
use globals, only: NZ2_G, ntrndsi_G, nspinDX, nspinDY, fieldMesh, iTemporal, s_chi_bar_G, &
  procelectrons_G
use GlobalTypes, only: tSimulationFlags, tFieldValues

implicit none (type, external)
private

public :: getinteract_3d

!                  Macroparticles per block in getInteract_3D_kernel. The
!                  pass 1 buffers come to NBLK * 14 * 8 bytes = 14 KB at 128,
!                  which leaves most of a 32 KB L1 for the field lines the
!                  gather and scatter touch.

integer, parameter :: NBLK = 128
integer, parameter :: I4 = selected_int_kind(9)


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
!> The macroparticles are taken in blocks of NBLK, each in two passes:
!>
!>  1. Per-macroparticle arithmetic: the cell each one is in, its 8 weights,
!>     its node index and its source term, into small buffers that stay in
!>     L1. This pass has no branches and no indirect addressing, so it
!>     vectorises - which matters because it holds the ~10 floating point
!>     divisions per macroparticle, and a packed divide does two (SSE2) or
!>     four (AVX) for about the cost of one scalar divide.
!>  2. The range checks, then the gather of the field to each macroparticle
!>     and the scatter of its source term back to the mesh, in macroparticle
!>     order. This is the part with branches and indirect addressing, and it
!>     stays scalar (gfortran will not if-convert the checks, and with them
!>     in pass 1 it does not vectorise at all).
!>
!> Every value is formed by the same operations, in the same order, as the
!> one-pass loop it replaces, and the contributions to each node are added in
!> the same macroparticle order, so the result is bit-for-bit unchanged. Two
!> details keep pass 1 vectorisable without changing anything:
!>
!>  - Converting a double to a 64-bit integer has no packed instruction
!>    before AVX-512, so floor(v) is formed as a double instead: truncate
!>    through a 32-bit integer, and subtract one where that rounded up. For
!>    |v| < 2**31 it is exactly the old real(floor(v)), which is all the
!>    weights ever used the integer for.
!>  - The range checks are made on the quotient v itself, kept from pass 1,
!>    not on a converted integer: floor(v) + 1 >= n is exactly v >= n - 1,
!>    and floor(v) + 1 < 1 is exactly v < 0. So a macroparticle far enough
!>    out to overflow the 32-bit conversion is still caught, and the
!>    converted value is never used for it. They are written so that a NaN
!>    coordinate fails them; the old integer checks let a NaN z2 through to
!>    index the field.
!>
!> The node index truncates z2, as the old int() did, while the weights use
!> floor - the two differ only for -1 < z2/dz2 < 0, and that is kept as it
!> was. For x and y it uses floor, which equals truncation for every
!> macroparticle that passes the checks.
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


real(kind=wp) :: bw(NBLK, 8), bv(NBLK, 3), bdr(NBLK), bdi(NBLK), bp(NBLK)

integer(kind=ipl) :: i0, nBlocks, ib
integer :: j, nb
integer(kind=ip) :: p, p2, p3, p4, p5, p6, p7, p8
real(kind=wp) :: vx, vy, vz2
real(kind=wp) :: xHi, yHi, z2HiTemporal, z2HiLocal, rnx, rnxy, rOff
real(kind=wp) :: w1, w2, w3, w4, w5, w6, w7, w8
real(kind=wp) :: fr, fi, dadzRInst, dadzIInst
logical :: qAtomic

  qAtomic = .false.
!$ qAtomic = (omp_get_num_threads() > 1)

!                  Limits of the old integer checks, as limits on the quotient

  xHi = real(nspinDX - 1_ip, kind=wp)
  yHi = real(nspinDY - 1_ip, kind=wp)
  z2HiTemporal = real(nz2 - 1_ip, kind=wp)
  z2HiLocal = real(bz2 - 1_ip, kind=wp)

!                  Node index terms. All are integers well below 2**53, so
!                  the index sums exactly in double precision.

  rnx = real(nspinDX, kind=wp)
  rnxy = real(nspinDX * nspinDY, kind=wp)
  rOff = real((fz2-1)*ntrndsi, kind=wp)

  nBlocks = (nLocalElecs + NBLK - 1) / NBLK

!$OMP DO PRIVATE(i0, nb, j, p, p2, p3, p4, p5, p6, p7, p8, &
!$OMP vx, vy, vz2, w1, w2, w3, w4, w5, w6, w7, w8, fr, fi, dadzRInst, dadzIInst, &
!$OMP bw, bv, bdr, bdi, bp)
  do ib = 0, nBlocks - 1

    i0 = ib * NBLK
    nb = int(min(int(NBLK, kind=ipl), nLocalElecs - i0))

!                  Pass 1: cells, weights, node index and source term

    call blockArith(nb, sx(i0+1:i0+nb), sy(i0+1:i0+nb), sz2(i0+1:i0+nb), &
                    spr(i0+1:i0+nb), spi(i0+1:i0+nb), sgam(i0+1:i0+nb), &
                    s_chi_bar(i0+1:i0+nb), sp2(i0+1:i0+nb), seta, &
                    halfx, halfy, dx, dy, dz2, dV3, rnx, rnxy, rOff, &
                    bw, bv, bp, bdr, bdi)

!                  Pass 2: checks, gather and scatter, in macroparticle
!                  order. The checks are the old ones, in the old order.

    do j = 1, nb

      vx = bv(j,1)
      vy = bv(j,2)
      vz2 = bv(j,3)

      if (qTemporal) then
        if (vz2 >= z2HiTemporal) then
          print*, "Z2 coord is too large!! with node:", &
                  floor(vz2) + 1_IP, " and pos ", sz2(i0+j)
          STOP
        end if
      end if

      if (.not. ((vx >= 0.0_wp) .and. (vx < xHi) .and. &
                 (vy >= 0.0_wp) .and. (vy < yHi))) then
        flags%inner_xy_ok = .false.
        flags%parallel_arrays_ok = .false.
        cycle
      end if

      if (.not. (vz2 < z2HiLocal)) then
        flags%parallel_arrays_ok = .false.
        cycle
      end if

      p = int(bp(j), kind=ip)
      p2 = p + 1_ip
      p3 = p + nspinDX
      p4 = p + nspinDX + 1_ip
      p5 = p + ntrndsi
      p6 = p + ntrndsi + 1_ip
      p7 = p + ntrndsi + nspinDX
      p8 = p + ntrndsi + nspinDX + 1_ip

      w1 = bw(j,1)
      w2 = bw(j,2)
      w3 = bw(j,3)
      w4 = bw(j,4)
      w5 = bw(j,5)
      w6 = bw(j,6)
      w7 = bw(j,7)
      w8 = bw(j,8)

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
      sFr(i0+j) = fr

      fi = 0.0_wp
      fi = w1 * sAi(p)  + fi
      fi = w2 * sAi(p2) + fi
      fi = w3 * sAi(p3) + fi
      fi = w4 * sAi(p4) + fi
      fi = w5 * sAi(p5) + fi
      fi = w6 * sAi(p6) + fi
      fi = w7 * sAi(p7) + fi
      fi = w8 * sAi(p8) + fi
      sFi(i0+j) = fi

!                  Source term back to the mesh

      dadzRInst = bdr(j)
      dadzIInst = bdi(j)

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

  end do
!$OMP END DO

end subroutine getInteract_3D_kernel


!> Pass 1 of getInteract_3D_kernel for one block of nb macroparticles: see
!> there. It is a procedure of its own, and kept out of line, because
!> gfortran does not vectorise the loop once it sits inside the kernel's
!> OpenMP-outlined body; with plain dummy arguments it does.

subroutine blockArith(nb, sx, sy, sz2, spr, spi, sgam, s_chi_bar, sp2, seta, &
                      halfx, halfy, dx, dy, dz2, dV3, rnx, rnxy, rOff, &
                      bw, bv, bp, bdr, bdi)

!GCC$ ATTRIBUTES noinline :: blockArith

integer, intent(in) :: nb
real(kind=wp), contiguous, intent(in) :: sx(:), sy(:), sz2(:)
real(kind=wp), contiguous, intent(in) :: spr(:), spi(:), sgam(:)
real(kind=wp), contiguous, intent(in) :: s_chi_bar(:), sp2(:)
real(kind=wp), intent(in) :: seta, halfx, halfy, dx, dy, dz2, dV3
real(kind=wp), intent(in) :: rnx, rnxy, rOff
real(kind=wp), intent(out) :: bw(NBLK, 8), bv(NBLK, 3), bp(NBLK)
real(kind=wp), intent(out) :: bdr(NBLK), bdi(NBLK)

integer :: j
real(kind=wp) :: vx, vy, vz2, fx, fy, fz2r, tz2, locx, locy, locz2
real(kind=wp) :: x_in1, x_in2, y_in1, y_in2, z2_in1, z2_in2

  do j = 1, nb

    vx = (sx(j) + halfx ) / dx
    fx = real(int(vx, kind=I4), kind=wp)
    fx = fx - merge(1.0_wp, 0.0_wp, fx > vx)
    locx = sx(j) + halfx - fx * dx
    x_in2 = locx / dx
    x_in1 = (1.0_wp - x_in2)

    vy = (sy(j) + halfy )  / dy
    fy = real(int(vy, kind=I4), kind=wp)
    fy = fy - merge(1.0_wp, 0.0_wp, fy > vy)
    locy = sy(j) + halfy - fy * dy
    y_in2 = locy / dy
    y_in1 = (1.0_wp - y_in2)

    vz2 = sz2(j)  / dz2
    tz2 = real(int(vz2, kind=I4), kind=wp)
    fz2r = tz2 - merge(1.0_wp, 0.0_wp, tz2 > vz2)
    locz2 = sz2(j) - fz2r * dz2
    z2_in2 = locz2 / dz2
    z2_in1 = (1.0_wp - z2_in2)

    bv(j,1) = vx
    bv(j,2) = vy
    bv(j,3) = vz2

    bw(j,1) = x_in1 * y_in1 * z2_in1
    bw(j,2) = x_in2 * y_in1 * z2_in1
    bw(j,3) = x_in1 * y_in2 * z2_in1
    bw(j,4) = x_in2 * y_in2 * z2_in1
    bw(j,5) = x_in1 * y_in1 * z2_in2
    bw(j,6) = x_in2 * y_in1 * z2_in2
    bw(j,7) = x_in1 * y_in2 * z2_in2
    bw(j,8) = x_in2 * y_in2 * z2_in2

!                  The z2 term truncates, as the old int() did; x and y
!                  truncate and floor alike once past the checks.

    bp(j) = (fx + 1.0_wp) + (fy * rnx) + (rnxy * tz2) - rOff

    bdr(j) = ((s_chi_bar(j)/dV3) * (1 + seta * sp2(j) ) &
                   * spr(j) / sgam(j) )

    bdi(j) = ((s_chi_bar(j)/dV3) * (1 + seta * sp2(j) ) &
                   * spi(j) / sgam(j) )

  end do

end subroutine blockArith

end module FiElec
