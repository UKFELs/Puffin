! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Module containing routines dealing with the interpolation of the
!> macroparticles to the 1D field mesh.

module FiElec1D

use puffin_kinds, only: WP, IPL, IP
use globals, only: NZ2_G, fieldMesh, iTemporal, s_chi_bar_G, procelectrons_G, dadz_w
use GlobalTypes, only: tSimulationFlags, tFieldValues
use shape_functions, only: getShapeWeights, shapeStencilWidth, iShapeMaxWidth

implicit none (type, external)
private

public :: getffelecs_1d, getinterps_1d, getsource_1d, &
          getffelecs_1d_bspline, getinterps_1d_bspline, getsource_1d_bspline


contains


subroutine getInterps_1D(sz2, flags, field)

use rhs_vars, only: lis_GR, dz2, WP, IPL, IP

real(kind=wp), intent(in) :: sz2(:)
type(tSimulationFlags), intent(inout) :: flags
type(tFieldValues), intent(in) :: field

integer(kind=ip) :: z2node
integer(kind=ipl) :: i

real(kind=wp) :: locz2


!$OMP DO PRIVATE(z2node, locz2)
  do i = 1, procelectrons_G(1)

!                  Get surrounding nodes

      z2node = floor(sz2(i)  / dz2)  + 1_IP
      locz2 = sz2(i) - REAL(z2node  - 1_IP, kind=wp) * dz2

      if (fieldMesh == itemporal) then
        if (z2node >= NZ2_G) then
          print*, "Z2 coord is too large!! with node:", z2node, &
                  " and pos ", sz2(i)
          STOP
        end if
      end if


      if (z2node >= field%bz2) then
        flags%parallel_arrays_ok = .false.
      end if


      lis_GR(1,i) = (1.0_wp - locz2/dz2)
      lis_GR(2,i) = 1 - lis_GR(1,i)

  end do
!$OMP END DO


end subroutine getInterps_1D






!      ##################################################




subroutine getFFelecs_1D(sAr, sAi)


use rhs_vars, only: p_nodes, lis_GR, sField4ElecReal, sField4ElecImag, WP, IP

real(kind=wp), intent(in) :: sAr(:), sAi(:)
integer(kind=ip) :: i


!$OMP DO
  do i = 1, procelectrons_G(1)

      sField4ElecReal(i) = lis_GR(1,i) * sAr(p_nodes(i)) + sField4ElecReal(i)
      sField4ElecReal(i) = lis_GR(2,i) * sAr(p_nodes(i) + 1_ip) + sField4ElecReal(i)

  end do
!$OMP END DO

!$OMP DO
  do i = 1, procelectrons_G(1)


      sField4ElecImag(i) = lis_GR(1,i) * sAi(p_nodes(i)) + sField4ElecImag(i)
      sField4ElecImag(i) = lis_GR(2,i) * sAi(p_nodes(i) + 1_ip) + sField4ElecImag(i)

  end do
!$OMP END DO



end subroutine getFFelecs_1D



!         ########################################################



!> Scatter the electron source term into the local field arrays.
!>
!> Split into a wrapper and getSource_1D_kernel below for the same reason as
!> getSource_3D in system_interpolation.f90 - see the comment there.  The
!> !$OMP ATOMIC updates are memory barriers, so a loop reading dadz_w, p_nodes
!> and lis_GR straight from their modules has to reload each array descriptor
!> from memory every iteration, which puts the linker's choice of module symbol
!> addresses on the critical path.  Passed as dummy arguments, the base
!> addresses are loop-invariant by construction.

subroutine getSource_1D(sDADzr, sDADzi, spr, spi, sgam, seta)


use rhs_vars, only: p_nodes, lis_GR, dV3, sp2, WP, IPL

real(kind=wp), contiguous, intent(inout) :: sDADzr(:), sDADzi(:)
real(kind=wp), contiguous, intent(in) :: spr(:), spi(:)
real(kind=wp), contiguous, intent(in) :: sgam(:)
real(kind=wp), intent(in) :: seta


!$OMP WORKSHARE
dadz_w = (s_chi_bar_G/dV3) * (1 + seta * sp2 ) &
                        / sgam
!$OMP END WORKSHARE

  call getSource_1D_kernel(sDADzr, sDADzi, spr, spi, &
                           dadz_w, p_nodes, lis_GR, procelectrons_G(1))

end subroutine getSource_1D



subroutine getSource_1D_kernel(sDADzr, sDADzi, spr, spi, &
                               dadz_w, p_nodes, lis_GR, nLocalElecs)

real(kind=wp), contiguous, intent(inout) :: sDADzr(:), sDADzi(:)
real(kind=wp), contiguous, intent(in) :: spr(:), spi(:)
real(kind=wp), contiguous, intent(in) :: dadz_w(:)
integer(kind=ip), contiguous, intent(in) :: p_nodes(:)
real(kind=wp), contiguous, intent(in) :: lis_GR(:,:)
integer(kind=ipl), intent(in) :: nLocalElecs

integer(kind=ipl) :: i
real(kind=wp) :: dadzRInst, dadzIInst

!$OMP DO PRIVATE(dadzRInst, dadzIInst)
  do i = 1, nLocalElecs

!                  Get 'instantaneous' dAdz

      !dadzRInst = ((s_chi_bar_G(i)/dV3) * (1 + seta * sp2(i) ) &
      !                  * spr(i) / sgam(i) )

      dadzRInst = dadz_w(i) * spr(i)

      !$OMP ATOMIC
      sDADzr(p_nodes(i)) =                         &
        lis_GR(1,i) * dadzRInst + sDADzr(p_nodes(i))

      !$OMP ATOMIC
      sDADzr(p_nodes(i) + 1_ip) =                  &
        lis_GR(2,i) * dadzRInst + sDADzr(p_nodes(i) + 1_ip)


!                   Imaginary part

      !dadzIInst = ((s_chi_bar_G(i)/dV3) * (1 + seta * sp2(i) ) &
      !                  * spi(i) / sgam(i) )


      dadzIInst = dadz_w(i) * spi(i)


      !$OMP ATOMIC
      sDADzi(p_nodes(i)) =                             &
        lis_GR(1,i) * dadzIInst + sDADzi(p_nodes(i))

      !$OMP ATOMIC
      sDADzi(p_nodes(i) + 1_ip) =                      &
        lis_GR(2,i) * dadzIInst + sDADzi(p_nodes(i) + 1_ip)

  end do
!$OMP END DO


end subroutine getSource_1D_kernel



!         ########################################################
!
!     Higher-order (B-spline) deposition.
!
!     Same three steps as above - build the weights, gather the field, scatter
!     the source - but over a stencil of iShapeOrder+1 nodes instead of 2, and
!     with the weights taken from shape_functions.  These are separate routines
!     rather than a generalisation of the linear ones so that the default path
!     keeps its hard-coded two-node arithmetic, and stays bit-for-bit identical.
!
!         ########################################################



!> Build the B-spline weights, and the node each particle's stencil starts on.
!>
!> Unlike the linear path, p_nodes is filled here rather than in getrhs: where
!> the stencil starts depends on the degree (the quadratic centres on the
!> nearest node, the cubic starts one node behind the particle), so it falls
!> out of the same evaluation as the weights.

subroutine getInterps_1D_bspline(sz2, flags, field)

use rhs_vars, only: p_nodes, lis_GR, dz2, WP, IPL, IP

real(kind=wp), intent(in) :: sz2(:)
type(tSimulationFlags), intent(inout) :: flags
type(tFieldValues), intent(in) :: field

integer(kind=ip) :: n0, nwid, iOrder, k
integer(kind=ipl) :: i

real(kind=wp) :: w(iShapeMaxWidth)

  iOrder = flags%shape_order
  nwid = shapeStencilWidth(iOrder)

!$OMP DO PRIVATE(n0, w, k)
  do i = 1, procelectrons_G(1)

    call getShapeWeights(iOrder, sz2(i) / dz2, n0, w)

!   Front of the mesh: the node before it does not exist, so fold its weight
!   onto the first node.  That keeps the deposited charge exact and makes the
!   gather see the field held constant off the end.  At most one node is ever
!   folded, since no supported degree reaches further than one cell to the left.

    if (n0 < 1_ip) then

      w(2) = w(2) + w(1)

      do k = 1_ip, nwid - 1_ip
        w(k) = w(k+1_ip)
      end do

      w(nwid) = 0.0_wp
      n0 = n0 + 1_ip

    end if

    if (fieldMesh == itemporal) then
      if (n0 + nwid - 1_ip > NZ2_G) then
        print*, "Z2 coord is too large!! with node:", n0 + nwid - 1_ip, &
                " and pos ", sz2(i)
        STOP
      end if
    end if

!   Past the end of the halo this rank holds - or, defensively, in front of its
!   first node, which the shifted ownership in rearrElecs should already rule
!   out.  Either way the parallel layout has to be rebuilt before this step.

    if ((n0 + nwid - 1_ip > field%bz2) .or. (n0 < field%fz2)) then
      flags%parallel_arrays_ok = .false.
    end if

    p_nodes(i) = n0 - (field%fz2 - 1_ip)
    lis_GR(1_ip:nwid, i) = w(1_ip:nwid)

  end do
!$OMP END DO


end subroutine getInterps_1D_bspline



!      ##################################################



subroutine getFFelecs_1D_bspline(sAr, sAi, nwid)


use rhs_vars, only: p_nodes, lis_GR, sField4ElecReal, sField4ElecImag, WP, IP

real(kind=wp), contiguous, intent(in) :: sAr(:), sAi(:)
integer(kind=ip), intent(in) :: nwid

integer(kind=ipl) :: i
integer(kind=ip) :: k

!$OMP DO PRIVATE(k)
  do i = 1, procelectrons_G(1)

    do k = 1_ip, nwid

      sField4ElecReal(i) = lis_GR(k,i) * sAr(p_nodes(i) + k - 1_ip) &
                             + sField4ElecReal(i)

      sField4ElecImag(i) = lis_GR(k,i) * sAi(p_nodes(i) + k - 1_ip) &
                             + sField4ElecImag(i)

    end do

  end do
!$OMP END DO


end subroutine getFFelecs_1D_bspline



!         ########################################################



!> Scatter the source over the B-spline stencil.
!>
!> Split into wrapper and kernel for the same reason as the linear version -
!> see the comment on getSource_1D.

subroutine getSource_1D_bspline(sDADzr, sDADzi, spr, spi, sgam, seta, nwid)


use rhs_vars, only: p_nodes, lis_GR, dV3, sp2, WP, IPL

real(kind=wp), contiguous, intent(inout) :: sDADzr(:), sDADzi(:)
real(kind=wp), contiguous, intent(in) :: spr(:), spi(:)
real(kind=wp), contiguous, intent(in) :: sgam(:)
real(kind=wp), intent(in) :: seta
integer(kind=ip), intent(in) :: nwid


!$OMP WORKSHARE
dadz_w = (s_chi_bar_G/dV3) * (1 + seta * sp2 ) &
                        / sgam
!$OMP END WORKSHARE

  call getSource_1D_bspline_kernel(sDADzr, sDADzi, spr, spi, &
                                   dadz_w, p_nodes, lis_GR, nwid, &
                                   procelectrons_G(1))

end subroutine getSource_1D_bspline



subroutine getSource_1D_bspline_kernel(sDADzr, sDADzi, spr, spi, &
                                       dadz_w, p_nodes, lis_GR, nwid, &
                                       nLocalElecs)

real(kind=wp), contiguous, intent(inout) :: sDADzr(:), sDADzi(:)
real(kind=wp), contiguous, intent(in) :: spr(:), spi(:)
real(kind=wp), contiguous, intent(in) :: dadz_w(:)
integer(kind=ip), contiguous, intent(in) :: p_nodes(:)
real(kind=wp), contiguous, intent(in) :: lis_GR(:,:)
integer(kind=ip), intent(in) :: nwid
integer(kind=ipl), intent(in) :: nLocalElecs

integer(kind=ipl) :: i
integer(kind=ip) :: k, inode
real(kind=wp) :: dadzRInst, dadzIInst

!$OMP DO PRIVATE(dadzRInst, dadzIInst, k, inode)
  do i = 1, nLocalElecs

!                  Get 'instantaneous' dAdz

    dadzRInst = dadz_w(i) * spr(i)
    dadzIInst = dadz_w(i) * spi(i)

    do k = 1_ip, nwid

      inode = p_nodes(i) + k - 1_ip

      !$OMP ATOMIC
      sDADzr(inode) = lis_GR(k,i) * dadzRInst + sDADzr(inode)

      !$OMP ATOMIC
      sDADzi(inode) = lis_GR(k,i) * dadzIInst + sDADzi(inode)

    end do

  end do
!$OMP END DO


end subroutine getSource_1D_bspline_kernel

end module FiElec1D
