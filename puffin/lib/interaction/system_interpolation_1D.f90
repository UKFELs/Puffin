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
use parafield, only: bz2
use GlobalTypes, only: tSimulationFlags

implicit none (type, external)

contains


subroutine getInterps_1D(sz2, flags)

use rhs_vars, only: lis_GR, dz2, WP, IPL, IP

real(kind=wp), intent(in) :: sz2(:)
type(tSimulationFlags), intent(inout) :: flags

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


      if (z2node >= bz2) then
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

end module FiElec1D
