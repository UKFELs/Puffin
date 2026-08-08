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


module FiElec

use puffin_kinds
use globals
use parafield
use GlobalTypes, only: tSimulationFlags

implicit none

contains


subroutine getInterps_3D(sx, sy, sz2, flags)

use rhs_vars

real(kind=wp), intent(in) :: sx(:), sy(:), sz2(:)
type(tSimulationFlags), intent(inout) :: flags

integer(kind=ip) :: xnode, ynode, z2node
integer(kind=ipl) :: i

real(kind=wp) :: locx, locy, locz2, &
                 x_in1, x_in2, y_in1, y_in2, z2_in1, z2_in2

!$OMP DO PRIVATE(xnode, ynode, z2node, locx, locy, locz2, &
!$OMP x_in1, x_in2, y_in1, y_in2, z2_in1, z2_in2)
  do i = 1, maxEl
    if (i<=procelectrons_G(1)) then 


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

      if ((xnode >= nspinDX) .or. (xnode < 1)) then
        flags%inner_xy_ok = .false.
        flags%parallel_arrays_ok = .false.
      end if

      if ((ynode >= nspinDY) .or. (ynode < 1)) then
        flags%inner_xy_ok = .false.
        flags%parallel_arrays_ok = .false.
      end if

      if (fieldMesh == itemporal) then
        if (z2node >= NZ2_G) then
          print*, 'Z2 coord is too large!! with node:', z2node, &
                  ' and pos ', sz2(i)
          STOP
        end if
      end if

      if (z2node >= bz2) then
        flags%parallel_arrays_ok = .false.
      end if

!                  Get weights for interpolant

      lis_GR(1,i) = x_in1 * y_in1 * z2_in1
      lis_GR(2,i) = x_in2 * y_in1 * z2_in1
      lis_GR(3,i) = x_in1 * y_in2 * z2_in1
      lis_GR(4,i) = x_in2 * y_in2 * z2_in1
      lis_GR(5,i) = x_in1 * y_in1 * z2_in2
      lis_GR(6,i) = x_in2 * y_in1 * z2_in2
      lis_GR(7,i) = x_in1 * y_in2 * z2_in2
      lis_GR(8,i) = x_in2 * y_in2 * z2_in2

    end if
  end do
!$OMP END DO


end subroutine getInterps_3D






!      ##################################################




subroutine getFFelecs_3D(sAr, sAi)


use rhs_vars

real(kind=wp), contiguous, intent(in) :: sAr(:), sAi(:)
integer(kind=ip) :: i

!$OMP DO
  do i = 1, procelectrons_G(1)

      sField4ElecReal(i) = lis_GR(1,i) * sAr(p_nodes(i)) + sField4ElecReal(i)
      sField4ElecReal(i) = lis_GR(2,i) * sAr(p_nodes(i) + 1_ip) + sField4ElecReal(i)
      sField4ElecReal(i) = lis_GR(3,i) * sAr(p_nodes(i) + nspinDX) + sField4ElecReal(i)
      sField4ElecReal(i) = lis_GR(4,i) * sAr(p_nodes(i) + nspinDX + 1_ip) + sField4ElecReal(i)
      sField4ElecReal(i) = lis_GR(5,i) * sAr(p_nodes(i) + ntrndsi_G) + sField4ElecReal(i)
      sField4ElecReal(i) = lis_GR(6,i) * sAr(p_nodes(i) + ntrndsi_G + 1_ip) + sField4ElecReal(i)
      sField4ElecReal(i) = lis_GR(7,i) * sAr(p_nodes(i) + ntrndsi_G + nspinDX) + sField4ElecReal(i)
      sField4ElecReal(i) = lis_GR(8,i) * sAr(p_nodes(i) + ntrndsi_G + nspinDX + 1) + sField4ElecReal(i)
  
      sField4ElecImag(i) = lis_GR(1,i) * sAi(p_nodes(i)) + sField4ElecImag(i)
      sField4ElecImag(i) = lis_GR(2,i) * sAi(p_nodes(i)  + 1_ip) + sField4ElecImag(i)
      sField4ElecImag(i) = lis_GR(3,i) * sAi(p_nodes(i)  + nspinDX) + sField4ElecImag(i)
      sField4ElecImag(i) = lis_GR(4,i) * sAi(p_nodes(i)  + nspinDX + 1_ip) + sField4ElecImag(i)
      sField4ElecImag(i) = lis_GR(5,i) * sAi(p_nodes(i)  + ntrndsi_G) + sField4ElecImag(i)
      sField4ElecImag(i) = lis_GR(6,i) * sAi(p_nodes(i)  + ntrndsi_G + 1_ip) + sField4ElecImag(i)
      sField4ElecImag(i) = lis_GR(7,i) * sAi(p_nodes(i)  + ntrndsi_G + nspinDX) + sField4ElecImag(i)
      sField4ElecImag(i) = lis_GR(8,i) * sAi(p_nodes(i)  + ntrndsi_G + nspinDX + 1) + sField4ElecImag(i)
  
  end do 
!$OMP END DO

end subroutine getFFelecs_3D



!         ########################################################



!> Scatter the electron source term into the local field arrays.
!>
!> The scatter loop itself lives in getSource_3D_kernel below, and the module
!> data it needs is handed over as dummy arguments.  That is deliberate: the
!> !$OMP ATOMIC updates act as memory barriers, so if the loop reads
!> s_chi_bar_G, sp2, p_nodes, lis_GR and dV3 straight from their modules the
!> compiler must reload each array descriptor from memory on every iteration.
!> Those reloads then sit on the critical path, and the speed of this routine
!> becomes hostage to wherever the linker happened to place the module symbols
!> - an unrelated commit that repacks a module can cost 2x here.  See
!> benchmark/RESULTS-2026-08-07-globals-refactor.md.  Passed as dummy
!> arguments, the base addresses are loop-invariant by construction.

subroutine getSource_3D(sDADzr, sDADzi, spr, spi, sgam, seta)


use rhs_vars

real(kind=wp), contiguous, intent(inout) :: sDADzr(:), sDADzi(:)
real(kind=wp), contiguous, intent(in) :: spr(:), spi(:)
real(kind=wp), contiguous, intent(in) :: sgam(:)
real(kind=wp), intent(in) :: seta

  call getSource_3D_kernel(sDADzr, sDADzi, spr, spi, sgam, seta, &
                           s_chi_bar_G, sp2, p_nodes, lis_GR, dV3, &
                           nspinDX, ntrndsi_G, maxEl, procelectrons_G(1))

end subroutine getSource_3D



subroutine getSource_3D_kernel(sDADzr, sDADzi, spr, spi, sgam, seta, &
                               s_chi_bar_G, sp2, p_nodes, lis_GR, dV3, &
                               nspinDX, ntrndsi_G, maxEl, nLocalElecs)

real(kind=wp), contiguous, intent(inout) :: sDADzr(:), sDADzi(:)
real(kind=wp), contiguous, intent(in) :: spr(:), spi(:)
real(kind=wp), contiguous, intent(in) :: sgam(:)
real(kind=wp), intent(in) :: seta
real(kind=wp), contiguous, intent(in) :: s_chi_bar_G(:), sp2(:)
integer(kind=ip), contiguous, intent(in) :: p_nodes(:)
real(kind=wp), contiguous, intent(in) :: lis_GR(:,:)
real(kind=wp), intent(in) :: dV3
integer(kind=ip), intent(in) :: nspinDX, ntrndsi_G
integer(kind=ipl), intent(in) :: maxEl, nLocalElecs

integer(kind=ipl) :: i
real(kind=wp) :: dadzRInst, dadzIInst

!$OMP DO PRIVATE(dadzRInst, dadzIInst)
  do i = 1, maxEl
  
    if (i<=nLocalElecs) then


!                  Get 'instantaneous' dAdz

      dadzRInst = ((s_chi_bar_G(i)/dV3) * (1 + seta * sp2(i) ) &
                        * spr(i) / sgam(i) )
    
      !$OMP ATOMIC
      sDADzr(p_nodes(i)) =                         &
        lis_GR(1,i) * dadzRInst + sDADzr(p_nodes(i))
      
      !$OMP ATOMIC
      sDADzr(p_nodes(i) + 1_ip) =                  &
        lis_GR(2,i) * dadzRInst + sDADzr(p_nodes(i) + 1_ip)                

      !$OMP ATOMIC
      sDADzr(p_nodes(i) + nspinDX) =           &
        lis_GR(3,i) * dadzRInst + sDADzr(p_nodes(i) + nspinDX)          

      !$OMP ATOMIC
      sDADzr(p_nodes(i) + nspinDX + 1_ip) =    &
        lis_GR(4,i) * dadzRInst + sDADzr(p_nodes(i) + nspinDX + 1_ip)   

      !$OMP ATOMIC
      sDADzr(p_nodes(i) + ntrndsi_G) =                &
        lis_GR(5,i) * dadzRInst + sDADzr(p_nodes(i) + ntrndsi_G)               

      !$OMP ATOMIC
      sDADzr(p_nodes(i) + ntrndsi_G + 1_ip) =         &
        lis_GR(6,i) * dadzRInst + sDADzr(p_nodes(i) + ntrndsi_G + 1_ip)         

      !$OMP ATOMIC
      sDADzr(p_nodes(i) + ntrndsi_G + nspinDX) =  &
        lis_GR(7,i) * dadzRInst + sDADzr(p_nodes(i) + ntrndsi_G + nspinDX)   

      !$OMP ATOMIC
      sDADzr(p_nodes(i) + ntrndsi_G + nspinDX + 1) = &
        lis_GR(8,i) * dadzRInst + sDADzr(p_nodes(i) + ntrndsi_G + nspinDX + 1)

!                   Imaginary part

      dadzIInst = ((s_chi_bar_G(i)/dV3) * (1 + seta * sp2(i) ) &
                        * spi(i) / sgam(i) ) 
    
      !$OMP ATOMIC
      sDADzi(p_nodes(i)) =                             & 
        lis_GR(1,i) * dadzIInst + sDADzi(p_nodes(i))                        

      !$OMP ATOMIC
      sDADzi(p_nodes(i) + 1_ip) =                      & 
        lis_GR(2,i) * dadzIInst + sDADzi(p_nodes(i) + 1_ip)           

      !$OMP ATOMIC
      sDADzi(p_nodes(i) + nspinDX) =               & 
        lis_GR(3,i) * dadzIInst + sDADzi(p_nodes(i) + nspinDX)           

      !$OMP ATOMIC
      sDADzi(p_nodes(i) + nspinDX + 1_ip) =        & 
        lis_GR(4,i) * dadzIInst + sDADzi(p_nodes(i) + nspinDX + 1_ip)    

      !$OMP ATOMIC
      sDADzi(p_nodes(i) + ntrndsi_G) =                    & 
        lis_GR(5,i) * dadzIInst + sDADzi(p_nodes(i) + ntrndsi_G)               

      !$OMP ATOMIC
      sDADzi(p_nodes(i) + ntrndsi_G + 1_ip) =             & 
        lis_GR(6,i) * dadzIInst + sDADzi(p_nodes(i) + ntrndsi_G + 1_ip)       

      !$OMP ATOMIC
      sDADzi(p_nodes(i) + ntrndsi_G + nspinDX) =      & 
        lis_GR(7,i) * dadzIInst + sDADzi(p_nodes(i) + ntrndsi_G + nspinDX)  

      !$OMP ATOMIC
      sDADzi(p_nodes(i) + ntrndsi_G + nspinDX + 1) =  & 
        lis_GR(8,i) * dadzIInst + sDADzi(p_nodes(i) + ntrndsi_G + nspinDX + 1)

    end if
  
  end do 
!$OMP END DO

end subroutine getSource_3D_kernel

end module FiElec
