! Copyright 2012-2017, University of Strathclyde
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

use paratype
use globals
use parafield

implicit none

contains


subroutine getInterps_1D(sz2)!, li1,li2)

use rhs_vars

real(kind=wp), intent(in) :: sz2(:)
!real(kind=wp), intent(out) :: li1, li2

integer(kind=ip) :: z2node
integer(kind=ipl) :: i

real(kind=wp) :: locz2


!$OMP DO PRIVATE(z2node, locz2)
  do i = 1, procelectrons_G(1)
  !do i = 1, maxEl
!    if (i<=procelectrons_G(1)) then 


!                  Get surrounding nodes 

      z2node = floor(sz2(i)  / dz2)  + 1_IP
      locz2 = sz2(i) - REAL(z2node  - 1_IP, kind=wp) * dz2
      
      if (fieldMesh == itemporal) then
        if (z2node >= NZ2_G) then
          print*, 'Z2 coord is too large!! with node:', z2node, &
                  ' and pos ', sz2(i)
          STOP
        end if
      end if


      if (z2node >= bz2) then
        qPArrOK_G = .false.
      end if


      lis_GR(1,i) = (1.0_wp - locz2/dz2)
      lis_GR(2,i) = 1 - lis_GR(1,i)

!    end if
  end do
!$OMP END DO


end subroutine getInterps_1D






!      ##################################################




subroutine getFFelecs_1D(sAr, sAi)


use rhs_vars

real(kind=wp), intent(in) :: sAr(:), sAi(:)
integer(kind=ip) :: i
integer error


!$OMP DO
  do i = 1, procelectrons_G(1)
!  do i = 1, maxEl
  
    !if (i<=procelectrons_G(1)) then

      sField4ElecReal(i) = lis_GR(1,i) * sAr(p_nodes(i)) + sField4ElecReal(i)
      sField4ElecReal(i) = lis_GR(2,i) * sAr(p_nodes(i) + 1_ip) + sField4ElecReal(i)
  
  end do
!$OMP END DO

!$OMP DO
  do i = 1, procelectrons_G(1)


      sField4ElecImag(i) = lis_GR(1,i) * sAi(p_nodes(i)) + sField4ElecImag(i)
      sField4ElecImag(i) = lis_GR(2,i) * sAi(p_nodes(i) + 1_ip) + sField4ElecImag(i)

!if (tProcInfo_G%rank == 0) print*, i, sField4ElecImag(i), sAi(p_nodes(i)), sAi(p_nodes(i)+1), &
!                                  p_nodes(i), lis_GR(1,i), lis_GR(2,i)

    !end if
  
  end do 
!$OMP END DO



end subroutine getFFelecs_1D



!         ########################################################



subroutine getSource_1D(sDADzr, sDADzi, spr, spi, sgam, seta)


use rhs_vars

real(kind=wp), intent(inout) :: sDADzr(:), sDADzi(:)
real(kind=wp), intent(in) :: spr(:), spi(:)
real(kind=wp), intent(in) :: sgam(:)
real(kind=wp), intent(in) :: seta

integer(kind=ipl) :: i
real(kind=wp) :: dadzRInst, dadzIInst, dadzCom

integer :: error




!$OMP WORKSHARE
dadz_w = (s_chi_bar_G/dV3) * (1 + seta * sp2 ) &
                        / sgam
!$OMP END WORKSHARE

!$OMP DO PRIVATE(dadzRInst, dadzIInst)
  do i = 1, procelectrons_G(1)

  ! do i = 1, maxEl
  
    !if (i<=procelectrons_G(1)) then


!                  Get 'instantaneous' dAdz

      

      !dadzRInst = ((s_chi_bar_G(i)/dV3) * (1 + seta * sp2(i) ) &
      !                  * spr(i) / sgam(i) )
      
      ! dadzCom = ((s_chi_bar_G(i)/dV3)

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



    !end if
  
  end do 
!$OMP END DO


end subroutine getSource_1D

end module FiElec1D