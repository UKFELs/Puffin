module FiElec1D

use paratype
use globals

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
  do i = 1, maxEl
    if (i<=procelectrons_G(1)) then 


!                  Get surrounding nodes 


      z2node = floor(sz2(i)  / dz2)  + 1_IP
      locz2 = sz2(i) - REAL(z2node  - 1_IP, kind=wp) * dz2
  
      lis_GR(1,i) = (1.0_wp - locz2/dz2)
      lis_GR(2,i) = 1 - lis_GR(1,i)

    end if
  end do
!$OMP END DO


end subroutine getInterps_1D






!      ##################################################




subroutine getFFelecs_1D(sA)


use rhs_vars

real(kind=wp), intent(in) :: sA(:)
integer(kind=ip) :: i

!$OMP DO
  do i = 1, maxEl
  
    if (i<=procelectrons_G(1)) then

      sField4ElecReal(i) = lis_GR(1,i) * sA(p_nodes(i)) + sField4ElecReal(i)
      sField4ElecReal(i) = lis_GR(2,i) * sA(p_nodes(i) + 1_ip) + sField4ElecReal(i)
  
      sField4ElecImag(i) = lis_GR(1,i) * sA(p_nodes(i) + retim) + sField4ElecImag(i)
      sField4ElecImag(i) = lis_GR(2,i) * sA(p_nodes(i) + retim + 1_ip) + sField4ElecImag(i)

    end if
  
  end do 
!$OMP END DO

end subroutine getFFelecs_1D



!         ########################################################



subroutine getSource_1D(sDADz, spr, spi, sgam, seta)


use rhs_vars

real(kind=wp), intent(inout) :: sDADz(:)
real(kind=wp), intent(in) :: spr(:), spi(:)
real(kind=wp), intent(in) :: sgam(:)
real(kind=wp), intent(in) :: seta

integer(kind=ipl) :: i
real(kind=wp) :: dadzRInst, dadzIInst


!$OMP DO PRIVATE(dadzRInst, dadzIInst)
  do i = 1, maxEl
  
    if (i<=procelectrons_G(1)) then


!                  Get 'instantaneous' dAdz

      dadzRInst = ((s_chi_bar_G(i)/dV3) * (1 + seta * sp2(i) ) &
                        * spr(i) / sgam(i) )
      
      !$OMP ATOMIC
      sDADz(p_nodes(i)) =                         &
        lis_GR(1,i) * dadzRInst + sDADz(p_nodes(i))
      
      !$OMP ATOMIC
      sDADz(p_nodes(i) + 1_ip) =                  &
        lis_GR(2,i) * dadzRInst + sDADz(p_nodes(i) + 1_ip)                


!                   Imaginary part

      dadzIInst = ((s_chi_bar_G(i)/dV3) * (1 + seta * sp2(i) ) &
                        * spi(i) / sgam(i) ) 
      
      !$OMP ATOMIC
      sDADz(p_nodes(i) + retim) =                             & 
        lis_GR(1,i) * dadzIInst + sDADz(p_nodes(i) + retim)                        

      !$OMP ATOMIC
      sDADz(p_nodes(i) + 1_ip + retim) =                      & 
        lis_GR(2,i) * dadzIInst + sDADz(p_nodes(i) + 1_ip + retim)

    end if
  
  end do 
!$OMP END DO


end subroutine getSource_1D

end module FiElec1D