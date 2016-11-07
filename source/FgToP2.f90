!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> This module contains functions to transform macroparticle variables
!> from the relativistic factor gamma to the scaled longitudinal 
!> velocity p2, and vice-versa.

module gtop2

use paratype
use globals

implicit none

contains

  subroutine getGamma(gamma, p2, px, py, eta, gamma0, aw)
 
    implicit none

! Return gamma, given p2, px, py and eta
! 
!           ARGUMENTS

    real(kind=wp), intent(in) :: px(:), py(:), p2(:), eta, &
                                 gamma0, aw

!            OUTPUT

    real(kind=wp), intent(out) :: gamma(:)
    
!          LOCAL ARGS

    gamma = SQRT((1.0_WP + ( aw**2 * (px**2.0_WP + py**2.0_WP) )) * &
                  (1.0_WP + eta * p2 )**2.0_WP / &
                  ( eta * p2 * (eta * p2 + 2.0_WP) ) ) / gamma0
  
  end subroutine getGamma

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

  subroutine getP2(p2, gamma, px, py, eta, gamma0, aw)

  implicit none
  
! Return p2, given Gamma, px, py and eta
!
!  ###########################################
! 
!           ARGUMENTS
!
!
!
!  p2       :          Scaled velocity along wiggler axis (output)
!
!  gamma    :          Relativistic factor normalised to gamma_0
!
!  px, py   :          Electron macroparticle transverse momenta,
!                      normalised to aw * m_e * c
!
!  eta      :          Scaled longitudinal velocity of 'reference' 
!                      energy
!
!  gamma_0  :          Relativistic factor of 'reference' energy
!
!  aw       :          Undulator parameter (peak) 
!

    real(kind=wp), intent(in) :: px(:), py(:), gamma(:), eta, gamma0, aw

!            OUTPUT

    real(kind=wp), intent(out) :: p2(:)

    integer(kind=ip) :: i !inels

!$OMP DO
    do i = 1, iNumberElectrons_G
      p2(i) = (( 1_wp/sqrt(1_wp - 1_wp / (gamma0**2 * gamma(i)**2) * ( 1 + &
               aw**2*(px(i)**2 + py(i)**2))))-1_wp) / eta
    end do
!$OMP END DO
  
  end subroutine getP2



end module gtop2