!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

module gtop2

! This module contains functions to transform macroparticle variables
! from the relativistic factor gamma to the scaled longitudinal 
! velocity p2, and vice-versa.

use Globals
use paratype

implicit none

contains

  FUNCTION getGamma(px, py, p2, eta, aw)
  
! Return gamma, given p2, px, py and eta
! 
!           ARGUMENTS

    REAL(KIND=WP), INTENT(IN) :: px, py, p2, eta, aw

!            OUTPUT

    REAL(KIND=WP) :: getGamma
    
!          LOCAL ARGS

    REAL(KIND=WP) :: sl1


    sl1 = 2.0_WP*aw**2/(fx_G**2 + fy_G**2)      

    getGamma = SQRT((1.0_WP + ( sl1 * (px**2.0_WP + py**2.0_WP) )) * &
                  (1.0_WP + eta * p2 )**2.0_WP / &
                  ( eta * p2 * (eta * p2 + 2.0_WP) ) )
  
  END FUNCTION getGamma

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

  FUNCTION getP2(gamma, px, py, eta, aw)
  
! Return p2, given gamma, px, py and eta
! 
!           ARGUMENTS

    REAL(KIND=WP), INTENT(IN) :: px(:), py(:), gamma(:), eta, aw

!            OUTPUT

    REAL(KIND=WP), DIMENSION(SIZE(gamma)) :: getP2

!              LOCAL ARGS

    REAL(KIND=WP) :: nc


    nc = 2.0_WP*aw**2/(fx_G**2 + fy_G**2)

    getP2 = ((gamma/SQRT(gamma**2 - 1.0_WP - &
             nc*(px**2 + py**2)))-1.0_WP)/eta
  
  END FUNCTION getP2

end module gtop2