!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

module initConds

! This module contains the functions calculating the initial
! electron macroparticle phase space conditions in Puffin.

use paratype
use Globals


implicit none

  INTERFACE xOffSet
    MODULE PROCEDURE xOffSet_OneValue, xOffSet_Array   
  END INTERFACE 


  INTERFACE yOffSet
    MODULE PROCEDURE yOffSet_OneValue, yOffSet_Array   
  END INTERFACE 

contains

!********************************************************

  FUNCTION xOffSet_OneValue(rho, aw, gamma_r, gamma_j, &
                            eta, k_beta, ff, px, py, &
                            ux, uy, sZ0)
!
! Calculate xOffset value
! Value of Range mid point offset from origin
!
! srho     - Pierce parameter, describe the strength
!            of the field
! saw      - Wiggler parameter
! sgammar  - Mean electron velocity at resonance
! sEpsilon - (1+aw^2)/(2*gammar^2) 
! sZ0      - Starting z position
    REAL(KIND=WP), INTENT(IN) :: rho,aw,gamma_r,gamma_j, &
         eta,px,py,k_beta,ff,ux,uy,sZ0
    REAL(KIND=WP) :: xOffSet_OneValue, nc
    REAL(KIND=WP) ::srBcoeff,s_Sin_zOver2rho
    
    nc = 2.0_WP*aw**2/(ux**2 + uy**2)
    
    srBcoeff = uy * 4.0_WP * sqrt(2.0) * ff * k_beta * & 
              rho**2.0_WP / sqrt(ux**2 + uy**2) / sqrt(eta)* &
              (gamma_r / sqrt(gamma_j**2 - (1.0_WP + nc*(px**2 + py**2)))) !!!TEMP!!!

    s_Sin_zOver2rho = SIN(sZ0 / (2.0_WP * rho))

    xOffSet_OneValue = -srBcoeff * n2col * s_Sin_zOver2rho

  END FUNCTION xOffSet_OneValue




  FUNCTION xOffSet_Array(rho, aw, gamma_r, gamma_j, &
                         eta, k_beta, ff, px, py, &
                         ux, uy, sZ0)
!
! Calculate xOffset value
! Value of Range mid point offset from origin
!
! srho     - Pierce parameter, describe the strength
!            of the field
! saw      - Wiggler parameter
! sgammar  - Mean electron velocity at resonance
! sEpsilon - (1+aw^2)/(2*gammar^2) 
! sZ0      - Starting z position
    REAL(KIND=WP), INTENT(IN) :: rho,aw,gamma_r,gamma_j, &
         eta,px(:),py(:),k_beta,ff,ux,uy,sZ0
    REAL(KIND=WP) :: xOffSet_Array(size(px)), nc
    
    nc = 2.0_WP*aw**2/(ux**2 + uy**2)
    
    xOffSet_Array = -uy * 4.0_WP * sqrt(2.0) * ff * k_beta * & 
                    rho**2.0_WP / sqrt(ux**2 + uy**2) / sqrt(eta)* &
                    (gamma_r / sqrt(gamma_j**2 - (1.0_WP + &
                    nc*(px**2 + py**2)))) * &
                    n2col * SIN(sZ0 / (2.0_WP * rho))


  END FUNCTION xOffSet_Array


!********************************************************

  FUNCTION yOffSet_OneValue(rho, aw, gamma_r, gamma_j, &
                            eta, k_beta, ff, px, py, &
                            ux, uy, sZ0)

! Calculate xOffset value
! Value of Range mid point offset from origin
!
! ARGS:-
!
! srho     - Pierce parameter, describe the strength
!            of the field
! saw      - Wiggler parameter
! sgammar  - Mean electron velocity at resonance
! sEpsilon - (1+aw^2)/(2*gammar^2) 
! sZ0      - Starting z position
!	
    REAL(KIND=WP), INTENT(IN) :: rho,aw,gamma_r,gamma_j, &
         eta,px,py,k_beta,ff,ux,uy,sZ0
    REAL(KIND=WP) :: yOffSet_OneValue, nc
    REAL(KIND=WP) ::srBcoeff,s_Cos_zOver2rho
!
    nc = 2.0_WP*aw**2/(ux**2 + uy**2)
    
    srBcoeff = ux * 4.0_WP * sqrt(2.0_WP) * ff * k_beta * & 
              rho**2.0_WP / sqrt(ux**2 + uy**2) / sqrt(eta) * &
              (gamma_r / sqrt(gamma_j**2 - (1.0_WP + nc*(px**2 + py**2))))
          
    s_Cos_zOver2rho = COS(sZ0 / (2.0_WP * rho))	
! Initial values for the electron pulse in all direction
    yOffSet_OneValue         = srBcoeff * n2col * s_Cos_zOver2rho
      
  END FUNCTION yOffSet_OneValue



  FUNCTION yOffSet_Array(rho, aw, gamma_r, gamma_j, &
                         eta, k_beta, ff, px, py, &
                         ux, uy, sZ0)
!
! Calculate xOffset value
! Value of Range mid point offset from origin
!
! srho     - Pierce parameter, describe the strength
!            of the field
! saw      - Wiggler parameter
! sgammar  - Mean electron velocity at resonance
! sEpsilon - (1+aw^2)/(2*gammar^2) 
! sZ0      - Starting z position

    REAL(KIND=WP), INTENT(IN) :: rho,aw,gamma_r,gamma_j, &
         eta,px(:),py(:),k_beta,ff,ux,uy,sZ0
    REAL(KIND=WP) :: yOffSet_Array(size(px)), nc
    
    nc = 2.0_WP*aw**2/(ux**2 + uy**2)
    
    yOffSet_Array = ux * 4.0_WP * sqrt(2.0_wp) * ff * k_beta * & 
                    rho**2.0_WP / sqrt(ux**2 + uy**2) / sqrt(eta)* &
                    (gamma_r / sqrt(gamma_j**2 - (1.0_WP + &
                    nc*(px**2 + py**2)))) * &
                    n2col * cos(sZ0 / (2.0_WP * rho))


  END FUNCTION yOffSet_Array


!********************************************************

  FUNCTION pxOffset(z, rho, uy)
  
! Equation for the initial electron px offset due to
! the undulator field.  
! 
!               ARGUMENTS

    REAL(KIND=WP), INTENT(IN) :: z, rho, uy

!                OUTPUT

    REAL(KIND=WP) :: pxOffset

    pxOffset = -uy * n2col * COS(z / (2.0_WP * rho))
  
  END FUNCTION pxOffset

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  FUNCTION pyOffset(z, rho, ux)

! Equation for the initial electron py offset due to
! the undulator field.  
! 
!               ARGUMENTS

    REAL(KIND=WP), INTENT(IN) :: z, rho, ux

!                OUTPUT

    REAL(KIND=WP) :: pyOffset

    pyOffset = -ux * n2col * SIN(z / (2.0_WP * rho))
    
  END FUNCTION pyOffset

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

  FUNCTION pz2Offset(gamma, px, py, eta, aw)

! Equation for the initial electron p2 offset due to
! the undulator field. (Doesn't work for n2col /= 1)  
! 
!               ARGUMENTS

    REAL(KIND=WP), INTENT(IN) :: gamma, px, py, eta, aw

!                OUTPUT

    REAL(KIND=WP) :: pz2Offset
    
!              LOCAL ARGS

    REAL(KIND=WP) :: nc


    nc = 2.0_WP*aw**2/(fx_G**2 + fy_G**2)
           
          
    pz2Offset = ((gamma/SQRT(gamma**2 - 1.0_WP - &
                   nc*(px**2 + py**2)))-1.0_WP)/eta
    
  END FUNCTION pz2Offset

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE getOffsets(sZ,samLenE,sZ2_center,gamma_d,offsets)

  IMPLICIT NONE

!             ARGUMENTS

  REAL(KIND=WP), INTENT(IN) :: sZ, samLenE(:),gamma_d
  REAL(KIND=WP), INTENT(INOUT) :: sZ2_center
  REAL(KIND=WP), INTENT(INOUT) :: offsets(:)

!             LOCAL ARGS

  REAL(KIND=WP) :: spx_offset, spy_offset, sx_offset, sy_offset, &
                   sGamma_offset, sz2_offset


!     Get offsets

  spx_offset     = pxOffset(sZ, sRho_G, fy_G)
  
  spy_offset     = pyOffset(sZ, sRho_G, fx_G)
  
  sGamma_offset  = sGammaR_G * gamma_d
         
  sx_offset      = xOffSet(sRho_G, sAw_G,  sGammaR_G, sGamma_offset, &
                           sEta_G, sKBeta_G, sFocusfactor_G, &
                           spx_offset, spy_offset, &
                           fx_G,fy_G, sZ)
            
  sy_offset      = yOffSet(sRho_G, sAw_G,  sGammaR_G, sGamma_offset, &
                           sEta_G, sKBeta_G, sFocusfactor_G, &
                           spx_offset, spy_offset, &
                           fx_G,fy_G, sZ)
              
!  sz2_offset     = samLenE(iZ2_CG)/2.0_WP

  IF (sZ2_center < (samLenE(iZ2_CG) / 2.0_WP)) THEN

    sz2_offset     = samLenE(iZ2_CG) / 2.0_WP

    sZ2_center     = sz2_offset

  ELSE

    sz2_offset     = sZ2_center

  END IF

  offsets(iX_CG)    = sx_offset
  offsets(iY_CG)    = sy_offset
  offsets(iZ2_CG)   = sz2_offset
  offsets(iPX_CG)   = spx_offset
  offsets(iPY_CG)   = spy_offset
  offsets(iPZ2_CG)  = sGamma_offset

END SUBROUTINE getOffsets

end module initConds
