! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

module initConds

! This module contains the functions calculating the initial
! electron macroparticle phase space conditions in Puffin.

use puffin_kinds
use Globals
use GlobalTypes, only: tFELFrame


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
                            eta, kappa, ff, px, py, &
                            ux, uy, sZ0, n2col)
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
         eta,px,py,kappa,ff,ux,uy,sZ0,n2col
    REAL(KIND=WP) :: xOffSet_OneValue, nc
    REAL(KIND=WP) ::srBcoeff,s_Sin_zOver2rho

    !nc = 2.0_WP*aw**2/(ux**2 + uy**2)
    nc = aw**2_wp

!    srBcoeff = uy * 4.0_WP * sqrt(2.0) * ff * k_beta * &
!              rho**2.0_WP / sqrt(ux**2 + uy**2) / sqrt(eta)* &
!              (gamma_r / sqrt(gamma_j**2 - (1.0_WP + nc*(px**2 + py**2)))) !!!TEMP!!!


    srBcoeff = -uy * 4.0_WP * kappa * &
              rho**2.0_WP / sqrt(eta)* &
              (gamma_r / sqrt(gamma_j**2 &
                  - (1.0_WP + nc*(px**2 + py**2)))) !!!TEMP!!!


    s_Sin_zOver2rho = SIN(sZ0 / (2.0_WP * rho))

    xOffSet_OneValue = -srBcoeff * n2col * s_Sin_zOver2rho

  END FUNCTION xOffSet_OneValue




  FUNCTION xOffSet_Array(rho, aw, gamma_r, gamma_j, &
                         eta, kappa, ff, px, py, &
                         ux, uy, sZ0, n2col)
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
    REAL(KIND=WP), INTENT(IN) :: rho,aw,gamma_r,gamma_j(:), &
         eta,px(:),py(:),kappa,ff,ux,uy,sZ0,n2col
    REAL(KIND=WP) :: xOffSet_Array(size(px)), nc

    nc = aw**2_wp

!    xOffSet_Array = -uy * 4.0_WP * sqrt(2.0) * ff * k_beta * &
!                    rho**2.0_WP / sqrt(ux**2 + uy**2) / sqrt(eta)* &
!                    (gamma_r / sqrt(gamma_j**2 - (1.0_WP + &
!                    nc*(px**2 + py**2)))) * &
!                    n2col * SIN(sZ0 / (2.0_WP * rho))


    xOffSet_Array = -uy * 4.0_WP * kappa * &
              rho**2.0_WP / sqrt(eta)* &
              (gamma_r / sqrt(gamma_j**2 &
                  - (1.0_WP + nc*(px**2 + py**2)))) * &
              n2col * SIN(sZ0 / (2.0_WP * rho))

  END FUNCTION xOffSet_Array


!********************************************************

  FUNCTION yOffSet_OneValue(rho, aw, gamma_r, gamma_j, &
                            eta, kappa, ff, px, py, &
                            ux, uy, sZ0, n2col)

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
         eta,px,py,kappa,ff,ux,uy,sZ0,n2col
    REAL(KIND=WP) :: yOffSet_OneValue, nc
    REAL(KIND=WP) ::srBcoeff,s_Cos_zOver2rho
!
!    nc = 2.0_WP*aw**2/(ux**2 + uy**2)
    nc = aw**2

!    srBcoeff = ux * 4.0_WP * sqrt(2.0_WP) * ff * k_beta * &
!              rho**2.0_WP / sqrt(ux**2 + uy**2) / sqrt(eta) * &
!              (gamma_r / sqrt(gamma_j**2 - (1.0_WP + nc*(px**2 + py**2))))


    srBcoeff = ux * 4.0_WP * kappa * &
              rho**2.0_WP / sqrt(eta)* &
              (gamma_r / sqrt(gamma_j**2 &
                  - (1.0_WP + nc*(px**2 + py**2))))

    s_Cos_zOver2rho = COS(sZ0 / (2.0_WP * rho))
! Initial values for the electron pulse in all direction
    yOffSet_OneValue         = srBcoeff * n2col * s_Cos_zOver2rho

  END FUNCTION yOffSet_OneValue



  FUNCTION yOffSet_Array(rho, aw, gamma_r, gamma_j, &
                         eta, kappa, ff, px, py, &
                         ux, uy, sZ0, n2col)
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

    REAL(KIND=WP), INTENT(IN) :: rho,aw,gamma_r,gamma_j(:), &
         eta,px(:),py(:),kappa,ff,ux,uy,sZ0,n2col
    REAL(KIND=WP) :: yOffSet_Array(size(px)), nc

    nc = 2.0_WP*aw**2/(ux**2 + uy**2)

!    yOffSet_Array = ux * 4.0_WP * sqrt(2.0_wp) * ff * k_beta * &
!                    rho**2.0_WP / sqrt(ux**2 + uy**2) / sqrt(eta)* &
!                    (gamma_r / sqrt(gamma_j**2 - (1.0_WP + &
!                    nc*(px**2 + py**2)))) * &
!                    n2col * cos(sZ0 / (2.0_WP * rho))


    yOffSet_Array = ux * 4.0_WP * kappa * &
                    rho**2.0_WP / sqrt(eta)* &
                    (gamma_r / sqrt(gamma_j**2 &
                       - (1.0_WP + nc*(px**2 + py**2)))) * &
                    n2col * cos(sZ0 / (2.0_WP * rho))


  END FUNCTION yOffSet_Array


!********************************************************

  FUNCTION pxOffset(z, rho, uy, n2col)

! Equation for the initial electron px offset due to
! the undulator field.
!
!               ARGUMENTS

    REAL(KIND=WP), INTENT(IN) :: z, rho, uy, n2col

!                OUTPUT

    REAL(KIND=WP) :: pxOffset

    pxOffset = -uy * n2col * COS(z / (2.0_WP * rho))

  END FUNCTION pxOffset

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  FUNCTION pyOffset(z, rho, ux, n2col)

! Equation for the initial electron py offset due to
! the undulator field.  (NOTE:- py offset, NOT
! IM(pperp) offset)
!
!               ARGUMENTS

    REAL(KIND=WP), INTENT(IN) :: z, rho, ux, n2col

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

SUBROUTINE getOffsets(sZ,samLenE,sZ2_center,gamma_d,offsets,frame,n2col)

  IMPLICIT NONE

!             ARGUMENTS

  REAL(KIND=WP), INTENT(IN) :: sZ, samLenE(:),gamma_d,n2col
  REAL(KIND=WP), INTENT(INOUT) :: sZ2_center
  REAL(KIND=WP), INTENT(INOUT) :: offsets(:)
  type(tFELFrame), intent(in) :: frame

!             LOCAL ARGS

  REAL(KIND=WP) :: spx_offset, spy_offset, sx_offset, sy_offset, &
                   sGamma_offset, sz2_offset


!     Get offsets

  spx_offset     = pxOffset(sZ, frame%rho, fy_G, n2col)

  spy_offset     = pyOffset(sZ, frame%rho, fx_G, n2col)

  sGamma_offset  = frame%gamma_ref * gamma_d

  sx_offset      = xOffSet(frame%rho, frame%aw, frame%gamma_ref, sGamma_offset, &
                           frame%eta, frame%kappa, sFocusfactor_G, &
                           spx_offset, spy_offset, &
                           fx_G, fy_G, sZ, n2col)

  sy_offset      = yOffSet(frame%rho, frame%aw, frame%gamma_ref, sGamma_offset, &
                           frame%eta, frame%kappa, sFocusfactor_G, &
                           spx_offset, spy_offset, &
                           fx_G, fy_G, sZ, n2col)

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
  offsets(iGam_CG)  = sGamma_offset

END SUBROUTINE getOffsets

end module initConds
