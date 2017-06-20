! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

module initConds


!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> This module contains the functions calculating the initial
!> electron macroparticle phase space conditions in Puffin.

use paratype
use Globals


implicit none

  interface xOffSet
    module procedure xOffSet_OneValue, xOffSet_Array   
  end interface 


  interface yOffSet
    module procedure yOffSet_OneValue, yOffSet_Array   
  end interface 

contains

!********************************************************


! Calculate xOffset value
! Value of Range mid point offset from origin
!
! srho     - Pierce parameter, describe the strength
!            of the field
! saw      - Wiggler parameter
! sgammar  - Mean electron velocity at resonance
! sEpsilon - (1+aw^2)/(2*gammar^2) 
! sZ0      - Starting z position

  function xOffSet_OneValue(tScaling, tUndMod, gamma_j, &
                            px, py, sZ0)

    type(fScale), intent(in) :: tScaling
    type(fUndMod), intent(in) :: tUndMod
    real(kind=wp), intent(in) :: gamma_j, px, py, sZ0
    real(kind=wp) :: xOffSet_OneValue, nc
    real(kind=wp) :: srBcoeff,s_Sin_zOver2rho
    real(kind=wp) :: rho, eta, kappa, gamma_r, aw, ux, uy

    kappa = tScaling%kappa
    rho = tScaling%rho
    eta = tScaling%eta
    gamma_r = tScaling%gamma_r
    aw = tScaling%aw
    ux = tUndMod%ux
    uy = tUndMod%uy

    nc = aw**2_wp

    srBcoeff = -uy * 4.0_WP * kappa * & 
              rho**2.0_WP / sqrt(eta)* &
              (gamma_r / sqrt(gamma_j**2 &
                  - (1.0_WP + nc*(px**2 + py**2)))) !!!TEMP!!!


    s_Sin_zOver2rho = SIN(sZ0 / (2.0_WP * rho))

    xOffSet_OneValue = -srBcoeff * n2col * s_Sin_zOver2rho

  end function xOffSet_OneValue


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


  function xOffSet_Array(tScaling, tUndMod, gamma_j, &
                         px, py, sZ0)

    type(fScale), intent(in) :: tScaling
    type(fUndMod), intent(in) :: tUndMod
    real(kind=wp), intent(in) :: gamma_j(:), px(:), py(:), sZ0
    real(kind=wp) :: xOffSet_Array(size(px)), nc
    real(kind=wp) :: rho, eta, kappa, gamma_r, aw, ux, uy

    kappa = tScaling%kappa
    rho = tScaling%rho
    eta = tScaling%eta
    gamma_r = tScaling%gamma_r
    aw = tScaling%aw
    ux = tUndMod%ux
    uy = tUndMod%uy

    nc = aw**2_wp

    xOffSet_Array = -uy * 4.0_WP * kappa * & 
              rho**2.0_WP / sqrt(eta)* &
              (gamma_r / sqrt(gamma_j**2 &
                  - (1.0_WP + nc*(px**2 + py**2)))) * &
              n2col * sin(sZ0 / (2.0_WP * rho))

  end function xOffSet_Array


!********************************************************

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


  function yOffSet_OneValue(tScaling, tUndMod, gamma_j, &
                            px, py, sZ0)

    type(fScale), intent(in) :: tScaling
    type(fUndMod), intent(in) :: tUndMod
    real(kind=wp), intent(in) :: gamma_j, px, py, sZ0
    real(kind=wp) :: yOffSet_OneValue, nc
    real(kind=wp) ::srBcoeff,s_Cos_zOver2rho
    real(kind=wp) :: rho, eta, kappa, gamma_r, aw, ux, uy

    kappa = tScaling%kappa
    rho = tScaling%rho
    eta = tScaling%eta
    gamma_r = tScaling%gamma_r
    aw = tScaling%aw
    ux = tUndMod%ux
    uy = tUndMod%uy
    
    nc = aw**2


    srBcoeff = ux * 4.0_WP * kappa * & 
              rho**2.0_WP / sqrt(eta)* &
              (gamma_r / sqrt(gamma_j**2 &
                  - (1.0_WP + nc*(px**2 + py**2))))

    s_Cos_zOver2rho = COS(sZ0 / (2.0_WP * rho))

    yOffSet_OneValue = srBcoeff * n2col * s_Cos_zOver2rho
      
  end function yOffSet_OneValue


! Calculate xOffset value
! Value of Range mid point offset from origin
!
! srho     - Pierce parameter, describe the strength
!            of the field
! saw      - Wiggler parameter
! sgammar  - Mean electron velocity at resonance
! sEpsilon - (1+aw^2)/(2*gammar^2) 
! sZ0      - Starting z position


  function yOffSet_Array(tScaling, tUndMod, gamma_j, &
                         px, py, sZ0)


    type(fScale), intent(in) :: tScaling
    type(fUndMod), intent(in) :: tUndMod
    real(kind=wp), intent(in) :: gamma_j(:), px(:), py(:),sZ0
    real(kind=wp) :: yOffSet_Array(size(px)), nc
    real(kind=wp) :: rho, eta, kappa, gamma_r, aw, ux, uy

    kappa = tScaling%kappa
    rho = tScaling%rho
    eta = tScaling%eta
    gamma_r = tScaling%gamma_r
    aw = tScaling%aw
    ux = tUndMod%ux
    uy = tUndMod%uy
    
    nc = 2.0_WP*aw**2/(ux**2 + uy**2)
    

    yOffSet_Array = ux * 4.0_WP * kappa * & 
                    rho**2.0_WP / sqrt(eta)* &
                    (gamma_r / sqrt(gamma_j**2 &
                       - (1.0_WP + nc*(px**2 + py**2)))) * &
                    n2col * cos(sZ0 / (2.0_WP * rho))


  end function yOffSet_Array


!********************************************************

! Equation for the initial electron px offset due to
! the undulator field.  

  function pxOffset(tScaling, tUndMod, z)

    type(fScale), intent(in) :: tScaling
    type(fUndMod), intent(in) :: tUndMod
    real(kind=wp), intent(in) :: z

    real(kind=wp) :: pxOffset

    pxOffset = - tUndMod%uy * n2col * cos(z / (2.0_WP * tScaling%rho))
  
  end function pxOffset

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Equation for the initial electron py offset due to
! the undulator field.  (NOTE:- py offset, NOT
! IM(pperp) offset)

  function pyOffset(tScaling, tUndMod, z)

    type(fScale), intent(in) :: tScaling
    type(fUndMod), intent(in) :: tUndMod
    real(kind=wp), intent(in) :: z
    real(kind=wp) :: pyOffset

    pyOffset = -tUndMod%ux * n2col * sin(z / (2.0_WP * tScaling%rho))
    
  end function pyOffset

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  

!  function pz2Offset(gamma, px, py, eta, aw)
!
!! Equation for the initial electron p2 offset due to
!! the undulator field. (Doesn't work for n2col /= 1)  
!! 
!!               ARGUMENTS
!
!    REAL(KIND=WP), INTENT(IN) :: gamma, px, py, eta, aw
!
!!                OUTPUT
!
!    REAL(KIND=WP) :: pz2Offset
!    
!!              LOCAL ARGS
!
!    REAL(KIND=WP) :: nc
!
!
!    nc = 2.0_WP*aw**2/(fx_G**2 + fy_G**2)
!           
!          
!    pz2Offset = ((gamma/SQRT(gamma**2 - 1.0_WP - &
!                   nc*(px**2 + py**2)))-1.0_WP)/eta
!    
!  END FUNCTION pz2Offset

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine getOffsets(tScaling, tUndMod, sZ, samLenE, sZ2_center, gamma_d, offsets)

  implicit none

  type(fScale), intent(in) :: tScaling
  type(fUndMod), intent(in) :: tUndMod
  real(kind=wp), intent(in) :: sZ, samLenE(:),gamma_d
  real(kind=wp), intent(inout) :: sZ2_center
  real(kind=wp), intent(inout) :: offsets(:)

  real(kind=wp) :: spx_offset, spy_offset, sx_offset, sy_offset, &
                   sGamma_offset, sz2_offset, ux, uy


!     Get offsets

  spx_offset     = pxOffset(tScaling, tUndMod, sZ)
  
  spy_offset     = pyOffset(tScaling, tUndMod, sZ)
  
  sGamma_offset  = tScaling%gamma_r * gamma_d
         
  sx_offset      = xOffSet(tScaling, tUndMod, sGamma_offset, &
                           spx_offset, spy_offset, sZ)
            
  sy_offset      = yOffSet(tScaling, tUndMod, sGamma_offset, &
                           spx_offset, spy_offset, sZ)

  if (sZ2_center < (samLenE(iZ2_CG) / 2.0_WP)) then

    sz2_offset     = samLenE(iZ2_CG) / 2.0_WP

    sZ2_center     = sz2_offset

  else

    sz2_offset     = sZ2_center

  end if

  offsets(iX_CG)    = sx_offset
  offsets(iY_CG)    = sy_offset
  offsets(iZ2_CG)   = sz2_offset
  offsets(iPX_CG)   = spx_offset
  offsets(iPY_CG)   = spy_offset
  offsets(iGam_CG)  = sGamma_offset

end subroutine getOffsets

end module initConds
