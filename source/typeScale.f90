! ###############################################
! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause
! ###############################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> This module contains the type definition to hold the scaling constants
!> describing the \f$  (\bar{z}, \bar{z}_2) \f$ radiation frame used in Puffin
!> and associated subroutines.

module typeScale

  use paratype
  use typesAndConstants
  
  implicit none

  type fScale

    real(kind=wp) :: eta
    real(kind=wp) :: rho
    real(kind=wp) :: aw
    real(kind=wp) :: lambda_w
    real(kind=wp) :: gamma_r

    real(kind=wp) :: lambda_r
    real(kind=wp) :: lc, lg
    real(kind=wp) :: ux, uy
    real(kind=wp) :: kappa
    logical :: qOneD

  end type fScale


  INTERFACE scaleG
      MODULE PROCEDURE scaleG_single, scaleG_array
  END INTERFACE


  INTERFACE unscaleG
      MODULE PROCEDURE unscaleG_single, unscaleG_array
  END INTERFACE


  INTERFACE scaleX
      MODULE PROCEDURE scaleX_single, scaleX_array
  END INTERFACE


  INTERFACE unscaleX
      MODULE PROCEDURE unscaleX_single, unscaleX_array
  END INTERFACE


  INTERFACE scalePx
      MODULE PROCEDURE scalePx_single, scalePx_array
  END INTERFACE


  INTERFACE unscalePx
      MODULE PROCEDURE unscalePx_single, unscalePx_array
  END INTERFACE


  INTERFACE scaleT
      MODULE PROCEDURE scaleT_single, scaleT_array
  END INTERFACE


  INTERFACE unscaleT
      MODULE PROCEDURE unscaleT_single, unscaleT_array
  END INTERFACE


  contains





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  Scaling of emittance



!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert unnormalised emittance -> Puffin scaled emittance \f$ \bar{\epsilon} \f$ 
!> @param[in] tScaling Custom Fortran type describing scaling
!> @param[inout] sEmit Input geometric emittance, is output as Scaled emittance

    subroutine scaleEmit(tScaling, sEmit)

      real(kind=wp), intent(inout) :: sEmit
      type(fScale), intent(in) :: tScaling

      sEmit = sEmit / (tScaling%lambda_r / (4.0_wp * pi) )

    end subroutine scaleEmit

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled emittance \f$ \bar{\epsilon} \f$ ->  unnormalised emittance
!> @param[in] tScaling Custom Fortran type describing scaling
!> @param[inout] sEmit Input Scaled emittance, is output as geometric emittance

    subroutine unscaleEmit(tScaling, sEmit)

      real(kind=wp), intent(inout) :: sEmit
      type(fScale), intent(in) :: tScaling


      sEmit = sEmit * (tScaling%lambda_r / (4.0_wp * pi) )

    end subroutine unscaleEmit

!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  Scaling of energy - gamma


!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Lorentz factor \f$ \Gamma \f$ ->  Puffin scaled energy \f$ \Gamma \f$
!> @param[in] tScaling Custom Fortran type describing scaling
!> @param[inout] sGamma Lorentz factor in input, Scaled energy on output

    subroutine scaleG_single(tScaling, sGamma)

      type(fScale), intent(in) :: tScaling
      real(kind=wp), intent(inout) :: sGamma

      sGamma = sGamma / tScaling%gamma_r

    end subroutine scaleG_single

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled energy \f$ \Gamma \f$ ->  Lorentz factor \f$ \gamma \f$
!> @param[in] tScaling Custom Fortran type describing scaling
!> @param[inout] sGamma Scaled energy in input, Lorentz factor on output

    subroutine unscaleG_single(tScaling, sGamma)

      type(fScale), intent(in) :: tScaling
      real(kind=wp), intent(inout) :: sGamma

      sGamma = sGamma * tScaling%gamma_r

    end subroutine unscaleG_single


!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Lorentz factor \f$ \gamma \f$ ->  Puffin scaled energy \f$ \Gamma \f$
!> @param[in] tScaling Custom Fortran type describing scaling
!> @param[inout] sGamma Lorentz factor in input, Scaled energy on output

    subroutine scaleG_array(tScaling, sGamma)

      type(fScale), intent(in) :: tScaling
      real(kind=wp), intent(inout) :: sGamma(:)

      sGamma = sGamma / tScaling%gamma_r

    end subroutine scaleG_array

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled energy \f$ \Gamma \f$ ->  Lorentz factor \f$ \gamma \f$
!> @param[in] tScaling Custom Fortran type describing scaling
!> @param[inout] sGamma Scaled energy in input, Lorentz factor on output

    subroutine unscaleG_array(tScaling, sGamma)

      type(fScale), intent(in) :: tScaling
      real(kind=wp), intent(inout) :: sGamma(:)
    
      sGamma = sGamma * tScaling%gamma_r
    
    end subroutine unscaleG_array


!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  Scaling of transverse coordinate X

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert transverse (x or y) coordinate (in meters) -> Puffin scaled transverse
!> coordinate (\f$ \bar{x} \f$ or \f$ \bar{y} \f$).
!> @param[in] tScaling Custom Fortran type describing scaling.
!> @param[inout] sx Transverse (x or y) coordinate in input, scaled \f$ \bar{x} \f$ 
!> or \f$ \bar{y} \f$ on output.


    subroutine scaleX_single(tScaling, sx)

      type(fScale), intent(in) :: tScaling
      real(kind=wp), intent(inout) :: sx

      sx = sx / sqrt(tScaling%lg * tScaling%lc)

    end subroutine scaleX_single

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled transverse coordinate (\f$ \bar{x} \f$ or \f$ \bar{y} \f$)
!> ->  transverse (x or y) coordinate (in meters).
!> @param[in] tScaling Custom Fortran type describing scaling.
!> @param[inout] sx Scaled \f$ \bar{x} \f$ or scaled \f$ \bar{y} \f$ on input,
!> transverse (x or y) coordinate on output, in meters.

    subroutine unscaleX_single(tScaling, sx)

      type(fScale), intent(in) :: tScaling
  	  real(kind=wp), intent(inout) :: sx

      sx = sx * sqrt(tScaling%lg * tScaling%lc)

    end subroutine unscaleX_single

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert transverse (x or y) coordinate (in meters) -> Puffin scaled transverse
!> coordinate (\f$ \bar{x} \f$ or \f$ \bar{y} \f$).
!> @param[in] tScaling Custom Fortran type describing scaling.
!> @param[inout] sx Transverse (x or y) coordinate in input, scaled \f$ \bar{x} \f$ 
!> or \f$ \bar{y} \f$ on output.

    subroutine scaleX_array(tScaling, sx)

      type(fScale), intent(in) :: tScaling
      real(kind=wp), intent(inout) :: sx(:)

      sx = sx / sqrt(tScaling%lg * tScaling%lc)

    end subroutine scaleX_array

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled transverse coordinate (\f$ \bar{x} \f$ or \f$ \bar{y} \f$)
!> ->  transverse (x or y) coordinate (in meters).
!> @param[in] tScaling Custom Fortran type describing scaling.
!> @param[inout] sx Scaled \f$ \bar{x} \f$ or scaled \f$ \bar{y} \f$ on input,
!> transverse (x or y) coordinate on output, in meters.

    subroutine unscaleX_array(tScaling, sx)

      type(fScale), intent(in) :: tScaling
      real(kind=wp), intent(inout) :: sx(:)

      sx = sx * sqrt(tScaling%lg * tScaling%lc)

    end subroutine unscaleX_array

!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Scaling of transverse momenta px

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert transverse \f$ \frac{dx}{dz} \f$ or \f$ \frac{dy}{dz} \f$ -> Puffin scaled transverse
!> momenta \f$ \bar{p}_x \f$ or \f$ \bar{p}_y \f$.
!> @param[in] tScaling Custom Fortran type describing scaling.
!> @param[in] sgamma Lorentz factor \f$ \gamma \f$ of particle. 
!> @param[inout] sPx Transverse \f$ dx/dz \f$ on input, scaled \f$ \bar{p}_x \f$ 
!> on output.

    subroutine scalePx_single(tScaling, sgamma, sPx)

      type(fScale), intent(in) :: tScaling
      real(kind=wp), intent(in) :: sgamma
      real(kind=wp), intent(inout) :: sPx

      sPx = sPx * sgamma / tScaling%aw

    end subroutine scalePx_single

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled transverse momenta \f$ \bar{p}_x \f$ or \f$ \bar{p}_y \f$
!> -> transverse \f$ \frac{dx}{dz} \f$ or \f$ \frac{dy}{dz} \f$. 
!> @param[in] tScaling Custom Fortran type describing scaling.
!> @param[in] sgamma Lorentz factor \f$ \gamma \f$ of particle. 
!> @param[inout] sPx Scaled \f$ \bar{p}_x \f$ on input, transverse \f$ dx/dz \f$
!> on output.

    subroutine unscalePx_single(tScaling, sgamma, sPx)

      type(fScale), intent(in) :: tScaling
      real(kind=wp), intent(in) :: sgamma
      real(kind=wp), intent(inout) :: sPx

      sPx = sPx * tScaling%aw / sgamma 

    end subroutine unscalePx_single

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert transverse \f$ \frac{dx}{dz} \f$ or \f$ \frac{dy}{dz} \f$ -> Puffin scaled transverse
!> momenta \f$ \bar{p}_x \f$ or \f$ \bar{p}_y \f$.
!> @param[in] tScaling Custom Fortran type describing scaling.
!> @param[in] sgamma Lorentz factor \f$ \gamma \f$ of particle. 
!> @param[inout] sPx Transverse \f$ dx/dz \f$ on input, scaled \f$ \bar{p}_x \f$ 
!> on output.

    subroutine scalePx_array(tScaling, sgamma, sPx)

      type(fScale), intent(in) :: tScaling
      real(kind=wp), intent(in) :: sgamma(:)
      real(kind=wp), intent(inout) :: sPx(:)

      sPx = sPx * sgamma / tScaling%aw

    end subroutine scalePx_array

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled transverse momenta \f$ \bar{p}_x \f$ or \f$ \bar{p}_y \f$
!> -> transverse \f$ \frac{dx}{dz} \f$ or \f$ \frac{dy}{dz} \f$. 
!> @param[in] tScaling Custom Fortran type describing scaling.
!> @param[in] sgamma Lorentz factor \f$ \gamma \f$ of particle. 
!> @param[inout] sPx Scaled \f$ \bar{p}_x \f$ on input, transverse \f$ dx/dz \f$
!> on output.

    subroutine unscalePx_array(tScaling, sgamma, sPx)

      type(fScale), intent(in) :: tScaling
      real(kind=wp), intent(inout) :: sPx(:)
      real(kind=wp), intent(in) :: saw, sgamma(:)

      sPx = sPx * tScaling%aw / sgamma 

    end subroutine unscalePx_array






!!  subroutine scalePx_single(sPx, saw)
!!  
!!  !    Inputting px, outputting \bar{px}
!!  
!!  	real(kind=wp), intent(inout) :: sPx
!!      real(kind=wp), intent(in) :: saw
!!  
!!      sPx = sPx / saw / m_e / c
!!  
!!  end subroutine scalePx_single
!!  
!!  
!!  subroutine unscalePx_single(sPx, saw)
!!  
!!  !    Inputting \bar{px}, outputting px
!!  
!!  	real(kind=wp), intent(inout) :: sPx
!!      real(kind=wp), intent(in) :: saw
!!  
!!      sPx = sPx * saw * m_e * c
!!  
!!  end subroutine unscalePx_single
!!  
!!  subroutine scalePx_array(sPx, saw)
!!  
!!  !    Inputting px, outputting \bar{px}
!!  
!!  	real(kind=wp), intent(inout) :: sPx(:)
!!      real(kind=wp), intent(in) :: saw
!!  
!!      sPx = sPx / saw / m_e / c
!!  
!!  end subroutine scalePx_array
!!  
!!  
!!  subroutine unscalePx_array(sPx, saw)
!!  
!!  !    Inputting \bar{px}, outputting px
!!  
!!  	real(kind=wp), intent(inout) :: sPx(:)
!!      real(kind=wp), intent(in) :: saw
!!  
!!      sPx = sPx * saw * m_e * c
!!  
!!  end subroutine unscalePx_array


!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Scaling of t -> z2



!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert temporal coordinate \f$ t \f$ (in seconds) -> Puffin scaled coordinate 
!> \f$ \bar{z}_2 \f$.
!> @param[in] tScaling Custom Fortran type describing scaling.
!> @param[inout] sT temporal coordinate \f$ t \f$ on input, Puffin scaled
!> coordinate \f$ \bar{z}_2 \f$ on output.

    subroutine scaleT_single(tScaling, sT)

      type(fScale), intent(in) :: tScaling
      real(kind=wp), intent(inout) :: sT

      sT = c * sT / tScaling%lc

    end subroutine scaleT_single

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled coordinate \f$ \bar{z}_2 \f$ -> temporal coordinate 
!> \f$ t \f$ (in seconds).
!> @param[in] tScaling Custom Fortran type describing scaling.
!> @param[inout] sT Puffin scaled coordinate \f$ \bar{z}_2 \f$ on input,
!> temporal coordinate \f$ t \f$ (in seconds) on output.

    subroutine unscaleT_single(tScaling, sT)

      type(fScale), intent(in) :: tScaling
      real(kind=wp), intent(inout) :: sT

      sT = sT * tScaling%lc / c

    end subroutine unscaleT_single


!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert temporal coordinate \f$ t \f$ (in seconds) -> Puffin scaled coordinate 
!> \f$ \bar{z}_2 \f$.
!> @param[in] tScaling Custom Fortran type describing scaling.
!> @param[inout] sT temporal coordinate \f$ t \f$ on input, Puffin scaled
!> coordinate \f$ \bar{z}_2 \f$ on output.

    subroutine scaleT_array(tScaling, sT)

      type(fScale), intent(in) :: tScaling
      real(kind=wp), intent(inout) :: sT(:)

      sT = c * sT / tScaling%lc

    end subroutine scaleT_array

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled coordinate \f$ \bar{z}_2 \f$ -> temporal coordinate 
!> \f$ t \f$ (in seconds).
!> @param[in] tScaling Custom Fortran type describing scaling.
!> @param[inout] sT Puffin scaled coordinate \f$ \bar{z}_2 \f$ on input,
!> temporal coordinate \f$ t \f$ (in seconds) on output.

    subroutine unscaleT_array(tScaling, sT)

      type(fScale), intent(in) :: tScaling
      real(kind=wp), intent(inout) :: sT(:)

      sT = sT * tScaling%lc / c

    end subroutine unscaleT_array

!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Scaling of z -> zbar

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert propagation distance \f$ z \f$ (in seconds) -> Puffin scaled coordinate 
!> \f$ \bar{z} = z / l_g \f$.
!> @param[in] tScaling Custom Fortran type describing scaling.
!> @param[inout] sT Distance \f$ z \f$ (in metres) on input, Puffin scaled
!> coordinate \f$ \bar{z} \f$ on output.

    subroutine scaleZ(tScaling, sZ)

      type(fScale), intent(in) :: tScaling
      real(kind=wp), intent(inout) :: sZ

      sZ = sZ / tScaling%lg

    end subroutine scaleZ

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled distance \f$ \bar{z} \f$ -> propagation distance 
!> \f$ z \f$ (in metres).
!> @param[in] tScaling Custom Fortran type describing scaling.
!> @param[inout] sT Puffin scaled distance \f$ \bar{z} \f$ on input,
!> propagation distance \f$ z \f$ (in metres) on output.

    subroutine unscaleZ(tScaling, sZ)

      type(fScale), intent(in) :: tScaling
      real(kind=wp), intent(inout) :: sZ

      sZ = sZ * tScaling%lg

    end subroutine unscaleZ

!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module typeScale
