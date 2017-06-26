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

  private

  type, public :: fScale

    real(kind=wp) :: eta
    real(kind=wp) :: rho
    real(kind=wp) :: aw
    real(kind=wp) :: lambda_w
    real(kind=wp) :: gamma0

    real(kind=wp) :: lambda_r
    real(kind=wp) :: lc, lg
    real(kind=wp) :: ux, uy
    real(kind=wp) :: kappa
    logical :: qOneD

  contains
    
    procedure :: init => initScaling
    procedure :: scaleEmit, unscaleEmit
    procedure :: scaleZ, unscaleZ

    generic :: scaleG => scaleG_single, scaleG_array
    generic :: unscaleG => unScaleG_single, unscaleG_array
    generic :: scaleX => scaleX_single, scaleX_array
    generic :: unscaleX => unscaleX_single, unscaleX_array
    generic :: scalePx => scalePX_single, scalePX_array
    generic :: unscalePx => unscalePX_single, unscalePX_array
    generic :: scaleT => scaleT_single, scaleT_array
    generic :: unscaleT => unscaleT_single, unscaleT_array
    
    procedure :: scaleG_single, scaleG_array
    procedure :: unScaleG_single, unscaleG_array
    procedure :: scaleX_single, scaleX_array
    procedure :: unscaleX_single, unscaleX_array
    procedure :: scalePX_single, scalePX_array
    procedure :: unscalePX_single, unscalePX_array
    procedure :: scaleT_single, scaleT_array
    procedure :: unscaleT_single, unscaleT_array
    
  end type fScale

  contains

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Initialize the scaling frame type in Puffin.
!> @param[inout] this Puffin scaling type.
!> @param[in] srho FEL parameter \f$ \rho \f$.
!> @param[in] saw Peak undulator parameter \f$ a_w \f$ (or \f$ K \f$).
!> @param[in] sgamr Lorentz factor \f$ \gamma0 \f$ of reference beam.
!> @param[in] sgamr Undulator period \f$ \lambda_w \f$ (meters).
!> @param[in] zUndType String - undulator type. See manual for legal options.
!> @param[in] sfx Undulator x-polarization \f$ u_x \f$. Ignored for non-empty
!> zUndType.
!> @param[in] sfx Undulator y-polarization \f$ u_y \f$. Ignored for non-empty
!> zUndType.

    subroutine initScaling(this, srho, saw, sgamr, slam_w, &
                           zUndType, sfx, sfy)

      class(fScale), intent(inout) :: this
      real(kind=wp), intent(in) :: srho, saw, sgamr, slam_w, &
                                   sfx, sfy

      character(32_IP), intent(in) :: zUndType

      real(kind=wp) :: saw_rms, sBetaz

      this%rho = srho
      this%ux = sfx
      this%uy = sfy
      this%gamma0 = sgamr
      this%aw = saw
      this%lambda_w = slam_w


      if (zUndType == 'curved') then

        saw_rms =  saw / sqrt(2.0_wp)
        this%ux = 0.0_wp
        this%uy = 1.0_wp

      else if (zUndType == 'planepole') then

        saw_rms =  saw / sqrt(2.0_wp)
        this%ux = 0.0_wp
        this%uy = 1.0_wp

      else if (zUndType == 'helical') then

        saw_rms = saw
        this%ux = 1.0_wp
        this%uy = 1.0_wp

      else  ! Puffin elliptical wiggler...

        saw_rms = saw * SQRT(sfx**2 + sfy**2) / sqrt(2.0_wp)

      end if

      sbetaz = SQRT(sgamr**2.0_WP - 1.0_WP - (saw_rms)**2.0_WP) / &
               sgamr

      this%eta = (1.0_WP - sbetaz) / sbetaz
      this%kappa = saw / 2.0_WP / srho / sgamr

      this%lambda_r = this%lambda_w * this%eta

      this%lg = this%lambda_w / 4.0_WP / pi / srho
      this%lc = this%lambda_r / 4.0_WP / pi / srho

    end subroutine initScaling




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  Scaling of emittance



!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert unnormalised emittance -> Puffin scaled emittance \f$ \bar{\epsilon} \f$
!> @param[in] this Custom Fortran type describing scaling
!> @param[inout] sEmit Input geometric emittance, is output as Scaled emittance

    subroutine scaleEmit(this, sEmit)

      class(fScale), intent(in) :: this
      real(kind=wp), intent(inout) :: sEmit

      sEmit = sEmit / (this%lambda_r / (4.0_wp * pi) )

    end subroutine scaleEmit

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled emittance \f$ \bar{\epsilon} \f$ ->  unnormalised emittance
!> @param[in] this Custom Fortran type describing scaling
!> @param[inout] sEmit Input Scaled emittance, is output as geometric emittance

    subroutine unscaleEmit(this, sEmit)

      class(fScale), intent(in) :: this
      real(kind=wp), intent(inout) :: sEmit

      sEmit = sEmit * (this%lambda_r / (4.0_wp * pi) )

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
!> @param[in] this Custom Fortran type describing scaling
!> @param[inout] sGamma Lorentz factor in input, Scaled energy on output

    subroutine scaleG_single(this, sGamma)

      class(fScale), intent(in) :: this
      real(kind=wp), intent(inout) :: sGamma

      sGamma = sGamma / this%gamma0

    end subroutine scaleG_single

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled energy \f$ \Gamma \f$ ->  Lorentz factor \f$ \gamma \f$
!> @param[in] this Custom Fortran type describing scaling
!> @param[inout] sGamma Scaled energy in input, Lorentz factor on output

    subroutine unscaleG_single(this, sGamma)

      class(fScale), intent(in) :: this
      real(kind=wp), intent(inout) :: sGamma

      sGamma = sGamma * this%gamma0

    end subroutine unscaleG_single


!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Lorentz factor \f$ \gamma \f$ ->  Puffin scaled energy \f$ \Gamma \f$
!> @param[in] this Custom Fortran type describing scaling
!> @param[inout] sGamma Lorentz factor in input, Scaled energy on output

    subroutine scaleG_array(this, sGamma)

      class(fScale), intent(in) :: this
      real(kind=wp), intent(inout) :: sGamma(:)

      sGamma = sGamma / this%gamma0

    end subroutine scaleG_array

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled energy \f$ \Gamma \f$ ->  Lorentz factor \f$ \gamma \f$
!> @param[in] this Custom Fortran type describing scaling
!> @param[inout] sGamma Scaled energy in input, Lorentz factor on output

    subroutine unscaleG_array(this, sGamma)

      class(fScale), intent(in) :: this
      real(kind=wp), intent(inout) :: sGamma(:)

      sGamma = sGamma * this%gamma0

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
!> @param[in] this Custom Fortran type describing scaling.
!> @param[inout] sx Transverse (x or y) coordinate in input, scaled \f$ \bar{x} \f$
!> or \f$ \bar{y} \f$ on output.


    subroutine scaleX_single(this, sx)

      class(fScale), intent(in) :: this
      real(kind=wp), intent(inout) :: sx

      sx = sx / sqrt(this%lg * this%lc)

    end subroutine scaleX_single

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled transverse coordinate (\f$ \bar{x} \f$ or \f$ \bar{y} \f$)
!> ->  transverse (x or y) coordinate (in meters).
!> @param[in] this Custom Fortran type describing scaling.
!> @param[inout] sx Scaled \f$ \bar{x} \f$ or scaled \f$ \bar{y} \f$ on input,
!> transverse (x or y) coordinate on output, in meters.

    subroutine unscaleX_single(this, sx)

      class(fScale), intent(in) :: this
  	  real(kind=wp), intent(inout) :: sx

      sx = sx * sqrt(this%lg * this%lc)

    end subroutine unscaleX_single

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert transverse (x or y) coordinate (in meters) -> Puffin scaled transverse
!> coordinate (\f$ \bar{x} \f$ or \f$ \bar{y} \f$).
!> @param[in] this Custom Fortran type describing scaling.
!> @param[inout] sx Transverse (x or y) coordinate in input, scaled \f$ \bar{x} \f$
!> or \f$ \bar{y} \f$ on output.

    subroutine scaleX_array(this, sx)

      class(fScale), intent(in) :: this
      real(kind=wp), intent(inout) :: sx(:)

      sx = sx / sqrt(this%lg * this%lc)

    end subroutine scaleX_array

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled transverse coordinate (\f$ \bar{x} \f$ or \f$ \bar{y} \f$)
!> ->  transverse (x or y) coordinate (in meters).
!> @param[in] this Custom Fortran type describing scaling.
!> @param[inout] sx Scaled \f$ \bar{x} \f$ or scaled \f$ \bar{y} \f$ on input,
!> transverse (x or y) coordinate on output, in meters.

    subroutine unscaleX_array(this, sx)

      class(fScale), intent(in) :: this
      real(kind=wp), intent(inout) :: sx(:)

      sx = sx * sqrt(this%lg * this%lc)

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
!> @param[in] this Custom Fortran type describing scaling.
!> @param[in] sgamma Lorentz factor \f$ \gamma \f$ of particle.
!> @param[inout] sPx Transverse \f$ dx/dz \f$ on input, scaled \f$ \bar{p}_x \f$
!> on output.

    subroutine scalePx_single(this, sgamma, sPx)

      class(fScale), intent(in) :: this
      real(kind=wp), intent(in) :: sgamma
      real(kind=wp), intent(inout) :: sPx

      sPx = sPx * sgamma / this%aw

    end subroutine scalePx_single

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled transverse momenta \f$ \bar{p}_x \f$ or \f$ \bar{p}_y \f$
!> -> transverse \f$ \frac{dx}{dz} \f$ or \f$ \frac{dy}{dz} \f$.
!> @param[in] this Custom Fortran type describing scaling.
!> @param[in] sgamma Lorentz factor \f$ \gamma \f$ of particle.
!> @param[inout] sPx Scaled \f$ \bar{p}_x \f$ on input, transverse \f$ dx/dz \f$
!> on output.

    subroutine unscalePx_single(this, sgamma, sPx)

      class(fScale), intent(in) :: this
      real(kind=wp), intent(in) :: sgamma
      real(kind=wp), intent(inout) :: sPx

      sPx = sPx * this%aw / sgamma

    end subroutine unscalePx_single

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert transverse \f$ \frac{dx}{dz} \f$ or \f$ \frac{dy}{dz} \f$ -> Puffin scaled transverse
!> momenta \f$ \bar{p}_x \f$ or \f$ \bar{p}_y \f$.
!> @param[in] this Custom Fortran type describing scaling.
!> @param[in] sgamma Lorentz factor \f$ \gamma \f$ of particle.
!> @param[inout] sPx Transverse \f$ dx/dz \f$ on input, scaled \f$ \bar{p}_x \f$
!> on output.

    subroutine scalePx_array(this, sgamma, sPx)

      class(fScale), intent(in) :: this
      real(kind=wp), intent(in) :: sgamma(:)
      real(kind=wp), intent(inout) :: sPx(:)

      sPx = sPx * sgamma / this%aw

    end subroutine scalePx_array

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled transverse momenta \f$ \bar{p}_x \f$ or \f$ \bar{p}_y \f$
!> -> transverse \f$ \frac{dx}{dz} \f$ or \f$ \frac{dy}{dz} \f$.
!> @param[in] this Custom Fortran type describing scaling.
!> @param[in] sgamma Lorentz factor \f$ \gamma \f$ of particle.
!> @param[inout] sPx Scaled \f$ \bar{p}_x \f$ on input, transverse \f$ dx/dz \f$
!> on output.

    subroutine unscalePx_array(this, sgamma, sPx)

      class(fScale), intent(in) :: this
      real(kind=wp), intent(inout) :: sPx(:)
      real(kind=wp), intent(in) :: sgamma(:)

      sPx = sPx * this%aw / sgamma

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
!> @param[in] this Custom Fortran type describing scaling.
!> @param[inout] sT temporal coordinate \f$ t \f$ on input, Puffin scaled
!> coordinate \f$ \bar{z}_2 \f$ on output.

    subroutine scaleT_single(this, sT)

      class(fScale), intent(in) :: this
      real(kind=wp), intent(inout) :: sT

      sT = c * sT / this%lc

    end subroutine scaleT_single

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled coordinate \f$ \bar{z}_2 \f$ -> temporal coordinate
!> \f$ t \f$ (in seconds).
!> @param[in] this Custom Fortran type describing scaling.
!> @param[inout] sT Puffin scaled coordinate \f$ \bar{z}_2 \f$ on input,
!> temporal coordinate \f$ t \f$ (in seconds) on output.

    subroutine unscaleT_single(this, sT)

      class(fScale), intent(in) :: this
      real(kind=wp), intent(inout) :: sT

      sT = sT * this%lc / c

    end subroutine unscaleT_single


!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert temporal coordinate \f$ t \f$ (in seconds) -> Puffin scaled coordinate
!> \f$ \bar{z}_2 \f$.
!> @param[in] this Custom Fortran type describing scaling.
!> @param[inout] sT temporal coordinate \f$ t \f$ on input, Puffin scaled
!> coordinate \f$ \bar{z}_2 \f$ on output.

    subroutine scaleT_array(this, sT)

      class(fScale), intent(in) :: this
      real(kind=wp), intent(inout) :: sT(:)

      sT = c * sT / this%lc

    end subroutine scaleT_array

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled coordinate \f$ \bar{z}_2 \f$ -> temporal coordinate
!> \f$ t \f$ (in seconds).
!> @param[in] this Custom Fortran type describing scaling.
!> @param[inout] sT Puffin scaled coordinate \f$ \bar{z}_2 \f$ on input,
!> temporal coordinate \f$ t \f$ (in seconds) on output.

    subroutine unscaleT_array(this, sT)

      class(fScale), intent(in) :: this
      real(kind=wp), intent(inout) :: sT(:)

      sT = sT * this%lc / c

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
!> @param[in] this Custom Fortran type describing scaling.
!> @param[inout] sT Distance \f$ z \f$ (in metres) on input, Puffin scaled
!> coordinate \f$ \bar{z} \f$ on output.

    subroutine scaleZ(this, sZ)

      class(fScale), intent(in) :: this
      real(kind=wp), intent(inout) :: sZ

      sZ = sZ / this%lg

    end subroutine scaleZ

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Convert Puffin scaled distance \f$ \bar{z} \f$ -> propagation distance
!> \f$ z \f$ (in metres).
!> @param[in] this Custom Fortran type describing scaling.
!> @param[inout] sT Puffin scaled distance \f$ \bar{z} \f$ on input,
!> propagation distance \f$ z \f$ (in metres) on output.

    subroutine unscaleZ(this, sZ)

      class(fScale), intent(in) :: this
      real(kind=wp), intent(inout) :: sZ

      sZ = sZ * this%lg

    end subroutine unscaleZ

!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module typeScale
