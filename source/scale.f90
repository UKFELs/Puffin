! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

module scale

	! Module containing routines to scale SI input to rho

use paratype
use typesAndConstants

implicit none


interface scaleG
    module procedure scaleG_single, scaleG_array
end interface


interface unscaleG
    module procedure unscaleG_single, unscaleG_array
end interface


interface scaleX
    module procedure scaleX_single, scaleX_array
end interface


interface unscaleX
    module procedure unscaleX_single, unscaleX_array
end interface


interface scalePx
    module procedure scalePx_single, scalePx_array
end interface


interface unscalePx
    module procedure unscalePx_single, unscalePx_array
end interface


interface scaleT
    module procedure scaleT_single, scaleT_array
end interface


interface unscaleT
    module procedure unscaleT_single, unscaleT_array
end interface

interface scaleIntensity
    module procedure scaleIntensity_single, scaleIntensity_array
end interface

interface unscaleIntensity
    module procedure unscaleIntensity_single, unscaleIntensity_array
end interface

contains





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  Scaling of emittance



subroutine scaleEmit(sEmit, sLambda_R)

!     Inputting unnormalized emittance, 
!     outputting scaled emittance \bar{\epsilon}

    real(kind=wp), intent(inout) :: sEmit
    real(kind=wp), intent(in) :: sLambda_R

    sEmit = sEmit / (sLambda_R / (4.0_wp * pi) )

end subroutine scaleEmit


subroutine unscaleEmit(sEmit, sLambda_R)

!     Inputting scaled emittance \bar{\epsilon},
!     outputting unnormalized emittance

    real(kind=wp), intent(inout) :: sEmit
    real(kind=wp), intent(in) :: sLambda_R

    sEmit = sEmit * (sLambda_R / (4.0_wp * pi) )

end subroutine unscaleEmit

!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  Scaling of energy - gamma



subroutine scaleG_single(sGamma, sGamma0)

	real(kind=wp), intent(inout) :: sGamma
    real(kind=wp), intent(in) :: sGamma0

    sGamma = sGamma / sGamma0

end subroutine scaleG_single


subroutine unscaleG_single(sGamma, sGamma0)

	real(kind=wp), intent(inout) :: sGamma
    real(kind=wp), intent(in) :: sGamma0

    sGamma = sGamma * sGamma0

end subroutine unscaleG_single



subroutine scaleG_array(sGamma, sGamma0)

	real(kind=wp), intent(inout) :: sGamma(:)
    real(kind=wp), intent(in) :: sGamma0

    sGamma = sGamma / sGamma0

end subroutine scaleG_array


subroutine unscaleG_array(sGamma, sGamma0)

	real(kind=wp), intent(inout) :: sGamma(:)
    real(kind=wp), intent(in) :: sGamma0

    sGamma = sGamma * sGamma0

end subroutine unscaleG_array


!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  Scaling of transverse coordinate X

subroutine scaleX_single(sx, Lg, Lc)

	real(kind=wp), intent(inout) :: sx
    real(kind=wp), intent(in) :: Lg, Lc

    sx = sx / sqrt(Lg*Lc)

end subroutine scaleX_single


subroutine unscaleX_single(sx, Lg, Lc)

	real(kind=wp), intent(inout) :: sx
    real(kind=wp), intent(in) :: Lg, Lc

    sx = sx * sqrt(Lg*Lc)

end subroutine unscaleX_single







subroutine scaleX_array(sx, Lg, Lc)

	real(kind=wp), intent(inout) :: sx(:)
    real(kind=wp), intent(in) :: Lg, Lc

    sx = sx / sqrt(Lg*Lc)

end subroutine scaleX_array


subroutine unscaleX_array(sx, Lg, Lc)

	real(kind=wp), intent(inout) :: sx(:)
    real(kind=wp), intent(in) :: Lg, Lc

    sx = sx * sqrt(Lg*Lc)

end subroutine unscaleX_array

!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Scaling of transverse momenta px



subroutine scalePx_single(sPx, sgamma, saw)

!    Inputting dx/dz, outputting \bar{px}

    real(kind=wp), intent(inout) :: sPx
    real(kind=wp), intent(in) :: saw, sgamma


    sPx = sPx * sgamma / saw

end subroutine scalePx_single



subroutine unscalePx_single(sPx, sgamma, saw)

!    Inputting \bar{px}, outputting dx/dz

    real(kind=wp), intent(inout) :: sPx
    real(kind=wp), intent(in) :: saw, sgamma


    sPx = sPx * saw / sgamma 

end subroutine unscalePx_single



subroutine scalePx_array(sPx, sgamma, saw)

!    Inputting dx/dz, outputting \bar{px}

    real(kind=wp), intent(inout) :: sPx(:)
    real(kind=wp), intent(in) :: saw, sgamma(:)


    sPx = sPx * sgamma / saw

end subroutine scalePx_array



subroutine unscalePx_array(sPx, sgamma, saw)

!    Inputting \bar{px}, outputting dx/dz

    real(kind=wp), intent(inout) :: sPx(:)
    real(kind=wp), intent(in) :: saw, sgamma(:)


    sPx = sPx * saw / sgamma 

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

subroutine scaleT_single(sT, Lc)

! Subroutine to scale t -> z2, for use in Puffin
! Takes in Lc, the cooperation length

	real(kind=wp), intent(inout) :: sT
    real(kind=wp), intent(in) :: Lc

    sT = c * sT / Lc

end subroutine scaleT_single


subroutine unscaleT_single(sT, Lc)

	real(kind=wp), intent(inout) :: sT
    real(kind=wp), intent(in) :: Lc

    sT = sT * Lc / c

end subroutine unscaleT_single

subroutine scaleT_array(sT, Lc)

! Subroutine to scale t -> z2, for use in Puffin
! Takes in Lc, the cooperation length

	real(kind=wp), intent(inout) :: sT(:)
    real(kind=wp), intent(in) :: Lc

    sT = c * sT / Lc

end subroutine scaleT_array


subroutine unscaleT_array(sT, Lc)

	real(kind=wp), intent(inout) :: sT(:)
    real(kind=wp), intent(in) :: Lc

    sT = sT * Lc / c

end subroutine unscaleT_array

!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Scaling of z -> zbar

subroutine scaleZ(sZ, Lg)

	  real(kind=wp), intent(inout) :: sZ
    real(kind=wp), intent(in) :: Lg

    sZ = sZ / Lg

end subroutine scaleZ


subroutine unscaleZ(sZ, Lg)

	real(kind=wp), intent(inout) :: sZ
    real(kind=wp), intent(in) :: Lg

    sZ = sZ * Lg

end subroutine unscaleZ

!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! Scaling of I -> |A|^2

subroutine scaleIntensity_single(intensity, Lg, Lc, gammar, kappa)

    real(kind=wp), intent(out) :: intensity
    real(kind=wp), intent(in) :: Lg, Lc, gammar, kappa

    real(kind=wp) :: intScale

    intScale = c * e_0 * ((gammar * m_e * c**2.0_wp ) / &
                 (q_e * kappa * Lg ))**2.0_wp

	  intensity = intensity / intScale

end subroutine scaleIntensity_single

subroutine scaleIntensity_array(intensity, Lg, Lc, gammar, kappa)

    real(kind=wp), intent(out) :: intensity(:)
    real(kind=wp), intent(in) :: Lg, Lc, gammar, kappa

    real(kind=wp) :: intScale

    intScale = c * e_0 * ((gammar * m_e * c**2.0_wp ) / &
                 (q_e * kappa * Lg ))**2.0_wp

	  intensity = intensity / intScale

end subroutine scaleIntensity_array

subroutine unscaleIntensity_single(intensity, Lg, Lc, gammar, kappa)

    real(kind=wp), intent(out) :: intensity
    real(kind=wp), intent(in) :: Lg, Lc, gammar, kappa

    real(kind=wp) :: intScale

    intScale = c * e_0 * ((gammar * m_e * c**2.0_wp ) / &
                 (q_e * kappa * Lg ))**2.0_wp

	  intensity = intensity * intScale

end subroutine unscaleIntensity_single

subroutine unscaleIntensity_array(intensity, Lg, Lc, gammar, kappa)

    real(kind=wp), intent(out) :: intensity(:)
    real(kind=wp), intent(in) :: Lg, Lc, gammar, kappa

    real(kind=wp) :: intScale

    intScale = c * e_0 * ((gammar * m_e * c**2.0_wp ) / &
                 (q_e * kappa * Lg ))**2.0_wp

	  intensity = intensity * intScale

end subroutine unscaleIntensity_array

!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



end module scale
