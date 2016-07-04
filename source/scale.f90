module scale

	! Module containing routines to scale SI input to rho

use paratype
use typesAndConstants

implicit none


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


subroutine scalePx_single(sPx, saw)

	real(kind=wp), intent(inout) :: sPx
    real(kind=wp), intent(in) :: saw

    sPx = sPx / saw / m_e / c

end subroutine scalePx_single


subroutine unscalePx_single(sPx, saw)

	real(kind=wp), intent(inout) :: sPx
    real(kind=wp), intent(in) :: saw

    sPx = sPx * saw * m_e * c

end subroutine unscalePx_single

subroutine scalePx_array(sPx, saw)

	real(kind=wp), intent(inout) :: sPx(:)
    real(kind=wp), intent(in) :: saw

    sPx = sPx / saw / m_e / c

end subroutine scalePx_array


subroutine unscalePx_array(sPx, saw)

	real(kind=wp), intent(inout) :: sPx(:)
    real(kind=wp), intent(in) :: saw

    sPx = sPx * saw * m_e * c

end subroutine unscalePx_array


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


end module scale
