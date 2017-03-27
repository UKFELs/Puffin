! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Module to add additional preparation steps to the electron beam. Can add
!> beam modulations in z2, and/or a linear energy chirp.

module beamPrep

use paratype

contains 

subroutine addChirp(gamj, z2, Nk, center, chirp)

    real(kind=wp), intent(inout) :: gamj(:), z2(:)
    integer(kind=ipl), intent(in) :: Nk
    real(kind=wp), intent(in) :: center, chirp

    real(kind=wp), allocatable :: Qchoff(:)


    allocate(Qchoff(Nk))

!     Add linear chirp to the beam

    Qchoff = chirp*(z2 - center)
    gamj = gamj + Qchoff

    deallocate(Qchoff)

end subroutine addChirp


subroutine addModulation(gamj, z2, Nk, mag, fr)

    real(kind=wp), intent(inout) :: gamj(:), z2(:)
    integer(kind=ipl), intent(in) :: Nk
    real(kind=wp), intent(in) :: mag, fr


!     Add energy modulation to the beam

    gamj = gamj + ( mag * cos(fr * z2) )

end subroutine addModulation


end module beamPrep