! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> This module contains functions to transform macroparticle variables
!> from the relativistic factor gamma to the scaled longitudinal
!> velocity p2, and vice-versa.

module gtop2

use puffin_kinds, only: WP, IP

implicit none (type, external)
private

public :: getp2


contains

  subroutine getGamma(gamma, p2, px, py, eta, gamma0, aw)

    implicit none (type, external)

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

  implicit none (type, external)

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

    real(kind=wp), contiguous, intent(in) :: px(:), py(:), gamma(:)
    real(kind=wp), intent(in) :: eta, gamma0, aw

!            OUTPUT

    real(kind=wp), contiguous, intent(out) :: p2(:)

!            LOCAL

    integer(kind=ip) :: i
    real(kind=wp) :: u, rt

! Computed via the algebraically-equivalent but numerically-stable form.
! Writing this as (1/sqrt(1-u) - 1)/eta subtracts two quantities both very
! close to 1 to produce a result of order u/2 ~ 1e-8, losing ~8 significant
! digits before the division by eta (~1e-8) scales it back up. Since
! (1 - sqrt(1-u)) * (1 + sqrt(1-u)) = u, the same value is
! u / (sqrt(1-u) * (1 + sqrt(1-u))), which has no cancellation: u itself is
! a sum of positive terms.
!
! Written as an explicit !$OMP DO over private scalars rather than array
! expressions in a WORKSHARE. getP2 is called from inside the !$OMP PARALLEL
! region in getrhs, where any local array temporary would be thread-private:
! a WORKSHARE would then have each thread fill only its own slice of its own
! copy and read the rest uninitialised. Only p2 -- a dummy argument, and so
! shared -- may be written across a worksharing construct here.

!$OMP DO PRIVATE(u, rt)

    do i = 1, size(p2, kind=ip)
      u = ( 1.0_wp + aw**2*(px(i)**2 + py(i)**2) ) / (gamma0**2 * gamma(i)**2)
      rt = sqrt(1.0_wp - u)
      p2(i) = u / (eta * rt * (1.0_wp + rt))
    end do

!$OMP END DO

  end subroutine getP2



end module gtop2
