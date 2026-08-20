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

use puffin_kinds, only: WP

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

    real(kind=wp), allocatable :: u(:), rt(:)

! Computed via the algebraically-equivalent but numerically-stable form.
! Writing this as (1/sqrt(1-u) - 1)/eta subtracts two quantities both very
! close to 1 to produce a result of order u/2 ~ 1e-8, losing ~8 significant
! digits before the division by eta (~1e-8) scales it back up. Since
! (1 - sqrt(1-u)) * (1 + sqrt(1-u)) = u, the same value is
! u / (sqrt(1-u) * (1 + sqrt(1-u))), which has no cancellation: u itself is
! a sum of positive terms.

    allocate(u(size(p2)), rt(size(p2)))

!$OMP WORKSHARE

    u = ( 1.0_wp + aw**2*(px**2 + py**2) ) / (gamma0**2 * gamma**2)
    rt = sqrt(1.0_wp - u)
    p2 = u / (eta * rt * (1.0_wp + rt))

!$OMP END WORKSHARE

    deallocate(u, rt)

  end subroutine getP2



end module gtop2
