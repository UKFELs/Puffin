!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

module Equations2


use paratype
use ArrayFunctions
use Globals
use rhs_vars

implicit none



!private real(kind=wp), allocatable :: dp2f(:), sField4ElecReal(:), &
!                                      sField4ElecImag(:) !, &
!                                      !Lj(:)



!real(kind=wp), allocatable :: dp2f(:), sField4ElecReal(:), &
!                              sField4ElecImag(:) !, &
                              !Lj(:)



contains

  subroutine dppdz_r_fT(sx, sy, sz2, spr, spi, sgam, sZ, spt2, &
                       sF4ER, sF4EI, bxut, byut, bzut, sdpr, qOK)

  	implicit none


    real(kind=wp), intent(in) :: sx, sy, sz2, spr, spi, sgam, sZ, &
                                 spt2, sF4ER, sF4EI, bxut, byut, bzut
    real(kind=wp), intent(out) :: sdpr

    real(kind=wp) :: szt

    logical, intent(inout) :: qOK

    integer(kind=ip) :: i

    LOGICAL :: qOKL

    qOK = .false.

! !$OMP DO
!    do i=1,iNumberElectrons_G
      sdpr = sInv2rho * ( n2col * byut  & 
                          - sEta_G * spt2 / sKappa_G**2 *    &
                          sF4ER ) & 
             + sKappa_G * spi / sgam * (1 + sEta_G * spt2) &
                 * n2col * bzut
!    end do
! !$OMP END DO

! Set the error flag and exit

    qOK = .true.

    !goto 2000 

    !1000 call Error_log('Error in equations:dppdz_r',tErrorLog_G)
    
    !print*,'Error in equations:dppdz_r'

    !2000 continue

  end subroutine dppdz_r_fT


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






  subroutine dppdz_i_fT(sx, sy, sz2, spr, spi, sgam, sZ, spt2, &
                       sF4ER, sF4EI, bxut, byut, bzut, sdpi, qOK)

    implicit none


    real(kind=wp), intent(in) :: sx, sy, sz2, spr, spi, sgam, sZ, &
                                 spt2, sF4ER, sF4EI, bxut, byut, bzut
    real(kind=wp), intent(out) :: sdpi

    real(kind=wp) :: szt

    logical, intent(inout) :: qOK

    integer(kind=ip) :: i
    
    LOGICAL :: qOKL                   


    qOK = .false.

! !$OMP DO
!    do i=1,iNumberElectrons_G
      sdpi = sInv2rho * (  n2col * bxut  & 
             - sEta_G * spt2 / sKappa_G**2 * &
                          sF4EI ) & 
             - sKappa_G * spr / sgam * (1 + sEta_G * spt2) &
                 * n2col * bzut
!    end do
! !$OMP END DO

! Set the error flag and exit

    qOK = .true.

    !goto 2000 

    !1000 call Error_log('Error in equations:dppdz_i',tErrorLog_G)
    
    !print*,'Error in equations:dppdz_i'
    
    !2000 continue


  end subroutine dppdz_i_fT







!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dgamdz_fT(sx, sy, sz2, spr, spi, sgam, spt2, &
                      sF4ER, sF4EI, sdgam, qOK)

    implicit none


    real(kind=wp), intent(in) :: sx, sy, sz2, spr, spi, sgam, spt2, sF4ER, sF4EI
    real(kind=wp), intent(out) :: sdgam

    logical, intent(inout) :: qOK

    integer(kind=ip) :: i

    LOGICAL :: qOKL


    qOK = .false.

! !$OMP DO
!    do i=1,iNumberElectrons_G
      sdgam = -sRho_G * ( 1 + sEta_G * spt2 ) / sgam * 2_wp *   &
             ( spr * sF4ER + spi * sF4EI ) 
!    end do
! !$OMP END DO

    ! Set the error flag and exit

    qOK = .true.

    !goto 2000 

    !1000 call Error_log('Error in equations:dp2dz',tErrorLog_G)
    
    !print*,'Error in equations:dp2dz'

    !2000 continue


  end subroutine dgamdz_fT







  subroutine dxdz_fT(sx, sy, sz2, spr, spi, sgam, spt2, &
                    sdx, qOK)

    implicit none

!   Calculate dx/dz
!
!              Arguments:


    real(kind=wp), intent(in) :: sx, sy, sz2, spr, spi, sgam, spt2
    real(kind=wp), intent(out) :: sdx


!    real(kind=wp), intent(in) :: sy(:), Lj(:), nd
!    real(kind=wp), intent(inout) :: sb(:)
    logical, intent(inout) :: qOK

!              Local vars

    integer(kind=ip) :: i

    logical :: qOKL ! Local error flag


    qOK = .false.


! !$OMP DO
!    do i=1,iNumberElectrons_G
      sdx = 2_wp * sRho_G * sKappa_G / sqrt(sEta_G) * &
            (1 + sEta_G * spt2) / sgam *  &
            spr
!    end do
! !$OMP END DO

!    sdx = spr * Lj / nd
    

    qOK = .true.
    
    !goto 2000
    
    !1000 call Error_log('Error in equations:dxdz',tErrorLog_G)
    
    !2000 continue


  end subroutine dxdz_fT





  subroutine dydz_fT(sx, sy, sz2, spr, spi, sgam, spt2, &
                    sdy, qOK)

    implicit none

!   Calculate dy/dz
!
!              Arguments:


    real(kind=wp), intent(in) :: sx, sy, sz2, spr, spi, sgam, spt2
    real(kind=wp), intent(out) :: sdy

    logical, intent(inout) :: qOK

!              Local vars

    integer(kind=ip) :: i

    logical :: qOKL ! Local error flag


    qOK = .false.


! !$OMP DO
!    do i=1,iNumberElectrons_G
      sdy = - 2_wp * sRho_G * sKappa_G / sqrt(sEta_G) * &
            (1 + sEta_G * spt2) / sgam *  &
            spi
!    end do
! !$OMP END DO

!    sdy = - spi * Lj / nd


    qOK = .true.
    
    !goto 2000
    
    !1000 call Error_log('Error in equations:dydz',tErrorLog_G)
    
    !2000 continue


  end subroutine dydz_fT




  subroutine dz2dz_fT(sx, sy, sz2, spr, spi, sgam, spt2, &
                     sdz2, qOK)

    implicit none

!   Calculate dz2/dz
!
!              Arguments:


    real(kind=wp), intent(in) :: sx, sy, sz2, spr, spi, sgam, spt2
    real(kind=wp), intent(out) :: sdz2


!    real(kind=wp), intent(in) :: sy(:), Lj(:), nd
!    real(kind=wp), intent(inout) :: sb(:)
    logical, intent(inout) :: qOK

!              Local vars

    integer(kind=ip) :: i
    logical :: qOKL ! Local error flag


    qOK = .false.

! !$OMP DO
!    do i=1,iNumberElectrons_G
      sdz2 = spt2
!    end do
! !$OMP END DO

    qOK = .true.
    
    !goto 2000
    
    !1000 call Error_log('Error in equations:dz2dz',tErrorLog_G)
    
    !2000 continue

  end subroutine dz2dz_fT
  






  subroutine alct_e_srtcts(ar_sz)

    implicit none

! Allocate the arrays used in the calculation of
! the electron eqns  

    integer(kind=ip), intent(in) :: ar_sz

    allocate(sp2(ar_sz), sField4ElecReal(ar_sz), &
             sField4ElecImag(ar_sz))! , Lj(ar_sz))

!    allocate(bxu(ar_sz), byu(ar_sz), bzu(ar_sz))

  end subroutine alct_e_srtcts



  subroutine dalct_e_srtcts()

    implicit none

! Allocate the arrays used in the calculation of
! the electron eqns  

    deallocate(sp2, sField4ElecReal, &
             sField4ElecImag)! , Lj(ar_sz))

!    deallocate(bxu, byu, bzu)

  end subroutine dalct_e_srtcts



  subroutine adjUndPlace(szl)

    real(kind=wp) :: szl

      if (qUndEnds_G) then

        if (szl < 0) then

          print*, 'undulator section not recognised, sz < 0!!'
          stop

        else if (sZl <= sZFS) then 

          iUndPlace_G = iUndStart_G

        else if (sZl >= sZFE) then
 
          iUndPlace_G = iUndEnd_G

        else if ((sZl > sZFS) .and. (sZl < sZFE)) then

          iUndPlace_G = iUndMain_G

        else 

          print*, 'undulator section not recognised, sz > sZFE!!'
          stop

        end if

      else

        iUndPlace_G = iUndMain_G

      end if

  end subroutine adjUndPlace






end module equations2

