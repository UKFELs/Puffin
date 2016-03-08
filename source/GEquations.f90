!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

module Equations


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

  subroutine dppdz_r_f(sx, sy, sz2, spr, spi, sgam, &
                       sZ, sdpr, qOK)

  	implicit none


    real(kind=wp), intent(in) :: sx(:), sy(:), sz2(:), spr(:), spi(:), sgam(:), sZ
    real(kind=wp), intent(out) :: sdpr(:)

    real(kind=wp) :: szt

    logical, intent(inout) :: qOK

    
    LOGICAL :: qOKL

    qOK = .false.

    if (zUndType_G == 'curved') then

!     For curved pole undulator

!$OMP WORKSHARE

        sdpr = sInv2rho * (  n2col * cosh(kx_und_G * sx) * cosh(ky_und_G * sy) &
                            * sin(ZOver2rho)  &
                            - sEta_G * sp2 / sKappa_G**2 *    &
                            sField4ElecReal )  +      &
               ( sKappa_G * spi / sgam * (1 + sEta_G * sp2) * &
                     sqrt(sEta_G) * sInv2rho / kx_und_G  *    & 
                     cosh(sx * kx_und_G) * sinh(sy * ky_und_G) *  &
                     n2col * cos(ZOver2rho) )
    
!$OMP END WORKSHARE

    else if (zUndType_G == 'planepole')  then

!     Re(p_perp) equation for plane-poled undulator

!$OMP WORKSHARE

        sdpr = sInv2rho * ( n2col * cosh( sqrt(sEta_G) * sInv2rho * sy) & 
                            * sin(ZOver2rho) & 
                            - sEta_G * sp2 / sKappa_G**2 *    &
                            sField4ElecReal ) & 
               + ( sKappa_G * spi / sgam * (1 + sEta_G * sp2)  &
                   * sinh( sqrt(sEta_G) * sInv2rho * sy ) &
                   * n2col * cos(ZOver2rho) )

!$OMP END WORKSHARE

    else if (zUndType_G == 'helical') then

        if (iUndPlace_G == iUndStart_G) then 

          szt = sZ
          szt = szt / 2_wp / sRho_G

!$OMP WORKSHARE

          sdpr = sInv2rho * ( n2col0 * (- sin(szt / 8_wp) * &
                      cos(szt / 8_wp) * cos(szt) / 4_wp   &
                    +  sin(szt/8_wp)**2_wp  * sin(szt) )  &
                              - sEta_G * sp2 / sKappa_G**2 *    &
                              sField4ElecReal ) & 
                 + ( sKappa_G * spi / sgam * (1 + sEta_G * sp2) &
                     * sqrt(sEta_G) * sInv2rho * n2col0 * (     & 
                      sx * ( 1/32_wp * cos(szt/4_wp) * sin(szt) + &
                              1/4_wp * sin(szt/4_wp) * cos(szt) - & 
                              sin(szt/8_wp)**2 * sin(szt) )    + &
                      sy * ( -1/32_wp * cos(szt/4_wp) * cos(szt) + &
                              1/4_wp * sin(szt/4_wp) * sin(szt) + & 
                              sin(szt/8_wp)**2 * cos(szt) ) ) ) 

!$OMP END WORKSHARE

        else if (iUndPlace_G == iUndEnd_G) then
 
          szt = sZ - sZFE
          szt = szt / 2_wp / sRho_G

!$OMP WORKSHARE

          sdpr = sInv2rho * ( n2col * ( cos(szt / 8_wp) * &
                      sin(szt / 8_wp) * cos(szt)  / 4_wp  &
                    +  cos(szt/8_wp)**2_wp  * sin(szt) ) &
                              - sEta_G * sp2 / sKappa_G**2 *    &
                              sField4ElecReal ) & 
                 + ( sKappa_G * spi / sgam * (1 + sEta_G * sp2) &
                     * sqrt(sEta_G) * sInv2rho * n2col * (     & 
                      sx * ( -1/32_wp * cos(szt/4_wp) * sin(szt) - &
                              1/4_wp * sin(szt/4_wp) * cos(szt) - & 
                              cos(szt/8_wp)**2 * sin(szt) )    + &
                      sy * ( 1/32_wp * cos(szt/4_wp) * cos(szt) - &
                              1/4_wp * sin(szt/4_wp) * sin(szt) + & 
                              cos(szt/8_wp)**2 * cos(szt) ) ) ) 

!$OMP END WORKSHARE

        else if (iUndPlace_G == iUndMain_G) then

!$OMP WORKSHARE

          sdpr = sInv2rho * ( n2col * sin(ZOver2rho)  & 
                              - sEta_G * sp2 / sKappa_G**2 *    &
                              sField4ElecReal ) & 
                 + ( sKappa_G * spi / sgam * (1 + sEta_G * sp2) &
                     * sqrt(sEta_G) * sInv2rho * n2col * (     &
                      -sx * sin( ZOver2rho )  + sy * cos( ZOver2rho ) ) )

!$OMP END WORKSHARE

        end if



    else


!     "normal" PUFFIN case with no off-axis undulator
!     field variation

        if (iUndPlace_G == iUndStart_G) then 

          szt = sZ
          szt = szt / 2_wp / sRho_G

!$OMP WORKSHARE

          sdpr = sInv2rho * (  n2col0 * fy_G * (- sin(szt / 8_wp) * &
                    cos(szt / 8_wp) * cos(szt) / 4_wp   &
                  +  sin(szt/8_wp)**2_wp  * sin(szt) ) &
                 - &
                ( sEta_G * sp2 / sKappa_G**2 * &
                sField4ElecReal ) )

!$OMP END WORKSHARE

        else if (iUndPlace_G == iUndEnd_G) then
 
          szt = sZ - sZFE
          szt = szt / 2_wp / sRho_G

!$OMP WORKSHARE

          sdpr = sInv2rho * ( n2col0 * fy_G * ( cos(szt / 8_wp) * &
                    sin(szt / 8_wp) * cos(szt)  / 4_wp  &
                  +  cos(szt/8_wp)**2_wp  * sin(szt) ) &
                 - &
                ( sEta_G * sp2 / sKappa_G**2 * &
                sField4ElecReal *  sInv2rho )     )

!$OMP END WORKSHARE


        else if (iUndPlace_G == iUndMain_G) then

!$OMP WORKSHARE

!   Using 'shortcuts'
!        sdpr = sInv2rho * (fy_G * n2col *sin(ZOver2rho) - &
!              ( cf1_G * sp2 * &
!              sField4ElecReal ) )

          sdpr = sInv2rho * (fy_G*n2col *sin(ZOver2rho) - &
                ( sEta_G * sp2 / sKappa_G**2 * &
                sField4ElecReal ) )

!$OMP END WORKSHARE

        end if

    end if

    ! Set the error flag and exit

    qOK = .true.

    !goto 2000 

    !1000 call Error_log('Error in equations:dppdz_r',tErrorLog_G)
    
    !print*,'Error in equations:dppdz_r'

    !2000 continue

  end subroutine dppdz_r_f


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






  subroutine dppdz_i_f(sx, sy, sz2, spr, spi, sgam, sZ, &
                       sdpi, qOK)

    implicit none


    real(kind=wp), intent(in) :: sx(:), sy(:), sz2(:), spr(:), spi(:), sgam(:), sZ
    real(kind=wp), intent(out) :: sdpi(:)

    real(kind=wp) :: szt

    logical, intent(inout) :: qOK

    LOGICAL :: qOKL                   


    qOK = .false.


    if (zUndType_G == 'curved') then


!     For curved pole undulator

!$OMP WORKSHARE

        sdpi = sInv2rho * ( n2col * kx_und_G / ky_und_G * &
                            sinh(kx_und_G * sx) * sinh(ky_und_G * sy) &
                            * sin(ZOver2rho)  &
                            - sEta_G * sp2 / sKappa_G**2 *    &
                            sField4ElecReal )  -      &
               ( sKappa_G * spr / sgam * (1 + sEta_G * sp2) * &
                     sqrt(sEta_G) * sInv2rho / kx_und_G  *    & 
                     cosh(sx * kx_und_G) * sinh(sy * ky_und_G) *  &
                     n2col * cos(ZOver2rho) )

!$OMP END WORKSHARE

    else if (zUndType_G == 'planepole') then 

!     Im(p_perp) equation for plane-poled undulator


!$OMP WORKSHARE

        sdpi = sInv2rho * ( 0_wp  &
                            - sEta_G * sp2 / sKappa_G**2 *    &
                            sField4ElecReal ) & 
               - ( sKappa_G * spr / sgam * (1 + sEta_G * sp2) &
                   * sinh( sqrt(sEta_G) * sInv2rho * sy ) &
                   * n2col * cos(ZOver2rho) )

!$OMP END WORKSHARE


    else if (zUndType_G == 'helical') then

        if (iUndPlace_G == iUndStart_G) then 

          szt = sZ
          szt = szt / 2_wp / sRho_G

!$OMP WORKSHARE

        sdpi = sInv2rho * (  n2col0 * ( sin(szt / 8_wp) * &
                   cos(szt / 8_wp) * sin(szt) / 4_wp &
                  +   sin(szt/8_wp)**2_wp  * cos(szt) )  & 
                            - sEta_G * sp2 / sKappa_G**2 *    &
                            sField4ElecReal ) & 
               - ( sKappa_G * spr / sgam * (1 + sEta_G * sp2) &
                   * sqrt(sEta_G) * sInv2rho * n2col0 * (   &
                    sx * ( 1/32_wp * cos(szt/4_wp) * sin(szt) + &
                              1/4_wp * sin(szt/4_wp) * cos(szt) - & 
                              sin(szt/8_wp)**2 * sin(szt) )    + &
                      sy * ( -1/32_wp * cos(szt/4_wp) * cos(szt) + &
                              1/4_wp * sin(szt/4_wp) * sin(szt) + & 
                              sin(szt/8_wp)**2 * cos(szt) ) ) )

!$OMP END WORKSHARE

        else if (iUndPlace_G == iUndEnd_G) then

          szt = sZ - sZFE
          szt = szt / 2_wp / sRho_G

!$OMP WORKSHARE

        sdpi = sInv2rho * (  n2col * (- cos(szt / 8_wp) * &
                    sin(szt / 8_wp) * sin(szt) / 4_wp  &
                  +  cos(szt/8_wp)**2_wp  * cos(szt) )  & 
                            - sEta_G * sp2 / sKappa_G**2 *    &
                            sField4ElecReal ) & 
               - ( sKappa_G * spr / sgam * (1 + sEta_G * sp2) &
                   * sqrt(sEta_G) * sInv2rho * n2col * (   &
                    sx * ( -1/32_wp * cos(szt/4_wp) * sin(szt) - &
                              1/4_wp * sin(szt/4_wp) * cos(szt) - & 
                              cos(szt/8_wp)**2 * sin(szt) )    + &
                      sy * ( 1/32_wp * cos(szt/4_wp) * cos(szt) - &
                              1/4_wp * sin(szt/4_wp) * sin(szt) + & 
                              cos(szt/8_wp)**2 * cos(szt) ) ) )

!$OMP END WORKSHARE

        else if (iUndPlace_G == iUndMain_G) then

!$OMP WORKSHARE

        sdpi = sInv2rho * (  n2col * cos(ZOver2rho)  & 
                            - sEta_G * sp2 / sKappa_G**2 *    &
                            sField4ElecReal ) & 
               - ( sKappa_G * spr / sgam * (1 + sEta_G * sp2) &
                   * sqrt(sEta_G) * sInv2rho * n2col * (   &
                    -sx * sin( ZOver2rho )  + sy * cos( ZOver2rho ) ) )

!$OMP END WORKSHARE

        end if


    else


!     "normal" PUFFIN case with no off-axis undulator
!     field variation

!      if (qFocussing_G) then
!
!        sdpi = sInv2rho * (fx_G*n2col*cos(ZOver2rho) - &
!                        (salphaSq * sEta_G * sp2 * &
!                        sField4ElecImag ) ) + &
!                        nd / Lj * & ! focusing term
!                        (  ( kbeta**2 * sy) + (sEta_G / &
!                        ( 1.0_WP + (sEta_G * sp2) ) * &
!                        sdy * dp2f ) )
!
!
!      else

        if (iUndPlace_G == iUndStart_G) then 

          szt = sZ
          szt = szt / 2_wp / sRho_G

!$OMP WORKSHARE

          sdpi =   sInv2rho * ( n2col0 * fx_G * ( sin(szt / 8_wp) * &
                   cos(szt / 8_wp) * sin(szt) / 4_wp &
                  +   sin(szt/8_wp)**2_wp  * cos(szt) ) &
                  - &
                  ( sEta_G * sp2 / sKappa_G**2 * &
                  sField4ElecImag ) )

!$OMP END WORKSHARE

        else if (iUndPlace_G == iUndEnd_G) then

          szt = sZ - sZFE
          szt = szt / 2_wp / sRho_G

!$OMP WORKSHARE

          sdpi = sInv2rho * ( n2col0 * fx_G * (- cos(szt / 8_wp) * &
                    sin(szt / 8_wp) * sin(szt) / 4_wp  &
                  +  cos(szt/8_wp)**2_wp  * cos(szt) )  &
                  - &
                  ( sEta_G * sp2 / sKappa_G**2 * &
                  sField4ElecImag * sInv2rho) )

!$OMP END WORKSHARE

        else if (iUndPlace_G == iUndMain_G) then

!$OMP WORKSHARE

! Using 'shortcuts':
!        sdpi = sInv2rho * (fx_G * n2col * cos(ZOver2rho) - &
!                      ( cf1_G * sp2 * &
!                      sField4ElecImag ) )

          sdpi = sInv2rho * (fx_G * n2col * cos(ZOver2rho) - &
                        ( sEta_G * sp2 / sKappa_G**2 * &
                        sField4ElecImag ) )

!$OMP END WORKSHARE

        end if

    end if

    ! Set the error flag and exit

    qOK = .true.

    !goto 2000 

    !1000 call Error_log('Error in equations:dppdz_i',tErrorLog_G)
    
    !print*,'Error in equations:dppdz_i'
    
    !2000 continue


  end subroutine dppdz_i_f







!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dgamdz_f(sx, sy, sz2, spr, spi, sgam, &
                      sdgam, qOK)

    implicit none


    real(kind=wp), intent(in) :: sx(:), sy(:), sz2(:), spr(:), spi(:), sgam(:)
    real(kind=wp), intent(out) :: sdgam(:)

    logical, intent(inout) :: qOK

    LOGICAL :: qOKL


    qOK = .false.

!$OMP WORKSHARE

    sdgam = -sRho_G * ( 1 + sEta_G * sp2 ) / sgam * 2_wp *   &
           ( spr * sField4ElecReal + spi * sField4ElecImag ) 

!$OMP END WORKSHARE

    ! Set the error flag and exit

    qOK = .true.

    !goto 2000 

    !1000 call Error_log('Error in equations:dp2dz',tErrorLog_G)
    
    !print*,'Error in equations:dp2dz'

    !2000 continue


  end subroutine dgamdz_f







  subroutine dxdz_f(sx, sy, sz2, spr, spi, sgam, &
                    sdx, qOK)

    implicit none

!   Calculate dx/dz
!
!              Arguments:


    real(kind=wp), intent(in) :: sx(:), sy(:), sz2(:), spr(:), spi(:), sgam(:)
    real(kind=wp), intent(out) :: sdx(:)


!    real(kind=wp), intent(in) :: sy(:), Lj(:), nd
!    real(kind=wp), intent(inout) :: sb(:)
    logical, intent(inout) :: qOK

!              Local vars

    logical :: qOKL ! Local error flag


    qOK = .false.


!$OMP WORKSHARE

    sdx = 2 * sRho_G * sKappa_G / sqrt(sEta_G) * &
          (1 + sEta_G * sp2) / sgam *  &
          spr

!$OMP END WORKSHARE

!    sdx = spr * Lj / nd
    

    qOK = .true.
    
    !goto 2000
    
    !1000 call Error_log('Error in equations:dxdz',tErrorLog_G)
    
    !2000 continue


  end subroutine dxdz_f





  subroutine dydz_f(sx, sy, sz2, spr, spi, sgam, &
                    sdy, qOK)

    implicit none

!   Calculate dy/dz
!
!              Arguments:


    real(kind=wp), intent(in) :: sx(:), sy(:), sz2(:), spr(:), spi(:), sgam(:)
    real(kind=wp), intent(out) :: sdy(:)

    logical, intent(inout) :: qOK

!              Local vars

    logical :: qOKL ! Local error flag


    qOK = .false.


!$OMP WORKSHARE

    sdy = - 2 * sRho_G * sKappa_G / sqrt(sEta_G) * &
          (1 + sEta_G * sp2) / sgam *  &
          spi

!$OMP END WORKSHARE

!    sdy = - spi * Lj / nd


    qOK = .true.
    
    !goto 2000
    
    !1000 call Error_log('Error in equations:dydz',tErrorLog_G)
    
    !2000 continue


  end subroutine dydz_f




  subroutine dz2dz_f(sx, sy, sz2, spr, spi, sgam, &
                     sdz2, qOK)

    implicit none

!   Calculate dz2/dz
!
!              Arguments:


    real(kind=wp), intent(in) :: sx(:), sy(:), sz2(:), spr(:), spi(:), sgam(:)
    real(kind=wp), intent(out) :: sdz2(:)


!    real(kind=wp), intent(in) :: sy(:), Lj(:), nd
!    real(kind=wp), intent(inout) :: sb(:)
    logical, intent(inout) :: qOK

!              Local vars

    logical :: qOKL ! Local error flag


    qOK = .false.

!$OMP WORKSHARE

    sdz2 = sp2

!$OMP END WORKSHARE

    qOK = .true.
    
    !goto 2000
    
    !1000 call Error_log('Error in equations:dz2dz',tErrorLog_G)
    
    !2000 continue

  end subroutine dz2dz_f
  






  subroutine alct_e_srtcts(ar_sz)

    implicit none

! Allocate the arrays used in the calculation of
! the electron eqns  

    integer(kind=ip), intent(in) :: ar_sz

    allocate(sp2(ar_sz), sField4ElecReal(ar_sz), &
             sField4ElecImag(ar_sz))! , Lj(ar_sz))


  end subroutine alct_e_srtcts



  subroutine dalct_e_srtcts()

    implicit none

! Allocate the arrays used in the calculation of
! the electron eqns  

    deallocate(sp2, sField4ElecReal, &
             sField4ElecImag)! , Lj(ar_sz))


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






end module equations

