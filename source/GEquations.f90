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


        sdpr = sInv2rho * (  n2col * cosh(kx_und_G * sx) * cosh(ky_und_G * sy) &
                            * sin(ZOver2rho)  &
                            - sEta_G * sp2 / sKappa_G**2 *    &
                            sField4ElecReal )  +      &
               ( sKappa_G * spi / sgam * (1 + sEta_G * sp2) * &
                     sqrt(sEta_G) * sInv2rho / kx_und_G  *    & 
                     cosh(sx * kx_und_G) * sinh(sy * ky_und_G) *  &
                     n2col * cos(ZOver2rho) )
    


!        sdpr = sInv2rho * ( cosh(sx * kx_und_G) * &
!                        sinh(sy * ky_und_G) * sqrt(2.0_WP) &
!                        * sqrt(sEta_G) * Lj * spi  &      
!                        *  cos(ZOver2rho) &
!                        / (sqrt(salphaSq) * ky_und_G) + &
!                        cosh(sx * kx_und_G) &
!                        * cosh(sy * ky_und_G) * sin(ZOver2rho) - &
!                        sEta_G * sp2 / sKappa_G**2 * sField4ElecReal)
!
        

    else if (zUndType_G == 'planepole')  then

!     Re(p_perp) equation for plane-poled undulator

        sdpr = sInv2rho * ( n2col * cosh( sqrt(sEta_G) * sInv2rho * sy) & 
                            * sin(ZOver2rho) & 
                            - sEta_G * sp2 / sKappa_G**2 *    &
                            sField4ElecReal ) & 
               + ( sKappa_G * spi / sgam * (1 + sEta_G * sp2)  &
                   * sinh( sqrt(sEta_G) * sInv2rho * sy ) &
                   * n2col * cos(ZOver2rho) )



!        sdpr = sInv2rho * (-sField4ElecReal*salphaSq * &
!                        sEta_G * sp2 &
!                        + ((sqrt(6.0_WP) *sRho_G) /sqrt(salphaSq)) * &
!                        sinh(sInv2rho * sy &
!                        * sqrt(sEta_G))  * cos(ZOver2rho) * Lj * &
!                        spi + &
!                        cosh(sInv2rho * sy * &
!                        sqrt(sEta_G)) *sin(ZOver2rho))

    else if (zUndType_G == 'helical') then

        sdpr = sInv2rho * ( n2col * sin(ZOver2rho)  & 
                            - sEta_G * sp2 / sKappa_G**2 *    &
                            sField4ElecReal ) & 
               + ( sKappa_G * spi / sgam * (1 + sEta_G * sp2) &
                   * sqrt(sEta_G) * sInv2rho * n2col * (     &
                    -sx * sin( ZOver2rho )  + sy * cos( ZOver2rho ) ) )

    else


!     "normal" PUFFIN case with no off-axis undulator
!     field variation

!      if (qFocussing_G) then

!        sdpr = sInv2rho * (fy_G*n2col*sin(ZOver2rho) - &
!                        (salphaSq * sEta_G * sp2 * &
!                        sField4ElecReal ) ) - & 
!                        nd / Lj * & ! focusing term
!                        (  ( kbeta**2 * sx) + (sEta_G / &
!                        ( 1.0_WP + (sEta_G * sp2) ) * &
!                        sdx * dp2f ) )
!
!      else 


        if (sZ <= sZFS) then 

          szt = sZ - sZ0
          szt = szt / 2_wp / sRho_G

          sdpr =   -2_wp * n2col0 * sInv2rho / 8_wp * sin(szt / 8_wp) * &
                    cos(szt / 8_wp) * cos(szt)    &
                  +  sInv2rho * n2col0 * sin(szt/8_wp)**2_wp  * sin(szt) &
                 - &
                ( sEta_G * sp2 / sKappa_G**2 * &
                sField4ElecReal *  sInv2rho ) 

        else if (sZ >= sZFE) then
 
          szt = sZ - sZFE
          szt = szt / 2_wp / sRho_G

          sdpr =    sInv2rho * n2col0 / 8_wp * sin(szt / 4_wp) * &
                    cos(szt)    &
                  +  sInv2rho * n2col0 * cos(szt/8_wp)**2_wp  * sin(szt) &
                 - &
                ( sEta_G * sp2 / sKappa_G**2 * &
                sField4ElecReal *  sInv2rho )     


        else 

          sdpr = sInv2rho * (fy_G*n2col *sin(ZOver2rho) - &
                ( sEta_G * sp2 / sKappa_G**2 * &
                sField4ElecReal ) )

        end if

!      end if


    end if

    ! Set the error flag and exit

    qOK = .true.

    goto 2000 

    1000 call Error_log('Error in equations:dppdz_r',tErrorLog_G)
    
    print*,'Error in equations:dppdz_r'

    2000 continue

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


        sdpi = sInv2rho * ( n2col * kx_und_G / ky_und_G * &
                            sinh(kx_und_G * sx) * sinh(ky_und_G * sy) &
                            * sin(ZOver2rho)  &
                            - sEta_G * sp2 / sKappa_G**2 *    &
                            sField4ElecReal )  -      &
               ( sKappa_G * spr / sgam * (1 + sEta_G * sp2) * &
                     sqrt(sEta_G) * sInv2rho / kx_und_G  *    & 
                     cosh(sx * kx_und_G) * sinh(sy * ky_und_G) *  &
                     n2col * cos(ZOver2rho) )
    



!        sdpi = sInv2rho * ( -1.0_WP * sqrt(2.0_WP) * sqrt(sEta_G) &
!                        * Lj * spr &
!                        * cosh(sx * kx_und_G) &
!                        * sinh(sy * ky_und_G) &
!                        * cos(ZOver2rho)  / (sqrt(salphaSq) * ky_und_G) &
!                        + sin(ZOver2rho) * kx_und_G/ky_und_G * &
!                        sinh(sx * kx_und_G) * &
!                        sinh(sy * ky_und_G) & 
!                        - sEta_G * sp2 * salphaSq * &
!                        sField4ElecImag)


    else if (zUndType_G == 'planepole') then 

!     Im(p_perp) equation for plane-poled undulator



        sdpi = sInv2rho * ( 0_ip  &
                            - sEta_G * sp2 / sKappa_G**2 *    &
                            sField4ElecReal ) & 
               - ( sKappa_G * spr / sgam * (1 + sEta_G * sp2) &
                   * sinh( sqrt(sEta_G) * sInv2rho * sy ) &
                   * n2col * cos(ZOver2rho) )





!        sdpi = sInv2rho * (- sField4ElecImag * salphaSq * &
!                        sEta_G * sp2  &
!                        - ((sqrt(6.0_WP) * sRho_G)/ sqrt(salphaSq)) * &
!                        sinh(sInv2rho * sy &
!                        * sqrt(sEta_G)) * cos(ZOver2rho) * Lj * &
!                        spr)

    else if (zUndType_G == 'helical') then

        sdpi = sInv2rho * (  n2col * cos(ZOver2rho)  & 
                            - sEta_G * sp2 / sKappa_G**2 *    &
                            sField4ElecReal ) & 
               - ( sKappa_G * spr / sgam * (1 + sEta_G * sp2) &
                   * sqrt(sEta_G) * sInv2rho * n2col * (   &
                    -sx * sin( ZOver2rho )  + sy * cos( ZOver2rho ) ) )


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


        if (sZ <= sZFS) then 


!  sdpr =   -2_wp * sInv2rho / 8_wp * sin(ZOver2rho / 8_wp) * &
!                      cos(ZOver2rho / 8_wp) * cos(ZOver2rho)    &
!                  +  sInv2rho * sin(ZOver2rho/8_wp)**2_wp  * sin(ZOver2rho) &
!                 - &
!                ( sEta_G * sp2 / sKappa_G**2 * &
!                sField4ElecReal *  sInv2rho ) 


          szt = sZ - sZ0
          szt = szt / 2_wp / sRho_G


!          sdpi = sInv2rho * (fx_G * n2col0 * &
!                    2_wp / 8_wp *  sin(szt / 8_wp) * &
!                   cos(szt / 8_wp) * sin(szt)  &
!                  +   n2col0 * sin(szt/8_wp)**2_wp  * cos(szt)     &
!                  - &
!                  ( sEta_G * sp2 / sKappa_G**2 * &
!                  sField4ElecImag ) )

          sdpi = sInv2rho * (fx_G * n2col0 * &
                    sin(szt/8_wp)**2_wp  * cos(szt)     &
                  - &
                  ( sEta_G * sp2 / sKappa_G**2 * &
                  sField4ElecImag ) )


        else if (sZ >= sZFE) then

          szt = sZ - sZFE
          szt = szt / 2_wp / sRho_G

!          sdpi = sInv2rho * (fx_G * n2col0 * &
!                    - 1_wp / 8_wp *  sin(szt / 4_wp) * &
!                    sin(szt)  &
!                  +   n2col0 * cos(szt/8_wp)**2_wp  * cos(szt)     &
!                  - &
!                  ( sEta_G * sp2 / sKappa_G**2 * &
!                  sField4ElecImag ) )

          sdpi = sInv2rho * (fx_G * n2col0 * &
                    cos(szt/8_wp)**2_wp  * cos(szt)     &
                  - &
                  ( sEta_G * sp2 / sKappa_G**2 * &
                  sField4ElecImag ) )

        else

          sdpi = sInv2rho * (fx_G * n2col * cos(ZOver2rho) - &
                        ( sEta_G * sp2 / sKappa_G**2 * &
                        sField4ElecImag ) )


        end if

!      end if


    end if

    ! Set the error flag and exit

    qOK = .true.

    goto 2000 

    1000 call Error_log('Error in equations:dppdz_i',tErrorLog_G)
    
    print*,'Error in equations:dppdz_i'
    
    2000 continue


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

    

    sdgam = -sRho_G * ( 1 + sEta_G * sp2 ) / sgam * 2_wp *   &
           ( spr * sField4ElecReal + spi * sField4ElecImag ) 

    ! Set the error flag and exit

    qOK = .true.

    goto 2000 

    1000 call Error_log('Error in equations:dp2dz',tErrorLog_G)
    
    print*,'Error in equations:dp2dz'

    2000 continue


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


    sdx = 2 * sRho_G * sKappa_G / sqrt(sEta_G) * &
          (1 + sEta_G * sp2) / sgam *  &
          spr


!    sdx = spr * Lj / nd
    

    qOK = .true.
    
    goto 2000
    
    1000 call Error_log('Error in equations:dxdz',tErrorLog_G)
    
    2000 continue


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


    sdy = - 2 * sRho_G * sKappa_G / sqrt(sEta_G) * &
          (1 + sEta_G * sp2) / sgam *  &
          spi


!    sdy = - spi * Lj / nd


    qOK = .true.
    
    goto 2000
    
    1000 call Error_log('Error in equations:dydz',tErrorLog_G)
    
    2000 continue


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

    sdz2 = sp2


    qOK = .true.
    
    goto 2000
    
    1000 call Error_log('Error in equations:dz2dz',tErrorLog_G)
    
    2000 continue

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


end module equations

