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



real(kind=wp), allocatable :: dp2f(:), sField4ElecReal(:), &
                              sField4ElecImag(:) !, &
                              !Lj(:)



contains

!  subroutine dppdz_r(sInv2rho,ZOver2rho,salphaSq,&
!    sField4ElecReal,nd,Lj,kbeta,sb,sy,dp2f,qOK)

  subroutine dppdz_r_f(sx, sy, sz2, spr, spi, sp2, &
                       sdx, sdy, sdz2, sdpr, sdpi, sdp2, &
                       Lj, qOK)

  	implicit none


    real(kind=wp), intent(in) :: sx(:), sy(:), sz2(:), spr(:), spi(:), sp2(:)
    real(kind=wp), intent(in) :: sdx(:), sdy(:), sdz2(:), sdpi(:), sdp2(:)
    real(kind=wp), intent(in) :: Lj(:)
    real(kind=wp), intent(out) :: sdpr(:)

!    REAL(KIND=WP),INTENT(IN) :: sInv2rho
!    REAL(KIND=WP),INTENT(IN) :: ZOver2rho,salphaSq
!    REAL(KIND=WP),DIMENSION(:),ALLOCATABLE ,INTENT(IN):: sField4ElecReal
!    REAL(KIND=WP),ALLOCATABLE ,INTENT(IN):: Lj(:), dp2f(:)
!    REAL(KIND=WP),INTENT(IN) :: kbeta,nd
!    REAL(KIND=WP),INTENT(IN) :: sy(:)
!    REAL(KIND=WP),INTENT(OUT) :: sb(:)
    logical, intent(inout) :: qOK

    
    LOGICAL :: qOKL

    qOK = .false.

    if (zUndType_G == 'curved') then

!     For curved pole undulator

        sdpr = sInv2rho * ( cosh(sx * kx_und_G) * &
                        sinh(sy * ky_und_G) * sqrt(2.0_WP) &
                        * sqrt(sEta_G) * Lj * spi  &      
                        *  cos(ZOver2rho) &
                        / (sqrt(salphaSq) * ky_und_G) + &
                        cosh(sx * kx_und_G) &
                        * cosh(sy * ky_und_G) * sin(ZOver2rho) - &
                        sEta_G * sp2 * salphaSq * sField4ElecReal)

        

    else if (zUndType_G == 'planepole')  then

!     Re(p_perp) equation for plane-poled undulator

        sdpr = sInv2rho * (-sField4ElecReal*salphaSq * &
                        sEta_G * sp2 &
                        + ((sqrt(6.0_WP) *sRho_G) /sqrt(salphaSq)) * &
                        sinh(sInv2rho * sy &
                        * sqrt(sEta_G))  * cos(ZOver2rho) * Lj * &
                        spi + &
                        cosh(sInv2rho * sy * &
                        sqrt(sEta_G)) *sin(ZOver2rho))

    else 

!     "normal" PUFFIN case with no off-axis undulator
!     field variation

      if (qFocussing_G) then

! !$OMP PARALLEL WORKSHARE

        sdpr = sInv2rho * (fy_G*n2col*sin(ZOver2rho) - &
                        (salphaSq * sEta_G * sp2 * &
                        sField4ElecReal ) ) - & 
                        nd / Lj * & ! focusing term
                        (  ( kbeta**2 * sx) + (sEta_G / &
                        ( 1.0_WP + (sEta_G * sp2) ) * &
                        sdx * dp2f ) )

! !$OMP END PARALLEL WORKSHARE

      else 


        sdpr = sInv2rho * (fy_G* n2col * sin(ZOver2rho) - &
              ( salphaSq * sEta_G * sp2 * &
              sField4ElecReal ) )


      end if


    end if

    ! Set the error flag and exit

    qOK = .true.

    goto 2000 

    1000 call Error_log('Error in equations:dppdz_r',tErrorLog_G)
    
    print*,'Error in equations:dppdz_r'

    2000 continue

  end subroutine dppdz_r_f


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






  subroutine dppdz_i_f(sx, sy, sz2, spr, spi, sp2, &
                     sdx, sdy, sdz2, sdpr, sdpi, sdp2, &
                     Lj, qOK)

    implicit none


    real(kind=wp), intent(in) :: sx(:), sy(:), sz2(:), spr(:), spi(:), sp2(:)
    real(kind=wp), intent(in) :: sdx(:), sdy(:), sdz2(:), sdpr(:), sdp2(:)
    real(kind=wp), intent(in) :: Lj(:)
    real(kind=wp), intent(out) :: sdpi(:)


!  	implicit none



!    REAL(KIND=WP),INTENT(IN) :: sInv2rho
!    REAL(KIND=WP),INTENT(IN) :: ZOver2rho,salphaSq
!    REAL(KIND=WP),DIMENSION(:),ALLOCATABLE ,INTENT(IN):: sField4ElecImag
!    REAL(KIND=WP),ALLOCATABLE,INTENT(IN) :: Lj(:), dp2f(:)
!    REAL(KIND=WP),INTENT(IN) :: kbeta,nd
!    REAL(KIND=WP),INTENT(IN) :: sy(:)
!    REAL(KIND=WP),INTENT(OUT) :: sb(:)
    logical, intent(inout) :: qOK

    LOGICAL :: qOKL                   


    qOK = .false.


    if (zUndType_G == 'curved') then


!     For curved pole undulator

        sdpi = sInv2rho * ( -1.0_WP * sqrt(2.0_WP) * sqrt(sEta_G) &
                        * Lj * spr &
                        * cosh(sx * kx_und_G) &
                        * sinh(sy * ky_und_G) &
                        * cos(ZOver2rho)  / (sqrt(salphaSq) * ky_und_G) &
                        + sin(ZOver2rho) * kx_und_G/ky_und_G * &
                        sinh(sx * kx_und_G) * &
                        sinh(sy * ky_und_G) & 
                        - sEta_G * sp2 * salphaSq * &
                        sField4ElecImag)


    else if (zUndType_G == 'planepole') then 

!     Im(p_perp) equation for plane-poled undulator



        sdpi = sInv2rho * (- sField4ElecImag * salphaSq * &
                        sEta_G * sp2  &
                        - ((sqrt(6.0_WP) * sRho_G)/ sqrt(salphaSq)) * &
                        sinh(sInv2rho * sy &
                        * sqrt(sEta_G)) * cos(ZOver2rho) * Lj * &
                        spr)


    else

!     "normal" PUFFIN case with no off-axis undulator
!     field variation


      if (qFocussing_G) then

! !$OMP PARALLEL WORKSHARE

        sdpi = sInv2rho * (fx_G*n2col*cos(ZOver2rho) - &
                        (salphaSq * sEta_G * sp2 * &
                        sField4ElecImag ) ) + &
                        nd / Lj * & ! focusing term
                        (  ( kbeta**2 * sy) + (sEta_G / &
                        ( 1.0_WP + (sEta_G * sp2) ) * &
                        sdy * dp2f ) )

! !$OMP END PARALLEL WORKSHARE

      else


        sdpi = sInv2rho * (fx_G * n2col * cos(ZOver2rho) - &
                      ( salphaSq * sEta_G * sp2 * &
                      sField4ElecImag ) )

      end if


    end if

    ! Set the error flag and exit

    qOK = .true.

    goto 2000 

    1000 call Error_log('Error in equations:dppdz_i',tErrorLog_G)
    
    print*,'Error in equations:dppdz_i'
    
    2000 continue


  end subroutine dppdz_i_f







!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dp2dz_f(sx, sy, sz2, spr, spi, sp2, &
                     sdx, sdy, sdz2, sdpr, sdpi, sdp2, &
                     Lj, qOK)

    implicit none


    real(kind=wp), intent(in) :: sx(:), sy(:), sz2(:), spr(:), spi(:), sp2(:)
    real(kind=wp), intent(in) :: sdx(:), sdy(:), sdz2(:), sdpr(:), sdpi(:)
    real(kind=wp), intent(in) :: Lj(:)
    real(kind=wp), intent(out) :: sdp2(:)


!  	implicit none
!
!
!
!    REAL(KIND=WP),INTENT(IN) :: sInv2rho
!    REAL(KIND=WP),INTENT(IN) :: ZOver2rho,salphaSq
!    REAL(KIND=WP),DIMENSION(:),ALLOCATABLE ,INTENT(IN):: sField4ElecReal,sField4ElecImag
!    REAL(KIND=WP),ALLOCATABLE,INTENT(IN) :: Lj(:), dp2f(:)
!    REAL(KIND=WP),INTENT(IN) :: kbeta,nd,nb
!    REAL(KIND=WP),INTENT(IN) :: sy(:)
!    REAL(KIND=WP),INTENT(OUT) :: sb(:)   
    logical, intent(inout) :: qOK

    LOGICAL :: qOKL


    qOK = .false.

    if (zUndType_G == 'curved') then


!     For curved pole undulator

        sdp2 = (4_WP * sRho_G / sEta_G) * Lj**2 * &
                        ( (spr * sField4ElecReal &
                        + spi * sField4ElecImag) * &
                        sp2 * sEta_G &
                        + (1.0_WP/salphaSq) * (1 + sEta_G * sp2) &
                        * sin(ZOver2rho) * &
                        (spr * cosh(sx &
                        * kx_und_G) * cosh(sy * ky_und_G) &
                        + spi * kx_und_G/ky_und_G * &
                        sinh(sx * kx_und_G) * &
                        sinh(sy * ky_und_G)))


    else if (zUndType_G == 'planepole') then 

!     p2 equation for plane-poled undulator

 
        sdp2 = (4_WP * sRho_G / sEta_G) * Lj**2 * &
                        ( (spr * sField4ElecReal &
                        + spi * sField4ElecImag) * &
                        sp2 * sEta_G &
                        + (1.0_WP/salphaSq) * (1 + sEta_G * sp2) &
                        * cosh(sInv2rho * sy * sqrt(sEta_G)) &
                        * sin(ZOver2rho) *  spr  )

 
    else

!     "normal" PUFFIN case with no off-axis undulator
!     field variation

      if (qFocussing_G) then

! !$OMP PARALLEL WORKSHARE

        sdp2 = 2.0_WP * nb * Lj**2 * &
                        ((sEta_G * sp2 + 1.0_WP) &
                        / salphaSq * n2col * &
                        (spi * fx_G*cos(ZOver2Rho) + &
                        spr * fy_G*sin(ZOver2rho)) + &
                        sEta_G * sp2 * &
                        (spr*sField4ElecReal + &
                        spi*sField4ElecImag)) &
                        + dp2f

! !$OMP END PARALLEL WORKSHARE

      else


        sdp2 = 2.0_WP * nb * Lj**2.0_WP * &
                ((sEta_G * sp2 + 1.0_WP)/ salphaSq * n2col * &
                (spi * fx_G*cos(ZOver2Rho) + &
                spr * fy_G*sin(ZOver2rho)) + &
                sEta_G * sp2 * &
                (spr * sField4ElecReal + &
                spi * sField4ElecImag))

      end if

    end if

    ! Set the error flag and exit

    qOK = .true.

    goto 2000 

    1000 call Error_log('Error in equations:dp2dz',tErrorLog_G)
    
    print*,'Error in equations:dp2dz'

    2000 continue


  end subroutine dp2dz_f







  subroutine dxdz_f(sx, sy, sz2, spr, spi, sp2, &
                     sdx, sdy, sdz2, sdpr, sdpi, sdp2, &
                     Lj, qOK)

    implicit none

!   Calculate dx/dz
!
!              Arguments:


    real(kind=wp), intent(in) :: sx(:), sy(:), sz2(:), spr(:), spi(:), sp2(:)
    real(kind=wp), intent(in) :: sdy(:), sdz2(:), sdpr(:), sdpi(:), sdp2(:)
    real(kind=wp), intent(in) :: Lj(:)
    real(kind=wp), intent(out) :: sdx(:)


!    real(kind=wp), intent(in) :: sy(:), Lj(:), nd
!    real(kind=wp), intent(inout) :: sb(:)
    logical, intent(inout) :: qOK

!              Local vars

    logical :: qOKL ! Local error flag


    qOK = .false.


    sdx = spr * Lj / nd


    qOK = .true.
    
    goto 2000
    
    1000 call Error_log('Error in equations:dxdz',tErrorLog_G)
    
    2000 continue


  end subroutine dxdz_f





  subroutine dydz_f(sx, sy, sz2, spr, spi, sp2, &
                     sdx, sdy, sdz2, sdpr, sdpi, sdp2, &
                     Lj, qOK)

    implicit none

!   Calculate dy/dz
!
!              Arguments:


    real(kind=wp), intent(in) :: sx(:), sy(:), sz2(:), spr(:), spi(:), sp2(:)
    real(kind=wp), intent(in) :: sdx(:), sdz2(:), sdpr(:), sdpi(:), sdp2(:)
    real(kind=wp), intent(in) :: Lj(:)
    real(kind=wp), intent(out) :: sdy(:)


!    real(kind=wp), intent(in) :: sy(:), Lj(:), nd
!    real(kind=wp), intent(inout) :: sb(:)
    logical, intent(inout) :: qOK

!              Local vars

    logical :: qOKL ! Local error flag


    qOK = .false.

    sdy = - spi * Lj / nd


    qOK = .true.
    
    goto 2000
    
    1000 call Error_log('Error in equations:dydz',tErrorLog_G)
    
    2000 continue


  end subroutine dydz_f




  subroutine dz2dz_f(sx, sy, sz2, spr, spi, sp2, &
                     sdx, sdy, sdz2, sdpr, sdpi, sdp2, &
                     Lj, qOK)

    implicit none

!   Calculate dz2/dz
!
!              Arguments:


    real(kind=wp), intent(in) :: sx(:), sy(:), sz2(:), spr(:), spi(:), sp2(:)
    real(kind=wp), intent(in) :: sdx(:), sdy(:), sdpr(:), sdpi(:), sdp2(:)
    real(kind=wp), intent(in) :: Lj(:)
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
  






  subroutine caldp2f_f(sx, sy, sdx, sdy, sp2, kbeta, qOK)

    real(kind=wp), intent(in) :: sx(:), sy(:), sdx(:), sdy(:), sp2(:)
    real(kind=wp), intent(in) :: kbeta

    logical, intent(inout) :: qOK

    qOK = .false.

    if (qFocussing_G) then

        dp2f = -(kbeta**2.0_WP) * &   ! New focusing term
               (1.0_WP + (sEta_G * sp2  ) ) * &
               ( ( sx * sdx  ) + & 
               ( sy * sdy  ) ) / &
               (1.0_WP + sEta_G * ( ( sdx )**2.0_WP  + &
               sdy**2.0_WP ) )

    else 

        dp2f=0.0_WP

    end if

    qOK = .true.
    
    goto 2000
    
    1000 call Error_log('Error in equations:caldp2f',tErrorLog_G)
    
    2000 continue

  end subroutine caldp2f_f




  subroutine alct_e_srtcts(ar_sz)

    implicit none

! Allocate the arrays used in the calculation of
! the electron eqns  

    integer(kind=ip), intent(in) :: ar_sz

    allocate(dp2f(ar_sz), sField4ElecReal(ar_sz), &
             sField4ElecImag(ar_sz))! , Lj(ar_sz))


  end subroutine alct_e_srtcts



  subroutine dalct_e_srtcts()

    implicit none

! Allocate the arrays used in the calculation of
! the electron eqns  

    deallocate(dp2f, sField4ElecReal, &
             sField4ElecImag)! , Lj(ar_sz))


  end subroutine dalct_e_srtcts


end module equations