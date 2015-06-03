!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

module Equations


use paratype
use ArrayFunctions
use Globals


implicit none

contains

  subroutine dppdz_r(sInv2rho,ZOver2rho,salphaSq,&
    sField4ElecReal,nd,Lj,kbeta,sb,sy,dp2f,qOKL)


  	implicit none



    REAL(KIND=WP),INTENT(IN) :: sInv2rho
    REAL(KIND=WP),INTENT(IN) :: ZOver2rho,salphaSq
    REAL(KIND=WP),DIMENSION(:),ALLOCATABLE ,INTENT(IN):: sField4ElecReal
    REAL(KIND=WP),ALLOCATABLE ,INTENT(IN):: Lj(:), dp2f(:)
    REAL(KIND=WP),INTENT(IN) :: kbeta,nd
    REAL(KIND=WP),INTENT(IN) :: sy(:)
    REAL(KIND=WP),INTENT(OUT) :: sb(:)

    
    LOGICAL :: qOKL


    if (zUndType_G == 'curved') then

!     For curved pole undulator

        call PutValueInVector(iRe_PPerp_CG, &
                sInv2rho * ( cosh(Vector(iRe_X_CG,sy) * kx_und_G) * &
                sinh(Vector(iRe_Y_CG,sy) * ky_und_G) * sqrt(2.0_WP) &
                * sqrt(sEta_G) * Lj * Vector(iIm_PPerp_CG,sy)  &      
                *  cos(ZOver2rho) &
                / (sqrt(salphaSq) * ky_und_G) + &
                cosh(Vector(iRe_X_CG,sy) * kx_und_G) &
                * cosh(Vector(iRe_Y_CG,sy) * ky_und_G) * sin(ZOver2rho) - &
                sEta_G * Vector(iRe_Q_CG,sy) * salphaSq * sField4ElecReal), &
                sb, &
                qOKL)



    else if (zUndType_G == 'planepole')  then

!     Re(p_perp) equation for plane-poled undulator

        call PutValueInVector(iRe_PPerp_CG, &
                sInv2rho * (-sField4ElecReal*salphaSq * &
                sEta_G * Vector(iRe_Q_CG,sy) &
                + ((sqrt(6.0_WP) *sRho_G) /sqrt(salphaSq)) * &
                sinh(sInv2rho * Vector(iRe_Y_CG,sy) &
                * sqrt(sEta_G))  * cos(ZOver2rho) * Lj * &
                Vector(iIm_PPerp_CG,sy) + &
                cosh(sInv2rho * Vector(iRe_Y_CG,sy) * &
                sqrt(sEta_G)) *sin(ZOver2rho)), &
                sb,       &       
                qOKL)

    else 

!     "normal" PUFFIN case with no off-axis undulator
!     field variation

      if (qFocussing_G) then

        call PutValueInVector(iRe_PPerp_CG, &
                sInv2rho * (fy_G*n2col*sin(ZOver2rho) - &
                (salphaSq * sEta_G * Vector(iRe_Q_CG,sy) * &
                sField4ElecReal ) ) - & 
                nd / Lj * & ! focusing term
                (  ( kbeta**2 * Vector(iRe_X_CG,sy)) + (sEta_G / &
                ( 1.0_WP + (sEta_G * Vector(iRe_Q_CG,sy)) ) * &
                Vector(iRe_X_CG,sb) * dp2f ) ), &
                sb,       &     
                qOKL)

      else 

        call PutValueInVector(iRe_PPerp_CG, &
              sInv2rho * (fy_G* n2col * sin(ZOver2rho) - &
              ( salphaSq * sEta_G * Vector(iRe_Q_CG,sy) * &
              sField4ElecReal ) ), &
              sb,         &       
              qOKL)

      end if


    end if


  end subroutine dppdz_r


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






  subroutine dppdz_i(sInv2rho,ZOver2rho,salphaSq,sField4ElecImag,nd,Lj,kbeta,sb,sy,dp2f,qOKL)


  	implicit none



    REAL(KIND=WP),INTENT(IN) :: sInv2rho
    REAL(KIND=WP),INTENT(IN) :: ZOver2rho,salphaSq
    REAL(KIND=WP),DIMENSION(:),ALLOCATABLE ,INTENT(IN):: sField4ElecImag
    REAL(KIND=WP),ALLOCATABLE,INTENT(IN) :: Lj(:), dp2f(:)
    REAL(KIND=WP),INTENT(IN) :: kbeta,nd
    REAL(KIND=WP),INTENT(IN) :: sy(:)
    REAL(KIND=WP),INTENT(OUT) :: sb(:)
    LOGICAL :: qOKL                   




    if (zUndType_G == 'curved') then


!     For curved pole undulator

        call PutValueInVector(iIM_PPerp_CG, &
                sInv2rho * ( -1.0_WP * sqrt(2.0_WP) * sqrt(sEta_G) &
                * Lj * Vector(iRe_PPerp_CG,sy) &
                * cosh(Vector(iRe_X_CG,sy) * kx_und_G) &
                * sinh(Vector(iRe_Y_CG,sy) * ky_und_G) &
                * cos(ZOver2rho)  / (sqrt(salphaSq) * ky_und_G) &
                + sin(ZOver2rho) * kx_und_G/ky_und_G * &
                sinh(Vector(iRe_X_CG,sy) * kx_und_G) * &
                sinh(Vector(iRe_Y_CG,sy) * ky_und_G) & 
                - sEta_G * Vector(iRe_Q_CG,sy) * salphaSq * &
                sField4ElecImag), &
                sb, &
                qOKL)


    else if (zUndType_G == 'planepole') then 

!     Im(p_perp) equation for plane-poled undulator



        call PutValueInVector(iIM_PPerp_CG, &
                sInv2rho * (- sField4ElecImag * salphaSq * &
                sEta_G * Vector(iRe_Q_CG,sy)  &
                - ((sqrt(6.0_WP) * sRho_G)/ sqrt(salphaSq)) * &
                sinh(sInv2rho * Vector(iRe_Y_CG,sy) &
                * sqrt(sEta_G)) * cos(ZOver2rho) * Lj * &
                Vector(iRe_PPerp_CG,sy)) , &
                sb, &
                qOKL)

    else

!     "normal" PUFFIN case with no off-axis undulator
!     field variation


      if (qFocussing_G) then

        call PutValueInVector(iIm_PPerp_CG, &
                sInv2rho * (fx_G*n2col*cos(ZOver2rho) - &
                (salphaSq * sEta_G * Vector(iRe_Q_CG,sy) * &
                sField4ElecImag ) ) + &
                nd / Lj * & ! focusing term
                (  ( kbeta**2 * Vector(iRe_Y_CG,sy)) + (sEta_G / &
                ( 1.0_WP + (sEta_G * Vector(iRe_Q_CG,sy)) ) * &
                Vector(iRe_Y_CG,sb) * dp2f ) ), &
                sb,       &     
                qOKL)

      else

        call PutValueInVector(iIm_PPerp_CG, &
                      sInv2rho * (fx_G * n2col * cos(ZOver2rho) - &
                      ( salphaSq * sEta_G * Vector(iRe_Q_CG,sy) * &
                      sField4ElecImag ) ),&
                      sb,         &       
                      qOKL)

      end if


    end if

  end subroutine dppdz_i







!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine dp2dz(sInv2rho,ZOver2rho,salphaSq, &
                      sField4ElecImag,sField4ElecReal, &
                      nd,Lj,kbeta,sb,sy,dp2f,nb,qOKL)


  	implicit none



    REAL(KIND=WP),INTENT(IN) :: sInv2rho
    REAL(KIND=WP),INTENT(IN) :: ZOver2rho,salphaSq
    REAL(KIND=WP),DIMENSION(:),ALLOCATABLE ,INTENT(IN):: sField4ElecReal,sField4ElecImag
    REAL(KIND=WP),ALLOCATABLE,INTENT(IN) :: Lj(:), dp2f(:)
    REAL(KIND=WP),INTENT(IN) :: kbeta,nd,nb
    REAL(KIND=WP),INTENT(IN) :: sy(:)
    REAL(KIND=WP),INTENT(OUT) :: sb(:)   
    LOGICAL :: qOKL




    if (zUndType_G == 'curved') then


!     For curved pole undulator

        call PutValueInVector(iRe_Q_CG, &
                (4_WP * sRho_G / sEta_G) * Lj**2 * &
                ( (Vector(iRe_pPerp_CG,sy) * sField4ElecReal &
                + Vector(iIm_pPerp_CG,sy) * sField4ElecImag) * &
                Vector(iRe_Q_CG,sy) * sEta_G &
                + (1.0_WP/salphaSq) * (1 + sEta_G * Vector(iRe_Q_CG,sy)) &
                * sin(ZOver2rho) * &
                (Vector(iRe_pPerp_CG,sy) * cosh(Vector(iRe_X_CG,sy) &
                * kx_und_G) * cosh(Vector(iRe_Y_CG,sy) * ky_und_G) &
                + Vector(iIm_pPerp_CG,sy) * kx_und_G/ky_und_G * &
                sinh(Vector(iRe_X_CG,sy) * kx_und_G) * &
                sinh(Vector(iRe_Y_CG,sy) * ky_und_G))),&
                sb,&
                qOKL)



    else if (zUndType_G == 'planepole') then 

!     p2 equation for plane-poled undulator

 
        call PutValueInVector(iRe_Q_CG, &
                (4_WP * sRho_G / sEta_G) * Lj**2 * &
                ( (Vector(iRe_pPerp_CG,sy) * sField4ElecReal &
                + Vector(iIm_pPerp_CG,sy) * sField4ElecImag) * &
                Vector(iRe_Q_CG,sy) * sEta_G &
                + (1.0_WP/salphaSq) * (1 + sEta_G * Vector(iRe_Q_CG,sy)) &
                * cosh(sInv2rho * Vector(iRe_Y_CG,sy) * sqrt(sEta_G)) &
                * sin(ZOver2rho) *  Vector(iRe_pPerp_CG,sy)  ) , &
                sb,&
                qOKL)


 
    else

!     "normal" PUFFIN case with no off-axis undulator
!     field variation

      if (qFocussing_G) then

        call PutValueInVector(iRe_Q_CG, &
                2.0_WP * nb * Lj**2 * &
                ((sEta_G * Vector(iRe_Q_CG,sy) + 1.0_WP) &
                / salphaSq * n2col * &
                (Vector(iIm_pPerp_CG,sy) * fx_G*cos(ZOver2Rho) + &
                Vector(iRe_pPerp_CG,sy) * fy_G*sin(ZOver2rho)) + &
                sEta_G * Vector(iRe_Q_CG,sy) * &
                (Vector(iRe_pPerp_CG,sy)*sField4ElecReal + &
                Vector(iIm_pPerp_CG,sy)*sField4ElecImag)) &
                + dp2f, & 
                sb,&
                qOKL)

      else


        CALL PutValueInVector(iRe_Q_CG, &
                2.0_WP * nb * Lj**2.0_WP * &
                ((sEta_G * Vector(iRe_Q_CG,sy) + 1.0_WP)/ salphaSq * n2col * &
                (Vector(iIm_pPerp_CG,sy) * fx_G*cos(ZOver2Rho) + &
                Vector(iRe_pPerp_CG,sy) * fy_G*sin(ZOver2rho)) +&
                sEta_G * Vector(iRe_Q_CG,sy) *&
                (Vector(iRe_pPerp_CG,sy)*sField4ElecReal +&
                Vector(iIm_pPerp_CG,sy)*sField4ElecImag)), &
                sb,&
                qOKL)


      end if

    end if



  end subroutine dp2dz







  subroutine dxdz(sy, Lj, sb, qOK)

!   Calculate dx/dz
!
!              Arguments:

    real(kind=wp), intent(in) :: sy(:), Lj(:)
    real(kind=wp), intent(inout) :: sb(:)
    logical, intent(inout) :: qOK

!              Local vars

    logical :: qOKL ! Local error flag


    qOK = .false.

    CALL PutValueInVector(iRe_X_CG,&
                Vector(iRe_pPerp_CG,sy) * Lj / nd, &
                sb,&          
                qOKL)    

    if (qOKL = .false.) goto 1000  ! Error checking


    qOK = .true.
    
    goto 2000
    
    1000 call Error_log('Error in equations:dz2dz',tErrorLog_G)
    
    2000 continue


  end subroutine dxdz





  subroutine dydz(sy, Lj, sb, qOK)

!   Calculate dz2/dz
!
!              Arguments:

    real(kind=wp), intent(in) :: sy(:), Lj(:)
    real(kind=wp), intent(inout) :: sb(:)
    logical, intent(inout) :: qOK

!              Local vars

    logical :: qOKL ! Local error flag


    qOK = .false.

    CALL PutValueInVector(iRe_Y_CG, &
                          -Vector(iIm_pPerp_CG,sy) * Lj / nd, &
                          sb,&          
                          qOKL)

    if (qOKL = .false.) goto 1000  ! Error checking


    qOK = .true.
    
    goto 2000
    
    1000 call Error_log('Error in equations:dz2dz',tErrorLog_G)
    
    2000 continue


  end subroutine dydz




  subroutine dz2dz(sy, sb, qOK)

!   Calculate dz2/dz
!
!              Arguments:

    real(kind=wp), intent(in) :: sy(:)
    real(kind=wp), intent(inout) :: sb(:)
    logical, intent(inout) :: qOK

!              Local vars

    logical :: qOKL ! Local error flag


    qOK = .false.

    call PutValueInVector(iRe_Z2_CG,&
                          Vector(iRe_Q_CG,sy),&
                          sb,&          
                          qOKL)

    if (qOKL = .false.) goto 1000  ! Error checking


    qOK = .true.
    
    goto 2000
    
    1000 call Error_log('Error in equations:dz2dz',tErrorLog_G)
    
    2000 continue

  end subroutine dz2dz
  






  subroutine caldp2f(kbeta, sy, sb, dp2f)

    real(kind=wp), intent(in) :: kbeta, sy(:), sb(:)
    real(kind=wp), intent(out) :: dp2f(:)

    if (qFocussing_G) then

        dp2f = -(kbeta**2.0_WP) * &   ! New focusing term
               (1.0_WP + (sEta_G*Vector(iRe_Q_CG,sy))) * &
               ((Vector(iRe_X_CG,sy)*Vector(iRe_X_CG,sb)) + & 
               (Vector(iRe_Y_CG,sy)*Vector(iRe_Y_CG,sb))) / &
               (1.0_WP + sEta_G * ( (Vector(iRe_X_CG,sb))**2.0_WP  + &
               (Vector(iRe_Y_CG,sb))**2.0_WP ) )

    else 

        dp2f=0.0_WP

    end if

  end subroutine caldp2f


end module equations