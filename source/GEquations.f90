MODULE Equations


USE paratype
USE ArrayFunctions
USE DerivsGlobals


IMPLICIT NONE

CONTAINS

  SUBROUTINE CALCULATE_PX(sInv2rho,ZOver2rho,salphaSq,sField4ElecReal,nd,Lj,kbeta,sb,sy,dp2f,qOKL)


  	IMPLICIT NONE



    REAL(KIND=WP) :: sInv2rho
    REAL(KIND=WP) :: ZOver2rho,salphaSq
    REAL(KIND=WP),DIMENSION(:),ALLOCATABLE :: sField4ElecReal
    REAL(KIND=WP),ALLOCATABLE :: Lj(:), dp2f(:)
    REAL(KIND=WP) :: kbeta,nd,kx,ky
    REAL(KIND=WP),INTENT(IN) :: sy(:)
    REAL(KIND=WP),INTENT(OUT) :: sb(:)
    LOGICAL :: qOKL

kx = SQRT(sEta_G/(8*sRho_G**2))
ky = SQRT(sEta_G/(8*sRho_G**2))
!original Puffin p_x equations
!        CALL PutValueInVector(iRe_PPerp_CG, &
!            sInv2rho * (fy_G*sin(ZOver2rho) - &
!            (salphaSq * sEta_G * Vector(iRe_Q_CG,sy) * &
!            sField4ElecReal ) ) - & 
!            nd / Lj * & ! New focusing term
!            (  ( kbeta**2 * Vector(iRe_X_CG,sy)) + (sEta_G / &
!            ( 1.0_WP + (sEta_G * Vector(iRe_Q_CG,sy)) ) * &
!            Vector(iRe_X_CG,sb) * dp2f ) ), &
!            sb,	      & 	  
!            qOKL)





!Curved poles p_x equation
        CALL PutValueInVector(iRe_PPerp_CG, &
            sInv2rho * (SQRT(2.0) * SQRT(sEta_G) * Lj * Vector(iIm_PPerp_CG,sy) * &
                ( 1 + Vector(iRe_X_CG,sy)**2 * kx**2 /2) * &
                Vector(iRe_Y_CG,sy) * ky * cos(ZOver2rho) &
             / (SQRT(salphaSq) * ky) + sin(ZOver2rho) &
              * ( 1 + Vector(iRe_X_CG,sy)**2 * kx**2)* &
              ( 1 + Vector(iRe_Y_CG,sy)**2 * ky**2) - sEta_G * Vector(iRe_Q_CG,sy) &
              * salphaSq * sField4ElecReal), &
            sb,       &       
            qOKL)


  END SUBROUTINE CALCULATE_PX


  SUBROUTINE CALCULATE_PY(sInv2rho,ZOver2rho,salphaSq,sField4ElecImag,nd,Lj,kbeta,sb,sy,dp2f,qOKL)


  	IMPLICIT NONE



    REAL(KIND=WP) :: sInv2rho
    REAL(KIND=WP) :: ZOver2rho,salphaSq
    REAL(KIND=WP),DIMENSION(:),ALLOCATABLE :: sField4ElecImag
    REAL(KIND=WP),ALLOCATABLE :: Lj(:), dp2f(:)
    REAL(KIND=WP) :: kbeta,nd,kx,ky
    REAL(KIND=WP),INTENT(IN) :: sy(:)
    REAL(KIND=WP),INTENT(OUT) :: sb(:)
    LOGICAL :: qOKL

kx = SQRT(sEta_G/(8*sRho_G**2))
ky = SQRT(sEta_G/(8*sRho_G**2))
!original Puffin p_y equations
!       CALL PutValueInVector(iIm_PPerp_CG, &
!            sInv2rho * (fx_G*cos(ZOver2rho) - &
!            (salphaSq * sEta_G * Vector(iRe_Q_CG,sy) * &
!            sField4ElecImag ) ) + &
!            nd / Lj * & ! New focusing term
!            (  ( kbeta**2 * Vector(iRe_Y_CG,sy)) + (sEta_G / &
!            ( 1.0_WP + (sEta_G * Vector(iRe_Q_CG,sy)) ) * &
!            Vector(iRe_Y_CG,sb) * dp2f ) ), &
!            sb,	      & 	  
!            qOKL)
!


!curved poles equation for p_y
        CALL PutValueInVector(iRe_PPerp_CG, &
            sInv2rho * (SQRT(2.0) * SQRT(sEta_G) * Lj * Vector(iRe_PPerp_CG,sy) &
             * ( 1 + Vector(iRe_X_CG,sb)**2 * kx**2 /2) &
             * Vector(iRe_Y_CG,sy) * ky * cos(ZOver2rho) &
             / (SQRT(salphaSq) * ky) + sin(ZOver2rho) &
              * Vector(iRe_X_CG,sy) * Vector(iRe_Y_CG,sy)* &
               kx**2- sEta_G * Vector(iRe_Q_CG,sy) &
              * salphaSq * sField4ElecImag), &
            sb,       &       
            qOKL)


  END SUBROUTINE CALCULATE_PY


  SUBROUTINE CALCULATE_P2(sInv2rho,ZOver2rho,salphaSq,sField4ElecImag,sField4ElecReal,nd,Lj,kbeta,sb,sy,dp2f,nb,qOKL)


  	IMPLICIT NONE



    REAL(KIND=WP) :: sInv2rho
    REAL(KIND=WP) :: ZOver2rho,salphaSq
    REAL(KIND=WP),DIMENSION(:),ALLOCATABLE :: sField4ElecReal,sField4ElecImag
    REAL(KIND=WP),ALLOCATABLE :: Lj(:), dp2f(:)
    REAL(KIND=WP) :: kbeta,nd,nb,kx,ky
    REAL(KIND=WP),INTENT(IN) :: sy(:)
    REAL(KIND=WP),INTENT(OUT) :: sb(:)
    LOGICAL :: qOKL


kx = SQRT(sEta_G/(8*sRho_G**2))
ky = SQRT(sEta_G/(8*sRho_G**2)) 
!original Puffin p_2 equations
!        CALL PutValueInVector(iRe_Q_CG, &
!             2.0_WP * nb * Lj**2 * &
!             ((sEta_G * Vector(iRe_Q_CG,sy) + 1.0_WP)/ salphaSq * &
!             (Vector(iIm_pPerp_CG,sy) * fx_G*cos(ZOver2Rho) + &
!             Vector(iRe_pPerp_CG,sy) * fy_G*sin(ZOver2rho)) +&
!             sEta_G * Vector(iRe_Q_CG,sy) *&
!             (Vector(iRe_pPerp_CG,sy)*sField4ElecReal +&
!             Vector(iIm_pPerp_CG,sy)*sField4ElecImag)) &
!             + dp2f,& ! New focusing term
!             sb,&
!             qOKL)




!curved pole version of p_2 equation
       CALL PutValueInVector(iRe_Q_CG, &
            (4 * sRho_G / sEta_G) * Lj**2 * ( (Vector(iRe_pPerp_CG,sy) * sField4ElecReal &
            + Vector(iIm_pPerp_CG,sy) * sField4ElecImag) * Vector(iRe_Q_CG,sy) * sEta_G &
            + (1/salphaSq) * (1 + sEta_G * Vector(iRe_Q_CG,sy)) * sin(ZOver2rho) * &
            (Vector(iRe_pPerp_CG,sy) * (1 + kx**2 * Vector(iRe_X_CG,sy)**2 &
                + ky**2 * Vector(iRe_Y_CG,sy)**2) - Vector(iIm_pPerp_CG,sy) &
            * kx**2 * Vector(iRe_X_CG,sy) * Vector(iRe_Y_CG,sy) ) ),&
            sb,&
            qOKL)


  END SUBROUTINE CALCULATE_P2



END MODULE Equations