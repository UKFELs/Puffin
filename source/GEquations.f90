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

  subroutine getdpp_r(sInv2rho,ZOver2rho,salphaSq,&
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


         CALL PutValueInVector(iRe_PPerp_CG, &
             sInv2rho * ( COSH(Vector(iRe_X_CG,sy) * kx_und_G) * &
             SINH(Vector(iRe_Y_CG,sy) * ky_und_G) *SQRT(2.0_WP) &
              * SQRT(sEta_G) * Lj * Vector(iIm_PPerp_CG,sy)  &      
               *  cos(ZOver2rho) &
              / (SQRT(salphaSq) * ky_und_G) +   COSH(Vector(iRe_X_CG,sy) * kx_und_G) &
              *COSH(Vector(iRe_Y_CG,sy) * ky_und_G) * sin(ZOver2rho) - &
               sEta_G * Vector(iRe_Q_CG,sy) * salphaSq * sField4ElecReal), &
             sb,       &       
             qOKL)


! Curved poles p_x equation - 1st order expansion
!
!         CALL PutValueInVector(iRe_PPerp_CG, &
!             sInv2rho * ((1.0_WP + 0.5_WP * Vector(iRe_X_CG,sy)**2 * kx_und_G**2 ) *SQRT(2.0_WP) &
!              * SQRT(sEta_G) * Lj * Vector(iIm_PPerp_CG,sy)  &      
!               *  Vector(iRe_Y_CG,sy) *  cos(ZOver2rho) &
!              / SQRT(salphaSq)  +  &
!                ( 1 + 0.5_WP * Vector(iRe_X_CG,sy)**2 * kx_und_G**2 )* &
!               ( 1 + 0.5_WP * Vector(iRe_Y_CG,sy)**2 * ky_und_G**2 ) * sin(ZOver2rho) - &
!               sEta_G * Vector(iRe_Q_CG,sy) * salphaSq * sField4ElecReal), &
!             sb,       &       
!             qOKL)







    else if (zUndType_G == 'planepole')  then

!     Re(p_perp) equation for plane-poled undulator

         CALL PutValueInVector(iRe_PPerp_CG, &
           sInv2rho * (-sField4ElecReal*salphaSq * sEta_G * Vector(iRe_Q_CG,sy) &
           + ((SQRT(6.0_WP) *sRho_G) /SQRT(salphaSq))*SINH(sInv2rho * Vector(iRe_Y_CG,sy) &
           * SQRT(sEta_G))  * cos(ZOver2rho) * Lj * Vector(iIm_PPerp_CG,sy) + &
            COSH(sInv2rho * Vector(iRe_Y_CG,sy) * SQRT(sEta_G)) *sin(ZOver2rho)), &
            sb,       &       
            qOKL)



! plane pole - 1st order expansion
!
!         CALL PutValueInVector(iRe_PPerp_CG, &
!             sInv2rho * (-1.0_WP  *SQRT(2.0_WP) &
!              * SQRT(sEta_G) * Lj * Vector(iIm_PPerp_CG,sy) * &      
!                 Vector(iRe_Y_CG,sy) *  cos(ZOver2rho) &
!              / SQRT(salphaSq)  +  sin(ZOver2rho) - &
!               sEta_G * Vector(iRe_Q_CG,sy) * salphaSq * sField4ElecReal), &
!             sb,       &       
!             qOKL)







    else 

!     "normal" PUFFIN case with no off-axis undulator
!     field variation

        CALL PutValueInVector(iRe_PPerp_CG, &
            sInv2rho * (fy_G*n2col*sin(ZOver2rho) - &
            (salphaSq * sEta_G * Vector(iRe_Q_CG,sy) * &
            sField4ElecReal ) ) - & 
            nd / Lj * & ! New focusing term
            (  ( kbeta**2 * Vector(iRe_X_CG,sy)) + (sEta_G / &
            ( 1.0_WP + (sEta_G * Vector(iRe_Q_CG,sy)) ) * &
            Vector(iRe_X_CG,sb) * dp2f ) ), &
            sb,       &     
            qOKL)


    end if

  end subroutine getdpp_r


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!






  SUBROUTINE getdpp_i(sInv2rho,ZOver2rho,salphaSq,sField4ElecImag,nd,Lj,kbeta,sb,sy,dp2f,qOKL)


  	IMPLICIT NONE



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

               CALL PutValueInVector(iIM_PPerp_CG, &
              sInv2rho * ( -1.0_WP * SQRT(2.0_WP) * SQRT(sEta_G) * Lj * Vector(iRe_PPerp_CG,sy) &
              *COSH(Vector(iRe_X_CG,sy) * kx_und_G) * SINH(Vector(iRe_Y_CG,sy) * ky_und_G) &
                   * cos(ZOver2rho)  / (SQRT(salphaSq) * ky_und_G)  + sin(ZOver2rho) &
               * kx_und_G/ky_und_G * SINH(Vector(iRe_X_CG,sy) * kx_und_G) * SINH(Vector(iRe_Y_CG,sy) * ky_und_G) &
                - sEta_G * Vector(iRe_Q_CG,sy)  * salphaSq * sField4ElecImag), &
             sb,       &       
             qOKL)      

!     Curved poles p_x equation - 1st order expansion
!
!         CALL PutValueInVector(iIM_PPerp_CG, &
!              sInv2rho * ( -1.0_WP * SQRT(2.0_WP) * SQRT(sEta_G) * Lj * Vector(iRe_PPerp_CG,sy) &
!              * ( 1.0_WP + 0.5_WP * Vector(iRe_X_CG,sb)**2 * kx_und_G**2) &
!              * Vector(iRe_Y_CG,sy)  * cos(ZOver2rho) &
!              / SQRT(salphaSq)  + sin(ZOver2rho) &
!               * Vector(iRe_X_CG,sy) * Vector(iRe_Y_CG,sy)* &
!                kx_und_G**2 - sEta_G * Vector(iRe_Q_CG,sy) &
!               * salphaSq * sField4ElecImag), &
!             sb,       &       
!             qOKL)






    else if (zUndType_G == 'planepole') then 

!     Im(p_perp) equation for plane-poled undulator



           CALL PutValueInVector(iIM_PPerp_CG, &
            sInv2rho * (- sField4ElecImag * salphaSq * sEta_G * Vector(iRe_Q_CG,sy)  &
             - ((SQRT(6.0_WP) * sRho_G)/ SQRT(salphaSq)) * SINH(sInv2rho * Vector(iRe_Y_CG,sy) &
             * SQRT(sEta_G)) * cos(ZOver2rho) * Lj * Vector(iRe_PPerp_CG,sy)) , &
            sb,       &       
            qOKL)    

            
            
! plane pole - 1st order expansion (DOESN'T LOOK RIGHT [-LAWRENCE])
!
!         CALL PutValueInVector(iIM_PPerp_CG, &
!             sInv2rho * ( SQRT(2.0_WP) * SQRT(sEta_G) * Lj * Vector(iRe_PPerp_CG,sy) &
!              * Vector(iRe_Y_CG,sy)  * cos(ZOver2rho) &
!              / SQRT(salphaSq)  - sin(ZOver2rho) &
!               * Vector(iRe_X_CG,sy) * Vector(iRe_Y_CG,sy)* &
!                kx_und_G**2 - sEta_G * Vector(iRe_Q_CG,sy) &
!               * salphaSq * sField4ElecImag), &
!             sb,       &       
!             qOKL)







    else

!     "normal" PUFFIN case with no off-axis undulator
!     field variation


       call PutValueInVector(iIm_PPerp_CG, &
            sInv2rho * (fx_G*n2col*cos(ZOver2rho) - &
            (salphaSq * sEta_G * Vector(iRe_Q_CG,sy) * &
            sField4ElecImag ) ) + &
            nd / Lj * & ! New focusing term
            (  ( kbeta**2 * Vector(iRe_Y_CG,sy)) + (sEta_G / &
            ( 1.0_WP + (sEta_G * Vector(iRe_Q_CG,sy)) ) * &
            Vector(iRe_Y_CG,sb) * dp2f ) ), &
            sb,       &     
            qOKL)

    end if

  end subroutine getdpp_i





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine getdp2(sInv2rho,ZOver2rho,salphaSq, &
                      sField4ElecImag,sField4ElecReal, &
                      nd,Lj,kbeta,sb,sy,dp2f,nb,qOKL)


  	IMPLICIT NONE



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

        CALL PutValueInVector(iRe_Q_CG, &
             (4_WP * sRho_G / sEta_G) * Lj**2 * ( (Vector(iRe_pPerp_CG,sy) * sField4ElecReal &
             + Vector(iIm_pPerp_CG,sy) * sField4ElecImag) * Vector(iRe_Q_CG,sy) * sEta_G &
             + (1.0_WP/salphaSq) * (1 + sEta_G * Vector(iRe_Q_CG,sy)) * sin(ZOver2rho) * &
             (Vector(iRe_pPerp_CG,sy) * COSH(Vector(iRe_X_CG,sy) * kx_und_G) * COSH(Vector(iRe_Y_CG,sy) * ky_und_G) &
             + Vector(iIm_pPerp_CG,sy) * kx_und_G/ky_und_G * SINH(Vector(iRe_X_CG,sy) * kx_und_G) * &
             SINH(Vector(iRe_Y_CG,sy) * ky_und_G))),&
             sb,&
             qOKL)

!     Curved pole to 1st order
!
!        CALL PutValueInVector(iRe_Q_CG, &
!             (4_WP * sRho_G / sEta_G) * Lj**2 * ( (Vector(iRe_pPerp_CG,sy) * sField4ElecReal &
!             + Vector(iIm_pPerp_CG,sy) * sField4ElecImag) * Vector(iRe_Q_CG,sy) * sEta_G &
!             + (1.0_WP/salphaSq) * (1 + sEta_G * Vector(iRe_Q_CG,sy)) * sin(ZOver2rho) * &
!             (Vector(iRe_pPerp_CG,sy) * (1.0_WP + 0.5_WP* kx_und_G**2 * Vector(iRe_X_CG,sy)**2 ) * &
!                (1.0_WP + 0.5_WP * ky_und_G**2 * Vector(iRe_Y_CG,sy)**2) + Vector(iIm_pPerp_CG,sy) &
!             * kx_und_G**2 * Vector(iRe_X_CG,sy) * Vector(iRe_Y_CG,sy) ) ),&
!             sb,&
!             qOKL)

    else if (zUndType_G == 'planepole') then 

!     p2 equation for plane-poled undulator

 
        CALL PutValueInVector(iRe_Q_CG, &
            (4_WP * sRho_G / sEta_G) * Lj**2 * ( (Vector(iRe_pPerp_CG,sy) * sField4ElecReal &
            + Vector(iIm_pPerp_CG,sy) * sField4ElecImag) * Vector(iRe_Q_CG,sy) * sEta_G &
            + (1.0_WP/salphaSq) * (1 + sEta_G * Vector(iRe_Q_CG,sy)) &
            * COSH(sInv2rho * Vector(iRe_Y_CG,sy) * SQRT(sEta_G)) &
             * sin(ZOver2rho) *  Vector(iRe_pPerp_CG,sy)  ) , &
            sb,&
            qOKL)

! Plane pole to 1st order (DOESN'T LOOK RIGHT [-LAWRENCE])
!
!        CALL PutValueInVector(iRe_Q_CG, &
!             (4_WP * sRho_G / sEta_G) * Lj**2 * ( (Vector(iRe_pPerp_CG,sy) * sField4ElecReal &
!             + Vector(iIm_pPerp_CG,sy) * sField4ElecImag) * Vector(iRe_Q_CG,sy) * sEta_G &
!             + (1.0_WP/salphaSq) * (1 + sEta_G * Vector(iRe_Q_CG,sy)) * sin(ZOver2rho) * &
!             (Vector(iRe_pPerp_CG,sy) - Vector(iIm_pPerp_CG,sy) &
!             * kx**2 * Vector(iRe_X_CG,sy) * Vector(iRe_Y_CG,sy) ) ),&
!             sb,&
!             qOKL)



 
    else

!     "normal" PUFFIN case with no off-axis undulator
!     field variation

        call PutValueInVector(iRe_Q_CG, &
             2.0_WP * nb * Lj**2 * &
             ((sEta_G * Vector(iRe_Q_CG,sy) + 1.0_WP)/ salphaSq * n2col * &
             (Vector(iIm_pPerp_CG,sy) * fx_G*cos(ZOver2Rho) + &
             Vector(iRe_pPerp_CG,sy) * fy_G*sin(ZOver2rho)) +&
             sEta_G * Vector(iRe_Q_CG,sy) *&
             (Vector(iRe_pPerp_CG,sy)*sField4ElecReal + &
             Vector(iIm_pPerp_CG,sy)*sField4ElecImag)) &
             + dp2f, & 
             sb,&
             qOKL)

    end if



end subroutine getdp2

end module equations