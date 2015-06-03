!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE rhs

! Module to calculate the RHS of the field source equation
! and d/dz of electron equations.
!

USE paratype
USE ArrayFunctions
USE Globals
USE Functions
USE extra
USE basis_fn
USE TransformInfoType
USE ParallelInfoType
USE stiffness
USE Equations

IMPLICIT NONE

CONTAINS

  SUBROUTINE getrhs(sz,&
       sA,&
       sy,&
       sb,&
       sDADz)

    IMPLICIT NONE

! Inputs %%%
!
! sZ - Propagation distance
! sA - current radiation field vals
! sy - current electron coordinates in all dimensions
! 
! Output
! sb  - d/dz of electron phase space positions
! sDADz - RHS of field source term

    REAL(KIND=WP),INTENT(IN) :: sz
    REAL(KIND=WP),INTENT(IN) :: sA(:)
    REAL(KIND=WP),INTENT(IN) :: sy(:)
    REAL(KIND=WP),INTENT(OUT) :: sb(:)
    REAL(KIND=WP), INTENT(INOUT) :: sDADz(:) !!!!!!!

! i
! dx,dy,dz2 - step size in x y z2
! xx,yy,zz2 - arrays of the principle node for each electron
! sa1_elxnew,sa1_elynew,sa1_elz2new - ARRAYS STORING THE
! PARTICLE'S POSITION X,Y,Z2
! s_Lex,s_Ley,s_Lez2 - co-ordinates of electrons locally
! N - storing the interploation function
! i_n4e - temporary storing the global nodes number 
! iNodeList_Re
! iNodeList_Im
! sInv2rho - 1/2rho
! sInv4rho - 1/4rho
! sTheta - z_2/2rho
! ZOver2rho - z/2rho
! salphaSq - alpha^2
! iAstartR - A_real index
! iAstartI - A_imag index	
! spPerpSq_i
! sBetaz_i	
! sField4ElecReal
! sField4ElecImag
! sBetaz_i         - Beta z for each electron
! sPPerp_Re        - Real PPerp value for ithelectron
! sPPerp_Im        - Imaginary PPerp value for ithelectron
! sQ_Re            - Real Q value for ithelectron
! qOKL             - Local error flag

    INTEGER(KIND=IP) :: icheck
    REAL(KIND=WP) :: dx,dy,dz2
    REAL(KIND=WP) :: dV3
    INTEGER(KIND=IP) :: xx,yy,xred,yred,zz2
    REAL(KIND=WP) :: s_Lex,s_Ley,s_Lez2
    INTEGER(KIND=IP),DIMENSION(:),ALLOCATABLE ::&
                i_n4e,iNodeList_Re,iNodeList_Im,&
                i_n4ered
    REAL(KIND=WP),DIMENSION(:),ALLOCATABLE :: N
    REAL(KIND=WP) :: sInv2rho,sInv4rho
    REAL(KIND=WP) :: ZOver2rho,salphaSq
    REAL(KIND=WP),DIMENSION(:),ALLOCATABLE ::&
         sField4ElecReal,sField4ElecImag
    INTEGER(KIND=IP) :: iAstartR,&
         iAstartI,NN
    REAL(KIND=WP) :: spPerpSq			   
    REAL(KIND=WP),ALLOCATABLE :: Lj(:), dp2f(:)
    REAL(KIND=WP) :: sBetaz_i,sInvGamma_i
    REAL(KIND=WP) :: sPPerp_Re
    REAL(KIND=WP) :: sPPerp_Im
    REAL(KIND=WP) :: sQ_Re 
    REAL(KIND=WP) :: sXcoord 
    REAL(KIND=WP) :: sYcoord
    REAL(KIND=WP) :: sZ2coord,z2test 
    REAL(KIND=WP) :: FieldConst,econst
    REAL(KIND=WP) :: stheta, kbeta, un, nc, nd, nb, fkb
    REAL(KIND=WP),DIMENSION(6) :: sendbuff, recvbuff 
    INTEGER(KIND=IP) :: x_inc, y_inc, z2_inc, istart, iend
    INTEGER(KIND=IP) :: iNodesX,iNodesZ2,iNodesY, j
    INTEGER(KIND=IPL) :: maxEl,i
    INTEGER(KIND=IP) :: local_z2_start, local_nz2, index, ti
    INTEGER(KIND=IP) :: iOutside
    INTEGER :: stat,req,error,lrank,rrank
    REAL(KIND=WP),DIMENSION(10)	:: couple 
    INTEGER(KIND=IP) :: retim
    REAL(KIND=WP) :: halfx, halfy

    REAL(KIND=WP) :: time1, start_time
    LOGICAL :: qOKL,qoutside

!     Begin

    qOKL = .FALSE.
    
!     SETUP AND INITIALISE THE PARTICLE'S POSITION
!     ALLOCATE THE ARRAYS

    ALLOCATE(i_n4e(iNodesPerElement_G),N(iNodesPerElement_G),&
             iNodeList_Re(iNodesPerElement_G),&
             iNodeList_Im(iNodesPerElement_G))
    AlLOCATE(i_n4ered(iNodesPerElement_G))
    ALLOCATE(sField4ElecReal(iNumberElectrons_G),&
             sField4ElecImag(iNumberElectrons_G))
    ALLOCATE(Lj(iNumberElectrons_G),dp2f(iNumberElectrons_G))

    ioutside=0

!     Set up Pointers to the field equations

    iAstartR = iBStartPosition_G(iRe_A_CG)
    iAstartI = iBStartPosition_G(iIm_A_CG)

!     Define the size of each element

    dx  = sLengthOfElmX_G
    dy  = sLengthOfElmY_G
    dz2 = sLengthOfElmZ2_G
    dV3 = sLengthOfElmX_G*sLengthOfElmY_G*sLengthOfElmZ2_G

!     Time savers

    sInv2rho    = 1.0_WP/(2.0_WP * sRho_G)
    sInv4rho    = 1.0_WP/(4.0_WP * sRho_G)
    ZOver2rho   = sz * sInv2rho
    salphaSq    = (2.0_WP * sGammaR_G * sRho_G / sAw_G)**2

    kbeta = sAw_G / (2.0_WP * sFocusFactor_G * sRho_G * sGammaR_G)
    un = sqrt(fx_G**2.0_WP + fy_G**2.0_WP)

!     Nodes in X, Y and Z2 dimensions

    iNodesX = NX_G
    iNodesY=NY_G
    iNodesZ2 = NZ2_G

!     Diff between real and imaginary nodes in the reduced system

    retim = ReducedNX_G*ReducedNY_G*nZ2_G

!     Initialise right hand side to zero

    sb = 0.0_WP
    sField4ElecReal = 0.0_WP
    sField4ElecImag = 0.0_WP


!     Adjust undulator tuning (linear taper)

!    n2col = n2col0 * (1 + undgrad*(sz - sz0))
    n2col = n2col0  + undgrad*(sz - sz0)

    fkb= sFocusfactor_G * kbeta

    econst = sAw_G/(sRho_G*SQRT(2.0_WP*(fx_G**2.0_WP+fy_G**2.0_WP)))

    nc = 2.0_WP*saw_G**2/(fx_G**2.0_WP + fy_G**2.0_WP)
    
    nd = SQRT((fx_G**2.0_WP+fy_G**2.0_WP)*(sEta_G))/(2.0_WP*SQRT(2.0_WP)* &
                             fkb*sRho_G)
    
    nb = 2.0_WP * sRho_G / ((fx_G**2.0_WP+fy_G**2.0_WP)*sEta_G)
    
    maxEl = maxval(procelectrons_G)
    qoutside=.FALSE.
    iOutside=0_IP

    halfx = ((ReducedNX_G-1) / 2.0_WP) * sLengthOfElmX_G
    halfy = ((ReducedNY_G-1) / 2.0_WP) * sLengthOfElmY_G

!     Looping (summing) over all the electrons

    DO i=1,maxEl
    
       IF (i<=procelectrons_G(1)) THEN	
       
!     Get electron variables for electron and field evolution.

          sPPerp_Re = GetValueFromVector(iRe_pPerp_CG, i, sy, qOKL)
          IF (.NOT. qOKL) THEN
              CALL Error_log('Error retrieving pperpre in RHS:ifrhs',tErrorLog_G)
          END IF              

          sPPerp_Im = GetValueFromVector(iIm_pPerp_CG, i, sy, qOKL)
          IF (.NOT. qOKL) THEN
              CALL Error_log('Error retrieving pperpim in RHS:ifrhs',tErrorLog_G)
          END IF

          sQ_Re     = GetValueFromVector(iRe_Q_CG,     i, sy, qOKL)
          IF (.NOT. qOKL) THEN
              CALL Error_log('Error retrieving p2 in RHS:ifrhs',tErrorLog_G)
          END IF
		
          sPperpSq = sPPerp_Re**2 + sPPerp_Im**2		
		
          sZ2coord = GetValueFromVector(iRe_Z2_CG,i,sy,qOKL)
          IF (.NOT. qOKL) THEN
              CALL Error_log('Error retrieving z2 in RHS:ifrhs',tErrorLog_G)
          END IF

          stheta    = sZ2coord * sinv2rho	
		 
          sXcoord = GetValueFromVector(iRe_X_CG,i,sy,qOKL)&
               + halfx

          sYcoord = GetValueFromVector(iRe_Y_CG,i,sy,qOKL)&
               + halfy

       ENDIF

!     Get info for dydz of ith electron

       IF (i<=procelectrons_G(1)) THEN

!     Calculate the coordinates of the principle node for
!     each electron 

          zz2 = floor(sZ2coord / dz2) + 1_IP

!     Calculate the co-ordinate for each electron locally

          s_Lez2 = sZ2coord - REAL(zz2 - 1_IP, KIND=WP)&
               * sLengthOfElmZ2_G

!     If s_lez2 is outside the boundary then let it = nearest boundary

          IF (s_Lez2<0.0_WP) THEN
             s_Lez2=0.0_WP
          END IF
		 
          IF (s_Lez2>sLengthOfElmZ2_G) THEN 
             s_Lez2=sLengthOfElmZ2_G
          END IF

!     Calculate pperpsq, betaz, and 1/gamma, and perform checks.

          IF (sEta_G * sQ_Re == -1.0_WP) THEN
             PRINT *, 'EPSILON +Q=-1,divide by zero need to exit'
             STOP	
          ENDIF
          sBetaz_i    =  1.0_WP / ( 1.0_WP + (sEta_G * sQ_Re)) 
		
          IF (sBetaz_i > 1.0_WP) THEN
             PRINT *, 'BETA_I > 1, sqrt of negative need to exit'
             STOP
          ENDIF
          IF (spPerpSq==-1.0_WP) THEN
             IF (tProcInfo_G%qRoot) PRINT*, 'electron', i,'ppsq=-1!!'
            ! CALL MPI_FINALIZE(error)
             STOP
          ENDIF
	  	
          sInvGamma_i = sqrt((1.0_WP - sBetaz_i**2.0_WP)&
               / (1.0_WP + nc*spPerpSq))

!     Calculate Lj term: full or approximated....

          !Lj(i) = 2.0_WP*(1.0_WP + sEta_G * sQ_Re ) / &
          !       (3.0_WP - sQ_Re)

          Lj(i) = sInvGamma_i*(1.0_WP + sEta_G * sQ_Re ) * sGammaR_G

!     Calculate the nodes surrounding the ith electron and the corresponding
!     interpolation function.
!     Work out what is the principal node for each electron and 7 
!     other nodes in the same element. See extra.f90

          IF (tTransInfo_G%qOneD) THEN

!     Work out the indices of the two surrounding nodes for this electron.

             i_n4e(1) = CEILING(sZ2coord/sLengthOfElmZ2_G)
             i_n4e(2) = i_n4e(1) + 1_IP

             IF (i_n4e(1)>SIZE(sA)) THEN
                IF (tProcInfo_G%qRoot) PRINT*, 'electron',&
                     i,'out of bounds of system'
                CALL MPI_FINALIZE(error)
                STOP
             ENDIF	
             IF (i_n4e(2)>SIZE(sA)) THEN
                IF (tProcInfo_G%qRoot) PRINT*,&
                     'electron', i,'out of bounds of system'
                CALL MPI_FINALIZE(error)
                STOP
             ENDIF
		
		
             CALL intpl_fn(s_Lez2,2_IP,sLengthOfElmZ2_G,N)			
		
          ELSE
             xx  = floor(sXcoord  / dx)  + 1_IP
             yy  = floor(sYcoord  / dy)  + 1_IP 			
             !xred=xx-(outnodex_G/2_IP)
             !yred=yy-(outnodey_G/2_IP)
			
             s_Lex  = sXcoord  - REAL(xx  - 1_IP,&
                  KIND=WP) * sLengthOfElmX_G
             s_Ley  = sYcoord  - REAL(yy  - 1_IP,&
                  KIND=WP) * sLengthOfElmY_G

             CALL principal(ReducedNX_G,ReducedNY_G,iNodesPerElement_G,&
                  iGloNumA_G,iNodCodA_G,xx,yy,zz2,i_n4e)

             qoutside=.false.
             !CALL principal2(ReducedNX_G,ReducedNY_G,&
             !     iNodesPerElement_G,xred,yred,zz2,i_n4ered,qoutside)

             IF (qoutside) ioutside=ioutside+1
			
!     Calculate how much the macroparticle contributes to each node in 
!     its current element. See basis_fn.f90

             CALL intpl_fn(s_Lex,&
                  s_Ley,&
                  s_Lez2,&
                  iNodesPerElement_G,&
                  sLengthOfElmX_G,&
                  sLengthOfElmY_G,&
                  sLengthOfElmZ2_G,&
                  N)						  
          END IF

          IF (SUM(N) > (1.0+1E-4) .OR. SUM(N) < (1.0-1E-4) ) THEN
             PRINT *,&
             'THE SUM OF INTERPOLATION FUNCTION IS WRONG',i,SUM(N)
             DO icheck=1,8
                IF (N(icheck)<0.0_WP) THEN
                   PRINT *,&
                   'INTERPOLATION FUNCTION HAS NEGATIVE VALUE(S)',&
                   icheck 
                ENDIF
             ENDDO
          ENDIF

          iNodeList_Re = i_n4e
          iNodeList_Im = i_n4e+retim

!     Get radiation field for coupling terms in electron macroparticle
!     equations.

          IF (qElectronFieldCoupling_G) THEN
             sField4ElecReal(i) = SUM( sA(iNodeList_Re) * N )
             sField4ElecImag(i) = SUM( sA(iNodeList_Im) * N )
          END IF

       END IF

!     Field eqn RHS

       IF ((qFieldEvolve_G) .AND. (.not. qoutside)) THEN
          IF (i<=procelectrons_G(1)) THEN
             IF (.NOT. qoutside) THEN
                IF (.NOT. tTransInfo_G%qOneD) iNodeList_Re = i_n4e

                sDADz(iNodeList_Re) = ((s_chi_bar_G(i)/dV3) * Lj(i)&
                      *  N * sPPerp_Re ) + &
                     sDADz(iNodeList_Re)
                     
                sDADz(iNodeList_Re+retim) = &
                     ((s_chi_bar_G(i)/dV3) *&
                     Lj(i) * N * sPPerp_Im ) + &
                     sDADz(iNodeList_Re+retim)
 
             END IF
          END IF
       END IF

    ENDDO

    IF (ioutside>0) THEN 
       Print*, 'WARNING: ',ioutside,&
            ' electrons are outside the inner driving core'
    END IF


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      Calculate electron d/dz of electron equations - if needed

    if (qElectronsEvolve_G) then   

!     z2

        CALL dz2dz(sy, sb, qOK) 

!     X

        call dxdz(sy, Lj, sb, qOK)              

!     Y

        call dydz(sy, Lj, sb, qOK)


!     dp2f is the focusing correction for dp2/dz

        call caldp2f(kbeta, sy, sb, dp2f)	



!     PX (Real pperp)
       
        call dppdz_r(sInv2rho,ZOver2rho,salphaSq,sField4ElecReal,nd,Lj,kbeta,sb,sy,dp2f,qOKL)


!     -PY (Imaginary pperp)

        call dppdz_i(sInv2rho,ZOver2rho,salphaSq,sField4ElecImag,nd,Lj,kbeta,sb,sy,dp2f,qOKL)

!     P2

        call dp2dz(sInv2rho,ZOver2rho,salphaSq,sField4ElecImag,sField4ElecReal,nd,Lj,kbeta,sb,sy,dp2f,nb,qOKL)

    end if 






    IF (qFieldEvolve_G) THEN

!     Sum dadz from different MPI processes together

        call sum2RootArr(sDADz,ReducedNX_G*ReducedNY_G*NZ2_G*2,0)

!     Boundary condition dadz = 0 at head of field

        IF (tProcInfo_G%qRoot) sDADz(1:ReducedNX_G*ReducedNY_G)=0.0_WP
 
        !IF (tTransInfo_G%qOneD) THEN
        !  IF (tProcInfo_G%qRoot) sDADz=sDADz !sDADz=6.0_WP*sDADz
        !ELSE
        !   IF (tProcInfo_G%qRoot) sDADz=sDADz !216.0_WP/8.0_WP*sDADz
        !END IF

    END IF
    
!     Switch field off

    IF (.NOT. qFieldEvolve_G) THEN
       sDADz = 0.0_WP
    END IF

!     If electrons not allowed to evolve then      

    IF (.NOT. qElectronsEvolve_G) THEN
       CALL PutValueInVector(iRe_pPerp_CG, 0.0_WP, sb, qOKL)
       CALL PutValueInVector(iIm_pPerp_CG, 0.0_WP, sb, qOKL)
       CALL PutValueInVector(iRe_Q_CG,     0.0_WP, sb, qOKL)
       CALL PutValueInVector(iRe_Z2_CG,    0.0_WP, sb, qOKL)
       CALL PutValueInVector(iRe_X_CG,     0.0_WP, sb, qOKL)
       CALL PutValueInVector(iRe_Y_CG,     0.0_WP, sb, qOKL)
    END IF

!     Deallocate arrays

    DEALLOCATE(i_n4e,N,iNodeList_Re,iNodeList_Im,i_n4ered)
    DEALLOCATE(sField4ElecReal,sField4ElecImag,Lj,dp2f)

  END SUBROUTINE getrhs

END MODULE rhs
