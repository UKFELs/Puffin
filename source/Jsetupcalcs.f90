MODULE setupcalcs

USE Paratype
USE ParallelInfoType
USE TransformInfoType
USE IO
USE ArrayFunctions
USE GlobalNodes
USE extra
USE typesAndConstants
USE DerivsGlobals
USE electronInit

IMPLICIT NONE

CONTAINS

SUBROUTINE CheckParameters(sLenEPulse,iNumElectrons,nbeams,&
       sLengthofElm,iNodes,sWigglerLength,sStepSize,&
       nSteps,srho,sEta,sKBeta,focusfactor,sSigE,f_x, f_y, iRedNodesX,&
       iRedNodesY,qSwitches,qOK)

  IMPLICIT NONE

! Subroutine to check that the electron and field
! parameters are sensible.
!
! Code for the checks originally written by Cynthia Nam
! and Dr Pamela Johnston. This subroutine was initially
! created by collecting the code from parts of an older
! code. It has been modified further to include other checks,
! and modified to be compatible with the fuller
! parallelization of this code.
!
!                ARGUMENTS

  REAL(KIND=WP),INTENT(INOUT) :: sLenEPulse(:,:)
  INTEGER(KIND=IP),INTENT(INOUT) :: iNumElectrons(:,:)
  INTEGER(KIND=IP), INTENT(IN) :: nbeams
  REAL(KIND=WP),INTENT(INOUT) :: sLengthofElm(:)
  INTEGER(KIND=IP),INTENT(INOUT) :: iNodes(:)
  REAL(KIND=WP), INTENT(INOUT) :: sWigglerLength(:)
  REAL(KIND=WP), INTENT(INOUT) :: sStepSize
  INTEGER(KIND=IP), INTENT(INOUT) :: nSteps
  REAL(KIND=WP), INTENT(IN) :: srho,sEta,sKBeta,focusfactor
  REAL(KIND=WP), INTENT(INOUT) :: sSigE(:,:)
  REAL(KIND=WP), INTENT(INOUT) :: f_x, f_y
  INTEGER(KIND=IP),INTENT(INOUT) :: iRedNodesX,iRedNodesY
  LOGICAL, INTENT(INOUT) :: qSwitches(:)
  LOGICAL, INTENT(OUT)  :: qOK

!           Local vars

  INTEGER(KIND=IP) :: i,nninner
  REAL(KIND=WP) :: sEE,dx,lwx,lexm,maxr
  LOGICAL :: qOKL  

  qOK = .FALSE.

  DO i = 1,nbeams

    CALL chkESampleLens(sLenEPulse(i,:),iNumElectrons(i,:),srho,qSwitches(iOneD_CG),qOKL)
    IF (.NOT. qOKL) GOTO 1000
    
  END DO

  CALL stpFSampleLens(iNodes,sWigglerLength,sLengthOfElm,qSwitches(iOneD_CG),qOKL)
 
  CALL chkFSampleLens(iNodes,sWigglerLength,sLengthOfElm,srho,qOKL)
  IF (.NOT. qOKL) GOTO 1000

  CALL checkIntSampling(sStepSize,nSteps,sLengthOfElm,qSwitches,qOKL)
  IF (.NOT. qOKL) GOTO 1000

  CALL checkFreeParams(srho,sEta,sKBeta,f_x,f_y,focusfactor,qOKL)
  IF (.NOT. qOKL) GOTO 1000

  IF(tProcInfo_G%qRoot) PRINT '(I5,1X,A17,1X,F8.4)',nsteps,&
       'Step(s) and z-bar=',nsteps*sStepSize
  
!     Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000 CALL Error_log('Error in setupcalcs:CheckParameters',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE CheckParameters
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


SUBROUTINE stpFSampleLens(iNodes,sWigglerLength,sLengthOfElm,qOneD,qOK)

  INTEGER(KIND=IP), INTENT(INOUT) :: iNodes(:)
  REAL(KIND=WP), INTENT(IN) :: sWigglerLength(:)
  LOGICAL, INTENT(IN) :: qOneD
  REAL(KIND=WP), INTENT(OUT) :: sLengthOfElm(:)
  LOGICAL, INTENT(OUT) :: qOK


  qOK = .FALSE.

  IF (qOneD) THEN
  
    iNodes(iX_CG) = 1_IP
    iNodes(iY_CG) = 1_IP
    
    sLengthOfElm(iX_CG) = 1.0_WP
    sLengthOfElm(iY_CG) = 1.0_WP
    
  ELSE
  
    IF (iNodes(iX_CG)==1_IP) THEN
      CALL Error_log('The field in x is sampled with only one node...',tErrorLog_G)
      CALL Error_log('...and you have not specified a 1D option in the input file.',tErrorLog_G)
      GOTO 1000
    END IF

    IF (iNodes(iY_CG)==1_IP) THEN
      CALL Error_log('The field in x is sampled with only one node...',tErrorLog_G)
      CALL Error_log('...and you have not specified a 1D option in the input file.',tErrorLog_G)
      GOTO 1000
    END IF  

    sLengthOfElm(iX_CG) = sWigglerLength(iX_CG) / (iNodes(iX_CG) - 1_IP)
    sLengthOfElm(iY_CG) = sWigglerLength(iY_CG) / (iNodes(iY_CG) - 1_IP)

  END IF

  sLengthOfElm(iZ2_CG) = sWigglerLength(iZ2_CG) / (iNodes(iZ2_CG) - 1_IP)


!     Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000 CALL Error_log('Error in setupcalcs:stpFSampleLens',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE stpFSampleLens


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE chkESampleLens(sLenEPulse,iNumElectrons,rho,qOneD,qOK)

!                 ARGUMENTS

  REAL(KIND=WP), INTENT(INOUT) :: sLenEPulse(:)
  REAL(KIND=WP), INTENT(IN) ::  rho
  INTEGER(KIND=IP), INTENT(IN) :: iNumElectrons(:)
  LOGICAL, INTENT(IN) :: qOneD
  LOGICAL, INTENT(INOUT) :: qOK

!                 LOCAL ARGS

  INTEGER(KIND=IP) :: i
  REAL(KIND=WP) :: wlen, maxspcing, spcing
  
  qOK = .FALSE.

  wlen = 4.0_WP * pi * rho
  maxspcing = wlen / 8.0_WP

  spcing = sLenEPulse(iZ2_CG) / iNumElectrons(iZ2_CG)
 
!     Check sampling

  IF (spcing > maxspcing) THEN
    CALL Error_log('Macroparticle spacing in z2 > 1/8th of resonant wavelength',tErrorLog_G)
    GOTO 1000
  END IF 
    
  DO i= 1, SIZE(sLenEPulse)

    IF (sLenEPulse(i) <=0.0_WP .AND. iNumElectrons(i) > 1_IP) THEN
      CALL Error_log('Negative or zero electron pulse length',tErrorLog_G)
      GOTO 1000
    END IF

    IF (iNumElectrons(i) < 1_IP) THEN
      CALL Error_log('Number of electrons is less than 1 in one or more dimensions.',tErrorLog_G)
      GOTO 1000
    END IF

!     Check length of electron pulse is valid

    IF (iNumElectrons(i) == 1_IP) THEN
      sLenEPulse(i)=1.0_WP
    END IF

  END DO

!     Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000 CALL Error_log('Error in setupcalcs:chkESampleLens',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE chkESampleLens

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE chkFSampleLens(iNodes,sWigglerLength,sLengthOfElm,rho,qOK)

!                  ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN) :: iNodes(:)
  REAL(KIND=WP), INTENT(IN) :: sWigglerLength(:), sLengthOfElm(:),rho
  LOGICAL, INTENT(OUT) :: qOK

!                  LOCAL ARGS

  INTEGER(KIND=IP) :: i
  REAL(KIND=WP) :: wlen,maxspcing

  qOK = .FALSE.

  wlen = 4.0_WP * pi * rho
  maxspcing = wlen / 8.0_WP

!     Check got nodes set in every direction
  
  DO i = 1, SIZE(iNodes)

    IF (iNodes(i) <= 0_IP) THEN
      CALL Error_log('Number of nodes <=0 in one or more dimensions',tErrorLog_G)
      GOTO 1000
    END IF

  END DO
 
!     Check length of elements in field are valid

  DO i = 1, SIZE(sLengthOfElm)

    IF(sLengthOfElm(i) <= 0_IP) THEN
      CALL Error_log('Length of elements <=0 in one or more dimensions.',tErrorLog_G)
      GOTO 1000
    END IF

    IF (sLengthOfElm(iZ2_CG) > maxspcing) THEN
      CALL Error_log('Length of field elements > 1/8th of resonant wavelength in z2.',tErrorLog_G)
      GOTO 1000
    END IF
    
  END DO

!     Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000 CALL Error_log('Error in setupcalcs:chkFSampleLens',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE chkFSampleLens

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
  
SUBROUTINE checkIntSampling(sStepSize,nSteps,sLengthOfElm,qSwitches,qOK)

!                 ARGUMENTS

  REAL(KIND=WP), INTENT(IN) :: sStepSize,sLengthOfElm(:)
  INTEGER(KIND=IP), INTENT(IN) :: nSteps
  LOGICAL, INTENT(IN) :: qSwitches(:)
  LOGICAL, INTENT(OUT) :: qOK

  qOK = .FALSE.

!     Check if the stepsize is OK

  IF(sStepSize <= 0.0_WP) THEN
    CALL Error_log('Step size <=0.',tErrorLog_G)
    GOTO 1000
  END IF
  
  IF (qSwitches(iFieldEvolve_CG)) THEN

    IF( sStepSize >= (0.5_WP * sLengthOfElm(iZ2_CG))) THEN
      CALL Error_log('The integration step size is too large.',tErrorLog_G)
      GOTO 1000
    END IF

  END IF

  IF(nSteps <= 0_IP) THEN
    CALL Error_log('Number of steps <=0.',tErrorLog_G)
    GOTO 1000
  END IF

!     If nsteps not set then set to one Raleigh length

!  IF (nSteps <= 0_IP) THEN

!    nSteps = CEILING((sSigE(iX_CG)**2 &
!      / (2.0_WP *srho)) / sStepSize)

!  END IF

!     Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000 CALL Error_log('Error in setupcalcs:checkIntSampling',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE checkIntSampling

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE getElmLengths(sLenEPulse,sWigglerLength,iNodes,qOneD,sLengthOfElm)

  REAL(KIND=WP), INTENT(IN) :: sLenEPulse(3), sWigglerLength(3)
  INTEGER(KIND=IP), INTENT(IN) :: iNodes(3)
  LOGICAL, INTENT(IN) :: qOneD
  REAL(KIND=WP), INTENT(OUT) :: sLengthOfElm(3)

!     Calculate field element lengths, dx dy dz2

  IF (qOneD) THEN

    sLengthOfElm(iX_CG:iY_CG) = 1.0_WP

  ELSE

    sLengthOfElm(iX_CG:iY_CG) = sWigglerLength(iX_CG:iY_CG)&
                      / REAL(iNodes(iX_CG:iY_CG)-1,KIND=WP)

  END IF

  sLengthOfElm(iZ2_CG) = (sLenEPulse(iZ2_CG) + &
                          sWigglerLength(iZ2_CG)) / &
                          REAL(iNodes(iZ2_CG)-1,KIND=WP) 

END SUBROUTINE getElmLengths

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE checkFreeParams(srho,sEta,sKBeta,f_x,f_y,focusfactor,qOK)

  REAL(KIND=WP), INTENT(IN) :: srho,sEta,sKBeta,f_x,f_y,focusfactor
  LOGICAL, INTENT(OUT) :: qOK

  qOK = .FALSE.

!     Check got valid value for rho

  IF (srho <= 0.0_WP) THEN
    CALL Error_log('Parameter rho <=0.',tErrorLog_G)
    GOTO 1000    
  END IF

!     Check got valid value for eta
  
  IF (sEta <= 0.0_WP) THEN
    CALL Error_log('Parameter eta <=0.',tErrorLog_G)
    GOTO 1000    
  END IF
  
  IF (sEta >= 1.0_WP) THEN
    CALL Error_log('Parameter eta >=1.',tErrorLog_G)
    GOTO 1000    
  END IF

!     Check got valid value for kBeta
  
  IF (sKBeta <= 0.0_WP) THEN
    CALL Error_log('Parameter kBeta <=1.',tErrorLog_G)
    GOTO 1000    
  END IF
    
!     Check got valid value for fx or fy

  IF (.NOT. (f_x==1.0_WP .OR. f_y==1.0_WP)) THEN
    CALL Error_log('Either fx OR fy must be 1.',tErrorLog_G)
    GOTO 1000
  END IF

!     Check got valid value for focusing factor

  IF (focusfactor<=0_WP) THEN
    CALL Error_log('focusfactor must be > 0.',tErrorLog_G)
    GOTO 1000
  END IF

!     Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000 CALL Error_log('Error in setupcalcs:checkFreeParams',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE checkFreeParams

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE passToGlobals(rho,eta,kbeta,iNN, &
                   sRNX,sRNY,iNM, &
                   sElmLen,sLenEPulse,sSigE,&
                   fx,fy,sFocusFactor,sFiltFrac, &
                   dStepFrac,qSwitch,qOK)

    IMPLICIT NONE

! Subroutine to pass all the temporary variables to global
! vars used in the integration loop.
!
!                    ARGUMENTS
!
! rho                Pierce parameter, describe the strngth of the field
! aw                 Wiggler parameter
! gamma_r            Mean electron velocity at resonance
! nElements          number of elements in each dimension (x,y,z2,px,py,p2)
! iNM                number of electrons in each dimension (x,y,z2,px,py,p2)
! iNN                number of nodes in each dimension (x,y,z2)
! sRNX,sRNY          number of nodes in the inner reduced 'active'
!                    set of nodes
! sElmLen            Length of ONE element in each dimension (x,y,z2)
! sLenEPulse         Length of the electron pulse in each
!                    dimension (x,y,z2,px,py,p2)
! sSigE              Sigma spread of electron gaussian
!                    distribution in each dimension
! qSwitch            If letting electrons evolve, field evolve,
!                    diffraction and gaussian field
! qOK                Error flag

    REAL(KIND=WP),     INTENT(IN)    :: rho,eta,kbeta
    INTEGER(KIND=IP),  INTENT(IN)    :: iNM(:,:),sRNX,sRNY
    INTEGER(KIND=IP),  INTENT(IN)    :: iNN(:)
    REAL(KIND=WP),     INTENT(IN)    :: sElmLen(:)	
    REAL(KIND=WP),     INTENT(INOUT) :: sLenEPulse(:,:)   
    REAL(KIND=WP),     INTENT(IN)    :: sSigE(:,:)
    REAL(KIND=WP),     INTENT(IN)    :: fx,fy,sFocusFactor
    REAL(KIND=WP),     INTENT(IN)    :: sFiltFrac
    LOGICAL,           INTENT(IN)    :: qSwitch(nSwitches_CG)
    LOGICAL,           INTENT(OUT)   :: qOK

!                    LOCAL ARGS
!
! lambda_r           Resonant wavelength in scaled units
! qOKL               Local error flag

    REAL(KIND=WP) :: lambda_r,LenZ2
    LOGICAL :: qOKL

    qOK = .FALSE.

!                  Pass to globals

    NX_G  = iNN(iX_CG)
    NY_G  = iNN(iY_CG)
    NZ2_G = iNN(iZ2_CG)

    IF (NX_G == 1 .AND. NY_G == 1) THEN
    
       iNodesPerElement_G = 2_IP
    
    ELSE
    
       iNodesPerElement_G = 8_IP
    
    END IF

    IF (tTransInfo_G%qOneD) THEN
    
       IF (iNM(1,iX_CG)==1_IP .AND. iNM(1,iY_CG)==1_IP) THEN
    
          transA_G=sLenEPulse(1,iX_CG)*sLenEPulse(1,iY_CG)
    
       ELSE IF (sLenEPulse(1,iX_CG)<6.0_WP*sSigE(1,iX_CG) .AND. &
            sLenEPulse(1,iY_CG)<6.0_WP*sSigE(1,iY_CG)) THEN
    
          transA_G=sLenEPulse(1,iX_CG)*sLenEPulse(1,iY_CG)
       
       ELSE IF (sLenEPulse(1,iX_CG)<=1.0E-6_WP)  THEN
       
          transA_G=sLenEPulse(1,iX_CG)*sLenEPulse(1,iY_CG)
       
       ELSE
       
          transA_G=2.0_WP*pi*sSigE(1,iX_CG)**2.0_WP
       
       END IF
    
    ELSE
    
       transA_G=1.0_WP
    
    END IF



    ReducedNX_G=sRNX
    ReducedNY_G=sRNY

    IF (NX_G*NY_G==1) THEN
      ReducedNX_G=1_IP
      ReducedNY_G=1_IP
    END IF
    
    outnodex_G=NX_G-ReducedNX_G
    outnodey_G=NY_G-ReducedNY_G

!     Set up the length of ONE element globally

    sLengthOfElmX_G  = sElmLen(iX_CG)
    sLengthOfElmY_G  = sElmLen(iY_CG)
    sLengthOfElmZ2_G = sElmLen(iZ2_CG)

    delta_G = sLengthOfElmX_G*sLengthOfElmY_G*sLengthOfElmZ2_G

!     Filter fraction to frequency in Fourier space

    Lenz2 = sLengthOfElmZ2_G * NZ2_G
    lambda_r = 4*pi*rho
    sFilt = Lenz2 / lambda_r * sFiltFrac

!     Set up parameters

    sRho_G = rho
    sEta_G = eta
    sKBeta_G = kbeta
    sFocusfactor_G = sFocusfactor    
    fx_G = fx
    fy_G = fy
    sAw_G = ((1.0_WP / (2.0_WP*rho*sFocusFactor*kbeta)**2.0_WP) *   &
            (1.0_WP - (1.0_WP / ( 1.0_WP + eta )**2) ) - 1.0_WP) ** (-0.5_WP)
    sGammaR_G = sAw_G / (2.0_WP * rho * sFocusFactor * kbeta)

    diffStep = dStepFrac * 4.0_WP * pi * rho

!     Get the number of nodes

    iNumberNodes_G = iNN(iX_CG)*iNN(iY_CG)*iNN(iZ2_CG)

    IF(iNumberNodes_G <= 0_IP) THEN
       CALL Error_log('iNumberNodes_G <=0.',tErrorLog_G)
       GOTO 1000    
    END IF

!     Get global node numbers

    IF (.NOT. tTransInfo_G%qOneD) THEN
       ALLOCATE(iNodCodA_G(ReducedNX_G,ReducedNY_G,NZ2_G))
       ALLOCATE(iGloNumA_G(8*(ReducedNX_G-1)*(ReducedNY_G-1)*(NZ2_G-1)))
       CALL GL_NUM(ReducedNX_G,ReducedNY_G,NZ2_G,iNodCodA_G,iGloNumA_G)
    END IF

    qElectronsEvolve_G       = qSwitch(iElectronsEvolve_CG)
    qFieldEvolve_G           = qSwitch(iFieldEvolve_CG)
    qElectronFieldCoupling_G = qSwitch(iElectronFieldCoupling_CG)
    qDiffraction_G           = qSwitch(iDiffraction_CG)
    qFocussing_G             = qSwitch(iFocussing_CG)

!     Set error flag and exit

    qOK = .TRUE.				    

    GOTO 2000

1000 CALL Error_log('Error in setupCalcs:setupParams',tErrorLog_G)

2000 CONTINUE

  END SUBROUTINE passToGlobals
!**********************************************************
                            
  SUBROUTINE SetUpInitialValues(nseeds, freqf, SmeanZ2, qFlatTopS, sSigmaF, &
       sLengthOfElm, &
       sA0_x, &
       sA0_y, &
       sX0, &
       sY0, &
       sZ20, &
       sV, &
       sA, &
       qOK)

    IMPLICIT NONE
!
! Set up the initial macroparticle and field values
!
!                   ARGUMENTS
!
! sSeed              INPUT    Descriptions of the sSeed field
! qInitialGauss      INPUT    If to use gauss
! sA0_Re             INPUT    Initial field value (real)
! sA0_Im             INPUT    Initial field value (imaginary)
! sX0                INPUT    Initial X values
! sY0                INPUT    Initial Y values
! sZ20               INPUT    Initial z2 values
! sV                 UPDATE   Initial values of variables to
!                                      be integrated
! qOK                OUTPUT   Error flag

    INTEGER(KIND=IP), INTENT(IN) :: nseeds
    REAL(KIND=WP), INTENT(IN)    :: sSigmaF(:,:), SmeanZ2(:), freqf(:)
    LOGICAL, INTENT(IN) :: qFlatTopS(:)
    REAL(KIND=WP), INTENT(IN)    :: sLengthOfElm(:)
    REAL(KIND=WP), INTENT(IN)    :: sA0_x(:)
    REAL(KIND=WP), INTENT(IN)    :: sA0_y(:)
    REAL(KIND=WP), INTENT(IN)    :: sX0(:)      
    REAL(KIND=WP), INTENT(IN)    :: sY0(:)      
    REAL(KIND=WP), INTENT(IN)    :: sz20(:)      
    REAL(KIND=WP), INTENT(INOUT) :: sV(:)
    REAL(KIND=WP), INTENT(INOUT) :: sA(:)
    LOGICAL,       INTENT(OUT)   :: qOK

!                LOCAL ARGS
! 
! qOKL         Local error flag
! iZ2          Number of nodes in Z2
! iXY          Number of nodes in XY plane
! sA0gauss_Re  Initial field over all planes

    LOGICAL           :: qOKL
    LOGICAL           :: qInitialGauss
    INTEGER(KIND=IP)  :: iZ2,iXY,i,lowind,highind,error,NN(3)
    REAL(KIND=WP)     :: z2bar,rho
    REAL(KIND=WP),DIMENSION(:),ALLOCATABLE :: sAx_mag,sAy_mag,&
                                              sAreal,sAimag

!     Set error flag to false

    qOK = .FALSE. 

    iZ2 = NZ2_G
    iXY = NX_G*NY_G

    NN(iX_CG) = NX_G
    NN(iY_CG) = NY_G
    NN(iZ2_CG) = NZ2_G
    
    ALLOCATE(sAreal(iXY*iZ2),sAimag(iXY*iZ2))
   
    CALL getSeeds(NN,sSigmaF,SmeanZ2,sA0_x,sA0_y,qFlatTopS,sRho_G,freqf, &
                  nseeds,sLengthOfElm,sAreal,sAimag)

    sA(1:iXY*iZ2) = sAreal
    sA(iXY*iZ2 + 1:2*iXY*iZ2) = sAimag

    DEALLOCATE(sAreal,sAimag)

!     Set up initial electron perp values

    CALL PutValueInVector(iRe_PPerp_CG, sEl_PX0Position_G,&
         sV, qOKL)
    IF (.NOT. qOKL) GOTO 1000

    CALL PutValueInVector(iIm_PPerp_CG,-sEl_PY0Position_G,&
         sV, qOKL)		
    IF (.NOT. qOKL) GOTO 1000

!     Set up initial electron Q values

    CALL PutValueInVector(iRe_Q_CG, sEl_PZ20Position_G,&
         sV, qOKL)
    IF (.NOT. qOKL) GOTO 1000

!     Set up initial electron starting positions in z2 direction

    CALL PutValueInVector(iRe_Z2_CG, sz20, sV, qOKL)
    IF (.NOT. qOKL) GOTO 1000

!     Set up initial electron starting positions in x direction

    CALL PutValueInVector(iRe_X_CG, sX0, sV, qOKL)
    IF (.NOT. qOKL) GOTO 1000

!     Set up initial electron starting positions in y direction

    CALL PutValueInVector(iRe_Y_CG, sY0, sV, qOKL)
    IF (.NOT. qOKL) GOTO 1000

!     Set error flag and exit

    qOK = .TRUE.				    
    GOTO 2000

1000 CALL Error_log('Error in FEMethod:SetUpInitialValues',tErrorLog_G)
    PRINT*,'Error in FEMethod:SetUpInitialValues'
2000 CONTINUE

  END SUBROUTINE SetUpInitialValues

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE xygaussnew(iX,iY,sigma_X,sigma_Y,sLengthOfElm_X,&
       sLengthOfElm_Y,a2XYgauss)
	
    IMPLICIT NONE
!
! Calculate the initial seed field in the transverse plane
!
! iX                - INPUT   - Number of Nodes in the x direction
! iY                - INPUT   - Number of Nodes in the y direction
! sigma_X           - INPUT   - The seed field sigma in x direction
! sigma_Y           - INPUT   - The seed field sigma in y direction
! sLengthOfElm_X    - INPUT   - The element size in x direction
! sLengthOfElm_Y    - INPUT   - The element size in y direction
! a2XYgauss         - OUTPUT  - The initial seed field in the
!                               transverse plane
    INTEGER(KIND=IP),INTENT(IN)	:: iX,iY
    REAL(KIND=WP),INTENT(IN) :: sigma_X,sigma_Y
    REAL(KIND=WP),INTENT(IN) :: sLengthOfElm_X,sLengthOfElm_Y
    REAL(KIND=WP), DIMENSION(iX*iY), INTENT(OUT) :: a2XYgauss
! Define local variables
! i,j,index  - local index
! alXgauss   - local array storing seed field in x
! a1Ygauss   - local array storing seed field in x
    INTEGER(KIND=IP) :: i,j,index,error
    REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: a1Xgauss
    REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: a1Ygauss	
!--------------------------------------------------------
! BEGIN:-
! Allocate the size of arrays
    ALLOCATE(a1Xgauss(iX),a1Ygauss(iY))

    CALL Gaussnew(iX,sigma_X,sLengthOfElm_X,a1Xgauss,.FALSE.)
    CALL Gaussnew(iY,sigma_Y,sLengthOfElm_Y,a1Ygauss,.FALSE.)

    a2XYgauss = 0.0_WP
    DO i = 1, iX
       DO j = 1, iY
          index = iY*(i-1_IP)+j
          a2XYgauss(index)=a1Xgauss(i)*a1Ygauss(j)
       ENDDO
    ENDDO

    DEALLOCATE(a1Xgauss,a1Ygauss)
	
  END SUBROUTINE xygaussnew
!********************************************************
  SUBROUTINE Gaussnew(iNodes,sigma,sLengthOfElm,&
       a1gauss,qxgauss)
	
    IMPLICIT NONE
!
! Setting the seed field to be zero at outer edges and
! seed field only exist in the centre. We are doing this
! to look at the diffraction and remove effects due to the boundary conditions
!
! ARGS:-
!
! iNodes            - INPUT   - Number of Nodes 
! sigma             - INPUT   - The seed field sigma 
! sLengthOfElm      - INPUT   - The element size 
! a1gauss           - OUTPUT  - The initial seed field 
!
    INTEGER(KIND=IP),INTENT(IN) :: iNodes
    REAL(KIND=WP),INTENT(IN) :: sigma
    REAL(KIND=WP),INTENT(IN) :: sLengthOfElm
    REAL(KIND=WP), DIMENSION(:), INTENT(OUT) :: a1gauss
    LOGICAL, INTENT(IN) :: qxgauss
! Define local variables
! a1X            - Local array storing seed field
! sTotalLength   - Total length in one direction
! SFNodes        - Number of nodes used for the seed field
! WLNodes        - Number of nodes used for the wiggler length
! FrontNodes     - First half of WLNodes (SFNodes in between)
! BackNodes      - Second half of WLNodes
    REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: a1X
    REAL(KIND=WP) :: sTotalLength,sigma2
    INTEGER(KIND=IP) :: SFNodes,WLNodes,FrontNodes,BackNodes
!--------------------------------------------------------
! BEGIN:-
! ALLOCATE MATRIX OR ARRAY SIZE

    ALLOCATE(a1X(iNodes))
    
! Uniformly spaced in one direction

    a1X = linspace(0.0_WP,REAL(iNodes-1)*sLengthOfElm,iNodes)
    
    sTotalLength=REAL(iNodes-1_IP)*sLengthOfElm
    
! Calculate the number of seed field nodes

    SFNodes=CEILING((6.0_WP*sigma/sTotalLength)*iNodes)	
    
! Calculate the number of wiggler length nodes	

    WLNodes=iNodes-SFNodes
    
    IF (qxgauss) THEN
       FrontNodes=CEILING(REAL(WLNodes)/2.0_WP)+5_IP 
    ELSE
       FrontNodes=CEILING(REAL(WLNodes)/2.0_WP)
    END IF
    
    BackNodes=WLNodes-FrontNodes
    	
    a1gauss = 0.0_WP	
    						       
    a1gauss(BackNodes+1:BackNodes+SFNodes)=&
         gaussian(a1X(BackNodes+1:BackNodes+SFNodes),&
         (a1X(BackNodes+SFNodes)+a1X(BackNodes+1))/2.0_WP,sigma)
         
         
    DEALLOCATE(a1X)

  END SUBROUTINE Gaussnew
!********************************************************
  SUBROUTINE x2gauss(iZ2,sigmaz2,sLengthOfElm_Z2,&
       z2_center,a1Z2Gauss)
	
    IMPLICIT NONE
! Setting the seed field to be zero at outer edges
! and seed field only exist in the centre
! We are doing this to look at the diffraction and
! remove effects due to the boundary conditions
!
! ARGS:-
!
! iZ2                - INPUT   - Number of Nodes 
! sigmaz2            - INPUT   - The seed field sigma 
! sLengthOfElm_Z2    - INPUT   - The element size 
! a1Z2gauss          - OUTPUT  - The initial seed field 
!
    INTEGER(KIND=IP),INTENT(IN) :: iZ2
    REAL(KIND=WP),INTENT(IN)    :: sigmaz2
    REAL(KIND=WP),INTENT(IN)    :: sLengthOfElm_Z2
    REAL(KIND=WP),INTENT(IN)    :: z2_center
    REAL(KIND=WP), DIMENSION(iZ2),INTENT(OUT) :: a1Z2Gauss	
!
! Define local variables
!
! a1Ztwo         - Local array storing seed field for the z2 direction
! sTotalLength   - Total length in z2 direction
! SFNodes        - Number of nodes used for the seed field
!
    REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: a1Ztwo
    REAL(KIND=WP)    :: sstart,send,lseed

    INTEGER(KIND=IP) :: sstartn,sendn
    INTEGER(KIND=IP) :: center,tnodes


! Length of non-zero seed field - for now extend to 6*sigma (gaussian)
    lseed = 6.0_WP*sigmaz2

! Get start and end points of seed field distr...and check it's in bounds
    sstart = z2_center - (lseed / 2.0_WP)
    sstartn = FLOOR(sstart / sLengthOfElm_Z2) + 1_IP ! index of start node

    send = z2_center + (lseed / 2.0_WP)
    sendn = CEILING(send / sLengthOfElm_Z2) + 1_IP ! index of end node

    IF (sstartn < 1_IP) sstartn = 1_IP
    IF (sendn > iZ2) sendn = iZ2

    IF (sstartn > iZ2) PRINT*, 'WARNING, seed outside longitudinal bounds'
    IF (sendn < 1_IP) PRINT*, 'WARNING, seed outside longitudinal bounds'

    tnodes = sendn - sstartn + 1_IP

! Array for coordinates of each node
    ALLOCATE(a1Ztwo(tnodes))

    a1Ztwo = linspace(0.0_WP,REAL(tnodes-1)*sLengthOfElm_Z2,tnodes)

    a1Z2Gauss = 0.0_WP

    a1Z2Gauss(sstartn:sendn)=gaussian(a1Ztwo,(a1Ztwo(1)+a1Ztwo(tnodes))/2.0_WP,sigmaz2)
    
    DEALLOCATE(a1Ztwo)
  END SUBROUTINE x2gauss
  
!********************************************************
  SUBROUTINE GetInitFieldEnv(sSeed,sLengthOfElm,&
       z2_center,&
       sA0_Re,&
       iX,&
       iY,&
       iZ2,&
       sA0gauss_Re,&
       qOK)

    IMPLICIT NONE
!
! Calculate Initial Gauss Field
!
! ARGS:-
!
! sSeed                   - INPUT    - Descriptions of the seed field
! sLengthOfElm            - INPUT    - Element size in x,y,z2
! sA0_Re  		  - INPUT    - Initial field value (real)
! NX 	 	          - INPUT    - Number of X elements
! NY 	 	          - INPUT    - Number of Y elements
! Nz2 	 	          - INPUT    - Number of Z2 elements
! sA0gauss_Re             - OUTPUT   - Initial field over all planes 
! qOK                     - OUTPUT   - Error flag
!
    REAL(KIND=WP),    INTENT(IN)  :: sSeed(:)
    REAL(KIND=WP),    INTENT(IN)  :: sLengthOfElm(:)
    REAL(KIND=WP),    INTENT(IN)  :: z2_center
    REAL(KIND=WP),    INTENT(IN)  :: sA0_Re
    INTEGER(KIND=IP), INTENT(IN)  :: iX      
    INTEGER(KIND=IP), INTENT(IN)  :: iY      
    INTEGER(KIND=IP), INTENT(IN)  :: iZ2      
    REAL(KIND=WP),    INTENT(OUT) :: sA0gauss_Re(:)
    LOGICAL, INTENT(OUT) :: qOK                 
!
! Define local variables
! 
! iX	      - Number of nodes in X
! iY	      - Number of nodes in Y
! iZ2         - Number of nodes in Z2
! iXY         - Number of nodes in XY plane
! iS	      - Loop counter over z2 nodes
! iT          - Loop counter over xy nodes
! a1ZtwoGauss - Initial field in z2 plane
! a2XYgauss   - Initial field in XY plane
!
    INTEGER(KIND=IP)  :: iXY,iS,iT,endofelpulse,error
    REAL(KIND=WP),DIMENSION(:),ALLOCATABLE :: a1ZtwoGauss,a2XYgauss
!--------------------------------------------------------
! BEGIN:-
! Set error flag to false         
    qOK = .FALSE.
! Calculate the number of nodes in xy plane
    iXY = iX * iY
! Check nodes - Error log Subroutine in CIO.f90 line 709
    IF(iZ2 <= 0_IP) THEN
       CALL Error_log('iZ2 <=0.',tErrorLog_G)
       GOTO 1000    
    END IF
    IF(iXY <= 0_IP) THEN
       CALL Error_log('iXY <=0.',tErrorLog_G)
       GOTO 1000    
    END IF
! Allocate arrays
    IF (.NOT.(tTransInfo_G%qOneD) ) THEN
       ALLOCATE(a1ZtwoGauss(iZ2),a2XYgauss(iXY))
       IF (NX_G==2 .OR. sSeed(iX_CG)==1E8) THEN
          a2XYgauss = 1.0
       ELSE
! Calculate the initial condition on the transverse(x & y)
! plane - see line 2108 in this file
          CALL MPI_BARRIER(tProcInfo_G%comm,error)
          CALL xygaussnew(iX,&
               iY,&
               sSeed(iX_CG),&
               sSeed(iY_CG),&
               sLengthOfElm(iX_CG),&
               sLengthOfElm(iY_CG),&
               a2XYgauss)
       END IF
    ELSE
       ALLOCATE(a1ZtwoGauss(iZ2),a2XYgauss(iXY))
       a2XYgauss = 1.0_WP
    END IF
! Calculate the initial condition on the z2 plane
! - see line 2242 in this file
    CALL x2gauss(iZ2,&
         sSeed(iZ2_CG),&
         sLengthOfElm(iZ2_CG),&
         z2_center,&
         a1ZtwoGauss)

! Construct the full 3D field envelope


    DO iS=1,iZ2
       DO iT=1,iXY
          sA0gauss_Re(iT+iXY*(iS-1)) = a2XYgauss(iT)*&
               a1ZtwoGauss(iS)*sA0_Re
       ENDDO
    ENDDO

    IF (tTransInfo_G%qOneD) THEN
       DEALLOCATE(a2XYgauss)
    ENDIF
    DEALLOCATE(a1ZtwoGauss)      
!--------------------------------------------------------
!  Set error flag and exit         
    qOK = .TRUE.				    
    GOTO 2000
! Error Handler - Error log Subroutine in CIO.f90 line 709
1000 CALL Error_log('Error in BMathlib:CalculateInitialGaussField',&
          tErrorLog_G)
    PRINT*,'Error in BMathlib:CalculateInitialGaussField'
2000 CONTINUE
  END SUBROUTINE GetInitFieldEnv
  
!*****************************************************

SUBROUTINE getSeeds(NN,sigs,cens,magxs,magys,qFTs,rho,frs,nSeeds,dels,xfieldt,yfieldt)

!             ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN) :: NN(:)
  REAL(KIND=WP), INTENT(IN) :: sigs(:,:), cens(:), rho, frs(:), &
                               magxs(:), magys(:), dels(:)
  LOGICAL, INTENT(IN) :: qFTs(:)
  INTEGER(KIND=IP), INTENT(IN) :: nSeeds
  REAL(KIND=WP), INTENT(OUT) :: xfieldt(:), yfieldt(:)

!            LOCAL ARGS

  INTEGER(KIND=IP) :: ind
  REAL(KIND=WP) :: xfield(size(xfieldt)), yfield(size(yfieldt))


  xfieldt = 0.0_WP
  yfieldt = 0.0_WP

  DO ind = 1, nSeeds
  
    CALL getSeed(NN(:),sigs(ind,:),cens(ind),magxs(ind),magys(ind),qFTs(ind),rho,frs(ind), &
                   dels,xfield,yfield)

    xfieldt = xfieldt + xfield
    yfieldt = yfieldt + yfield
    
  END DO

END SUBROUTINE getSeeds

!*****************************************************

SUBROUTINE getSeed(NN,sig,cen,magx,magy,qFT,rho,fr,dels,xfield,yfield)

  IMPLICIT NONE

!             ARGUMENTS

  INTEGER(KIND=IP), INTENT(IN) :: NN(:)
  REAL(KIND=WP), INTENT(IN) :: sig(:), cen, rho, fr, magx, magy, dels(:)
  LOGICAL, INTENT(IN) :: qFT
  REAL(KIND=WP), INTENT(OUT) :: xfield(:), yfield(:)

!             LOCAL ARGS

  REAL(KIND=WP) :: xnds(NN(iX_CG)), ynds(NN(iY_CG)), &
                   z2nds(NN(iZ2_CG)), &
                   xenv(NN(iX_CG)), yenv(NN(iY_CG)), &
                   z2env(NN(iZ2_CG)), oscx(NN(iZ2_CG)), &
                   oscy(NN(iZ2_CG))
 
  REAL(KIND=WP) :: lx, ly, lz2
                   
  INTEGER(KIND=IP) :: ind1, ind2, ind3, gind

!     Sample length of the field in each dimension

  lx = dels(iX_CG) * (NN(iX_CG) - 1_IP)
  ly = dels(iY_CG) * (NN(iY_CG) - 1_IP)
  lz2 = dels(iZ2_CG) * (NN(iZ2_CG) - 1_IP)

!     Coordinates of field nodes in x, y and z2 (sample points)

  IF (NN(iX_CG) == 1_IP) THEN
    xnds = 1_WP
  ELSE
    xnds = linspace(-lx/2_WP, lx/2_WP, NN(iX_CG))
  END IF  


  IF (NN(iY_CG) == 1_IP) THEN
    ynds = 1_WP
  ELSE
    ynds = linspace(-ly/2_WP, ly/2_WP, NN(iY_CG))
  END IF  

  z2nds = linspace(0.0_WP,lz2,NN(iZ2_CG))

!     Profile in each dimension

  IF (NN(iX_CG) == 1_IP) THEN
    xenv = 1_WP
  ELSE
    xenv = gaussian(xnds,0.0_WP,sig(iX_CG))
  END IF

  
  IF (NN(iY_CG) == 1_IP) THEN
    yenv = 1_WP
  ELSE
    yenv = gaussian(ynds,0.0_WP,sig(iY_CG))    
  END IF
   

  IF (qFT) THEN
    
    WHERE (z2nds < (cen - sig(iZ2_CG)))

      z2env = 0.0_WP

    ELSEWHERE (z2nds > (cen + sig(iZ2_CG)))

        z2env = 0.0_WP

    ELSEWHERE

      z2env = 1.0_WP

    END WHERE

  ELSE
  
    z2env = gaussian(z2nds,cen,sig(iZ2_CG))

  END IF


!     x and y polarized fields in z2

  oscx = z2env * cos(fr * z2nds / (2.0_WP * rho))
  oscy = z2env * sin(fr * z2nds / (2.0_WP * rho))


!     Full 3D field

  do ind1 = 1, NN(iZ2_CG)
    do ind2 = 1,NN(iY_CG)
      do ind3 = 1,NN(iX_CG)
        
        gind = ind3 + NN(iX_CG)*(ind2-1) + NN(iX_CG)*NN(iY_CG)*(ind1-1)
        xfield(gind) = magx * xenv(ind3) * yenv(ind2) * oscx(ind1)
        yfield(gind) = magy * xenv(ind3) * yenv(ind2) * oscy(ind1)
      
      end do
    end do     
  end do

END SUBROUTINE getSeed

END MODULE setupcalcs
