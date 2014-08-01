!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE checks

USE Paratype
USE ParallelInfoType
USE TransformInfoType
USE IO
USE typesAndConstants


IMPLICIT NONE

CONTAINS

SUBROUTINE CheckParameters(sLenEPulse,iNumElectrons,nbeams,&
       sLengthofElm,iNodes,sWigglerLength,sStepSize,&
       nSteps,srho,saw,sgammar,focusfactor,sSigE,f_x, f_y, iRedNodesX,&
       iRedNodesY,qSwitches,qSimple,qOK)

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
  REAL(KIND=WP), INTENT(IN) :: srho,saw,sgammar,focusfactor
  REAL(KIND=WP), INTENT(INOUT) :: sSigE(:,:)
  REAL(KIND=WP), INTENT(INOUT) :: f_x, f_y
  INTEGER(KIND=IP),INTENT(INOUT) :: iRedNodesX,iRedNodesY
  LOGICAL, INTENT(INOUT) :: qSwitches(:)
  logical, intent(in) :: qSimple
  LOGICAL, INTENT(OUT)  :: qOK

!           Local vars

  INTEGER(KIND=IP) :: i,nninner, error
  REAL(KIND=WP) :: sEE,dx,lwx,lexm,maxr
  LOGICAL :: qOKL  

  qOK = .FALSE.

  if (qSimple) call check1D(qSwitches(iOneD_CG), qSwitches(iDiffraction_CG), qSwitches(iFocussing_CG), &
  	                        iNodes)

  do i = 1,nbeams

    if (qSimple) then

      call chkESampleLens(sLenEPulse(i,:),iNumElectrons(i,:),srho,qSwitches(iOneD_CG),qOKL)
      if (.NOT. qOKL) goto 1000

    end if  

  end do

  call stpFSampleLens(iNodes,sWigglerLength,sLengthOfElm,qSwitches(iOneD_CG),qOKL)

  if (qSimple) then

    call chkFSampleLens(iNodes,sWigglerLength,sLengthOfElm,srho,qOKL)
    if (.NOT. qOKL) goto 1000

    call checkIntSampling(sStepSize,nSteps,sLengthOfElm,qSwitches,qOKL)
    if (.NOT. qOKL) goto 1000

    call checkFreeParams(srho,saw,sgammar,f_x,f_y,focusfactor,qOKL)
    if (.NOT. qOKL) goto 1000

  end if

  if (tProcInfo_G%qRoot) print '(I5,1X,A17,1X,F8.4)',nsteps,&
       'Step(s) and z-bar=',nsteps*sStepSize
  
!     Set error flag and exit

  qOK = .true.

  goto 2000

1000 call Error_log('Error in setupcalcs:CheckParameters',tErrorLog_G)

2000 continue

END SUBROUTINE CheckParameters
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine check1D(qOneD, qDiffraction, qfocusing, iNodes)

! If 1D, make sure no diffraction or focusing is employed
!
! -Lawrence

  logical, intent(in) :: qOneD
  logical, intent(inout) :: qDiffraction, qfocusing
  integer(kind=ip), intent(inout) :: iNodes(:)

  if (qOneD) then

    qDiffraction = .false.
    qfocusing = .false.

    iNodes(iX_CG) = 1_IP
    iNodes(iY_CG) = 1_IP

  end if


end subroutine check1D

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
  INTEGER(KIND=IP), INTENT(OUT) :: iNumElectrons(:)
  LOGICAL, INTENT(IN) :: qOneD
  LOGICAL, INTENT(INOUT) :: qOK

!                 LOCAL ARGS

  INTEGER(KIND=IP) :: i
  REAL(KIND=WP) :: wlen, maxspcing, spcing
  
  qOK = .FALSE.

  if (qOneD) then
    iNumElectrons(iX_CG)  = 1_IP
    iNumElectrons(iY_CG)  = 1_IP
    iNumElectrons(iPX_CG) = 1_IP
    iNumElectrons(iPY_CG) = 1_IP
  end if

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
  
  !IF (qSwitches(iFieldEvolve_CG)) THEN

    !IF( sStepSize >= (0.5_WP * sLengthOfElm(iZ2_CG))) THEN
      !CALL Error_log('The integration step size is too large.',tErrorLog_G)
      !GOTO 1000
    !END IF

  !END IF

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

SUBROUTINE checkFreeParams(srho,saw,sgammar,f_x,f_y,focusfactor,qOK)

  REAL(KIND=WP), INTENT(IN) :: srho,saw,sgammar,f_x,f_y,focusfactor
  LOGICAL, INTENT(OUT) :: qOK

  qOK = .FALSE.

!     Check got valid value for rho

  IF (srho <= 0.0_WP) THEN
    CALL Error_log('Parameter rho <=0.',tErrorLog_G)
    GOTO 1000    
  END IF

!     Check got valid value for aw and gamma
  
  IF (saw <= 0.0_WP) THEN
    CALL Error_log('Parameter saw <=0.',tErrorLog_G)
    GOTO 1000    
  END IF
  
  IF (sgammar <= 0.0_WP) THEN
    CALL Error_log('Parameter gamma_r <= 0.',tErrorLog_G)
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

end module checks
