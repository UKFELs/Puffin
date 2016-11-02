!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE sddsPuffin

USE ArrayFunctions
USE TypesandConstants
USE Globals
USE ParallelSetUp
Use avWrite
use sddsROutput
use createSDDS
use parafield


implicit none

CONTAINS

  SUBROUTINE  WriteParameterData(zDataFileName, &
       iNodes,&
       iNumElectrons, &
       sLengthOfElm, &
       sStepSize, &
       nSteps, &
       sLenEPulse, &
       sWigglerLength, &
       sSigmaGaussian, &
       sA0_Re, &
       sA0_Im, &
       rho,aw,epsilon,gamma_r, &
       kbeta, &
       lam_w, lam_r, &
       l_g, l_c, &
       npk_bar, &
       totalNumberElectrons, &
       nWaveEquations, &
       nElectronEquations, &  
       sZ, &
       iWriteNthSteps, &
       iIntWriteNthSteps, &
       sSeedSigma, &
       qSwitch, &
       fx, &
       fy, &
       qOK)

! Write input data used to create results
!
! zDataFileName      - INPUT  - Data file name
! iNodes             - INPUT  - Number of Nodes
! iNumElectrons      - INPUT  - number of electrons
! sLengthOfElm       - INPUT  - Element length
! sStepSize          - INPUT  - Integration step size
! nSteps             - INPUT  - Number of steps 
! sLenEPulse 	     - INPUT  - L-electron pulse
! sWigglerLength     - INPUT  - Wiggler length
! sSigmaGaussian     - INPUT  - e-pulse sigma
! sA0_Re,            - INPUT  - Initial field value (real)
! sA0_Im,            - INPUT  - Initial field value (imag)
! iTotalNumElectrons - INPUT  - Acutal Number of electrons used
! nWaveEquations     - INPUT  - Number of Wave Equations
! nElectronEquations - INPUT  - Number of Electron Equations
! sZ                 - UPDATE - IN: Starting z position
! iWriteNthSteps     - UPDATE - Steps to write data at
! sSeedSigma         - INPUT  - Sigma of initial seed field
! qSwitch            - UPDATE - Optional if letting electrons
!                               evolve, field evolve,
!                               diffraction,
!                               gauss inital field
! sx0_offset         - INPUT  - Electron offset value
! sy0_offset         - INPUT  - Electron offset value
! qOK                - OUTPUT - Error flag
!
    IMPLICIT NONE
!
    CHARACTER(1024_IP), INTENT(IN) :: zDataFileName
    INTEGER(KIND=IP), INTENT(IN) :: iNodes(:)
    INTEGER(KIND=IP), INTENT(IN) :: iNumElectrons(:)
    REAL(KIND=WP),    INTENT(IN) :: sLengthOfElm(:)
    REAL(KIND=WP),    INTENT(IN) :: sStepSize
    INTEGER(KIND=IP), INTENT(IN) :: nSteps
    REAL(KIND=WP),    INTENT(IN) :: sLenEPulse(:)   
    REAL(KIND=WP),    INTENT(IN) :: sWigglerLength(:) 
    REAL(KIND=WP),    INTENT(IN) :: sSigmaGaussian(:)
    REAL(KIND=WP),    INTENT(IN) :: sA0_Re   
    REAL(KIND=WP),    INTENT(IN) :: sA0_Im   
    REAL(KIND=WP),    INTENT(IN) :: rho,aw,epsilon,gamma_r
    REAL(KIND=WP),    INTENT(IN) :: kbeta
    real(kind=wp),    intent(in) :: lam_w, lam_r, l_g, l_c
    real(kind=wp),    intent(in) :: npk_bar
    INTEGER(KIND=IPL), INTENT(IN) :: totalNumberElectrons
    INTEGER(KIND=IP), INTENT(IN) :: nWaveEquations    
    INTEGER(KIND=IP), INTENT(IN) :: nElectronEquations
    REAL(KIND=WP),    INTENT(IN) :: sZ
    INTEGER(KIND=IP), INTENT(IN) :: iWriteNthSteps, iIntWriteNthSteps
    REAL(KIND=WP),    INTENT(IN) :: sSeedSigma(:)
    LOGICAL,          INTENT(IN) :: qSwitch(:)
    REAL(KIND=WP),    INTENT(IN) :: fx,fy
  
    LOGICAL,          INTENT(OUT) :: qOK      
!
! Define local variables
! 
! tParamFile   - Write Parameter data to file
! qOKL         - Local error flag
!	
    TYPE(cFileType) :: tParamFile
    LOGICAL         :: qOKL
!********************************************************
! BEGIN:-
! Set error flag to false         
    qOK = .FALSE.    

    If (tProcInfo_G%qROOT) Then

! Open the file to receive data output -
! This subroutine is in IO.f90 line 793
       tParamFile%qFormatted = .TRUE.
       call InitBasicSDDSFile('Param' // TRIM(zDataFileName), &
            tParamFile, &
            qOKL)
       If (.NOT. qOKL) Goto 1000
! Write variables names that are going to be written
! to the files (in order of output)   
! This subroutine is in BsddsWriter.f90 line 73   
       call SddsWriteParameter('nX','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('nY','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('nZ2','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('iNumElectronsX','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('iNumElectronsY','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('iNumElectronsZ2','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('iNumElectronsPX','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('iNumElectronsPY','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('iNumElectronsGam','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sLengthOfElmX','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sLengthOfElmY','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sLengthOfElmZ2','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sStepSize','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('nSteps','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sLenEPulseX','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sLenEPulseY','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sLenEPulseZ2','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sLenEPulsePX','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sLenEPulsePY','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sLenEPulseGam','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sWigglerLengthX','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sWigglerLengthY','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sWigglerLengthZ2','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sSigmaGaussianX','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sSigmaGaussianY','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sSigmaGaussianZ2','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sSigmaGaussianPX','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sSigmaGaussianPY','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sSigmaGaussianGam','double',&
            tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sA0_Re','double',tFileType=tParamFile)	   
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sA0_Im','double',tFileType=tParamFile)	   
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('rho','double',tFileType=tParamFile)	   
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('aw','double',tFileType=tParamFile)	  
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('eta','double',tFileType=tParamFile)	  
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('gamma_r','double',tFileType=tParamFile)	  
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('k_beta','double',tFileType=tParamFile)   
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('lambda_w','double',tFileType=tParamFile)   
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('lambda_r','double',tFileType=tParamFile)   
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('Lg','double',tFileType=tParamFile)   
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('Lc','double',tFileType=tParamFile)   
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('npk_bar','double',tFileType=tParamFile)   
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('totalNumberElectrons','long',&
            tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('nWaveEquations','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('nElectronEquations','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sZ','double',tFileType=tParamFile)	  
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('iWriteNthSteps','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('iIntWriteNthSteps','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sSeedSigmaX','double',tFileType=tParamFile)	
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sSeedSigmaY','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('sSeedSigmaZ2','double',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('qElectronsEvolve','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('qFieldEvolve','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('qDiffraction','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('qElectronFieldCoupling',&
            'long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('qFocussing','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('qMatchedBeam','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('qOneD','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('fx','double',tFileType=tParamFile)	   
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('fy','double',tFileType=tParamFile)	   
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('kappa','double',tFileType=tParamFile)     
       If (.NOT. qOKL) Goto 1000


! Write data mode - This subroutine is in BsddsWriter.f90  line 316
       call SddsWriteDataMode('ascii',tFileType=tParamFile)	
! Write data - These subroutines is in CIO.f90 line 100,166 and 25
       call WriteINTEGER(iNodes(iX_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteINTEGER(iNodes(iY_CG),tParamFile,qOKL)  
       If (.NOT. qOKL) Goto 1000
       call WriteINTEGER(iNodes(iZ2_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteINTEGER(iNumElectrons(iX_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteINTEGER(iNumElectrons(iY_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteINTEGER(iNumElectrons(iZ2_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteINTEGER(iNumElectrons(iPX_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteINTEGER(iNumElectrons(iPY_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteINTEGER(iNumElectrons(iGam_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sLengthOfElm(iX_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sLengthOfElm(iY_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sLengthOfElm(iZ2_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sStepSize,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteINTEGER(nSteps,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sLenEPulse(iX_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sLenEPulse(iY_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sLenEPulse(iZ2_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sLenEPulse(iPX_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sLenEPulse(iPY_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sLenEPulse(iGam_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sWigglerLength(iX_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sWigglerLength(iY_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sWigglerLength(iZ2_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sSigmaGaussian(iX_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sSigmaGaussian(iY_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sSigmaGaussian(iZ2_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sSigmaGaussian(iPX_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sSigmaGaussian(iPY_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sSigmaGaussian(iGam_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sA0_Re,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sA0_Im,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(rho,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(aw,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(epsilon,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(gamma_r,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(kbeta,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(lam_w,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(lam_r,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(l_g,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(l_c,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(npk_bar,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteINTEGERL(totalNumberElectrons,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteINTEGER(nWaveEquations,tParamFile,qOKL) 
       If (.NOT. qOKL) Goto 1000
       call WriteINTEGER(nElectronEquations,tParamFile,qOKL) 
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sZ,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteINTEGER(iWriteNthSteps,tParamFile,qOKL) 
       If (.NOT. qOKL) Goto 1000
       call WriteINTEGER(iIntWriteNthSteps,tParamFile,qOKL) 
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sSeedSigma(iX_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sSeedSigma(iY_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sSeedSigma(iZ2_CG),tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       !
       call WriteLOGICINTEGER(qSwitch(iElectronsEvolve_CG),tParamFile,qOKL) 
       If (.NOT. qOKL) Goto 1000
       call WriteLOGICINTEGER(qSwitch(iFieldEvolve_CG),tParamFile,qOKL) 
       If (.NOT. qOKL) Goto 1000
       call WriteLOGICINTEGER(qSwitch(iDiffraction_CG),tParamFile,qOKL) 
       If (.NOT. qOKL) Goto 1000
       call WriteLOGICINTEGER(qSwitch(iElectronFieldCoupling_CG),&
            tParamFile,qOKL) 
       If (.NOT. qOKL) Goto 1000
       call WriteLOGICINTEGER(qSwitch(iFocussing_CG),tParamFile,qOKL) 
       If (.NOT. qOKL) Goto 1000
       call WriteLOGICINTEGER(qSwitch(iMatchedBeam_CG),tParamFile,qOKL) 
       If (.NOT. qOKL) Goto 1000
       call WriteLOGICINTEGER(qOneD_G,tParamFile,qOKL) 
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(fx,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(fy,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(sKappa_G,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000



! close data file - This subroutine is in CIO.f90 line 560       
       call CloseFile(tParamFile, qOKL) 
       If (.NOT. qOKL) Goto 1000
    End If

!  Set error flag and exit         
    qOK = .TRUE.				    
    GoTo 2000     

! Error Handler - Error log Subroutine in CIO.f90 line 709
1000 call Error_log('Error in sddsPuffin:WriteParameterData',&
          tErrorLog_G)
    Print*,'Error in sddsPuffin:WriteParameterData'
2000 CONTINUE
  END SUBROUTINE WriteParameterData



























  subroutine wr_sdds(sZ, istep, tArrayA, tArrayE, tArrayZ, &
                     iIntWr, iWr, qSep, zDFname, qWriteFull, &
                     qWriteInt, qOK)

    implicit none

! Write Data FileS


    real(kind=wp), intent(in) :: sZ
    type(cArraySegment), intent(inout) :: tArrayA(:), tArrayE(:), tArrayZ
    integer(kind=ip), intent(in) :: istep
    integer(kind=ip), intent(in) :: iIntWr, iWr
    character(1024_IP), intent(in) :: zDFName
    logical, intent(in) :: qSep
    logical, intent(inout) :: qOK

    logical :: qWriteInt, qWriteFull, qOKL

    if (qWriteFull) then

      call outputBeamFiles(tArrayE, iStep, qSep, zDFName, qOKL)
      if (.not. qOKL) goto 1000

      call outputField(tArrayA, iStep, qSep, zDFName, qOKL)
      if (.not. qOKL) goto 1000

      call outputZ(sZ, tArrayZ, iStep, qSep, zDFName, qOKL)
      if (.not. qOKL) goto 1000

    end if

    

     if (qWriteInt) then
 
       call writeIntData()
     
     end if


!  Set error flag and exit         
    qOK = .TRUE.            
    goto 2000     

! Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call Error_log('Error in sddsPuffin:wr_sdds',&
          tErrorLog_G)
    print*,'Error in sddsPuffin:wr_sdds'
2000 continue

  end subroutine wr_sdds







!   subroutine wdfs(sA, sZ, istep, tArrayA, tArrayE, tArrayZ, &
!                   iIntWr, iWr, qSep, zDFname, qWDisp, qOK)

!     implicit none

! ! Write Data FileS


!     real(kind=wp), intent(in) :: sA(:), sZ
!     type(cArraySegment), intent(inout) :: tArrayA(:), tArrayE(:), tArrayZ
!     integer(kind=ip), intent(in) :: istep
!     integer(kind=ip), intent(in) :: iIntWr, iWr
!     character(1024_IP), intent(in) :: zDFName
!     logical, intent(in) :: qSep, qWDisp
!     logical, intent(inout) :: qOK

!     logical :: qWriteInt, qWriteFull, qOKL

!     qOK = .false.
    
!     qWriteInt = .false.
!     qWriteFull = .false.

!     if ((mod(iStep,iIntWr)==0) .or. (iStep == nSteps) .or. (iStep == 0) .or. (qWDisp) ) then

!       qWriteInt = .true.

!     end if




!     if ((mod(iStep,iWr)==0) .or. (iStep == nSteps) .or. (iStep == 0) .or. (qWDisp) ) then

!       qWriteFull = .true.

!     end if



!     if (qWriteFull) then

!       call outputBeamFiles(tArrayE, iStep, qSep, zDFName, qOKL)
!       if (.not. qOKL) goto 1000

!     end if


!     if (qWriteFull) then 

!       call outputField(sA, tArrayA, iStep, qSep, zDFName, qOKL)
!       if (.not. qOKL) goto 1000

!     end if

!     if (qWriteFull) then

!       call outputZ(sZ, tArrayZ, iStep, qSep, zDFName, qOKL)
!       if (.not. qOKL) goto 1000

!     end if

!     if (qWriteInt) then

!       call writeIntData(sA)
    
!     end if

! !     etc...

! !     So writeData below will become redundant




! !     Set error flag and exit

!     qOK = .true.
!     goto 2000


! !     Error Handler - Error log Subroutine in CIO.f90 line 709

! 1000 call Error_log('Error in sddsPuffin:wdfs',tErrorLog_G)
!     print*,'Error in sddsPuffin:wdfs'

! 2000 continue


!   end subroutine wdfs








  subroutine createFFiles(tArrayY, zDFName, zOptionalString, qOK)

    implicit none

! Create "Full" Files - creates either 
! the full data sets for the field and 
! electron phase space.

    type(cArraySegment), intent(inout) :: tArrayY(:)
    character(1024_IP), intent(in)   ::   zDFName
    character(*), intent(in), optional  :: zOptionalString
    logical, intent(inout) :: qOK

    integer(kind=ip) :: iap
    character(1024_IP) :: zFileName
    logical :: qOptional, qOKL



    qOK = .false.


    if (present(zOptionalString)) then

      if (len(trim(adjustl(zOptionalString))) > 0) then

        qOptional = .TRUE.
    
      end if
  
    end if

!     Loop around array segments, creating files

    do iap = 1, size(tArrayY)

      if (tArrayY(iap)%qWrite) then
        
        if (tProcInfo_G%qRoot) then

!     Prepare filename      

          zFilename = (trim(adjustl(tArrayY(iap)%zVariable)) // trim(adjustl(zDFName))) 

          if (qOptional) then

            zFilename = (trim(adjustl(zOptionalString)) // '_' // trim(adjustl(zFilename)) )

          end if


          call CreateSDDSFile(zFilename, &
                              tArrayY(iap)%zVariable, &
                              tArrayY(iap)%tFileType, &
                              qOKL)    
      

        end if


      end if

    end do

!     Set error flag and exit

    qOK = .true.
    goto 2000


!     Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call Error_log('Error in sddsPuffin:createFFiles',tErrorLog_G)
    print*,'Error in sddsPuffin:createFFiles'

2000 continue


  end subroutine createFFiles









  subroutine outputField(tArrayA, iStep, qSeparate, zDFName, qOK)

    implicit none

! Output radiation field from Puffin

! Arguments

    type(cArraySegment), intent(inout) :: tArrayA(:)
    integer(kind=ip), intent(in) :: iStep
    logical, intent(in) :: qSeparate
    character(1024_IP), intent(in) :: zDFName
    logical, intent(inout) :: qOK


! Local vars...

    integer(kind=ip) :: ifp, fieldsize
    logical :: qOKL



    qOK = .false.




!    if (qSeparate) then

      call createFFiles(tArrayA, zDFName, trim(IntegerToString(iStep)),qOKL)
      if (.not. qOKL) goto 1000

!    end if


    call writeParaField(tArrayA(1)%tFileType, tArrayA(2)%tFileType)


!     Write out field data:-only root processor needs to do this
    
!    fieldsize = SIZE(sA)/2_IP
!
!
!
!    if (tProcInfo_G%qRoot) then
!
!      do ifp = 1_IP, size(tArrayA)
!
!        if (tArrayA(ifp)%qWrite) then
!
!!     Write the data
!      
!          call OutputIntegrationData(tArrayA(ifp)%tFileType, &
!                                     sA(((ifp-1)*fieldsize) + 1: ifp*fieldsize), &
!                                     fieldsize, &
!                                     qOKL)
!
!          if (.not. qOKL) goto 1000
!
!        end if
!
!      end do
!
!    end if



!     Set error flag and exit

    qOK = .true.            
    goto 2000


!     Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call Error_log('Error in sddsPuffin:outputField',tErrorLog_G)
    print*,'Error in sddsPuffin:outputField'

2000 continue

  end subroutine outputField





















  subroutine outputBeamFiles(tArrayE, iStep, qSeparate, zDFName, qOK)


    implicit none

! Output the electron bean macroparticle 
! 6D phase space coordinates in Puffin.
! 
! sV        -      Electron coordinates
!
! tArrayE   -      Array describing the 
!                  layout of data in 
!                  sV
!
 
    type(cArraySegment), intent(inout) :: tArrayE(:)
    integer(kind=ip), intent(in) :: iStep
    logical, intent(in) :: qSeparate
    character(1024_IP), intent(in) :: zDFName
    logical, intent(inout) :: qOK

! Local vars

    integer(kind=ip) :: iep
    logical :: qOKL




    qOK = .false.

!     Create data files



!    if (qSeparate) then

      call createFFiles(tArrayE, zDFName, trim(IntegerToString(iStep)), qOKL)
      if (.not. qOKL) goto 1000

!    end if



!     Write full 6D electron phase space
!     to file. This will create very large
!     files!!!

    call wrt_phs_coord(iRe_X_CG, sElX_G, qOKL)
    call wrt_phs_coord(iRe_Y_CG, sElY_G, qOKL)
    call wrt_phs_coord(iRe_Z2_CG, sElZ2_G, qOKL)
    call wrt_phs_coord(iRe_PPerp_CG, sElPX_G, qOKL)
    call wrt_phs_coord(iIm_PPerp_CG, sElPY_G, qOKL)
    call wrt_phs_coord(iRe_Gam_CG, sElGam_G, qOKL)
    if (.not. qOKL) goto 1000

!     Set error flag and exit

    qOK = .true.            
    goto 2000


!     Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call Error_log('Error in sddsPuffin:outputBeamFiles',tErrorLog_G)
    print*,'Error in sddsPuffin:outputBeamFiles'

2000 continue


  end subroutine outputBeamFiles






  subroutine wrt_phs_coord(iPh_id,ph_coord,qOK)

    integer(kind=ip), intent(in) :: iPh_id
    real(kind=wp), intent(in) :: ph_coord(:)
    logical, intent(out) :: qOK

    logical :: qOKL

    qOK = .false.

    if (tArrayE(iPh_id)%qWrite) then

!     Write the data
      
      call OutputIntegrationData(tArrayE(iPh_id)%tFileType, &
                                 ph_coord, &
                                 tProcInfo_G%rank, &
                                 iGloNumElectrons_G, &
                                 qOKL)

      if (.NOT. qOKL) goto 1000

    end if


!     Set error flag and exit

    qOK = .true.            
    goto 2000


1000 call Error_log('Error in sddsPuffin:wrt_phs_coord',tErrorLog_G)
    print*,'Error in sddsPuffin:wrt_phs_coord'

2000 continue

  end subroutine wrt_phs_coord










  subroutine outputZ(sZ, tArrayZ, iStep, qSeparate, zDFName, qOK)

    implicit none

! Output radiation field from Puffin

! Arguments

    real(kind=wp), intent(in) :: sZ
    type(cArraySegment), intent(inout) :: tArrayZ
    integer(kind=ip), intent(in) :: iStep
    logical, intent(in) :: qSeparate
    character(1024_IP), intent(in) :: zDFName
    logical, intent(inout) :: qOK

! Local vars...

    logical :: qOKL





    qOK = .false.

!    if (qSeparate) then

      call createZFile(tArrayZ, zDFName, trim(IntegerToString(iStep)),qOKL)
      if (.not. qOKL) goto 1000

!    end if

!     Write out Zbar data:-only root processor needs to do this
    



    if (tProcInfo_G%qRoot) then

      if (tArrayZ%qWrite) then

!     Write the data
      
        call OutputIntegrationData(tArrayZ%tFileType, sZ, qOKL)

          if (.not. qOKL) goto 1000

      end if

    end if

!     Set error flag and exit

    qOK = .true.            
    goto 2000


!     Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call Error_log('Error in sddsPuffin:outputZ',tErrorLog_G)
    print*,'Error in sddsPuffin:outputZ'

2000 continue

  end subroutine outputZ










  subroutine createZFile(tArrayY, zDFName, zOptionalString, qOK)

    implicit none

! Create Files - creates file for Zbar

    type(cArraySegment), intent(inout) :: tArrayY
    character(1024_IP), intent(in)   ::   zDFName
    character(*), intent(in), optional  :: zOptionalString
    logical, intent(inout) :: qOK


    character(1024_IP) :: zFileName
    logical :: qOptional, qOKL



    qOK = .false.



    if (present(zOptionalString)) then

      if (len(trim(adjustl(zOptionalString))) > 0) then

        qOptional = .TRUE.

      end if
  
    end if

!     create files

    if (tArrayY%qWrite) then
        
      if (tProcInfo_G%qRoot) then

!     Prepare filename      

        zFilename = (trim(adjustl(tArrayY%zVariable)) // trim(adjustl(zDFName))) 

        if (qOptional) then

          zFilename = (trim(adjustl(zOptionalString)) // '_' // trim(adjustl(zFilename)) )

        end if


        call CreateSDDSFile(zFilename, &
                            tArrayY%zVariable, &
                            tArrayY%tFileType, &
                            qOKL)    
    

      end if


    end if




!     Set error flag and exit

    qOK = .true.
    goto 2000


!     Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call Error_log('Error in sddsPuffin:createZFile',tErrorLog_G)
    print*,'Error in sddsPuffin:createZFile'

2000 continue

  end subroutine createZFile


















































  SUBROUTINE WriteEleData(zDataFileName,fname,vname,&
       qFormatted,nelectrons,edata,qOK)                          

  IMPLICIT NONE
!
! This subroutine writes the electron chi data to
! file
!
! zDataFileName           - INPUT    - Data file name
! qFormatted              - INPUT    - Write formatted or binary file
! s_chi_bar		  - INPUT    -
! qOK                     - OUTPUT   - Error flag
!
  CHARACTER(*),INTENT(IN) :: zDataFileName
  CHARACTER(*),INTENT(IN) :: fname,vname
  LOGICAL, INTENT(IN) :: qFormatted
  INTEGER(KIND=IPL), INTENT(IN) :: nelectrons
  REAL(KIND=WP),   INTENT(IN) :: edata(:)
  LOGICAL,         INTENT(OUT):: qOK      
!
! Define local variables
! 
! tParamFile   - Write Parameter data to file
! qOKL         - Local error flag
!
  TYPE(cFileType) :: tParamFile
  INTEGER         :: error, i
  LOGICAL         :: qOKL



!     Set error flag to false

    qOK = .FALSE.    

!     Open the file to receive data output - 
!     This subroutine is in CIO.f90 line 793

    tParamFile%qFormatted = qFormatted




!     Only the root process does the file initialization

  IF (tProcInfo_G%rank==0) THEN

    call InitBasicSDDSFile(fname // TRIM(zDataFileName), &
         tParamFile, &
         qOKL)
    If (.NOT. qOKL) Goto 1000

!     Write variables names that are going to be written to 
!     the files (in order of output)   
!     This subroutine is in BsddsWriter.f90 line 228
    
    call SddsWriteColumn(vname,'double',tFileType=tParamFile)
    If (.NOT. qOKL) Goto 1000

!     Write data mode - This subroutine is in BsddsWriter.f90 line 316
    
    If (qFormatted) then
    
       call SddsWriteDataMode('ascii',tFileType=tParamFile)	
    
    Else
    
       call SddsWriteDataMode('binary',tFileType=tParamFile)
    
    End If

!     Set up new page - see CIO.f90 line 651         
    
    call WriteSDDSNewPage(tParamFile,qOKL)
    If (.NOT. qOKL) Goto 1000

!     Write length of column data - see CIO.f90 line 100         
    
    call WriteINTEGERL(nelectrons,tParamFile,qOKL)
    If (.NOT. qOKL) Goto 1000	 

!     Write data - These subroutines is in CIO.f90 line 232
    
    call CloseFile(tParamFile, &
                   qOKL)
    If (.NOT. qOKL) Goto 1000
		
  END IF 



!     Loop over processes and take in turns to write data		 
  
  CALL MPI_BARRIER(tProcInfo_G%comm, error)

!     File data was setup on process 0, need to share filetype with the
!     rest of the processors in the MPI communicator 

  CALL shareFileType(tParamFile)
       
  DO i = 0,tProcInfo_G%size-1

    IF (tProcInfo_G%rank == i) THEN

      call OpenFileForAppend(tParamFile%zFileName, &
             tParamFile,qOKL)

      call Write1DRealArray(edata,tParamFile,qOKL)      
      If (.NOT. qOKL) Goto 1000
          
      call CloseFile(tParamFile,qOKL)
       
    END IF
    
    CALL MPI_BARRIER(tProcInfo_G%comm, error)

  END DO



!     Set error flag and exit

    qOK = .TRUE.				    
    GoTo 2000     



!     Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call Error_log('Error in FEMethod:WriteChiData',tErrorLog_G)
    Print*,'Error in FEMethod:WriteChiData'
2000 CONTINUE

  END SUBROUTINE WriteEleData









!!!!!!!!!!!!!!!!!!!!   REDUNDANT ROUTINES

!   subroutine WriteData(qSeparateStepFiles,&
!                        zDataFileName,tArrayZ,tArrayA,tArrayE,&
!                        iStep,sZ,sA,sV,qStart,qFormattedFiles, &
!                        qOK)

!     implicit none

! ! Subroutine to write the data to files
! ! inputs:

!     CHARACTER(*),        INTENT(IN)  :: zDataFileName
!     LOGICAL,             INTENT(IN)  :: qSeparateStepFiles
!     TYPE(cArraySegment), INTENT(INOUT) :: tArrayZ, &
!                                           tArrayA(:),tArrayE(:)
!     INTEGER(KIND=IP),    INTENT(IN)  :: iStep
!     REAL(KIND=WP),       INTENT(IN)  :: sZ,sA(:),sV(:)
!     LOGICAL,             INTENT(IN)  :: qStart,qFormattedFiles
!     LOGICAL,             INTENT(OUT) :: qOK

! ! Local vars:

!     LOGICAL  :: qOKL


! !     Set error flag to false

!     qOK = .FALSE. 

! !     Open the file to receive data output - This
! !     subroutine is in EArrayFunctions.f90 line 449  
! !     IntegerToString FUNCTION see line 2469
    
!     if (qSeparateStepFiles .or. qStart) then

!        call CreateSDDSFiles(zDataFileName,&
!             qFormattedFiles,&
!             tArrayZ,&
!             tArrayA,&
!             tArrayE,&
!             qOKL,&
!             trim(IntegerToString(iStep)))   
!             if (.not. qOKL) goto 1000
    
!     end if


    
!     call WriteIntegrationData(sZ,&
!          sA, sV, &
!          tArrayZ, tArrayA,&
!          tArrayE, qOKL)
!     if (.not. qOKL) goto 1000


! !     Set error flag and exit         

!     qOK = .TRUE.				    
!     goto 2000 



! !     Error Handler - Error log Subroutine in CIO.f90 line 709

! 1000 call Error_log('Error in FsddsPuffin:WriteChiData',tErrorLog_G)
!     print*,'Error in FsddsPuffin:WriteChiData'

! 2000 CONTINUE

!   END SUBROUTINE WriteData
  
! !**********************************************************

!   SUBROUTINE WriteIntegrationData(sZ,sA,sY,tWriteZData,&
!        tWriteAData,tArraySegment,qOK)

! ! Write Integration data to file 
! !
! ! sZ                      - INPUT    - At Z
! ! sY                      - INPUT    - Result Y 
! ! iLenY                   - INPUT    - Length of Y
! ! tArraySegment           - UPDATE   - Description of segments 
! !                                      making up sY 
! ! qOK                     - OUTPUT   - Error flag

!     implicit none

!     REAL(KIND=WP),      INTENT(IN)      :: sZ
!     REAL(KIND=WP),      INTENT(IN)      :: sA(:)
!     REAL(KIND=WP),      INTENT(IN)      :: sY(:)
!     TYPE(cArraySegment),INTENT(INOUT)   :: tWriteZData
!     TYPE(cArraySegment),INTENT(INOUT)   :: tWriteAData(:)
!     TYPE(cArraySegment),INTENT(INOUT)   :: tArraySegment(:)
!     LOGICAL,            INTENT(OUT)     :: qOK      

! ! Define local variables
! !
! ! iSegment   - Current segment number 
! ! iStart     - Start position of data in array Y
! ! iEnd       - End position of data in array Y
! ! qOKL       - Local error flag

!     INTEGER(KIND=IP)               :: iSegment
!     INTEGER(KIND=IP)               :: iStart
!     INTEGER(KIND=IP)               :: iEnd, fieldsize, error
!     REAL(KIND=WP), DIMENSION(:), ALLOCATABLE :: write_in 
!     LOGICAL                        :: qOKL

! !     Set error flag to false
    
!     qOK = .FALSE.

! !     Write out field data:-only root processor needs to do this
    
!     fieldsize = SIZE(sA)/2_IP



!     if (tProcInfo_G%qRoot) then
       
!        if (tWriteAData(iRe_A_CG)%qWrite) then
          
!           allocate(write_in(fieldsize))

!           write_in = Vector(iRe_A_CG,sA)
        	
!           call OutputIntegrationData(tWriteAData(iRe_A_CG)%tFileType,&
!                                      write_in,fieldsize,qOKL)

!           deallocate(write_in)

!        end if

!     end if




!     if (tProcInfo_G%qRoot) then

!        if (tWriteAData(iIm_A_CG)%qWrite) then

!           allocate(write_in(fieldsize))

!           write_in = Vector(iIm_A_CG,sA)
          
!           call OutputIntegrationData(tWriteAData(iIm_A_CG)%tFileType,&
!                                      write_in,fieldsize,qOKL)

!           deallocate(write_in)

!        end if

!     end if



! !     Write Electron Segments to file      
! !     OutputIntegrationData routines are
! !     either starts at line 491 or 585 in this file 

!     do iSegment = 1_IP, SIZE(tArraySegment)

!        if (tArraySegment(iSegment)%qWrite) then
     
!           iStart = tArraySegment(iSegment)%iStart
!           iEnd   = tArraySegment(iSegment)%iEnd

! !     Write the data
      
!           call OutputIntegrationData(tArraySegment(iSegment)%tFileType, &
!                sY(iStart:iEnd),tProcInfo_G%rank,iGloNumElectrons_G,&
!                qOKL)
!           if (.NOT. qOKL) goto 1000

!        end if

!     end do



! !     Write out z data

!     if (tProcInfo_G%qRoot) then

!        If (tWriteZData%qWrite) then

!           call OutputIntegrationData(tWriteZData%tFileType, &
!                                      sZ,qOKL)
!           if (.not. qOKL) goto 1000
       
!        end if

!     end if

! !     Set error flag and exit         

!     qOK = .TRUE.				    
!     goto 2000     

! !     Error Handler - Error log Subroutine in CIO.f90 line 709

! 1000 call Error_log('Error in FsddsPuffin:WriteIntegrationData',tErrorLog_G)
!     print*,'Error in FsddsPuffin:WriteIntegrationData'

! 2000 continue

!   end subroutine WriteIntegrationData

!!!!!!!!!!!!!!!!!!!!!!! END REDUNDANT ROUTINES












	
FUNCTION IntegerToString(iInteger)

! Convert an integer into a string

! iInteger    - INPUT  - Integer to convert

! Define variables

	IMPLICIT NONE

        INTEGER(KIND=IP),          INTENT(IN)                   :: iInteger
        CHARACTER(32_IP)               	                        :: IntegerToString                                          

! Define local variables

        CHARACTER(32_IP) :: zCharacter

! Write character to internal file      

      write(zCharacter,*) iInteger

! Output without blanks      

      IntegerToString = TRIM(ADJUSTL(zCharacter))

!  Set error flag and exit         

       GoTo 2000     

! Error Handler - Error log Subroutine in CIO.f90 line 709
         
1000 call Error_log('Error in sddsPuffin:IntegerToString',tErrorLog_G)
      Print*,'Error in sddsPuffin:IntegerToString'
2000 CONTINUE
     
END FUNCTION IntegerToString





END MODULE sddsPuffin
