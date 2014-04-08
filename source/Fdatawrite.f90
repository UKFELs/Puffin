!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE DataWrite

USE ArrayFunctions
USE TypesandConstants
USE Globals
USE ParallelSetUp

INTERFACE OutputIntegrationData
   MODULE PROCEDURE OutputIntegrationData_RealValue, &
        OutputIntegrationData_RealArray, &
        OutputIntegrationData_ParRealArray   
END INTERFACE

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
       totalNumberElectrons, &
       nWaveEquations, &
       nElectronEquations, &  
       sZ, &
       iWriteNthSteps, &
       sSeedSigma, &
       qSwitch, &
       fx, &
       fy, &
       qOK)                          
!--------------------------------------------------------
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
    CHARACTER(32_IP), INTENT(IN) :: zDataFileName
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
    INTEGER(KIND=IPL), INTENT(IN) :: totalNumberElectrons
    INTEGER(KIND=IP), INTENT(IN) :: nWaveEquations    
    INTEGER(KIND=IP), INTENT(IN) :: nElectronEquations
    REAL(KIND=WP),    INTENT(IN) :: sZ
    INTEGER(KIND=IP), INTENT(IN) :: iWriteNthSteps
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
       call InitialiseSDDSFile('Param' // TRIM(zDataFileName), &
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
       call SddsWriteParameter('iNumElectronsPZ2','long',tFileType=tParamFile)
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
       call SddsWriteParameter('sLenEPulsePZ2','double',tFileType=tParamFile)
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
       call SddsWriteParameter('sSigmaGaussianPZ2','double',&
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
       call SddsWriteParameter('epsilon','double',tFileType=tParamFile)	  
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('gamma_r','double',tFileType=tParamFile)	  
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
       call SddsWriteParameter('qFocussing_CG','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('qMatchedBeam_CG','long',tFileType=tParamFile)
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('fx','double',tFileType=tParamFile)	   
       If (.NOT. qOKL) Goto 1000
       call SddsWriteParameter('fy','double',tFileType=tParamFile)	   
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
       call WriteINTEGER(iNumElectrons(iPZ2_CG),tParamFile,qOKL)
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
       call WriteRealNumber(sLenEPulse(iPZ2_CG),tParamFile,qOKL)
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
       call WriteRealNumber(sSigmaGaussian(iPZ2_CG),tParamFile,qOKL)
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
       call WriteRealNumber(fx,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000
       call WriteRealNumber(fy,tParamFile,qOKL)
       If (.NOT. qOKL) Goto 1000

! close data file - This subroutine is in CIO.f90 line 560       
       call CloseFile(tParamFile, qOKL) 
       If (.NOT. qOKL) Goto 1000
    End If

!  Set error flag and exit         
    qOK = .TRUE.				    
    GoTo 2000     

! Error Handler - Error log Subroutine in CIO.f90 line 709
1000 call Error_log('Error in FEMethod:WriteParameterData',&
          tErrorLog_G)
    Print*,'Error in FEMethod:WriteParameterData'
2000 CONTINUE
  END SUBROUTINE WriteParameterData
!********************************************************

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
!********************************************************
! Set error flag to false         
    qOK = .FALSE.    

! Open the file to receive data output - 
! This subroutine is in CIO.f90 line 793
    tParamFile%qFormatted = qFormatted
!
! Only the root process does the file initialization
  IF (tProcInfo_G%rank==0) THEN
    call InitialiseSDDSFile(fname // TRIM(zDataFileName), &
         tParamFile, &
         qOKL)
    If (.NOT. qOKL) Goto 1000

! Write variables names that are going to be written to 
! the files (in order of output)   
! This subroutine is in BsddsWriter.f90 line 228
    call SddsWriteColumn(vname,'double',tFileType=tParamFile)
    If (.NOT. qOKL) Goto 1000

! Write data mode - This subroutine is in BsddsWriter.f90 line 316
    If (qFormatted) then
       call SddsWriteDataMode('ascii',tFileType=tParamFile)	
    Else
       call SddsWriteDataMode('binary',tFileType=tParamFile)
    End If

!  Set up new page - see CIO.f90 line 651         
    call WriteSDDSNewPage(tParamFile,qOKL)
    If (.NOT. qOKL) Goto 1000

!  Write length of column data - see CIO.f90 line 100         
    call WriteINTEGERL(nelectrons,tParamFile,qOKL)
    If (.NOT. qOKL) Goto 1000	 

! Write data - These subroutines is in CIO.f90 line 232
    call CloseFile(tParamFile, &
         qOKL)
    If (.NOT. qOKL) Goto 1000
		
  END IF 
! Loop over processes and take in turns to write data		 
  CALL MPI_BARRIER(tProcInfo_G%comm, error)

! File data was setup on process 0, need to share filetype with the
! rest of the processors in the MPI communicator 
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

!  Set error flag and exit         
    qOK = .TRUE.				    
    GoTo 2000     

! Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call Error_log('Error in FEMethod:WriteChiData',tErrorLog_G)
    Print*,'Error in FEMethod:WriteChiData'
2000 CONTINUE
  END SUBROUTINE WriteEleData

!********************************************************

  SUBROUTINE WriteData(qSeparateStepFiles,&
       zDataFileName,tArrayZ,tArrayA,tArrayE,&
       iStep,sZ,sA,sV,qStart,qFormattedFiles, &
       qOK)

    IMPLICIT NONE
! Subroutine to write the data to files
! INS-
!
    CHARACTER(*),INTENT(IN) :: zDataFileName
    LOGICAL,INTENT(IN) :: qSeparateStepFiles
    TYPE(cArraySegment), INTENT(INOUT) :: tArrayZ,&
                               tArrayA(:),tArrayE(:)
    INTEGER(KIND=IP),INTENT(IN) :: iStep
    REAL(KIND=WP),INTENT(IN) :: sZ,sA(:),sV(:)
    LOGICAL, INTENT(IN)  :: qStart,qFormattedFiles
    LOGICAL, INTENT(OUT) :: qOK
! Local vars
    LOGICAL  :: qOKL
!----------------------------------
! BEGIN:-
! Set error flag to false

    qOK = .FALSE. 

! Open the file to receive data output - This
! subroutine is in EArrayFunctions.f90 line 449  
! IntegerToString FUNCTION see line 2469
    IF (qSeparateStepFiles.OR.qStart) THEN

       CALL SetUpDataFiles(zDataFileName,&
            qFormattedFiles,&
            tArrayZ,&
            tArrayA,&
            tArrayE,&
            qOKL,&
            TRIM(IntegerToString(iStep)))   
       IF (.NOT. qOKL) GOTO 1000
    END IF
    CALL WriteIntegrationData(sZ,&
         sA,&
         sV,&
         tArrayZ,&
         tArrayA,&
         tArrayE,&
         qOKL)
    IF (.NOT. qOKL) GOTO 1000

!  Set error flag and exit         
    qOK = .TRUE.				    
    GoTo 2000 
! Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call Error_log('Error in Fdatawrite:WriteChiData',tErrorLog_G)
    Print*,'Error in Fdatawrite:WriteChiData'
2000 CONTINUE

  END SUBROUTINE WriteData
  
!**********************************************************

  SUBROUTINE WriteIntegrationData(sZ,sA,sY,tWriteZData,&
       tWriteAData,tArraySegment,qOK)

! Write Integration data to file 
!
! sZ                      - INPUT    - At Z
! sY                      - INPUT    - Result Y 
! iLenY                   - INPUT    - Length of Y
! tArraySegment           - UPDATE   - Description of segments 
!                                      making up sY 
! qOK                     - OUTPUT   - Error flag
    IMPLICIT NONE
    REAL(KIND=WP),      INTENT(IN)      :: sZ
    REAL(KIND=WP),      INTENT(IN)      :: sA(:)
    REAL(KIND=WP),      INTENT(IN)      :: sY(:)
    TYPE(cArraySegment),INTENT(INOUT)   :: tWriteZData
    TYPE(cArraySegment),INTENT(INOUT)   :: tWriteAData(:)
    TYPE(cArraySegment),INTENT(INOUT)   :: tArraySegment(:)
    LOGICAL,            INTENT(OUT)     :: qOK      

!====================================================================
! Define local variables
!
! iSegment   - Current segment number 
! iStart     - Start position of data in array Y
! iEnd       - End position of data in array Y
! qOKL       - Local error flag
!
!=====================================================================
!
    INTEGER(KIND=IP)               :: iSegment
    INTEGER(KIND=IP)               :: iStart
    INTEGER(KIND=IP)               :: iEnd, fieldsize, error
    REAL(KIND=WP), DIMENSION(:), ALLOCATABLE :: write_in 
    LOGICAL                        :: qOKL

! Set error flag to false
    qOK = .FALSE.

! Write out field data:-only root processor needs to do this        
    fieldsize = SIZE(sA)/2_IP
     
    IF(tProcInfo_G%qRoot) THEN
       IF(tWriteAData(iRe_A_CG)%qWrite) THEN
          ALLOCATE(write_in(fieldsize))
          write_in = Vector(iRe_A_CG,sA)
        	
          CALL OutputIntegrationData(tWriteAData(iRe_A_CG)%tFileType,&
               write_in,fieldsize,qOKL)
          
          DEALLOCATE(write_in)		      
       END IF
    END IF
		
    IF(tProcInfo_G%qRoot) THEN
       IF(tWriteAData(iIm_A_CG)%qWrite) THEN
          ALLOCATE(write_in(fieldsize))
          write_in = Vector(iIm_A_CG,sA)
          call OutputIntegrationData(tWriteAData(iIm_A_CG)%tFileType,&
               write_in,fieldsize,qOKL)
          DEALLOCATE(write_in)
       END IF
    END IF

!  Write Electron Segments to file      
!  OutputIntegrationData routines are
!  either starts at line 491 or 585 in this file 
    Do iSegment = 1_IP, SIZE(tArraySegment)
       If (tArraySegment(iSegment)%qWrite) Then
          iStart = tArraySegment(iSegment)%iStart
          iEnd   = tArraySegment(iSegment)%iEnd

!  Write the data
          call OutputIntegrationData(tArraySegment(iSegment)%tFileType, &
               sY(iStart:iEnd),tProcInfo_G%rank,iGloNumElectrons_G,&
               qOKL)
          If (.NOT. qOKL) Goto 1000
       End If
    End Do

!  Write out z data         
    IF(tProcInfo_G%qRoot) THEN	
       If (tWriteZData%qWrite) Then
          call OutputIntegrationData(tWriteZData%tFileType, &
               sZ,qOKL)
          If (.NOT. qOKL) Goto 1000
       End if
    End If

!  Set error flag and exit         
    qOK = .TRUE.				    
    GoTo 2000     
! Error Handler - Error log Subroutine in CIO.f90 line 709
1000 call Error_log('Error in Fdatawrite:WriteIntegrationData',tErrorLog_G)
    Print*,'Error in Fdatawrite:WriteIntegrationData'
2000 CONTINUE

  END SUBROUTINE WriteIntegrationData
!-------------------------------------------------------
  SUBROUTINE OutputIntegrationData_RealArray(tFileType,sY,&
       iLenY,qOK)
!
!********************************************************************
! Output Integration data to file 
!********************************************************************
!
! tFileTypeY              - INPUT    -File type properties
! sY                      - INPUT    - Result Y 
! iLenY                   - INPUT    - Length of Y
! qOK                     - OUTPUT   - Error flag
!
      IMPLICIT NONE
      TYPE(cFileType),         INTENT(INOUT)   :: tFileType
      REAL(KIND=WP),           INTENT(IN)      :: sY(:)
      INTEGER(KIND=IP),        INTENT(IN)      :: iLenY
      LOGICAL,                 INTENT(OUT)     :: qOK      
!
!--------------------------------------------------------------------------------	
! Local Scalars         
!--------------------------------------------------------------------------------	
!====================================================================
! Define local variables
!
!=====================================================================
!!
      LOGICAL                        :: qOKL
!
!--------------------------------------------------------------------------------	
! Set error flag to false         
!--------------------------------------------------------------------------------	
!
      qOK = .FALSE.
!
!--------------------------------------------------------------------------------	
!  Open the file - In CIO.f90 line 862
!--------------------------------------------------------------------------------	
!
     ! PRINT*,'OPENING********'
	  call OpenFileForAppend(tFileType%zFileName, &
      			     tFileType, &
			     qOKL)
      If (.NOT. qOKL) Goto 1000
!
!--------------------------------------------------------------------------------	
!  Set up new page - see CIO.f90 line 651        
!--------------------------------------------------------------------------------	
! 
     ! PRINT*,'WRITING NEW PAGE*****'
	  call WriteSDDSNewPage(tFileType,qOKL)
	  
      If (.NOT. qOKL) Goto 1000
!
!--------------------------------------------------------------------------------	
!  Write length of column data - see CIO.f90 line 100          
!--------------------------------------------------------------------------------	
! 
     ! PRINT*,'WRITING INTEGER@@@@@@@@@@'
	  call WriteINTEGER(iLenY,tFileType,qOKL)
      If (.NOT. qOKL) Goto 1000  
!
!--------------------------------------------------------------------------------	
!  Write real part - see CIO.f90 line 232        
!--------------------------------------------------------------------------------	
! 	 
     ! PRINT*,'WRITING ARRAY SIZE',iLenY,'@@@@@@@@@@@'
	  call Write1DRealArray(sY,tFileType,qOKL)     
      If (.NOT. qOKL) Goto 1000
!
!--------------------------------------------------------------------------------	
!  Close the file - see CIO.f90 line 560
!--------------------------------------------------------------------------------	
!
    !  PRINT*,'CLOSING FILE@@@@@@@@@@'
	  call CloseFile(tFileType, &
                     qOKL)
      If (.NOT. qOKL) Goto 1000
!
!--------------------------------------------------------------------------------	
!  Set error flag and exit         
!--------------------------------------------------------------------------------	
!
      qOK = .TRUE.				    
      GoTo 2000     
!
!--------------------------------------------------------------------------------
! Error Handler - Error log Subroutine in CIO.f90 line 709
!--------------------------------------------------------------------------------
!            
1000 call Error_log('Error in Fdatawrite:OutputIntegrationData_RealArray',tErrorLog_G)
      Print*,'Error in Fdatawrite:OutputIntegrationData_RealArray'
      call CloseFile(tFileType, &
                     qOKL)
	  !PRINT*,'ERRORS@@@@@@@@@@@@@@'
2000 CONTINUE
!
      !PRINT*,'FINISHING))))))))(((((((('
	  END SUBROUTINE OutputIntegrationData_RealArray     
!---------------------------------------------------------------------------------!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
      SUBROUTINE OutputIntegrationData_ParRealArray(tFileType, &
 				                       sY,     &
						       rank,   &
				                       iLenY,  &
						       qOK)
!
!********************************************************************
! Output Integration data to file 
!********************************************************************
!
! tFileTypeY              - INPUT    -File type properties
! sY                      - INPUT    - Result Y 
! iLenY                   - INPUT    - Length of Y
! qOK                     - OUTPUT   - Error flag

      IMPLICIT NONE
      TYPE(cFileType),         INTENT(INOUT)   :: tFileType
      REAL(KIND=WP),           INTENT(IN)      :: sY(:)
      INTEGER(KIND=IP),        INTENT(IN)      :: rank
      INTEGER(KIND=IPL),        INTENT(IN)      :: iLenY
	  LOGICAL,                 INTENT(OUT)     :: qOK      
!
!--------------------------------------------------------------------------------	
! Local Scalars         
!--------------------------------------------------------------------------------	
!====================================================================
! Define local variables
!
!=====================================================================
!!
      LOGICAL  :: qOKL
	  
      REAL(KIND=WP), DIMENSION(:),ALLOCATABLE  ::  sendbuff, &
                                                   recvbuff
      INTEGER  ::  error, i, req, stat
      INTEGER(KIND=IP)  ::  mpifiletype
! Set error flag to false         
      qOK = .FALSE.

! CALL GetMPIFileType(tFileType,mpifiletype)

!--------------------------------------------------------------------------------	
!  Open the file:- this only need to be done on one processor - see CIO.f90 line 862
	IF (rank==0) THEN
	
        call OpenFileForAppend(tFileType%zFileName, &
      			       tFileType, &
			       qOKL)
           If (.NOT. qOKL) Goto 1000

!  Set up new page - see CIO.f90 line 651        
      	call WriteSDDSNewPage(tFileType,qOKL)
      	   If (.NOT. qOKL) Goto 1000

!  Write length of column data - see CIO.f90 line 100          
      	call WriteINTEGERL(iLenY,tFileType,qOKL)
      	If (.NOT. qOKL) Goto 1000

!  Close File 
      	call CloseFile(tFileType, &
                       qOKL)
           If (.NOT. qOKL) Goto 1000
		
	END IF   
	
! Synchronize processors      
    CALL MPI_BARRIER(tProcInfo_G%comm, error)
	
! File data was setup on process 0, need to share filetype with the
! rest of the processors in the MPI communicator 
    CALL shareFileType(tFileType)
!	print*,'rank has',tFileType
!	CALL MPI_FINALIZE(error)
!	STOP
	
!---------------------------------------------------------------------------	
!  Cycle through processes and write data one by one - see CIO.f90 line 232       
    DO i = 0,tProcInfo_G%size-1
 		IF (rank == i) THEN
                        if (procelectrons_G(1) > 0) then

                          call OpenFileForAppend(tFileType%zFileName, &
                                                 tFileType, qOKL)
					
                          call Write1DRealArray(sY,tFileType,qOKL)
                          If (.NOT. qOKL) Goto 1000

                          call CloseFile(tFileType, qOKL)

                        end if
                END IF
!Synchronize
		CALL MPI_BARRIER(tProcInfo_G%comm, error)
    END DO	
!----------------------------------------------------------------------------	
!  Set error flag and exit         
      qOK = .TRUE.				    
      GoTo 2000   
	    
! Error Handler - Error log Subroutine in CIO.f90 line 709
1000 call Error_log('Error in Fdatawrite:OutputIntegrationData_RealArray',tErrorLog_G)
      Print*,'Error in Fdatawrite:OutputIntegrationData_RealArray'
      call CloseFile(tFileType, &
                     qOKL)
2000 CONTINUE

      END SUBROUTINE OutputIntegrationData_ParRealArray     

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

      SUBROUTINE OutputIntegrationData_RealValue(tFileType,     &
   				                 sY,            &
			                         qOK)
!
!********************************************************************
! Output Integration data to file 
!********************************************************************
!
! tFileType               - INPUT    -File type properties
! sY                      - INPUT    - Result Y 
! qOK                     - OUTPUT   - Error flag
!********************************************************************
      IMPLICIT NONE
      TYPE(cFileType),         INTENT(INOUT)   :: tFileType
      REAL(KIND=WP),           INTENT(IN)      :: sY
      LOGICAL,                 INTENT(OUT)     :: qOK      
!
!--------------------------------------------------------------------------------	
! Local Scalars         
!--------------------------------------------------------------------------------	
!====================================================================
! Define local variables
!
!=====================================================================
!!
      LOGICAL                        :: qOKL
!
!--------------------------------------------------------------------------------	
! Set error flag to false         
!--------------------------------------------------------------------------------	
!
      qOK = .FALSE.
!
!--------------------------------------------------------------------------------	
!  Open the file - In CIO.f90 line 862
!--------------------------------------------------------------------------------	
!
      call OpenFileForAppend(tFileType%zFileName, &
      			     tFileType, &
			     qOKL)
      If (.NOT. qOKL) Goto 1000
!
!--------------------------------------------------------------------------------	
!  Set up new page - see CIO.f90 line 651       
!--------------------------------------------------------------------------------	
! 
      call WriteSDDSNewPage(tFileType,qOKL)
      If (.NOT. qOKL) Goto 1000
!
!--------------------------------------------------------------------------------	
!  Write length of column data - see CIO.f90 line 100         
!--------------------------------------------------------------------------------	
! 
      call WriteINTEGER(1_IP,tFileType,qOKL)
      If (.NOT. qOKL) Goto 1000
!
!--------------------------------------------------------------------------------	
!  Write real part - see CIO.f90 line 166          
!--------------------------------------------------------------------------------	
! 	 
      call WriteRealNumber(sY,tFileType,qOKL)     
      If (.NOT. qOKL) Goto 1000
!
!--------------------------------------------------------------------------------	
!  Close the file - see CIO.f90 line 560
!--------------------------------------------------------------------------------	
!
      call CloseFile(tFileType, &
                     qOKL)
      If (.NOT. qOKL) Goto 1000
!
!--------------------------------------------------------------------------------	
!  Set error flag and exit         
!--------------------------------------------------------------------------------	
!
      qOK = .TRUE.				    
      GoTo 2000     
!
!--------------------------------------------------------------------------------
! Error Handler - Error log Subroutine in CIO.f90 line 709
!--------------------------------------------------------------------------------
!            
1000 call Error_log('Error in MathLib:OutputIntegrationData_RealValue',tErrorLog_G)
      Print*,'Error in MathLib:OutputIntegrationData_RealValue'
      call CloseFile(tFileType, &
                     qOKL)
2000 CONTINUE
!
      END SUBROUTINE OutputIntegrationData_RealValue

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
	
FUNCTION IntegerToString(iInteger)
!********************************************************************
! Convert an integer into a string
!********************************************************************
!
! iInteger    - INPUT  - Integer to convert
!
!====================================================================
! Define variables
!
!=====================================================================
!	
	IMPLICIT NONE
!	
        INTEGER(KIND=IP),          INTENT(IN)                   :: iInteger
        CHARACTER(32_IP)               	                        :: IntegerToString                                          
!====================================================================
! Define local variables
!
!=====================================================================
!
        CHARACTER(32_IP) :: zCharacter
!
!--------------------------------------------------------------------------------	
! Write character to internal file      
!--------------------------------------------------------------------------------	
!
      write(zCharacter,*) iInteger
!
!--------------------------------------------------------------------------------	
! Output without blanks      
!--------------------------------------------------------------------------------	
!
      IntegerToString = TRIM(ADJUSTL(zCharacter))
!
!--------------------------------------------------------------------------------	
!  Set error flag and exit         
!--------------------------------------------------------------------------------	
!
       GoTo 2000     
!
!--------------------------------------------------------------------------------
! Error Handler - Error log Subroutine in CIO.f90 line 709
!--------------------------------------------------------------------------------
!            
1000 call Error_log('Error in MathLib:IntegerToString',tErrorLog_G)
      Print*,'Error in MathLib:IntegerToString'
2000 CONTINUE
!	      
END FUNCTION IntegerToString

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!-------------------------------------------------------------------------------- 
SUBROUTINE DUMPDATA(sA,sV,rank,nnodes,nelectrons,sz,istep,page)

 REAL(KIND=WP),DIMENSION(:),INTENT(IN) :: sA
 REAL(KIND=WP),DIMENSION(:),INTENT(IN) :: sV
 INTEGER(KIND=IP),INTENT(IN) :: rank,nnodes,istep,page
 INTEGER(KIND=IPL), INTENT(IN) :: nelectrons
 REAL(KIND=WP),INTENT(IN) :: sz

 CHARACTER(32_IP) :: FileName

! FIELD

if (rank==0) then

! Real part
 FileName = 'reA' // TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) sA(1:nnodes)
 CLOSE(UNIT=213,STATUS='KEEP')
! Imaginary part
 FileName = 'imA' // TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) sA(nnodes+1:2*nnodes)
 CLOSE(UNIT=213,STATUS='KEEP') 

end if

! ELECTRONS

if (nelectrons > 0) then

! re pperp
 FileName = 'rePPerp'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) sV(1:nelectrons)
 CLOSE(UNIT=213,STATUS='KEEP') 
! Im pperp
 FileName = 'imPPerp'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) sV(nelectrons+1:2*nelectrons)
 CLOSE(UNIT=213,STATUS='KEEP') 
! Q 
 FileName = 'Q'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) sV(2*nelectrons+1:3*nelectrons)
 CLOSE(UNIT=213,STATUS='KEEP') 
! Z2 
 FileName = 'Z2'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) sV(3*nelectrons+1:4*nelectrons)
 CLOSE(UNIT=213,STATUS='KEEP') 
! X 
 FileName = 'X'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) sV(4*nelectrons+1:5*nelectrons)
 CLOSE(UNIT=213,STATUS='KEEP')
! Y
 FileName = 'Y'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) sV(5*nelectrons+1:6*nelectrons)
 CLOSE(UNIT=213,STATUS='KEEP') 
! Z
 FileName = 'Z'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) sz
 CLOSE(UNIT=213,STATUS='KEEP')  

end if
 
! step

if (rank==0) then

 FileName = 'step'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) istep
 CLOSE(UNIT=213,STATUS='KEEP') 

end if

 ! nelectrons
 FileName = 'nelectrons'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) nelectrons
 CLOSE(UNIT=213,STATUS='KEEP') 
 
! page

if (rank==0) then

 FileName = 'page'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) page
 CLOSE(UNIT=213,STATUS='KEEP') 

end if
        
END SUBROUTINE DUMPDATA

!==========================================================================
SUBROUTINE READNELEC(rank,nelectrons)

  INTEGER(KIND=IP),INTENT(IN) :: rank
  INTEGER(KIND=IPL),INTENT(INOUT) :: nelectrons
  CHARACTER(32_IP) :: FileName
  
  FileName = 'nelectrons'//TRIM(IntegerToString(RANK))//'.dump'
 
  OPEN(UNIT=213,FILE=FileName,STATUS='OLD',ACTION='READ',POSITION='REWIND',&
  FORM='UNFORMATTED')
  READ(213) nelectrons
  CLOSE(UNIT=213,STATUS='KEEP')

END SUBROUTINE
!========================================================================== 

SUBROUTINE READDUMP(sA,sV,rank,nnodes,nelectrons,sz,istep,page)

 REAL(KIND=WP),DIMENSION(:),INTENT(OUT) :: sA
 REAL(KIND=WP),DIMENSION(:),INTENT(OUT) :: sV
 INTEGER(KIND=IP),INTENT(OUT) :: rank,istep,page
 INTEGER(KIND=IP),INTENT(IN) :: nnodes
 INTEGER(KIND=IPL), INTENT(INOUT) :: nelectrons
 REAL(KIND=WP),INTENT(OUT) :: sz

 CHARACTER(32_IP) :: FileName

! FIELD

if (rank==0) then

! Real part
 FileName = 'reA'//TRIM(IntegerToString(RANK))//'.dump'
 

 OPEN(UNIT=213,FILE=FileName,STATUS='OLD',ACTION='READ',POSITION='REWIND',&
 FORM='UNFORMATTED')
 READ(213) sA(1:nnodes)
 CLOSE(UNIT=213,STATUS='KEEP')
! Imaginary part
 FileName = 'imA'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='OLD',ACTION='READ',POSITION='REWIND',&
 FORM='UNFORMATTED')
 READ(213) sA(nnodes+1:2*nnodes)
 CLOSE(UNIT=213,STATUS='KEEP') 

end if

! ELECTRONS

if (nelectrons>0) then

! re pperp
 FileName = 'rePPerp'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='OLD',ACTION='READ',POSITION='REWIND',&
 FORM='UNFORMATTED')
 READ(213) sV(1:nelectrons)
 CLOSE(UNIT=213,STATUS='KEEP') 
! Im pperp
 FileName = 'imPPerp'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='OLD',ACTION='READ',POSITION='REWIND',&
 FORM='UNFORMATTED')
 READ(213) sV(nelectrons+1:2*nelectrons)
 CLOSE(UNIT=213,STATUS='KEEP') 
! Q 
 FileName = 'Q'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='OLD',ACTION='READ',POSITION='REWIND',&
 FORM='UNFORMATTED')
 READ(213) sV(2*nelectrons+1:3*nelectrons)
 CLOSE(UNIT=213,STATUS='KEEP') 
! Z2 
 FileName = 'Z2'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='OLD',ACTION='READ',POSITION='REWIND',&
 FORM='UNFORMATTED')
 READ(213) sV(3*nelectrons+1:4*nelectrons)
 CLOSE(UNIT=213,STATUS='KEEP') 
! X 
 FileName = 'X'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='OLD',ACTION='READ',POSITION='REWIND',&
 FORM='UNFORMATTED')
 READ(213) sV(4*nelectrons+1:5*nelectrons)
 CLOSE(UNIT=213,STATUS='KEEP')
! Y
 FileName = 'Y'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='OLD',ACTION='READ',POSITION='REWIND',&
 FORM='UNFORMATTED')
 READ(213) sV(5_IPL*nelectrons+1_IPL:6_IPL*nelectrons)
 CLOSE(UNIT=213,STATUS='KEEP') 
! Z
 FileName = 'Z'//TRIM(IntegerToString(RANK))//'.dump'

 OPEN(UNIT=213,FILE=FileName,STATUS='OLD',ACTION='READ',POSITION='REWIND',&
 FORM='UNFORMATTED')
 READ(213) sz
 CLOSE(UNIT=213,STATUS='KEEP')  

end if
 
! step

if (rank==0) then

 FileName = 'step'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='OLD',ACTION='READ',POSITION='REWIND',&
 FORM='UNFORMATTED')
 READ(213) istep
 CLOSE(UNIT=213,STATUS='KEEP') 

! page
 FileName = 'page'//TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='OLD',ACTION='READ',POSITION='REWIND',&
 FORM='UNFORMATTED')
 READ(213) page
 CLOSE(UNIT=213,STATUS='KEEP') 

end if

END SUBROUTINE READDUMP
!------------------------------------------------------------------
!===================================================================
SUBROUTINE DUMPCHIDATA(chibar,normchi,rank)

 REAL(KIND=WP),DIMENSION(:),INTENT(IN) :: chibar
 REAL(KIND=WP),DIMENSION(:),INTENT(IN) :: normchi
 INTEGER(KIND=IP),INTENT(IN) :: rank

 CHARACTER(32_IP) :: FileName

! FIELD
! Real part
 FileName = 'chibar' // TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) chibar
 CLOSE(UNIT=213,STATUS='KEEP')
! Imaginary part
 FileName = 'normchi' // TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='REPLACE',FORM='UNFORMATTED')
 WRITE(213) normchi
 CLOSE(UNIT=213,STATUS='KEEP') 

        
END SUBROUTINE DUMPCHIDATA

!------------------------------------------------------------------
!===================================================================

SUBROUTINE READINCHIDATA(chibar,normchi,rank)

 REAL(KIND=WP),DIMENSION(:),INTENT(OUT) :: chibar
 REAL(KIND=WP),DIMENSION(:),INTENT(OUT) :: normchi
 INTEGER(KIND=IP),INTENT(IN) :: rank

 CHARACTER(32_IP) :: FileName

! FIELD
! Real part
 FileName = 'chibar' // TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='OLD',ACTION='READ',POSITION='REWIND',&
 FORM='UNFORMATTED')
 READ(213) chibar
 CLOSE(UNIT=213,STATUS='KEEP')
! Imaginary part
 FileName = 'normchi' // TRIM(IntegerToString(RANK))//'.dump'
 
 OPEN(UNIT=213,FILE=FileName,STATUS='OLD',ACTION='READ',POSITION='REWIND',&
 FORM='UNFORMATTED')
 READ(213) normchi
 CLOSE(UNIT=213,STATUS='KEEP') 

        
END SUBROUTINE READINCHIDATA

!___________--------------------------=============++++++++++++++++++++===
!-----------------++=========________________=====================+++++++++

END MODULE DataWrite
