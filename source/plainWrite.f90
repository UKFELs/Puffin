!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!


module plainWrite

  use paratype
  use ParallelSetUp


  subroutine  WriteParameterData(zDataFileName, &
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

! Write input data used to create results
!
! zDataFileName      - INPUT  - Data file name
! iNodes             - INPUT  - Number of Nodes
! iNumElectrons      - INPUT  - number of electrons
! sLengthOfElm       - INPUT  - Element length
! sStepSize          - INPUT  - Integration step size
! nSteps             - INPUT  - Number of steps 
! sLenEPulse         - INPUT  - L-electron pulse
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


!     Set error flag to false         

    qOK = .FALSE.    

    If (tProcInfo_G%qROOT) Then

!     Open the file to receive data output -
!     This subroutine is in IO.f90 line 793

      tParamFile%qFormatted = .TRUE.

      call InitialiseFile('Param' // TRIM(zDataFileName), &
                           tParamFile, qOKL)
      If (.NOT. qOKL) Goto 1000


!     Write data - These subroutines is in CIO.f90 line 100,166 and 25

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




!     close data file - This subroutine is in CIO.f90 line 560  

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

2000 continue

  end subroutine WriteParameterData





  subroutine OutputIntegrationData_RealValue(tFileType, sY, qOK)

! Output Integration data to file 
!
! tFileType               - INPUT    -File type properties
! sY                      - INPUT    - Result Y 
! qOK                     - OUTPUT   - Error flag

    implicit none

    type(cFileType), intent(inout)   :: tFileType
    real(kind=wp),   intent(in)      :: sY
    logical,         intent(out)     :: qOK      

! Local Scalars         

    logical  :: qOKL



!     Set error flag to false         

    qOK = .FALSE.
  
!     Open the file - In CIO.f90 line 862

    call OpenFileForAppend(tFileType%zFileName, &
                           tFileType, qOKL)
    If (.NOT. qOKL) Goto 1000

!     Write number to file - see CIO.f90 line 166          

    call WriteRealNumber(sY,tFileType,qOKL)
    If (.NOT. qOKL) Goto 1000
 
!     Close the file - see CIO.f90 line 560

    call CloseFile(tFileType, qOKL)
    If (.NOT. qOKL) Goto 1000

!     Set error flag and exit

    qOK = .TRUE.
    GoTo 2000

!     Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call Error_log('Error in MathLib:OutputIntegrationData_RealValue',tErrorLog_G)
      Print*,'Error in MathLib:OutputIntegrationData_RealValue'

      call CloseFile(tFileType, qOKL)

2000 CONTINUE

  end subroutine OutputIntegrationData_RealValue






  subroutine OutputIntegrationData_RealArray(tFileType,sY,&
                                             iLenY,qOK)

! Output Integration data to file 
!
! tFileTypeY              - INPUT    -File type properties
! sY                      - INPUT    - Result Y 
! iLenY                   - INPUT    - Length of Y
! qOK                     - OUTPUT   - Error flag
!

    implicit none

    type(cFileType),         intent(inout)   :: tFileType
    real(kind=wp),           intent(in)      :: sY(:)
    integer(kind=ip),        intent(in)      :: iLenY
    logical,                 intent(out)     :: qOK    

!     Define local variables

    logical                        :: qOKL

!     Set error flag to false

    qOK = .FALSE.

!     Open the file - In CIO.f90 line 862

    call OpenFileForAppend(tFileType%zFileName, &
                           tFileType, qOKL)
    if (.NOT. qOKL) Goto 1000

!     Write real part - see CIO.f90 line 232        

    call Write1DRealArray(sY,tFileType,qOKL)
    if (.NOT. qOKL) Goto 1000

!     Close the file - see CIO.f90 line 560

    call CloseFile(tFileType, qOKL)
    if (.NOT. qOKL) Goto 1000



!     Set error flag and exit         

    qOK = .TRUE.				    
    GoTo 2000     



!     Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call Error_log('Error in Fdatawrite:OutputIntegrationData_RealArray',tErrorLog_G)
    Print*,'Error in Fdatawrite:OutputIntegrationData_RealArray'

    call CloseFile(tFileType, qOKL)

2000 continue


  end subroutine OutputIntegrationData_RealArray     



  subroutine OutputIntegrationData_ParRealArray(tFileType, &
                                                sY, rank,   &
                                                iLenY, qOK)

! Output Integration data to file 
!
! tFileTypeY              - INPUT    -File type properties
! sY                      - INPUT    - Result Y 
! iLenY                   - INPUT    - Length of Y
! qOK                     - OUTPUT   - Error flag

    implicit none

    type(cFileType),    intent(inout)   :: tFileType
    real(kind=wp),      intent(in)      :: sY(:)
    integer(kind=ip),   INTENT(in)      :: rank
    integer(kind=ipl),  INTENT(in)      :: iLenY
      logical,            INTENT(out)     :: qOK      

! Local Scalars         
!
! Define local variables

    LOGICAL  :: qOKL
      
    REAL(KIND=WP), DIMENSION(:),ALLOCATABLE  ::  sendbuff, &
                                                   recvbuff
    INTEGER  ::  error, i, req, stat
    INTEGER(KIND=IP)  ::  mpifiletype

!     Set error flag to false         
      
    qOK = .FALSE.

    
!     Synchronize processors

    call MPI_BARRIER(tProcInfo_G%comm, error)
    
!     File data was setup on process 0, need to share filetype with the
!     rest of the processors in the MPI communicator 
    
    call shareFileType(tFileType)

!     Cycle through processes and write data one by one - see CIO.f90 line 232

    do i = 0,tProcInfo_G%size-1

    if (rank == i) then

      if (procelectrons_G(1) > 0) then

        call OpenFileForAppend(tFileType%zFileName, &
                               tFileType, qOKL)

        call Write1DRealArray(sY,tFileType,qOKL)
        if (.not. qOKL) Goto 1000

        call CloseFile(tFileType, qOKL)

      end if

    end if

!     Synchronize

        CALL MPI_BARRIER(tProcInfo_G%comm, error)
    END DO  

!     Set error flag and exit         

      qOK = .TRUE.                  
      GoTo 2000   
        
!     Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call Error_log('Error in Fdatawrite:OutputIntegrationData_RealArray',tErrorLog_G)
      Print*,'Error in Fdatawrite:OutputIntegrationData_RealArray'
      call CloseFile(tFileType, &
                     qOKL)
2000 CONTINUE

  end subroutine OutputIntegrationData_ParRealArray     



  SUBROUTINE WriteEleData(zDataFileName,fname,vname,&
       qFormatted,nelectrons,edata,qOK)                          

  IMPLICIT NONE
!
! This subroutine writes the electron chi data to
! file
!
! zDataFileName           - INPUT    - Data file name
! qFormatted              - INPUT    - Write formatted or binary file
! s_chi_bar     - INPUT    -
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

    call InitialiseFile(fname // TRIM(zDataFileName), &
         tParamFile, &
         qOKL)
    If (.NOT. qOKL) Goto 1000


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





  subroutine InitialiseFile(zOutFile, tFileType, qOK)

! Initialise output files 
!
! zOutFile        - INPUT   - Output file name    
! tFileType       - OUTPUT  -File type properties
! qOK             - OUTPUT  - Error flag
!
! Define local variables

    implicit none

    character(*),   intent(in)     :: zOutFile
    type(cFileType),intent(inout)  :: tFileType
    logical,        intent(out)    :: qOK      

! Local Scalars         

    logical :: qOKL




!     Set error flag to false         

    qOK = .FALSE.



!     Open the file to receive output     
!     OpenFileForOutput subroutine starts on line 474 in this file   


    call OpenFileForOutput(zOutFile, tFileType, qOKL)        
    if (.NOT. qOKL) Goto 1000



!     Set error flag and exit         

    qOK = .TRUE.            
    goto 2000     

!     Error Handler

            
1000 call Error_log('Error in DIO: InitialiseSDDSFile',tErrorLog_G)
   Print*,'Error in DIO: InitialiseSDDSFile'

2000 continue

  end subroutine InitialiseFile










end module defWrite