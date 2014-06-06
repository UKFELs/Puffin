!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE Setup

  USE SETUPTRANS
  USE FFTW_Constants
  USE setupcalcs
  USE transforms
  USE DataWrite
  USE lattice
  USE Stiffness
  USE Globals
  USE resume
  USE electronInit
  USE Read_data
  USE checks

! A module which allocates and initializes - or 
! destroys - the data used in Puffin.

  IMPLICIT NONE

  CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE init(sA,sV,sZ,qOK)

  USE InitVars

  IMPLICIT NONE

! Subroutine to perform the initialization of 
! the data for Puffin, and to write out initial
! values.
!
!                     ARGUMENTS
!
! sV             Electron macro-particle phase space
!                coordinates.
!
! sA             Radiation field.
!
! sZ             Electron propagation distance in z
!                through undulator.
! 
! qOK            Error flag; .false. if no error

  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT)  :: sV(:)
  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT)  :: sA(:)
  REAL(KIND=WP), INTENT(OUT) :: sZ
  LOGICAL, INTENT(OUT)   ::  qOK

!     Set error flag

  qOK = .FALSE.

!     Initialize the processors for MPI

  CALL InitializeProcessors(tProcInfo_G,qOKL)
  IF (.NOT. qOKL) GOTO 1000

!     Optional parameters

  qResume = .FALSE.
  qWrite = .TRUE.

!     Read in input file name 
!     (input on command line as variable at runtime)

  CALL getarg(1,infile)
  zFileName = infile

  IF (infile == emptstring) THEN

    PRINT *, 'ERROR, no input filename specified'
    STOP
    
  END IF

  CALL FileNameNoExtension(zFileName, zFile, qOKL)
  IF (.NOT. qOKL) GOTO 1000

!     Initialise Error log for this run

  tErrorLog_G%zFileName = TRIM(ADJUSTL(zFile))//"_Error.log"		
  tErrorLog_G%qFormatted = .TRUE.

  CALL Error_log('',tErrorLog_G)

!     Read input file



  CALL read_in(zFileName, &
       zDataFileName,     &
       qSeparateStepFiles,&
       qFormattedFiles,   &
       qResume,           &
       sStepSize,         &
       nSteps,            &
       sZ,                &
       LattFile,          &
       iWriteNthSteps,    &
       iIntWriteNthSteps, &
       tArrayZ,           &
       tArrayA,           &
       tArrayE,           &
       sLenEPulse,        &
       iNodes,            &
       sWigglerLength,    &
       sLengthofElm,      &
       iRedNodesX,        &
       iRedNodesY,        &
       sQe,               &
       q_noise,           &
       iNumElectrons,     &
       sSigmaGaussian,    &
       sElectronThreshold,&
       beamCenZ2,         &
       gamma_d,           &
       chirp,             &
       nbeams,            &
       dist_f,            &
       qSimple,           &
       sA0_Re,            &
       sA0_Im,            &
       sFiltFrac,         &
       sDiffFrac,         &
       sBeta,             &
       srho,              &
       sEta,              &
       sKBeta,            &
       sEmit_n,           &
       fx,                &
       fy,                &
       Dfact,             &
       sFocusfactor,      &
       taper,             &
       sSeedSigma,        &
       freqf, SmeanZ2,    &
       qFlatTopS, nseeds, &
       sPEOut,            &
       iDumpNthSteps,     &
       qSwitches,         &
       qOKL)
  
  IF (.NOT. qOKL) GOTO 1000

!    Check all the inputs e.g. wiggler and electron lengths etc 
!    to avoid errors.

  CALL CheckParameters(sLenEPulse,iNumElectrons,nbeams,sLengthofElm,iNodes,&
       sWigglerLength,sStepSize,nSteps,srho,sEta,sKBeta,sFocusfactor, &
       sSigmaGaussian,fx,fy,iRedNodesX,iRedNodesY,qSwitches,qSimple,qOKL)
  
  IF (.NOT. qOKL) GOTO 1000

!    Setup FFTW plans for the forward and backwards transforms.

  CALL getTransformPlans4FEL(iNodes,qOKL)
  
  IF (.NOT. qOKL) GOTO 1000

!    Calculate parameters for matched beam

  IF (qSwitches(iMatchedBeam_CG)) THEN

    if (qSimple) CALL MatchBeams(srho,sEmit_n,sKBeta,sFocusfactor,&
                    sEta,iNumElectrons,sLenEPulse,&
                    sSigmaGaussian,sSeedSigma(1,:),iNodes,sWigglerLength,&
                    sLengthofElm,iRedNodesX,iRedNodesY,fx,fy,qOKL)

    IF (.NOT. qOKL) GOTO 1000
  
  END IF

!     Check transverse sampled length of field is long enough to model 
!     diffraction of the resonant frequency.

  IF (qSwitches(iDiffraction_CG)) THEN

    if (qSimple)  CALL CheckSourceDiff(sStepSize,nSteps,srho,sSigmaGaussian,sWigglerLength,&
                          sLengthofElm,iNodes,qOKL)

    IF (.NOT. qOKL) GOTO 1000
  
  END IF

!    If using wiggler lattice read in lattice file

  IF (lattFile=='') THEN
    qMod = .FALSE.
    IF(tProcInfo_G%qRoot) PRINT*, 'There are no dispersive sections'
  ELSE
    qMod = .TRUE.
    IF(tProcInfo_G%qRoot) PRINT*, 'There are dispersive sections'
  END IF
      
  IF (qMod) THEN
    modNum=numOfMods(lattFile)
    !modNum=26
    ALLOCATE(D(ModNum),zMod(ModNum),delta(modNum))
    ALLOCATE(mf(ModNum),delmz(ModNum),tapers(modNum))

!    Latt file name, number of wigg periods converted to z-bar,
!    slippage in chicane in z-bar, 2 dispersive constants, 
!    number of modules

    CALL readLatt(lattFile,zMod,delta,D,Dfact,ModNum,taper,sRho,sStepSize)
    ModCount = 1
  END IF

!     Pass local vars to global vars

  CALL passToGlobals(srho,sEta,sKBeta,iNodes, &
                     iredNodesX,iredNodesY, &
                     sLengthOfElm,&
                     fx,fy,sFocusFactor,taper,sFiltFrac,sDiffFrac,sBeta, &
                     qSwitches,qOK)

  IF (.NOT. qOKL) GOTO 1000

!     Generate macroelectrons

  CALL PopMacroElectrons(qSimple, dist_f, sQe,iNumElectrons,q_noise,sZ,sLenEPulse,&
                         sSigmaGaussian,beamCenZ2,gamma_d,&
                         sElectronThreshold,chirp, &
                         nbeams,sV,qOK)

  IF (.NOT. qOKL) GOTO 1000  



!    IF qresume is .TRUE. then we are reading in data from the
!    dump files from a previous run....

  IF (qResume) THEN

    CALL InitFD(sV,sA,sZ,qOKL)

    IF (.NOT. qOKL) GOTO 1000  
  
!    ...or if qResume is .FALSE. then we are setting up the data
!    ourselves....

  ELSE

!    Set up tArrayE and tArrayA - these are arrays of 
!    pointers describing the layout of data

    CALL SetUpElectronArray(tArrayE,tArrayA,iNumberElectrons_G, &
         iNumberNodes_G , qOKL)
    IF (.NOT. qOKL) Goto 1000

!     Set up initial values     

    ALLOCATE(sA(nFieldEquations_CG*iNumberNodes_G)) 
        
    CALL SetUpInitialValues(nseeds, freqf, SmeanZ2, qFlatTopS,&
                            sSeedSigma, sLengthOfElm,&
                            sA0_Re,&
                            sA0_Im,&
                            sEl_X0Position_G,&
                            sEl_Y0Position_G,&
                            sEl_Z20Position_G,&
                            sV,&
                            sA,&
                            qOKL)
  
    start_step = 1_IP
  	
  END IF

  DEALLOCATE(sEl_X0Position_G,sEl_Y0Position_G,sEl_Z20Position_G)
  DEALLOCATE(sEl_PX0Position_G,sEl_PY0Position_G,sEl_PZ20Position_G)





!    Define the rescaling parameter "ffact" for rescaling
!    backwards transform data.

  ffact = iNodes(iX_CG)*iNodes(iY_CG)*iNodes(iZ2_CG)      
   	
  CALL MPI_BARRIER(tProcInfo_G%comm,error) ! Sync MPI processes
  
  ALLOCATE(lrecvs(tProcInfo_G%size),ldispls(tProcInfo_G%size))
    
  IF (tProcInfo_G%qRoot) PRINT*, 'reduced field sizes',&
       ReducedNX_G,reducedNY_G





!     Initialize stiffness matrix
  	
  CALL SETUPSTIFFMAT(ReducedNX_G,ReducedNY_G,NZ2_G,delta_G)
  	
  CALL MPI_BARRIER(tProcInfo_G%comm,error)
  IF (tProcInfo_G%qRoot) PRINT*, 'Setup active field'




  	
!    Get gathering arrays for reduced/active field arrays

  IF (tTransInfo_G%qOneD) THEN
    CALL getGathArrs(local_rows,lrecvs,ldispls)
  
    lfst_row=fst_row
    llst_row=lst_row
    llocal_rows=local_rows
  ELSE
!    Get sizes for large field array
    CALL getLargeLocSizes(fst_row,lst_row,lfst_row,llst_row,llocal_rows)
  	
!    Get gathering arrays for large field arrays
    CALL getGathArrs(llocal_rows,lrecvs,ldispls)
  END IF
  
  ALLOCATE(mrecvs(tProcInfo_G%size),mdispls(tProcInfo_G%size))

  CALL getGathArrs(local_rows,mrecvs,mdispls)

  IF (qResume) THEN
    CALL READINCHIDATA(s_chi_bar_G,s_Normalised_chi_G,tProcInfo_G%rank)
  !ELSE
  !  CALL DUMPCHIDATA(s_chi_bar_G,s_Normalised_chi_G,tProcInfo_G%rank)
  ENDIF





!    Calculate K-values for diffraction. In Ltransforms.f90

  IF (qDiffraction_G) THEN
    IF (tTransInfo_G%qOneD) THEN
      ALLOCATE(kx_G(1),ky_G(1))
      kx_G = 0
      ky_G = 0
              
      IF (tTransInfo_G%loc_nz2_aft_trans/=0) THEN      
        ALLOCATE(kz2_loc_G(0:tTransInfo_G%loc_nz2_aft_trans-1))
      END IF
              
    ELSE
      ALLOCATE(kx_G(0:iNodes(iX_CG)-1))
      ALLOCATE(ky_G(0:iNodes(iY_CG)-1)) 
        			 
      IF (tTransInfo_G%loc_nz2/=0) THEN      
         ALLOCATE(kz2_loc_G(0:tTransInfo_G%loc_nz2-1))
      END IF
              
    END IF
           
    ALLOCATE(frecvs(tProcInfo_G%size),fdispls(tProcInfo_G%size))

    CALL MPI_BARRIER(tProcInfo_G%comm,error) 
           
    CALL GetKValues(frecvs,fdispls,qOKL)
    IF (.NOT. qOKL) GOTO 1000
  
  END IF



!    Write the various parameter data to file.

  IF(tProcInfo_G%qROOT) PRINT *, 'Writing parameter data to file' 

  CALL WriteEleData(zDataFileName,'Chi','s_chi_bar',qFormattedFiles, &
       iGlonumElectrons_G,s_chi_bar_G,qOKL)
  If (.NOT. qOKL) GOTO 1000
  
  CALL WriteEleData(zDataFileName,'NormChi','s_Normalised_chi',qFormattedFiles, &
       iGlonumElectrons_G,s_Normalised_chi_G,qOKL)
  If (.NOT. qOKL) GOTO 1000
  
  IF (tProcInfo_G%qRoot) THEN
    CALL WriteParameterData(zDataFileName,&
         iNodes,&
         iNumElectrons(1,:),&
         sLengthOfElm, &
         sStepSize,    &
         nSteps,&
         sLenEPulse(1,:),&
         sWigglerLength,&
         sSigmaGaussian(1,:),&
         sA0_Re(1),&
         sA0_Im(1),&
         srho,&
         saw_G,&
         sEta_G,&
         sGammaR_G,&
         iGloNumElectrons_G,&
         nFieldEquations_CG,&
         nElectronEquations_CG,&  
         sZ,&
         iWriteNthSteps,&
         sSeedSigma(1,:),&
         qSwitches,&
         fx,fy,&
         qOKL)                             
    IF (.NOT. qOKL) GOTO 1000
          
  END IF

!    Write out initial values of electron and field data.
!    If not using separate files for each step then open 
!    file - In EArrayFunctions.f90 line 449

  CALL MPI_BARRIER(tProcInfo_G%comm,error)
  
  IF (qWrite.AND.(.NOT.(qSeparateStepFiles))) THEN
    IF(tProcInfo_G%qROOT) PRINT *,&
         'Writing field and electron values to a single file'
    CALL SetUpDataFiles(zDataFileName, &
         qFormattedFiles, &
         tArrayZ, &
         tArrayA, &
         tArrayE, &
         qOKL)   
    IF (.NOT. qOKL) GOTO 1000
  
  END IF
     
!    Write initial result to file - see line 374 for 
!    "WriteIntegrationData" routine

  CALL WriteData(qSeparateStepFiles,&
      zDataFileName,tArrayZ,tArrayA,tArrayE,&
      iStep,sZ,sA,sV,.TRUE.,qFormattedFiles,qOKL)
  IF (.NOT. qOKL) GOTO 1000
   
  CALL MPI_BARRIER(tProcInfo_G%comm,error)
  
  If(tProcInfo_G%qROOT) PRINT *, 'Initial data written'
  
  
  qSeparateStepFiles_G = qSeparateStepFiles
  qFormattedFiles_G = qFormattedFiles
  qMod_G = qMod

  if (qSwitches(iDump_CG)) call DUMPCHIDATA(s_chi_bar_G,s_Normalised_chi_G,tProcInfo_G%rank)
  if (qSwitches(iDump_CG)) call DUMPDATA(sA,sV,tProcInfo_G%rank,NX_G*NY_G*NZ2_G,&
                             iNumberElectrons_G,sZ,istep,tArrayA(1)%tFileType%iPage)

  DEALLOCATE(s_Normalised_chi_G)

  qOK = .TRUE.
  
  GOTO 2000
  
  1000 CALL Error_log('Error in Setup:init',tErrorLog_G)
  
  2000 CONTINUE
  
  END SUBROUTINE init

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE cleanup(sA,sV,sZ)
  
  IMPLICIT NONE

! Cleanup, deallocate, destroy
!
! -Lawrence

  REAL(KIND=WP), ALLOCATABLE, INTENT(INOUT)  :: sV(:)
  REAL(KIND=WP), ALLOCATABLE, INTENT(INOUT)  :: sA(:)
  REAL(KIND=WP), INTENT(IN) :: sZ

! Local

  LOGICAL qOKl

!    Dump data for resumption

  IF (qDump_G) CALL DUMPDATA(sA,sV,tProcInfo_G%rank,NX_G*NY_G*NZ2_G,&
       iNumberElectrons_G,sZ,(istep-1),tArrayA(1)%tFileType%iPage)

!    Deallocate electron and field arrays

  DEALLOCATE(sV)
  DEALLOCATE(sA)

!    Deallocate global positioning arrays

  DEALLOCATE(iBStartPosition_G,&
       iBEndPosition_G, &
       s_chi_bar_G)

!    Deallocate k-value arrays

  IF (qDiffraction_G) THEN
    DEALLOCATE(kx_G)
    DEALLOCATE(ky_G)      
    IF (tTransInfo_G%qOneD) THEN
      IF (tTransInfo_G%loc_nz2_aft_trans/=0) THEN
        DEALLOCATE(kz2_loc_G)
      ENDIF
    ELSE
      IF (tTransInfo_G%loc_nz2/=0) THEN
        DEALLOCATE(kz2_loc_G)
      ENDIF
    ENDIF
  END IF

!    Clear global node numbering arrays

  IF (.NOT. tTransInfo_G%qOneD) THEN
    DEALLOCATE(iGloNumA_G,iNodCodA_G)
  END IF
  
!    Clear FFTW plans

  CALL clearTransformPlans(qOKL)

!    Clear MUMPS structs/arrays

!  IF (qFieldEvolve_G) THEN
!    CALL DESTROYSTRUCTS()
!  END IF

!    Finalize MPI to free processors and end code.

  CALL UnDefineParallelLibrary(qOKL)
  
  GOTO 2000
  
1000 PRINT*, 'ERROR IN cleanuptemp'
   STOP
2000 CONTINUE

  END SUBROUTINE cleanup

END MODULE Setup

