!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE Setup

  USE SETUPTRANS
!  USE FFTW_Constants

  USE setupcalcs
  USE transforms
  USE sddsPuffin
  USE lattice
  USE Globals
  USE resume
  USE electronInit
  USE Read_data
  USE checks
  use dumpFiles
  use ParaField
  use dummyf

! A module which allocates and initializes - or 
! destroys - the data used in Puffin.

  IMPLICIT NONE

  CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE init(sZ, qOK)

  USE InitVars

  IMPLICIT NONE

! Subroutine to perform the initialization of 
! the data for Puffin, and to write out initial
! values.
!
!                     ARGUMENTS
!
!
! sA             Radiation field.
!
! sZ             Electron propagation distance in z
!                through undulator.
! 
! qOK            Error flag; .false. if no error

!  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT)  :: sA(:)
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

  zFileName_G = zFile

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
       sZ,                &
       LattFile,          &
       iWriteNthSteps,    &
       iIntWriteNthSteps, &
       tArrayZ,           &
       tArrayA,           &
       tArrayE,           &
       sLenEPulse,        &
       iNodes,            &
       sFieldModelLength, &
       iRedNodesX,        &
       iRedNodesY,        &
       nodesperlambda, &
       stepsPerPeriod, &
       nperiods, &
       sQe,               &
       q_noise,           &
       iNumElectrons,     &
       sEleSig,           &
       sElectronThreshold,&
       beamCenZ2,         &
       gamma_d,           &
       chirp,             &
       mag, fr,           &
       nbeams,            &
       dist_f,            &
       field_file,        &
       qSimple,           &
       sA0_Re,            &
       sA0_Im,            &
       sFiltFrac,         &
       sDiffFrac,         &
       sBeta,             &
       srho,              &
       saw,               &
       sgammar,           &
       lambda_w,          &
       sEmit_n,           &
       alphax, alphay, emitx, emity, &
       fx,                &
       fy,                &
       Dfact,             &
       sFocusfactor,      &
       taper,             &
       zUndType,          &
       sSeedSigma,        &
       freqf, SmeanZ2,    &
       ph_sh, &
       qFlatTopS, nseeds, &
       sPEOut,            &
       iDumpNthSteps,     &
       qSwitches,         &
       qMatched_A,        &
       qOKL)
  
  IF (.NOT. qOKL) GOTO 1000

!    Check all the inputs e.g. wiggler and electron lengths etc 
!    to avoid errors.



  call calcScaling(srho, saw, sgammar, lambda_w, &
    sFocusFactor, zUndType, fx, fy)

  
  if (.not. qscaled_G) then


    if (tProcInfo_G%qRoot) print*, '*******************'
    if (tProcInfo_G%qRoot) print*, ''
    if (tProcInfo_G%qRoot) print*, 'Scaling params....'
    if (tProcInfo_G%qRoot) print*, ''

    call scaleParams(sEleSig, sLenEPulse, sSigEj_G, &
                     beamCenZ2, chirp, sEmit_n, emitx, emity, gamma_d, &
                     sFieldModelLength, sLengthofElm, &
                     sSeedSigma)
  end if




  call calcSamples(sFieldModelLength, iNodes, sLengthofElm, &
                   sStepSize, stepsPerPeriod, nSteps, &
                   nperiods, nodesperlambda, gamma_d, sLenEPulse, &
                   iNumElectrons)



!  if (qscaled_G) then

  CALL CheckParameters(sLenEPulse,iNumElectrons,nbeams,sLengthofElm,iNodes,&
                       sFieldModelLength,sStepSize,nSteps,srho,saw,sgammar, &
                       sFocusfactor, mag, sEleSig,fx,fy, iRedNodesX, &
                       iRedNodesY,qSwitches,qSimple, sSeedSigma, freqf, & 
                       SmeanZ2, qFlatTopS, nseeds, qOKL)
  
  IF (.NOT. qOKL) GOTO 1000

!  end if


!    Setup FFTW plans for the forward and backwards transforms.

  CALL getTransformPlans4FEL(iNodes,qOKL)
  
  IF (.NOT. qOKL) GOTO 1000

!    Calculate parameters for matched beam










!  IF (qMatched_A(1)) THEN
!
!    if (qSimple) CALL MatchBeams(srho,sEmit_n,saw,sFocusfactor,&
!                    sgammar,gamma_d,iNumElectrons,sLenEPulse,&
!                    sEleSig,sSeedSigma,iNodes,sFieldModelLength,&
!                    sLengthofElm,zUndType,iRedNodesX,iRedNodesY,fx,fy,qOKL)
!
!    IF (.NOT. qOKL) GOTO 1000
!  
!  END IF
!
!
!
!!     Check transverse sampled length of field is long enough to model 
!!     diffraction of the resonant frequency.
!
!  IF (qSwitches(iDiffraction_CG)) THEN
!
!    if (qSimple)  CALL CheckSourceDiff(sStepSize,nSteps,srho, &
!                                       sEleSig, &
!                                       sFieldModelLength,&
!                                       sLengthofElm,iNodes,qOKL)
!
!    IF (.NOT. qOKL) GOTO 1000
!  
!  END IF



  call setupMods(lattFile, taper, sRho, nSteps, sStepSize, fx, fy, &
                  sKBetaXSF_G, sKBetaYSF_G)
      
!     Pass local vars to global vars

  CALL passToGlobals(srho,saw,sgammar,lambda_w,iNodes, &
                     sLengthOfElm, qSimple, iNumElectrons, &
                     fx,fy,sFocusFactor,taper, &
                     sFiltFrac,sDiffFrac,sBeta, &
                     zUndType,qFormattedFiles, qSwitches,qOK)

  IF (.NOT. qOKL) GOTO 1000






  if (.not. qOneD_G) then

    if (qSimple) then

      call stptrns(sEleSig, sLenEPulse, iNumElectrons, &
                   emitx, emity, gamma_d, &
                   qMatched_A, qMatchS_G, qFMesh_G, sSeedSigma)

      sFieldModelLength(iX_CG) = sLengthOfElmX_G * real((NX_G-1_ip),kind=wp)
      sFieldModelLength(iY_CG) = sLengthOfElmY_G * real((NY_G-1_ip),kind=wp)

      sLengthOfElm(iX_CG) = sLengthOfElmX_G
      sLengthOfElm(iY_CG) = sLengthOfElmY_G

      delta_G = sLengthOfElmX_G*sLengthOfElmY_G*sLengthOfElmZ2_G

    end if


    if (qSwitches(iDiffraction_CG)) then

      call CheckSourceDiff(sStepSize,nSteps,srho, &
                           sEleSig, &
                           sFieldModelLength,&
                           sLengthofElm,iNodes,qOKL)

      if (.not. qOKL) goto 1000
  
    end if

  end if

  if (qsdds_G) call initPFile(tPowF, qFormattedFiles) ! initialize power file type

  if (.not. qResume_G) call initPowerCalc()

!     Generate macroelectrons


!   Fixing charge only for 1st beam

  if (qFixCharge_G) then

    call fixCharge(sQe(1), sEleSig(1,iZ2_CG), sLenEPulse(1,iZ2_CG), &
                       sSigEj_G(1), qRndEj_G(1), sEleSig(1,iX_CG), &
                       sEleSig(1,iY_CG))

  end if

  call PopMacroElectrons(qSimple, dist_f, sQe,iNumElectrons,q_noise,sZ,sLenEPulse,&
                         sEleSig, alphax, alphay, emitx, emity, beamCenZ2,gamma_d,&
                         sElectronThreshold,chirp, mag, fr, &
                         nbeams, qOK)

  IF (.NOT. qOKL) GOTO 1000  


  if (qresume_G) then

    iUnd_cr = tInitData_G%iUnd_cr
    iChic_cr = tInitData_G%iChic_cr
    iDrift_cr = tInitData_G%iDrift_cr
    iQuad_cr = tInitData_G%iQuad_cr
    iModulation_cr = tInitData_G%iModulation_cr
    
  end if


!    IF qresume is .TRUE. then we are reading in data from the
!    dump files from a previous run....

!  IF (qResume) THEN

    !CALL InitFD(sA,sZ,qOKL)

    !IF (.NOT. qOKL) GOTO 1000  
  

!  ELSE

!    ...or if qResume is .FALSE. then we are setting up the data
!    ourselves....

!    ALLOCATE(sA(nFieldEquations_CG*iNumberNodes_G)) 

  qStart_new = .true.

  call getLocalFieldIndices(sRedistLen_G)




  
  CALL SetUpInitialValues(nseeds, freqf, ph_sh, SmeanZ2, qFlatTopS,&
                            sSeedSigma, &
                            sA0_Re,&
                            sA0_Im,&
                            field_file, &
                            qOKL)
  
  if (qResume_G) call initPowerCalc()
                            sFieldModelLength(iX_CG) = sLengthOfElmX_G * real((NX_G-1_ip),kind=wp)
                            sFieldModelLength(iY_CG) = sLengthOfElmY_G * real((NY_G-1_ip),kind=wp)

                            sLengthOfElm(iX_CG) = sLengthOfElmX_G
                            sLengthOfElm(iY_CG) = sLengthOfElmY_G

                            delta_G = sLengthOfElmX_G*sLengthOfElmY_G*sLengthOfElmZ2_G

!  CALL MPI_BARRIER(tProcInfo_G%comm,error)
!  call mpi_finalize(error)
!  stop

  start_step = 1_IP
  	
!  END IF





!    Define the rescaling parameter "ffact" for rescaling
!    backwards transform data.

  ffact = real(iNodes(iX_CG), kind=wp) * &
          real(iNodes(iY_CG), kind=wp) * &
          real(iNodes(iZ2_CG), kind=wp)      
   	


!  IF (qResume) THEN
!    CALL READINCHIDATA(s_chi_bar_G,s_Normalised_chi_G,tProcInfo_G%rank)
  !ELSE
  !  CALL DUMPCHIDATA(s_chi_bar_G,s_Normalised_chi_G,tProcInfo_G%rank)
!  ENDIF





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

  if (qsdds_G) then

    if (tProcInfo_G%qROOT) print*, 'Writing parameter data to file' 

    qSeparateStepFiles_G = qSeparateStepFiles

    call WriteEleData(zDataFileName,'Chi','s_chi_bar',qFormattedFiles, &
         iGlonumElectrons_G,s_chi_bar_G,qOKL)
    if (.not. qOKL) goto 1000
  
    call WriteEleData(zDataFileName,'NormChi','s_Normalised_chi',qFormattedFiles, &
                      iGlonumElectrons_G,s_Normalised_chi_G,qOKL)
    if (.not. qOKL) goto 1000
  
    if (tProcInfo_G%qRoot) then
      call WriteParameterData(zDataFileName,&
                             iNodes,&
                             iNumElectrons(1,:),&
                             sLengthOfElm, &
                             sStepSize,    &
                             nSteps,&
                             sLenEPulse(1,:),&
                             sFieldModelLength,&
                             sEleSig(1,:),&
                             sA0_Re(1),&
                             sA0_Im(1),&
                             srho,&
                             saw_G,&
                             sEta_G,&
                             sGammaR_G,&
                             sKBeta_G, &
                             sFocusfactor_G, &
                             lam_w_G, lam_r_G, &
                             lg_G, lc_G, &
                             npk_bar_G, &  
                             iGloNumElectrons_G,&
                             nFieldEquations_CG,&
                             nElectronEquations_CG,&  
                             sZ,&
                             iWriteNthSteps, &
                             iIntWriteNthSteps, &
                             sSeedSigma(1,:),&
                             qSwitches,&
                             fx,fy,&
                             qOKL)                             
      if (.not. qOKL) goto 1000
          
    end if
  end if

!    Write out initial values of electron and field data.
!    If not using separate files for each step then open 
!    file - In EArrayFunctions.f90 line 449

  CALL MPI_BARRIER(tProcInfo_G%comm,error)


!  call writeIM(sA, Ar_local, sZ, &
!               zDataFileName, iStep, iWriteNthSteps, &
!               lrecvs, ldispls, &
!               iIntWriteNthSteps, nSteps, qWDisp, qOKL)

!  if (qWrite)  call wr_sdds(sZ, 0, tArrayA, tArrayE, tArrayZ, &
!                 iIntWriteNthSteps, iWriteNthSteps, .true., &
!                 zDataFileName, .true., .true., qOK)

  iCSteps = 0_ip

  if (.not. qResume_G) then

    call writeIM(sZ, sZlSt_G, &
                 zDataFileName, 0_ip, 0_ip, 0_ip, iWriteNthSteps, &
                 iIntWriteNthSteps, nSteps, qOKL)

  end if

!  iCsteps = 1_ip

!  if (qWrite) call wdfs(sA, sZ, 0, tArrayA, tArrayE, tArrayZ, &
!                        iIntWriteNthSteps, iWriteNthSteps, &
!                        qSeparateStepFiles, zDataFileName, .false., qOKL)

  if (.not. qOKL) goto 1000

!   IF (qWrite.AND.(.NOT.(qSeparateStepFiles))) THEN
!     IF(tProcInfo_G%qROOT) PRINT *,&
!          'Writing field and electron values to a single file'
!     CALL SetUpDataFiles(zDataFileName, &
!          qFormattedFiles, &
!          tArrayZ, &
!          tArrayA, &
!          tArrayE, &
!          qOKL)   
!     IF (.NOT. qOKL) GOTO 1000
  
!   END IF
     
! !    Write initial result to file - see line 374 for 
! !    "WriteIntegrationData" routine

!   CALL WriteData(qSeparateStepFiles,&
!       zDataFileName,tArrayZ,tArrayA,tArrayE,&
!       iStep,sZ,sA,sV,.TRUE.,qFormattedFiles,qOKL)
!   IF (.NOT. qOKL) GOTO 1000
   
  CALL MPI_BARRIER(tProcInfo_G%comm,error)
  
  If(tProcInfo_G%qROOT) PRINT *, 'Initial data written'
  
  
!  if (qSwitches(iDump_CG)) call DUMPCHIDATA(s_chi_bar_G,s_Normalised_chi_G,tProcInfo_G%rank)
!  if (qSwitches(iDump_CG)) call DUMPDATA(sA,tProcInfo_G%rank,NX_G*NY_G*NZ2_G,&
!                             iNumberElectrons_G,sZ,istep,tArrayA(1)%tFileType%iPage)

  DEALLOCATE(s_Normalised_chi_G)
 
  qOK = .TRUE.
  
  GOTO 2000
  
  1000 CALL Error_log('Error in Setup:init',tErrorLog_G)
  
  2000 CONTINUE
  
  END SUBROUTINE init

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE cleanup(sZ)
  
  IMPLICIT NONE

! Cleanup, deallocate, destroy
!
! -Lawrence

!  REAL(KIND=WP), ALLOCATABLE, INTENT(INOUT)  :: sA(:)
  REAL(KIND=WP), INTENT(IN) :: sZ

! Local

  LOGICAL qOKl

!    Dump data for resumption

!  IF (qDump_G) CALL DUMPDATA(sA,tProcInfo_G%rank,NX_G*NY_G*NZ2_G,&
!       iNumberElectrons_G,sZ,(istep-1),tArrayA(1)%tFileType%iPage)

!    Deallocate electron and field arrays

  DEALLOCATE(sElPX_G, sElPY_G, sElGam_G)
  DEALLOCATE(sElX_G, sElY_G, sElZ2_G)
!  DEALLOCATE(sA)

!    Deallocate global positioning arrays

  DEALLOCATE(s_chi_bar_G)

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

