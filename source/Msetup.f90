! ###############################################
! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause
! ###############################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> A module which contains top-level subroutines to allocate and initialize,
!> or destroy, the data used in Puffin.

module Setup

  use setuptrans
  use setupcalcs
  use transforms
  use lattice
  use Globals
  use electronInit
  use Read_data
  use checks
  use ParaField
  use dummyf

  implicit none

  contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine init(sZ, qOK)

  use InitVars

  implicit none

! Subroutine to perform the initialization of
! the data for Puffin, and to write out initial
! values.
!
!                     ARGUMENTS
!
! sZ             Electron propagation distance in z
!                through undulator.
!
! qOK            Error flag; .false. if no error

  real(kind=wp), intent(out) :: sZ
  logical, intent(out) :: qOK

!     Set error flag

  qOK = .false.

!     Initialize the processors for MPI

  call InitializeProcessors(tProcInfo_G,qOKL)
  if (.not. qOKL) goto 1000

!     Optional parameters

  qResume = .false.
  qWrite = .true.

!     Read in input file name
!     (input on command line as variable at runtime)

  call getarg(1,infile)
  zFileName = infile

  if (infile == emptstring) then

    print *, 'ERROR, no input filename specified'
    stop

  end if

  call FileNameNoExtension(zFileName, zFile, qOKL)
  if (.not. qOKL) goto 1000

  zFileName_G = zFile

  igwr = -1_ip

!     Initialise Error log for this run

  tErrorLog_G%zFileName = TRIM(ADJUSTL(zFile))//"_Error.log"
  tErrorLog_G%qFormatted = .true.

  call Error_log('',tErrorLog_G)

!     Read input file



  call read_in(zFileName, &
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
       taper,             &
       zUndType,          &
       sSeedSigma,        &
       freqf, SmeanZ2,    &
       ph_sh, &
       qFlatTopS, nseeds, &
       qSwitches,         &
       qMatched_A,        &
       qmeasure, &
       qOKL)

  if (.not. qOKL) goto 1000

!    Check all the inputs e.g. wiggler and electron lengths etc
!    to avoid errors.



  call calcScaling(srho, saw, sgammar, lambda_w, &
                   zUndType, fx, fy)


  if (.not. qscaled_G) then


    if ((tProcInfo_G%qRoot) .and. (ioutInfo_G > 1)) then
      print*, '*******************'
      print*, ''
      print*, 'Scaling params....'
      print*, ''
    end if

    call scaleParams(sEleSig, sLenEPulse, sSigEj_G, &
                     beamCenZ2, chirp, sEmit_n, emitx, emity, gamma_d, &
                     sFieldModelLength, sLengthofElm, &
                     sSeedSigma)
  end if




  call calcSamples(sFieldModelLength, iNodes, sLengthofElm, &
                   sStepSize, stepsPerPeriod, nSteps, &
                   nperiods, nodesperlambda, gamma_d, sLenEPulse, &
                   iNumElectrons, qSimple)



!  if (qscaled_G) then

  call CheckParameters(sLenEPulse,iNumElectrons,nbeams,sLengthofElm,iNodes,&
                       sFieldModelLength,sStepSize,nSteps,srho,saw,sgammar, &
                       mag, sEleSig,fx,fy, &
                       qSwitches,qSimple, sSeedSigma, freqf, &
                       SmeanZ2, qFlatTopS, nseeds, qOKL)

  if (.not. qOKL) goto 1000

!  end if


!    Setup FFTW plans for the forward and backwards transforms.

  call getTransformPlans4FEL(iNodes,qmeasure,qOKL)

  if (.not. qOKL) goto 1000

!    Calculate parameters for matched beam



  call setupMods(lattFile, taper, sRho, nSteps, sStepSize, fx, fy, &
                  sKBetaXSF_G, sKBetaYSF_G)

  if ((tProcInfo_G%qroot) .and. (ioutInfo_G > 0)) print*, 'setup lattice'

!     Pass local vars to global vars

  call passToGlobals(srho,saw,sgammar,lambda_w,iNodes, &
                     sLengthOfElm, qSimple, iNumElectrons, &
                     fx,fy,taper, sEleSig(1,iX_CG), sEleSig(1,iY_CG), &
                     sFiltFrac,sDiffFrac,sBeta, &
                     zUndType,qFormattedFiles, qSwitches,qOK)

  if (.not. qOKL) goto 1000






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

      call CheckSourceDiff(srho, &
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


  if (iFieldSeedType_G==iSimpleSeed_G) then

    qStart_new = .true.

    call getLocalFieldIndices(sRedistLen_G)

    CALL SetUpInitialValues(nseeds, freqf, &
                            ph_sh, SmeanZ2, &
                            sFiltFrac, qFlatTopS,&
                            sSeedSigma, &
                            sA0_Re,&
                            sA0_Im,&
                            qOKL)

!  send init'd seed field to periodic buffer

    call pupd8(ac_rfield, ac_ifield)

  else if (iFieldSeedType_G==iReadH5Field_G) then

    call readH5FieldfileSingleDump(field_file(1), sFiltFrac)
    call initPowerCalc()

    sFieldModelLength(iX_CG) = sLengthOfElmX_G * real((NX_G-1_ip),kind=wp)
    sFieldModelLength(iY_CG) = sLengthOfElmY_G * real((NY_G-1_ip),kind=wp)

    sLengthOfElm(iX_CG) = sLengthOfElmX_G
    sLengthOfElm(iY_CG) = sLengthOfElmY_G
    delta_G = sLengthOfElmX_G*sLengthOfElmY_G*sLengthOfElmZ2_G

  end if

!  CALL MPI_BARRIER(tProcInfo_G%comm,error)
!  call mpi_finalize(error)
!  stop

  start_step = 1_IP

!  END IF





!    Define the rescaling parameter "ffact" for rescaling
!    backwards transform data.

  if (fieldMesh == iPeriodic) then

    ffact = real(iNodes(iX_CG), kind=wp) * &
            real(iNodes(iY_CG), kind=wp) * &
            real(iNodes(iZ2_CG)-1_ip, kind=wp)

  else

    ffact = real(iNodes(iX_CG), kind=wp) * &
            real(iNodes(iY_CG), kind=wp) * &
            real(iNodes(iZ2_CG), kind=wp)

  end if


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

  if ((tProcInfo_G%qROOT) .and. (ioutInfo_G > 0)) print*, 'Initial data written'


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
