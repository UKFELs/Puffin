! ###############################################
! Copyright 2012-2018, University of Strathclyde
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

   use setuptrans, only: stptrns, checksourcediff, WP, IP, tProcInfo_G, iX_CG, iY_CG, tErrorLog_G, &
     log_error
   use setupcalcs, only: passtoglobals, fixcharge, setupinitialvalues, scaleparams, calcscaling, &
     calccharge, calcsamples, popmacroelectrons, iZ2_CG, iDiffraction_CG, tSimulationContext
   use transforms, only: gettransformplans4fel, cleartransformplans, getkvalues, tTransInfo_G
   use lattice, only: setupmods
   use Globals, only: NX_G, NY_G, sLengthOfElmX_G, sLengthOfElmY_G, sLengthOfElmZ2_G, kx_G, ky_G, &
     kz2_loc_G, fieldMesh, iPeriodic, delta_G, qMatchS_G, qFMesh_G, qFixCharge_G, s_chi_bar_G, &
     s_Normalised_chi_G, qRndEj_G, sSigEj_G, iFieldSeedType_G, iSimpleSeed_G, iReadH5Field_G, &
     sElX_G, sElY_G, sElZ2_G, sElPX_G, sElPY_G, sElGam_G, sZlSt_G, tInitData_G, sKBetaXSF_G, &
     sKBetaYSF_G, ffact, start_step, sStepSize, nSteps, sRedistLen_G, tArrayE, tArrayA, tArrayZ, &
     iWriteNthSteps, iIntWriteNthSteps, zFileName_G, ioutInfo_G, frecvs, fdispls, qDiffraction_G, &
     qResume_G, qResume, qWrite, qOneD_G, qscaled_G
   use Read_data, only: read_in, filenamenoextension, initializeprocessors, &
     readh5fieldfilesingledump
   use checks, only: checkparameters
   use ParaField, only: ac_rfield, ac_ifield, qStart_new, getlocalfieldindices, pupd8
   use write_adapter, only: writeim
   use avwrite, only: initPowerCalc
   use mpi, only: mpi_barrier

   implicit none (type, external)
private

public :: cleanup, init


contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   subroutine init(infile, sZ, ctx, qOK)
      use InitVars, only: sLenEPulse, iNumElectrons, sEleSig, sQe, beamCenZ2, gamma_d, chirp, &
        sEmit_n, mag, fr, sA0_Re, sA0_Im, nbeams, qMatched_A, Ipk, alphax, alphay, emitx, emity, &
        iMPsZ2PerWave, nseeds, freqf, ph_sh, SmeanZ2, qFlatTopS, sSeedSigma, emptstring, &
        zUndType, sFieldModelLength, q_noise, qMeasure, sElectronThreshold, sDiffFrac, sBeta, &
        srho, saw, sgammar, lambda_w, fx, fy, qOKL, qSwitches, qSeparateStepFiles, &
        qFormattedFiles, sFiltFrac, taper, qSimple, dist_f, field_file, zFileName, zFile, &
        LattFile, sLengthOfElm, iNodes, nodesperlambda, stepsPerPeriod, nperiods, qWrite, &
        qResume, error, WP, tProcInfo_G, tErrorLog_G, log_error, filenamenoextension, &
        initializeprocessors
      use GlobalTypes, only: tSimulationContext, iX_CG, iY_CG, iZ2_CG, iDiffraction_CG
      use AdapterGlobals, only: PopulateFieldMeshFromGlobals, PopulateSimulationFlagsFromGlobals, &
        PopulateOutputConfigFromGlobals, PopulateLatticeElementsFromGlobals, &
        PopulateIntegrationStateFromGlobals, PopulateUndulatorFromGlobals
      implicit none (type, external)

! Subroutine to perform the initialization of
! the data for Puffin, and to write out initial
! values.
!
!                     ARGUMENTS
!
! sZ             Electron propagation distance in z
!                through undulator.
!
! ctx            Simulation context (populated from globals before writeIM)
!
! qOK            Error flag; .false. if no error

      character(1024_IP), intent(in) :: infile
      real(kind=wp), intent(out) :: sZ
      type(tSimulationContext), intent(inout) :: ctx
      logical, intent(out) :: qOK

!     Set error flag

      qOK = .false.

!     Initialize the processors for MPI

      call InitializeProcessors(tProcInfo_G,qOKL)
      if (.not. qOKL) goto 1000

!     Optional parameters

      qResume = .false.
      qWrite = .true.

      zFileName = infile

      if (infile == emptstring) then

         print *, "ERROR, no input filename specified"
         stop

      end if

      call FileNameNoExtension(zFileName, zFile, qOKL)
      if (.not. qOKL) goto 1000

      zFileName_G = zFile

!     Initialise Error log for this run

      tErrorLog_G%zFileName = TRIM(ADJUSTL(zFile))//"_Error.log"
      tErrorLog_G%qFormatted = .true.

      call log_error("",tErrorLog_G)

!     Rlog_error file



      call read_in(zFileName, &
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
         Ipk, &
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
         iMPsZ2PerWave, &
         qFlatTopS, nseeds, &
         qSwitches,         &
         qMatched_A,        &
         qmeasure, &
         qOKL)

      if (.not. qOKL) goto 1000

!    Check all the inputs e.g. wiggler and electron lengths etc
!    to avoid errors.



      call calcScaling(srho, saw, sgammar, lambda_w, &
         zUndType, fx, fy, ctx)


      if (.not. qscaled_G) then


         if ((tProcInfo_G%qRoot) .and. (ioutInfo_G > 1)) then
            print*, "*******************"
            print*, ""
            print*, "Scaling params...."
            print*, ""
         end if

         call scaleParams(sEleSig, sLenEPulse, sSigEj_G, &
            beamCenZ2, chirp, sEmit_n, emitx, emity, gamma_d, &
            sFieldModelLength, sLengthofElm, &
            sSeedSigma, sA0_Re, sA0_Im, SmeanZ2, fr, sKBetaXSF_G, sKBetaYSF_G, ctx%frame)
      end if


      sA0_Re = sqrt(2.0_wp*sA0_Re)
      sA0_Im = sqrt(2.0_wp*sA0_Im)  ! Convert intensity to peak field magnitude


      call calcSamples(sFieldModelLength, iNodes, sLengthofElm, &
         sStepSize, stepsPerPeriod, nSteps, &
         nperiods, nodesperlambda, gamma_d, sEleSig, sLenEPulse, &
         iNumElectrons, iMPsZ2PerWave, qSimple, ctx%frame)

      call calcCharge(sQe, Ipk, sEleSig(:,iZ2_CG), sLenEPulse(:, iZ2_CG), sSigEj_G, &
                       qRndEj_G, ctx%frame)

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


      call setupMods(lattFile, taper, sRho, nSteps, sStepSize, fx, fy, &
         sKBetaXSF_G, sKBetaYSF_G, ctx%frame)

      if ((tProcInfo_G%qroot) .and. (ioutInfo_G > 0)) print*, "setup lattice"

!     Pass local vars to global vars

      call passToGlobals(srho,saw,sgammar,lambda_w,iNodes, &
         sLengthOfElm, qSimple, iNumElectrons, &
         fx,fy,taper, sEleSig(1,iX_CG), sEleSig(1,iY_CG), &
         sFiltFrac,sDiffFrac,sBeta, &
         zUndType,qFormattedFiles, qSwitches, ctx, qOK)

      if (.not. qOKL) goto 1000






      if (.not. qOneD_G) then

         if (qSimple) then

            call stptrns(sEleSig, sLenEPulse, iNumElectrons, &
               emitx, emity, gamma_d, &
               qMatched_A, qMatchS_G, qFMesh_G, sSeedSigma, ctx%frame)

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
         nbeams, ctx%frame, ctx%flags, ctx%und%n2col, qOK)

      IF (.NOT. qOKL) GOTO 1000


      if (iFieldSeedType_G==iSimpleSeed_G) then

         qStart_new = .true.

         call getLocalFieldIndices(sRedistLen_G, ctx%flags, ctx%frame)

         CALL SetUpInitialValues(nseeds, freqf, &
            ph_sh, SmeanZ2, &
            sFiltFrac, qFlatTopS,&
            sSeedSigma, &
            sA0_Re,&
            sA0_Im,&
            ctx%frame%rho, &
            qOKL)

!  send init'd seed field to periodic buffer

         call pupd8(ac_rfield, ac_ifield)

      else if (iFieldSeedType_G==iReadH5Field_G) then

         call readH5FieldfileSingleDump(field_file(1), sFiltFrac, ctx%frame, ctx%flags)
         call initPowerCalc()

         sFieldModelLength(iX_CG) = sLengthOfElmX_G * real((NX_G-1_ip),kind=wp)
         sFieldModelLength(iY_CG) = sLengthOfElmY_G * real((NY_G-1_ip),kind=wp)

         sLengthOfElm(iX_CG) = sLengthOfElmX_G
         sLengthOfElm(iY_CG) = sLengthOfElmY_G
         delta_G = sLengthOfElmX_G*sLengthOfElmY_G*sLengthOfElmZ2_G

      end if

      start_step = 1_IP


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

!     Fully populate ctx from all globals now set by init so that puffin_main
!     needs no further Populate calls after init returns.
!
!     This MUST happen before the initial write below. The Populate routines
!     reset per-run state to its start-of-run value - in particular
!     PopulateFieldMeshFromGlobals hardcodes highpass_filter_gr to -1, and that
!     is the running output file index, which wr_h5 advances on every dump.
!     Populating after the initial write would rewind the index and make the
!     next dump overwrite the step-0 files.
      call PopulateFieldMeshFromGlobals(ctx%mesh)
      call PopulateSimulationFlagsFromGlobals(ctx%flags)
      call PopulateOutputConfigFromGlobals(ctx%output)
      call PopulateLatticeElementsFromGlobals(ctx%lattice)
      call PopulateIntegrationStateFromGlobals(ctx%integration)
      call PopulateUndulatorFromGlobals(ctx%und)
      ctx%init_data = tInitData_G

!     Start-of-run zeroing of the accumulated interaction length (old sZi_G).
!     This is the ONLY place it may be zeroed - it must accumulate across all
!     undulator modules, so PopulateIntegrationStateFromGlobals deliberately
!     leaves it alone (it is called once per module from UndSection).
!     On a resume, UndSection overwrites it with tInitData_G%Zbarinter.
      ctx%integration%z_inter = 0.0_wp

      ! Override element-type counters and mesh state from restart data
      ! (PopulateLatticeElementsFromGlobals hardcodes counters to 1;
      !  PopulateFieldMeshFromGlobals hardcodes highpass_filter_gr to -1)
      if (qresume_G) then
        ctx%lattice%current_und_index = tInitData_G%iUnd_cr
        ctx%lattice%current_chic_index = tInitData_G%iChic_cr
        ctx%lattice%current_drift_index = tInitData_G%iDrift_cr
        ctx%lattice%current_quad_index = tInitData_G%iQuad_cr
        ctx%lattice%current_modulation_index = tInitData_G%iModulation_cr
        ctx%mesh%highpass_filter_gr = tInitData_G%igwr
      end if

      if (.not. qResume_G) then

        ctx%lattice%cumulative_steps = 0_ip
        ctx%integration%current_step = 0_ip

        call writeIM(sZ, sZlSt_G, ctx, 0_ip, qOKL)

      end if

      if (.not. qOKL) goto 1000


      CALL MPI_BARRIER(tProcInfo_G%comm,error)

      if ((tProcInfo_G%qROOT) .and. (ioutInfo_G > 0)) print*, "Initial data written"
      deallocate(s_Normalised_chi_G)

      qOK = .true.

      goto 2000

1000  call log_error("Error in Setup:init",tErrorLog_G)

2000  continue

   END SUBROUTINE init

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE cleanup(sZ)

      IMPLICIT NONE (type, external)

! Cleanup, deallocate, destroy
!
! -Lawrence

!  REAL(KIND=WP), ALLOCATABLE, INTENT(INOUT)  :: sA(:)
      REAL(KIND=WP), INTENT(IN) :: sZ

! Local

      LOGICAL :: qOKl

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
            END IF
         ELSE
            IF (tTransInfo_G%loc_nz2/=0) THEN
               DEALLOCATE(kz2_loc_G)
            END IF
         END IF
      END IF

!    Clear FFTW plans

      CALL clearTransformPlans(qOKL)

!    Clear MUMPS structs/arrays

!  IF (qFieldEvolve_G) THEN
!    CALL DESTROYSTRUCTS()
!  END IF

!    Finalize MPI to free processors and end code.

      GOTO 2000

      PRINT*, "ERROR IN cleanuptemp"
      STOP
2000  CONTINUE

   END SUBROUTINE cleanup

END MODULE Setup
