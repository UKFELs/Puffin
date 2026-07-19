! Copyright 2012-2018, University of Strathclyde
! Authors: Jonathan Smith (Tech-X UK Ltd) & Lawrence T. Campbell
! License: BSD-3-Clause

module hdf5_puff

use ArrayFunctions, only: cArraySegment, tProcInfo_G, tErrorLog_G, log_error
USE puffin_constants, only: pi, c, e_0, m_e, q_e
USE Globals, only: NZ2_G, npts_I_G, dz2_I_G, ata_G, tArrayE, tArrayA, tArrayZ, qOneD_G
use avWrite, only: gpowerp, getcurr, getslicetwiss, fr_rfield, bk_rfield, ac_rfield, fr_ifield, &
  bk_ifield, ac_ifield, mainlen, tlflen, tlelen
use puffin_kinds, only: WP, IP
use hdf5PuffID, only: outputh5beamfilesid, outputh5field3did
use hdf5PuffColl, only: outputh5beamfilessd, outputh5field3dsd, outputh5field1d2compsd, &
  createintegrated1dfloat, addh5field1dfloat
use GlobalTypes, only: tSimulationContext
use ParaField, only: fz2, ez2, ffs, ffe, ees, eee

!use MPI
use hdf5PuffLow, only: iStep
implicit none (type, external)

contains

!> Overall script to write the larger full h5 output data files
!! Not including the integrated quantities
!! @ Todo remove unused vars - filename parameters are not required.

  subroutine wr_h5(sZ, sZ_loc, tArrayA, tArrayE, tArrayZ, iL, &
                   iIntWr, iWr, qSep, qWriteFull, &
                   qWriteInt, nslices, ctx, qOK)

    implicit none (type, external)

    real(kind=wp), intent(in) :: sZ, sZ_loc  !< zbar and local zbar for current module
    real(kind=wp), dimension(NZ2_G) :: power !<power data (called here)
    real(kind=wp), dimension(NZ2_G) :: wrFArray
    real(kind=wp), dimension(npts_I_G) :: Iarray !< current data (called here)
    type(cArraySegment), intent(inout) :: tArrayA(:), tArrayE(:), tArrayZ
    integer(kind=ip), intent(in)  :: nslices, iL
    real(kind=wp), dimension(nslices) :: aveX,aveY,avePX,avePY,aveGamma,aveDgamma
    real(kind=wp), dimension(nslices) :: sdX, sdY, sdpx, sdpy, eX, ey, aX, aY, bX, bY
    real(kind=wp), dimension(nslices) :: bun1,bun2,bun3,bun4,bun5,sq, wrEArray, avGam4Unsc

    integer(kind=ip), intent(in) :: iIntWr, iWr
    logical, intent(in) :: qSep !< whether to write separate files
    type(tSimulationContext), intent(inout) :: ctx
    logical, intent(inout) :: qOK  !< Flag set if any probs happen
    integer :: error, numSpatialDims
    real(kind=wp) :: slicetrim
    logical, intent(in) :: qWriteInt, qWriteFull !<Flags identifying if it is time to write
    real(kind=wp) :: time,stime,ftime !<Simulation time, calcualted here
    real(kind=wp) :: PowScale !< Scaling factor for power
    error = 0
    slicetrim=(4*pi*ctx%frame%rho*nslices)-ctx%mesh%dz2*ctx%mesh%nz2

    ctx%mesh%highpass_filter_gr = ctx%mesh%highpass_filter_gr + 1_ip
    time = sZ

    if (qWriteFull) then

!      time = sZ ! real(iCSteps,kind=wp)*sStepSize*lg_G/c

      if (qSep) then
!        print *,'Dumping particles individually...'

        call cpu_time(stime)
        call outputH5BeamFilesID(time, sz_loc, iL, error, ctx)
        call cpu_time(ftime)
!        print '("Dumped particles separately. Took time = ",f6.3," secs on rank ",i5)'&
!          ,ftime-stime,tProcInfo_G%rank

      else

!        print *,'Dumping particles to single file...'
        call cpu_time(stime)
        call outputH5BeamFilesSD(time, sz_loc, iL, error, ctx)
        call cpu_time(ftime)
!        print '("Dumped particles to one file. Took time = ",f6.3," seconds on rank ",i5)'&
!          ,ftime-stime,tProcInfo_G%rank

      end if

      if (error /= 0) goto 1000

      if (qONED_G) then

        numSpatialDims=1

!      print *,'2 component 1D field output not currently supported'
!        print *, "But trying anyway - Dumping all fields together"
        call cpu_time(stime)
        call outputH5Field1D2CompSD(time, sz_loc, iL, error, tlflen, fr_rfield, &
                                     ffs, ffe, 0, 1, .false., ctx)
        call outputH5Field1D2CompSD(time, sz_loc, iL, error, tlflen, fr_ifield, &
                                     ffs, ffe, 1, 2, .false., ctx)
        call outputH5Field1D2CompSD(time, sz_loc, iL, error, mainlen, ac_rfield, &
                                     fz2, ez2, 0, 2, .true., ctx)
        call outputH5Field1D2CompSD(time, sz_loc, iL, error, mainlen, ac_ifield, &
                                     fz2, ez2, 1, 2, .true., ctx)
        call outputH5Field1D2CompSD(time, sz_loc, iL, error, tlelen, bk_rfield, &
                                     ees, eee, 0, 2, .false., ctx)
        call outputH5Field1D2CompSD(time, sz_loc, iL, error, tlelen, bk_ifield, &
                                     ees, eee, 1, 2, .false., ctx)
        call cpu_time(ftime)

!        print '("Dumped fields together. Took time = ",f6.3," secs on rank ",i5)' &
!          ,ftime-stime,tprocinfo_g%rank

      ! todo generalize/split 1D field output to take two component fields.
! to do raw data write in one place, and prob limits, and write other
! field attributes which are shared elsewhere. Lims are the same. Mesh
! is (probably) the same

! signature: nlonglength, dsetname, data, nlo, nhi, chkactiveflag
! tlflen, 'aperp_front_real', fr_rfield, [ffs,ffe], .false.
! tlflen, 'aperp_front_imag', fr_ifield, [ffs,ffe], .false.
! mainlen, 'aperp_active_real', ac_rfield, [fz2,ez2], .true.
! mainlen, 'aperp_active_imag', ac_ifield, [fz2,ez2], .true.
! tlelen, 'aperp_back_real', bk_rfield, [ees,eee], .false.
! tlelen, 'aperp_back_imag', bk_ifield, [ees,eee], .false.
! final argument  checks for all active field on single root node ...
! should say if qUnique or rank=0...

!!!
! This is the behaviour for individual dumping.
!


      else

        numSpatialDims=3

        if (qSep) then

!        print *, "Dumping separate fields"

          call cpu_time(stime)
          call outputH5Field3DID(time, sz_loc, iL, error, tlflen, "aperp_front_real", &
                                  fr_rfield,  ffs, ffe, .false., ctx)
          call outputH5Field3DID(time, sz_loc, iL, error, tlflen, "aperp_front_imag", &
                                  fr_ifield,  ffs, ffe, .false., ctx)
          call outputH5Field3DID(time, sz_loc, iL, error, mainlen, "aperp_active_real", &
                                  ac_rfield,  fz2, ez2, .true., ctx)
          call outputH5Field3DID(time, sz_loc, iL, error, mainlen, "aperp_active_imag", &
                                  ac_ifield,  fz2, ez2, .true., ctx)
          call outputH5Field3DID(time, sz_loc, iL, error, tlelen, "aperp_back_real", &
                                  bk_rfield,  ees, eee, .false., ctx)
          call outputH5Field3DID(time, sz_loc, iL, error, tlelen, "aperp_back_imag", &
                                  bk_rfield,  ees, eee, .false., ctx)
          call cpu_time(ftime)
!        print '("Dumped separate fields. Took time = ",f6.3," secs on rank ",i5)' &
!          ,ftime-stime,tprocinfo_g%rank

        else

!        print *, "Dumping all fields together"
          call cpu_time(stime)
          call outputH5Field3DSD(time, sz_loc, iL, error, tlflen, fr_rfield, &
                                  ffs, ffe, 0, 1, .false., ctx)
          call outputH5Field3DSD(time, sz_loc, iL, error, tlflen, fr_ifield, &
                                  ffs, ffe, 1, 2, .false., ctx)
          call outputH5Field3DSD(time, sz_loc, iL, error, mainlen, ac_rfield, &
                                  fz2, ez2, 0, 2, .true., ctx)
          call outputH5Field3DSD(time, sz_loc, iL, error, mainlen, ac_ifield, &
                                  fz2, ez2, 1, 2, .true., ctx)
          call outputH5Field3DSD(time, sz_loc, iL, error, tlelen, bk_rfield, &
                                  ees, eee, 0, 2, .false., ctx)
          call outputH5Field3DSD(time, sz_loc, iL, error, tlelen, bk_ifield, &
                                  ees, eee, 1, 2, .false., ctx)
          call cpu_time(ftime)
!        print '("Dumped fields together. Took time = ",f6.3," secs on rank ",i5)' &
!          ,ftime-stime,tprocinfo_g%rank
        end if
!      call outputH5Field3DSDattrs(time, error, tlflen, fr_rfield,  ffs, ffe, 0, .false.)
        if (error /= 0) goto 1000

      end if

! Zposition is probably to be recorded not at each timestep
! Todo: move into main
!      call outputH5Z(sZ, tArrayZ, iStep, qSep, zDFName, qOKL)
!      if (.not. qOKL) goto 1000

    end if



    if (qWriteInt) then

! These call requires all ranks to participate
      call gPowerP(power)
      call getCurr(dz2_I_G, Iarray, ctx%frame)
      call getSliceTwiss(nslices,slicetrim,aveX,aveY,avePX,avePY, &
        sdX,sdY,sdpx,sdpy,eX,eY,ax,ay,bx,by,aveGamma,aveDgamma, &
        bun1,bun2,bun3,bun4,bun5,sq,ctx%frame)

! For starters, write on rank 0 only


! but the write operation does not, as the data has been collected on rank0.

      if (tProcInfo_G%qRoot) then

        PowScale = ctx%frame%gain_length * ctx%frame%cooperation_length * c * e_0 * &
                   ((ctx%frame%gamma_ref * m_e * c**2.0_wp ) / &
                   (q_e * ctx%frame%kappa * ctx%frame%gain_length))**2.0_wp

        avGam4Unsc = aveGamma
        where (avGam4Unsc == 0.0_wp) avGam4Unsc = 1.0_wp

        call CreateIntegrated1DFloat(time, sz_loc, iL, error, nslices, ctx)

        if (qOneD_G) then

          call addH5Field1DFloat(power, "Intensity", "intFieldMeshSc", &
                                "z2, Intensity (Scaled)", time, sz_loc, iL, error, ctx)

          wrFArray = power * powScale / ctx%frame%gain_length / ctx%frame%cooperation_length

          call addH5Field1DFloat(wrFArray, "IntensitySI", "intFieldMeshSI", &
                                "ct-z (m), Intensity (Wm-2)", time, sz_loc, iL, error, ctx)

          wrFArray = power * ata_G

          call addH5Field1DFloat(wrFArray, "power", "intFieldMeshSc", &
                                "z2, Power (Scaled)", time, sz_loc, iL, error, ctx)

          wrFArray = power * powScale * ata_G

          call addH5Field1DFloat(wrFArray, "powerSI", "intFieldMeshSI", &
                                "ct-z (m), Power (W)", time, sz_loc, iL, error, ctx)

        else

          call addH5Field1DFloat(power, "power", "intFieldMeshSc", &
                                "z2, Power (Scaled)", time, sz_loc, iL, error, ctx)

          wrFArray = power * powScale

          call addH5Field1DFloat(wrFArray, "powerSI", "intFieldMeshSI", &
                                "ct-z (m), Power (W)", time, sz_loc, iL, error, ctx)

        end if

        call addH5Field1DFloat(Iarray, "beamCurrent",  "intCurrMeshSc", &
                               "z2, Current (A)", time, sz_loc, iL, error, ctx)

        call addH5Field1DFloat(Iarray, "beamCurrentSI",  "intCurrMeshSI", &
                               "ct-z, Current (A)", time, sz_loc, iL, error, ctx)


        call addH5Field1DFloat(aveX, "meanXbar", "intPtclMeshSc", &
                               "z2, xbar", time, sz_loc, iL, error, ctx)

        wrEArray = aveX * (SQRT(ctx%frame%gain_length*ctx%frame%cooperation_length))

        call addH5Field1DFloat(wrEArray, "meanXSI", "intPtclMeshSI", &
                               "ct-z (m), x (m)", time, sz_loc, iL, error, ctx)






        call addH5Field1DFloat(aveY, "meanYbar", "intPtclMeshSc", &
                               "z2, ybar", time, sz_loc, iL, error, ctx)

        wrEArray = aveY * (SQRT(ctx%frame%gain_length*ctx%frame%cooperation_length))

        call addH5Field1DFloat(wrEArray, "meanYSI", "intPtclMeshSI", &
                               "ct-z (m), y (m)", time, sz_loc, iL, error, ctx)





        call addH5Field1DFloat(avepx, "meanPXbar", "intPtclMeshSc", &
                               "z2, pxbar", time, sz_loc, iL, error, ctx)

        wrEArray = avepx * 2.0_wp * ctx%frame%rho * ctx%frame%kappa / avGam4Unsc


        call addH5Field1DFloat(wrEArray, "mean_dxdzSI", "intPtclMeshSI", &
                               "ct-z (m), dxdz", time, sz_loc, iL, error, ctx)




        call addH5Field1DFloat(avepy, "meanPYbar", "intPtclMeshSc", &
                               "z2, pxbar", time, sz_loc, iL, error, ctx)

        wrEArray = -avepy * 2.0_wp * ctx%frame%rho * ctx%frame%kappa / avGam4Unsc


        call addH5Field1DFloat(wrEArray, "mean_dydzSI", "intPtclMeshSI", &
                               "ct-z (m), dxdz", time, sz_loc, iL, error, ctx)






        call addH5Field1DFloat(aveGamma, "meanGamma", "intPtclMeshSc", &
                               "z2, gamma / gamma0", time, sz_loc, iL, error, ctx)


        wrEArray = aveGamma * ctx%frame%gamma_ref * 0.511_wp

        call addH5Field1DFloat(wrEArray, "meanEnergySI", "intPtclMeshSI", &
                               "ct-z (m), E (MeV)", time, sz_loc, iL, error, ctx)




        call addH5Field1DFloat(aveDGamma, "meanSigmaGamma", "intPtclMeshSc", &
                               "z2, sigma_gamma / gamma0", time, sz_loc, iL, error, ctx)

        call addH5Field1DFloat(aveDGamma, "meanSigmaGammaSI", "intPtclMeshSI", &
                               "ct-z (m), sigma_gamma / gamma0", time, sz_loc, iL, error, ctx)





        call addH5Field1DFloat(sdx, "sigmaXbar", "intPtclMeshSc", &
                               "z2, sigma_xbar", time, sz_loc, iL, error, ctx)

        wrEArray = sdx * (SQRT(ctx%frame%gain_length*ctx%frame%cooperation_length))

        call addH5Field1DFloat(wrEArray, "sigmaXSI", "intPtclMeshSI", &
                               "ct-z (m), sigma_x (m)", time, sz_loc, iL, error, ctx)


        call addH5Field1DFloat(sdy, "sigmaYbar", "intPtclMeshSc", &
                               "z2, sigma_ybar", time, sz_loc, iL, error, ctx)

        wrEArray = sdy * (SQRT(ctx%frame%gain_length*ctx%frame%cooperation_length))

        call addH5Field1DFloat(wrEArray, "sigmaYSI", "intPtclMeshSI", &
                               "ct-z (m), sigma_y (m)", time, sz_loc, iL, error, ctx)



        call addH5Field1DFloat(sdpx, "sigmaPxbar", "intPtclMeshSc", &
                               "z2, sigma_pxbar", time, sz_loc, iL, error, ctx)

        wrEArray = sdpx * 2.0_wp * ctx%frame%rho * ctx%frame%kappa / avGam4Unsc

        call addH5Field1DFloat(wrEArray, "sigma_dxdzSI", "intPtclMeshSI", &
                               "ct-z (m), sigma_dxdz", time, sz_loc, iL, error, ctx)




        call addH5Field1DFloat(sdpy, "sigmaPybar", "intPtclMeshSc", &
                               "z2, sigma_pybar", time, sz_loc, iL, error, ctx)

        wrEArray = sdpy * 2.0_wp * ctx%frame%rho * ctx%frame%kappa / avGam4Unsc

        call addH5Field1DFloat(wrEArray, "sigma_dydzSI", "intPtclMeshSI", &
                               "ct-z (m), sigma_dydz", time, sz_loc, iL, error, ctx)



        wrEArray = 2.0_wp * ctx%frame%kappa / sqrt(ctx%frame%eta) /  avGam4Unsc * ex

        call addH5Field1DFloat(wrEArray, "emittanceXbar", "intPtclMeshSc", &
                               "z2, scaled x emittance", time, sz_loc, iL, error, ctx)

        wrEArray = wrEArray * ctx%frame%rho * ctx%frame%cooperation_length

        call addH5Field1DFloat(wrEArray, "emittanceXSI", "intPtclMeshSI", &
                               "ct-z (m), emittance_x (unnormalised)", time, sz_loc, iL, error, ctx)




        wrEArray = 2.0_wp * ctx%frame%kappa / sqrt(ctx%frame%eta) /  avGam4Unsc * ey

        call addH5Field1DFloat(wrEArray, "emittanceYbar", "intPtclMeshSc", &
                               "z2, scaled y emittance", time, sz_loc, iL, error, ctx)

        wrEArray = wrEArray * ctx%frame%rho * ctx%frame%cooperation_length

        call addH5Field1DFloat(wrEArray, "emittanceYSI", "intPtclMeshSI", &
                               "ct-z (m), emittance_y (unnormalised)", time, sz_loc, iL, error, ctx)




        call addH5Field1DFloat(bun1, "bunchingFundamental", "intPtclMeshSc", &
                               "z2, bunching", time, sz_loc, iL, error, ctx)

        call addH5Field1DFloat(bun1, "bunchingFundamentalSI", "intPtclMeshSI", &
                               "ct-z (m), bunching", time, sz_loc, iL, error, ctx)


        call addH5Field1DFloat(bun2, "bunching2ndHarmonic", "intPtclMeshSc", &
                               "z2, 2nd harmonic bunching", time, sz_loc, iL, error, ctx)

        call addH5Field1DFloat(bun2, "bunching2ndHarmonicSI", "intPtclMeshSI", &
                               "ct-z (m), 2nd harmonic bunching", time, sz_loc, iL, error, ctx)



        call addH5Field1DFloat(bun3, "bunching3ndHarmonic", "intPtclMeshSc", &
                               "z2, 3rd harmonic bunching", time, sz_loc, iL, error, ctx)

        call addH5Field1DFloat(bun3, "bunching3ndHarmonicSI", "intPtclMeshSI", &
                               "ct-z (m), 3rd harmonic bunching", time, sz_loc, iL, error, ctx)



        call addH5Field1DFloat(bun4, "bunching4thHarmonic", "intPtclMeshSc", &
                               "z2, 4th harmonic bunching", time, sz_loc, iL, error, ctx)

        call addH5Field1DFloat(bun4, "bunching4thHarmonicSI", "intPtclMeshSI", &
                               "ct-z (m), 4th harmonic bunching", time, sz_loc, iL, error, ctx)



        call addH5Field1DFloat(bun5, "bunching5thHarmonic", "intPtclMeshSc", &
                               "z2, 5th harmonic bunching", time, sz_loc, iL, error, ctx)

        call addH5Field1DFloat(bun5, "bunching5thHarmonicSI", "intPtclMeshSI", &
                               "ct-z (m), 5th harmonic bunching", time, sz_loc, iL, error, ctx)


        call addH5Field1DFloat(sq, "Slice Charge", "intPtclMeshSc", &
                               "z2, Charge", time, sz_loc, iL, error, ctx)

        call addH5Field1DFloat(sq, "Slice Charge SI", "intPtclMeshSI", &
                               "ct-z (m), Charge", time, sz_loc, iL, error, ctx)


        if (error /= 0) goto 1000
! Todo not yet implemented
!     call outputH5SliceEmittance
!       NOT YET IMPLEMENTED

      end if

    end if

!  Set error flag and exit

    error = 0

    goto 2000

! Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call log_error("Error in hdfPuffin:wr_h5",&
          tErrorLog_G)
    print*,"Error in hdfPuffin:wr_h5"
2000 continue

  end subroutine wr_h5













end module hdf5_puff
