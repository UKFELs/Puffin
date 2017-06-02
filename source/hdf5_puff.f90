! Copyright 2012-2017, University of Strathclyde
! Authors: Jonathan Smith (Tech-X UK Ltd) & Lawrence T. Campbell
! License: BSD-3-Clause

module hdf5_puff

USE ArrayFunctions
USE TypesandConstants
USE Globals
USE ParallelSetUp
Use avWrite
use paratype
use HDF5
use lattice
use hdf5PuffID
use hdf5PuffLow
use hdf5PuffColl

!use MPI
implicit none 

contains

!> Overall script to write the larger full h5 output data files
!! Not including the integrated quantities
!! @ Todo remove unused vars - filename parameters are not required.

  subroutine wr_h5(sZ, sZ_loc, tArrayA, tArrayE, tArrayZ, iL, &
                   iIntWr, iWr, qSep, zDFname, qWriteFull, &
                   qWriteInt, nslices, qOK)

    implicit none

    real(kind=wp), intent(in) :: sZ, sZ_loc  !< zbar and local zbar for current module
    real(kind=wp), dimension(NZ2_G) :: power !<power data (called here)
    real(kind=wp), dimension(NZ2_G) :: wrFArray
    real(kind=wp), dimension(npts_I_G) :: Iarray !< current data (called here)
    type(cArraySegment), intent(inout) :: tArrayA(:), tArrayE(:), tArrayZ
    integer(kind=ip), intent(in)  :: nslices, iL
    real(kind=wp), dimension(nslices) :: aveX,aveY,avePX,avePY,aveGamma,aveDgamma
    real(kind=wp), dimension(nslices) :: sdX, sdY, sdpx, sdpy, eX, ey, aX, aY, bX, bY
    real(kind=wp), dimension(nslices) :: bun1,bun2,bun3,bun4,bun5,sq, wrEArray, avGam4Unsc

    integer(kind=ip), intent(in) :: iIntWr, iWr !<Aren't these global?
    character(1024_IP), intent(in) :: zDFName 
    logical, intent(in) :: qSep !< Probably not used here, whether to write separate files
    logical, intent(inout) :: qOK  !< Flag set if any probs happen
    integer :: error, numSpatialDims
    real(kind=wp) :: slicetrim
    logical :: qWriteInt, qWriteFull !<Flags identifying if it is time to write
    real(kind=wp) :: time,stime,ftime !<Simulation time, calcualted here
    real(kind=wp) :: PowScale !< Scaling factor for power
    error = 0
    slicetrim=(4*pi*srho_g*nslices)-sLengthOfElmZ2_G*NZ2_G

! now this is passed in so that we can set the arrays properly

    !    nslices = int( (sLengthOfElmZ2_G*NZ2_G)/(4*pi*srho_g))

    time = sZ

    if (qWriteFull) then

!      time = sZ ! real(iCSteps,kind=wp)*sStepSize*lg_G/c

      if (qSep) then
!        print *,'Dumping particles individually...'

        call cpu_time(stime)
        call outputH5BeamFilesID(time, sz_loc, iL, error)
        call cpu_time(ftime)
!        print '("Dumped particles separately. Took time = ",f6.3," secs on rank ",i5)'&
!          ,ftime-stime,tProcInfo_G%rank

      else 

!        print *,'Dumping particles to single file...'
        call cpu_time(stime)
        call outputH5BeamFilesSD(time, sz_loc, iL, error)
        call cpu_time(ftime)
!        print '("Dumped particles to one file. Took time = ",f6.3," seconds on rank ",i5)'&
!          ,ftime-stime,tProcInfo_G%rank

      end if

      if (error .ne. 0) goto 1000

      if (qONED_G) then

        numSpatialDims=1

!      print *,'2 component 1D field output not currently supported'
!        print *, "But trying anyway - Dumping all fields together"
        call cpu_time(stime)
        call outputH5Field1D2CompSD(time, sz_loc, iL, error, tlflen, fr_rfield,  ffs, ffe, 0, 1, .false.)
        call outputH5Field1D2CompSD(time, sz_loc, iL, error, tlflen, fr_ifield,  ffs, ffe, 1, 2, .false.)
        call outputH5Field1D2CompSD(time, sz_loc, iL, error, mainlen, ac_rfield, fz2, ez2, 0, 2, .true.)
        call outputH5Field1D2CompSD(time, sz_loc, iL, error, mainlen, ac_ifield, fz2, ez2, 1, 2, .true.)
        call outputH5Field1D2CompSD(time, sz_loc, iL, error, tlelen, bk_rfield,  ees, eee, 0, 2, .false.)
        call outputH5Field1D2CompSD(time, sz_loc, iL, error, tlelen, bk_ifield,  ees, eee, 1, 2, .false.)
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
          call outputH5Field3DID(time, sz_loc, iL, error, tlflen, 'aperp_front_real', fr_rfield,  ffs, ffe, .false.)
          call outputH5Field3DID(time, sz_loc, iL, error, tlflen, 'aperp_front_imag', fr_ifield,  ffs, ffe, .false.)
          call outputH5Field3DID(time, sz_loc, iL, error, mainlen, 'aperp_active_real', ac_rfield,  fz2, ez2, .true.)
          call outputH5Field3DID(time, sz_loc, iL, error, mainlen, 'aperp_active_imag', ac_ifield,  fz2, ez2, .true.)
          call outputH5Field3DID(time, sz_loc, iL, error, tlelen, 'aperp_back_real', bk_rfield,  ees, eee, .false.)
          call outputH5Field3DID(time, sz_loc, iL, error, tlelen, 'aperp_back_imag', bk_rfield,  ees, eee, .false.)
          call cpu_time(ftime)
!        print '("Dumped separate fields. Took time = ",f6.3," secs on rank ",i5)' &
!          ,ftime-stime,tprocinfo_g%rank

        else

!        print *, "Dumping all fields together"
          call cpu_time(stime)
          call outputH5Field3DSD(time, sz_loc, iL, error, tlflen, fr_rfield,  ffs, ffe, 0, 1, .false.)
          call outputH5Field3DSD(time, sz_loc, iL, error, tlflen, fr_ifield,  ffs, ffe, 1, 2, .false.)
          call outputH5Field3DSD(time, sz_loc, iL, error, mainlen, ac_rfield, fz2, ez2, 0, 2, .true.)
          call outputH5Field3DSD(time, sz_loc, iL, error, mainlen, ac_ifield, fz2, ez2, 1, 2, .true.)
          call outputH5Field3DSD(time, sz_loc, iL, error, tlelen, bk_rfield,  ees, eee, 0, 2, .false.)
          call outputH5Field3DSD(time, sz_loc, iL, error, tlelen, bk_ifield,  ees, eee, 1, 2, .false.)
          call cpu_time(ftime)
!        print '("Dumped fields together. Took time = ",f6.3," secs on rank ",i5)' &
!          ,ftime-stime,tprocinfo_g%rank
        end if
!      call outputH5Field3DSDattrs(time, error, tlflen, fr_rfield,  ffs, ffe, 0, .false.)
        if (error .ne. 0) goto 1000

      end if

! Zposition is probably to be recorded not at each timestep
! Todo: move into main
!      call outputH5Z(sZ, tArrayZ, iStep, qSep, zDFName, qOKL)
!      if (.not. qOKL) goto 1000

    end if



    if (qWriteInt) then

! These call requires all ranks to participate
      call gPowerP(power)
      call getCurr(dz2_I_G, Iarray)
      call getSliceTwiss(nslices,slicetrim,aveX,aveY,avePX,avePY, &
        sdX,sdY,sdpx,sdpy,eX,eY,ax,ay,bx,by,aveGamma,aveDgamma, &
        bun1,bun2,bun3,bun4,bun5,sq)

! For starters, write on rank 0 only


! but the write operation does not, as the data has been collected on rank0.

      if (tProcInfo_G%qRoot) then

        PowScale = lg_G * lc_G * c * e_0 * ((sgammaR_G * m_e * c**2.0_wp ) / &
                               (q_e * skappa_G * lg_G ))**2.0_wp

        avGam4Unsc = aveGamma
        where (avGam4Unsc == 0.0_wp) avGam4Unsc = 1.0_wp

        call CreateIntegrated1DFloat(time, sz_loc, iL,error,nslices)

        if (qOneD_G) then

          call addH5Field1DFloat(power, 'Intensity', "intFieldMeshSc", &
                                "z2, Intensity (Scaled)", time, sz_loc, iL, error)

          wrFArray = power * powScale / lg_G * lc_G

          call addH5Field1DFloat(wrFArray, 'IntensitySI', "intFieldMeshSI", &
                                "ct-z (m), Intensity (Wm-2)", time, sz_loc, iL, error)

          wrFArray = power * ata_G

          call addH5Field1DFloat(wrFArray, 'power', "intFieldMeshSc", &
                                "z2, Power (Scaled)", time, sz_loc, iL, error)

          wrFArray = power * powScale * ata_G

          call addH5Field1DFloat(wrFArray, 'powerSI', "intFieldMeshSI", &
                                "ct-z (m), Power (W)", time, sz_loc, iL, error)

        else

          call addH5Field1DFloat(power, 'power', "intFieldMeshSc", &
                                "z2, Power (Scaled)", time, sz_loc, iL, error)

          wrFArray = power * powScale

          call addH5Field1DFloat(wrFArray, 'powerSI', "intFieldMeshSI", &
                                "ct-z (m), Power (W)", time, sz_loc, iL, error)

        end if

        call addH5Field1DFloat(Iarray, 'beamCurrent',  "intPtclMeshSc", &
                               "z2, Current (A)", time, sz_loc, iL, error)

        call addH5Field1DFloat(Iarray, 'beamCurrentSI',  "intPtclMeshSI", &
                               "ct-z, Current (A)", time, sz_loc, iL, error)


        call addH5Field1DFloat(aveX, 'meanXbar', "intPtclMeshSc", &
                               "z2, xbar", time, sz_loc, iL, error)

        wrEArray = aveX * (DSQRT(lg_G*lc_G))

        call addH5Field1DFloat(wrEArray, 'meanXSI', "intPtclMeshSI", &
                               "ct-z (m), x (m)", time, sz_loc, iL, error)
        





        call addH5Field1DFloat(aveY, 'meanYbar', "intPtclMeshSc", &
                               "z2, ybar", time, sz_loc, iL, error)

        wrEArray = aveY * (DSQRT(lg_G*lc_G))

        call addH5Field1DFloat(wrEArray, 'meanYSI', "intPtclMeshSI", &
                               "ct-z (m), y (m)", time, sz_loc, iL, error)





        call addH5Field1DFloat(avepx, 'meanPXbar', "intPtclMeshSc", &
                               "z2, pxbar", time, sz_loc, iL, error)

        wrEArray = avepx * 2.0_wp * sRho_G * sKappa_G / avGam4Unsc


        call addH5Field1DFloat(wrEArray, 'mean_dxdzSI', "intPtclMeshSI", &
                               "ct-z (m), dxdz", time, sz_loc, iL, error)
                               



        call addH5Field1DFloat(avepy, 'meanPYbar', "intPtclMeshSc", &
                               "z2, pxbar", time, sz_loc, iL, error)

        wrEArray = -avepy * 2.0_wp * sRho_G * sKappa_G / avGam4Unsc


        call addH5Field1DFloat(wrEArray, 'mean_dydzSI', "intPtclMeshSI", &
                               "ct-z (m), dxdz", time, sz_loc, iL, error)






        call addH5Field1DFloat(aveGamma, 'meanGamma', "intPtclMeshSc", &
                               "z2, gamma / gamma0", time, sz_loc, iL, error)

        
        wrEArray = aveGamma * sGammaR_G * 0.511_wp
        
        call addH5Field1DFloat(wrEArray, 'meanEnergySI', "intPtclMeshSI", &
                               "ct-z (m), E (MeV)", time, sz_loc, iL, error)




        call addH5Field1DFloat(aveDGamma, 'meanSigmaGamma', "intPtclMeshSc", &
                               "z2, sigma_gamma / gamma0", time, sz_loc, iL, error)

        call addH5Field1DFloat(aveDGamma, 'meanSigmaGammaSI', "intPtclMeshSI", &
                               "ct-z (m), sigma_gamma / gamma0", time, sz_loc, iL, error)





        call addH5Field1DFloat(sdx, 'sigmaXbar', "intPtclMeshSc", &
                               "z2, sigma_xbar", time, sz_loc, iL, error)

        wrEArray = sdx * (DSQRT(lg_G*lc_G))

        call addH5Field1DFloat(wrEArray, 'sigmaXSI', "intPtclMeshSI", &
                               "ct-z (m), sigma_x (m)", time, sz_loc, iL, error)


        call addH5Field1DFloat(sdy, 'sigmaYbar', "intPtclMeshSc", &
                               "z2, sigma_ybar", time, sz_loc, iL, error)

        wrEArray = sdy * (DSQRT(lg_G*lc_G))

        call addH5Field1DFloat(wrEArray, 'sigmaYSI', "intPtclMeshSI", &
                               "ct-z (m), sigma_y (m)", time, sz_loc, iL, error)
                               


        call addH5Field1DFloat(sdpx, 'sigmaPxbar', "intPtclMeshSc", &
                               "z2, sigma_pxbar", time, sz_loc, iL, error)

        wrEArray = sdpx * 2.0_wp * sRho_G * sKappa_G / avGam4Unsc

        call addH5Field1DFloat(wrEArray, 'sigma_dxdzSI', "intPtclMeshSI", &
                               "ct-z (m), sigma_dxdz", time, sz_loc, iL, error)




        call addH5Field1DFloat(sdpy, 'sigmaPybar', "intPtclMeshSc", &
                               "z2, sigma_pybar", time, sz_loc, iL, error)

        wrEArray = sdpy * 2.0_wp * sRho_G * sKappa_G / avGam4Unsc

        call addH5Field1DFloat(wrEArray, 'sigma_dydzSI', "intPtclMeshSI", &
                               "ct-z (m), sigma_dydz", time, sz_loc, iL, error)



        wrEArray = 2.0_wp * sKappa_G / sqrt(sEta_G) /  avGam4Unsc * ex
        
        call addH5Field1DFloat(wrEArray, 'emittanceXbar', "intPtclMeshSc", &
                               "z2, scaled x emittance", time, sz_loc, iL, error)

        wrEArray = wrEArray * sRho_G * lc_G

        call addH5Field1DFloat(wrEArray, 'emittanceXSI', "intPtclMeshSI", &
                               "ct-z (m), emittance_x (unnormalised)", time, sz_loc, iL, error)




        wrEArray = 2.0_wp * sKappa_G / sqrt(sEta_G) /  avGam4Unsc * ey
        
        call addH5Field1DFloat(wrEArray, 'emittanceYbar', "intPtclMeshSc", &
                               "z2, scaled y emittance", time, sz_loc, iL, error)

        wrEArray = wrEArray * sRho_G * lc_G

        call addH5Field1DFloat(wrEArray, 'emittanceYSI', "intPtclMeshSI", &
                               "ct-z (m), emittance_y (unnormalised)", time, sz_loc, iL, error)
                               
                               


        call addH5Field1DFloat(bun1, 'bunchingFundamental', "intPtclMeshSc", &
                               "z2, bunching", time, sz_loc, iL, error)

        call addH5Field1DFloat(bun1, 'bunchingFundamentalSI', "intPtclMeshSI", &
                               "ct-z (m), bunching", time, sz_loc, iL, error)


        call addH5Field1DFloat(bun2, 'bunching2ndHarmonic', "intPtclMeshSc", &
                               "z2, 2nd harmonic bunching", time, sz_loc, iL, error)

        call addH5Field1DFloat(bun2, 'bunching2ndHarmonicSI', "intPtclMeshSI", &
                               "ct-z (m), 2nd harmonic bunching", time, sz_loc, iL, error)



        call addH5Field1DFloat(bun3, 'bunching3ndHarmonic', "intPtclMeshSc", &
                               "z2, 3rd harmonic bunching", time, sz_loc, iL, error)

        call addH5Field1DFloat(bun3, 'bunching3ndHarmonicSI', "intPtclMeshSI", &
                               "ct-z (m), 3rd harmonic bunching", time, sz_loc, iL, error)



        call addH5Field1DFloat(bun4, 'bunching4thHarmonic', "intPtclMeshSc", &
                               "z2, 4th harmonic bunching", time, sz_loc, iL, error)

        call addH5Field1DFloat(bun4, 'bunching4thHarmonicSI', "intPtclMeshSI", &
                               "ct-z (m), 4th harmonic bunching", time, sz_loc, iL, error)



        call addH5Field1DFloat(bun5, 'bunching5thHarmonic', "intPtclMeshSc", &
                               "z2, 5th harmonic bunching", time, sz_loc, iL, error)

        call addH5Field1DFloat(bun5, 'bunching5thHarmonicSI', "intPtclMeshSI", &
                               "ct-z (m), 5th harmonic bunching", time, sz_loc, iL, error)
                               

        call addH5Field1DFloat(sq, 'Slice Charge', "intPtclMeshSc", &
                               "z2, Charge", time, sz_loc, iL, error)

        call addH5Field1DFloat(sq, 'Slice Charge SI', "intPtclMeshSI", &
                               "ct-z (m), Charge", time, sz_loc, iL, error)


        if (error .ne. 0) goto 1000
! Todo not yet implemented
!     call outputH5SliceEmittance
!       NOT YET IMPLEMENTED

      end if  

    end if

!  Set error flag and exit         

    error = 0            

    goto 2000     

! Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call Error_log('Error in hdfPuffin:wr_h5',&
          tErrorLog_G)
    print*,'Error in hdfPuffin:wr_h5'
2000 continue

  end subroutine wr_h5












	
end module hdf5_puff
