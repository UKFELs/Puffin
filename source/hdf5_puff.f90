!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013-2016, University of Strathclyde              **!
!** Written by Jonathan Smith (Tech-X UK Ltd)                   **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

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
    real(kind=wp), dimension(npts_I_G) :: Iarray !< current data (called here)
    type(cArraySegment), intent(inout) :: tArrayA(:), tArrayE(:), tArrayZ
    integer(kind=ip), intent(in)  :: nslices, iL
    real(kind=wp), dimension(nslices) :: aveX,aveY,avePX,avePY,aveGamma,aveDgamma
    real(kind=wp), dimension(nslices) :: sdX, sdY, sdpx, sdpy, eX, ey, aX, aY, bX, bY
    real(kind=wp), dimension(nslices) :: bun1,bun2,bun3,bun4,bun5,sq

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

        call CreateIntegrated1DFloat(time,error,nslices)
        call addH5Field1DFloat(power, 'power', PowScale, "intFieldMesh", time, sz_loc, iL, error)
        call addH5Field1DFloat(Iarray, 'beamCurrent', 1._wp, "intPtclMesh", time, sz_loc, iL, error)
        call addH5Field1DFloat(aveX, 'meanPosX', (DSQRT(lg_G*lc_G)), "intPtclMesh", time, sz_loc, iL, error)
        call addH5Field1DFloat(aveY, 'meanPosY', (DSQRT(lg_G*lc_G)), "intPtclMesh", time, sz_loc, iL, error)
        call addH5Field1DFloat(avepy, 'meanMomentumX', 1._wp, "intPtclMesh", time, sz_loc, iL, error)
        call addH5Field1DFloat(avepy, 'meanMomentumY', 1._wp, "intPtclMesh", time, sz_loc, iL, error)
        call addH5Field1DFloat(aveGamma, 'meanGamma', 1._wp, "intPtclMesh", time, sz_loc, iL, error)
        call addH5Field1DFloat(aveDGamma, 'meanDeltaGamma', 1._wp, "intPtclMesh", time, sz_loc, iL, error)
        call addH5Field1DFloat(sdx, 'sigmaX', (DSQRT(lg_G*lc_G)), "intPtclMesh", time, sz_loc, iL, error)
        call addH5Field1DFloat(sdy, 'sigmaY', (DSQRT(lg_G*lc_G)), "intPtclMesh", time, sz_loc, iL, error)
        call addH5Field1DFloat(sdpx, 'sigmapX', 1._wp, "intPtclMesh", time, sz_loc, iL, error)
        call addH5Field1DFloat(sdpy, 'sigmapY', 1._wp, "intPtclMesh", time, sz_loc, iL, error)
        call addH5Field1DFloat(ex, 'emittanceX', 1._wp, "intPtclMesh", time, sz_loc, iL, error)
        call addH5Field1DFloat(ey, 'emittanceY', 1._wp, "intPtclMesh", time, sz_loc, iL, error)
        call addH5Field1DFloat(bun1, 'bunchingFundamental', 1._wp, "intPtclMesh", time, sz_loc, iL, error)
        call addH5Field1DFloat(bun2, 'bunching2ndHarmonic', 1._wp, "intPtclMesh", time, sz_loc, iL, error)
        call addH5Field1DFloat(bun3, 'bunching3rdHarmonic', 1._wp, "intPtclMesh", time, sz_loc, iL, error)
        call addH5Field1DFloat(bun4, 'bunching4thHarmonic', 1._wp, "intPtclMesh", time, sz_loc, iL, error)
        call addH5Field1DFloat(bun5, 'bunching5thHarmonic', 1._wp, "intPtclMesh", time, sz_loc, iL, error)
        call addH5Field1DFloat(sq, 'sliceCharge', 1._wp, "intPtclMesh", time, sz_loc, iL, error)
          

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
