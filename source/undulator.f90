!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

module undulator



!  use paratype
!  use ParallelSetUp
!  use globals
!  use TransformInfoType
!  use stiffness

use FFTW_Constants
use pdiff
use sddsPuffin
use lattice
use RK4int
use dumpFiles
use dummyf
use ParaField


implicit none


contains


  subroutine UndSection(iM, sZ)


    implicit none

! nPeriods        -      Number of periods in the module
! nStepsPerPeriod -      Number of steps per undulator period
!                        to be used in the integration
! iM          -      Which module is this?
! sV              -      6D electron data, arranged according
!                 -      to ArrayFunctions.f90
! sA              -      Field array
! sZ              -      zbar

    integer(kind=ip), intent(in) :: iM
    real(kind=wp), intent(inout) :: sZ ! , sA(:)


! Local args

    real(kind=wp), allocatable  :: sAr(:), Ar_local(:)
    integer(kind=ip) :: iPer, iS ! Loop index - period counter
    integer(kind=ip) :: nW
    integer(kind=ip) :: iSteps4Diff, igoes
    real(kind=wp) :: delz_D, nextDiff, szl
    logical :: qFirst, qLast, qDiffrctd
    logical :: qWPF
    logical :: qWIF
    logical :: qOKL
    integer error

!    stepsize = delmz(iM)
    
!    iUndType_G = iDetail(iM)




!    nW = nPeriods(iM)
!    nSPP = nStepsPerPeriod(iM)

!     Need to match into undulator

  call mpi_barrier(tProcInfo_G%comm, error)
  if (tProcInfo_G%qRoot) print*, 'init undulator...'

  call initUndulator(iUnd_cr, sZ, szl)

  call mpi_barrier(tProcInfo_G%comm, error)
  if (tProcInfo_G%qRoot) print*, 'init beam...'

  if (.not. qUndEnds_G) call matchIn(sZ)


  qDiffrctd = .false.

  start_step = 1_ip  ! ...TEMP...

  if (start_step==1_IP) then

    iCount = 0_IP

  else

    iCount = mod(start_step-1_IP,iWriteNthSteps)

  end if


  call mpi_barrier(tProcInfo_G%comm, error)
  if (tProcInfo_G%qRoot) print*, 'redisting field...'

  call getLocalFieldIndices(sRedistLen_G*2.0_wp)


  if (start_step == 1) then
    sStep = diffStep*0.5_WP ! Integration step size for first diffraction step
    nextDiff = 0.0_WP
  else 
    sStep = diffStep
    nextDiff = ceiling(sZ/diffStep) * diffStep
  end if




!     #####
!     Begin integration through undulator



!   First step of split-step method:- field diffraction only


  if (qDiffraction_G) then
  
    call diffractIM(diffStep*0.5_WP, &
                    qDiffrctd, qOKL)
  
    nextDiff = nextDiff + diffStep
  
  end if


  call mpi_barrier(tProcInfo_G%comm, error)
  if (tProcInfo_G%qRoot) print*, 'alloc arrs...'

  call allact_rk4_arrs()




  istep = start_step

  do  

!   Second half of split step method: electron propagation
!                    and field driving.

    if (qElectronsEvolve_G .OR. qFieldEvolve_G &
             .OR. qElectronFieldCoupling_G) then

      igoes = 1_ip  
      do 
        call rk4par(sZl,sStepSize,qDiffrctd)
        if (igoes>3_ip) exit
        if (.not. qPArrOK_G) then
          if (tProcInfo_G%qRoot) print*, 'Layout not working'
          if (tProcInfo_G%qRoot) print*, 'Rearranging...'
          call deallact_rk4_arrs()
          if (.not. qInnerXYOK_G) then
            call getInNode()
          end if
          call getLocalFieldIndices(sRedistLen_G)
          call allact_rk4_arrs()
        else 
          exit
        end if
        igoes = igoes + 1_ip
      end do
  
    end if 


    if (igoes>3) exit

!                  Increment z position  
!       (we now have solution at zbar + sStepsize) 

    sZl = sZl + sStepSize
    sZ = sZ0 + szl



!   diffract field to complete diffraction step

    if (qDiffraction_G) then
  
      if ((sZ>(nextDiff-sStepsize/100.0_WP)) .or. (iStep == nSteps))  then

!        call deallact_rk4_arrs()

        call inner2Outer(ac_rfield_in, ac_ifield_in)
  
        if ((iStep == nSteps) .or. &
             qWriteq(iStep, iCsteps, iWriteNthSteps, iIntWriteNthSteps, nSteps) ) then
    
          call diffractIM(diffStep * 0.5_wp, &
                          qDiffrctd, qOKL)
  
        else
    
          call diffractIM(diffStep, &
                          qDiffrctd, qOKL)
    
        end if

        call outer2Inner(ac_rfield_in, ac_ifield_in)
  
        nextDiff = nextDiff + diffStep
    
      end if
  
    end if
!                   Write result to file
 
  iCount = iCount + 1_IP
  
  if ( qWriteq(iStep, iCsteps, iWriteNthSteps, iIntWriteNthSteps, nSteps) ) then

    call inner2Outer(ac_rfield_in, ac_ifield_in)

    call writeIM(sZ, &
                 zDataFileName, iStep, iCsteps, iWriteNthSteps, &
                 iIntWriteNthSteps, nSteps, qOKL)


     if (qDiffraction_G) then
 
 !             If field diffraction occurred this step, need to complete it....  
 !             ...the diffraction only diffracts a half step if data is going
 !             to be written (to match up the split-step data)
 
        if (qDiffrctd) call diffractIM(diffStep * 0.5_wp, &
                         qDiffrctd, qOKL)
   
     end if

    call outer2Inner(ac_rfield_in, ac_ifield_in)
   
  end if


  call Get_time(end_time)
    
  if (tProcInfo_G%QROOT ) then
    print*,' finished step ',iCsteps, end_time-start_time
    WRITE(137,*) ' finished step ',iCsteps, end_time-start_time
  end if


  
  if (mod(iCsteps, iRedistStp_G) == 0) then

    call deallact_rk4_arrs()
    call getLocalFieldIndices(sRedistLen_G)
    call allact_rk4_arrs()
    
  end if


  iCsteps = iCsteps + 1_ip
  iStep = iStep + 1_ip

  if (iStep > nSteps) exit


!    if (modCount > ModNum) EXIT


  end do   ! End of integration loop

  call deallact_rk4_arrs()


  if (igoes>3_ip) then

    if (tProcInfo_G%qRoot) print*, 'Tried rearranging 3 times...'
    if (tProcInfo_G%qRoot) print*, '...didnt work, so stopping...'
    call mpi_finalize(error) 
    stop

  end if

  if (.not. qUndEnds_G) call matchOut(sZ)

  call correctTrans()  ! correct transverse motion at undulator exit

  iUnd_cr = iUnd_cr + 1_ip

end subroutine UndSection









end module undulator
