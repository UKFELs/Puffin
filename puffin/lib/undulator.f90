! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

module undulator



!  use paratype
!  use ParallelSetUp
!  use globals
!  use TransformInfoType
!  use stiffness

! use FFTW_Constants

use pdiff
!use sddsPuffin
use lattice
use RK4int
!use dumpFiles
use write_adapter
use ParaField
use InitDataType
use GlobalTypes, only: tIntegrationState, tLatticeElements, tFieldMesh, tFELPhysics
use AdapterGlobals, only: PopulateIntegrationStateFromGlobals, UpdateGlobalsFromIntegrationState, &
                          PopulateLatticeElementsFromGlobals, UpdateGlobalsFromLatticeElements, &
                          PopulateFieldMeshFromGlobals, UpdateGlobalsFromFieldMesh, &
                          PopulateFELPhysicsFromGlobals, UpdateGlobalsFromFELPhysics


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
    real(kind=wp) :: delz_D, nextDiff, szl, locTimeSt
    logical :: qFirst, qLast, qDiffrctd
    logical :: qWPF
    logical :: qWIF
    logical :: qOKL
    integer(kind=ip) :: drstart, stepsLeft
    real(kind=wp) :: dzdS, dzdF, dzd
    logical :: qDWrDone
    integer error
    type(tIntegrationState) :: integration
    type(tLatticeElements) :: latt
    type(tFieldMesh) :: mesh
    type(tFELPhysics) :: physics

  call Get_time(locTimeSt)

!     Need to match into undulator

  call initUndulator(iUnd_cr, sZ, szl)

! Populate integration state from globals set by initUndulator

  call PopulateIntegrationStateFromGlobals(integration)

! Populate lattice state from globals

  call PopulateLatticeElementsFromGlobals(latt)

! Populate field mesh state from globals

  call PopulateFieldMeshFromGlobals(mesh)

! Populate FEL physics parameters from globals

  call PopulateFELPhysicsFromGlobals(physics)

  if (qResume_G) then

    integration%start_step = tInitData_G%iStep
    latt%cumulative_steps = tInitData_G%iCSteps
    iCSteps = latt%cumulative_steps
    sz = tInitData_G%zbarTotal
    szl = tInitData_G%zbarlocal
    sZi_G = tInitData_G%Zbarinter
    mesh%highpass_filter_gr = tInitData_G%igwr
    igwr = mesh%highpass_filter_gr

  else

    integration%start_step = 0_ip  ! ...TEMP...

    if (.not. physics%model_undulator_ends) call matchIn(szl)

  end if

  qDiffrctd = .false.

  if (integration%start_step==1_IP) then

    integration%count = 0_IP

  else

    integration%count = mod(integration%start_step-1_IP,iWriteNthSteps)

  end if


  call getLocalFieldIndices(integration%redistribution_length*2.0_wp)


  iSteps4Diff = nint(integration%diffraction_step_size / integration%step_size)

!     #####
!     Begin integration through undulator

! if resuming, work out where we are on the diffraction part...
! will need to do first half step since writes are done on COMPLETED
! split-steps.

if (qresume_G) then
  if (qDiffraction_G) then

    drstart = integration%start_step - mod(integration%start_step,isteps4diff)

    if (drstart == integration%total_steps) then

      dzdS = 0.0_wp

    else

      if ((drstart + isteps4diff) <= integration%total_steps) then

        dzdS = real(isteps4diff,kind=wp) * integration%step_size / 2.0_wp

      else if ((drstart + isteps4diff) > integration%total_steps) then

        stepsLeft = integration%total_steps - drstart
        dzdS = real(stepsLeft,kind=wp)*integration%step_size / 2.0_wp

      end if

    end if

! If this write was done on a diffraction stage,
! need to diffract

    if (dzdS > 0.0_wp) then

      if (mod(integration%start_step,isteps4diff) == 0_ip) then

        call diffractIM(dzdS, qDiffrctd, qOKL)

      end if

    end if

  end if

else  ! if not resuming, just do first half diffraction step

  if (qDiffraction_G) then

    dzdS = real(isteps4diff, kind=wp) * integration%step_size / 2.0_wp

    call diffractIM(dzdS, qDiffrctd, qOKL)

!    nextDiff = nextDiff + diffStep

  end if

end if

!   First step of split-step method:- field diffraction only

  call mpi_barrier(tProcInfo_G%comm, error)

  call allact_rk4_arrs()


  igoes = 0_ip

  qDWrDone = .false.


  integration%current_step = integration%start_step
  iStep = integration%current_step

  do


    integration%current_step = integration%current_step + 1_ip
    iStep = integration%current_step

    if (integration%current_step > integration%total_steps) exit

    latt%cumulative_steps = latt%cumulative_steps + 1_ip
    iCsteps = latt%cumulative_steps

!   Second half of split step method: electron propagation
!                    and field driving.

    if (qElectronsEvolve_G .OR. qFieldEvolve_G &
             .OR. qElectronFieldCoupling_G) then

      igoes = 1_ip
      do
        call rk4par(sZl,integration%step_size,qDiffrctd)
        if (igoes>3_ip) exit
        if (.not. qPArrOK_G) then
          call deallact_rk4_arrs()
          if (.not. qInnerXYOK_G) then
            call getInNode()
          end if
          call getLocalFieldIndices(integration%redistribution_length)
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

    sZl = sZl + integration%step_size
    sZ = physics%z_taper_start + szl
    sZi_G = sZi_G + integration%step_size


!   diffract field to complete diffraction step

    if (qDiffraction_G) then

      if ((mod(integration%current_step,isteps4diff) == 0_ip) .or. &
          (integration%current_step == integration%total_steps))  then

!        call deallact_rk4_arrs()

        call inner2Outer(ac_rfield_in, ac_ifield_in)

        dzdF = dzdS  ! Finishing last diffraction step
                     ! - must be indentical size

! Start of next diffraction step is this ->
! dzdS = either 0, steps4diff*dz / 2, or stepsLeft*dz / 2

        if (integration%current_step == integration%total_steps) then

          dzdS = 0.0_wp

        else

          if ((integration%current_step + isteps4diff) <= integration%total_steps) then

            dzdS = real(isteps4diff,kind=wp)*integration%step_size / 2

          else if ((integration%current_step + isteps4diff) > integration%total_steps) then

            stepsLeft = integration%total_steps - integration%current_step
            dzdS = real(stepsLeft,kind=wp)*integration%step_size / 2

          end if

        end if

        if (.not. qWriteq(integration%current_step, latt%cumulative_steps, iWriteNthSteps, iIntWriteNthSteps, &
                                                         integration%total_steps)) then

        ! if not writing then we can do the last half of the
        ! last diffraction step and the first half of the next
        ! in the same step...

          dzd = dzdF + dzdS

          call diffractIM(dzd, qDiffrctd, qOKL)
          call outer2Inner(ac_rfield_in, ac_ifield_in)
        else

        ! If writing in this step, then we need to first
        ! finish the last diffraction step, and THEN write,
        ! and then start the next diffraction step.

          call diffractIM(dzdF, qDiffrctd, qOKL)  ! Finish diffraction step
          call writeIM(sZ, sZl, &
                       integration%current_step, latt%cumulative_steps, iM, iWriteNthSteps, &
                       iIntWriteNthSteps, integration%total_steps, qOKL)   ! Write data
          if (dzdS > 0.0_wp) call diffractIM(dzdS, qDiffrctd, qOKL)  ! Start new diffraction step
          call outer2Inner(ac_rfield_in, ac_ifield_in)
          qDWrDone = .true.

        end if
      end if
    end if  ! end diffraction step

!                   Write result to file

  integration%count = integration%count + 1_IP

    if (qWriteq(integration%current_step, latt%cumulative_steps, iWriteNthSteps, iIntWriteNthSteps, &
                integration%total_steps)) then

      if (.not. qDWrDone) then

        ! if not already written in diffraction step

        call inner2Outer(ac_rfield_in, ac_ifield_in)

        call writeIM(sZ, sZl, &
                     integration%current_step, latt%cumulative_steps, iM, iWriteNthSteps, &
                     iIntWriteNthSteps, integration%total_steps, qOKL)

      else

      	qDWrDone = .false.  ! reset

      end if

    end if


  call Get_time(end_time)

  if ((tProcInfo_G%QROOT ) .and. (ioutInfo_G > 1)) then
    print*,' finished step ',latt%cumulative_steps, integration%current_step, end_time-start_time
    WRITE(137,*) ' finished step ',latt%cumulative_steps, integration%current_step, end_time-start_time
  end if



  if (mod(latt%cumulative_steps, integration%redistribution_step) == 0) then

    call deallact_rk4_arrs()
    call getLocalFieldIndices(integration%redistribution_length)
    call allact_rk4_arrs()

  end if

  end do

  call deallact_rk4_arrs()


  if (igoes>3_ip) then

    if (tProcInfo_G%qRoot) print*, 'Tried rearranging 3 times...'
    if (tProcInfo_G%qRoot) print*, '...didnt work, so stopping...'
    call mpi_finalize(error)
    stop

  end if

  if (.not. physics%model_undulator_ends) call matchOut(sZ)

  call correctTrans()  ! correct transverse motion at undulator exit

  iUnd_cr = iUnd_cr + 1_ip
  qResume_G = .false.

  if ((tProcInfo_G%QROOT ) .and. (ioutInfo_G > 0)) then
    print*,' Finished undulator module in ', end_time-locTimeSt, 'seconds'
  end if

! Re-sync taper-mutated fields from globals before writing back physics state.
! wiggler_taper::getAlpha modifies n2col and n2col0 during the integration loop,
! so we must pull the current values back into physics before calling the update.
  physics%n2col = n2col
  physics%n2col_initial = n2col0

  call UpdateGlobalsFromFieldMesh(mesh)
  call UpdateGlobalsFromLatticeElements(latt)
  call UpdateGlobalsFromIntegrationState(integration)
  call UpdateGlobalsFromFELPhysics(physics)

end subroutine UndSection


end module undulator
