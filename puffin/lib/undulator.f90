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
use GlobalTypes, only: tIntegrationState, tLatticeElements, tFieldMesh, tFELFrame, &
                       tUndulator, tSimulationFlags, tOutputConfig
use AdapterGlobals, only: PopulateIntegrationStateFromGlobals, UpdateGlobalsFromIntegrationState, &
                          PopulateUndulatorFromGlobals, UpdateGlobalsFromUndulator


implicit none


contains


  subroutine UndSection(iM, sZ, mesh, frame, flags, output, latt)

! -----------------------------------------------------------------------
! Remaining globals used in UndSection that CANNOT be removed yet.
! These are read or written by deep callees via `use Globals` / `use lattice`,
! so removing them requires threading types through callee signatures (Phase 9).
!
! Variable        | Why it remains global
! ----------------|------------------------------------------------------
! iStep           | hdf5PuffID.f90 reads it for output filenames
! iCsteps         | hdf5PuffLow.f90 reads it for dataset writes
! igwr            | hdf5PuffLow.f90, hdf5_puff.f90, hdf5_puff_coll.f90
! sZi_G           | hdf5PuffLow.f90 reads it for z-coordinate output
! iUnd_cr         | hdf5PuffLow.f90 reads it for undulator section index
! end_time        | diffraction.f90 reads it; start_time set by puffin_module
! start_time      | Set externally by puffin_module.f90 before entry
! n2col / n2col0  | Modified by wiggler_taper callees mid-loop
! qPArrOK_G       | Set by rk4par callees mid-loop
! qInnerXYOK_G    | Set by rk4par callees mid-loop
!
! Infrastructure globals (not candidates for migration):
! tProcInfo_G     | MPI communicator/rank info, used in 67+ locations
! ac_rfield_in    | Module-level RK4 work arrays, managed by RK4int
! ac_ifield_in    | Module-level RK4 work arrays, managed by RK4int
! -----------------------------------------------------------------------

    implicit none

! iM   - Which lattice module is this?
! sZ   - zbar position through the machine
! mesh, frame, flags, output, latt - simulation-lifetime types owned by puffin_main

    integer(kind=ip), intent(in) :: iM
    real(kind=wp), intent(inout) :: sZ
    type(tFieldMesh),       intent(inout) :: mesh
    type(tFELFrame),        intent(in)    :: frame
    type(tSimulationFlags), intent(inout) :: flags
    type(tOutputConfig),    intent(in)    :: output
    type(tLatticeElements), intent(inout) :: latt


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
    type(tUndulator) :: und
    logical :: qResuming
    type(cInitData) :: init_data

  call Get_time(locTimeSt)

!     Need to match into undulator

  call initUndulator(iUnd_cr, sZ, szl)

! Populate integration state from globals set by initUndulator

  call PopulateIntegrationStateFromGlobals(integration)

! Populate per-element undulator parameters from globals set by initUndulator

  call PopulateUndulatorFromGlobals(und)

  qResuming = qResume_G
  init_data = tInitData_G

  if (qResuming) then

    integration%start_step = init_data%iStep
    latt%cumulative_steps = init_data%iCSteps
    iCSteps = latt%cumulative_steps
    sz = init_data%zbarTotal
    szl = init_data%zbarlocal
    sZi_G = init_data%Zbarinter
    mesh%highpass_filter_gr = init_data%igwr
    igwr = mesh%highpass_filter_gr

  else

    integration%start_step = 0_ip  ! ...TEMP...

    if (.not. und%model_undulator_ends) call matchIn(szl)

  end if

  qDiffrctd = .false.

  if (integration%start_step==1_IP) then

    integration%count = 0_IP

  else

    integration%count = mod(integration%start_step-1_IP,output%write_nth_steps)

  end if


  call getLocalFieldIndices(integration%redistribution_length*2.0_wp)


  iSteps4Diff = nint(integration%diffraction_step_size / integration%step_size)

!     #####
!     Begin integration through undulator

! if resuming, work out where we are on the diffraction part...
! will need to do first half step since writes are done on COMPLETED
! split-steps.

if (qResuming) then
  if (flags%diffraction) then

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

  if (flags%diffraction) then

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

    if (flags%electrons_evolve .OR. flags%field_evolve &
             .OR. flags%electron_field_coupling) then

      igoes = 1_ip
      do
        call rk4par(sZl, integration%step_size, qDiffrctd, und, frame, flags)
        if (igoes>3_ip) exit
        if (.not. flags%parallel_arrays_ok) then
          call deallact_rk4_arrs()
          if (.not. flags%inner_xy_ok) then
            call getInNode()
            qInnerXYOK_G = .true.
            flags%inner_xy_ok = .true.
          end if
          call getLocalFieldIndices(integration%redistribution_length)
          qPArrOK_G = .true.
          flags%parallel_arrays_ok = .true.
          call allact_rk4_arrs()
          flags%inner_xy_ok = .true.
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
    sZ = und%z_taper_start + szl
    sZi_G = sZi_G + integration%step_size


!   diffract field to complete diffraction step

    if (flags%diffraction) then

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

        if (.not. qWriteq(integration%current_step, latt%cumulative_steps, output%write_nth_steps, output%write_nth_steps_intermediate, &
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
                       integration%current_step, latt%cumulative_steps, iM, output%write_nth_steps, &
                       output%write_nth_steps_intermediate, integration%total_steps, qOKL)   ! Write data
          if (dzdS > 0.0_wp) call diffractIM(dzdS, qDiffrctd, qOKL)  ! Start new diffraction step
          call outer2Inner(ac_rfield_in, ac_ifield_in)
          qDWrDone = .true.

        end if
      end if
    end if  ! end diffraction step

!                   Write result to file

  integration%count = integration%count + 1_IP

    if (qWriteq(integration%current_step, latt%cumulative_steps, output%write_nth_steps, output%write_nth_steps_intermediate, &
                integration%total_steps)) then

      if (.not. qDWrDone) then

        ! if not already written in diffraction step

        call inner2Outer(ac_rfield_in, ac_ifield_in)

        call writeIM(sZ, sZl, &
                     integration%current_step, latt%cumulative_steps, iM, output%write_nth_steps, &
                     output%write_nth_steps_intermediate, integration%total_steps, qOKL)

      else

      	qDWrDone = .false.  ! reset

      end if

    end if


  call Get_time(end_time)

  if ((tProcInfo_G%QROOT ) .and. (output%output_info_level > 1)) then
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

  if (.not. und%model_undulator_ends) call matchOut(sZ)

  call correctTrans()  ! correct transverse motion at undulator exit

  iUnd_cr = iUnd_cr + 1_ip
  qResume_G = .false.

  if ((tProcInfo_G%QROOT ) .and. (output%output_info_level > 0)) then
    print*,' Finished undulator module in ', end_time-locTimeSt, 'seconds'
  end if

  call UpdateGlobalsFromIntegrationState(integration)
  call UpdateGlobalsFromUndulator(und)

end subroutine UndSection


end module undulator
