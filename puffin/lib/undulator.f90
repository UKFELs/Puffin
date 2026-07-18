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
use GlobalTypes, only: tSimulationContext
use AdapterGlobals, only: PopulateIntegrationStateFromGlobals, UpdateGlobalsFromIntegrationState, &
                          PopulateUndulatorFromGlobals, UpdateGlobalsFromUndulator


implicit none


contains


  subroutine UndSection(iM, sZ, ctx)

! -----------------------------------------------------------------------
! Remaining globals used in UndSection that CANNOT be removed yet.
! These are read or written by deep callees via `use Globals` / `use lattice`,
! so removing them requires threading types through callee signatures.
!
! Variable        | Why it remains global
! ----------------|------------------------------------------------------
! n2col / n2col0  | Modified by wiggler_taper callees mid-loop
!
! Infrastructure globals (not candidates for migration):
! tProcInfo_G     | MPI communicator/rank info, used in 67+ locations
! ac_rfield_in    | Module-level RK4 work arrays, managed by RK4int
! ac_ifield_in    | Module-level RK4 work arrays, managed by RK4int
! -----------------------------------------------------------------------

    implicit none

! iM   - Which lattice module is this?
! sZ   - zbar position through the machine
! ctx  - simulation context owned by puffin_main

    integer(kind=ip), intent(in) :: iM
    real(kind=wp), intent(inout) :: sZ
    type(tSimulationContext), intent(inout) :: ctx


! Local args

    integer(kind=ip) :: iSteps4Diff, igoes
    real(kind=wp) :: szl, locTimeSt, locEndTime
    logical :: qDiffrctd
    logical :: qOKL
    integer(kind=ip) :: drstart, stepsLeft
    real(kind=wp) :: dzdS, dzdF, dzd
    logical :: qDWrDone
    integer :: error
    logical :: qResuming

  call Get_time(locTimeSt)

!     Need to match into undulator

  call initUndulator(ctx%lattice%current_und_index, sZ, szl, ctx%frame, ctx%und)

! Populate integration state from globals set by initUndulator

  call PopulateIntegrationStateFromGlobals(ctx%integration)

! Populate per-element undulator parameters from globals set by initUndulator

  call PopulateUndulatorFromGlobals(ctx%und)

  qResuming = qResume_G

  if (qResuming) then

    ctx%integration%start_step = ctx%init_data%iStep
    ctx%lattice%cumulative_steps = ctx%init_data%iCSteps
    sz = ctx%init_data%zbarTotal
    szl = ctx%init_data%zbarlocal
    ctx%integration%z_inter = ctx%init_data%Zbarinter
    ctx%mesh%highpass_filter_gr = ctx%init_data%igwr

  else

    ctx%integration%start_step = 0_ip  ! ...TEMP...

    if (.not. ctx%und%model_undulator_ends) call matchIn(szl, ctx%frame, ctx%und%n2col)

  end if

  qDiffrctd = .false.

  if (ctx%integration%start_step==1_IP) then

    ctx%integration%count = 0_IP

  else

    ctx%integration%count = mod(ctx%integration%start_step-1_IP, &
                                ctx%output%write_nth_steps)

  end if


  call getLocalFieldIndices(ctx%integration%redistribution_length*2.0_wp, ctx%flags, ctx%frame)


  iSteps4Diff = nint(ctx%integration%diffraction_step_size / ctx%integration%step_size)

!     #####
!     Begin integration through undulator

! if resuming, work out where we are on the diffraction part...
! will need to do first half step since writes are done on COMPLETED
! split-steps.

if (qResuming) then
  if (ctx%flags%diffraction) then

    drstart = ctx%integration%start_step - mod(ctx%integration%start_step,isteps4diff)

    if (drstart == ctx%integration%total_steps) then

      dzdS = 0.0_wp

    else

      if ((drstart + isteps4diff) <= ctx%integration%total_steps) then

        dzdS = real(isteps4diff,kind=wp) * ctx%integration%step_size / 2.0_wp

      else if ((drstart + isteps4diff) > ctx%integration%total_steps) then

        stepsLeft = ctx%integration%total_steps - drstart
        dzdS = real(stepsLeft,kind=wp)*ctx%integration%step_size / 2.0_wp

      end if

    end if

! If this write was done on a diffraction stage,
! need to diffract

    if (dzdS > 0.0_wp) then

      if (mod(ctx%integration%start_step,isteps4diff) == 0_ip) then

        call diffractIM(dzdS, qDiffrctd, qOKL, ctx)

      end if

    end if

  end if

else  ! if not resuming, just do first half diffraction step

  if (ctx%flags%diffraction) then

    dzdS = real(isteps4diff, kind=wp) * ctx%integration%step_size / 2.0_wp

    call diffractIM(dzdS, qDiffrctd, qOKL, ctx)

!    nextDiff = nextDiff + diffStep

  end if

end if

!   First step of split-step method:- field diffraction only

  call mpi_barrier(tProcInfo_G%comm, error)

  call allact_rk4_arrs()


  igoes = 0_ip

  qDWrDone = .false.


  ctx%integration%current_step = ctx%integration%start_step

  do


    ctx%integration%current_step = ctx%integration%current_step + 1_ip

    if (ctx%integration%current_step > ctx%integration%total_steps) exit

    ctx%lattice%cumulative_steps = ctx%lattice%cumulative_steps + 1_ip

!   Second half of split step method: electron propagation
!                    and field driving.

    if (ctx%flags%electrons_evolve .OR. ctx%flags%field_evolve &
             .OR. ctx%flags%electron_field_coupling) then

      igoes = 1_ip
      do
        call rk4par(sZl, ctx%integration%step_size, qDiffrctd, ctx)
        if (igoes>3_ip) exit
        if (.not. ctx%flags%parallel_arrays_ok) then
          call deallact_rk4_arrs()
          if (.not. ctx%flags%inner_xy_ok) then
            call getInNode(ctx%flags)
            ctx%flags%inner_xy_ok = .true.
          end if
          call getLocalFieldIndices(ctx%integration%redistribution_length, ctx%flags, ctx%frame)
          ctx%flags%parallel_arrays_ok = .true.
          call allact_rk4_arrs()
          ctx%flags%inner_xy_ok = .true.
        else
          exit
        end if
        igoes = igoes + 1_ip
      end do

    end if


    if (igoes>3) exit

!                  Increment z position
!       (we now have solution at zbar + sStepsize)

    sZl = sZl + ctx%integration%step_size
    sZ = ctx%und%z_taper_start + szl
    ctx%integration%z_inter = ctx%integration%z_inter + ctx%integration%step_size


!   diffract field to complete diffraction step

    if (ctx%flags%diffraction) then

      if ((mod(ctx%integration%current_step,isteps4diff) == 0_ip) .or. &
          (ctx%integration%current_step == ctx%integration%total_steps))  then

!        call deallact_rk4_arrs()

        call inner2Outer(ac_rfield_in, ac_ifield_in)

        dzdF = dzdS  ! Finishing last diffraction step
                     ! - must be indentical size

! Start of next diffraction step is this ->
! dzdS = either 0, steps4diff*dz / 2, or stepsLeft*dz / 2

        if (ctx%integration%current_step == ctx%integration%total_steps) then

          dzdS = 0.0_wp

        else

          if ((ctx%integration%current_step + isteps4diff) <= ctx%integration%total_steps) then

            dzdS = real(isteps4diff,kind=wp)*ctx%integration%step_size / 2

          else if ((ctx%integration%current_step + isteps4diff) > ctx%integration%total_steps) then

            stepsLeft = ctx%integration%total_steps - ctx%integration%current_step
            dzdS = real(stepsLeft,kind=wp)*ctx%integration%step_size / 2

          end if

        end if

        if (.not. qWriteq(ctx%integration%current_step, ctx%lattice%cumulative_steps, &
                          ctx%output%write_nth_steps, ctx%output%write_nth_steps_intermediate, &
                          ctx%integration%total_steps)) then

        ! if not writing then we can do the last half of the
        ! last diffraction step and the first half of the next
        ! in the same step...

          dzd = dzdF + dzdS

          call diffractIM(dzd, qDiffrctd, qOKL, ctx)
          call outer2Inner(ac_rfield_in, ac_ifield_in)
        else

        ! If writing in this step, then we need to first
        ! finish the last diffraction step, and THEN write,
        ! and then start the next diffraction step.

          call diffractIM(dzdF, qDiffrctd, qOKL, ctx)  ! Finish diffraction step
          call writeIM(sZ, sZl, ctx, iM, qOKL)   ! Write data
          if (dzdS > 0.0_wp) call diffractIM(dzdS, qDiffrctd, qOKL, ctx)  ! Start new diffraction step
          call outer2Inner(ac_rfield_in, ac_ifield_in)
          qDWrDone = .true.

        end if
      end if
    end if  ! end diffraction step

!                   Write result to file

  ctx%integration%count = ctx%integration%count + 1_IP

    if (qWriteq(ctx%integration%current_step, ctx%lattice%cumulative_steps, &
                ctx%output%write_nth_steps, ctx%output%write_nth_steps_intermediate, &
                ctx%integration%total_steps)) then

      if (.not. qDWrDone) then

        ! if not already written in diffraction step

        call inner2Outer(ac_rfield_in, ac_ifield_in)

        call writeIM(sZ, sZl, ctx, iM, qOKL)

      else

        qDWrDone = .false.  ! reset

      end if

    end if


  call Get_time(locEndTime)

  if ((tProcInfo_G%QROOT ) .and. (ctx%output%output_info_level > 1)) then
    print*," finished step ",ctx%lattice%cumulative_steps, &
           ctx%integration%current_step, locEndTime - ctx%integration%time_start
    WRITE(137,*) " finished step ",ctx%lattice%cumulative_steps, &
                 ctx%integration%current_step, locEndTime - ctx%integration%time_start
  end if



  if (mod(ctx%lattice%cumulative_steps, ctx%integration%redistribution_step) == 0) then

    call deallact_rk4_arrs()
    call getLocalFieldIndices(ctx%integration%redistribution_length, ctx%flags, ctx%frame)
    call allact_rk4_arrs()

  end if

  end do

  call deallact_rk4_arrs()


  if (igoes>3_ip) then

    if (tProcInfo_G%qRoot) print*, "Tried rearranging 3 times..."
    if (tProcInfo_G%qRoot) print*, "...didnt work, so stopping..."
    call mpi_finalize(error)
    stop

  end if

  if (.not. ctx%und%model_undulator_ends) call matchOut(sZ, ctx%frame, ctx%und%n2col)

  call correctTrans()  ! correct transverse motion at undulator exit

  ctx%lattice%current_und_index = ctx%lattice%current_und_index + 1_ip
  qResume_G = .false.

  if ((tProcInfo_G%QROOT ) .and. (ctx%output%output_info_level > 0)) then
    print*," Finished undulator module in ", locEndTime-locTimeSt, "seconds"
  end if

  call UpdateGlobalsFromIntegrationState(ctx%integration)
  call UpdateGlobalsFromUndulator(ctx%und)

end subroutine UndSection


end module undulator
