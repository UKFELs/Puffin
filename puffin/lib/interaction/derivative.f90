! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Module which calculates d/dz of the local electron and field variables, and 
!> then sums the global regions together.

module Derivative

! Module to calculate derivative required to integrate
! using rk4

use rhs
use ParaField
use GlobalTypes, only: tUndulator, tFELFrame, tSimulationFlags

implicit none

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!> Subroutine which calculate d/dz of the local electron and field 
!> variables, and then sums the global regions together.
!> @param sz position in undulator module.
!> @param sAr real field 
!> @param sAi imaginary field 
!> @param sx electron macroparticles' x position


  subroutine derivs(sz, sAr, sAi, sx, sy, sz2, spr, spi, sp2, &
                    sdx, sdy, sdz2, sdpr, sdpi, sdp2, sdAr, sdAi, &
                    und, frame, flags)

  implicit none

! External subroutine returning dydz at z for use with RK4
!
!                 ARGUMENTS
!
! sz      INPUT     value of z to evaluate derivative at
! sy      INPUT     Value of y at this z
! sdydz   OUTPUT    Derivative of z and y

    real(kind=wp), intent(in)  :: sz
    real(kind=wp), contiguous, intent(in)  :: sAr(:), sAi(:)
    real(kind=wp), contiguous, intent(in)  :: sx(:), sy(:), sz2(:), &
                                  spr(:), spi(:), sp2(:)

    real(kind=wp), contiguous, intent(inout)  :: sdx(:), sdy(:), sdz2(:), &
                                  sdpr(:), sdpi(:), sdp2(:)

    real(kind=wp), contiguous, intent(inout) :: sdAr(:), sdAi(:)
    type(tUndulator),       intent(inout) :: und
    type(tFELFrame),        intent(in)    :: frame
    type(tSimulationFlags), intent(inout) :: flags

!                 LOCAL ARGS
!
! qOKL      Local error flag
! sb        Vector holding the right hand sides

!    real(kind=wp), allocatable :: ldadz(:)
    integer(kind=ip) :: error, iArEr
    logical :: qOKL

!    allocate(LDADz(ReducedNX_G*ReducedNY_G*NZ2_G*2))
!    ldadz = 0.0_WP   ! Local dadz (MPI)




!     Get RHS of field eqn and d/dz of electron variables

    CALL getrhs(sz, &
                sAr, sAi, &
                sx, sy, sz2, &
                spr, spi, sp2, &
                sdx, sdy, sdz2, &
                sdpr, sdpi, sdp2, &
                sdAr, sdAi, &
                qOKL, und, frame)

! Sync flags from globals after getrhs: getInterps_3D (system_interpolation.f90)
! writes qPArrOK_G and qInnerXYOK_G directly; pull them into flags here.
    flags%parallel_arrays_ok = qPArrOK_G
    flags%inner_xy_ok = qInnerXYOK_G




!    update fields in buffers

      call upd8da(sdAr, sdAi)



!     Check to see if parallel field setup OK...

      if (.not. flags%parallel_arrays_ok) then
        iArEr = 1_ip
      else
        iArEr = 0_ip
      end if

      call mpi_allreduce(mpi_in_place, iArEr, 1, &
            MPI_INTEGER, &
            MPI_SUM, MPI_COMM_WORLD, error)

      if (iArEr > 0_ip) then
        flags%parallel_arrays_ok = .false.
        qPArrOK_G = .false.  ! keep global in sync (para_field.f90 reads it)
        if ((tProcInfo_G%qRoot) .and. (ioutInfo_G > 2)) then
          print*, 'electron outside parallel bounds!'
          print*, 'Emergency redistribute!!!'
          print*, 'If this happens often, then &
                & it is possible the parallel &
                & tuning parameters are inefficient, &
                & and not suitable...'
        end if
      end if



      if (.not. flags%inner_xy_ok) then
        iArEr = 1_ip
      else
        iArEr = 0_ip
      end if

      call mpi_allreduce(mpi_in_place, iArEr, 1, &
            MPI_INTEGER, &
            MPI_SUM, MPI_COMM_WORLD, error)

      if (iArEr > 0_ip) then
        flags%inner_xy_ok = .false.
        qInnerXYOK_G = .false.  ! keep global in sync
        if ((tProcInfo_G%qRoot) .and. (ioutInfo_G > 2) ) then
          print*, 'electron outside transverse bounds!'
          print*, 'Emergency redistribute!!!'
        end if
      end if


!    scatter dadz to local MPI process (split amongst processors)

!    CALL scatter2Loc(sda, ldadz, local_rows, &
!         ReducedNX_G*ReducedNY_G*NZ2_G, mrecvs, mdispls, 0)


!    DEALLOCATE(LDADz)

    GOTO 2000

!     Error Handler

1000 CALL log_error('Error in Derivative:derivs',tErrorLog_G)
    PRINT*,'Error in Derivative:derivs'
2000 CONTINUE

  END SUBROUTINE derivs

END MODULE Derivative
