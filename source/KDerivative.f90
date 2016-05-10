!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

module Derivative

! Module to calculate derivative required to integrate
! using rk4

use rhs
use ParaField

implicit none

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





  subroutine derivs(sz, sAr, sAi, sx, sy, sz2, spr, spi, sp2, &
                    sdx, sdy, sdz2, sdpr, sdpi, sdp2, sdAr, sdAi)

  implicit none

! External subroutine returning dydz at z for use with RK4
!
!                 ARGUMENTS
!
! sz      INPUT     value of z to evaluate derivative at
! sy      INPUT     Value of y at this z
! sdydz   OUTPUT    Derivative of z and y
	
    real(kind=wp), intent(in)  :: sz
    real(kind=wp), intent(in)  :: sAr(:), sAi(:)
    real(kind=wp), intent(in)  :: sx(:), sy(:), sz2(:), &
                                  spr(:), spi(:), sp2(:)

    real(kind=wp), intent(inout)  :: sdx(:), sdy(:), sdz2(:), &
                                  sdpr(:), sdpi(:), sdp2(:)

    real(kind=wp), intent(inout) :: sdAr(:), sdAi(:)

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
                qOKL)




!    update fields in buffers

      call upd8da(sdAr, sdAi)



!     Check to see if parallel field setup OK...

      if (.not. qPArrOK_G) then
        iArEr = 1_ip
      else
        iArEr = 0_ip
      end if

      call mpi_allreduce(mpi_in_place, iArEr, 1, &
            MPI_INTEGER, &
            MPI_SUM, MPI_COMM_WORLD, error)

      if (iArEr > 0_ip) then 
        qPArrOK_G = .false.
        if (tProcInfo_G%qRoot) then
          print*, 'electron outside parallel bounds!'
          print*, 'Emergency redistribute!!!'
          print*, 'If this happens often, then &
                & it is possible the parallel &
                & tuning parameters are inefficient, &
                & and not suitable...'
        end if
      end if

!    scatter dadz to local MPI process (split amongst processors)

!    CALL scatter2Loc(sda, ldadz, local_rows, &
!         ReducedNX_G*ReducedNY_G*NZ2_G, mrecvs, mdispls, 0)


!    DEALLOCATE(LDADz)
    
    GOTO 2000

!     Error Handler

1000 CALL Error_log('Error in Derivative:derivs',tErrorLog_G)
    PRINT*,'Error in Derivative:derivs'
2000 CONTINUE

  END SUBROUTINE derivs

END MODULE Derivative
