!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

module Derivative

! Module to calculate derivative required to integrate
! using rk4

use rhs

implicit none

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




  call derivs(x, &
       sA, &
       sElX_G, sElY_G, sElZ2_G, sElPX_G, sElPY_G, sElPZ2_G, &
       dxdx, dydx, dz2dx, dpxdx, dpydx, dpz2dx, &
       dAdx)




  subroutine derivs(sz, sA, sx, sy, sz2, spr, spi, sp2, &
                    sdx, sdy, sdz2, sdpr, sdpi, sdp2, sdA)

  implicit none

! External subroutine returning dydz at z for use with RK4
!
!                 ARGUMENTS
!
! sz      INPUT     value of z to evaluate derivative at
! sy      INPUT     Value of y at this z
! sdydz   OUTPUT    Derivative of z and y
	
    real(kind=wp), intent(in)  :: sz
    real(kind=wp), intent(in)  :: sA(:)
    real(kind=wp), intent(in)  :: sx(:), sdx(:)
    real(kind=wp), intent(in)  :: sy(:), sdy(:)
    real(kind=wp), intent(in)  :: sz2(:), sdz2(:)
    real(kind=wp), intent(in)  :: spr(:), sdpr(:)
    real(kind=wp), intent(in)  :: spi(:), sdpi(:)
    real(kind=wp), intent(in)  :: sp2(:), sdp2(:)

    real(kind=wp), intent(out) :: sda(:)

!                 LOCAL ARGS
!
! qOKL      Local error flag
! sb        Vector holding the right hand sides

    real(kind=wp), allocatable :: ldadz(:)
    integer(kind=ip) :: error
    logical :: qOKL

    allocate(LDADz(ReducedNX_G*ReducedNY_G*NZ2_G*2))
    ldadz = 0.0_WP   ! Local dadz (MPI)




!     Get RHS of field eqn and d/dz of electron variables

    CALL getrhs(sz,&
                sA,&
                sx, sy, sz2, &
                spr, spi, sp2, &
                sdx, sdy, sdz2, &
                sdpr, sdpi, sdp2, &
                ldadz, &
                qOKL)




!    scatter dadz to local MPI process (split amongst processors)

    CALL scatter2Loc(sda, ldadz, local_rows, &
         ReducedNX_G*ReducedNY_G*NZ2_G, mrecvs, mdispls, 0)


    DEALLOCATE(LDADz)
    
    GOTO 2000

!     Error Handler

1000 CALL Error_log('Error in Derivative:derivs',tErrorLog_G)
    PRINT*,'Error in Derivative:derivs'
2000 CONTINUE

  END SUBROUTINE derivs

END MODULE Derivative
