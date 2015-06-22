!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE Derivative

! Module to calculate derivative required to integrate
! using rk4

USE rhs
IMPLICIT NONE

CONTAINS

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE derivs(sz, &
       sA, &
       sy, &
       sdydz, &
       sDADz)
!
! External subroutine returning dydz at z for use with RK4
!
!                 ARGUMENTS
!
! sz      INPUT     value of z to evaluate derivative at
! sy      INPUT     Value of y at this z
! sdydz   OUTPUT    Derivative of z and y
	
    REAL(KIND=WP),INTENT(IN) :: sz
    REAL(KIND=WP),DIMENSION(:), INTENT(IN)  :: sA
    REAL(KIND=WP),DIMENSION(:), INTENT(IN)  :: sy
    REAL(KIND=WP),DIMENSION(:), INTENT(OUT) :: sdydz
    REAL(KIND=WP),DIMENSION(:), INTENT(OUT) :: sDADz

!                 LOCAL ARGS
!
! qOKL      Local error flag
! sb        Vector holding the right hand sides

    REAL(KIND=WP), ALLOCATABLE, Dimension(:) :: LDADz
    INTEGER(KIND=IP) :: error
    LOGICAL :: qOKL

    ALLOCATE(LDADz(ReducedNX_G*ReducedNY_G*NZ2_G*2))
    LDADz = 0.0_WP   ! Local dadz (MPI)




!     Get RHS of field eqn and d/dz of electron variables

    CALL getrhs(sz,&
                sA,&
                sy,&
                sdydz,&
                LDADz, &
                qOKL)




!    scatter dadz to local MPI process (split amongst processors)

    CALL scatter2Loc(sDADz,LDADz,local_rows,ReducedNX_G*ReducedNY_G*NZ2_G,mrecvs,mdispls,0)


    DEALLOCATE(LDADz)
    
    GOTO 2000

!     Error Handler

1000 CALL Error_log('Error in Derivative:derivs',tErrorLog_G)
    PRINT*,'Error in Derivative:derivs'
2000 CONTINUE

  END SUBROUTINE derivs

END MODULE Derivative
