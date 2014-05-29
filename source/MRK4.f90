!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE RK4int

USE ParallelInfoType
USE TransformInfoType
USE FFTW_Constants
USE Globals
USE Derivative
USE IO
CONTAINS

SUBROUTINE rk4par(y,sA,A_local,x,h,recvs,displs,qD)

  IMPLICIT NONE
!
! Perform 4th order Runge-Kutta integration, tailored
! to Puffin and its method of parallelization: 
! This is NOT a general, all-purpose RK4 routine, it
! is specific to Puffin. Includes MPI_gathers and 
! scatters etc between calculation of derivatives for 
! use with the parallel field derivative.
!
!                ARGUMENTS
!
! y       INPUT/OUTPUT   Electron values
! SA      INPUT/OUTPUT   Field values
! x       INPUT          Propagation distance zbar
! h       INPUT          Step size in zbar
      
  REAL(KIND=WP),  DIMENSION(:), INTENT(INOUT) :: y
  REAL(KIND=WP),  DIMENSION(:), INTENT(INOUT) :: sA, A_local
  REAL(KIND=WP),  INTENT(IN)                  :: x
  REAL(KIND=WP),                INTENT(IN)  :: h
  INTEGER(KIND=IP),DIMENSION(:),INTENT(IN)  :: recvs,displs
  LOGICAL, INTENT(INOUT) :: qD

!               LOCAL ARGS
!
! h6         Step size divided by 6
! hh         Half of the step size 
! xh         x position incremented by half a step
! dym        Intermediate derivatives
! dyt        Intermediate derivatives
! yt         Incremental solution
! dAdx       Field derivative
! dydx       Electron derivatives

  INTEGER(KIND=IP) :: iy,idydx,iyout,i,p
  REAL(KIND=WP)    :: h6, hh, xh
  REAL(KIND=WP), DIMENSION(size(y)) :: dym, dyt, yt
  REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: dAm, dAt
  REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: dydx
  REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: dAdx
  REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: A_localt 
  INTEGER(KIND=IP) :: error, trans

!    Transverse nodes

  trans = (NX_G)*(NY_G)

!    Step sizes

  hh = h * 0.5_WP    
  h6 = h / 6.0_WP
  xh = x + hh

  ALLOCATE(DyDx(nElectronEquations_CG*iNumberElectrons_G))	  
  ALLOCATE(DADx(2*local_rows))
  ALLOCATE(A_localt(2*local_rows))

!    A_local from A_big	  

  IF (qD) THEN

!    IF (tTransInfo_G%qOneD) THEN
!       A_local(1:local_rows)=sA(fst_row:lst_row)
!       A_local(local_rows+1:2*local_rows)=&
!            sA(fst_row+iNumberNodes_G:lst_row+iNumberNodes_G)
!    ELSE
!       CALL getAlocalFS(sA,A_local)
!    END IF
!
    qD = .false.
!
  END IF

!    First step       
!    Incrementing Y and A
!    Error checking         

  iy =size(y)
  idydx =size(dydx)
      
  IF (iy /= idydx ) THEN
     GOTO 1000
  END IF
  
!    Get derivatives

  CALL derivs(x, &
       sA, &
       y, &
       dydx,&
       dAdx)

  ALLOCATE(dAm(2*local_rows),dAt(2*local_rows))

!    Increment local electron and field values
  yt = y + hh * dydx
  A_localt = A_local + hh * dAdx  	  

!    Update large field array with new values 
  CALL local2globalA(A_localt,sA,recvs,displs,tTransInfo_G%qOneD)

!    Second step       
!    Get derivatives

  CALL derivs(xh, &
       sA, &
       yt, &
       dyt,&
       dAt)

!    Incrementing with newest derivative value...

  yt = y + hh * dyt
  A_localt = A_local + hh * dAt

!    Update full field array

  CALL local2globalA(A_localt,sA,recvs,displs,tTransInfo_G%qOneD)

!    Third step       
!    Get derivatives

  CALL derivs(xh, &
       sA, &
       yt, &
       dym,&
       dAm)

!    Incrementing

  yt = y + h * dym
  A_localt=A_local + h * dAm

  CALL local2globalA(A_localt,sA,recvs,displs,tTransInfo_G%qOneD)

  dym = dyt + dym
  dAm = dAt + dAm

!    Fourth step       

  xh=x+h

!    Get derivatives

  CALL derivs(xh, &
       sA, &
       yt, &
       dyt,&
       dAt)  

!    Accumulate increments with proper weights       

  y = y + h6 * (dydx + dyt + 2.0_WP * dym)
  A_local = A_local + h6 * (dAdx + dAt + 2.0_WP * dAm)

  CALL local2globalA(A_local,sA,recvs,displs,tTransInfo_G%qOneD)

!    Deallocating temp arrays

  DEALLOCATE(dAm,dAt,A_localt)
  DEALLOCATE(DyDx)
  DEALLOCATE(DADx)

!   Set error flag and exit         

  GOTO 2000

!   Error Handler - Error log Subroutine in CIO.f90 line 709

1000 CALL Error_log('Error in MathLib:rk4',tErrorLog_G)
  PRINT*,'Error in MathLib:rk4'
2000 CONTINUE

END SUBROUTINE rk4par

END MODULE rk4int
