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

SUBROUTINE rk4par(sA,A_local,x,h,recvs,displs,qD)

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
  !REAL(KIND=WP), DIMENSION(size(y)) :: dym, dyt, yt

  REAL(KIND=WP), DIMENSION(iNumberElectrons_G) :: dxm, dxt, xt    ! *t is 'temp', for use in next rhs call...
  REAL(KIND=WP), DIMENSION(iNumberElectrons_G) :: dym, dyt, yt
  REAL(KIND=WP), DIMENSION(iNumberElectrons_G) :: dpxm, dpxt, pxt
  REAL(KIND=WP), DIMENSION(iNumberElectrons_G) :: dpym, dpyt, pyt
  REAL(KIND=WP), DIMENSION(iNumberElectrons_G) :: dz2m, dz2t, z2t
  REAL(KIND=WP), DIMENSION(iNumberElectrons_G) :: dpz2m, dpz2t, pz2t  

  REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: dAm, dAt
  REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: dxdx, dydx, dz2dx, dpxdx, dpydx, dpz2dx
  REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: dAdx
  REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: A_localt 
  INTEGER(KIND=IP) :: error, trans

!    Transverse nodes

  trans = (NX_G)*(NY_G)

!    Step sizes

  hh = h * 0.5_WP    
  h6 = h / 6.0_WP
  xh = x + hh

  ALLOCATE(DxDx(iNumberElectrons_G))	  
  ALLOCATE(DyDx(iNumberElectrons_G))    
  ALLOCATE(DpxDx(iNumberElectrons_G))    
  ALLOCATE(DpyDx(iNumberElectrons_G))    
  ALLOCATE(Dz2Dx(iNumberElectrons_G))    
  ALLOCATE(Dpz2Dx(iNumberElectrons_G))    



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

  iy = size(sElX_G)
  idydx = size(dxdx)
      
  IF (iy /= idydx ) THEN
     GOTO 1000
  END IF
  
!    Get derivatives

  CALL derivs(x, &
       sA, &
       sElX_G, sElY_G, sElZ2_G, sElPX_G, sElPY_G, sElPZ2_G, &
       dxdx, dydx, dz2dx, dpxdx, dpydx, dpz2dx, &
       dAdx)

  ALLOCATE(dAm(2*local_rows),dAt(2*local_rows))

!    Increment local electron and field values
  
  xt = sElX_G      +  hh*dxdx
  yt = sElY_G      +  hh*dydx
  z2t = sElZ2_G    +  hh*dz2dx
  pxt = sElPX_G    +  hh*dpxdx
  pyt = sElPY_G    +  hh*dpydx
  pz2t = sElPZ2_G  +  hh*dpz2dx


  A_localt = A_local + hh * dAdx  	  

!    Update large field array with new values 
  CALL local2globalA(A_localt,sA,recvs,displs,tTransInfo_G%qOneD)

!    Second step       
!    Get derivatives

  CALL derivs(xh, &
       sA, &
       xt, yt, z2t, pxt, py2, pz2t, &
       dxt, dyt, dz2t, dpxt, dpyt, dz2t, &
       dAt)

!    Incrementing with newest derivative value...

  xt = sElX_G      +  hh*dxt
  yt = sElY_G      +  hh*dyt
  z2t = sElZ2_G    +  hh*dz2t
  pxt = sElPX_G    +  hh*dpxt
  pyt = sElPY_G    +  hh*dpyt
  pz2t = sElPZ2_G  +  hh*dpz2t

  A_localt = A_local + hh * dAt

!    Update full field array

  CALL local2globalA(A_localt,sA,recvs,displs,tTransInfo_G%qOneD)

!    Third step       
!    Get derivatives

  CALL derivs(xh, &
       sA, &
       xt, yt, z2t, pxt, py2, pz2t, &
       dxm, dym, dz2m, dpxm, dpym, dz2m, &
       dAm)

!    Incrementing

  xt = sElX_G      +  h*dxm
  yt = sElY_G      +  h*dym
  z2t = sElZ2_G    +  h*dz2m
  pxt = sElPX_G    +  h*dpxm
  pyt = sElPY_G    +  h*dpym
  pz2t = sElPZ2_G  +  h*dpz2m

  A_localt=A_local + h * dAm

  CALL local2globalA(A_localt,sA,recvs,displs,tTransInfo_G%qOneD)

  dxm = dxt + dxm
  dym = dyt + dym
  dz2m = dz2t + dz2m
  dpxm = dpxt + dpxm
  dpym = dpyt + dpym
  dpz2m = dpz2t + dpz2m

  dAm = dAt + dAm

!    Fourth step       

  xh=x+h

!    Get derivatives

  CALL derivs(xh, &
       sA, &
       xt, yt, z2t, pxt, py2, pz2t, &
       dxt, dyt, dz2t, dpxt, dpyt, dz2t, &
       dAt)  

!    Accumulate increments with proper weights       

  sElX_G    = sElX_G   + h6 * ( dxdx   + dxt   + 2.0_WP * dxm  )
  sElY_G    = sElY_G   + h6 * ( dydx   + dyt   + 2.0_WP * dym  )
  sElZ2_G   = sElZ2_G  + h6 * ( dz2dx  + dz2t  + 2.0_WP * dz2m )
  sElPX_G   = sElPX_G  + h6 * ( dpxdx  + dpxt  + 2.0_WP * dpxm )
  sElPY_G   = sElPY_G  + h6 * ( dpydx  + dpyt  + 2.0_WP * dpym )
  sElPZ2_G  = sElPZ2_G + h6 * ( dpz2dx + dpz2t + 2.0_WP * dpz2m)



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
