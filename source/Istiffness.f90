MODULE STIFFNESS

USE PARATYPE
USE ParallelInfoType
USE ParallelSetup
USE DerivsGlobals

IMPLICIT NONE

!!!!!!!!!!!!!!
!Global arrays describing stiffness matrix

INTEGER(KIND=IP)  ::  fst_row,lst_row,fst_rowC,lst_rowC,&
                      local_rows,lfst_row,llst_row,&
                      llocal_rows,outx,outy

LOGICAL  ::  qFirst_time

CONTAINS

SUBROUTINE SETUPSTIFFMAT(nx,ny,nz2,elmvol)
!This subroutine sets up the stiffness matrix and
!structures needed to solve the field equation. The
!arrays describing the stiffness matrix are global
!arrays.
!------------------------------------------------------

  INTEGER(KIND=IP),INTENT(IN)  ::  nx,ny,nz2
  REAL(KIND=WP),INTENT(IN)  ::  elmvol

  INTEGER(KIND=IP)  ::  row,col,ix,iy,iz2,highx,lowx,&
       highy,lowy,&
       highz2,lowz2,xa,ya,z2a,&
       local,glob,nnodes,iam,&
       nnz,nnz_loc,nnz_check,ldb,&
       rank,nproc,remainder,error,&
       nnzindex,rowindex,m,n,lrow_base

  REAL(KIND=WP)  ::  val,trans,fact
  LOGICAL  ::  qOneD
  
  nproc = tProcInfo_G%size
  rank = tProcInfo_G%rank 

!     Get total number of nodes

  nnodes=nx*ny*nz2
  m=nnodes
  n=nnodes

  qOneD=.FALSE.

!     Coordinates in transverse plane for boundary conditions

  trans=nx*ny
    
  IF(trans==1) THEN
     qOneD=.TRUE.
  END IF

!     Get stiffness matrix common factor

!  fact=elmvol/216.0_WP
!  fact=1.0_WP/216.0_WP

  IF (qOneD) THEN
     fact=1.0_WP
  ELSE
     fact=1.0_WP/8.0_WP
  END IF

!     Stiffness matrix has order nnodes. Number of local
!     rows given by nnodes/numproc

  IF (MOD(nnodes,nproc)==0) THEN
     local_rows=nnodes/nproc
     fst_row=rank*local_rows+1
     lst_row=(rank+1)*local_rows
  ELSE

!     Get local_rows "base" value(the minimum value for
!     local rows on any process)

     lrow_base=FLOOR(REAL(nnodes,KIND=WP)/REAL(nproc,KIND=WP))
     remainder=MOD(nnodes,nproc)
     IF (rank<remainder) THEN
        local_rows=lrow_base+1_IP
        fst_row=(rank)*local_rows+1
        lst_row=fst_row+local_rows-1
     ELSE
        local_rows=lrow_base
        fst_row=remainder*(local_rows+1_IP)&
             +((rank-remainder)*local_rows)+1
        lst_row=fst_row+local_rows-1_IP
     END IF
  ENDIF

END SUBROUTINE SETUPSTIFFMAT

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE oldG2newG(old,new)

  INTEGER(KIND=IP),INTENT(IN) :: old
  INTEGER(KIND=IP),INTENT(INOUT) :: new

  INTEGER(KIND=IP) :: xold,yold,zold,xnew,ynew,znew,trans

  trans=reducedNX_G*reducedNY_G

!IF (tProcInfo_G%rank==0) PRINT*, 'my old is',old		
!IF (old<200) PRINT*, 'my old is',old		
  xold=mod((old-1_IP),reducedNX_G)+1_IP
  yold=mod(FLOOR(REAL((old-1_IP),KIND=WP)/&
       REAL(reducedNX_G,KIND=WP)),reducedNY_G)+1_IP
  zold=(old-1_IP)/trans+1.0_IP

!IF (old<200) PRINT*, 'my old x is ',xold
!IF (old<200) PRINT*, 'my old y is ',yold
!IF (old<200) PRINT*, 'my old z is ',zold

!IF (tProcInfo_G%rank==0) PRINT*, 'my old x is ',xold
!IF (tProcInfo_G%rank==0) PRINT*, 'my old y is ',yold
!IF (tProcInfo_G%rank==0) PRINT*, 'my old z is ',zold

  xnew=xold+(outnodex_G/2_IP)
  ynew=yold+(outnodey_G/2_IP)
  znew=zold

!IF (old<200) PRINT*, 'my new x is ',xnew
!IF (old<200) PRINT*, 'my new y is ',ynew
!IF (old<200) PRINT*, 'my new z is ',znew

!IF (tProcInfo_G%rank==0) PRINT*, 'my new x is ',xnew
!IF (tProcInfo_G%rank==0) PRINT*, 'my new y is ',ynew
!IF (tProcInfo_G%rank==0) PRINT*, 'my new z is ',znew

  new=xnew+(ynew-1)*(NX_G)+(NX_G)*(NY_G)*(znew-1)

!IF (tProcInfo_G%rank==0) PRINT*, 'so my new is',new
!IF (old<200) PRINT*, 'so my new is',new

END SUBROUTINE oldG2newG

!======================================================

SUBROUTINE getLargeLocSizes(fst_row,lst_row,newfst,&
     newlst,newlocsize)

  INTEGER(KIND=IP),INTENT(IN) :: fst_row,lst_row
  INTEGER(KIND=IP),INTENT(OUT) :: newfst,newlst,newlocsize

  INTEGER(KIND=IP) :: xfst,yfst,xlst,ylst

!     Begin

  xfst=mod((fst_row-1_IP),reducedNX_G)+1_IP
  yfst=mod(FLOOR(REAL((fst_row-1_IP),KIND=WP)&
       /REAL(reducedNX_G,KIND=WP)),&
       reducedNY_G)+1_IP

  CALL oldG2newG(fst_row,newfst)

  IF ((xfst==1) .AND. (.NOT. yfst==1)) THEN
     newfst=newfst-(outnodex_G/2)
  ELSE IF ((xfst==1) .AND. (yfst==1)) THEN
     newfst=newfst-NX_G*(outnodey_G/2)-(outnodex_G/2)
  END IF

!!!!!!

  xlst=mod((lst_row-1_IP),reducedNX_G)+1_IP
  ylst=mod(FLOOR(REAL((lst_row-1_IP),KIND=WP)/&
       REAL(reducedNX_G,KIND=WP)),&
       reducedNY_G)+1_IP
		  
  CALL oldG2newG(lst_row,newlst)

  IF ((xlst==ReducedNX_G) .AND. (.NOT. ylst==ReducedNY_G)) THEN
     newlst=newlst+(outnodex_G/2)
  ELSE IF ((xlst==ReducedNX_G) .AND. (ylst==ReducedNX_G)) THEN
     newlst=newlst+NX_G*(outnodey_G/2)+(outnodex_G/2)
  END IF
 
  newlocsize=newlst-newfst+1_IP

END SUBROUTINE getLargeLocSizes

!========================================================

SUBROUTINE getAlocalFS(sA,A_local)

! get A local From Small

  REAL(KIND=WP), DIMENSION(:),INTENT(INOUT) :: sA
  REAL(KIND=WP), DIMENSION(:),INTENT(INOUT) :: A_local

!     local vars

  INTEGER(KIND=IP) :: i,j,count

  count=0_IP

!!!! THIS loop gives us a local array from an already 
!!!! "reduced" field array

  DO i=fst_row,lst_row
     count=count+1_IP
     A_local(count)=sA(i)
     A_local(count+local_rows)=sA(i+(ReducedNX_G*ReducedNY_G*NZ2_G))
  END DO

!!! THIS loop gives us a local array from a "full" field
!!! array

!  DO i=fst_row,lst_row
!     count=count+1_IP
!     CALL oldG2newG(i,j)
!     A_local(count)=sA(j)
!     A_local(count+local_rows)=sA(j+iNumberNodes_G)
!  END DO


!!! Save the local array for each RK4 loop, UNLESS
!!! it's time to do diffraction...then you don't have to
!!! use sends to set things up at the beginning and end...

! so only if diffraction has occured do we setup a new inner, local 
! field, and only if diffraction is about to occur do we slot the 
! old field into a new "full" field array. We always need to use 
! sends to update the inner, global field array at the end of RK4.
! However, unless diffraction will occur, we don't need to update
! the full, global field at the end of RK4, and we can save the inner
! local and global fields for the next RK4 routine.







END SUBROUTINE getAlocalFS
!========================================================
SUBROUTINE getAlocalFL(sA,A_local)

! Get A local From Large

  REAL(KIND=WP), DIMENSION(:),INTENT(INOUT) :: sA
  REAL(KIND=WP), DIMENSION(:),INTENT(INOUT) :: A_local

!     local vars

  INTEGER(KIND=IP) :: i,j,count

  count=0_IP

!!! THIS loop gives us a local array from a "full" field
!!! array

  DO i=fst_row,lst_row
     count=count+1_IP
     CALL oldG2newG(i,j)
     A_local(count)=sA(j)
     A_local(count+local_rows)=sA(j+iNumberNodes_G)
  END DO


END SUBROUTINE getAlocalFL !getASmall

!==========================================================

SUBROUTINE innerLA2largeA(A_local,sA,recvs,displs,qOneD)

! Inner Local A to large global A

  REAL(KIND=WP), DIMENSION(:),INTENT(INOUT) :: sA
  INTEGER(KIND=IP), INTENT(IN) :: recvs(:),displs(:)
  REAL(KIND=WP), DIMENSION(:),INTENT(INOUT) :: A_local
  LOGICAL, INTENT(IN) :: qOneD

!     local vars

  INTEGER(KIND=IP) :: i,j,count,error
  REAL(KIND=WP) :: outerA_local(2_IP*llocal_rows)

!  ALLOCATE(outerA_local(2_IP*llocal_rows))

  IF (qOneD) THEN
    
    outerA_local = A_local

  ELSE
 ! First part converts inner to outer
    count=0_IP
    outerA_local(1_IP:llocal_rows)=sA(lfst_row:llst_row)
 
    outerA_local(llocal_rows+1_IP:2_IP*llocal_rows)=&
         sA(lfst_row+iNumberNodes_G:llst_row+iNumberNodes_G)

    count=0_IP
 
    DO i=fst_row,lst_row
     count=count+1_IP
     CALL oldG2newG(i,j)
     j=j-displs(tProcInfo_G%rank+1_IP)
     outerA_local(j)=A_local(count)
     outerA_local(j+llocal_rows)=A_local(count+local_rows)
    END DO

    CALL MPI_BARRIER(tProcInfo_G%comm,error)
  
  END IF
    ! 2nd part sends locals to globals
  CALL Gather2A(outerA_local,sA,llocal_rows,&  ! llocal_rows, &
                iNumberNodes_G,recvs,displs)

END SUBROUTINE innerLA2largeA
!====================================================













SUBROUTINE local2globalA(A_local,sA,recvs,displs,qOneD)

! Inner Local A to small global A

  REAL(KIND=WP), DIMENSION(:),INTENT(INOUT) :: sA
  INTEGER(KIND=IP), INTENT(IN) :: recvs(:),displs(:)
  REAL(KIND=WP), DIMENSION(:),INTENT(INOUT) :: A_local
  LOGICAL, INTENT(IN) :: qOneD

!     local vars

  INTEGER(KIND=IP) :: i,j,count,error
  REAL(KIND=WP) :: outerA_local(2_IP*llocal_rows)

!  ALLOCATE(outerA_local(2_IP*llocal_rows))

!  IF (qOneD) THEN
    
!    outerA_local = A_local

!  ELSE
! ! First part converts inner to outer
!    count=0_IP
!    outerA_local(1_IP:llocal_rows)=sA(lfst_row:llst_row)
! 
!    outerA_local(llocal_rows+1_IP:2_IP*llocal_rows)=&
!         sA(lfst_row+iNumberNodes_G:llst_row+iNumberNodes_G)
!
!    count=0_IP
! 
!    DO i=fst_row,lst_row
!     count=count+1_IP
!     CALL oldG2newG(i,j)
!     j=j-displs(tProcInfo_G%rank+1_IP)
!     outerA_local(j)=A_local(count)
!     outerA_local(j+llocal_rows)=A_local(count+local_rows)
!    END DO
!
!    CALL MPI_BARRIER(tProcInfo_G%comm,error)
!  
!  END IF
    ! 2nd part sends locals to globals
  CALL Gather2A(A_local,sA,local_rows,&  ! llocal_rows, &
                ReducedNX_G*ReducedNY_G*NZ2_G,recvs,displs)

END SUBROUTINE local2globalA

!=====================================================

END MODULE STIFFNESS
