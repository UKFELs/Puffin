! ###############################################
! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause
! ###############################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Module containing routines to setup and deallocate MPI processes,
!> and handle MPI communication in Puffin.

module ParallelSetUp

use paratype
use typempicomm
use IO
! use mpi


implicit none

include 'mpif.h'

integer :: MPI_INT_HIGH

contains

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Initialize and retrieve info on the MPI processes. Just sets up the basic
!> MPI communicator across all processes.
!> @param[out] tProcInfo Custom type to hold MPI info
!> @param[out] qOK Error flag for Puffin

subroutine InitializeProcessors(tProcInfo, qOk)

  implicit none

  type(fMPIComm), intent(out)	   :: tProcInfo  
	logical,  intent(out) :: qOk

  integer(kind=ip)    :: error, provided

!     Begin

  qOK = .false.

!     Initialize MPI

  call MPI_INIT_THREAD(MPI_THREAD_FUNNELED, provided, error)

!     Define comm as the global communicator.

  tProcInfo%comm = MPI_COMM_WORLD

!     Get local data on processor numbers. Rank is local number in the
!     global communicator, size is the total number of processors

  call MPI_COMM_RANK(tProcInfo%comm, tProcInfo%rank, error)

  call MPI_COMM_SIZE(tProcInfo%comm, tProcInfo%size, error)

!     Define root processor as rank 0.

  if (tProcInfo%rank==0) then
    tProcInfo%qRoot = .true.
  end if

!     Define MPI type for high precision integers

!  CALL MPI_TYPE_CREATE_F90_INTEGER(14,MPI_INT_HIGH,error)
  MPI_INT_HIGH = MPI_INTEGER


!     Set error flag and exit

  qOK = .true.
  GoTo 2000

! Error Handler

1000 call Error_log('Error in ParallelSetUp:DefineParallelLibrary', &
                    tErrorLog_G)

  print*,'Error in ParallelSetUp:DefineParallelLibrary'

2000 continue

end subroutine InitializeProcessors

! ############################################################


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Get system time from MPI
!> @param[inout] in_time System time

subroutine Get_time(in_time)

  implicit none

  real(kind=wp), intent(inout)	::	in_time
  integer(kind=ip)	::	error

  in_time = MPI_Wtime()

end subroutine Get_time

! ############################################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Destroy the MPI environment
!> @param[out] qOK Error flag for Puffin

subroutine  UnDefineParallelLibrary(qOK)

  implicit none
  logical, intent(out) :: qOK

  integer(kind=ip)  :: error

  qOK = .FALSE.

  call MPI_FINALIZE(error)

  qOK = .TRUE.
  goto 2000

1000 call Error_log('Error in ParallelSetUp:UnDefineParallelLibrary',tErrorLog_G)
       Print*,'Error in ParallelSetUp:UnDefineParallelLibrary'
2000 CONTINUE

end subroutine UnDefineParallelLibrary

! ############################################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Stop code
!> @param[out] qOK Error flag for Puffin

subroutine  StopCode(qOK)

  implicit none

  logical, intent(out) :: qOK

  logical :: qOKL
  integer(kind=ip)  :: error

  qOK = .false.

  call UnDefineParallelLibrary(qOKL)
  if (.not. qOKL) goto 1000

  stop

  qOK = .true.
  goto 2000

1000 call Error_log('Error in ParallelSetUp:StopCode',tErrorLog_G)
      Print*,'Error in ParallelSetUp:StopCode'
2000 continue

end subroutine StopCode

!======================================================================

SUBROUTINE gather2A(A_local,sA,nA_loc,nA,recvs,displs)

! Gather from A_local to A

REAL(KIND=WP),INTENT(IN)  ::  A_local(:)
INTEGER,INTENT(IN)  ::  nA_loc,nA
INTEGER,INTENT(IN)  ::  recvs(:),displs(:)
REAL(KIND=WP),INTENT(OUT) ::  sA(:)

INTEGER(KIND=IP)  ::  error

 CALL MPI_ALLGATHERV( A_local(1:nA_loc),nA_loc,MPI_DOUBLE_PRECISION, &
	                  sA(1:nA), recvs,displs, MPI_DOUBLE_PRECISION, &
			  		  tProcInfo_G%comm, error)

 CALL MPI_ALLGATHERV( A_local(nA_loc+1:2*nA_loc),nA_loc,MPI_DOUBLE_PRECISION, &
	                  sA(nA+1:2*nA), recvs,displs, MPI_DOUBLE_PRECISION, &
			  		  tProcInfo_G%comm, error)


END SUBROUTINE gather2A

!======================================================================

SUBROUTINE gather1A(A_local,sA,nA_loc,nA,recvs,displs)

! Gather from A_local to A

REAL(KIND=WP),INTENT(IN)  ::  A_local(:)
INTEGER(kind=ip),INTENT(IN)  ::  nA_loc,nA
INTEGER(kind=ip),INTENT(IN)  ::  recvs(:),displs(:)
REAL(KIND=WP),INTENT(OUT) ::  sA(:)

INTEGER(KIND=IP)  ::  error

 CALL MPI_ALLGATHERV( A_local(1:nA_loc),nA_loc,MPI_DOUBLE_PRECISION, &
                    sA(1:nA), recvs,displs, MPI_DOUBLE_PRECISION, &
              tProcInfo_G%comm, error)

END SUBROUTINE gather1A

!======================================================================

SUBROUTINE scatter2Loc(A_local,sA,nA_loc,nA,recvs,displs,root)
! Scatter from A on root to A_local on all processes
REAL(KIND=WP),INTENT(INOUT)  ::  A_local(:)
INTEGER,INTENT(IN)  ::  nA_loc,nA,root
INTEGER,INTENT(IN)  ::  recvs(:),displs(:)
REAL(KIND=WP),INTENT(INOUT) ::  sA(:)

INTEGER(KIND=IP)  ::  error

 CALL MPI_SCATTERV( sA(1:nA),recvs,displs,MPI_DOUBLE_PRECISION, &
	                 A_local(1:nA_loc),nA_loc,MPI_DOUBLE_PRECISION, &
			  	root,tProcInfo_G%comm, error)

 CALL MPI_SCATTERV( sA(nA+1:2*nA),recvs,displs,MPI_DOUBLE_PRECISION, &
	                 A_local(nA_loc+1:2*nA_loc),nA_loc,MPI_DOUBLE_PRECISION, &
			  	root,tProcInfo_G%comm, error)

END SUBROUTINE scatter2Loc


!======================================================================

!======================================================================

SUBROUTINE scatterE2Loc(A_local,sA,nA_loc,nA,recvs,displs,root)
! Scatter from A on root to A_local on all processes
REAL(KIND=WP),INTENT(INOUT)  ::  A_local(:)
INTEGER,INTENT(IN)  ::  nA_loc,nA,root
INTEGER,INTENT(IN)  ::  recvs(:),displs(:)
REAL(KIND=WP),INTENT(INOUT) ::  sA(:)

INTEGER(KIND=IP)  ::  error

 CALL MPI_SCATTERV( sA(1:nA),recvs,displs,MPI_DOUBLE_PRECISION, &
                   A_local(1:nA_loc),nA_loc,MPI_DOUBLE_PRECISION, &
          root,tProcInfo_G%comm, error)


END SUBROUTINE scatterE2Loc

!======================================================================

SUBROUTINE gather2Acomtoreal(A_local,sA,nA_loc,nA,totsize,recvs,displs)

IMPLICIT NONE

! Gather from A_local to A

INTEGER(KIND=IP), INTENT(IN) :: totsize
COMPLEX(KIND=WP),DIMENSION(0:totsize-1),INTENT(IN)  ::  A_local
INTEGER,INTENT(IN)  ::  nA_loc,nA
INTEGER,INTENT(IN)  ::  recvs(:),displs(:)
REAL(KIND=WP),INTENT(OUT) ::  sA(:)

INTEGER(KIND=IP)  ::  error

 CALL MPI_ALLGATHERV( REAL(A_local(0:nA_loc-1),KIND=WP),nA_loc,MPI_DOUBLE_PRECISION, &
	                  sA(1:nA), recvs,displs, MPI_DOUBLE_PRECISION, &
			  		  tProcInfo_G%comm, error)

 CALL MPI_ALLGATHERV( AIMAG(A_local(0:nA_loc-1)),nA_loc,MPI_DOUBLE_PRECISION, &
	                  sA(nA+1:2*nA), recvs,displs, MPI_DOUBLE_PRECISION, &
			  		  tProcInfo_G%comm, error)


END SUBROUTINE gather2Acomtoreal

!===========================================================================

SUBROUTINE getGathArrs(nlocalvals,recvs,displs)

INTEGER(KIND=IP),INTENT(IN)  ::  nlocalvals
INTEGER(KIND=IP),INTENT(OUT)  ::  recvs(:),displs(:)

INTEGER(KIND=IP)  ::  i,error

   CALL MPI_ALLGATHER(nlocalvals, 1, MPI_INTEGER, &
   					  recvs, 1, MPI_INTEGER, &
					  tProcInfo_G%comm, error)

   displs(1) = 0

   DO i=2, tProcInfo_G%size
    displs(i) = displs(i-1) + recvs(i-1)
   END DO

END SUBROUTINE getGathArrs

!======================================================================

SUBROUTINE sum2GlobalArr(loc_arr,nvals)

REAL(KIND=WP),INTENT(INOUT)  ::  loc_arr(:)
INTEGER(KIND=IP),INTENT(IN)  ::  nvals

INTEGER(KIND=IP)  ::  error

 CALL MPI_ALLREDUCE(MPI_IN_PLACE,loc_arr,nvals,MPI_DOUBLE_PRECISION,&
 			MPI_SUM,MPI_COMM_WORLD,error)

END SUBROUTINE sum2GlobalArr

!======================================================================

SUBROUTINE sum2RootArr(loc_arr,nvals,root)

! Takes each process's local array, and calculates
! the global sum of each element in the array.
!
! The resultant array is stored on the rank specified.
!
! loc_arr - the local array. This MUST be of the same size
!           on each process. On the rank specified by 'root',
!           this will be overwritten by the global sum.
!
! nvals   - size of loc_arr
!
! root    - rank of the 'root' process. The root process is
!           where the result will be stored. The root
!           process's array will be overwritten.

REAL(KIND=WP),INTENT(INOUT)  ::  loc_arr(:)
INTEGER(KIND=IP),INTENT(IN)  ::  nvals
INTEGER(KIND=IP),INTENT(IN)  ::  root

INTEGER(KIND=IP)  ::  error

 IF (tProcInfo_G%rank == root) THEN

  CALL MPI_REDUCE(MPI_IN_PLACE,loc_arr,nvals,MPI_DOUBLE_PRECISION,&
 			 MPI_SUM,root,MPI_COMM_WORLD,error)

 ELSE

  CALL MPI_REDUCE(loc_arr,loc_arr,nvals,MPI_DOUBLE_PRECISION,&
 			 MPI_SUM,root,MPI_COMM_WORLD,error)

 END IF

END SUBROUTINE sum2RootArr

!======================================================================

! SUBROUTINE GetMPIfiletype(filetype,mpifiletype)
!
! TYPE(cFileType),INTENT(IN)	::	filetype
! INTEGER(KIND=IP),INTENT(OUT)  ::  mpifiletype
!
! INTEGER(KIND=IP) ::  error
! INTEGER(KIND=IP) ::  arrayoftypes(8)
! INTEGER(KIND=IP) ::  arrayofdisps(8)
! INTEGER(KIND=IP) ::  arrayofblocklengths(8)
! INTEGER(KIND=IP) ::  startaddress,address
!
! arrayoftypes(1)=MPI_CHARACTER
! arrayoftypes(2)=MPI_LOGICAL
! arrayoftypes(3)=MPI_INTEGER
!
! arrayofblocklengths(1)=LEN(fileType%zFileName)
! arrayofblocklengths(2)=3
! arrayofblocklengths(3)=4
!
!  CALL MPI_GET_ADDRESS(filetype%zFileName,startaddress)
!  CALL MPI_GET_ADDRESS(filetype%qFormatted,address)
! arrayofdisps(1)=0
! arrayofdisps(2)=address-startaddress
!  CALL MPI_GET_ADDRESS(filetype%iPos,address)
! arrayofdisps(3)=address-startaddress
!
!  CALL MPI_TYPE_STRUCT(3,arrayofblocklengths,&
!  					  arrayofdisps,arrayoftypes,&
! 					  mpifiletype,error)
!
!  CALL MPI_TYPE_COMMIT(mpifiletype,error)
!
! END SUBROUTINE GetMPIfileType

!================================================================

SUBROUTINE shareFileType(fileType)

TYPE(cFileType),INTENT(INOUT)  ::  filetype

INTEGER error,strsize

strsize=len(filetype%zFileName)

 CALL MPI_BCAST(filetype%zFileName,strsize,MPI_CHARACTER,0,&
                tProcInfo_G%comm,error)

 CALL MPI_BCAST(filetype%qFormatted,1,MPI_LOGICAL,0,&
                tProcInfo_G%comm,error)

 CALL MPI_BCAST(filetype%qForInput,1,MPI_LOGICAL,0,&
                tProcInfo_G%comm,error)

 CALL MPI_BCAST(filetype%qAppend,1,MPI_LOGICAL,0,&
                tProcInfo_G%comm,error)

 CALL MPI_BCAST(filetype%iPos,1,MPI_INTEGER,0,&
                tProcInfo_G%comm,error)

 CALL MPI_BCAST(filetype%iUnit,1,MPI_INTEGER,0,&
                tProcInfo_G%comm,error)

 CALL MPI_BCAST(filetype%iFileLength,1,MPI_INTEGER,0,&
                tProcInfo_G%comm,error)

 CALL MPI_BCAST(filetype%iPage,1,MPI_INTEGER,0,&
                tProcInfo_G%comm,error)

END SUBROUTINE shareFileType

!==============================================================

  SUBROUTINE sum_mpi_int14(in1,o1)

    IMPLICIT NONE

    INTEGER(KIND=IPL), INTENT(IN) :: in1
    INTEGER(KIND=IPL), INTENT(OUT) :: o1

    INTEGER :: error ! for MPI errors

    INTEGER(KIND=IPL), ALLOCATABLE :: trecv(:)

    ALLOCATE(trecv(tProcInfo_G%size))

    CALL MPI_ALLGATHER(in1,1,MPI_INT_HIGH, &
                       trecv,1,MPI_INT_HIGH, &
                       tProcInfo_G%comm,error)

    o1 = sum(trecv)

  END SUBROUTINE sum_mpi_int14


  SUBROUTINE sum_mpi_real(in1,o1)

    IMPLICIT NONE

    real(kind=wp), intent(in) :: in1
    real(kind=wp), intent(out) :: o1

    integer :: error ! for MPI errors

    real(kind=wp), allocatable :: trecv(:)

    ALLOCATE(trecv(tProcInfo_G%size))

    CALL MPI_ALLGATHER(in1,1,MPI_DOUBLE_PRECISION, &
                       trecv,1,MPI_DOUBLE_PRECISION, &
                       tProcInfo_G%comm,error)

    o1 = sum(trecv)

  END SUBROUTINE sum_mpi_real

END MODULE ParallelSetUp
