!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

Module ParallelSetUp

! Module containing routines to setup and deallocate MPI processes,
! and handle MPI communication in Puffin.
!
! -Lawrence Campbell
!  University of STrathclyde
!  14/06/2012
!

USE paratype
USE ParallelInfoType
USE IO

IMPLICIT NONE

INCLUDE 'mpif.h'

INTEGER :: MPI_INT_HIGH

CONTAINS


SUBROUTINE  InitializeProcessors(tProcInfo, &
				       qOk)

  IMPLICIT NONE

! Initialize and retrieve info on the MPI processes
!
!               ARGUMENTS
!
! tProcInfo    OUTPUT    Custom type to hold MPI info
! q0k          OUTPUT    Error flag for Puffin

  TYPE(cParallelInfoType), INTENT(OUT)	   :: tProcInfo
  LOGICAL,                 INTENT(OUT)         :: qOk      

!              Local Vars
!
! error - error flag for MPI

  INTEGER(KIND=IP)    :: error


!     Begin

  qOK = .FALSE.

!     Initialize MPI
      
  CALL MPI_INIT(error)

!     Define comm as the global communicator.
	      
  tProcInfo%comm = MPI_COMM_WORLD

!     Get local data on processor numbers. Rank is local number in the 
!     global communicator, size is the total number of processors
	            
  CALL MPI_COMM_RANK(tProcInfo%comm, tProcInfo%rank, error)
			 
  CALL MPI_COMM_SIZE(tProcInfo%comm, tProcInfo%size, error) 

!     Define root processor as rank 0.

  IF (tProcInfo%rank==0) THEN
    tProcInfo%qRoot = .TRUE.
  END IF

!     Define MPI type for high precision integers

  CALL MPI_TYPE_CREATE_F90_INTEGER(14,MPI_INT_HIGH,error)

!     Set error flag and exit
      
  qOK = .TRUE.				    
  GoTo 2000
      
! Error Handler
            
1000 CALL Error_log('Error in ParallelSetUp:DefineParallelLibrary', &
                    tErrorLog_G)
  
  PRINT*,'Error in ParallelSetUp:DefineParallelLibrary'
       
2000 CONTINUE
  
END SUBROUTINE InitializeProcessors

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	
      SUBROUTINE Get_time(in_time)
	  
      IMPLICIT NONE
	  
	  REAL(KIND=WP)	::	in_time
	  
	  INTEGER(KIND=IP)	::	error
	  
	  in_time = MPI_Wtime(error)
	  
	  END SUBROUTINE Get_time

!------------------------------------------------

      SUBROUTINE  UnDefineParallelLibrary(qOK)
!
!********************************************************************
! Define the parallel library grid
! Set up parallel processors
!********************************************************************
!
! qOK                     - OUTPUT   - Error flag
!
!********************************************************************
!	
      IMPLICIT NONE
!
      LOGICAL,                 INTENT(OUT)         :: qOK 
      
      
      
      INTEGER(KIND=IP)		:: error     
!
!--------------------------------------------------------------------------------	
! Set error flag to false         
!--------------------------------------------------------------------------------	
!
      qOK = .FALSE.    
!
!--------------------------------------------------------------------------------	
! UnInitialise library
!--------------------------------------------------------------------------------	
! 
      call MPI_FINALIZE(error)
!
!--------------------------------------------------------------------------------	
!  Set error flag and exit         
!--------------------------------------------------------------------------------	
!
      qOK = .TRUE.				    
      GoTo 2000     
!
!--------------------------------------------------------------------------------
! Error Handler
!--------------------------------------------------------------------------------
!            
1000 call Error_log('Error in ParallelSetUp:UnDefineParallelLibrary',tErrorLog_G)
       Print*,'Error in ParallelSetUp:UnDefineParallelLibrary'
2000 CONTINUE
      
      END SUBROUTINE UnDefineParallelLibrary
      

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

      SUBROUTINE  StopCode(qOK)
!
!********************************************************************
! Stop code runing
!********************************************************************
! qOK                     - OUTPUT   - Error flag
!
!********************************************************************
!	
      IMPLICIT NONE
!
      LOGICAL,                 INTENT(OUT)         :: qOK      
!
!====================================================================
! Define local variables
! 
! qOKL  - Error flag 
!=====================================================================
!	
    	LOGICAL           :: qOKL
        INTEGER(KIND=IP)		:: error
!
!--------------------------------------------------------------------------------	
! Set error flag to false         
!--------------------------------------------------------------------------------	
!
      qOK = .FALSE.    
!
!--------------------------------------------------------------------------------	
! UnInitialise library
!--------------------------------------------------------------------------------	
! 
      call UnDefineParallelLibrary(qOKL)
      If (.NOT. qOKL) Goto 1000
!
!--------------------------------------------------------------------------------	
! Stop
!--------------------------------------------------------------------------------	
!
      Stop
!
!--------------------------------------------------------------------------------	
!  Set error flag and exit         
!--------------------------------------------------------------------------------	
!
      qOK = .TRUE.				    
      GoTo 2000     
!
!--------------------------------------------------------------------------------
! Error Handler
!--------------------------------------------------------------------------------
!            
1000 call Error_log('Error in ParallelSetUp:StopCode',tErrorLog_G)
      Print*,'Error in ParallelSetUp:StopCode'
2000 CONTINUE
	      
      END SUBROUTINE StopCode

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

REAL(KIND=WP),INTENT(INOUT)  ::  loc_arr(:)
INTEGER(KIND=IP),INTENT(IN)  ::  nvals
INTEGER(KIND=IP),INTENT(IN)  ::  root

INTEGER(KIND=IP)  ::  error

 IF (tProcInfo_G%qRoot) THEN

  CALL MPI_REDUCE(MPI_IN_PLACE,loc_arr,nvals,MPI_DOUBLE_PRECISION,&
 			 MPI_SUM,root,MPI_COMM_WORLD,error)

 ELSE
 
  CALL MPI_REDUCE(loc_arr,loc_arr,nvals,MPI_DOUBLE_PRECISION,&
 			 MPI_SUM,root,MPI_COMM_WORLD,error) 
 
 END IF

END SUBROUTINE sum2RootArr

!======================================================================

SUBROUTINE GetMPIfiletype(filetype,mpifiletype)

TYPE(cFileType),INTENT(IN)	::	filetype
INTEGER(KIND=IP),INTENT(OUT)  ::  mpifiletype

INTEGER(KIND=IP) ::  error
INTEGER(KIND=IP) ::  arrayoftypes(8)
INTEGER(KIND=IP) ::  arrayofdisps(8)
INTEGER(KIND=IP) ::  arrayofblocklengths(8)
INTEGER(KIND=IP) ::  startaddress,address

arrayoftypes(1)=MPI_CHARACTER
arrayoftypes(2)=MPI_LOGICAL
arrayoftypes(3)=MPI_INTEGER

arrayofblocklengths(1)=LEN(fileType%zFileName)
arrayofblocklengths(2)=3
arrayofblocklengths(3)=4

 CALL MPI_ADDRESS(filetype%zFileName,startaddress)
 CALL MPI_ADDRESS(filetype%qFormatted,address)
arrayofdisps(1)=0
arrayofdisps(2)=address-startaddress
 CALL MPI_ADDRESS(filetype%iPos,address)
arrayofdisps(3)=address-startaddress

 CALL MPI_TYPE_STRUCT(3,arrayofblocklengths,&
 					  arrayofdisps,arrayoftypes,&
					  mpifiletype,error)

 CALL MPI_TYPE_COMMIT(mpifiletype,error)
   
END SUBROUTINE GetMPIfileType

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

END MODULE ParallelSetUp
