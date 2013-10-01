MODULE RESUME

  USE paratype
  USE typesandconstants
  USE DerivsGlobals
  USE ParallelInfoType
  USE DataWrite

  IMPLICIT NONE

  CONTAINS

  SUBROUTINE initFD(sV,sA,sZ,qOK)

! Subroutine to read in the dump files produced,
! so in event of system failure can resume from 
! last dump files
!
! -Lawrence

!         ARGUMENTS

  REAL(KIND=WP), ALLOCATABLE, INTENT(OUT) :: sV(:), sA(:)
  REAL(KIND=WP), INTENT(OUT) :: sZ
  LOGICAL, INTENT(OUT)  ::  qOK

!         LOCAL ARGS:-

  INTEGER(KIND=IP) :: sendbuff,recvbuff,statr,req,lrank,rrank
  INTEGER(KIND=IP) :: i
  LOGICAL :: qOKL
  INTEGER :: error

!     Set error flag

  qOK = .FALSE.
  
!     Read in the number of macroparticles in each local process

  CALL READNELEC(tProcInfo_G%rank,iNumberElectrons_G)

!     Sum the number of local macroparticles to get the total number of 
!     macroparticles in the system
   
  CALL MPI_ALLREDUCE(iNumberElectrons_G,iGloNumElectrons_G,&
               1, MPI_INTEGER,MPI_SUM,tProcInfo_G%comm,error)
   
  DEALLOCATE(s_chi_bar_G,s_Normalised_chi_G)

  ALLOCATE(s_chi_bar_G(iNumberElectrons_G),&
         s_Normalised_chi_G(iNumberElectrons_G))

  ALLOCATE(sV(nElectronEquations_CG*iNumberElectrons_G), &
           sA(nFieldEquations_CG*iNumberNodes_G))
  
  CALL READINCHIDATA(s_chi_bar_G,s_Normalised_chi_G,tProcInfo_G%rank)
    
  IF (tProcInfo_G%qRoot) PRINT*, 'RESUMING, reading in previous data'

  CALL READDUMP(sA,sV,tProcInfo_G%rank,NX_G*NY_G*NZ2_G,&
      iNumberElectrons_G,sz,start_step,&
      tArrayA(1)%tFileType%iPage)

!     Pass around most recent page numbers for SDDS
  
  tArrayA(2)%tFileType%iPage = tArrayA(1)%tFileType%iPage
  tArrayE(1)%tFileType%iPage = tArrayA(1)%tFileType%iPage
  tArrayE(2)%tFileType%iPage = tArrayA(1)%tFileType%iPage
  tArrayE(3)%tFileType%iPage = tArrayA(1)%tFileType%iPage
  tArrayE(4)%tFileType%iPage = tArrayA(1)%tFileType%iPage
  tArrayE(5)%tFileType%iPage = tArrayA(1)%tFileType%iPage
  tArrayE(6)%tFileType%iPage = tArrayA(1)%tFileType%iPage
  tArrayZ%tFileType%iPage = tArrayA(1)%tFileType%iPage
  
  start_step = start_step+1_IP
	
!tArrayE(:)%tFileType%iFileLength=iNumberElectrons_G

!    Set up arrays of pointers to locations of data

  CALL SetUpElectronArray(tArrayE,tArrayA,iNumberElectrons_G, &
         iNumberNodes_G,qOKL)
  IF (.NOT. qOKL) Goto 1000

!     Define processes to the left and right, for 
!     a ring communication

  IF (tProcInfo_G%rank == tProcInfo_G%size-1) THEN
    
    rrank = 0
    lrank = tProcInfo_G%rank-1
       
  ELSE IF (tProcInfo_G%rank==0) THEN
    
    rrank = tProcInfo_G%rank+1
    lrank = tProcInfo_G%size-1
       
  ELSE
    
    rrank = tProcInfo_G%rank+1
    lrank = tProcInfo_G%rank-1
       
  END IF

  procelectrons_G(1) = iNumberElectrons_G
  
  sendbuff = iNumberElectrons_G
  recvbuff = iNumberElectrons_G

!     Send local macroparticle numbers to define Allgather arrays
    
  DO i=2,tProcInfo_G%size
    
    CALL MPI_ISSEND( sendbuff,1,MPI_INTEGER,rrank,0,tProcInfo_G%comm,req,error )
    CALL MPI_RECV( recvbuff,1,MPI_INTEGER,lrank,0,tProcInfo_G%comm,statr,error )
    CALL MPI_WAIT( req,statr,error )	  
  
    procelectrons_G(i) = recvbuff
    
    sendbuff=recvbuff
      
  END DO

  PRINT*, 'resuming from step ', start_step

!     Set error flag to success and exit

  qOK = .TRUE.
  
  GOTO 2000

1000 CALL Error_log('Error in setupcalcs:GetInnerNodes',tErrorLog_G)

2000 CONTINUE

  END SUBROUTINE initFD


END MODULE RESUME
