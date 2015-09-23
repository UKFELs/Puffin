module sddsROutput


use paratype
use filetype
use IO
use SddsWriter
use globals
use sddsSetup

implicit none


INTERFACE OutputIntegrationData
   MODULE PROCEDURE OutputIntegrationData_RealValue, &
        OutputIntegrationData_RealArray, &
        OutputIntegrationData_ParRealArray   
END INTERFACE



contains

  subroutine OutputIntegrationData_RealArray(tFileType,sY,&
                                             iLenY,qOK)

! Output Integration data to file 
!
! tFileTypeY              - INPUT    -File type properties
! sY                      - INPUT    - Result Y 
! iLenY                   - INPUT    - Length of Y
! qOK                     - OUTPUT   - Error flag
!

    implicit none

    type(cFileType),         intent(inout)   :: tFileType
    real(kind=wp),           intent(in)      :: sY(:)
    integer(kind=ip),        intent(in)      :: iLenY
    logical,                 intent(out)     :: qOK    

!     Define local variables

    logical                        :: qOKL

!     Set error flag to false

    qOK = .FALSE.

!     Open the file - In CIO.f90 line 862

    call OpenFileForAppend(tFileType%zFileName, &
                           tFileType, qOKL)
    if (.NOT. qOKL) Goto 1000

!     Set up new page - see CIO.f90 line 651 

    call WriteSDDSNewPage(tFileType,qOKL)
    if (.NOT. qOKL) Goto 1000

!     Write length of column data - see CIO.f90 line 100

    call WriteINTEGER(iLenY,tFileType,qOKL)
    if (.NOT. qOKL) Goto 1000  

!     Write real part - see CIO.f90 line 232        

    call Write1DRealArray(sY,tFileType,qOKL)
    if (.NOT. qOKL) Goto 1000

!     Close the file - see CIO.f90 line 560

    call CloseFile(tFileType, qOKL)
    if (.NOT. qOKL) Goto 1000



!     Set error flag and exit         

    qOK = .TRUE.				    
    GoTo 2000     



!     Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call Error_log('Error in sddsOutput:OutputIntegrationData_RealArray',tErrorLog_G)
    Print*,'Error in sddsOutput:OutputIntegrationData_RealArray'

    call CloseFile(tFileType, qOKL)

2000 continue


  end subroutine OutputIntegrationData_RealArray     






  subroutine OutputIntegrationData_ParRealArray(tFileType, &
                                                sY, rank,   &
                                                iLenY, qOK)

! Output Integration data to file 
!
! tFileTypeY              - INPUT    -File type properties
! sY                      - INPUT    - Result Y 
! iLenY                   - INPUT    - Length of Y
! qOK                     - OUTPUT   - Error flag

    implicit none

    type(cFileType),    intent(inout)   :: tFileType
    real(kind=wp),      intent(in)      :: sY(:)
    integer(kind=ip),   INTENT(in)      :: rank
    integer(kind=ipl),  INTENT(in)      :: iLenY
	  logical,            INTENT(out)     :: qOK      

! Local Scalars         
!
! Define local variables

    LOGICAL  :: qOKL
	  
    REAL(KIND=WP), DIMENSION(:),ALLOCATABLE  ::  sendbuff, &
                                                   recvbuff
    INTEGER  ::  error, i, req, stat
    INTEGER(KIND=IP)  ::  mpifiletype

!     Set error flag to false         
      
    qOK = .FALSE.

! CALL GetMPIFileType(tFileType,mpifiletype)

!     Open the file:- this only need to be done on one processor - see CIO.f90 line 862
    
    IF (rank==0) THEN
	
      call OpenFileForAppend(tFileType%zFileName, &
                             tFileType, qOKL)
      if (.NOT. qOKL) Goto 1000

!     Set up new page - see CIO.f90 line 651        

      call WriteSDDSNewPage(tFileType,qOKL)
      if (.NOT. qOKL) Goto 1000

!     Write length of column data - see CIO.f90 line 100          

      call WriteINTEGERL(iLenY,tFileType,qOKL)
      if (.NOT. qOKL) Goto 1000

!     Close File 

      call CloseFile(tFileType, qOKL)
      If (.NOT. qOKL) Goto 1000
		
    end if   
	
!     Synchronize processors

    call MPI_BARRIER(tProcInfo_G%comm, error)
	
!     File data was setup on process 0, need to share filetype with the
!     rest of the processors in the MPI communicator 
    
    call shareFileType(tFileType)

!     Cycle through processes and write data one by one - see CIO.f90 line 232

    do i = 0,tProcInfo_G%size-1

    if (rank == i) then

      if (procelectrons_G(1) > 0) then

        call OpenFileForAppend(tFileType%zFileName, &
                               tFileType, qOKL)

        call Write1DRealArray(sY,tFileType,qOKL)
        if (.not. qOKL) Goto 1000

        call CloseFile(tFileType, qOKL)

      end if

    end if

!     Synchronize

		CALL MPI_BARRIER(tProcInfo_G%comm, error)
    END DO	

!     Set error flag and exit         

      qOK = .TRUE.				    
      GoTo 2000   
	    
!     Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call Error_log('Error in sddsOutput:OutputIntegrationData_RealArray',tErrorLog_G)
      Print*,'Error in sddsOutput:OutputIntegrationData_RealArray'
      call CloseFile(tFileType, &
                     qOKL)
2000 CONTINUE

  end subroutine OutputIntegrationData_ParRealArray     





  subroutine OutputIntegrationData_RealValue(tFileType, sY, qOK)

! Output Integration data to file 
!
! tFileType               - INPUT    -File type properties
! sY                      - INPUT    - Result Y 
! qOK                     - OUTPUT   - Error flag

    implicit none

    type(cFileType), intent(inout)   :: tFileType
    real(kind=wp),   intent(in)      :: sY
    logical,         intent(out)     :: qOK      

! Local Scalars         

    logical  :: qOKL



!     Set error flag to false         

    qOK = .FALSE.
  
!     Open the file - In CIO.f90 line 862

    call OpenFileForAppend(tFileType%zFileName, &
                           tFileType, qOKL)
    If (.NOT. qOKL) Goto 1000
  
!     Set up new page - see CIO.f90 line 651

    call WriteSDDSNewPage(tFileType,qOKL)
    If (.NOT. qOKL) Goto 1000

!     Write length of column data - see CIO.f90 line 100

    call WriteINTEGER(1_IP,tFileType,qOKL)
    If (.NOT. qOKL) Goto 1000

!     Write real part - see CIO.f90 line 166          

    call WriteRealNumber(sY,tFileType,qOKL)
    If (.NOT. qOKL) Goto 1000
 
!     Close the file - see CIO.f90 line 560

    call CloseFile(tFileType, qOKL)
    If (.NOT. qOKL) Goto 1000

!     Set error flag and exit

    qOK = .TRUE.
    GoTo 2000

!     Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call Error_log('Error in sddsOutput:OutputIntegrationData_RealValue',tErrorLog_G)
      Print*,'Error in sddsOutput:OutputIntegrationData_RealValue'

      call CloseFile(tFileType, qOKL)

2000 CONTINUE

  end subroutine OutputIntegrationData_RealValue


end module sddsROutput