!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!



module createSDDS


use paratype
use arrayfunctions
use CIOWrapper
use IO
use ParallelInfoType
use sddsSetup


contains

! was SetUpDataFiles

!   subroutine CreateSDDSFiles(zDataFileName,  &
!                              qFormattedFiles, &
!                              tWriteZData,    &
!                              tWriteAData, &
!                              tArraySegment,  &
!                              qOK,            &
!                              zOptionalString)

! ! Open data files
! !
! ! zDataFileName           - INPUT    - Data file name
! ! qFormattedFiles         - INPUT    - if output data files to be formatted or binary
! ! tArraySegment           - UPDATE   - Result data file info
! ! qOK                     - OUTPUT   - Error flag

!     IMPLICIT NONE

!     character(32_IP),        INTENT(IN)                :: zDataFileName
!     logical,                 INTENT(IN)                :: qFormattedFiles
!     TYPE(cArraySegment),     INTENT(INOUT)             :: tWriteZData
!     TYPE(cArraySegment),     INTENT(INOUT)             :: tWriteAData(:)
!     TYPE(cArraySegment),     INTENT(INOUT)             :: tArraySegment(:)
!     LOGICAL,                 INTENT(OUT)               :: qOK      
!     character(*),            INTENT(IN), OPTIONAL      :: zOptionalString

! ! Define local variables
! ! 
! ! iSegment - Local loop variable
! ! zFileName - Filename to use 
! ! qOKL   - Local error flag

!     INTEGER(KIND=IP)               :: iSegment
!     character(32_IP)               :: zFileName
!     LOGICAL                        :: qOptional
!     LOGICAL                        :: qOKL

! !     Set error flag to false         
! !     This type is defined is "ParallelInfoType.f90"

!     qOK = .false.    

!     qOptional = .false.



!     if (present(zOptionalString)) then

!       if (len(trim(adjustl(zOptionalString))) > 0) then

!         qOptional = .TRUE.
    
!       end if
  
!     end if


! !     ''tArraySegment'' type is defined in "AMathLibGlobals.f90"

! !     Loop over allsegments and open data file if required

!     do iSegment = 1_IP, size(tArraySegment)



! !     If to write data open file

!       if (tArraySegment(iSegment)%qWrite) Then

! !     Create filename

! !     Pamela: need to put something sensible here

!         zFilename = (trim(adjustl(tArraySegment(iSegment)%zVariable)) // trim(adjustl(zDataFileName)) )

!         if (qOptional) then
!           zFilename = (trim(adjustl(zOptionalString)) // '_' // trim(adjustl(zFilename)) )
!         end if

! !     Open file - This subroutine is in this file - line 517

!         if (tProcInfo_G%qRoot) then
      
!           call CreateSDDSFile(zFilename, qFormattedFiles, &
!                               tArraySegment(iSegment)%zVariable, &
!                               tArraySegment(iSegment)%tFileType, &
!                               qOKL)
!           If (.NOT. qOKL) Goto 1000

!         end if

!         call shareFileType(tArraySegment(iSegment)%tFileType)

!       end if


!     end do



! !      Field Data files

!     if (tWriteAData(iRe_A_CG)%qWrite) then
      
!       if (tProcInfo_G%qRoot) then

! !     Create filename      

! !     Pamela: need to put something sensible here

!         zFilename = (trim(adjustl(tWriteAData(iRe_A_CG)%zVariable)) // trim(adjustl(zDataFileName))) 

!         if (qOptional) then

!           zFilename = (trim(adjustl(zOptionalString)) // '_' // trim(adjustl(zFilename)) )

!         end if

! !     Open file - This subroutine is in this file - line 517

!         call CreateSDDSFile(zFilename, qFormattedFiles, &
!                             tWriteAData(iRe_A_CG)%zVariable, &
!                             tWriteAData(iRe_A_CG)%tFileType, &
!                             qOKL)

!         zFilename = (trim(adjustl(tWriteAData(iIm_A_CG)%zVariable)) // trim(adjustl(zDataFileName)))

!         if (qOptional) then
!           zFilename = (trim(adjustl(zOptionalString)) // '_' // trim(adjustl(zFilename)) )
!         end if


!         call CreateSDDSFile(zFilename, qFormattedFiles,&
!                             tWriteAData(iIm_A_CG)%zVariable, &
!                             tWriteAData(iIm_A_CG)%tFileType, &
!                             qOKL)

!       end if

!     end if



! !     z Data file         

!     if (tWriteZData%qWrite) then

!       zFilename = (trim(adjustl(tWriteZData%zVariable)) // trim(adjustl(zDataFileName))) 
      

!       if (qOptional) then
      
!         zFilename = (trim(adjustl(zOptionalString)) // '_' // trim(adjustl(zFilename)) )
      
!       end if



!       if (tProcInfo_G%qRoot) then

!         call CreateSDDSFile(zFilename, qFormattedFiles,  &
!                             tWriteZData%zVariable, &
!                             tWriteZData%tFileType, &
!                             qOKL)
!         if (.not. qOKL) goto 1000
      
!       end if


!     end if

! !     Set error flag and exit

!     qOK = .TRUE.
!     GoTo 2000

! !     Error Handler
           
! 1000 call Error_log('Error in EArrayFunctions:SetUpDataFiles',tErrorLog_G)
!     Print*,'Error in EArrayFunctions:SetUpDataFiles'
! 2000 continue

!   end subroutine CreateSDDSFiles















! was SetUpDataFile


  subroutine CreateSDDSFile(zDataFileName,  &
                            zVariable,      &
                            tYDataFile,     &
                            qOK)

    implicit none

! Set up data file to receive information

! zDataFileName           - INPUT    - Data file name
! qFormattedFiles         - INPUT    - if output data files to be formatted or binary
! zVariable               - INPUT    - Variable to write
! tYDataFile              - UPDATE   - Result data file info
! qOK                     - OUTPUT   - Error flag




    character(32_IP),  intent(in)      :: zDataFileName
    character(32_IP),  intent(in)      :: zVariable
    type(cFileType),   intent(inout)   :: tYDataFile
    logical,           intent(out)     :: qOK      

! Define local variables

!     qOKL   - Local error flag
       
    logical :: qOKL



!     Set error flag to false         

    qOK = .false.    



!     Open the file to receive data output - This subroutine is in "IO.f90"

!    tYDataFile%qFormatted = qFormattedFiles   ! Redundant now....

    call InitBasicSDDSFile(zDataFileName, &
                            tYDataFile, &
                            qOKL)
    if (.not. qOKL) goto 1000



!     Write variables names that are going to be written to the files (in order of output)  
!     This subroutine is in "GSddsWriter.f90"

    call SddsWriteColumn(zVariable,'double',tFileType=tYDataFile)    



!     Write data mode - This subroutine is in "GSddsWriter.f90"

    if (tYDataFile%qFormatted) then

      call SddsWriteDataMode('ascii',tFileType=tYDataFile)     

    else

      call SddsWriteDataMode('binary',tFileType=tYDataFile)    

    end if
      
!     Close the file

    call CloseFile(tYDataFile,qOKL)
    if (.not. qOKL) goto 1000



!     Set error flag and exit

    qOK = .TRUE.            
    GoTo 2000     

!     Error Handler
            
1000 call Error_log('Error in EArrayFunctions:SetUpDataFile',tErrorLog_G)
    Print*,'Error in EArrayFunctions:SetUpDataFile'

2000 continue
        
  end subroutine CreateSDDSFile

















  subroutine CloseDataFiles(tArraySegment, qOK)

    implicit none

! close data files
!
! tArraySegment           - UPDATE   - Result data file info
! qOK                     - OUTPUT   - Error flag
! 

    type(cArraySegment), intent(inout) :: tArraySegment(:)
    logical,             intent(out)   :: qOK      


! Define local variables
! 
! iSegment - Local loop variable
! qOKL   - Local error flag

    integer(kind=ip)   :: iSegment
    logical            :: qOKL




!     Set error flag to false         

    qOK = .FALSE.    



!     Close all required files - ''CloseFile'' subroutine is in "IO.f90"      

    if (tProcInfo_G%qROOT) then

      do iSegment = 1_IP, size(tArraySegment)
      
        if (tArraySegment(iSegment)%qWrite) then

          Call CloseFile(tArraySegment(iSegment)%tFileType,qOKL)
          if (.not. qOKL) goto 1000

        end if

      end do

    end if





!     Set error flag and exit         

    qOK = .TRUE.            
    goto 2000     

!     Error Handler

1000 call Error_log('Error in EArrayFunctions:CloseDataFiles',tErrorLog_G)
    print*,'Error in EArrayFunctions:CloseDataFiles'

2000 continue

        
  end subroutine CloseDataFiles



end module createSDDS
