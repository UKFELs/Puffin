! Copyright 2012-2018, University of Strathclyde
! Authors: Cynthia Nam, Pamela Aitken, and Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Cynthia Nam, Pamela Aitken, and Lawrence Campbell
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Module to deal with writing out to file.
!> @param mvar_iFreeFile holds last free file number
!> @param tErrorLog_G Custom errorlog type for error tracking

module IO

use paratype
use FileType

implicit none
 
integer(kind=ip) :: mvar_iFreeFile = 0_IP
type(cFileType), save :: tErrorLog_G

contains

!> @author
!> Cynthia Nam and Pamela Aitken
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Write Logical data to file as an integer (TRUE = 1, FALSE = 0)
!> @param[in] qLogic Logic data to be written
!> @param[in] tFileType Custom type describing the output file
!> @param[out] qOK Error flag
!> @param[in] zFormat (Optional) Format for data write
!> @param iInt Logic converted to integer (TRUE = 1, FALSE = 0)
!> @param qOKL Local error flag

  subroutine WriteLOGICINTEGER(qLogic, tFileType, qOK, zFormat)

    implicit none

    logical,         intent(in)            :: qLOGIC
    type(cFileType), intent(inout)         :: tFileType
    character(*),    intent(in), optional  :: zFormat
    logical,         intent(out)	         :: qOK

    integer(kind=ip)  :: iInt
    logical           :: qOKL

    qOK = .FALSE.

!     Convert logic -> integer

    if (qLogic) then
       iInt = 1
    else
       iInt = 0
    end if

!    Write coverted data:

    if (PRESENT(zFormat) ) then
    	 call WriteINTEGER(iInt,tFileType,qOKL,zFormat)
       if (.not. qOKL) goto 1000
    else
       call WriteINTEGER(iInt,tFileType,qOKL)
       if (.not. qOKL) goto 1000
    end if

!  Set error flag and exit         

    qOK = .true.				    
    goto 2000     

! Error Handler

1000 call Error_log('Error in DIO:WriteLOGICINTEGER',tErrorLog_G)
    print*,'Error in DIO:WriteLOGICINTEGER'
2000 continue

  end subroutine WriteLOGICINTEGER

!> @author
!> Cynthia Nam and Pamela Aitken
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Write integer data to file
!> @param[in] iInt Integer data to be written
!> @param[in] tFileType Custom type for properties of output file
!> @param[out] qOK Error flag
!> @param[in] zFormat (Optional) Format for data write
!> @param qOKL Local error flag

  subroutine WriteINTEGER(iInt, tFileType, qOK, zFormat)

    implicit none

    integer(kind=ip),intent(in)             :: iInt
    type(cFileType), intent(inout)          :: tFileType
    character(*),    intent(in),  optional  :: zFormat 
    logical,         intent(out)	      :: qOK      

    logical           :: qOKL

    qOK = .false.

!     Write data       

    if (tFileType%qFormatted) then 
      if (present(zFormat) ) then
        write (tFileType%iUnit,zFormat) (iInt)
      else
        write (tFileType%iUnit,'(I9)') (iInt)
      end if
    else
!   call C_WriteInteger(tFileType%zFileName, iInt, qOKL)
!      if (.not. qOKL) goto 1000
    end if 

!      Set error flag and exit

    qOK = .TRUE.				    
    goto 2000     

!          Error Handler

1000 call Error_log('Error in DIO:WriteINTEGER',tErrorLog_G)
    print*,'Error in DIO:WriteINTEGER'
2000 continue      

  end subroutine WriteINTEGER

!> @author
!> Cynthia Nam and Pamela Aitken
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Write integer (kind=ipl) data to file
!> @param[in] iInt Integer data to be written
!> @param[in] tFileType Custom type for properties of output file
!> @param[out] qOK Error flag
!> @param[in] zFormat (Optional) Format for data write
!> @param qOKL Local error flag

  subroutine WriteINTEGERL(iInt, tFileType, qOK, zFormat)

    implicit none
    integer(KIND=IPL),intent(in)             :: iInt
    type(cFileType), intent(inout)          :: tFileType
    character(*),    intent(in),  optional  :: zFormat 
    logical,         intent(out)	      :: qOK      

    logical           :: qOKL

    qOK = .false.

    if (tFileType%qFormatted) then 
      if (present(zFormat) ) then
        write (tFileType%iUnit,zFormat) (iInt)
      else
        write (tFileType%iUnit,'(I14)') (iInt)
      end if
    else
!      call C_WriteIntegerL(tFileType%zFileName, iInt, qOKL)
      if (.not. qOKL) goto 1000
    end if 

!      Set error flag and exit    

    qOK = .true.				    
    goto 2000     

!      Error Handler

1000 call Error_log('Error in DIO:WriteINTEGERL',tErrorLog_G)
    print*,'Error in DIO:WriteINTEGERL'
2000 continue      

  end subroutine WriteINTEGERL

!> @author
!> Cynthia Nam and Pamela Aitken
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Write integer (kind=ipn) data to file
!> @param[in] iInt Integer data to be written
!> @param[in] tFileType Custom type for properties of output file
!> @param[out] qOK Error flag
!> @param[in] zFormat (Optional) Format for data write
!> @param qOKL Local error flag

  subroutine WriteINTEGERL64(iInt, tFileType, qOK, zFormat)

    implicit none

    integer(kind=ipn),intent(in)            :: iInt
    type(cFileType), intent(inout)          :: tFileType
    character(*),    intent(in),  optional  :: zFormat 
    logical,         intent(out)	  :: qOK      

    logical           :: qOKL

!     Set error flag to false

    qOK = .false.

!          Write data       

    if (tFileType%qFormatted) then 
      if (present(zFormat) ) then
        write (tFileType%iUnit,zFormat) (iInt)
      else
        write (tFileType%iUnit,'(I14)') (iInt)
      end if
    else
!      call C_WriteIntegerL64(tFileType%zFileName, iInt, qOKL)
      if (.not. qOKL) Goto 1000
    end if 

!      Set error flag and exit

    qOK = .true.				    
    goto 2000     

! Error Handler

1000 call Error_log('Error in DIO:WriteINTEGERL64', tErrorLog_G)
    print*,'Error in DIO:WriteINTEGERL64'
2000 continue

  end subroutine WriteINTEGERL64


!> @author
!> Cynthia Nam and Pamela Aitken
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Write real (kind=wp) data to file
!> @param[in] sReal Real data to be written
!> @param[in] tFileType Custom type for properties of output file
!> @param[out] qOK Error flag
!> @param[in] zFormat (Optional) Format for data write
!> @param qOKL Local error flag

  subroutine WriteRealNumber(sReal, tFileType, qOK, zFormat)

    implicit none

    real(kind=wp),  intent(in)             :: sReal
    type(cFileType),intent(inout)          :: tFileType
    character(*),   intent(in),  optional  :: zFormat 
    logical,        intent(out)	     :: qOK      

    logical           :: qOKL

    qOK = .false.
  
    if (tFileType%qFormatted) then 
      if (present(zFormat) ) then
    	  write (tFileType%iUnit,zFormat) (sReal)
      else
        write (tFileType%iUnit,'(E22.14E3)') (sReal)
      end IF
    else
!     call C_WriteReal(tFileType%zFileName, sReal, qOKL)
      if (.not. qOKL) goto 1000
    end if 

!        Set error flag and exit         

    qOK = .true.				    
    goto 2000     

! Error Handler

1000 call Error_log('Error in DIO:WriteRealNumber',tErrorLog_G)
    print*,'Error in DIO:WriteRealNumber'
2000 continue

  end subroutine WriteRealNumber

!> @author
!> Cynthia Nam and Pamela Aitken
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Write real (kind=wp) 1D array data to file
!> @param[in] sReal Real, 1D array to be written
!> @param[in] tFileType Custom type for properties of output file
!> @param[out] qOK Error flag
!> @param[in] zFormat (Optional) Format for data write
!> @param qOKL Local error flag

  subroutine Write1DRealArray(sReal, tFileType, qOK, zFormat)

    implicit none
    real(kind=wp),  intent(in)             :: sReal(:)
    type(cFileType),intent(inout)          :: tFileType
    character(*),   intent(in),  optional  :: zFormat 
    logical,        intent(out)	     :: qOK      

    logical           :: qOKL

    qOK = .false.

    if (tFileType%qFormatted) then 
      if (present(zFormat) ) then
        write (tFileType%iUnit,zFormat) (sReal)
      else
        write (tFileType%iUnit,'(E22.14E3)') (sReal)
      end if
    else
!        Call C_WriteRealArray(tFileType%zFilename, sReal, qOKL)
      if (.not. qOKL) goto 1000
    end if 

!         Set error flag and exit

      qOK = .true.				    
      goto 2000     

!     Error Handler

1000 call Error_log('Error in DIO: Write1DRealArray',tErrorLog_G)
   print*,'Error in DIO: Write1DRealArray'
2000 continue

  end subroutine Write1DRealArray

!> @author
!> Cynthia Nam and Pamela Aitken
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Write real (kind=wp) 2D array data to file
!> @param[in] sReal Real, 2D array to be written
!> @param[in] tFileType Custom type for properties of output file
!> @param[out] qOK Error flag
!> @param[in] zFormat (Optional) Format for data write
!> @param qOKL Local error flag

  subroutine Write2DRealArray(sReal, tFileType, qOK, zFormat)

    implicit none
    real(kind=wp),  intent(in)             :: sReal(:,:)
    type(cFileType),intent(inout)          :: tFileType
    character(*),   intent(in),  optional  :: zFormat 
    logical,        intent(out)	     :: qOK      

    logical           :: qOKL

    qOK = .false.

    if (tFileType%qFormatted) then 
      if (present(zFormat) ) then
        write (tFileType%iUnit,zFormat) (sReal)
      else
        write (tFileType%iUnit,'(E22.14E3)') (sReal)
      end if
    else
!      call C_WriteRealArray(tFileType%zFileName, sReal, qOKL)
      if (.not. qOKL) goto 1000
    end if 

!      Set error flag and exit         

    qOK = .true.				    
    goto 2000     

!     Error Handler

1000 call Error_log('Error in DIO: Write2DRealArray',tErrorLog_G)
    print*,'Error in DIO: Write2DRealArray'
2000 continue
  
  end subroutine Write2DRealArray

!> @author
!> Cynthia Nam and Pamela Aitken
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Write real (kind=wp) 3D array data to file
!> @param[in] sReal Real, 3D array to be written
!> @param[in] tFileType Custom type for properties of output file
!> @param[out] qOK Error flag
!> @param[in] zFormat (Optional) Format for data write
!> @param qOKL Local error flag

  subroutine Write3DRealArray(sReal, tFileType, qOK, zFormat)

    implicit none
    real(kind=wp),  intent(in)             :: sReal(:,:,:)
    type(cFileType),intent(inout)          :: tFileType
    character(*),   intent(in),  optional  :: zFormat 
    logical,        intent(out)	     :: qOK      

    logical           :: qOKL

    qOK = .false.

    if (tFileType%qFormatted) then 
      if (present(zFormat) ) then
        write (tFileType%iUnit,zFormat) (sReal)
      else
        write (tFileType%iUnit,'(E22.14E3)') (sReal)
      end if
    else
!      call C_WriteRealArray(tFileType%zFileName, sReal, qOKL)
      if (.not. qOKL) goto 1000	
    end if 

!     Set error flag and exit         

      qOK = .true.				    
      goto 2000     

!     Error Handler

1000 call Error_log('Error in DIO: Write3DRealArray',tErrorLog_G)
    print*,'Error in DIO: Write3DRealArray'
2000 continue

  end subroutine Write3DRealArray



      FUNCTION FreeFile()
!
!****************************************************
!  Get next free unit file
!****************************************************
!
!
!====================================================
! Define local variables
!
! qOKL   - Local error flag
!
!====================================================
!
      IMPLICIT NONE
!
      INTEGER(KIND=IP)   :: FreeFile
! 
      mvar_iFreeFile=mvar_iFreeFile + 1_IP
      Select Case(mvar_iFreeFile)
         Case (0_IP,10_IP)
	    mvar_iFreeFile=mvar_iFreeFile + 1_IP
	 Case (5_IP,6_IP,7_IP)
	    mvar_iFreeFile = 8_IP
      End Select
      FreeFile = mvar_iFreeFile
!
!--------------------------------------------------------------------------------	
!  Set error flag and exit         
!--------------------------------------------------------------------------------	
!
      GoTo 2000     
!
!--------------------------------------------------------------------------------
! Error Handler
!--------------------------------------------------------------------------------
!            
1000 call Error_log('Error in DIO: FreeFile',tErrorLog_G)
   Print*,'Error in DIO: FreeFile'
2000 CONTINUE      

      End Function FreeFile
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

      SUBROUTINE OpenFileForOutput(zFileName, &
      				   tFileType, &
			           qOK)
!
!********************************************************************
! Write one dimensional data to file
!********************************************************************
!
! zFileName - INPUT    - FileName
! tFileType - UPDATE   - Properties of output file
! qOK       - OUTPUT   - Error flag
!
!====================================================================
! Define local variables
!
!=====================================================================
!	
      IMPLICIT NONE
 
      CHARACTER(*),   INTENT(IN)            :: zFileName
      TYPE(cFileType),INTENT(INOUT)         :: tFileType
      LOGICAL,        INTENT(OUT)	    :: qOK      
!
!--------------------------------------------------------------------------------	
! Local Scalars         
!--------------------------------------------------------------------------------	
!
      INTEGER      ::ios
      LOGICAL      ::qOKL
!
!--------------------------------------------------------------------------------	
! Set error flag to false         
!--------------------------------------------------------------------------------	
!
      qOK = .FALSE.
!
!--------------------------------------------------------------------------------	
! Initialise file properties type         
!--------------------------------------------------------------------------------	
!
      tFILETYPE%iUnit = FreeFile()
      tFILETYPE%qForInput = .False.
      tFILETYPE%zFileName = zFileName
!
!--------------------------------------------------------------------------------	
! Open new file        
!--------------------------------------------------------------------------------	
!
      IF (tFileType%qFormatted) Then 
         OPEN(tFileType%iUnit, &
              FILE=tFileType%zFileName, &
	      IOSTAT=ios, &
	      STATUS='REPLACE', &
	      ACCESS='SEQUENTIAL', &
	      ACTION='WRITE', &
	      POSITION='REWIND')
!	  
         If (ios /= 0_IP) Goto 1000
!
!--------------------------------------------------------------------------------	
! Open new binary file        
!--------------------------------------------------------------------------------	
!
      ELSE
      !   Call C_OpenFileForOutput(zFileName, qOKL)
	 If (.NOT. qOKL) Goto 1000  
      END IF
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
1000 call Error_log('Error in DIO: OpenFileForOutput',tErrorLog_G)
   Print*,'Error in DIO: OpenFileForOutput'
2000 CONTINUE			  
      END SUBROUTINE OpenFileForOutput
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

      SUBROUTINE CloseFile(tFILETYPE, &
                           qOK)
!
!****************************************************
!  Close the file
!****************************************************
!
! A tFILETYPE - INPUT   - File description
! A qOK       - OUTPUT - Error flag
!
!====================================================
! Define local variables
!
!====================================================
!
      IMPLICIT NONE
! 
      TYPE(cFileType),INTENT(INOUT)           :: tFileType
      LOGICAL,       INTENT(OUT)	      :: qOK      
!
      IF (tFileType%qFormatted) Then 
         CLOSE(tFileType%iUnit, STATUS='KEEP')
      END IF
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
1000 call Error_log('Error in DIO: CloseFile',tErrorLog_G)
   Print*,'Error in DIO: CloseFile'
2000 CONTINUE	
End SubRoutine CloseFile


!--------------------------------------------------------------------------------

 SUBROUTINE Error_log(zError,tFileType)
!
!*************************************************
! Write Error log
!*************************************************
!
! zError    - INPUT     - Error message
! tFileType - UPDATE    - Properties of output file
!
! Define local variables
!
! ios - Input/ output status
! qOKL - Local error flag
!
      use ParallelInfoType
      IMPLICIT NONE
!
      CHARACTER(*), INTENT(IN)       :: zError
      TYPE(cFileType), INTENT(INOUT) :: tFileType
! Local Scalars
      LOGICAL                        :: qOKL

      IF (tProcInfo_G%qROOT) Then

! Check if error log file initialised
! OpenFileForOutput subroutine starts on line 474 in this file
!
         IF (tFileType%iUnit == 0_IP) THEN
!
            call OpenFileForOutput(tFileType%zFileName, &
      			           tFileType, &
			           qOKL)
!			       
 	    If (.NOT. qOKL) Goto 1000  
         ELSE
!
            call OpenFileForAppend(tFileType%zFileName, &
      			           tFileType, &
			           qOKL)
!			       
 	    If (.NOT. qOKL) Goto 1000  
!
         END IF
!
! Write Error
!
         WRITE(UNIT=tFileType%iUnit,FMT='(A)') zError
!
! Close file
         call CloseFile(tFileType,qOKL)
         If (.NOT. qOKL) Goto 1000 
      End If
! Exit function
!      
      Goto 2000
! Error Handler
!

1000  Print*,'Error setting up error log: critical error'
      Stop

2000  Continue
	
END SUBROUTINE Error_log

!--------------------------------------------------------------------------------

      SUBROUTINE OpenFileForAppend(zFileName, &
      				   tFileType, &
			           qOK)
!
!********************************************************************
! Open file to append data
!********************************************************************
!
! zFileName - INPUT    - FileName
! tFileType - UPDATE   - Properties of output file
! qOK       - OUTPUT   - Error flag
!
!====================================================================
! Define local variables
!
!=====================================================================
!	
      IMPLICIT NONE
 
      CHARACTER(*),   INTENT(IN)            :: zFileName
      TYPE(cFileType),INTENT(INOUT)         :: tFileType
      LOGICAL,        INTENT(OUT)	    :: qOK      
!
!--------------------------------------------------------------------------------	
! Local Scalars         
!--------------------------------------------------------------------------------	
!
      CHARACTER*12 :: zForm
      INTEGER      :: ios
      LOGICAL               :: qOKL
!
!--------------------------------------------------------------------------------	
! Set error flag to false         
!--------------------------------------------------------------------------------	
!
      qOK = .FALSE.
!
!--------------------------------------------------------------------------------	
! Check if file initialised   
! OpenFileForOutput subroutine starts on line 474 in this file
!--------------------------------------------------------------------------------	
!
      IF (tFileType%iUnit == 0_IP) THEN
!
         call OpenFileForOutput(zFileName, &
      			       tFileType, &
			       qOKL)
 	 If (.NOT. qOKL) Goto 1000  
!
      END IF
!
!--------------------------------------------------------------------------------	
! Initialise file properties type         
!--------------------------------------------------------------------------------	
!
      tFILETYPE%qAppend   = .TRUE.
!
      IF (tFILETYPE%qFormatted) THEN
         zForm = 'FORMATTED'
      ELSE
         zForm = 'UNFORMATTED'
      END IF
!
!--------------------------------------------------------------------------------	
! Open old file for append
!--------------------------------------------------------------------------------	
!
      IF (tFileType%qFormatted) Then 

         OPEN(tFileType%iUnit, &
              FILE=tFileType%zFileName, &
	      IOSTAT=ios, &
	      STATUS='OLD', &
    	      ACCESS='SEQUENTIAL', &
	      ACTION='WRITE', &
	      POSITION='APPEND', &
	      FORM = zForm)
!	   	   
         If (ios /= 0_IP) Goto 1000
      END IF
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
1000 call Error_log('Error in DIO: OpenFileForAppend',tErrorLog_G)
   Print*,'Error in DIO: OpenFileForAppend'
2000 CONTINUE
			  
      END SUBROUTINE OpenFileForAppend
!--------------------------------------------------------------------------------
!-------------------------------------------------------------------------------- 

      subroutine FileNameNoExtension(zFileName, zFile, qOK)

! Return filename with extension removed ie '.*' removed 
!
! zFileName     - INPUT    - FileName
! zFile         - OUTPUT   - File no extension
! qOK           - OUTPUT   - Error flag
!
! Define local variables
	
      implicit none
 
      character(*),   intent(in)  :: zFileName
      character(*),   intent(out) :: zFile
      logical,        intent(out)  :: qOK      

! Local Scalars         

      integer(kind=ip)      :: iExtension

! Set error flag to false

      qOK = .FALSE.

! Find last position of '.' in filename

      iExtension = INDEX(zfilename,'.',.TRUE.)

! Return filename

      Select Case (iExtension)
          Case (0)
            zFile = zFileName
          Case (1)
  	        zFile = ''
  	      Case (2:)
  	        zFile = zFileName(1:iExtension-1)
          Case Default
            zFile = zFileName
      End Select

! Remove leading and trailing blanks

      zFile = TRIM(ADJUSTL(zFile))
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
1000 call Error_log('Error in DIO: OpenFileForAppend',tErrorLog_G)
   Print*,'Error in DIO: OpenFileForAppend'
2000 CONTINUE
			  
      END SUBROUTINE FileNameNoExtension
!--------------------------------------------------------------------------------

!> @author
!> Cynthia Nam
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Return filename extension ie '.*' section
!> @param[in] zFileName FileName
!> @param[out] zFile File extension
!> @param[out] qOK Error flag

      subroutine FileNameExtension(zFileName, zFile, qOK)
  
      implicit none
 
      character(*),   intent(in)  :: zFileName
      character(:), allocatable,  intent(out) :: zFile
      logical,        intent(out)  :: qOK      

! Local Scalars         

      integer(kind=ip)      :: iExtension

! Set error flag to false

      qOK = .FALSE.

! Find last position of '.' in filename

      iExtension = INDEX(zfilename,'.',.TRUE.)

! Return filename

      Select Case (iExtension)
          Case (0)
            zFile = zFileName
          Case (1)
            zFile = zFileName(iExtension:len(zFileName))
          Case (2:)
            zFile = zFileName(iExtension:len(zFileName))
          Case Default
            zFile = zFileName
      End Select

! Remove leading and trailing blanks

      zFile = TRIM(ADJUSTL(zFile))
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
1000 call Error_log('Error in DIO: OpenFileForAppend',tErrorLog_G)
   Print*,'Error in DIO: OpenFileForAppend'
2000 CONTINUE
        
      END SUBROUTINE FileNameExtension
!--------------------------------------------------------------------------------





END MODULE IO
