! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

MODULE IO

! Module to deal with writing out to file
!--------------------------------------------------------------------------------

      USE paratype
      USE ParallelInfoType
      USE FileType
!      USE SddsWriter
!      USE CIOWrapper

      IMPLICIT NONE

! Define local module variables
! mvar_iFreeFile   - holds last free file number
!--------------------------------------------------------------------------------
 
      INTEGER(KIND=IP)                :: mvar_iFreeFile = 0_IP
!Save ensures that the errorlog type preserves its value
      TYPE(cFileType),SAVE         :: tErrorLog_G

      CONTAINS
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

      SUBROUTINE WriteLOGICINTEGER(qLogic,        &
			      tFileType,    &
			      qOK,          &
			      zFormat)
!
!********************************************************************
! Write Logical data as INTEGER(KIND=IP) data to file
!********************************************************************
!
! qLogic    - INPUT    - Logic data (TRUE = 1, FALSE = 0)
! tFileType - INPUT    - Properties of output file
! qOK       - OUTPUT   - Error flag
! zFormat   - OPTIONAL - Format for data write
!
!====================================================================
! Define local variables
! iInt  - Logic as integer
! qOKL  - Local Error flag
!
!=====================================================================
!	
      IMPLICIT NONE
      LOGICAL,         INTENT(IN)             :: qLOGIC
      TYPE(cFileType), INTENT(INOUT)          :: tFileType
      CHARACTER(*),    INTENT(IN),  OPTIONAL  :: zFormat 
      LOGICAL,         INTENT(OUT)	      :: qOK     
      
      INTEGER(KIND=IP)  :: iInt 
      LOGICAL           :: qOKL
!
!--------------------------------------------------------------------------------	
! Set error flag to false         
!--------------------------------------------------------------------------------	
!
      qOK = .FALSE.
!
!--------------------------------------------------------------------------------	
! Get logic variable as integer     
!--------------------------------------------------------------------------------	
!
      If (qLogic) Then
         iInt = 1
      Else
         iInt = 0
      End If
!
!--------------------------------------------------------------------------------	
! Write data       
!--------------------------------------------------------------------------------	
!      
      IF (PRESENT(zFormat) ) THEN
      	 call WriteINTEGER(iInt,tFileType,qOKL,zFormat)
         If (.NOT. qOKL) Goto 1000
      ELSE
         call WriteINTEGER(iInt,tFileType,qOKL)
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
1000 call Error_log('Error in DIO:WriteLOGICINTEGER',tErrorLog_G)
   Print*,'Error in DIO:WriteLOGICINTEGER'
2000 CONTINUE
!	      
      END SUBROUTINE WriteLOGICINTEGER
!--------------------------------------------------------------------------------

      SUBROUTINE WriteINTEGER(iInt,        &
			      tFileType,    &
			      qOK,          &
			      zFormat)
!
!********************************************************************
! Write INTEGER(KIND=IP) data to file
!********************************************************************
!
! iInt      - INPUT    - Inetger data    
! tFileType - INPUT    - Properties of output file
! qOK       - OUTPUT   - Error flag
! zFormat   - OPTIONAL - Format for data write
!
!====================================================================
! Define local variables
!!
!=====================================================================
!	
      IMPLICIT NONE
      INTEGER(KIND=IP),INTENT(IN)             :: iInt
      TYPE(cFileType), INTENT(INOUT)          :: tFileType
      CHARACTER(*),    INTENT(IN),  OPTIONAL  :: zFormat 
      LOGICAL,         INTENT(OUT)	      :: qOK      
!
      LOGICAL           :: qOKL
!
!--------------------------------------------------------------------------------	
! Set error flag to false         
!--------------------------------------------------------------------------------	
!
      qOK = .FALSE.
!
!--------------------------------------------------------------------------------	
! Write data       
!--------------------------------------------------------------------------------	
!      
      IF (tFileType%qFormatted) THEN 
         IF (PRESENT(zFormat) ) THEN
      	    WRITE (tFileType%iUnit,zFormat) (iInt)
         ELSE
           WRITE (tFileType%iUnit,'(I9)') (iInt)
         END IF
      ELSE
      !   call C_WriteInteger(tFileType%zFileName, iInt, qOKL)
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
1000 call Error_log('Error in DIO:WriteINTEGER',tErrorLog_G)
   Print*,'Error in DIO:WriteINTEGER'
2000 CONTINUE      
END SUBROUTINE WriteINTEGER

!--------------------------------------------------------------------------------


      SUBROUTINE WriteINTEGERL(iInt,        &
			      tFileType,    &
			      qOK,          &
			      zFormat)
!
!********************************************************************
! Write INTEGER(KIND=IP) data to file
!********************************************************************
!
! iInt      - INPUT    - Inetger data    
! tFileType - INPUT    - Properties of output file
! qOK       - OUTPUT   - Error flag
! zFormat   - OPTIONAL - Format for data write
!
!====================================================================
! Define local variables
!!
!=====================================================================
!	
      IMPLICIT NONE
      INTEGER(KIND=IPL),INTENT(IN)             :: iInt
      TYPE(cFileType), INTENT(INOUT)          :: tFileType
      CHARACTER(*),    INTENT(IN),  OPTIONAL  :: zFormat 
      LOGICAL,         INTENT(OUT)	      :: qOK      
!
      LOGICAL           :: qOKL
!
!--------------------------------------------------------------------------------	
! Set error flag to false         
!--------------------------------------------------------------------------------	
!
      qOK = .FALSE.
!
!--------------------------------------------------------------------------------	
! Write data       
!--------------------------------------------------------------------------------	
!      
      IF (tFileType%qFormatted) THEN 
         IF (PRESENT(zFormat) ) THEN
      	    WRITE (tFileType%iUnit,zFormat) (iInt)
         ELSE
           WRITE (tFileType%iUnit,'(I14)') (iInt)
         END IF
      ELSE
        ! call C_WriteIntegerL(tFileType%zFileName, iInt, qOKL)
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
1000 call Error_log('Error in DIO:WriteINTEGER',tErrorLog_G)
   Print*,'Error in DIO:WriteINTEGER'
2000 CONTINUE      
END SUBROUTINE WriteINTEGERL


SUBROUTINE WriteINTEGERL64(iInt,        &
      tFileType,    &
      qOK,          &
      zFormat)
!
!********************************************************************
! Write INTEGER(KIND=IP) data to file
!********************************************************************
!
! iInt      - INPUT    - Inetger data    
! tFileType - INPUT    - Properties of output file
! qOK       - OUTPUT   - Error flag
! zFormat   - OPTIONAL - Format for data write
!
!====================================================================
! Define local variables
!!
!=====================================================================
!	
IMPLICIT NONE

INTEGER(KIND=IPN),INTENT(IN)             :: iInt
TYPE(cFileType), INTENT(INOUT)          :: tFileType
CHARACTER(*),    INTENT(IN),  OPTIONAL  :: zFormat 
LOGICAL,         INTENT(OUT)	      :: qOK      

LOGICAL           :: qOKL


!     Set error flag to false

qOK = .FALSE.

!          Write data       

IF (tFileType%qFormatted) THEN 
   IF (PRESENT(zFormat) ) THEN
      WRITE (tFileType%iUnit,zFormat) (iInt)
   ELSE
     WRITE (tFileType%iUnit,'(I14)') (iInt)
   END IF
ELSE
  ! call C_WriteIntegerL64(tFileType%zFileName, iInt, qOKL)
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
1000 call Error_log('Error in DIO:WriteINTEGER',tErrorLog_G)
Print*,'Error in DIO:WriteINTEGER'
2000 CONTINUE      

END SUBROUTINE WriteINTEGERL64



!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

      SUBROUTINE WriteRealNumber(sReal,        &
			       tFileType,    &
			       qOK,          &
			       zFormat)
!
!********************************************************************
! Write real data to file
!********************************************************************
!
! sReal     - INPUT -    Real data    
! tFileType - INPUT    - Properties of output file
! qOK       - OUTPUT   - Error flag
! zFormat   - OPTIONAL - Format for data write
!
!====================================================================
! Define local variables
!!
!=====================================================================
!	
      IMPLICIT NONE
      REAL(KIND=WP),  INTENT(IN)             :: sReal
      TYPE(cFileType),INTENT(INOUT)          :: tFileType
      CHARACTER(*),   INTENT(IN),  OPTIONAL  :: zFormat 
      LOGICAL,        INTENT(OUT)	     :: qOK      
!
      LOGICAL           :: qOKL
!
!--------------------------------------------------------------------------------	
! Set error flag to false         
!--------------------------------------------------------------------------------	
!
      qOK = .FALSE.
!
!--------------------------------------------------------------------------------	
! Write data       
!--------------------------------------------------------------------------------	
!      
      IF (tFileType%qFormatted) Then 
         IF (PRESENT(zFormat) ) THEN
      	    WRITE (tFileType%iUnit,zFormat) (sReal)
         ELSE
           WRITE (tFileType%iUnit,'(E22.14E3)') (sReal)
         END IF
      ELSE
    !     call C_WriteReal(tFileType%zFileName, sReal, qOKL)
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
1000 call Error_log('Error in DIO:WriteRealNumber',tErrorLog_G)
   Print*,'Error in DIO:WriteRealNumber'
2000 CONTINUE
      END SUBROUTINE WriteRealNumber
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

      SUBROUTINE Write1DRealArray(sReal,        &
			          tFileType,    &
			          qOK,          &
				  zFormat)
!
!********************************************************************
! Write one dimensional real data to file
!********************************************************************
!
! sReal     - INPUT -    Real array data    
! tFileType - INPUT    - Properties of output file
! qOK       - OUTPUT   - Error flag
! zFormat   - OPTIONAL - Format for data write
!
!====================================================================
! Define local variables
!!
!=====================================================================
!	
      IMPLICIT NONE
      REAL(KIND=WP),  INTENT(IN)             :: sReal(:)
      TYPE(cFileType),INTENT(INOUT)          :: tFileType
      CHARACTER(*),   INTENT(IN),  OPTIONAL  :: zFormat 
      LOGICAL,        INTENT(OUT)	     :: qOK      
!
      LOGICAL           :: qOKL
!
!--------------------------------------------------------------------------------	
! Set error flag to false         
!--------------------------------------------------------------------------------	
!
      qOK = .FALSE.
!
!--------------------------------------------------------------------------------	
! Write data       
!--------------------------------------------------------------------------------	
!      
      IF (tFileType%qFormatted) Then 
         IF (PRESENT(zFormat) ) THEN
      	    WRITE (tFileType%iUnit,zFormat) (sReal)
         ELSE
           WRITE (tFileType%iUnit,'(E22.14E3)') (sReal)
         END IF
      ELSE
    !     Call C_WriteRealArray(tFileType%zFilename, sReal, qOKL)
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
1000 call Error_log('Error in DIO: Write1DRealArray',tErrorLog_G)
   Print*,'Error in DIO: Write1DRealArray'
2000 CONTINUE
	      
      END SUBROUTINE Write1DRealArray
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

      SUBROUTINE Write2DRealArray(sReal,        &
			          tFileType,    &
			          qOK,          &
				  zFormat)
!
!********************************************************************
! Write two dimensional real data to file
!********************************************************************
!
! sReal     - INPUT -    Real array data    
! tFileType - INPUT    - Properties of output file
! qOK       - OUTPUT   - Error flag
! zFormat   - OPTIONAL - Format for data write
!
!====================================================================
! Define local variables
!!
!=====================================================================
!	
      IMPLICIT NONE
      REAL(KIND=WP),  INTENT(IN)             :: sReal(:,:)
      TYPE(cFileType),INTENT(INOUT)          :: tFileType
      CHARACTER(*),   INTENT(IN),  OPTIONAL  :: zFormat 
      LOGICAL,        INTENT(OUT)	     :: qOK      
!
      LOGICAL           :: qOKL
!
!--------------------------------------------------------------------------------	
! Set error flag to false         
!--------------------------------------------------------------------------------	
!
      qOK = .FALSE.
!
!--------------------------------------------------------------------------------	
! Write data       
!--------------------------------------------------------------------------------	
!      
      IF (tFileType%qFormatted) Then 
         IF (PRESENT(zFormat) ) THEN
      	    WRITE (tFileType%iUnit,zFormat) (sReal)
         ELSE
           WRITE (tFileType%iUnit,'(E22.14E3)') (sReal)
         END IF
      ELSE
      !   call C_WriteRealArray(tFileType%zFileName, sReal, qOKL)
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
1000 call Error_log('Error in DIO: Write2DRealArray',tErrorLog_G)
   Print*,'Error in DIO: Write2DRealArray'
2000 CONTINUE
      END SUBROUTINE Write2DRealArray
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

      SUBROUTINE Write3DRealArray(sReal,        &
			          tFileType,    &
			          qOK,          &
				  zFormat)
!
!********************************************************************
! Write three dimensional real data to file
!********************************************************************
!
! sReal     - INPUT -    Real array data    
! tFileType - INPUT    - Properties of output file
! qOK       - OUTPUT   - Error flag
! zFormat   - OPTIONAL - Format for data write
!
!====================================================================
! Define local variables
!!
!=====================================================================
!	
      IMPLICIT NONE
      REAL(KIND=WP),  INTENT(IN)             :: sReal(:,:,:)
      TYPE(cFileType),INTENT(INOUT)          :: tFileType
      CHARACTER(*),   INTENT(IN),  OPTIONAL  :: zFormat 
      LOGICAL,        INTENT(OUT)	     :: qOK      
!
      LOGICAL           :: qOKL
!
!--------------------------------------------------------------------------------	
! Set error flag to false         
!--------------------------------------------------------------------------------	
!
      qOK = .FALSE.
!
!--------------------------------------------------------------------------------	
! Write data       
!--------------------------------------------------------------------------------	
!     
      IF (tFileType%qFormatted) Then 
         IF (PRESENT(zFormat) ) THEN
      	    WRITE (tFileType%iUnit,zFormat) (sReal)
         ELSE
           WRITE (tFileType%iUnit,'(E22.14E3)') (sReal)
         END IF
      ELSE
      !   call C_WriteRealArray(tFileType%zFileName, sReal, qOKL)
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
1000 call Error_log('Error in DIO: Write3DRealArray',tErrorLog_G)
   Print*,'Error in DIO: Write3DRealArray'
2000 CONTINUE

      END SUBROUTINE Write3DRealArray
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

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

      SUBROUTINE FileNameNoExtension(zFileName, &
      				   zFile, &
			           qOK)
!
!********************************************************************
! Return filename with extension removed ie '.*' removed 
!********************************************************************
!
! zFileName     - INPUT    - FileName
! zFile         - OUTPUT   - File no extension
! qOK           - OUTPUT   - Error flag
!
!====================================================================
! Define local variables
!
!=====================================================================
!	
      IMPLICIT NONE
 
      CHARACTER(*),   INTENT(IN)            :: zFileName
      CHARACTER(*),   INTENT(OUT)           :: zFile
      LOGICAL,        INTENT(OUT)	    :: qOK      
!
!--------------------------------------------------------------------------------	
! Local Scalars         
!--------------------------------------------------------------------------------	
!
      INTEGER(KIND=IP)      :: iExtension
!
!--------------------------------------------------------------------------------	
! Set error flag to false         
!--------------------------------------------------------------------------------	
!
      qOK = .FALSE.
!
!--------------------------------------------------------------------------------	
! Find last position of '.' in filename  
!--------------------------------------------------------------------------------	
!
      iExtension = INDEX(zfilename,'.',.TRUE.)
!
!--------------------------------------------------------------------------------	
! Return filename
!--------------------------------------------------------------------------------	
!
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
!
!--------------------------------------------------------------------------------	
! Remove leading and trailing blanks
!--------------------------------------------------------------------------------	
!
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

END MODULE IO
