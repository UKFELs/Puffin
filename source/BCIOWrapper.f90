!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE CIOWrapper

!          Module with Fortran 90 wrappers for C commands.

  USE paratype
  !USE IO
		
  IMPLICIT NONE

  Private CString

!      TYPE(cFileType),SAVE         :: tErrorLog_G




! This interface allows you to call the routine OutputIntegrationData
! with either a single value or an array as input

  INTERFACE C_WriteRealArray
    MODULE PROCEDURE C_WriteRealArray1D, C_WriteRealArray2D, C_WriteRealArray3D
  END INTERFACE 

  INTERFACE C_WriteIntegerArray
    MODULE PROCEDURE C_WriteIntegerArray1D, C_WriteIntegerArray2D, C_WriteIntegerArray3D
  END INTERFACE 


  CONTAINS





  FUNCTION CString(zString)

!     Function to change a fortran string to a C string

!     zString    INPUT     Fortran string

    IMPLICIT NONE

    CHARACTER*1000                          :: CString
    CHARACTER(*),     INTENT(IN)            :: zString

!     Add null terminate to string      

    CString = Trim(zString) // CHAR(0)

!  Set error flag and exit         

    GoTo 2000     

!     Error Handler

!1000 call Error_log('Error in BCIOWrapper:CString',tErrorLog_G)

2000 CONTINUE

  END FUNCTION CString






  SUBROUTINE C_OpenFileForOutput(zFileName, qOK)

!     Open File For Output using C

!     zFileName   INPUT      FileName
!     qOK         OUTPUT     Error flag

    IMPLICIT NONE

    CHARACTER(*), INTENT(IN)    :: zFileName
    LOGICAL,      INTENT(OUT)   :: qOK     
    CHARACTER*9                 :: zFile      



!     Set error flag to false

    qOK = .FALSE.

! Write data       
! 
    Call openfileforoutput_c(Trim(CString(zFileName)))

!  Set error flag and exit         

    qOK = .TRUE.				    
    GoTo 2000     

!     Error Handler

!1000 call Error_log('Error in BCIOWrapper:C_OpenFileForOutput',tErrorLog_G)

2000 CONTINUE

  END SUBROUTINE C_OpenFileForOutput





  SUBROUTINE C_WriteInteger(zFileName, iNum, qOK)

!     Write integer to file using C
!
!     zFileName - INPUT    - FileName
!     iNum      - INPUT    - Integer to write
!     qOK       - OUTPUT   - Error flag
!

    IMPLICIT NONE
    CHARACTER(*),     INTENT(IN)   :: zFileName
    INTEGER(KIND=IP), INTENT(IN)   :: iNum
    LOGICAL,          INTENT(OUT)  :: qOK     

!     Set error flag to false

      qOK = .FALSE.

!     Write data
 
      Call writeinteger_c(Trim(CString(zFileName)), iNum)

!  Set error flag and exit         

      qOK = .TRUE.				    
      GoTo 2000

!     Error Handler
           
!1000 call Error_log('Error in BCIOWrapper:C_WriteInteger',tErrorLog_G)

2000 CONTINUE
	      
  END SUBROUTINE C_WriteInteger





  SUBROUTINE C_WriteIntegerL(zFileName, iNum, qOK)

!     Write integer to file using C

!     zFileName - INPUT    - FileName
!     iNum      - INPUT    - Integer to write
!     qOK       - OUTPUT   - Error flag
!
!     Define local variables

    IMPLICIT NONE

    CHARACTER(*),      INTENT(IN)   :: zFileName
    INTEGER(KIND=IPL), INTENT(IN)   :: iNum
    LOGICAL,           INTENT(OUT)  :: qOK     

!     Set error flag to false         

    qOK = .FALSE.

!     Write data

    Call writeinteger_c(Trim(CString(zFileName)), iNum)

!     Set error flag and exit         

      qOK = .TRUE.				    
      GoTo 2000     

!     Error Handler

!1000 call Error_log('Error in BCIOWrapper:C_WriteInteger',tErrorLog_G)

2000 CONTINUE
	      
  END SUBROUTINE C_WriteIntegerL




  SUBROUTINE C_WriteIntegerL64(zFileName, iNum, qOK)

!     Write integer to file using C

!     zFileName - INPUT    - FileName
!     iNum      - INPUT    - Integer to write
!     qOK       - OUTPUT   - Error flag
!
!     Define local variables

    IMPLICIT NONE

    CHARACTER(*),      INTENT(IN)   :: zFileName
    INTEGER(KIND=IPN), INTENT(IN)   :: iNum
    LOGICAL,           INTENT(OUT)  :: qOK     

!     Set error flag to false         

    qOK = .FALSE.

!     Write data

    Call writeinteger_c(Trim(CString(zFileName)), iNum)

!     Set error flag and exit         

      qOK = .TRUE.				    
      GoTo 2000     

!     Error Handler

!1000 call Error_log('Error in BCIOWrapper:C_WriteInteger',tErrorLog_G)

2000 CONTINUE
	      
  END SUBROUTINE C_WriteIntegerL64











  SUBROUTINE C_WriteReal(zFileName, sNum, qOK)

!     Write real to file using C
!
!     zFileName - INPUT    - FileName
!     iNum      - INPUT    - Real to write
!     qOK       - OUTPUT   - Error flag

    IMPLICIT NONE

    CHARACTER(*),     INTENT(IN)            :: zFileName
    REAL(KIND=WP),    INTENT(IN)            :: sNum
    LOGICAL,          INTENT(OUT)	      :: qOK     

!     Set error flag to false

    qOK = .FALSE.

!     Write data       

    Call writedouble_c(Trim(CString(zFileName)), sNum)

!      Set error flag and exit         

    qOK = .TRUE.				    
    GoTo 2000

!     Error Handler

!1000 call Error_log('Error in BCIOWrapper:C_WriteReal',tErrorLog_G)

2000 CONTINUE

  END SUBROUTINE C_WriteReal




  SUBROUTINE C_WriteString(zFileName, zString, qOK, qNewLine )

!     Write string to file using C
!
!     zFileName - INPUT    - FileName
!     zString   - INPUT    - String to write
!     qOK       - OUTPUT   - Error flag

      IMPLICIT NONE

      CHARACTER(*),     INTENT(IN)            :: zFileName
      CHARACTER(*),     INTENT(IN)            :: zString
      LOGICAL,          INTENT(OUT)	      :: qOK 
      LOGICAL,          INTENT(IN),  OPTIONAL :: qNewLine  

!     Set error flag to false         

      qOK = .FALSE.

!     Check if to add new line character (line feed) at end of string       
!
!      If (.NOT. PRESENT(qNewLine)) Then
!         qNewLine = .FALSE.
!      End If

!     Write data       

    If (PRESENT(qNewLine) .AND. qNewLine) Then

      Call writestring_c(Trim(CString(zFileName)), Trim((Trim(zString // ACHAR(10)))))

    Else

      Call writestring_c(Trim(CString(zFileName)), Trim((zString)))

    End if

!  Set error flag and exit

    qOK = .TRUE.				    
    GoTo 2000     

!     Error Handler

!1000 call Error_log('Error in BCIOWrapper:C_WriteString',tErrorLog_G)

2000 CONTINUE
	      
  END SUBROUTINE C_WriteString



  SUBROUTINE C_WriteIntegerArray1D(zFileName, iNum, qOK)

!     Write integer array to file using C
!
!     zFileName - INPUT    - FileName
!     iNum      - INPUT    - Integer to write
!     qOK       - OUTPUT   - Error flag
	
      IMPLICIT NONE

      CHARACTER(*),     INTENT(IN)            :: zFileName
      INTEGER(KIND=IP), INTENT(IN)            :: iNum(:)
      LOGICAL,          INTENT(OUT)	      :: qOK     

!     Set error flag to false	

      qOK = .FALSE.
!
!--------------------------------------------------------------------------------	
! Write data       
!--------------------------------------------------------------------------------	
! 
      Call writelongarray_c(Trim(CString(zFileName)), iNum, Size(iNum))
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
!1000 call Error_log('Error in BCIOWrapper:C_WriteIntegerArray',tErrorLog_G)
  ! Print*,'Error in BCIOWrapper:C_WriteIntegerArray1D'
2000 CONTINUE
!	      
      END SUBROUTINE C_WriteIntegerArray1D
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

      SUBROUTINE C_WriteIntegerArray2D(zFileName,        &
                                     iNum,            &
			             qOK)
!
!********************************************************************
! Write integer array to file using C
!********************************************************************
!
! zFileName - INPUT    - FileName
! iNum      - INPUT    - Integer to write
! qOK       - OUTPUT   - Error flag
!
!====================================================================
! Define local variables
!
!=====================================================================
!	
      IMPLICIT NONE
      CHARACTER(*),     INTENT(IN)            :: zFileName
      INTEGER(KIND=IP), INTENT(IN)            :: iNum(:,:)
      LOGICAL,          INTENT(OUT)	      :: qOK     
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
      Call writelongarray_c(Trim(CString(zFileName)), iNum, Size(iNum))
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
!1000 call Error_log('Error in BCIOWrapper:C_WriteIntegerArray',tErrorLog_G)
  ! Print*,'Error in BCIOWrapper:C_WriteIntegerArray2D'
2000 CONTINUE
!	      
      END SUBROUTINE C_WriteIntegerArray2D
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

      SUBROUTINE C_WriteIntegerArray3D(zFileName,        &
                                      iNum,            &
			              qOK)
!
!********************************************************************
! Write integer array to file using C
!********************************************************************
!
! zFileName - INPUT    - FileName
! iNum      - INPUT    - Integer to write
! qOK       - OUTPUT   - Error flag
!
!====================================================================
! Define local variables
!
!=====================================================================
!	
      IMPLICIT NONE
      CHARACTER(*),     INTENT(IN)            :: zFileName
      INTEGER(KIND=IP), INTENT(IN)            :: iNum(:,:,:)
      LOGICAL,          INTENT(OUT)	      :: qOK     
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
      Call writelongarray_c(Trim(CString(zFileName)), iNum, Size(iNum))
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
!1000 call Error_log('Error in BCIOWrapper:C_WriteIntegerArray',tErrorLog_G)
   !Print*,'Error in BCIOWrapper:C_WriteIntegerArray3D'
2000 CONTINUE
!	      
      END SUBROUTINE C_WriteIntegerArray3D
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

      SUBROUTINE C_WriteRealArray1D(zFileName,        &
                                  sNum,           &
			          qOK)
!
!********************************************************************
! Write real array to file using C
!********************************************************************
!
! zFileName - INPUT    - FileName
! sNum      - INPUT    - Real to write
! qOK       - OUTPUT   - Error flag
!
!====================================================================
! Define local variables
!
!=====================================================================
!	
      IMPLICIT NONE
      CHARACTER(*),     INTENT(IN)            :: zFileName
      REAL(KIND=WP),    INTENT(IN)            :: sNum(:)
      LOGICAL,          INTENT(OUT)	      :: qOK     
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
      Call writedoublearray_c(Trim(CString(zFileName)), sNum,Size(sNum))
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
!1000 call Error_log('Error in BCIOWrapper:C_WriteRealArray',tErrorLog_G)
   !Print*,'Error in BCIOWrapper:C_WriteRealArray1D'
2000 CONTINUE
!	      
      END SUBROUTINE C_WriteRealArray1D

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

      SUBROUTINE C_WriteRealArray2D(zFileName,        &
                                  sNum,           &
			          qOK)
!
!********************************************************************
! Write real array to file using C
!********************************************************************
!
! zFileName - INPUT    - FileName
! sNum      - INPUT    - Real to write
! qOK       - OUTPUT   - Error flag
!
!====================================================================
! Define local variables
!
!=====================================================================
!	
      IMPLICIT NONE
      CHARACTER(*),     INTENT(IN)            :: zFileName
      REAL(KIND=WP),    INTENT(IN)            :: sNum(:,:)
      LOGICAL,          INTENT(OUT)	      :: qOK     
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
      Call writedoublearray_c(Trim(CString(zFileName)), sNum,Size(sNum))
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
!1000 call Error_log('Error in BCIOWrapper:C_WriteRealArray',tErrorLog_G)
   !Print*,'Error in BCIOWrapper:C_WriteRealArray2D'
2000 CONTINUE
!	      
      END SUBROUTINE C_WriteRealArray2D
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

      SUBROUTINE C_WriteRealArray3D(zFileName,        &
                                  sNum,           &
			          qOK)
!
!********************************************************************
! Write real array to file using C
!********************************************************************
!
! zFileName - INPUT    - FileName
! sNum      - INPUT    - Real to write
! qOK       - OUTPUT   - Error flag
!
!====================================================================
! Define local variables
!
!=====================================================================
!	
      IMPLICIT NONE
      CHARACTER(*),     INTENT(IN)            :: zFileName
      REAL(KIND=WP),    INTENT(IN)            :: sNum(:,:,:)
      LOGICAL,          INTENT(OUT)	      :: qOK     
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
      Call writedoublearray_c(Trim(CString(zFileName)), sNum,Size(sNum))
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
!1000 call Error_log('Error in BCIOWrapper:C_WriteRealArray',tErrorLog_G)
   !Print*,'Error in BCIOWrapper:C_WriteRealArray3D'
2000 CONTINUE
!	      
      END SUBROUTINE C_WriteRealArray3D
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------


END MODULE CIOWrapper
