Module ArrayFunctions
!--------------------------------------------------------------------------------
! Module to retrieve and populate array
!--------------------------------------------------------------------------------

  USE paratype
  USE ParallelInfoType
  USE FileType
  USE IO
  USE ParallelSetup
!      
  IMPLICIT NONE
!
!
!        
!         Global Parameters
!
! iMtxStartPosition_G           Array holding pointers to equations in matrix
! iMtxEndPosition_G             Array holding pointers to equations in matrix
! iBStartPosition_G             Array holding pointers to equations in array B (rhs)
! iBEndPosition_G               Array holding pointers to equations in array B (rhs)
!
! nArrayVariables_CG            Number of variables held in array
! iRe_A_CG                      Position in pointer array of Real A values
! iIm_A_CG                      Position in pointer array of Imaginary A values
! iRe_PPerp_CG                  Position in pointer array of Real pperp values
! iIm_PPerp_CG                  Position in pointer array of Imaginary pperp values
! iRe_Q_CG                      Position in pointer array of Q values
! iRe_z2_CG                     Position in pointer array of z2 values
!
! nWaveEquations_CG             Number of wave equations (one real plus one imaginary)
! nElectronEquations_CG         Number of electron equations (real p, imaginary p, q, z2)
!

  INTEGER(KIND=IPL), ALLOCATABLE  :: iBStartPosition_G(:)
  INTEGER(KIND=IPL), ALLOCATABLE  :: iBEndPosition_G(:)

  INTEGER(KIND=IP), ALLOCATABLE  :: iAStartPosition_G(:)
  INTEGER(KIND=IP), ALLOCATABLE  :: iAEndPosition_G(:)

  INTEGER(KIND=IP), PARAMETER    :: nArrayVariables_CG = 8_IP
  INTEGER(KIND=IP), PARAMETER    :: iRe_PPerp_CG       = 3_IP
  INTEGER(KIND=IP), PARAMETER    :: iIm_PPerp_CG       = 4_IP
  INTEGER(KIND=IP), PARAMETER    :: iRe_Q_CG           = 5_IP
  INTEGER(KIND=IP), PARAMETER    :: iRe_z2_CG          = 6_IP
  INTEGER(KIND=IP), PARAMETER    :: iRe_X_CG           = 7_IP
  INTEGER(KIND=IP), PARAMETER    :: iRe_Y_CG           = 8_IP
      
  INTEGER(KIND=IP), PARAMETER    :: nFieldEquations_CG = 2_IP
  INTEGER(KIND=IP), PARAMETER    :: iRe_A_CG	       = 1_IP
  INTEGER(KIND=IP), PARAMETER    :: iIm_A_CG	       = 2_IP
 
  INTEGER(KIND=IP), PARAMETER    :: nElectronEquations_CG  = 6_IP
	  
  LOGICAL  :: qEmpty

!                 Define type cArraySegment
!
! The arrays used are made up of different variables, this type stores
! the start and end positions of the different variables making up the
! whole array.
! ie an array Y may be made up as
!
!              |-  -|
!              |Re_A|
!              |Im_A|
!       [Y] =  |Re_P|
!              |Im_P|
!              |Q   |
!              |Z2  |
!              |-  -|
!
!
! iStart         Start position of variable in large array
! iEnd           End position of variable in large array
! qWrite         If writing this variables data out to file
! zVariable      Name of variable
! tFileType      File information if writing data to file
!
  TYPE cArraySegment
    INTEGER(KIND=IP)    :: iStart = 0_IP
    INTEGER(KIND=IP)    :: iEnd   = 0_IP
    LOGICAL             :: qWrite = .FALSE.
    CHARACTER(32_IP)    :: zVariable = ''
    TYPE(cFileType)     :: tFileType 
  END TYPE

  Private PutValueInVector_OneValue
  Private PutValueInVector_Array 

! This interface allows you to call the routine PutValueInVector
! with either a single value or an array as input
      
  INTERFACE PutValueInVector
    MODULE PROCEDURE PutValueInVector_OneValue, PutValueInVector_Array   
  END INTERFACE   

  CONTAINS



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE SetUpElectronArray(tArraySegment,tArrayA,&
       iNME,iNNF,qOK)

    IMPLICIT NONE
!
! Setup electron array.
!
!
! tArraySegment - Holds the start and end points of each
!                 section of the electron array
! q0K  	 	- Error flag
!
! Define local variables
! i - Loop counter
!
    TYPE(cArraySegment), INTENT(INOUT) :: tArraySegment(:),tArrayA(:)
    INTEGER(KIND=IPL), INTENT(IN)   ::   iNME
    INTEGER(KIND=IP), INTENT(IN)   ::   iNNF
    LOGICAL, INTENT(OUT) :: qOK
! Local Scalars         
    INTEGER(KIND=IP)               :: i
    LOGICAL                        :: qOKL
!-------------------------------------------------------------------
! START:-
! Set error flag to false         
    qOK = .FALSE.       
! Set up array pointers for local electron array
! These subroutines can be found in EArrayFunctions.f90
    ALLOCATE(iBStartPosition_G(nElectronEquations_CG+nFieldEquations_CG))
    ALLOCATE(iBEndPosition_G(nElectronEquations_CG+nFieldEquations_CG))
       
    DO i = 1, nElectronEquations_CG+nFieldEquations_CG
       IF (i==1 .OR. i==2) THEN
          CALL SetPointer(i, INT(iNNF,KIND=IPL), iBStartPosition_G,&
               iBEndPosition_G, qOKL)
          tArrayA(i)%istart = iBStartPosition_G(i)
          tArrayA(i)%iEnd = iBEndPosition_G(i)
       ELSE
          CALL SetPointer(i, iNME, iBStartPosition_G,&
               iBEndPosition_G, qOKL)
          tArraySegment(i-2)%istart = iBStartPosition_G(i)
          tArraySegment(i-2)%iEnd = iBEndPosition_G(i)
          IF (.NOT. qOKL) GOTO 1000
       END IF
    END DO
!----------------------------------------------------------
!  Set error flag and exit         
    qOK = .TRUE.
    GOTO 2000     
! Error Handler - Error log Subroutine in CIO.f90 line 709
1000 CALL Error_log('Error in FEMethod:SetUpStiffnessMatrix',tErrorLog_G)
    PRINT*,'Error in FEMethod:SetUpStiffnessMatrix'
2000 CONTINUE
  END SUBROUTINE SetUpElectronArray

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE  PutValueInVector_OneValue(iPointer,sValue, &
                                        sVector, qOK)

  IMPLICIT NONE

!
! Place a value into B vector (Ie, right hand side or y)
!
!              ARGUMENTS
!
! iPointer          Pointer to use for value
! sValue            Value to place into array
! sArray            Array to insert value into
! qOK               Error flag

  INTEGER(KIND=IP), INTENT(IN)    :: iPointer
  REAL(KIND=WP),    INTENT(IN)    :: sValue 
  REAL(KIND=WP),    INTENT(INOUT) :: sVector(:)
  LOGICAL,          INTENT(OUT)   :: qOK

!             LOCAL ARGS
!
! iStart            Start Position in Y 
! iEnd              End Position in Y 
!
  INTEGER(KIND=IPL)    ::   iStart
  INTEGER(KIND=IPL)    ::   iEnd 

!     Set error flag to false

  qOK = .FALSE.

! Error handling

  IF (iPointer <= 0_IP) THEN 
    CALL Error_log('Pointer is negative',tErrorLog_G)
    GOTO  1000
  END IF

  IF (iPointer > Size(iBStartPosition_G)) THEN 
    CALL Error_log('Pointer is outwith scope of starting vals',tErrorLog_G)
    GOTO  1000
  END IF
  
  IF (iPointer > Size(iBEndPosition_G)) THEN 
    CALL Error_log('Pointer is outwith scope of ending vals',tErrorLog_G)
    GOTO  1000
  END IF

!     Set up initial field values

  iStart = iBStartPosition_G(iPointer)
  iEnd   = iBEndPosition_G(iPointer)

!     Error handling

  IF (iStart <= 0_IP) THEN 
    CALL Error_log('Starting index is negative',tErrorLog_G)
    GOTO  1000
  END IF

  IF (iEnd <= 0_IP) THEN 
    CALL Error_log('Ending index is negative',tErrorLog_G)
    GOTO  1000
  END IF

  IF (iStart > iEnd) THEN 
    CALL Error_log('Start index is larger than end index',tErrorLog_G)
    GOTO  1000
  END IF

  IF (iEnd > Size(sVector)) THEN 
    CALL Error_log('Ending index is too large',tErrorLog_G)
    GOTO  1000
  END IF

!     Assign value to array segment

  sVector(iStart:iEnd) = sValue
  
!     Set error flag and exit

  qOK = .TRUE.
  
  GOTO 2000     

! Error Handler
            
1000 CALL Error_log('Error in ArrayFunctions:PutValueInVector_OneValue',tErrorLog_G)

2000 CONTINUE
!
  END SUBROUTINE PutValueInVector_OneValue

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE  PutValueInVector_Array(iPointer,sValue, &
                                     sVector, qOK)

  IMPLICIT NONE

! Place svalue into svector (Ie, right hand side or y)
!
!                  ARGUMENTS
!
! iPointer            Pointer to use for value
! sValue              Array of Values to place into array
! sArray              Array to insert value into
! qOK                 Error flag

  INTEGER(KIND=IP), INTENT(IN)     :: iPointer   
  REAL(KIND=WP),    INTENT(IN)     :: sValue(:) 
  REAL(KIND=WP),    INTENT(INOUT)  :: sVector(:)
  LOGICAL,          INTENT(OUT)    :: qOK      

!                  LOCAL ARGS
! 
! iStart              Start Position in Y 
! iEnd                End Position in Y 

  INTEGER(KIND=IPL)   ::   iStart
  INTEGER(KIND=IPL)   ::   iEnd 

!     Set error flag to false

      qOK = .FALSE. 
  
  IF (qEmpty) THEN
    qOK = .TRUE.
    GOTO 2000 
  END IF

!     Error handling

  If (iPointer <= 0_IP) THEN 
    CALL Error_log('Pointer is negative',tErrorLog_G)
    GOTO  1000
  END IF

  If (iPointer > Size(iBStartPosition_G)) THEN 
    CALL Error_log('Pointer is too large for start array',tErrorLog_G)
    GOTO  1000
  END IF

  If (iPointer > Size(iBEndPosition_G)) THEN 
    CALL Error_log('Pointer is roo large for end array',tErrorLog_G)
    GOTO  1000
  END IF

  iStart = iBStartPosition_G(iPointer)
  iEnd = iBEndPosition_G(iPointer)

  If (iStart <= 0_IP) Goto  1000

  If (iEnd <= 0_IP)   Goto  1000

  If (iStart > iEnd)  Goto  1000

  If (iEnd > Size(sVector)) Goto  1000


  If ((iEnd - iStart + 1_IP) .NE. Size(sValue)) Goto  1000

  sVector(iStart:iEnd) = sValue

!  Set error flag and exit         

  qOK = .TRUE.
  GOTO 2000

! Error Handler
            
1000 call Error_log('Error in EArrayFunctions:PutValueInVector_Array',tErrorLog_G)
  PRINT*,'Error in EArrayFunctions:PutValueInVector_Array'
2000 CONTINUE
!	      
  END SUBROUTINE PutValueInVector_Array

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  FUNCTION  Vector(iPointer, sArray)

  IMPLICIT NONE

!             ARGUMENTS
!
! Get part of vector corresponding to pointer 
! (Ie, right hand side or y)
!
! iPointer           Pointer to use for value
! sArray             Array to extract vector from
! qOK                Error flag

  INTEGER(KIND=IP), INTENT(IN)  ::  iPointer   
  REAL(KIND=WP),    INTENT(IN)  ::  sArray(:)
  REAL(KIND=WP) :: Vector(iBEndPosition_G(iPointer) - iBStartPosition_G(iPointer) + 1_IP)

!             LOCAL ARGS
! 
! iStart             Start Position in Y 
! iEnd               End Position in Y 
!	
  INTEGER(KIND=IPL)  ::  iStart
  INTEGER(KIND=IPL)  ::  iEnd 

!     Set up initial field values
       
  iStart = iBStartPosition_G(iPointer)
  iEnd = iBEndPosition_G(iPointer)
     
  Vector = sArray(iStart:iEnd)

!     Set error flag and exit

  GoTo 2000     

! Error Handler
            
1000 call Error_log('Error in EArrayFunctions:Vector',tErrorLog_G)
  PRINT*,'Error in EArrayFunctions:Vector'
2000 CONTINUE
      
  END FUNCTION Vector

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  FUNCTION  GetValueFromVector(iPointer, ithValue, &
                               sArray, qOK)

  IMPLICIT NONE

! Get ithvalue from part of vector corresponding to pointer 
! (Ie, right hand side or y)
! ie get pperp for 10th electron from right hand side array 
!
!                ARGUMENTS
!
! iPointer            Pointer to use for value
! ithValue            ith value to return
! sArray              Array to extract vector from
! qOK                 Error flag

  INTEGER(KIND=IP),  INTENT(IN) :: iPointer   
  INTEGER(KIND=IPL), INTENT(IN) :: ithValue   
  REAL(KIND=WP),     INTENT(IN) :: sArray(:)
  LOGICAL,           INTENT(OUT):: qOK
  REAL(KIND=WP)                 :: GetValueFromVector

!                LOCAL ARGS
! 
! iStart              Start Position in Y 
! iEnd                End Position in Y 
!	
  INTEGER(KIND=IPL) :: iStart
  INTEGER(KIND=IPL) :: iEnd 

!     Set error flag to false

  qOK = .FALSE.  

!     Set up initial field values

  IF (iPointer <= 0_IP) GOTO  1000
  IF (iPointer > Size(iBStartPosition_G)) GOTO  1000
  IF (iPointer > Size(iBEndPosition_G))   GOTO  1000      

  iStart = iBStartPosition_G(iPointer)
  iEnd = iBEndPosition_G(iPointer)

  IF (iStart + ithValue - 1_IP <= iEnd) THEN     
    GetValueFromVector = sArray(iStart + ithValue - 1_IP)
  ELSE
    GOTO 1000
  END IF

!     Set error flag and exit

  qOK = .TRUE.
  GOTO 2000     

!     Error Handler

            
1000 CALL Error_log('Error in EArrayFunctions:GetValueFromVector',tErrorLog_G)
   PRINT*,'Error in EArrayFunctions:GetValueFromVector'
2000 CONTINUE
      
  END FUNCTION GetValueFromVector

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE  PutValueInVectorElm(iPointer,ithValue, &
                                  value, sArray, qOK)

  IMPLICIT NONE

!     Put value into ith element in the array sArray
!
!                 ARGUMENTS
!
! iPointer         Pointer to use for value
! ithValue         ith value to alter
! value            Value to put into ith element
! sArray           Array to alter vector from
! qOK              Error flag

  INTEGER(KIND=IP),   INTENT(IN)      :: iPointer   
  INTEGER(KIND=IPL),  INTENT(IN)      :: ithValue  
  REAL(KIND=WP),      INTENT(IN)      :: value 
  REAL(KIND=WP),      INTENT(INOUT)   :: sArray(:)
  LOGICAL,            INTENT(OUT)     :: qOK

!                 LOCAL ARGS
! 
! iStart      Start Position in Y 
! iEnd        End Position in Y 

  INTEGER(KIND=IPL)  ::  iStart
  INTEGER(KIND=IPL)  ::  iEnd 

!     Set error flag to false
!
  qOK = .FALSE.  

! Get start and end points, an plave value into vector.

  If (iPointer <= 0_IP) Goto  1000
  If (iPointer > Size(iBStartPosition_G)) Goto  1000
  If (iPointer > Size(iBEndPosition_G))   Goto  1000      
!
  iStart = iBStartPosition_G(iPointer)
  iEnd = iBEndPosition_G(iPointer)

!
  IF (iStart + ithValue - 1_IP <= iEnd) THEN
    sArray(iStart + ithValue - 1_IP) = value
  ELSE
    GOTO 1000
  END IF

!  Set error flag and exit

  qOK = .TRUE.
  GOTO 2000

!     Error Handler
            
1000 CALL Error_log('Error in EArrayFunctions:PutValueInVectorElm',tErrorLog_G)
   PRINT*,'Error in EArrayFunctions:PutValueInVectorElm'
2000 CONTINUE

  END SUBROUTINE PutValueInVectorElm

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  SUBROUTINE  SetUpArraySegment(nArraySegments,tArraySegment, &
                                qOK)

  IMPLICIT NONE
      
! Set up array segment data
!
!            ARGUMENTS
!
! nArraySegments   INPUT     number of array segments
! tArraySegment    UPDATE    Result data file info
! qOK              OUTPUT    Error flag
!
  INTEGER(KIND=IP),    INTENT(IN)    :: nArraySegments
  TYPE(cArraySegment), INTENT(INOUT) :: tArraySegment(:)
  LOGICAL,             INTENT(OUT)   :: qOK      
!
!====================================================================
! Define local variables
! iSegment - Local loop variable ( segment index)
!=====================================================================
!	
      INTEGER(KIND=IP)        :: iSegment
!
!--------------------------------------------------------------------------------	
! Set error flag to false         
!--------------------------------------------------------------------------------	
!
      qOK = .FALSE.    
!
!--------------------------------------------------------------------------------	
! Set up start and end values of array segment data     
!--------------------------------------------------------------------------------	
!
      Do iSegment = 1_IP, nArraySegments
!
	 tArraySegment(iSegment)%iStart  = iBStartPosition_G(iSegment)
	 tArraySegment(iSegment)%iEnd    = iBEndPosition_G(iSegment)
!
      End Do
!
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
1000 call Error_log('Error in EArrayFunctions:SetUpArraySegment',tErrorLog_G)
   Print*,'Error in EArrayFunctions:SetUpArraySegment'
2000 CONTINUE
!
      END SUBROUTINE SetUpArraySegment
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
      SUBROUTINE  SetUpDataFiles(zDataFileName,  &
                                 qFormattedFiles, &
                                 tWriteZData,    &
				 tWriteAData,	 &
                                 tArraySegment,  &
                                 qOK,            &
				 zOptionalString)
!
!********************************************************************
! Open data files
!********************************************************************
!
! zDataFileName           - INPUT    - Data file name
! qFormattedFiles         - INPUT    - if output data files to be formatted or binary
! tArraySegment           - UPDATE   - Result data file info
! qOK                     - OUTPUT   - Error flag
!
!********************************************************************
!	
      IMPLICIT NONE
!
      CHARACTER(32_IP),        INTENT(IN)                :: zDataFileName
      LOGICAL,                 INTENT(IN)                :: qFormattedFiles
      TYPE(cArraySegment),     INTENT(INOUT)             :: tWriteZData
      TYPE(cArraySegment),     INTENT(INOUT)             :: tWriteAData(:)
      TYPE(cArraySegment),     INTENT(INOUT)             :: tArraySegment(:)
      LOGICAL,                 INTENT(OUT)               :: qOK      
      CHARACTER(*),            INTENT(IN), OPTIONAL      :: zOptionalString
!
!====================================================================
! Define local variables
! 
! iSegment - Local loop variable
! zFileName - Filename to use 
! qOKL   - Local error flag
!=====================================================================
!	
       INTEGER(KIND=IP)               :: iSegment
       CHARACTER(32_IP)               :: zFileName
       LOGICAL                        :: qOptional
       LOGICAL                        :: qOKL
!
!--------------------------------------------------------------------------------	
! Set error flag to false         
! This type is defined is "ParallelInfoType.f90"
!--------------------------------------------------------------------------------	
!
      qOK = .FALSE.    
!
      qOptional = .FALSE.
      If (PRESENT(zOptionalString)) Then
         If (Len(TRIM(ADJUSTL(zOptionalString))) > 0) Then
            qOptional = .TRUE.
	 End if
      End if
!

!
!
!- ''tArraySegment'' type is defined in "AMathLibGlobals.f90"
!--------------------------------------------------------------------------------	
! Loop over allsegments and open data file if required        
!--------------------------------------------------------------------------------	
!
         !PRINT*,SIZE(tArraySegment)
		 Do iSegment = 1_IP, Size(tArraySegment)
!
!--------------------------------------------------------------------------------	
! If to write data open file       
!--------------------------------------------------------------------------------	
!
            If (tArraySegment(iSegment)%qWrite) Then
!
!--------------------------------------------------------------------------------	
! Create filename      
!--------------------------------------------------------------------------------	
!
!Pamela: need to put something sensible here
	       zFilename = (TRIM(ADJUSTL(tArraySegment(iSegment)%zVariable)) // TRIM(ADJUSTL(zDataFileName)) )
	       If (qOptional) Then
	          zFilename = (TRIM(ADJUSTL(zOptionalString)) // '_' // TRIM(ADJUSTL(zFilename)) )
	       End if
!
!--------------------------------------------------------------------------------	
! Open file - This subroutine is in this file - line 517
!--------------------------------------------------------------------------------	
!	      
	         IF (tProcInfo_G%qRoot) THEN
		   call SetUpDataFile(zFilename,                         &
	                          qFormattedFiles,                   &
	                          tArraySegment(iSegment)%zVariable, &
			          tArraySegment(iSegment)%tFileType, &
			          qOKL)
               If (.NOT. qOKL) Goto 1000
!	       
	    	END IF
			CALL shareFileType(tArraySegment(iSegment)%tFileType)
		End If
!
         End Do
!
!--------------------------------------------------------------------------------	
!  Field Data files         
!--------------------------------------------------------------------------------	
!
	 If (tWriteAData(iRe_A_CG)%qWrite) Then	      
	    IF (tProcInfo_G%qRoot) THEN
!
!--------------------------------------------------------------------------------	
! Create filename      
!--------------------------------------------------------------------------------	
!
!Pamela: need to put something sensible here
	       zFilename = (TRIM(ADJUSTL(tWriteAData(iRe_A_CG)%zVariable)) // TRIM(ADJUSTL(zDataFileName))) 
		   If (qOptional) Then
	          zFilename = (TRIM(ADJUSTL(zOptionalString)) // '_' // TRIM(ADJUSTL(zFilename)) )
	       End if
!
!--------------------------------------------------------------------------------	
! Open file - This subroutine is in this file - line 517
!--------------------------------------------------------------------------------	
!
	       call SetUpDataFile(zFilename,             &
                       	          qFormattedFiles,       &
	                          tWriteAData(iRe_A_CG)%zVariable, &
			          tWriteAData(iRe_A_CG)%tFileType, &
			          qOKL)
    
	      
	       zFilename = (TRIM(ADJUSTL(tWriteAData(iIm_A_CG)%zVariable)) // TRIM(ADJUSTL(zDataFileName))) 
	       If (qOptional) Then
	          zFilename = (TRIM(ADJUSTL(zOptionalString)) // '_' // TRIM(ADJUSTL(zFilename)) )
	       End if

	
	       call SetUpDataFile(zFilename,             &
                       	          qFormattedFiles,       &
	                          tWriteAData(iIm_A_CG)%zVariable, &
			          tWriteAData(iIm_A_CG)%tFileType, &
			          qOKL)
	
	  END IF
	END IF
!
!--------------------------------------------------------------------------------	
!  z Data file         
!--------------------------------------------------------------------------------	
!				  
				  
	 If (tWriteZData%qWrite) Then			  
				  
	       zFilename = (TRIM(ADJUSTL(tWriteZData%zVariable)) // TRIM(ADJUSTL(zDataFileName))) 
	       If (qOptional) Then
	          zFilename = (TRIM(ADJUSTL(zOptionalString)) // '_' // TRIM(ADJUSTL(zFilename)) )
	       End if

           
           IF (tProcInfo_G%qRoot) THEN
	         call SetUpDataFile(zFilename,             &
                                qFormattedFiles,       &
	                            tWriteZData%zVariable, &
			                    tWriteZData%tFileType, &
			                    qOKL)
               If (.NOT. qOKL) Goto 1000
           End If
!	   	 
	 End if
!      

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
1000 call Error_log('Error in EArrayFunctions:SetUpDataFiles',tErrorLog_G)
   Print*,'Error in EArrayFunctions:SetUpDataFiles'
2000 CONTINUE
!	      
      END SUBROUTINE SetUpDataFiles
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
      SUBROUTINE CloseDataFiles(tArraySegment,  &
                                qOK)
!
!********************************************************************
! close data files
!********************************************************************
!
! tArraySegment           - UPDATE   - Result data file info
! qOK                     - OUTPUT   - Error flag
!
!********************************************************************
!	
      IMPLICIT NONE
!
      TYPE(cArraySegment),     INTENT(INOUT)             :: tArraySegment(:)
      LOGICAL,                 INTENT(OUT)               :: qOK      
!
!====================================================================
! Define local variables
! 
! iSegment - Local loop variable
! qOKL   - Local error flag
!=====================================================================
!	
       INTEGER(KIND=IP)               :: iSegment
       LOGICAL                        :: qOKL
!
!--------------------------------------------------------------------------------	
! Set error flag to false         
!--------------------------------------------------------------------------------	
!
      qOK = .FALSE.    
!
!--------------------------------------------------------------------------------	
! Close all required files - ''CloseFile'' subroutine is in "IO.f90"      
!--------------------------------------------------------------------------------	
!
      If (tProcInfo_G%qROOT) Then

      Do iSegment = 1_IP, Size(tArraySegment)
         If (tArraySegment(iSegment)%qWrite) Then
!
	    Call CloseFile(tArraySegment(iSegment)%tFileType,qOKL)
            If (.NOT. qOKL) Goto 1000
!
	 End If
      End Do
!
      End If
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
1000 call Error_log('Error in EArrayFunctions:CloseDataFiles',tErrorLog_G)
   Print*,'Error in EArrayFunctions:CloseDataFiles'
2000 CONTINUE
!
	      
      END SUBROUTINE CloseDataFiles

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
      SUBROUTINE  SetUpDataFile(zDataFileName,  &
                                qFormattedFiles,&
                                zVariable,      &
                                tYDataFile,     &
                                qOK)
!
!********************************************************************
! Set up data file to receive information
!********************************************************************
!
! zDataFileName           - INPUT    - Data file name
! qFormattedFiles         - INPUT    - if output data files to be formatted or binary
! zVariable               - INPUT    - Variable to write
! tYDataFile              - UPDATE   - Result data file info
! qOK                     - OUTPUT   - Error flag
!
!********************************************************************
!	
      IMPLICIT NONE
!
      CHARACTER(32_IP),        INTENT(IN)                :: zDataFileName
      LOGICAL,                 INTENT(IN)                :: qFormattedFiles
      CHARACTER(32_IP),        INTENT(IN)                :: zVariable
      TYPE(cFileType),         INTENT(INOUT)             :: tYDataFile
      LOGICAL,                 INTENT(OUT)               :: qOK      
!
!====================================================================
! Define local variables
! 
! qOKL   - Local error flag
!=====================================================================
!	
       
       LOGICAL                        :: qOKL
!
!--------------------------------------------------------------------------------	
! Set error flag to false         
!--------------------------------------------------------------------------------	
!
      qOK = .FALSE.    
!
!--------------------------------------------------------------------------------	
! Open the file to receive data output - This subroutine is in "IO.f90"        
!--------------------------------------------------------------------------------	
!
      tYDataFile%qFormatted = qFormattedFiles
!
      call InitialiseSDDSFile(zDataFileName,     &
      		       	      tYDataFile, &
			      qOKL)
      If (.NOT. qOKL) Goto 1000
!
!--------------------------------------------------------------------------------	
! Write variables names that are going to be written to the files (in order of output)  
! This subroutine is in "GSddsWriter.f90"
!--------------------------------------------------------------------------------	
!
      call SddsWriteColumn(zVariable,'double',tFileType=tYDataFile)	   
!
!--------------------------------------------------------------------------------	
! Write data mode - This subroutine is in "GSddsWriter.f90"
!--------------------------------------------------------------------------------	
!
      If (tYDataFile%qFormatted) Then
         call SddsWriteDataMode('ascii',tFileType=tYDataFile)	   
      else
         call SddsWriteDataMode('binary',tFileType=tYDataFile)	   
      End if
      
      !END IF
!
!--------------------------------------------------------------------------------	
!  Close the file         
!--------------------------------------------------------------------------------	
!
      call CloseFile(tYDataFile,qOKL)
      If (.NOT. qOKL) Goto 1000
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
1000 call Error_log('Error in EArrayFunctions:SetUpDataFile',tErrorLog_G)
   Print*,'Error in EArrayFunctions:SetUpDataFile'
2000 CONTINUE
!	      
      END SUBROUTINE SetUpDataFile
!--------------------------------------------------------------------------------	
!--------------------------------------------------------------------------------	
      SUBROUTINE SetPointer(iPointer,       &
                            iSize,          &
                            iStartPosition, &
                            iEndPosition,   &
		            qOK)
!
!********************************************************************
! Set up pointer
!********************************************************************
!
! iPointer           - INPUT  - Pointer to set        
! iSize	             - INPUT  - Length of array for pointer
! iStartPosition(:)  - UPDATE - Array holding pointer start positions
! iEndPosition(:)    - UPDATE - Array holding pointer end positions
! qOK                - OUTPUT - Error flag
!
!====================================================================
! Define local variables
!
!=====================================================================
!
      INTEGER(KIND=IP),       INTENT(IN)                 :: iPointer	
      INTEGER(KIND=IPL),       INTENT(IN)                 :: iSize	
      INTEGER(KIND=IPL),       INTENT(INOUT)              :: iStartPosition(:)	
      INTEGER(KIND=IPL),       INTENT(INOUT)              :: iEndPosition(:)	
      LOGICAL,                INTENT(OUT)                :: qOK
!
!--------------------------------------------------------------------------------	
! Set error flag to false         
!--------------------------------------------------------------------------------	
!
      qOK = .FALSE.   
!
!--------------------------------------------------------------------------------	
! Set Pointer start and end position
!--------------------------------------------------------------------------------	
!
      If (iPointer <= 0_IP) Goto  1000
      If (iPointer > Size(iStartPosition)) Goto 1000
      If (iPointer > Size(iEndPosition)) Goto  1000       
!
      If (iPointer == 1_IP .OR. iPointer == 3_IP) Then
         iStartPosition(iPointer) = 1_IP
      else
         iStartPosition(iPointer) = iEndPosition(iPointer - 1_IP) + 1_IP
      end if
!   
      iEndPosition(iPointer)      = iStartPosition(iPointer) + iSize - 1_IP
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
1000 call Error_log('Error in EArrayFunctions:SetPointer',tErrorLog_G)
   Print*,'Error in EArrayFunctions:SetPointer'
2000 CONTINUE
!
      END SUBROUTINE SetPointer      
End Module ArrayFunctions
