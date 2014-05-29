!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE FileType

      USE paratype
		
      IMPLICIT NONE
      
!
!*****************************************************
! File type definition
!
! mvar_qUnix       - Unix produced file
! mvar_qFortran    - Fortran written file
! mvar_qFormatted  - Formatted data
! mvar_qForInput   - File opened for input
! mvar_qSwap       - Byte swapped file
! mvar_qAppend     - File opened for append
! mvar_iPos        - Position in file
! mvar_iBufPos     - Position in buffer
! mvar_LastPos     - Previous position in file
! mvar_iPause      - Paused position in file
! mvar_iUnit       - Unit stream
! mvar_Include     - Include file level
! mvar_FileLength  - Length of the file
! mvar_FileName    - File name
! mvar_FileParent  - Parent file name
! mvar_zBuffer     - Input file buffer
! mvar_iPage       - Page number in file
!*****************************************************
 
      TYPE cFileType
         CHARACTER(32_IP)  :: zFileName = ''
	 LOGICAL           :: qFormatted = .False.
	 LOGICAL           :: qForInput = .False.
	 LOGICAL           :: qAppend = .False.
	 INTEGER(KIND=IP)  :: iPos = 0_IP
	 INTEGER(KIND=IP)  :: iUnit = 0_IP
	 INTEGER(KIND=IP)  :: iFileLength = 0_IP
	 INTEGER(KIND=IP)  :: iPage = 0_IP
      END TYPE cFileType

END MODULE FileType
