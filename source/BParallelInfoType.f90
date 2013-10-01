MODULE ParallelInfoType

      USE paratype
		
      IMPLICIT NONE
!
!*****************************************************
! Originally created by Cynthia Nam

! Updated- Lawrence Campbell
! Place -  University of Strathclyde
!          Glasgow
!          Scotland
! Date -   24/11/2008
!
!
!
!
!*****************************************************
!
      TYPE cParallelInfoType
 	 INTEGER           :: comm = 0_IP
	 INTEGER           :: rank = 0_IP
	 INTEGER           :: size = 0_IP
	 LOGICAL           :: QROOT = .FALSE.
      END TYPE cParallelInfoType      
!
!====================================================================
! Define Global variables
!
! tProcInfo_G - Parallel processing information
!
!=====================================================================
!
!
      TYPE(cParallelInfoType) :: tProcInfo_G
      SAVE tProcInfo_G

END MODULE ParallelInfoType
