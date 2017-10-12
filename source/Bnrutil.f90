! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

MODULE nrutil

! Module to deal with writing out error messages 


USE paratype

IMPLICIT NONE

INTERFACE assert_eq
   MODULE PROCEDURE assert_eq2, assert_eq3, assert_eq4, assert_eqn
END INTERFACE       

INTERFACE assert
   MODULE PROCEDURE assert1, assert2
END INTERFACE assert


CONTAINS
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

FUNCTION assert_eq2(n1, n2, string)
!
!********************************************************************
! Report and die if INTEGER(KIND=IP)s not all equal (used for size checking)
!********************************************************************
   INTEGER(KIND=IP)              :: assert_eq2
   INTEGER(KIND=IP), INTENT(IN)  :: n1, n2
   CHARACTER(LEN=*), INTENT(IN)  :: string
	 
   if (n1 == n2) then
      assert_eq2 = n1
   else
      write(*,*) 'nrerror: an assert_eq failed:', string
      STOP 'program terminated by assert_eq2'
   end if
      
END FUNCTION assert_eq2
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

FUNCTION assert_eq3(n1, n2, n3, string)
!
!********************************************************************
! Report and die if INTEGER(KIND=IP)s not all equal (used for size checking)
!********************************************************************
   INTEGER(KIND=IP)              :: assert_eq3
   INTEGER(KIND=IP), INTENT(IN)  :: n1, n2, n3
   CHARACTER(LEN=*), INTENT(IN)  :: string
	 
   if (n1 == n2 .and. n2==n3) then
      assert_eq3 = n1
   else
      write(*,*) 'nrerror: an assert_eq failed:', string
      STOP 'program terminated by assert_eq3'
   end if
      
END FUNCTION assert_eq3

FUNCTION assert_eq4(n1, n2, n3, n4, string)
!
!********************************************************************
! Report and die if INTEGER(KIND=IP)s not all equal (used for size checking)
!********************************************************************
   INTEGER(KIND=IP)              :: assert_eq4
   INTEGER(KIND=IP), INTENT(IN)  :: n1, n2, n3, n4
   CHARACTER(LEN=*), INTENT(IN)  :: string
	 
   if (n1 == n2 .and. n2==n3 .and. n3==n4) then
      assert_eq4 = n1
   else
      write(*,*) 'nrerror: an assert_eq failed:',  string
      STOP 'program terminated by assert_eq4'
   end if
      
END FUNCTION assert_eq4
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

FUNCTION assert_eqn(nn, string)
!
!********************************************************************
! Report and die if INTEGER(KIND=IP)s not all equal (used for size checking)
!********************************************************************
   INTEGER(KIND=IP)                            :: assert_eqn
   INTEGER(KIND=IP), DIMENSION(:), INTENT(IN)  :: nn
   CHARACTER(LEN=*),               INTENT(IN)  :: string
	 
   if (all(nn(2:) == nn(1))) then
      assert_eqn = nn(1)
   else
      write(*,*) 'nrerror: an assert_eq failed:',  string
      STOP 'program terminated by assert_eqn'
   end if
      
END FUNCTION assert_eqn
SUBROUTINE assert1(n1,string)

IMPLICIT NONE

	CHARACTER(LEN=*), INTENT(IN)		:: string
	LOGICAL,INTENT(IN)			:: n1

	IF (.NOT. n1) THEN

		WRITE(*,*) 'nrerror: an assertion failed with this tag:', string
		STOP 'program terminated by assert1'
	ENDIF 

END SUBROUTINE assert1
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

SUBROUTINE assert2(n1,n2,string)

IMPLICIT NONE

	CHARACTER(LEN=*), INTENT(IN)		:: string
	LOGICAL,INTENT(IN)			:: n1,n2

	IF (.NOT. (n1 .and. n2)) THEN

		WRITE(*,*) 'nrerror: an assertion failed with this tag:', string
		STOP 'program terminated by assert2'
	ENDIF 

END SUBROUTINE assert2
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

SUBROUTINE nrerror(string)

	!Report a message, then die.
	CHARACTER(LEN=*), INTENT(IN) 		:: string

	write (*,*) 'nrerror: ',string

	STOP 'program terminated by nrerror'

END SUBROUTINE nrerror
!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

END MODULE nrutil
