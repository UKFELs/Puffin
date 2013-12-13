!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

MODULE TransformInfoType

      USE paratype
      
      IMPLICIT NONE
!      
!-----------------------------------------------------------------
! Author - Lawrence Campbell
! Place -  University of Strathclyde
!          Glasgow
!          Scotland
! Date -   24/11/2008
!
! Type definition: -
! Stores Information required for the FFTW transform data distributions
!
! 
! fplan - The forward transform FFTW-MPI plan
! bplan - The backward transform FFTW-MPI plan
! loc_nz2 - The number of nodes in z2 the local processor holds.
! loc_z2_start - The starting z2 value on which the local transform data begins.
! loc_ny_aft_trans - The number of nodes in y the local processor holds after the
!                    transform.
! loc_y_start_aft_trans - The starting y value on which the local transform data begins
!                         after the tranform.
! total_local_size - The total number of elements(nodes) the local processor needs 
!                    allocated to it.
!--------------------------------------------------------------------

      TYPE cTransformInfoType
         INTEGER*8		    :: fplan
         INTEGER*8		    :: bplan	 
 	 INTEGER(KIND=IP)           :: loc_nz2
	 INTEGER(KIND=IP)           :: loc_z2_start
	 INTEGER(KIND=IP)           :: loc_nz2_aft_trans
	 INTEGER(KIND=IP)	    :: loc_z2_start_aft_trans
	 INTEGER(KIND=IP)	    :: total_local_size
	 LOGICAL				:: qOneD
      END TYPE cTransformInfoType
      
!
!====================================================================
! Define Global variables
!
! tTransInfo_G - Transform distribution information
!
!=====================================================================
!
!
      TYPE(cTransformInfoType) :: tTransInfo_G
      SAVE tTransInfo_G

END MODULE TransformInfoType
       
      
      
