!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013-2016 University of Strathclyde               **!
!** Written by Lawrence Campbell and Brian McNeil.              **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Definitions of data types used in Puffin


MODULE paratype

IMPLICIT NONE

!              Definitions of data types
!                  used in Puffin

INTEGER, PARAMETER     ::      short   =SELECTED_INT_KIND(4), &
                               long    =SELECTED_INT_KIND(9), &
                               spec    =SELECTED_INT_KIND(14), &
                               float   =SELECTED_REAL_KIND(P=6),&
                               double  =SELECTED_REAL_KIND(P=14)

INTEGER, PARAMETER     ::      SP      =float
INTEGER, PARAMETER     ::      WP      =double
INTEGER, PARAMETER     ::      IPL     =long
INTEGER, PARAMETER     ::      IP      =long
INTEGER, PARAMETER     ::      LP      =long

INTEGER, PARAMETER     ::      LGT     =KIND(.true.)	

END MODULE paratype


