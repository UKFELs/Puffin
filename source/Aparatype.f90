! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Definitions of data types used in Puffin


MODULE paratype

use, intrinsic :: iso_fortran_env

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
INTEGER, PARAMETER     ::      IPN     =int64

INTEGER, PARAMETER     ::      LGT     =KIND(.true.)	

END MODULE paratype


