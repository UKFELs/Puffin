! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Definitions of data types used in Puffin

module puffin_kinds

    use, intrinsic :: iso_fortran_env

    implicit none

!              Definitions of data types
!                  used in Puffin

    integer, parameter :: short   = selected_int_kind(4), &
                          long    = selected_int_kind(9), &
                          spec    = selected_int_kind(14), &
                          float   = selected_real_kind(P=6),&
                          double  = selected_real_kind(P=14)

    integer, parameter :: SP  = float
    integer, parameter :: WP  = double
    integer, parameter :: IPL = long
    integer, parameter :: IP  = long
    integer, parameter :: LP  = long
    integer, parameter :: IPN = int64

    integer, parameter :: LGT = kind(.true.)	

end module puffin_kinds


