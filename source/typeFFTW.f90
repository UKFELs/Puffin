! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause


!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> This module contains the type definition to hold the info calculated by FFTW
!> for the parallel fourier transforms used in Puffin.

module TransformInfoType

      use paratype

      use, intrinsic :: iso_c_binding
      implicit none

      include 'fftw3-mpi.f03'

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

      type cTransformInfoType

         type(C_PTR)          :: fplan
         type(C_PTR)          :: bplan
         integer(kind=ip)     :: loc_nz2
         integer(kind=ip)     :: loc_z2_start
         integer(kind=ip)     :: loc_nz2_aft_trans
         integer(kind=ip)     :: loc_z2_start_aft_trans
         integer(kind=ip)     :: total_local_size
         logical              :: qOneD

      end type cTransformInfoType

      type(cTransformInfoType) :: tTransInfo_G
      save tTransInfo_G

end module TransformInfoType
