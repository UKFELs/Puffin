! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

module puffin_fftwInfo

   use puffin_kinds, only: ip
!  The FFTW3 bindings come from a build-mode-specific backend: the real
!  FFTW3-MPI interface for a parallel build, or plain FFTW3 plus one-rank
!  fftw_mpi_* shims for a serial one. See puffin/lib/backends.
   use puffin_fftw3, only: fftw_alloc_complex, FFTW_BACKWARD, fftw_destroy_plan, &
     FFTW_ESTIMATE, FFTW_FORWARD, fftw_free, FFTW_MEASURE, fftw_mpi_execute_dft, &
     fftw_mpi_init, fftw_mpi_local_size_3d, fftw_mpi_plan_dft_3d
   use, intrinsic :: iso_c_binding, only: C_INTPTR_T, C_PTR
   implicit none (type, external)
private

public :: fftw_alloc_complex, FFTW_BACKWARD, fftw_destroy_plan, FFTW_ESTIMATE, FFTW_FORWARD, &
           fftw_free, FFTW_MEASURE, fftw_mpi_execute_dft, fftw_mpi_init, fftw_mpi_local_size_3d, &
           fftw_mpi_plan_dft_3d, tTransInfo_G

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

   type fftwInfoType
      type(C_PTR)          :: fplan
      type(C_PTR)          :: bplan
      integer(kind=ip)     :: loc_nz2
      integer(kind=ip)     :: loc_z2_start
      integer(kind=ip)     :: loc_nz2_aft_trans
      integer(kind=ip)     :: loc_z2_start_aft_trans
      integer(kind=ip)     :: total_local_size
      logical              :: qOneD
   end type fftwInfoType

   type(fftwInfoType) :: tTransInfo_G
   save tTransInfo_G

end module puffin_fftwInfo
