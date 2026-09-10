! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> FFTW3 bindings for the parallel build.
!>
!> Compiled when Puffin is configured with -DENABLE_PARALLEL=ON. It exposes
!> the stock FFTW3-MPI Fortran interface; fftw3-mpi.f03 itself includes
!> fftw3.f03, so both the serial and the distributed entry points come in
!> together. The serial build supplies a module of the same name and with
!> the same public interface in backends/serial, so nothing downstream has
!> to know which one it got.

module puffin_fftw3

  use, intrinsic :: iso_c_binding, only: C_CHAR, C_DOUBLE, C_DOUBLE_COMPLEX, &
    C_FLOAT, C_FLOAT_COMPLEX, C_FUNPTR, C_INT, C_INT32_T, C_INTPTR_T, C_PTR, C_SIZE_T

  implicit none (type, external)
public

  include 'fftw3-mpi.f03'

end module puffin_fftw3
