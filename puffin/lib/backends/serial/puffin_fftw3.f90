! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> FFTW3 bindings for the serial build.
!>
!> Compiled when Puffin is configured with -DENABLE_PARALLEL=OFF. It brings
!> in the plain (non-MPI) FFTW3 interface from fftw3.f03, and adds the four
!> fftw_mpi_* entry points that Puffin's transform layer calls, implemented
!> in terms of their serial counterparts. That keeps transforms.f90 free of
!> build-mode branching: it goes on calling fftw_mpi_plan_dft_3d and friends
!> whichever backend is in use, and only libfftw3 is needed to link, not
!> libfftw3_mpi.
!>
!> The mapping is exact for one rank. FFTW-MPI distributes a 3D transform
!> over the slowest (first, C-order) dimension n0, so on a single process
!> the local size is the whole array, the offset is zero, and the plan is
!> an ordinary in-place serial plan over the same dimensions.

module puffin_fftw3

  use, intrinsic :: iso_c_binding, only: C_CHAR, C_DOUBLE, C_DOUBLE_COMPLEX, &
    C_FLOAT, C_FLOAT_COMPLEX, C_FUNPTR, C_INT, C_INT32_T, C_INTPTR_T, C_PTR, C_SIZE_T

  implicit none (type, external)
public

  include 'fftw3.f03'

contains

!> No distributed planner to start up.

  subroutine fftw_mpi_init()

    implicit none (type, external)

  end subroutine fftw_mpi_init


!> Local data distribution of an n0 x n1 x n2 complex transform. With one
!> rank the whole transform is local, so the caller allocates the full
!> array and starts at offset zero.

  integer(C_INTPTR_T) function fftw_mpi_local_size_3d(n0, n1, n2, comm, &
                                                      local_n0, local_0_start)

    implicit none (type, external)

    integer(C_INTPTR_T), value :: n0
    integer(C_INTPTR_T), value :: n1
    integer(C_INTPTR_T), value :: n2
    integer(C_INT32_T), value :: comm
    integer(C_INTPTR_T), intent(out) :: local_n0
    integer(C_INTPTR_T), intent(out) :: local_0_start

    local_n0 = n0
    local_0_start = 0_C_INTPTR_T

    fftw_mpi_local_size_3d = n0 * n1 * n2

  end function fftw_mpi_local_size_3d


!> Plan a 3D complex transform. Dimensions are passed in the same order as
!> the distributed planner uses, n0 slowest, so this is a straight handover
!> to the serial planner.

  type(C_PTR) function fftw_mpi_plan_dft_3d(n0, n1, n2, in, out, comm, sign, flags)

    implicit none (type, external)

    integer(C_INTPTR_T), value :: n0
    integer(C_INTPTR_T), value :: n1
    integer(C_INTPTR_T), value :: n2
    complex(C_DOUBLE_COMPLEX), dimension(*), intent(out) :: in
    complex(C_DOUBLE_COMPLEX), dimension(*), intent(out) :: out
    integer(C_INT32_T), value :: comm
    integer(C_INT), value :: sign
    integer(C_INT), value :: flags

    fftw_mpi_plan_dft_3d = fftw_plan_dft_3d(int(n0, C_INT), int(n1, C_INT), &
                                            int(n2, C_INT), in, out, sign, flags)

  end function fftw_mpi_plan_dft_3d


!> Execute a plan on new arrays.

  subroutine fftw_mpi_execute_dft(p, in, out)

    implicit none (type, external)

    type(C_PTR), value :: p
    complex(C_DOUBLE_COMPLEX), dimension(*), intent(inout) :: in
    complex(C_DOUBLE_COMPLEX), dimension(*), intent(out) :: out

    call fftw_execute_dft(p, in, out)

  end subroutine fftw_mpi_execute_dft

end module puffin_fftw3
