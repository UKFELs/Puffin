! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Parallel-HDF5 entry points for the parallel build.
!>
!> Compiled when Puffin is configured with -DENABLE_PARALLEL=ON. It is a
!> thin re-export of the four parallel-I/O names Puffin uses from HDF5, so
!> that the IO code imports them from here rather than from the HDF5 module
!> directly. The serial build supplies a module of the same name with the
!> same interface in backends/serial, and that is the point of the
!> indirection: a serial HDF5 library does not define these names at all,
!> so a bare `use hdf5, only: h5pset_fapl_mpio_f` would fail to compile
!> against one.

module puffin_h5_par

  use hdf5, only: H5FD_MPIO_COLLECTIVE_F, H5FD_MPIO_INDEPENDENT_F, &
    h5pset_dxpl_mpio_f, h5pset_fapl_mpio_f

  implicit none (type, external)
private

  public :: H5FD_MPIO_COLLECTIVE_F, H5FD_MPIO_INDEPENDENT_F, &
             h5pset_dxpl_mpio_f, h5pset_fapl_mpio_f

end module puffin_h5_par
