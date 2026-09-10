! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Parallel-HDF5 entry points for the serial build.
!>
!> Compiled when Puffin is configured with -DENABLE_PARALLEL=OFF. HDF5 only
!> defines h5pset_fapl_mpio_f and h5pset_dxpl_mpio_f when it has itself been
!> built against MPI, so a serial build cannot import them. It does not need
!> to either: on one rank the MPI-IO file driver and the default POSIX one
!> write byte-identical files, and a collective transfer degenerates to an
!> independent one. So both property-list calls become no-ops here and the
!> file access and transfer property lists are left at their defaults.
!>
!> Everything else in Puffin's HDF5 usage is already serial-safe and is
!> imported from the HDF5 module as normal. In particular the hyperslab
!> selections are untouched, so the layout of the files written by a serial
!> build matches the parallel one exactly.

module puffin_h5_par

  use hdf5, only: HID_T

  implicit none (type, external)
private

  public :: H5FD_MPIO_COLLECTIVE_F, H5FD_MPIO_INDEPENDENT_F, &
             h5pset_dxpl_mpio_f, h5pset_fapl_mpio_f


!     Data transfer modes. The values are arbitrary - h5pset_dxpl_mpio_f
!     below is the only thing that ever sees them, and it ignores them.

  integer, parameter :: H5FD_MPIO_INDEPENDENT_F = 0
  integer, parameter :: H5FD_MPIO_COLLECTIVE_F = 1

contains

!> Would select the MPI-IO file driver. With one rank the default driver
!> produces the same file, so leave the property list alone.

  subroutine h5pset_fapl_mpio_f(prp_id, comm, info, hdferr)

    implicit none (type, external)

    integer(kind=HID_T), intent(in) :: prp_id
    integer, intent(in)  :: comm
    integer, intent(in)  :: info
    integer, intent(out) :: hdferr

    hdferr = 0

  end subroutine h5pset_fapl_mpio_f


!> Would set collective or independent transfer. Both mean the same thing
!> when there is only one rank taking part.

  subroutine h5pset_dxpl_mpio_f(prp_id, data_xfer_mode, hdferr)

    implicit none (type, external)

    integer(kind=HID_T), intent(in) :: prp_id
    integer, intent(in)  :: data_xfer_mode
    integer, intent(out) :: hdferr

    hdferr = 0

  end subroutine h5pset_dxpl_mpio_f

end module puffin_h5_par
