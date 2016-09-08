module MASPin

use paratype
use globals
use ParallelSetUp
use parBeam
use scale
use HDF5
implicit none


integer(kind=ip) :: nMPs4MASP_G

contains


  subroutine readH5Beamfile(zFile)

    character(*), intent(in) :: zFile
    implicit none
    INTEGER(HID_T) :: file_id       !< File identifier
    INTEGER(HID_T) :: dset_id       !< Dataset identifier 
    INTEGER(HID_T) :: dspace_id     !< Dataspace identifier in memory
    INTEGER(HID_T) :: dtype         !< So we can check we're reading in doubles
    INTEGER(HID_T) :: dclass         !< So we can check we're reading in doubles
    INTEGER(HID_T) :: filespace     !< Dataspace identifier in file
    INTEGER(HID_T) :: attr_id       !< Attribute identifier
    INTEGER(HID_T) :: aspace_id     !< Attribute Dataspace identifier
    INTEGER(HID_T) :: atype_id      !< Attribute Data type identifier
!    INTEGER(HID_T) :: group_id      !< Group identifier
!    logical, intent(in) :: qSeparate !<May reinstitute this.
    CHARACTER(LEN=9), PARAMETER :: dsetname = "electrons" !< Dataset name
    CHARACTER(LEN=30) :: aname   !< Attribute name
    character(1024_IP) :: filename
!    logical, intent(inout) :: qOK
!    INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/iGloNumElectrons_G/) ! Dataset dimensions
    INTEGER(HSIZE_T), DIMENSION(2) :: dims   !< dims of ptcl dataset (coords*numelecs)
    INTEGER(HSIZE_T), DIMENSION(2) :: doffset!< Offset for write, could be rank dependent
    INTEGER(HSIZE_T), DIMENSION(2) :: dsize  !< Size of hyperslab to write
    INTEGER     ::  rank = 2                 !< Particle Dataset rank
    INTEGER     ::  arank = 1                !< Attribute rank - 1 is vector
    INTEGER(HSIZE_T), DIMENSION(1) :: adims  !< Attribute dims
    INTEGER(HSIZE_T), DIMENSION(1) :: attr_data_int !< For integer attribs (numdims)
    INTEGER     :: numSpatialDims    !< Attr content, and also num elsewhere  
!assumed 3D sim. May be 1D.
!    TYPE(C_PTR) :: f_ptr
    REAL(kind=WP) :: attr_data_double
    CHARACTER(LEN=100) :: attr_data_string
    CHARACTER(LEN=16) :: scaleToSIstring
    INTEGER(HSIZE_T) :: attr_string_len
    CHARACTER(LEN=4), PARAMETER :: timegrpname = "time"  ! Group name
    CHARACTER(LEN=12), PARAMETER :: limgrpname = "globalLimits"  ! Group name
    REAL(kind=WP), ALLOCATABLE :: limdata (:)  ! Data to write
    ! Local vars
    !integer(kind=ip) :: iep
    integer :: error ! Error flag
    character(LEN=40) :: errorstr !<String to write an error

    filename = zfile ! unless this is naughty due to different length
    ! could filename need a trim
    integer(kind=ip) :: nMPs
    CALL h5open_f(error)
      if (tProcInfo_G%qRoot) then 
    Print*,'h5in:H5 interface opened'
      print*,'reading hdf5 input - first opening file on rank 0'
      CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error)
      Print*,'hdf5_puff:outputH5Beamfile(file opened in serial)'
      Print*,error
      CALL h5dopen_f (file_id, dsetname, dset_id, error)
      Print*,'hdf5_puff:readH5Beamfile(dataset opened in serial)'
      Print*,error
      CALL h5get_type_f (dset_id, dtype, error)
      Print*,'hdf5_puff:readH5Beamfile(checking data type)'
      Print*,error
      CALL h5tget_class_f (dtype, dclass, error)
      Print*,'hdf5_puff:readH5Beamfile(dataset opened in serial)'
      Print*,error
      if (dclass==H5T_DOUBLE_F) then
       print*,'data is double precision'
      else
      errorstr = trim("data is not double float")
      goto 1000
      end if
      CALL h5Dget_space_f(dset_id,dspace_id,error)
      Print*,'hdf5_puff:readH5Beamfile(dataspace opened in serial)'
      Print*,error
      CALL h5Dget_simple_extent_ndims(dspace_id,rank,error)
      Print*,'hdf5_puff:readH5Beamfile(dataspace opened in serial)'
      Print*,error
      if (dclass==2) then
       print*,'data rank is 2, which is good'
      else
      errorstr = trim("data does not have rank 2")
      goto 1000
      end if
      CALL h5Dget_simple_extent_dims_f(dspace_id,dims,error)
      Print*,'hdf5_puff:readH5Beamfile(dataspace opened in serial)'
      Print*,error
      if (dims[2]==7) then
       print*,'data has seven columns, which is good'
      else
      errorstr = trim("data does not have seven columns")
      goto 1000
      end if
     print*,"number of particles in file: "
     print*, dims[1]
   end if

1000 print*, "abort, abort, in readH5Beamfile",errorstr
  end subroutine readH5Beamfile
