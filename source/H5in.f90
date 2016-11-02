module H5in

use paratype
USE ParallelInfoType
use globals
use ParallelSetUp
use parBeam
use scale
use HDF5

implicit none



contains


  subroutine readH5BeamfileSerial(zFile)


    character(*), intent(in) :: zFile
    INTEGER(HID_T) :: file_id       !< File identifier
    INTEGER(HID_T) :: dset_id       !< Dataset identifier 
    INTEGER(HID_T) :: dspace_id     !< Dataspace identifier in memory
    INTEGER(HID_T) :: dtype         !< So we can check we're reading in doubles
    INTEGER(HID_T) :: dclass         !< So we can check we're reading in doubles
    INTEGER(HID_T) :: filespace     !< Dataspace identifier in file
    INTEGER(HID_T) :: memspace     !< Dataspace identifier in file
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
    INTEGER(HSIZE_T), DIMENSION(2) :: mdims   !< maxdims of ptcl dataset (coords*numelecs)
    INTEGER(HSIZE_T), DIMENSION(2) :: doffset!< Offset for write, could be rank dependent
    INTEGER(HSIZE_T), DIMENSION(2) :: dsize  !< Size of hyperslab to write
    INTEGER(kind=ip) ::  rank                !< Particle Dataset rank
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
    integer(kind=ip) :: nMPs,nMPsLoc,firstParticleToRead
    integer(kind=ip) :: mpiinfo

    mpiinfo=MPI_INFO_NULL
    filename = zfile ! unless this is naughty due to different length
    ! could filename need a trim
    ! The below speaks for a single processor read, but that's going to 
    ! be the same in either case while we just read the size of the array 

      if (tProcInfo_G%qRoot) then 
    CALL h5open_f(error)
    Print*,'h5in:H5 interface opened'
      print*,'reading hdf5 input - first opening file on rank 0'
      CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error)
      Print*,'h5in:readH5Beamfile(file opened in serial)'
      Print*,error
      CALL h5dopen_f (file_id, dsetname, dset_id, error)
      Print*,'h5in:readH5Beamfile(dataset opened in serial)'
      Print*,error
      CALL h5dget_type_f (dset_id, dtype, error)
      Print*,'h5in:readH5Beamfile(checking data type)'
      Print*,error
      CALL h5tget_class_f (dtype, dclass, error)
      Print*,'h5in:readH5Beamfile(dataset opened in serial)'
      Print*,error
!      if (dclass==H5T_NATIVE_DOUBLE) then
      if (dclass==H5T_FLOAT_F) then
       print*,'data is float'
      else
      errorstr = trim("data is no float")
      print*,dclass
      goto 1000
      end if
      CALL h5Dget_space_f(dset_id,dspace_id,error)
      Print*,'h5in:readH5Beamfile(dataspace opened in serial)'
      Print*,error
      CALL h5Sget_simple_extent_ndims_f(dspace_id,rank,error)
      Print*,'h5in:readH5Beamfile(dataspace opened in serial)'
      Print*,rank
      Print*,error
      if (rank==2) then
       print*,'data rank is 2, which is good'
      else
      errorstr = trim("data does not have rank 2, has rank")
      goto 1000
      end if
      CALL h5Sget_simple_extent_dims_f(dspace_id,dims,mdims,error)
      Print*,'hdf5_puff:readH5Beamfile(dataspace getting dims)'
      Print*,error ! rank on success = 2
      print*,dims
      if (dims(1)==7) then
       print*,'data has seven columns, which is good'
      else
      errorstr = trim("data does not have seven columns")
      goto 1000
      end if
     nMPs=dims(2)
     print*,"number of particles in file: "
     print*, nMPs
     call h5sclose_f(dspace_id,error) !dspace_id
     print*,error
     print*,"h5s closed"
     call h5dclose_f(dset_id,error)
     print*,error
     print*,"h5d closed"
     call h5fclose_F(file_id,error)
     print*,error
     print*,"h5f closed"

   end if
!   CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
! In the VERY short term, we'll just read on rank 0
!   CALL h5pset_fapl_mpio_f(plist_id, tProcInfo_G%comm, mpiinfo, error)

! Allocate local MP arrays

      if (tProcInfo_G%qRoot) then 
    nMPsLoc=nMPs
iNumberElectrons_G=nMPs
else
nMPsLoc=0
iNumberElectrons_G=0
end if

    allocate(sElX_G(nMPsLoc),   &
             sElY_G(nMPsLoc),   &
             sElZ2_G(nMPsLoc),  &
             sElPX_G(nMPsLoc),  &
             sElPY_G(nMPsLoc),  &
             sElGam_G(nMPsLoc), &
             s_chi_bar_G(nMPsLoc), &
             s_Normalised_chi_G(nMPsLoc))
! open up the file and read
      if (tProcInfo_G%qRoot) then 
      CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error)
     print*,error
     print*,"h5f open"
      CALL h5dopen_f (file_id, dsetname, dset_id, error)
     print*,error
     print*,"h5d open"
      dims=(/1,nMPs/)
      dsize=(/7,nMPs/)
      doffset=(/0,0/)
      CALL h5screate_simple_f(rank, dims, memspace, error)
     print*,error
     print*,"h5s mem created"
!      CALL h5screate_simple_f(rank, dsize, filespace, error)
!      CALL h5screate_simple_f(rank, dsize, dspace_id, error)
     print*,error
     print*,"h5s file created"
      CALL h5Dget_space_f(dset_id,dspace_id,error)
     print*,error
     print*,"h5d space got"

      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
!      CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, doffset, &
!       dims, error)
     print*,error
     print*,"h5s slab selected"
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, sElX_G, dims, error, &
       file_space_id = dspace_id, mem_space_id = memspace)
     print*,error
     print*,"h5d slab read"
!      call h5sclose_f(dspace_id,error)
!     print*,error
!     print*,"h5s dspace closed"

      doffset=(/1,0/)
      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
     print*,error
     print*,"h5s slab1 selected"
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, sElY_G, dims, error, &
       file_space_id = dspace_id, mem_space_id = memspace)
     print*,error
     print*,"h5d slab1 read"
!      call h5sclose_f(dspace_id,error)

      doffset=(/2,0/)
      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
     print*,"h5s slab2 selected"
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, sElZ2_G, dims, error, &
       file_space_id = dspace_id, mem_space_id = memspace)
     print*,error
     print*,"h5d slab2 read"
!      call h5sclose_f(dspace_id,error)

      doffset=(/3,0/)
      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, sElPX_G, dims, error, &
       file_space_id = dspace_id, mem_space_id = memspace)
     print*,error
     print*,"h5d slab3 read"
!      call h5sclose_f(dspace_id,error)

      doffset=(/4,0/)
      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, sElPY_G, dims, error, &
       file_space_id = dspace_id, mem_space_id = memspace)
     print*,error
     print*,"h5d slab4 read"
!      call h5sclose_f(dspace_id,error)

      doffset=(/5,0/)
      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, sElGam_G, dims, error, &
       file_space_id = dspace_id, mem_space_id = memspace)
     print*,error
     print*,"h5d slab5 read"
!      call h5sclose_f(dspace_id,error)

      doffset=(/6,0/)
      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, s_chi_bar_G, dims, error, &
       file_space_id = dspace_id, mem_space_id = memspace)
     print*,error
     print*,"h5d slab6 read"
      call h5sclose_f(dspace_id,error)
     print*,"h5s dspace_id closed"


      call h5sclose_f(memspace,error)
     print*,"h5s memspace closed"
!      call h5sclose_f(filespace,error)
      call h5dclose_f(dset_id,error)
     print*,"h5d dset closed"
      call h5fclose_f(file_id,error)
     print*,"h5f file_id closed"
    CALL h5close_f(error)
     print*,"h5 interface closed"
!
! Low quality smoke test
!
print*,"Low grade smoke test"
print*,SElx_G(100)
print*,S_chi_bar_G(200)
print*,"Now, you tell me if we had electrons"
      end if !not on root any more.

    CALL MPI_ALLREDUCE(iNumberElectrons_G, iGloNumElectrons_G, &
                       1, MPI_INTEGER, &
                       MPI_SUM, MPI_COMM_WORLD, error)
    GoTo 2000

! Error Handler - Error log Subroutine in CIO.f90 line 709
1000 call Error_log('Error in H5in:readH5BeamFile',&
          tErrorLog_G)
   print*, "abort, abort, Error in readH5Beamfile",errorstr
2000 CONTINUE
  end subroutine readH5BeamfileSerial




  subroutine readH5Beamfile(zFile)

    character(*), intent(in) :: zFile
    INTEGER(HID_T) :: file_id       !< File identifier
    INTEGER(HID_T) :: dset_id       !< Dataset identifier 
    INTEGER(HID_T) :: dspace_id     !< Dataspace identifier in memory
    INTEGER(HID_T) :: dtype         !< So we can check we're reading in doubles
    INTEGER(HID_T) :: dclass         !< So we can check we're reading in doubles
    INTEGER(HID_T) :: filespace     !< Dataspace identifier in file
    INTEGER(HID_T) :: memspace     !< Dataspace identifier in file
    INTEGER(HID_T) :: attr_id       !< Attribute identifier
    INTEGER(HID_T) :: aspace_id     !< Attribute Dataspace identifier
    INTEGER(HID_T) :: atype_id      !< Attribute Data type identifier
    INTEGER(HID_T) :: plist_id      !< (parallel) Property list identifier
!    INTEGER(HID_T) :: group_id      !< Group identifier
!    logical, intent(in) :: qSeparate !<May reinstitute this.
    CHARACTER(LEN=9), PARAMETER :: dsetname = "electrons" !< Dataset name
    CHARACTER(LEN=30) :: aname   !< Attribute name
    character(1024_IP) :: filename
!    logical, intent(inout) :: qOK
!    INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/iGloNumElectrons_G/) ! Dataset dimensions
    INTEGER(HSIZE_T), DIMENSION(2) :: dims   !< dims of ptcl dataset (coords*numelecs)
    INTEGER(HSIZE_T), DIMENSION(2) :: mdims   !< maxdims of ptcl dataset (coords*numelecs)
    INTEGER(HSIZE_T), DIMENSION(2) :: doffset!< Offset for write, could be rank dependent
    INTEGER(HSIZE_T), DIMENSION(2) :: dsize  !< Size of hyperslab to write
    INTEGER(kind=ip)  ::  rank                 !< Particle Dataset rank
    INTEGER(kind=ip)  ::  arank = 1                !< Attribute rank - 1 is vector
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
    integer :: error !< Error flag
    character(LEN=40) :: errorstr !<String to write an error
    integer(kind=ip) :: nMPs
    integer(kind=ip) :: nMPsLoc,firstParticleToRead,lastParticleToRead
    integer(kind=ip) :: mpiinfo

    mpiinfo=MPI_INFO_NULL
    filename = zfile ! unless this is naughty due to different length
    ! could filename need a trim
    ! The below speaks for a single processor read, but that's going to 
    ! be the same in either case while we just read the size of the array 

    ! note above rank is not preset to 2, as we want to set it.

    CALL h5open_f(error)
    Print*,'h5in:H5 interface opened'
      print*,'reading hdf5 input, need info on all processors'
    CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
!      Print*,'hdf5_puff:outputH5BeamSD(property created)'
!      Print*,error
      CALL h5pset_fapl_mpio_f(plist_id, tProcInfo_G%comm, mpiinfo, error)
!      Print*,'hdf5_puff:outputH5BeamSD(property set up)'
!      Print*,error
      CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error, access_prp = plist_id)
      Print*,'h5in:readH5Beamfile(file opened in parallel)'
      Print*,error
      CALL h5dopen_f (file_id, dsetname, dset_id, error)
      Print*,'h5in:readH5Beamfile(dataset opened in parallel)'
      Print*,error
      CALL h5dget_type_f (dset_id, dtype, error)
      Print*,'h5in:readH5Beamfile(checking data type)'
      Print*,error
      CALL h5tget_class_f (dtype, dclass, error)
      Print*,'h5in:readH5Beamfile(dataset opened in parallel)'
      Print*,error
!      if (dclass==H5T_NATIVE_DOUBLE) then
      if (dclass==H5T_FLOAT_F) then
       print*,'data is float'
      else
      errorstr = trim("data is no float")
      print*,dclass
      goto 1000
      end if
      CALL h5Dget_space_f(dset_id,dspace_id,error)
      Print*,'h5in:readH5Beamfile(dataspace opened in parallel)'
      Print*,error
      CALL h5Sget_simple_extent_ndims_f(dspace_id,rank,error)
      Print*,'h5in:readH5Beamfile(dataspace opened in parallel)'
      Print*,rank
      Print*,error
      if (rank==2) then
       print*,'data rank is 2, which is good'
      else
      errorstr = trim("data does not have rank 2, has rank")
      goto 1000
      end if
      CALL h5Sget_simple_extent_dims_f(dspace_id,dims,mdims,error)
      Print*,'hdf5_puff:readH5Beamfile(dataspace getting dims)'
      Print*,error ! rank on success = 2
      print*,dims
      if (dims(1)==7) then
       print*,'data has seven columns, which is good'
      else
      errorstr = trim("data does not have seven columns")
      goto 1000
      end if
     nMPs=dims(2)
     print*,"number of particles in file: "
     print*, nMPs
     call h5sclose_f(dspace_id,error) !dspace_id
     print*,error
     print*,"h5s closed"
     if (nMPs .LT. tProcInfo_G%size) then
      errorstr = trim("Data has fewer particles than MPI ranks.")
      goto 1000
     end if

! we used to close the file down here, then reopen a little later

!   CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
! In the VERY short term, we'll just read on rank 0
!   CALL h5pset_fapl_mpio_f(plist_id, tProcInfo_G%comm, mpiinfo, error)

! Allocate local MP arrays
    firstParticleToRead=(nMPs*tProcInfo_g%rank/tProcInfo_g%size)+1 !does integer arithmetic, no NINT needed
    lastParticleToRead=(nMPs*(tProcInfo_g%rank+1)/tProcInfo_g%size) ! does integer arithmetic
    nMPsLoc=(lastParticleToRead-firstParticleToRead)+1
    iNumberElectrons_G=nMPs

    allocate(sElX_G(nMPsLoc),   &
             sElY_G(nMPsLoc),   &
             sElZ2_G(nMPsLoc),  &
             sElPX_G(nMPsLoc),  &
             sElPY_G(nMPsLoc),  &
             sElGam_G(nMPsLoc), &
             s_chi_bar_G(nMPsLoc), &
             s_Normalised_chi_G(nMPsLoc))
! since now we are leaving the file opened up
! reopening should not be necessary
!      CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error, access_prp = plist_id)
!     print*,error
!     print*,"h5f open"
!      CALL h5dopen_f (file_id, dsetname, dset_id, error)
!     print*,error
!     print*,"h5d open"
      dims=(/1,nMPsLoc/)
      dsize=(/7,nMPsLoc/)
      doffset=(/0,(firstParticleToRead-1)/)
      CALL h5screate_simple_f(rank, dims, memspace, error)
     print*,error
     print*,"h5s mem created"
!      CALL h5screate_simple_f(rank, dsize, filespace, error)
!      CALL h5screate_simple_f(rank, dsize, dspace_id, error)
     print*,error
     print*,"h5s file created"
      CALL h5Dget_space_f(dset_id,dspace_id,error)
     print*,error
     print*,"h5d space got"

      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
!      CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, doffset, &
!       dims, error)
     print*,error
     print*,"h5s slab selected"
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, sElX_G, dims, error, &
       file_space_id = dspace_id, mem_space_id = memspace)
     print*,error
     print*,"h5d slab read"
!      call h5sclose_f(dspace_id,error)
!     print*,error
!     print*,"h5s dspace closed"

      doffset=(/1,(firstParticleToRead-1)/)
      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
     print*,error
     print*,"h5s slab1 selected"
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, sElY_G, dims, error, &
       file_space_id = dspace_id, mem_space_id = memspace)
     print*,error
     print*,"h5d slab1 read"
!      call h5sclose_f(dspace_id,error)

      doffset=(/2,(firstParticleToRead-1)/)
      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
     print*,"h5s slab2 selected"
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, sElZ2_G, dims, error, &
       file_space_id = dspace_id, mem_space_id = memspace)
     print*,error
     print*,"h5d slab2 read"
!      call h5sclose_f(dspace_id,error)

      doffset=(/3,(firstParticleToRead-1)/)
      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, sElPX_G, dims, error, &
       file_space_id = dspace_id, mem_space_id = memspace)
     print*,error
     print*,"h5d slab3 read"
!      call h5sclose_f(dspace_id,error)

      doffset=(/4,(firstParticleToRead-1)/)
      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, sElPY_G, dims, error, &
       file_space_id = dspace_id, mem_space_id = memspace)
     print*,error
     print*,"h5d slab4 read"
!      call h5sclose_f(dspace_id,error)

      doffset=(/5,(firstParticleToRead-1)/)
      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, sElGam_G, dims, error, &
       file_space_id = dspace_id, mem_space_id = memspace)
     print*,error
     print*,"h5d slab5 read"
!      call h5sclose_f(dspace_id,error)

      doffset=(/6,(firstParticleToRead-1)/)
      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, s_chi_bar_G, dims, error, &
       file_space_id = dspace_id, mem_space_id = memspace)
     print*,error
     print*,"h5d slab6 read"
      call h5sclose_f(dspace_id,error)
     print*,"h5s dspace_id closed"


      call h5sclose_f(memspace,error)
     print*,"h5s memspace closed"
!      call h5sclose_f(filespace,error)
      call h5dclose_f(dset_id,error)
     print*,"h5d dset closed"
      call h5fclose_f(file_id,error)
     print*,"h5f file_id closed"
     call h5pclose_f(plist_id,error)
     print*,error
     print*,"h5p prop list closed"
    CALL h5close_f(error)
     print*,"h5 interface closed"
!
! Low quality smoke test
!
print*,"Low grade smoke test"
print*,SElx_G(10)
print*,S_chi_bar_G(20)
print*,"Now, you tell me if we had electrons"

    CALL MPI_ALLREDUCE(iNumberElectrons_G, iGloNumElectrons_G, &
                       1, MPI_INTEGER, &
                       MPI_SUM, MPI_COMM_WORLD, error)
    GoTo 2000

! Error Handler - Error log Subroutine in CIO.f90 line 709
1000 call Error_log('Error in H5in:readH5BeamFile',&
          tErrorLog_G)
   print*, "abort, abort, Error in readH5Beamfile",errorstr
2000 CONTINUE
  end subroutine readH5Beamfile

  subroutine readH5FieldfileSingleDump(zFile)
    character(*), intent(in) :: zFile
    INTEGER(HID_T) :: file_id       !< File identifier
    INTEGER(HID_T) :: dset_id       !< Dataset identifier 
    INTEGER(HID_T) :: dspace_id     !< Dataspace identifier in memory
    INTEGER(HID_T) :: plist_id      !< (parallel) Property list identifier
    INTEGER(HID_T) :: dtype         !< So we can check we're reading in doubles
    INTEGER(HID_T) :: dclass         !< So we can check we're reading in doubles
    INTEGER(HID_T) :: filespace     !< Dataspace identifier in file
    INTEGER(HID_T) :: memspace     !< Dataspace identifier in file
    INTEGER(kind=ip) ::  rank       !< Field file Dataset rank
    INTEGER(HSIZE_T), DIMENSION(2) :: dims1d   !< dims of ptcl dataset (NZ2_G*components)
    INTEGER(HSIZE_T), DIMENSION(4) :: dims3d   !< dims of ptcl dataset (NX_G,NY_G,NZ2_G,components)
    INTEGER(HSIZE_T), DIMENSION(2) :: mdims1d   !< maxdims of ptcl dataset (coords*numelecs)
    INTEGER(HSIZE_T), DIMENSION(4) :: mdims3d   !< maxdims of ptcl dataset (coords*numelecs)
    INTEGER(kind=ip) :: loopindex
    CHARACTER(LEN=5), PARAMETER :: dsetname = "aperp"     ! Dataset name
    character(1024_IP) :: filename
    integer :: error !< Error flag
    character(LEN=40) :: errorstr !<String to write an error
    integer(kind=ip) :: mpiinfo

    mpiinfo=MPI_INFO_NULL
    filename = zFile
    CALL h5open_f(error)
    Print*,'h5in:H5 interface opened'
      print*,'reading hdf5 input'
    CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
      Print*,'h5in:readH5FieldfileSingleDump(property created)'
      Print*,error
      CALL h5pset_fapl_mpio_f(plist_id, tProcInfo_G%comm, mpiinfo, error)
!      Print*,'hdf5_puff:outputH5BeamSD(property set up)'
!      Print*,error
      CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error, access_prp = plist_id)
      Print*,'h5in:readH5fieldfile(file opened in parallel)'
      Print*,error
      CALL h5dopen_f (file_id, dsetname, dset_id, error)
      Print*,'hdf5_puff:readH5fieldfile(dataset opened in parallel)'
      Print*,error
      CALL h5dget_type_f (dset_id, dtype, error)
      Print*,'hdf5_puff:readH5fieldfile(checking data type)'
      Print*,error
      CALL h5tget_class_f (dtype, dclass, error)
      Print*,'hdf5_puff:readH5fieldfile(dataset opened in parallel)'
      Print*,error
!      if (dclass==H5T_NATIVE_DOUBLE) then
      if (dclass==H5T_FLOAT_F) then
       print*,'data is float'
      else
      errorstr = trim("data is no float")
      print*,dclass
      goto 1000
      end if
      CALL h5Dget_space_f(dset_id,dspace_id,error)
      Print*,'hdf5_puff:readH5Fieldfile(dataspace opened in parallel)'
      Print*,error
      CALL h5Sget_simple_extent_ndims_f(dspace_id,rank,error)
      Print*,'hdf5_puff:readH5Fieldfile(dataspace opened in parallel)'
      Print*,rank
      Print*,error
      if (rank == 2) then
        print*, "Seems to be 1D input field"
        if (qoned_g) then
          ! Do some reading
          print*, "checking size"
          CALL h5Sget_simple_extent_dims_f(dspace_id,dims1d,mdims1d,error)
          Print*,'hdf5_puff:readH5FieldFile(dataspace getting dims)'
          Do loopindex=1,rank
            print*,"rank ", tProcInfo_G%rank, "  dim: ",loopindex,&
              ":",dims1d(loopindex)
          end do
        else
          errorstr=trim("2D input (z2+comp), but not a 1D sim")
          print*,"Abort - 1D + component input but not 1D sim."
          goto 1000
        end if
      else if (rank == 4) then
        print*, "Seems to be 3D input field"
        if (.not. qoned_g) then
        ! Do some reading
          print*, "checking size"
          CALL h5Sget_simple_extent_dims_f(dspace_id,dims3d,mdims3d,error)
          Print*,'hdf5_puff:readH5fieldFile(dataspace getting dims)'
          Do loopindex=1,rank
            print*,"rank ", tProcInfo_G%rank, "  dim: ",loopindex,&
              ":",dims3d(loopindex)
          end do

        else
        errorstr=trim("4D input (3d+comp), but not 3D sim")
        print*,"Abort - 3D + component input but not 3D sim."
        goto 1000
        end if
      else
        print*,"The rank of this data is not what I expected at all"
        errorstr="Error with dimensionality of input data"
        goto 1000 
      end if
! rank should depend on whether we have 1D or 3D fields.

      call h5sclose_f(dspace_id,error)
     print*,"h5s memspace closed"
!      call h5sclose_f(filespace,error)
      call h5dclose_f(dset_id,error)
     print*,"h5d dset closed"

     call h5pclose_f(plist_id,error)
     print*,error
     print*,"h5p prop list closed"

      call h5fclose_f(file_id,error)
     print*,"h5f file_id closed"
    CALL h5close_f(error)
     print*,"h5 interface closed"
    GoTo 2000

! Error Handler - Error log Subroutine in CIO.f90 line 709
1000 call Error_log('Error in H5in:readH5FieldfileSerialSingleDump',&
          tErrorLog_G)
   print*, "abort, abort, Error in readH5FieldfileSerialSingleDump",errorstr
2000 CONTINUE
  end subroutine readH5FieldfileSingleDump
  
end module H5in
