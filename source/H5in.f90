module H5in

use paratype
USE ParallelInfoType
use globals
use ParallelSetUp
use parBeam
use paraField
use scale
use HDF5
use initDataType

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

    use parBeam

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
    integer(kind=ip) :: mpiinfo, mpierr

    mpiinfo=MPI_INFO_NULL
    filename = zfile ! unless this is naughty due to different length
    ! could filename need a trim
    ! The below speaks for a single processor read, but that's going to 
    ! be the same in either case while we just read the size of the array 

    ! note above rank is not preset to 2, as we want to set it.

    CALL h5open_f(error)
!    Print*,'h5in:H5 interface opened'
!      print*,'reading hdf5 input, need info on all processors'
    CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
!      Print*,'hdf5_puff:outputH5BeamSD(property created)'
!      Print*,error
      CALL h5pset_fapl_mpio_f(plist_id, tProcInfo_G%comm, mpiinfo, error)
!      Print*,'hdf5_puff:outputH5BeamSD(property set up)'
!      Print*,error
      CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error, access_prp = plist_id)
!      Print*,'h5in:readH5Beamfile(file opened in parallel)'
!      Print*,error
      CALL h5dopen_f (file_id, dsetname, dset_id, error)
!      Print*,'h5in:readH5Beamfile(dataset opened in parallel)'
!      Print*,error
      CALL h5dget_type_f (dset_id, dtype, error)
!      Print*,'h5in:readH5Beamfile(checking data type)'
!      Print*,error
      CALL h5tget_class_f (dtype, dclass, error)
!      Print*,'h5in:readH5Beamfile(dataset opened in parallel)'
!      Print*,error
!      if (dclass==H5T_NATIVE_DOUBLE) then
      
      if (dclass==H5T_FLOAT_F) then
!       print*,'data is float'
      else
        errorstr = trim("data is no float")
        print*,dclass
        goto 1000
      end if

      if (qresume_G) then
      
        call readH5FloatAttribute(dset_id, "zbarTotal", tInitData_G%zbarTotal)
        call readH5FloatAttribute(dset_id, "zbarInter", tInitData_G%Zbarinter)
        call readH5FloatAttribute(dset_id, "zbarLocal", tInitData_G%zbarlocal)

        call readH5IntegerAttribute(dset_id, "iCsteps", tInitData_G%iCsteps)
        call readH5IntegerAttribute(dset_id, "istep", tInitData_G%iStep)
      
        call readH5IntegerAttribute(dset_id, "iUnd_cr", tInitData_G%iUnd_cr)
        call readH5IntegerAttribute(dset_id, "iChic_cr", tInitData_G%iChic_cr)
        call readH5IntegerAttribute(dset_id, "iDrift_cr", tInitData_G%iDrift_cr)
        call readH5IntegerAttribute(dset_id, "iQuad_cr", tInitData_G%iQuad_cr)
        call readH5IntegerAttribute(dset_id, "iModulation_cr", tInitData_G%iModulation_cr)
        call readH5IntegerAttribute(dset_id, "iL", tInitData_G%iL)

      end if
      
      CALL h5Dget_space_f(dset_id,dspace_id,error)
!      Print*,'h5in:readH5Beamfile(dataspace opened in parallel)'
!      Print*,error
      CALL h5Sget_simple_extent_ndims_f(dspace_id,rank,error)
!      Print*,'h5in:readH5Beamfile(dataspace opened in parallel)'
!      Print*,rank
    !  Print*,error
      if (rank==2) then
!       print*,'data rank is 2, which is good'
      else
        errorstr = trim("data does not have rank 2, has rank")
        goto 1000
      end if
      
      call h5Sget_simple_extent_dims_f(dspace_id,dims,mdims,error)

!      Print*,'hdf5_puff:readH5Beamfile(dataspace getting dims)'
!      Print*,error ! rank on success = 2
!      print*,dims

      if (dims(1)==7) then
        !print*,'data has seven columns, which is good'
      else
        errorstr = trim("data does not have seven columns")
        goto 1000
      end if

     nMPs=dims(2)
!     print*,"number of particles in file: "
!     print*, nMPs
     CALL h5pclose_f(plist_id, error)
     call h5sclose_f(dspace_id,error) !dspace_id
!     print*,error
!     print*,"h5s closed"
     if (nMPs .LT. tProcInfo_G%size) then
      errorstr = trim("Data has fewer particles than MPI ranks.")
      goto 1000
     end if

! we used to close the file down here, then reopen a little later

!   CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
! In the VERY short term, we'll just read on rank 0
!   CALL h5pset_fapl_mpio_f(plist_id, tProcInfo_G%comm, mpiinfo, error)

! Allocate local MP arrays

    call mpi_barrier(tProcInfo_G%comm, mpierr)

    call divMPs(nMPs, tProcInfo_g%size, tProcInfo_g%rank, &
                nMPsLoc, firstParticleToRead, lastParticleToRead)

    call mpi_barrier(tProcInfo_G%comm, mpierr)                  
                  
!    firstParticleToRead=(nMPs*tProcInfo_g%rank/tProcInfo_g%size)+1 !does integer arithmetic, no NINT needed
!    lastParticleToRead=(nMPs*(tProcInfo_g%rank+1)/tProcInfo_g%size) ! does integer arithmetic
!    nMPsLoc=(lastParticleToRead-firstParticleToRead)+1
    iNumberElectrons_G=nMPsLoc

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
!     print*,error
!     print*,"h5s mem created"
!      CALL h5screate_simple_f(rank, dsize, filespace, error)
!      CALL h5screate_simple_f(rank, dsize, dspace_id, error)
!     print*,error
!     print*,"h5s file created"
      CALL h5Dget_space_f(dset_id,dspace_id,error)
!     print*,error
!     print*,"h5d space got"

      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
!      CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, doffset, &
!       dims, error)
!     print*,error
!     print*,"h5s slab selected"
     call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error) 
     CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error)

      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, sElX_G, dims, error, &
       xfer_prp = plist_id, file_space_id = dspace_id, mem_space_id = memspace)
!     print*,error
!     print*,"h5d slab read"
!      call h5sclose_f(dspace_id,error)
!     print*,error
!     print*,"h5s dspace closed"

      doffset=(/1,(firstParticleToRead-1)/)
      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
!     print*,error
!     print*,"h5s slab1 selected"
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, sElY_G, dims, error, &
       xfer_prp = plist_id, file_space_id = dspace_id, mem_space_id = memspace)
!     print*,error
!     print*,"h5d slab1 read"
!      call h5sclose_f(dspace_id,error)

      doffset=(/2,(firstParticleToRead-1)/)
      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
!     print*,"h5s slab2 selected"
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, sElZ2_G, dims, error, &
       xfer_prp = plist_id, file_space_id = dspace_id, mem_space_id = memspace)
!     print*,error
!     print*,"h5d slab2 read"
!      call h5sclose_f(dspace_id,error)

      doffset=(/3,(firstParticleToRead-1)/)
      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, sElPX_G, dims, error, &
       xfer_prp = plist_id, file_space_id = dspace_id, mem_space_id = memspace)
!     print*,error
!     print*,"h5d slab3 read"
!      call h5sclose_f(dspace_id,error)

      doffset=(/4,(firstParticleToRead-1)/)
      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, sElPY_G, dims, error, &
       xfer_prp = plist_id, file_space_id = dspace_id, mem_space_id = memspace)
!     print*,error
!     print*,"h5d slab4 read"
!      call h5sclose_f(dspace_id,error)

      doffset=(/5,(firstParticleToRead-1)/)
      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, sElGam_G, dims, error, &
       xfer_prp = plist_id, file_space_id = dspace_id, mem_space_id = memspace)
!     print*,error
!     print*,"h5d slab5 read"
!      call h5sclose_f(dspace_id,error)

      doffset=(/6,(firstParticleToRead-1)/)
      CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset, &
       dims, error)
      CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, s_chi_bar_G, dims, error, &
       xfer_prp = plist_id, file_space_id = dspace_id, mem_space_id = memspace)
!     print*,error
!     print*,"h5d slab6 read"
      call h5sclose_f(dspace_id,error)
!     print*,"h5s dspace_id closed"


      call h5sclose_f(memspace,error)
!     print*,"h5s memspace closed"
!      call h5sclose_f(filespace,error)
      call h5dclose_f(dset_id,error)
!     print*,"h5d dset closed"
      call h5fclose_f(file_id,error)
!     print*,"h5f file_id closed"
     call h5pclose_f(plist_id,error)
!     print*,error
!     print*,"h5p prop list closed"
    CALL h5close_f(error)
!     print*,"h5 interface closed"
!
! Low quality smoke test
!
!print*,"Low grade smoke test"
!print*,SElx_G(10)
!print*,S_chi_bar_G(20)
!print*,"Now, you tell me if we had electrons"

    CALL MPI_ALLREDUCE(iNumberElectrons_G, iGloNumElectrons_G, &
                       1, MPI_INTEGER, &
                       MPI_SUM, MPI_COMM_WORLD, error)



    GoTo 2000

! Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call Error_log('Error in H5in:readH5BeamFile',&
          tErrorLog_G)
   print*, "abort, abort, Error in readH5Beamfile",errorstr
   
2000 continue

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
    INTEGER(HSIZE_T), DIMENSION(2) :: dims1d   !< dims of field dataset (NZ2_G*components)
    INTEGER(HSIZE_T), DIMENSION(4) :: dims3d   !< dims of field dataset (NX_G,NY_G,NZ2_G,components)
    INTEGER(HSIZE_T), DIMENSION(2) :: mdims1d   !< maxdims of field dataset (coords*numcomps)
    INTEGER(HSIZE_T), DIMENSION(4) :: mdims3d   !< maxdims of field dataset (coords*numcomps)
    INTEGER(HSIZE_T), DIMENSION(2) :: dsize1d   !< chunk of field file to read
    INTEGER(HSIZE_T), DIMENSION(4) :: dsize3d   !< chunk of field file to read
    INTEGER(HSIZE_T), DIMENSION(2) :: doffset1d   !< maxdims of ptcl dataset (coords*numelecs)
    INTEGER(HSIZE_T), DIMENSION(4) :: doffset3d   !< maxdims of ptcl dataset (coords*numelecs)
    INTEGER(HID_T) :: group_id

    INTEGER(kind=ip) :: loopindex
    CHARACTER(LEN=5), PARAMETER :: dsetname = "aperp"     ! Dataset name
    character(1024_IP) :: filename
    integer :: error !< Error flag
    character(LEN=40) :: errorstr !<String to write an error
    integer(kind=ip) :: mpiinfo
    integer(kind=ip) :: DUMMYgg

    mpiinfo=MPI_INFO_NULL
    filename = zFile
    CALL h5open_f(error)
    !Print*,'h5in:H5 interface opened'
    !  print*,'reading hdf5 input'
    CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
    !  Print*,error,tprocinfo_g%rank, &
    !    'h5in:readH5FieldfileSingleDump(property created)'
      CALL h5pset_fapl_mpio_f(plist_id, tProcInfo_G%comm, mpiinfo, error)
!      Print*,'hdf5_puff:outputH5BeamSD(property set up)'
!      Print*,error
      CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file_id, error, access_prp = plist_id)
    !  Print*,error,tprocinfo_g%rank, &
    !    'h5in:readH5fieldfile(file opened in parallel)'
      CALL h5dopen_f (file_id, dsetname, dset_id, error)
    !  Print*,error,tprocinfo_g%rank, &
    !    'hdf5_puff:readH5fieldfile(dataset opened in parallel)'
      CALL h5dget_type_f (dset_id, dtype, error)
    !  Print*,error,tprocinfo_g%rank, &
    !    'hdf5_puff:readH5fieldfile(checking data type)'
      CALL h5tget_class_f (dtype, dclass, error)
    !  Print*,error,tprocinfo_g%rank, &
    !    'hdf5_puff:readH5fieldfile(dataset opened in parallel)'
!      if (dclass==H5T_NATIVE_DOUBLE) then
      if (dclass==H5T_FLOAT_F) then
    !   print*,'data is float'
      else
      errorstr = trim("data is no float")
      print*,dclass
      goto 1000
      end if


      call readH5IntegerAttribute(dset_id, "iCsteps", DUMMYgg)
!(dset_id, "iCsteps", iCsteps, aspace_id)
      PRINT*, 'TESTING, the att is iCSteps and it = ', DUMMYgg
!      call mpi_finalize(tProcInfo_G%comm, error)
!      STOP


!     open the runInfo group

      call h5gOpen_f(file_id, 'runInfo', group_id, error)


!      read attributes in runInfo group

      call readH5FloatAttribute(group_id, "sLengthOfElmX", sLengthOfElmX_G)
      call readH5FloatAttribute(group_id, "sLengthOfElmY", sLengthOfElmY_G)
      call readH5FloatAttribute(group_id, "sLengthOfElmZ2", sLengthOfElmZ2_G)

      PRINT*, 'TESTING, the att is dx and it = ', sLengthOfElmX_G
      PRINT*, 'TESTING, the att is dy and it = ', sLengthOfElmY_G
      PRINT*, 'TESTING, the att is dz2 and it = ', sLengthOfElmZ2_G
      
      delta_G = sLengthOfElmX_G*sLengthOfElmY_G*sLengthOfElmZ2_G
      

!     Close runInfo group
      
      CALL h5gclose_f(group_id, error)
      
      
      
      
      
      
      CALL h5Dget_space_f(dset_id,dspace_id,error)
    !  Print*,error,tprocinfo_g%rank, &
    !    'hdf5_puff:readH5Fieldfile(dataspace opened in parallel)'
      CALL h5Sget_simple_extent_ndims_f(dspace_id,rank,error)
    !  Print*,error,tprocinfo_g%rank, &
    !    'hdf5_puff:readH5Fieldfile(dataspace opened in parallel)'
        
      if (rank == 2) then
    !    print*, "Seems to be 1D input field"
        
        if (qoned_g) then
          ! Do further checks
    !      print*, "checking size"
          CALL h5Sget_simple_extent_dims_f(dspace_id,dims1d,mdims1d,error)
    !      Print*,'hdf5_puff:readH5FieldFile(dataspace getting dims)'
          
    !      Do loopindex=1,rank
    !        print*,"rank ", tProcInfo_G%rank, "  dim: ",loopindex,&
    !          ":",dims1d(loopindex)
    !      end do
          
          if (dims1d(1)==NZ2_G) then
    !        print*,"NZ2 dims in file match"
          else
            print*,"NZ2 in .in:", NZ2_G, ", NZ2 in h5 file:", dims1d(1)
            errorstr="NZ2 dims mismatch between in and h5"
            goto 1000
          end if 
          
          if (dims1d(2)==2) then
    !        print*,"expected num components (2) present in hdf5 file"
          else
            print*,"Wrong number of components in h5 field file:", &
              dims1d(2)
            errorstr="num components incorrect in h5 field"
            goto 1000
          end if 

          CALL h5pclose_f(plist_id, error)

          ! Do some reading
          doffset1d=(/(ffs-1),0/)
          dsize1d=(/tlflen,1/)      
              
          CALL h5screate_simple_f(rank, dsize1d, memspace, error)
    !      print*,error,tprocinfo_g%rank,"h5s fr  memspace created"
          
          CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset1d, &
            dsize1d, error)
    !      print*,error,tprocinfo_g%rank,"h5s slab fr_rfield selected"


          call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error) 
          CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error)
          
          CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, fr_rfield, dsize1d, error, &
           xfer_prp = plist_id, file_space_id = dspace_id, mem_space_id = memspace)
    !      print*,error,tprocinfo_g%rank,"h5d slab fr_rfield read"
          
! use same memspace again
          doffset1d=(/(ffs-1),1/)
          CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset1d, &
            dsize1d, error)
    !      print*,error,tprocinfo_g%rank,"h5s slab fr_ifield selected"
          
          CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, fr_ifield, dsize1d, error, &
           xfer_prp = plist_id, file_space_id = dspace_id, mem_space_id = memspace)
    !      print*,error,tprocinfo_g%rank,"h5d slab fr_ifield read"
          
          call h5sclose_f(memspace,error)
    !      print*,error,tprocinfo_g%rank,"h5s fr memspace closed"

          doffset1d=(/(fz2-1),0/)
          dsize1d=(/mainlen,1/) 
                   
          CALL h5screate_simple_f(rank, dsize1d, memspace, error)
    !      print*,error,tprocinfo_g%rank,"h5s ac memspace created"
          
          CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset1d, &
            dsize1d, error)
    !      print*,error,tprocinfo_g%rank,"h5s slab ac_rfield selected"
          
          CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, ac_rfield, dsize1d, error, &
           xfer_prp = plist_id, file_space_id = dspace_id, mem_space_id = memspace)
    !      print*,error,tprocinfo_g%rank,"h5d slab ac_rfield read"
          
! use same memspace again
          doffset1d=(/(fz2-1),1/)
          CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset1d, &
            dsize1d, error)
    !      print*,error,tprocinfo_g%rank,"h5s slab ac_ifield selected"
          
          CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, ac_ifield, dsize1d, error, &
           xfer_prp = plist_id, file_space_id = dspace_id, mem_space_id = memspace)
    !      print*,error,tprocinfo_g%rank,"h5d slab ac_ifield read"
          
          call h5sclose_f(memspace,error)
    !      print*,error,tprocinfo_g%rank,"h5s ac memspace closed"

          doffset1d=(/(ees-1),0/)
          dsize1d=(/tlelen,1/)          
          CALL h5screate_simple_f(rank, dsize1d, memspace, error)
    !      print*,error,tprocinfo_g%rank,"h5s back memspace created"
          
          CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset1d, &
            dsize1d, error)
    !      print*,error,tprocinfo_g%rank,"h5s slab bk_rfield selected"
          
          CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, bk_rfield, dsize1d, error, &
           xfer_prp = plist_id, file_space_id = dspace_id, mem_space_id = memspace)
    !      print*,error,tprocinfo_g%rank,"h5d slab bk_rfield read"
          
! use same memspace again
          doffset1d=(/(ees-1),1/)
          CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset1d, &
            dsize1d, error)
    !      print*,error,tprocinfo_g%rank,"h5s slab bk_ifield selected"
          
          CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, bk_ifield, dsize1d, error, &
           xfer_prp = plist_id, file_space_id = dspace_id, mem_space_id = memspace)
    !      print*,error,tprocinfo_g%rank,"h5d slab bk_ifield read"
          
          CALL h5pclose_f(plist_id, error)
          call h5sclose_f(memspace,error)
    !      print*,error,tprocinfo_g%rank,"h5s back memspace closed"


        else

          errorstr=trim("2D input (z2+comp), but not a 1D sim")
          print*,"Abort - 1D + component input but not 1D sim."
          goto 1000
        end if
      else if (rank == 4) then
    !    print*, "Seems to be 3D input field"
        if (.not. qoned_g) then
        ! Do some reading
    !      print*, "checking size"
          CALL h5Sget_simple_extent_dims_f(dspace_id,dims3d,mdims3d,error)
    !      Print*,'hdf5_puff:readH5fieldFile(dataspace getting dims)'
    !      Do loopindex=1,rank
    !        print*,"rank ", tProcInfo_G%rank, "  dim: ",loopindex,&
    !          ":",dims3d(loopindex)
    !      end do
          if (dims3d(1)==NX_G) then
    !        print*,"NX dims in file match"
          else
            print*,"NX in .in:", NX_G, ", NX in h5 file:", dims3d(1)
            errorstr="NX dims mismatch between in and h5"
            goto 1000
          end if 
          if (dims3d(2)==NY_G) then
    !        print*,"NY dims in file match"
          else
            print*,"NY in .in:", NY_G, ", NY in h5 file:", dims3d(2)
            errorstr="NY dims mismatch between in and h5"
            goto 1000
          end if 
          if (dims3d(3)==NZ2_G) then
    !        print*,"NZ2 dims in file match"
          else
            print*,"NZ2 in .in:", NZ2_G, ", NZ2 in h5 file:", dims3d(3)
            errorstr="NZ2 dims mismatch between in and h5"
            goto 1000
          end if 
          if (dims3d(4)==2) then
    !        print*,"expected number of components present"
          else
            print*,"Wrong number of components in h5 field file:", &
              dims3d(4)
            errorstr="num components incorrect in h5 field"
            goto 1000
          end if  

          CALL h5pclose_f(plist_id, error)
          
          
          doffset3d=(/0,0,(ffs-1),0/)
          dsize3d=(/NX_G,NY_G,tlflen,1/)          
          
          CALL h5screate_simple_f(rank, dsize3d, memspace, error)
    !      print*,error,tprocinfo_g%rank,"h5s memspace 3d created"
          
          CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset3d, &
            dsize3d, error)
    !      print*,error,tprocinfo_g%rank,"h5s slab fr_rfield 3d selected"

          call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error) 
          CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error)

          CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, fr_rfield, dsize3d, error, &
           xfer_prp = plist_id, file_space_id = dspace_id, mem_space_id = memspace)
    !      print*,error,tprocinfo_g%rank,"h5d slab fr_rfield 3d read"

! keep memspace
          doffset3d=(/0,0,(ffs-1),1/)
          
          CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset3d, &
            dsize3d, error)
      !    print*,error,tprocinfo_g%rank,"h5s slab fr_ifield 3d selected"
          
          CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, fr_ifield, dsize3d, error, &
           xfer_prp = plist_id, file_space_id = dspace_id, mem_space_id = memspace)
      !    print*,error,tprocinfo_g%rank,"h5d slab fr_ifield 3d read"
          
          call h5sclose_f(memspace,error)
      !    print*,error,tprocinfo_g%rank,"h5s memspace 3d closed"

          doffset3d=(/0,0,(fz2-1),0/)
          dsize3d=(/NX_G,NY_G,mainlen,1/)          
          CALL h5screate_simple_f(rank, dsize3d, memspace, error)
      !    print*,error,tprocinfo_g%rank,"h5s memspace 3d created"
          
          CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset3d, &
            dsize3d, error)
      !    print*,error,tprocinfo_g%rank,"h5s slab ac_rfield 3d selected"
          
          CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, ac_rfield, dsize3d, error, &
           xfer_prp = plist_id, file_space_id = dspace_id, mem_space_id = memspace)
      !    print*,error,tprocinfo_g%rank,"h5d slab ac_rfield 3d read"
          
! keep memspace
          doffset3d=(/0,0,(fz2-1),1/)
          
          CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset3d, &
            dsize3d, error)
      !    print*,error,tprocinfo_g%rank,"h5s slab ac_rfield 3d selected"
          
          CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, ac_ifield, dsize3d, error, &
           xfer_prp = plist_id, file_space_id = dspace_id, mem_space_id = memspace)
      !    print*,error,tprocinfo_g%rank,"h5d slab ac_rfield 3d read"
          
          call h5sclose_f(memspace,error)
      !    print*,error,tprocinfo_g%rank,"h5s memspace 3d closed"

          doffset3d=(/0,0,(ees-1),0/)
          dsize3d=(/NX_G,NY_G,tlelen,1/)
          CALL h5screate_simple_f(rank, dsize3d, memspace, error)
      !    print*,error,tprocinfo_g%rank,"h5s back memspace 3d created"
          
          CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset3d, &
            dsize3d, error)
      !    print*,error,tprocinfo_g%rank,"h5s slab bk_rfield 3d selected"
          
          CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, bk_rfield, dsize3d, error, &
           xfer_prp = plist_id, file_space_id = dspace_id, mem_space_id = memspace)
      !    print*,error,tprocinfo_g%rank,"h5d slab bk_rfield 3d read"
          
! keep memspace
          doffset3d=(/0,0,(ees-1),1/)
          
          CALL h5sselect_hyperslab_f(dspace_id, H5S_SELECT_SET_F, doffset3d, &
            dsize3d, error)
      !    print*,error,tprocinfo_g%rank,"h5s slab bk_rfield 3d selected"
          
          CALL h5dread_f(dset_id, H5T_NATIVE_DOUBLE, bk_ifield, dsize3d, error, &
           xfer_prp = plist_id, file_space_id = dspace_id, mem_space_id = memspace)
      !    print*,error,tprocinfo_g%rank,"h5d slab bkrfield 3d read"
          
          CALL h5pclose_f(plist_id, error)
          call h5sclose_f(memspace,error)
      !    print*,error,tprocinfo_g%rank,"h5s back memspace 3d closed"

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
      !print*,error,tprocinfo_g%rank,"h5s memspace dspace_id closed"
      call h5dclose_f(dset_id,error)
      !print*,"h5d dset closed"

      !call h5pclose_f(plist_id,error)
!      print*,error
!      print*,"h5p prop list closed"

      call h5fclose_f(file_id,error)
      !print*,"h5f file_id closed"
    CALL h5close_f(error)
     print*,"h5 interface closed"
    GoTo 2000

! Error Handler - Error log Subroutine in CIO.f90 line 709
1000 call Error_log('Error in H5in:readH5FieldfileSerialSingleDump',&
          tErrorLog_G)
   print*, "abort, abort, Error in readH5FieldfileSerialSingleDump",errorstr
2000 CONTINUE
  end subroutine readH5FieldfileSingleDump
  




  subroutine readH5IntegerAttribute(locHandle,attrName,attrValue)
    
    implicit none

    integer(HID_T), intent(in) :: locHandle   !< h5 handle of write location
    character(LEN=*), intent(in) :: attrName  !<attrib name
    integer(kind=ip), intent(inout) :: attrValue !<attrib value
!    integer(HID_T), intent(in) :: aspace_id   !< Attribute Dataspace identifier
    ! Local vars

    integer(HID_T) :: attr_id                 !< Attribute identifier
    integer(HID_T) :: atype_id                !< Attribute Data type identifier
    integer(HSIZE_T) :: attr_string_len       !< Length of attribute string 
    integer(HSIZE_T), dimension(1) :: adims=(/1/) !< Attribute Data type identifier
    integer :: error                             !< Error flag

!    aname="vsType"
!    attr_data_string="vsVars"

    call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
    !call h5acreate_f(locHandle, attrName, atype_id, aspace_id, attr_id, error)
    call H5Aopen_name_f(locHandle, attrName, attr_id, error)
    !call h5awrite_f(attr_id, atype_id, attrValue, adims, error) 
    call h5aread_f(attr_id, atype_id, attrValue, adims, error)
    !call h5aread_f(attr_id, memtype_id, buf, dims, hdferr)
    call h5aclose_f(attr_id, error)
    call h5tclose_f(atype_id, error)

  end subroutine readH5IntegerAttribute

  
  
  

  subroutine readH5FloatAttribute(locHandle,attrName,attrValue)
    
    implicit none

    integer(HID_T), intent(in) :: locHandle   !< h5 handle of write location
    character(LEN=*), intent(in) :: attrName  !<attrib name
    real(kind=wp), intent(inout) :: attrValue !<attrib value
!    integer(HID_T), intent(in) :: aspace_id   !< Attribute Dataspace identifier
    ! Local vars

    integer(HID_T) :: attr_id                 !< Attribute identifier
    integer(HID_T) :: atype_id                !< Attribute Data type identifier
    integer(HSIZE_T) :: attr_string_len       !< Length of attribute string 
    integer(HSIZE_T), dimension(1) :: adims=(/1/) !< Attribute Data type identifier
    integer :: error                             !< Error flag

!    aname="vsType"
!    attr_data_string="vsVars"

    call h5tcopy_f(H5T_NATIVE_DOUBLE, atype_id, error)
    !call h5acreate_f(locHandle, attrName, atype_id, aspace_id, attr_id, error)
    call H5Aopen_name_f(locHandle, attrName, attr_id, error)
    !call h5awrite_f(attr_id, atype_id, attrValue, adims, error) 
    call h5aread_f(attr_id, atype_id, attrValue, adims, error)
    !call h5aread_f(attr_id, memtype_id, buf, dims, hdferr)
    call h5aclose_f(attr_id, error)
    call h5tclose_f(atype_id, error)

  end subroutine readH5FloatAttribute
  
  
end module H5in
