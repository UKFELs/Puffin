! Copyright 2012-2017, University of Strathclyde
! Authors: Jonathan Smith (Tech-X UK Ltd) & Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Jonathan Smith (Tech-X UK Ltd)
!> @brief
!> This module contains the routines for writing the dumps and integrated data
!> collectively in hdf5 format - so only a single file is written for each of
!> the field mesh, the macroparticles, and integrated data.
!>
!> Routines originally written by Jonathan Smith (Tech-X UK Ltd)

module hdf5PuffColl

use paratype
use hdf5
use globals
use ParallelInfoType
use lattice
USE ParallelSetUp
USE ArrayFunctions
USE TypesandConstants
Use avWrite
use hdf5PuffLow

contains

!> outputH5BeamFilesSD Output the electron bean macroparticle
!! 6D phase space coordinates (plus weight) in Puffin.
!! Places all data in a single rank
!! @params unused tArrayE global array (to this rank)
!! containing particles and layout of data in sV.
!! @params sElX_G, particle x coordinate
!! @params sElY_G, particle y coordinate
!! @params sElZ2_G, particle z2 (displacement from bunch centre)
!! @params iNumberElectrons_G number of electrons (global) on this rank
!! @todo Individual and collective writing to combined file to come
!! For collective write, we want to work out how many particles on
!! each rank, what the cumulative num electrons is, and then determine
!! the array slice based on that.
!! so instead of
  subroutine outputH5BeamFilesSD(time, sz_loc, iL, error)
    implicit none
    REAL(kind=WP),intent(in) :: time !< Current time
    REAL(kind=WP),intent(in) :: sz_loc
    integer(kind=ip), intent(in) :: iL  !< lattice element number
    INTEGER(HID_T) :: file_id       !< File identifier
    INTEGER(HID_T) :: dset_id       !< Dataset identifier
    INTEGER(HID_T) :: dspace_id     !< Dataspace identifier in memory
    INTEGER(HID_T) :: filespace     !< Dataspace identifier in file
    INTEGER(HID_T) :: plist_id      !< Property list identifier
    INTEGER(HID_T) :: attr_id       !< Attribute identifier
    INTEGER(HID_T) :: aspace_id     !< Attribute Dataspace identifier
    INTEGER(HID_T) :: atype_id      !< Attribute Data type identifier
    INTEGER(HID_T) :: group_id      !< Group identifier
!    logical, intent(in) :: qSeparate !<May reinstitute this.
    CHARACTER(LEN=9), PARAMETER :: dsetname = "electrons" !< Dataset name
    CHARACTER(LEN=30) :: aname   !< Attribute name
    character(1024_IP) :: filename !< Filename to write - 1024 chars is overkill but...
!    logical, intent(inout) :: qOK
!    INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/iGloNumElectrons_G/) ! Dataset dimensions
    INTEGER(HSIZE_T), DIMENSION(2) :: fdims,dims   !< dims of ptcl dataset (coords*numelecs)
    INTEGER(HSIZE_T), DIMENSION(2) :: doffset!< Offset for write, could be rank dependent
    INTEGER(HSIZE_T), DIMENSION(2) :: dsize  !< Size of hyperslab to write
    INTEGER     ::  rank = 2                 !< Particle Dataset rank
    INTEGER     ::  arank = 1                !< Attribute rank - 1 is vector
    INTEGER(HSIZE_T), DIMENSION(1) :: adims  !< Attribute dims
    INTEGER(HSIZE_T), DIMENSION(1) :: attr_data_int !< For integer attribs (numdims)
    INTEGER     :: numSpatialDims,mpiinfo    !< Attr content, and also num elsewhere
    INTEGER(kind=IP)     :: startOffset,rankIterator    !< For working out start index
!assumed 3D sim. May be 1D.
!    TYPE(C_PTR) :: f_ptr
    REAL(kind=WP) :: attr_data_double
    CHARACTER(LEN=100) :: attr_data_string
    CHARACTER(LEN=16) :: scaleToSIstring
    INTEGER(HSIZE_T) :: attr_string_len
    CHARACTER(LEN=4), PARAMETER :: timegrpname = "time"  ! Group name
    CHARACTER(LEN=12), PARAMETER :: limgrpname = "globalLimits"  ! Group name
    REAL(kind=WP), ALLOCATABLE :: limdata (:)  ! Data to write
    real(kind=wp), allocatable :: sz2_temp(:)
    real(kind=wp) :: ebound
    ! Local vars
    !integer(kind=ip) :: iep
    integer :: error ! Error flag
    mpiinfo=MPI_INFO_NULL

    if (qONED_G) then
      numSpatialDims=1
    else
      numSpatialDims=3
    end if

!   Fortran index of first particle to be written might be '1', however
!   We're interested in the offset from the first step.
    startOffset=0
    if (tProcInfo_G%rank .GT. 0) then
    Do rankIterator=1,tProcInfo_G%rank
      startOffset = startOffset+procelectrons_G(rankIterator+1)
    end do

    end if
!    print*,'Writing electron data on rank ' &
!      // trim(adjustl(IntegerToString(tProcInfo_G%rank))) &
!      // ' Starting at offset ' &
!      // trim(adjustl(IntegerToString(startOffset))) // ' n_elecs=' &
!      // trim(adjustl(IntegerToString(procelectrons_G(1)))) // ' ' &
!      // trim(adjustl(IntegerToString(iNumberElectrons_G)))

    attr_data_int(1)=numSpatialDims
    adims(1)=1
    adims = (/1/)
    dims = (/7,iNumberElectrons_G/) ! Dataset dimensions
    fdims = (/7,iGloNumElectrons_G/) ! Dataset dimensions
    doffset=(/0,startOffset/)
    dsize=(/1,iNumberElectrons_G/)
    attr_data_string="electrons_x,electrons_y,electrons_z,electrons_px," // &
      "electrons_py,electrons_gamma,electrons_weight"
    attr_string_len=94

! Prepare filename

    filename = ( trim(adjustl(zFilename_G)) // '_electrons_' // &
                 trim(adjustl(IntegerToString(igwr))) // '.h5' )


    CALL h5open_f(error)
    CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
!      Print*,'hdf5_puff:outputH5BeamSD(property created)'
!      Print*,error
      CALL h5pset_fapl_mpio_f(plist_id, tProcInfo_G%comm, mpiinfo, error)
!      Print*,'hdf5_puff:outputH5BeamSD(property set up)'
!      Print*,error
      CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error, access_prp = plist_id)
!     Print*,'hdf5_puff:outputH5BeamSD(file created)'
!     Print*,error
     CALL h5pclose_f(plist_id, error)
!     Print*,'hdf5_puff:outputH5BeamSD(property closed)'
!     Print*,error
     CALL h5screate_simple_f(rank, fdims, filespace, error)
!     Print*,'hdf5_puff:outputH5BeamSD(filespace created)'
     CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_DOUBLE, filespace, &
       dset_id, error)
!     Print*,'hdf5_puff:outputH5BeamSD(dataset created)'
!     Print*,error
     CALL h5sclose_f(filespace, error)
!     Print*,'hdf5_puff:outputH5BeamSD(filespace closed)'
!     Print*,error
! Select hyperslab in the file.


!      if (procelectrons_G(1).GT.0) then
! for the corresponding space on disk
        CALL h5screate_simple_f(rank, dsize, dspace_id, error)
        if (procelectrons_G(1) <= 0) CALL h5sselect_none_f(dspace_id, error)

!        Print*,'hdf5_puff:outputH5BeamFilesSD(memory dataspace allocated)'
!        Print*,error
        CALL h5dget_space_f(dset_id, filespace, error)
        CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, dsize, error)
        if (procelectrons_G(1) <= 0) CALL h5sselect_none_f(filespace,error)
!        Print*,trim(adjustl(IntegerToString(error))) // " selecting particles on rank" &
!          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
!      else
! all ranks must participate, so select no space to write when dealing with ranks which hold no data for this field fr_real, etc
!        CALL h5sselect_none_f(filespace,error)
!        Print*,trim(adjustl(IntegerToString(error))) // " selecting no particles on rank" &
!          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
!        CALL h5sselect_none_f(dspace_id,error)
!      end if
      CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
!      CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_INDEPENDENT_F, error)
      CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, error)
!      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, &
!         sElX_G((startOffset+1):(startOffset+procelectrons_G(1))), dsize, error, &
!         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, &
         sElX_G, dsize, error, &
         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!      Print*,'hdf5_puff:outputH5BeamSD(write done)'
      CALL h5sclose_f(filespace, error)
!      Print*,'hdf5_puff:outputH5BeamSD(x space closed) rank: ' // &
!         trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
!      Defer this until all coordinates/components are written
!      CALL h5sclose_f(dspace_id, error)
!      Print*,'hdf5_puff:outputH5FieldSD(hyperslab space closed)'
!      Print*,error

! repeat for some next y dataset


    doffset=(/1,startOffset/)

!if (procelectrons_G(1).GT.0) then
! for the corresponding space on disk

  CALL h5dget_space_f(dset_id, filespace, error)
  CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, dsize, error)
!        Print*,trim(adjustl(IntegerToString(error))) // " selecting particles on rank" &
!          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
!else
! all ranks must participate, so select no space to write when dealing with ranks which hold no data for this field fr_real, etc
  if (procelectrons_G(1) <= 0) CALL h5sselect_none_f(filespace,error)
!        Print*,trim(adjustl(IntegerToString(error))) // " selecting no particles on rank" &
!          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
!end if




    !CALL H5Dget_space_f(dset_id, filespace, error)
    !CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, doffset, &
    !   dsize, error)



    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, &
       sElY_G, dsize, error, &
       xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!      Print*,'hdf5_puff:outputH5BeamSD(y space closed) rank: ' // &
!         trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
! was       dspace_id, filespace)
    CALL h5sclose_f(filespace, error)

!
! repeat for some next z dataset
    doffset=(/2,startOffset/)

!    if (procelectrons_G(1).GT.0) then
    ! for the corresponding space on disk

      CALL h5dget_space_f(dset_id, filespace, error)
      CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, dsize, error)
    !        Print*,trim(adjustl(IntegerToString(error))) // " selecting particles on rank" &
    !          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
!    else
    ! all ranks must participate, so select no space to write when dealing with ranks which hold no data for this field fr_real, etc
      if (procelectrons_G(1) <= 0) CALL h5sselect_none_f(filespace,error)
    !        Print*,trim(adjustl(IntegerToString(error))) // " selecting no particles on rank" &
    !          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
!    end if

    if (fieldMesh == iPeriodic) then

      allocate(sz2_temp(procelectrons_G(1)))
      sz2_temp = sElZ2_G
      ebound = 4.0_wp * pi * sRho_G * sperwaves_G
      where (sz2_temp > ebound) sz2_temp = sz2_temp - &
                      (real(floor(sElZ2_G / ebound), kind=wp) * ebound )

      call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, &
         sz2_temp, dsize, error, &
         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
         
      deallocate(sz2_temp)

    else

      call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, &
         sElZ2_G, dsize, error, &
         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)

    end if


!      Print*,'hdf5_puff:outputH5BeamSD(z2 space closed) rank: ' // &
!         trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
! was       dspace_id, filespace)
    CALL h5sclose_f(filespace, error)

! repeat for some next px dataset
    doffset=(/3,startOffset/)

!    if (procelectrons_G(1).GT.0) then
    ! for the corresponding space on disk

      CALL h5dget_space_f(dset_id, filespace, error)
      CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, dsize, error)
    !        Print*,trim(adjustl(IntegerToString(error))) // " selecting particles on rank" &
    !          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
!    else
    ! all ranks must participate, so select no space to write when dealing with ranks which hold no data for this field fr_real, etc
      if (procelectrons_G(1) <= 0) CALL h5sselect_none_f(filespace,error)

    !        Print*,trim(adjustl(IntegerToString(error))) // " selecting no particles on rank" &
    !          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
!    end if

!    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, &
!       sElPX_G((startOffset+1):(startOffset+procelectrons_G(1))), dsize, error, &
!       xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, &
       sElPX_G, dsize, error, &
       xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!      Print*,'hdf5_puff:outputH5BeamSD(px space closed) rank: ' // &
!         trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
! was       dspace_id, filespace)
    CALL h5sclose_f(filespace, error)

! repeat for some next py dataset
    doffset=(/4,startOffset/)

!    if (procelectrons_G(1).GT.0) then
    ! for the corresponding space on disk

      CALL h5dget_space_f(dset_id, filespace, error)
      CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, dsize, error)
    !        Print*,trim(adjustl(IntegerToString(error))) // " selecting particles on rank" &
    !          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
!    else
    ! all ranks must participate, so select no space to write when dealing with ranks which hold no data for this field fr_real, etc
      if (procelectrons_G(1) <= 0) CALL h5sselect_none_f(filespace,error)
    !        Print*,trim(adjustl(IntegerToString(error))) // " selecting no particles on rank" &
    !          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
!    end if

!    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, &
!       sElPY_G((startOffset+1):(startOffset+procelectrons_G(1))), dsize, error, &
!       xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, &
       sElPY_G, dsize, error, &
       xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!      Print*,'hdf5_puff:outputH5BeamSD(py space closed) rank: ' // &
!         trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
! was       dspace_id, filespace)
    CALL h5sclose_f(filespace, error)

! repeat for some next gamma dataset (actually beta*gamma)
    doffset=(/5,startOffset/)

!    if (procelectrons_G(1).GT.0) then
    ! for the corresponding space on disk

      CALL h5dget_space_f(dset_id, filespace, error)
      CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, dsize, error)
    !        Print*,trim(adjustl(IntegerToString(error))) // " selecting particles on rank" &
    !          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
!    else
    ! all ranks must participate, so select no space to write when dealing with ranks which hold no data for this field fr_real, etc
      if (procelectrons_G(1) <= 0) CALL h5sselect_none_f(filespace,error)
    !        Print*,trim(adjustl(IntegerToString(error))) // " selecting no particles on rank" &
    !          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
!    end if

!    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, &
!       sElgam_G((startOffset+1):(startOffset+procelectrons_G(1))), dsize, error, &
!       xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, &
       sElgam_G, dsize, error, &
       xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!      Print*,'hdf5_puff:outputH5BeamSD(pz2 space closed) rank: ' // &
!         trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
! was       dspace_id, filespace)
    CALL h5sclose_f(filespace, error)
!
!
! put Chi in the file, slightly redundant as charge on a macroparticle
! doesn't increase or decrease through the simulation. But does make
! Everything self contained. Perhaps we use in future a funky h5 technique
! to point this column at a separate file which holds the data, reducing
! the size of this column from every written file.
    doffset=(/6,startOffset/)

!    if (procelectrons_G(1).GT.0) then
    ! for the corresponding space on disk

      CALL h5dget_space_f(dset_id, filespace, error)
      CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, dsize, error)
    !        Print*,trim(adjustl(IntegerToString(error))) // " selecting particles on rank" &
    !          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
!    else
    ! all ranks must participate, so select no space to write when dealing with ranks which hold no data for this field fr_real, etc
      if (procelectrons_G(1) <= 0) CALL h5sselect_none_f(filespace,error)
    !        Print*,trim(adjustl(IntegerToString(error))) // " selecting no particles on rank" &
    !          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
!    end if

!    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, &
!       s_chi_bar_G((startOffset+1):(startOffset+procelectrons_G(1))), dsize, error, &
!       xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, &
       s_chi_bar_G, dsize, error, &
       xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!      Print*,'hdf5_puff:outputH5BeamSD(charge dataspace closed) rank: ' // &
!         trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
! was       dspace_id, filespace)
    CALL h5sclose_f(filespace, error)
! Terminate access to the data space.
!
    CALL h5sclose_f(dspace_id, error)
!
! ATTRIBUTES FOR PARTICLE DATASET
!
! simple dataset for array of vals
!    CALL h5screate_simple_f(arank, adims, aspace_id, error)

! scalar dataset for simpler values
    CALL h5screate_f(H5S_SCALAR_F, aspace_id, error)
!
! Create datatype for the attribute.
!
    CALL h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
!    CALL h5tset_size_f(atype_id, attrlen, error)
    !
! Create dataset attribute.
!
    aname = "vsNumSpatialDims"
    CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)
! Write the attribute data.
    CALL h5awrite_f(attr_id, atype_id, numSpatialDims, adims, error) !
! Close the attribute.
    CALL h5aclose_f(attr_id, error)
! next attribute
    aname="numSpatialDims"
    CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)
    CALL h5awrite_f(attr_id, atype_id, numSpatialDims, adims, error)
    CALL h5aclose_f(attr_id, error)
    CALL h5tclose_f(atype_id, error)

! integers done, move onto floats
    CALL h5tcopy_f(H5T_NATIVE_DOUBLE, atype_id, error)
!    aname="time"
!    attr_data_double=time
!    CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)
!    CALL h5awrite_f(attr_id, atype_id, attr_data_double, adims, error)
!    CALL h5aclose_f(attr_id, error)
! then
    aname="mass"
!    attr_data_double=9.10938356E-31
    attr_data_double=m_e
    CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)
    CALL h5awrite_f(attr_id, atype_id, attr_data_double, adims, error)
    CALL h5aclose_f(attr_id, error)
    aname="charge"
!    attr_data_double=1.602176487E-19
    attr_data_double=q_e
    CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)
    CALL h5awrite_f(attr_id, atype_id, attr_data_double, adims, error)
    CALL h5aclose_f(attr_id, error)
!    Print*,'hdf5_puff:outputH5BeamFiles(charge written)'
    aname="numTotalPhysicalParticles"
    attr_data_double=npk_bar_G*q_e
    CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)
    CALL h5awrite_f(attr_id, atype_id, attr_data_double, adims, error)
    CALL h5aclose_f(attr_id, error)


    call writeCommonAtts(dset_id, time, sz_loc, iL, aspace_id)
!    aname="zbarInter"
!    attr_data_double=time !real(iCSteps,kind=wp)*sStepSize*lg_G
!    CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)
!    CALL h5awrite_f(attr_id, atype_id, attr_data_double, adims, error)
!    CALL h5aclose_f(attr_id, error)
!    aname="zInter"
!    attr_data_double=time*lg_G !real(iCSteps,kind=wp)*sStepSize*lg_G
!    CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)
!    CALL h5awrite_f(attr_id, atype_id, attr_data_double, adims, error)
!    CALL h5aclose_f(attr_id, error)
!
!    call addH5IntegerAttribute(dset_id, "iUnd_cr", iUnd_cr, aspace_id)
!    call addH5IntegerAttribute(dset_id, "iChic_cr", iChic_cr, aspace_id)
!    call addH5IntegerAttribute(dset_id, "iDrift_cr", iDrift_cr, aspace_id)
!    call addH5IntegerAttribute(dset_id, "iQuad_cr", iQuad_cr, aspace_id)
!    call addH5IntegerAttribute(dset_id, "iModulation_cr", iModulation_cr, aspace_id)
!    call addH5IntegerAttribute(dset_id, 'iL', iL, aspace_id)


    aname="gainLength"
    attr_data_double=lg_G
    CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)
    CALL h5awrite_f(attr_id, atype_id, attr_data_double, adims, error)
    CALL h5aclose_f(attr_id, error)
    aname="cooperationLength"
    attr_data_double=lc_G
    CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)
    CALL h5awrite_f(attr_id, atype_id, attr_data_double, adims, error)
    CALL h5aclose_f(attr_id, error)
    CALL h5tclose_f(atype_id, error)
    CALL h5sclose_f(aspace_id, error)
    CALL h5pclose_f(plist_id, error)
!    Print*,'hdf5_puff:outputH5BeamSD(propertylist closed)'
!    Print*,error
      CALL h5dclose_f(dset_id, error)
!      Print*,'hdf5_puff:outputH5BeamSD(dataset closed)'
!      Print*,error

      CALL h5fclose_f(file_id, error)
!      Print*,'hdf5_puff:outputH5BeamSD(file closed)'
!      Print*,error


!!!
!Close and reopen serial to write the other stuff.
!!!
      if (tProcInfo_G%qRoot) then
      CALL h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, error)
!      Print*,'hdf5_puff:outputH5BeamSD(file reopened in serial)'
!      Print*,error
      CALL h5dopen_f (file_id, dsetname, dset_id, error)
!      Print*,'hdf5_puff:outputH5BeamSD(dataset reopened in serial)'
!      Print*,error

    CALL h5screate_f(H5S_SCALAR_F, aspace_id, error)
! then text attributes
    CALL h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)
    CALL h5tset_size_f(atype_id, attr_string_len, error)
    CALL h5tset_strpad_f(atype_id, H5T_STR_SPACEPAD_F, error)
!    Print*,'hdf5_puff:outputH5BeamFiles(string padding enabled)'
    aname="vsLabels"
    CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)
    CALL h5awrite_f(attr_id, atype_id, attr_data_string, adims, error)
!    Print*,'hdf5_puff:outputH5BeamFiles(lables attribute written)'
    CALL h5aclose_f(attr_id, error)
    CALL addH5StringAttribute(dset_id,"vsType","variableWithMesh",aspace_id)
    CALL addH5StringAttribute(dset_id,"vsTimeGroup","time",aspace_id)
    CALL addH5StringAttribute(dset_id,"vsLimits","globalLimits",aspace_id)
    CALL addH5StringAttribute(dset_id,"vsAxisLabels","xbar,ybar,z2bar",aspace_id)
!
! Terminate access to the dataset space, still using the scalar identifier
    CALL h5dclose_f(dset_id, error)


! Write time Group
    CALL writeH5TimeGroup(file_id, timegrpname, time, 'outputH5Beam', error)

! Write run info
    CALL writeH5RunInfo(file_id,  time, sz_loc, iL, 'outputH5Beam', error)

! We make the limits
    CALL h5gcreate_f(file_id, limgrpname, group_id, error)
    CALL addH5StringAttribute(group_id,"vsType","limits",aspace_id)
    CALL addH5StringAttribute(group_id,"vsKind","Cartesian",aspace_id)
! end of scalars, need arrays (a vector) for the limits
    CALL h5sclose_f(aspace_id, error)

! And the limits themselves which require non-scalar attributes
! This is the 3D version.
    adims = (/numSpatialDims/)
    CALL h5screate_simple_f(arank, adims, aspace_id, error)
    aname="vsLowerBounds"
    CALL h5tcopy_f(H5T_NATIVE_DOUBLE, atype_id, error)
    CALL h5acreate_f(group_id, aname, atype_id, aspace_id, attr_id, error)
!    Print*,'hdf5_puff:outputH5BeamFiles(lower bounds attribute created)'
    ALLOCATE ( limdata(numSpatialDims))

    
    if (numSpatialDims == 3) then

        limdata(1)=-0.5_wp*NX_G*sLengthOfElmX_G
        limdata(2)=-0.5_wp*NY_G*sLengthOfElmY_G
        limdata(3)=0.0_wp

    else

        limdata(1)=0.0_wp

    end if
      
!    end if

    CALL h5awrite_f(attr_id, atype_id, limdata, adims, error)
    CALL h5aclose_f(attr_id, error)
    aname="vsUpperBounds"
    CALL h5acreate_f(group_id, aname, atype_id, aspace_id, attr_id, error)

!    Print*,'hdf5_puff:outputH5BeamFiles(upper bounds attribute created)'
    if (numSpatialDims == 3) then

        limdata(1)=0.5*NX_G*sLengthOfElmX_G
        limdata(2)=0.5*NY_G*sLengthOfElmY_G
        limdata(3) = real((NZ2_G-1),kind=wp)*sLengthOfElmZ2_G

    else

        limdata(1) = real((NZ2_G-1),kind=wp)*sLengthOfElmZ2_G

    end if

    CALL h5awrite_f(attr_id, atype_id, limdata, adims, error)
! Close the attribute should be done above.
    CALL h5aclose_f(attr_id, error)
    DEALLOCATE ( limdata)
    CALL h5tclose_f(atype_id, error)
    CALL h5sclose_f(aspace_id, error)
    CALL h5gclose_f(group_id, error)

    aname="electrons_xSI"
    write(scaleToSIstring, '(E16.9)' ) (DSQRT(lg_G*lc_G))
    attr_data_string=("electrons_x*" // scaleToSIstring)
    attr_string_len=len(trim(adjustl(attr_data_string)))
    CALL addH5derivedVariable(file_id,aname,attr_data_string,error)

! We make another group
    aname="electrons_ySI"
    attr_data_string=("electrons_y*" // scaleToSIstring)
    attr_string_len=len(trim(adjustl(attr_data_string)))
    CALL addH5derivedVariable(file_id,aname,attr_data_string,error)

! We make another group
    aname="electrons_zSI"
    write(scaleToSIstring, '(E16.9)' ) lc_G
    attr_data_string=("electrons_z*" // scaleToSIstring)
    attr_string_len=len(trim(adjustl(attr_data_string)))
    CALL addH5derivedVariable(file_id,aname,attr_data_string,error)!
! Were there an SI version of this, we might be in the right place to use it

    aname="electrons_dxdzSI"
    write(scaleToSIstring, '(E16.9)' ) 2.0_wp * sRho_G * sKappa_G
    attr_data_string=("electrons_px*" // scaleToSIstring // "/electrons_gamma")
    attr_string_len=len(trim(adjustl(attr_data_string)))
    CALL addH5derivedVariable(file_id,aname,attr_data_string,error)

    aname="electrons_dydzSI"
    write(scaleToSIstring, '(E16.9)' ) -2.0_wp * sRho_G * sKappa_G
    attr_data_string=("electrons_py*" // scaleToSIstring // "/electrons_gamma")
    CALL addH5derivedVariable(file_id,aname,attr_data_string,error)

    aname="electrons_gammaSI"
    write(scaleToSIstring, '(E16.9)' ) sGammaR_G
    attr_data_string=("electrons_gamma*" // scaleToSIstring)
    CALL addH5derivedVariable(file_id,aname,attr_data_string,error)

    aname="slice_nom_lamda"
! Todo: Actually needs to take account of slippage, and needs to identify
! which lamda was used (eg for 2 colour)
    write(scaleToSIstring, '(E16.9)' ) lam_r_G
    attr_data_string=("floor(electrons_zSI/" // scaleToSIstring // ")")
    CALL addH5derivedVariable(file_id,aname,attr_data_string,error)

    aname="phi_lamda"
! Todo: Actually needs to take account of slippage, and needs to identify
! which lamda was used (eg for 2 colour)
    attr_data_string=("(electrons_zSI-(slice_nom_lamda*" // &
     scaleToSIstring // "))*6.283185307179586/" // scaleToSIstring)
    CALL addH5derivedVariable(file_id,aname,attr_data_string,error)

    aname="electrons_numPhysicalParticles"
    write(scaleToSIstring, '(E16.9)' ) npk_bar_G
    attr_data_string=("electrons_weight*" // scaleToSIstring)
    CALL addH5derivedVariable(file_id,aname,attr_data_string,error)

    aname="electrons_chargeSI"
    write(scaleToSIstring, '(E16.9)' ) npk_bar_G*q_e
    attr_data_string=("electrons_weight*" // scaleToSIstring)
    CALL addH5derivedVariable(file_id,aname,attr_data_string,error)

! NOT Todo: write an expression for rms beam sizes - available through
! Data reduction operator.
! Todo: write an expression for slice emittance

! Close the file.
    CALL h5fclose_f(file_id, error)
    end if
!Close the interface
    CALL h5close_f(error)

!    qOK = .true.
    goto 2000

!     Error Handler - Error log Subroutine in CIO.f90 line 709
1000 call Error_log('Error in hdf5_puff:outputBeamFiles',tErrorLog_G)
    print*,'Error in hdf5_puff:outputBeamFiles'
2000 continue
  end subroutine outputH5BeamFilesSD




!> outputH5Field3DSD is for writing the full field output.
!! This version dumps one single file, but writes individually rather than collectively
  subroutine outputH5Field3DSD(time, sz_loc, iL, error, nlonglength, rawdata, nlo, nhi, component, createNewFlag, chkactiveflag)
    implicit none
    REAL(kind=WP), intent(in) :: time, sz_loc, rawdata(:) !< The data to write
    integer(kind=ip), intent(in) :: iL
    INTEGER(kind=IP), intent(in) :: nlonglength !<number of cells in z in this section
    INTEGER(kind=IP), intent(in) :: nlo,nhi !< cell range in z in this raw data selection
    INTEGER(kind=IP), intent(in) :: component, createNewFlag !< cell range in 4th dim in this raw data selection
    LOGICAL, intent(in) :: chkactiveflag !< flag determines whether to test for the entire field on every rank
    INTEGER(HID_T) :: file_id       !< File identifier
    INTEGER(HID_T) :: dset_id       !< Dataset identifier
    INTEGER(HID_T) :: dspace_id     !< Dataspace identifier in memory
    INTEGER(HID_T) :: filespace     !< Dataspace identifier in file
    INTEGER(HID_T) :: attr_id       !< Attribute identifier
    INTEGER(HID_T) :: aspace_id     !< Attribute Dataspace identifier
    INTEGER(HID_T) :: atype_id      !< Attribute Data type identifier
    INTEGER(HID_T) :: group_id      !< Group identifier
    INTEGER(HID_T) :: plist_id      !< Property list id.

! may yet need this, but field data is not separated amongst cores
!    logical, intent(in) :: qSeparate
    CHARACTER(LEN=5), PARAMETER :: dsetname = "aperp"     ! Dataset name
    CHARACTER(LEN=16) :: aname   ! Attribute name
!    character(1024_IP), intent(in) :: zDFName
    character(64_IP) :: filename
    INTEGER(HSIZE_T), DIMENSION(4) :: fdims,dims !<no longer includes component
    INTEGER(HSIZE_T), DIMENSION(4) :: doffset,dsize,stride !<no longer includes component
! Data as component*reducedNX*reducedNY*reducedNZ2
! Not described as a parameter, so can prob modify
! for single component (rank 3 data) like charge
    INTEGER     ::   rank = 4               !< Dataset rank
    INTEGER(HSIZE_T), DIMENSION(1) :: adims !< Attribute dims
    REAL(kind=WP) :: attr_data_double       !< holder of attribute double data
    REAL(kind=WP), DIMENSION(3) :: ub       !< holder of attribute double data
    REAL(kind=WP), DIMENSION(3) :: lb       !< holder of attribute double data
    CHARACTER(LEN=100) :: attr_data_string  !< holder of attribute strings
    INTEGER(HSIZE_T) :: attr_string_len     !< length of attribute strings
    INTEGER(kind=IP) :: numSpatialDims,mpiinfo      !< Attr content,
    INTEGER     ::  arank = 1               !< Attribute Dataset rank (1: vector)
    CHARACTER(LEN=4), PARAMETER :: timegrpname = "time"  !< Name of time group
    CHARACTER(LEN=12), PARAMETER :: limgrpname = "globalLimits"  !< Name of limits grp
    CHARACTER(LEN=10), PARAMETER :: meshScaledGrpname = "meshScaled" !< Name of mesh grp
    CHARACTER(LEN=6), PARAMETER :: meshSIGrpname = "meshSI"  !< Dummy scaled mesh grp name
    REAL(kind=WP), ALLOCATABLE :: limdata (:)  !< Data to write (diff for 1D and 3D)
    INTEGER(kind=IP), ALLOCATABLE :: numcelldata (:)  !< Mesh info for uniform grid
    ! Local vars
    integer :: error !< Error flag

! signature: nlonglength, dsetname, data, nlo, nhi, chkactiveflag
! tlflen, 'aperp_front_real', fr_rfield, [ffs,ffe], .false.
! tlflen, 'aperp_front_imag', fr_ifield, [ffs,ffe], .false.
! mainlen, 'aperp_active_real', ac_rfield, [fz2,ez2], .true.
! mainlen, 'aperp_active_imag', ac_ifield, [fz2,ez2], .true.
! tlelen, 'aperp_back_real', bk_rfield, [ees,eee], .false.
! tlelen, 'aperp_back_imag', bk_ifield, [ees,eee], .false.
! final argument  checks for all active field on single root node ...
! should say if qUnique or rank=0...
    mpiinfo=MPI_INFO_NULL
!    if (qUnique .OR. (tProcInfo_G%qRoot)) then
      if (qONED_G) then
        numSpatialDims=1
        dims = (/1,1,nlonglength,1/) ! Dataset dimensions
        fdims = (/2,2,NZ2_G,2/) ! Dataset dimensions
        doffset = (/0,0,(nlo-1),component/)
        dsize = (/1,1,nhi-nlo+1,1/)
      else
        numSpatialDims=3
        dims = (/nx_g,ny_g,nlonglength,1/) ! Dataset dimensions
        fdims = (/nx_g,ny_g,NZ2_G,2/) ! Dataset dimensions
        doffset = (/0,0,(nlo-1),component/)
!      dsize = (/nx_g,ny_g,nhi-nlo+1,1/)
        dsize = (/nx_g,ny_g,nlonglength,1/)
        
!        numSpatialDims=3
!        dims = (/nlonglength,ny_g,nx_g,1/) ! Dataset dimensions
!        fdims = (/NZ2_G,ny_g,nx_g,2/) ! Dataset dimensions
!        doffset = (/(nlo-1),0,0,component/)
!!      dsize = (/nx_g,ny_g,nhi-nlo+1,1/)
!        dsize = (/nlonglength,ny_g,nx_g,1/)
        
        
      end if
!    print *,IntegerToString(size(rawdata)) // " vs " // &
!      trim(adjustl(IntegerToString(Nx_g*ny_g*nlonglength))) // &
!      "   nlo-1 : " // trim(adjustl(IntegerToString(nlo-1))) // &
!      "   nlo+nlonglength-1 : " // trim(adjustl(IntegerToString(nlo+nlonglength-1)))
!! repeat for some next y dataset
!    doffset=(/1,0/)
!    CALL H5Dget_space_f(dset_id, filespace, error)
!    CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, doffset, &
!       dsize, error)
!    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, sElY_G, dims, error, &
!       file_space_id = filespace, mem_space_id = dspace_id)
!! was       dspace_id, filespace)
!    CALL h5sclose_f(filespace, error)


!    Print*,('Spatialdims: ' // trim(IntegerToString(numSpatialDims)))
      filename = (trim(adjustl(zFilename_G)) // '_' // trim(adjustl(dsetname)) &
          // '_' // trim(adjustl(IntegerToString(igwr))) // '.h5' )
      CALL h5open_f(error)
      CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)
!      Print*,'hdf5_puff:outputH5FieldSD(property created)'
!      Print*,error
      CALL h5pset_fapl_mpio_f(plist_id, tProcInfo_G%comm, mpiinfo, error)
!      Print*,'hdf5_puff:outputH5FieldSD(property set up)'
!      Print*,error
      if (createNewFlag .EQ. 1) then
        CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error, access_prp = plist_id)
!      Print*,'hdf5_puff:outputH5FieldSD(file created)'
!      Print*,error
        CALL h5pclose_f(plist_id, error)
!      Print*,'hdf5_puff:outputH5FieldSD(property closed)'
!      Print*,error
        CALL h5screate_simple_f(rank, fdims, filespace, error)
!      Print*,'hdf5_puff:outputH5FieldSD(filespace created)'
        CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_DOUBLE, filespace, &
                             dset_id, error)
!      Print*,'hdf5_puff:outputH5FieldSD(dataset created)'
!      Print*,error
        CALL h5sclose_f(filespace, error)
!      Print*,'hdf5_puff:outputH5FieldSD(filespace closed)'
!      Print*,error
      else  ! not creating a new file, just adding data to an existing one.
        CALL h5pset_fapl_mpio_f(plist_id, tProcInfo_G%comm, mpiinfo, error)
        CALL h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, error, access_prp = plist_id)
!        Print*,'hdf5_puff:outputH5FieldSD(file reopened in parallel)'
!        Print*,error
        CALL h5dopen_f (file_id, dsetname, dset_id, error)
!        Print*,'hdf5_puff:outputH5FieldSD(dataset reopened in parallel)'
!        Print*,error
        CALL h5pclose_f(plist_id, error)
      end if

      if ((qUnique) .or. (.not. chkactiveflag)) then

        CALL h5screate_simple_f(rank, dims, dspace_id, error)

        if (nlonglength <= 0) CALL h5sselect_none_f(dspace_id,error)
 ! For the space in memory
      !if (nlonglength.GT.0) then
! for the corresponding space on disk
        CALL h5dget_space_f(dset_id, filespace, error)
        CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, dims, error)
!        Print*,trim(adjustl(IntegerToString(error))) // " selecting slab on rank" &
!          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
      !else
! all ranks must participate, so select no space to write when dealing with ranks which hold no data for this field fr_real, etc
        if (nlonglength <= 0) CALL h5sselect_none_f(filespace,error)
!        Print*,trim(adjustl(IntegerToString(error))) // " selecting empty slab on rank" &
!          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
!        CALL h5sselect_none_f(dspace_id,error)
      !end if


        CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
        CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_INDEPENDENT_F, error)
        CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
              xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!      Print*,'hdf5_puff:outputH5FieldSD(write done)'
!      Print*,error

      ! ABORTIVE ATTEMPT TO WRITE NODAL DATA, when really it's nodal in z2 but 1D (so
      !shouldn't matter if zonal or nodal) in x,y.
!        if (qONED_G) then
!      if (nlonglength.GT.0) then
!      doffset = (/0,1,(nlo-1),component/)
!        CALL h5dget_space_f(dset_id, filespace, error)
!       CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, dims, error)
!      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
!         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!      doffset = (/1,0,(nlo-1),component/)
!        CALL h5dget_space_f(dset_id, filespace, error)
!        CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, dims, error)
!     CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
!         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!      doffset = (/1,1,(nlo-1),component/)
!        CALL h5dget_space_f(dset_id, filespace, error)
!        CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, dims, error)
!      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
!         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!     else
!        CALL h5sselect_none_f(filespace,error)
!      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
!         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!        CALL h5sselect_none_f(filespace,error)
!      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
!         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!        CALL h5sselect_none_f(filespace,error)
!      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
!         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!     end if
!     end if
        CALL h5sclose_f(filespace, error)
!      Print*,'hdf5_puff:outputH5FieldSD(file space closed)'
!      Print*,error
        CALL h5sclose_f(dspace_id, error)
!      Print*,'hdf5_puff:outputH5FieldSD(hyperslab space closed)'
!      Print*,error
        CALL h5pclose_f(plist_id, error)
!      Print*,'hdf5_puff:outputH5FieldSD(propertylist closed)'
!      Print*,error
        CALL h5dclose_f(dset_id, error)
!      Print*,'hdf5_puff:outputH5FieldSD(dataset closed)'
!      Print*,error
        CALL h5fclose_f(file_id, error)

      else   ! if not unique (so e.g. short bunch case)

        CALL h5screate_simple_f(rank, dims, dspace_id, error)

        if (tProcInfo_G%rank /= 0) CALL h5sselect_none_f(dspace_id,error)
 ! For the space in memory
      !if (nlonglength.GT.0) then
! for the corresponding space on disk
        CALL h5dget_space_f(dset_id, filespace, error)
        CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, dims, error)
!        Print*,trim(adjustl(IntegerToString(error))) // " selecting slab on rank" &
!          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
      !else
! all ranks must participate, so select no space to write when dealing with ranks which hold no data for this field fr_real, etc
        if (tProcInfo_G%rank /= 0) CALL h5sselect_none_f(filespace,error)
!        Print*,trim(adjustl(IntegerToString(error))) // " selecting empty slab on rank" &
!          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
!        CALL h5sselect_none_f(dspace_id,error)
      !end if


        CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
        CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_INDEPENDENT_F, error)
        CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
              xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!      Print*,'hdf5_puff:outputH5FieldSD(write done)'
!      Print*,error

      ! ABORTIVE ATTEMPT TO WRITE NODAL DATA, when really it's nodal in z2 but 1D (so
      !shouldn't matter if zonal or nodal) in x,y.
!        if (qONED_G) then
!      if (nlonglength.GT.0) then
!      doffset = (/0,1,(nlo-1),component/)
!        CALL h5dget_space_f(dset_id, filespace, error)
!       CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, dims, error)
!      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
!         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!      doffset = (/1,0,(nlo-1),component/)
!        CALL h5dget_space_f(dset_id, filespace, error)
!        CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, dims, error)
!     CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
!         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!      doffset = (/1,1,(nlo-1),component/)
!        CALL h5dget_space_f(dset_id, filespace, error)
!        CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, dims, error)
!      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
!         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!     else
!        CALL h5sselect_none_f(filespace,error)
!      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
!         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!        CALL h5sselect_none_f(filespace,error)
!      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
!         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!        CALL h5sselect_none_f(filespace,error)
!      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
!         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!     end if
!     end if
        CALL h5sclose_f(filespace, error)
!      Print*,'hdf5_puff:outputH5FieldSD(file space closed)'
!      Print*,error
        CALL h5sclose_f(dspace_id, error)
!      Print*,'hdf5_puff:outputH5FieldSD(hyperslab space closed)'
!      Print*,error
        CALL h5pclose_f(plist_id, error)
!      Print*,'hdf5_puff:outputH5FieldSD(propertylist closed)'
!      Print*,error
        CALL h5dclose_f(dset_id, error)
!      Print*,'hdf5_puff:outputH5FieldSD(dataset closed)'
!      Print*,error
        CALL h5fclose_f(file_id, error)


      end if

! end of parallel write stuff

! add stuff just on rank 0
      if (createNewFlag .EQ. 1) then

        if (tProcInfo_G%qRoot) then

          CALL h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, error)
!      Print*,'hdf5_puff:outputH5FieldSD(file reopened in serial)'
!      Print*,error
          CALL h5dopen_f (file_id, dsetname, dset_id, error)
!      Print*,'hdf5_puff:outputH5FieldSD(dataset reopened in serial)'
!      Print*,error
!      CALL h5dget_space_f(dset_id, filespace, error)
          CALL h5screate_f(H5S_SCALAR_F, aspace_id, error)
!      Print*,'hdf5_puff:outputH5Field3DSD(scalar space created)'
          CALL h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
!    CALL h5tset_size_f(atype_id, attrlen, error)
!
! Create dataset integer attributes.
! Always ndim=3 even if 1D as 1 cell in both transverse dims but same
! postprocessing should work, and make possible to display vs particles
!
!
          aname = "vsNumSpatialDims"
          CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)
! Write the attribute data.
          CALL h5awrite_f(attr_id, atype_id, 3, adims, error) !
! Close the attribute.
          CALL h5aclose_f(attr_id, error)
! next attribute
          aname="numSpatialDims"
          CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)
          CALL h5awrite_f(attr_id, atype_id, 3, adims, error)
          CALL h5aclose_f(attr_id, error)
          CALL h5tclose_f(atype_id, error)

! Create dataset floating point attributes.

          call writeCommonAtts(dset_id, time, sz_loc, iL, aspace_id)

!     CALL addH5FloatAttribute(dset_id, "time", time,aspace_id)
!     CALL addH5FloatAttribute(dset_id, "zbarInter", time, aspace_id)
!     CALL addH5FloatAttribute(dset_id, "zInter", time*lg_G, aspace_id)
!
!     call addH5IntegerAttribute(dset_id, "iUnd_cr", iUnd_cr, aspace_id)
!     call addH5IntegerAttribute(dset_id, "iChic_cr", iChic_cr, aspace_id)
!     call addH5IntegerAttribute(dset_id, "iDrift_cr", iDrift_cr, aspace_id)
!     call addH5IntegerAttribute(dset_id, "iQuad_cr", iQuad_cr, aspace_id)
!     call addH5IntegerAttribute(dset_id, "iModulation_cr", iModulation_cr, aspace_id)
!     call addH5IntegerAttribute(dset_id, 'iL', iL, aspace_id)

!    CALL h5tcopy_f(H5T_NATIVE_DOUBLE, atype_id, error)
!      aname="time"
!      attr_data_double=time
!      CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)
!      CALL h5awrite_f(attr_id, atype_id, attr_data_double, adims, error)
!      CALL h5aclose_f(attr_id, error)
!      CALL h5tclose_f(atype_id, error)
          CALL addH5StringAttribute(dset_id,"vsLabels","aperp_real, aperp_imaginary",aspace_id)
          CALL addH5StringAttribute(dset_id,"vsType","variable",aspace_id)
          CALL addH5StringAttribute(dset_id,"vsCentering","nodal",aspace_id)
          if (qoned_g) then
            CALL addH5StringAttribute(dset_id,"vsIndexOrder","compMinorF",aspace_id)
          else
            CALL addH5StringAttribute(dset_id,"vsIndexOrder","compMajorC",aspace_id)  ! WAS compMajorF
          end if

          CALL addH5StringAttribute(dset_id,"vsTimeGroup",timegrpname,aspace_id)
          CALL addH5StringAttribute(dset_id,"vsLimits",limgrpname,aspace_id)
          CALL addH5StringAttribute(dset_id,"vsMesh",meshScaledGrpname,aspace_id)
          CALL addH5StringAttribute(dset_id,"vsAxisLabels","xbar,ybar,z2bar",aspace_id)
          CALL h5dclose_f(dset_id, error)
! Time Group
          CALL writeH5TimeGroup(file_id, timegrpname, time, &
	               'outH5Field3D', error)
          CALL writeH5RunInfo(file_id,  time, sz_loc, iL, 'outH5Field3D', error)

          if (qOneD_G) then

              lb(1)=-0.5*NX_G*sLengthOfElmX_G
              lb(2)=-0.5*NY_G*sLengthOfElmY_G
              lb(3)=0.0_WP*sLengthOfElmZ2_G

              ub(1)=0.5*NX_G*sLengthOfElmX_G
              ub(2)=0.5*NY_G*sLengthOfElmY_G
              ub(3)=NZ2_G*sLengthOfElmZ2_G

          else

              lb(1)=0.0_WP*sLengthOfElmZ2_G
              lb(2)=-0.5*NY_G*sLengthOfElmY_G
              lb(3)=-0.5*NX_G*sLengthOfElmX_G
          
              ub(1)=NZ2_G*sLengthOfElmZ2_G
              ub(2)=0.5*NY_G*sLengthOfElmY_G
              ub(3)=0.5*NX_G*sLengthOfElmX_G

          end if
          
          CALL write3DlimGrp(file_id,limgrpname,lb,ub)

          if (qONED_G) then
            CALL write3DuniformMesh(file_id,meshScaledGrpname,lb,ub,(/1,1,NZ2_G-1/))
          else
            CALL write3DuniformMesh(file_id,meshScaledGrpname,lb,ub,(/NZ2_G-1,ny_g-1,nx_g-1/))
          end if

          aname="intensityScaled"
          attr_data_string="sqr(aperp_real)+sqr(aperp_imaginary)"
          attr_string_len=len(trim(adjustl(attr_data_string)))
          CALL addH5derivedVariable(file_id,aname,attr_data_string,error)
          CALL h5fclose_f(file_id, error)

        end if
      end if
!    end if

  end subroutine outputH5Field3DSD


















  !> outputH5Field3DSD is for writing the full field output.
  !! This version dumps one single file, but writes individually rather than collectively

    subroutine outputH5Field1D2CompSD(time, sz_loc, iL, error, nlonglength, rawdata, nlo, &
                                    nhi, component, createNewFlag, chkactiveflag)


      implicit none


      real(kind=wp), intent(in) :: time, sz_loc, rawdata(:)  !< The data to write
      integer(kind=ip), intent(in) :: iL
      integer(kind=ip), intent(in) :: nlonglength    !<number of cells in z in this section
      integer(kind=ip), intent(in) :: nlo,nhi        !< cell range in z in this raw data selection
      integer(kind=ip), intent(in) :: component, createNewFlag !< cell range in 4th dim in this raw data selection
      LOGICAL, intent(in) :: chkactiveflag   !< flag determines whether to test for the entire field on every rank


      integer(HID_T) :: file_id       !< File identifier
      integer(HID_T) :: dset_id       !< Dataset identifier
      integer(HID_T) :: dspace_id     !< Dataspace identifier in memory
      integer(HID_T) :: filespace     !< Dataspace identifier in file
      integer(HID_T) :: attr_id       !< Attribute identifier
      integer(HID_T) :: aspace_id     !< Attribute Dataspace identifier
      integer(HID_T) :: atype_id      !< Attribute Data type identifier
      integer(HID_T) :: group_id      !< Group identifier
      integer(HID_T) :: plist_id      !< Property list id.

  ! may yet need this, but field data is not separated amongst cores
  !    logical, intent(in) :: qSeparate

      character(len=5), parameter :: dsetname = "aperp"     ! Dataset name
      character(len=16) :: aname   ! Attribute name
  !    character(1024_IP), intent(in) :: zDFName
      character(64_IP) :: filename
      integer(HSIZE_T), dimension(2) :: fdims,dims !<no longer includes component
      integer(HSIZE_T), dimension(2) :: doffset,dsize !<no longer includes component

  ! Data as component*reducedNX*reducedNY*reducedNZ2
  ! Not described as a parameter, so can prob modify
  ! for single component (rank 3 data) like charge

      integer     ::   rank = 2               !< Dataset rank
      integer(HSIZE_T), dimension(1) :: adims !< Attribute dims
      real(kind=wp) :: attr_data_double       !< holder of attribute double data
      real(kind=wp) :: ub       !< holder of attribute double data
      real(kind=wp) :: lb       !< holder of attribute double data
      character(len=100) :: attr_data_string  !< holder of attribute strings
      integer(HSIZE_T) :: attr_string_len     !< length of attribute strings
      integer(kind=ip) :: numSpatialDims,mpiinfo      !< Attr content,
      integer     ::  arank = 1               !< Attribute Dataset rank (1: vector)
      character(len=4),  parameter :: timegrpname = "time"  !< Name of time group
      character(len=12), parameter :: limgrpname = "globalLimits"  !< Name of limits grp
      character(len=10), parameter :: meshScaledGrpname = "meshScaled" !< Name of mesh grp
      character(len=6),  parameter :: meshSIGrpname = "meshSI"  !< Dummy scaled mesh grp name
      real(kind=wp),  allocatable  :: limdata (:)  !< Data to write (diff for 1D and 3D)
      integer(kind=ip), allocatable :: numcelldata (:)  !< Mesh info for uniform grid
      ! Local vars
      integer :: error !< Error flag

  ! signature: nlonglength, dsetname, data, nlo, nhi, chkactiveflag
  ! tlflen, 'aperp_front_real', fr_rfield, [ffs,ffe], .false.
  ! tlflen, 'aperp_front_imag', fr_ifield, [ffs,ffe], .false.
  ! mainlen, 'aperp_active_real', ac_rfield, [fz2,ez2], .true.
  ! mainlen, 'aperp_active_imag', ac_ifield, [fz2,ez2], .true.
  ! tlelen, 'aperp_back_real', bk_rfield, [ees,eee], .false.
  ! tlelen, 'aperp_back_imag', bk_ifield, [ees,eee], .false.
  ! final argument  checks for all active field on single root node ...
  ! should say if qUnique or rank=0...



      mpiinfo=MPI_INFO_NULL

      if (qONED_G) then

        numSpatialDims=1
        dims = (/nlonglength,1/) ! Dataset dimensions (portion of single comp.)
        fdims = (/NZ2_G,2/)      ! File Dataset dimensions
        doffset = (/(nlo-1),component/)
        dsize = (/nhi-nlo+1,1/)

      else   ! if not 1D (WE SHOULD NOT BE HERE)
  !        numSpatialDims=3
        print *,"***Routine is set up for 1D data, but you do not have 1D data"
  !        dims = (/nx_g,ny_g,nlonglength,1/) ! Dataset dimensions
  !        fdims = (/nx_g,ny_g,NZ2_G,2/) ! Dataset dimensions
  !        doffset = (/0,0,(nlo-1),component/)
  !        dsize = (/nx_g,ny_g,nhi-nlo+1,1/)
  !        dsize = (/nx_g,ny_g,nlonglength,1/)

      end if

  !    Print*,('Spatialdims: ' // trim(IntegerToString(numSpatialDims)))

        filename = (trim(adjustl(zFilename_G)) // '_' // trim(adjustl(dsetname)) &
                 // '_' // trim(adjustl(IntegerToString(igwr))) // '.h5' )

      call h5open_f(error)

  !          Create property

      call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, error)

  !          Set property
      call h5pset_fapl_mpio_f(plist_id, tProcInfo_G%comm, mpiinfo, error)

      if (createNewFlag == 1) then

  !                  Create file

        call h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error, access_prp = plist_id)

        call h5pclose_f(plist_id, error)  ! Close property

        call h5screate_simple_f(rank, fdims, filespace, error)  ! Create filespace

        call h5dcreate_f(file_id, dsetname, H5T_NATIVE_DOUBLE, filespace, &
                         dset_id, error)  ! Create dataset

        call h5sclose_f(filespace, error) ! Close filespace

      else     ! not creating a new file, just adding data to an existing one.

        call h5pset_fapl_mpio_f(plist_id, tProcInfo_G%comm, mpiinfo, error)

  !             re-open file in parallel

        call h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, error, access_prp = plist_id)

  !             re-open dataset in parallel

        call h5dopen_f (file_id, dsetname, dset_id, error)

        call h5pclose_f(plist_id, error)

      end if

      if ((qUnique) .or. (.not. chkactiveflag)) then

        call h5screate_simple_f(rank, dims, dspace_id, error)

        if (nlonglength <= 0) call h5sselect_none_f(dspace_id,error)

  ! For the space in memory

  !      if (nlonglength > 0) then

  ! for the corresponding space on disk

        call h5dget_space_f(dset_id, filespace, error)

  !         Selecting slab on rank

        call h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, &
                                      dims, error)

        !else

  !         all ranks must participate, so select no space to write when dealing
  !         with ranks which hold no data for this field fr_real, etc

  !          Selecting empty slab on rank

        if (nlonglength <= 0) call h5sselect_none_f(filespace,error)

  !      end if


        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)

        call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_INDEPENDENT_F, error)

  !              Write data

        call h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
                        xfer_prp = plist_id, file_space_id = filespace, &
                        mem_space_id = dspace_id)

  !           Closing filespace

        call h5sclose_f(filespace, error)

  !          Closing hyperslab space

        call h5sclose_f(dspace_id, error)



  !          Closing property list

        call h5pclose_f(plist_id, error)

  !          Closing dataset

        call h5dclose_f(dset_id, error)

  !          Closing file

        call h5fclose_f(file_id, error)

      else ! if not unique

        CALL h5screate_simple_f(rank, dims, dspace_id, error)

        if (tProcInfo_G%rank /= 0) CALL h5sselect_none_f(dspace_id,error)
 ! For the space in memory
      !if (nlonglength.GT.0) then
! for the corresponding space on disk
        CALL h5dget_space_f(dset_id, filespace, error)
        CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, dims, error)
!        Print*,trim(adjustl(IntegerToString(error))) // " selecting slab on rank" &
!          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
      !else
! all ranks must participate, so select no space to write when dealing with ranks which hold no data for this field fr_real, etc
        if (tProcInfo_G%rank /= 0) CALL h5sselect_none_f(filespace,error)
!        Print*,trim(adjustl(IntegerToString(error))) // " selecting empty slab on rank" &
!          //   trim(adjustl(IntegerToString(tProcInfo_G%Rank)))
!        CALL h5sselect_none_f(dspace_id,error)
      !end if


        CALL h5pcreate_f(H5P_DATASET_XFER_F, plist_id, error)
        CALL h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_INDEPENDENT_F, error)
        CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
              xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!      Print*,'hdf5_puff:outputH5FieldSD(write done)'
!      Print*,error

      ! ABORTIVE ATTEMPT TO WRITE NODAL DATA, when really it's nodal in z2 but 1D (so
      !shouldn't matter if zonal or nodal) in x,y.
!        if (qONED_G) then
!      if (nlonglength.GT.0) then
!      doffset = (/0,1,(nlo-1),component/)
!        CALL h5dget_space_f(dset_id, filespace, error)
!       CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, dims, error)
!      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
!         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!      doffset = (/1,0,(nlo-1),component/)
!        CALL h5dget_space_f(dset_id, filespace, error)
!        CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, dims, error)
!     CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
!         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!      doffset = (/1,1,(nlo-1),component/)
!        CALL h5dget_space_f(dset_id, filespace, error)
!        CALL h5sselect_hyperslab_f (filespace, H5S_SELECT_SET_F, doffset, dims, error)
!      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
!         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!     else
!        CALL h5sselect_none_f(filespace,error)
!      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
!         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!        CALL h5sselect_none_f(filespace,error)
!      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
!         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!        CALL h5sselect_none_f(filespace,error)
!      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error, &
!         xfer_prp = plist_id, file_space_id = filespace, mem_space_id = dspace_id)
!     end if
!     end if
        CALL h5sclose_f(filespace, error)
!      Print*,'hdf5_puff:outputH5FieldSD(file space closed)'
!      Print*,error
        CALL h5sclose_f(dspace_id, error)
!      Print*,'hdf5_puff:outputH5FieldSD(hyperslab space closed)'
!      Print*,error
        CALL h5pclose_f(plist_id, error)
!      Print*,'hdf5_puff:outputH5FieldSD(propertylist closed)'
!      Print*,error
        CALL h5dclose_f(dset_id, error)
!      Print*,'hdf5_puff:outputH5FieldSD(dataset closed)'
!      Print*,error
        CALL h5fclose_f(file_id, error)

      end if

  !      end of parallel write stuff

  !        add stuff just on rank 0

      if (createNewFlag .EQ. 1) then

        if (tProcInfo_G%qRoot) then

  !             Re-opening file in serial

          call h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, error)

  !             Re-opening dataset in serial

          call h5dopen_f (file_id, dsetname, dset_id, error)

  !            Creating scalar space

          call h5screate_f(H5S_SCALAR_F, aspace_id, error)

          call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)

  ! Create dataset integer attributes.
  ! Always ndim=3 even if 1D as 1 cell in both transverse dims but same
  ! postprocessing should work, and make possible to display vs particles

          aname = "vsNumSpatialDims"

          call h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)

  !              Write the attribute data.
          call h5awrite_f(attr_id, atype_id, 1, adims, error) !

  !                Close the attribute.
          call h5aclose_f(attr_id, error)

  !              next attribute
          aname="numSpatialDims"
          call h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)
          call h5awrite_f(attr_id, atype_id, 1, adims, error)
          call h5aclose_f(attr_id, error)
          call h5tclose_f(atype_id, error)

  !     Create dataset floating point attributes.

          call writeCommonAtts(dset_id, time, sz_loc, iL, aspace_id)

  !          call addH5FloatAttribute(dset_id, "time", time,aspace_id)
  !          call addH5FloatAttribute(dset_id, "zBarInter", time, aspace_id)
  !          call addH5FloatAttribute(dset_id, "zInter", time*lg_G, aspace_id)
  !
  !          call addH5IntegerAttribute(dset_id, "iUnd_cr", iUnd_cr, aspace_id)
  !          call addH5IntegerAttribute(dset_id, "iChic_cr", iChic_cr, aspace_id)
  !          call addH5IntegerAttribute(dset_id, "iDrift_cr", iDrift_cr, aspace_id)
  !          call addH5IntegerAttribute(dset_id, "iQuad_cr", iQuad_cr, aspace_id)
  !          call addH5IntegerAttribute(dset_id, "iModulation_cr", iModulation_cr, aspace_id)
  !          call addH5IntegerAttribute(dset_id, 'iL', iL, aspace_id)


          call addH5StringAttribute(dset_id,"vsLabels","aperp_real, aperp_imaginary",aspace_id)
          call addH5StringAttribute(dset_id,"vsType","variable",aspace_id)
          call addH5StringAttribute(dset_id,"vsCentering","nodal",aspace_id)

          if (qoned_g) then

            call addH5StringAttribute(dset_id,"vsIndexOrder","compMajorC",aspace_id)

          else

            call addH5StringAttribute(dset_id,"vsIndexOrder","compMajorF",aspace_id)

          end if

          call addH5StringAttribute(dset_id,"vsTimeGroup",timegrpname,aspace_id)
          call addH5StringAttribute(dset_id,"vsLimits",limgrpname,aspace_id)
          call addH5StringAttribute(dset_id,"vsMesh",meshScaledGrpname,aspace_id)
          call addH5StringAttribute(dset_id,"vsAxisLabels","z2",aspace_id)
          call h5dclose_f(dset_id, error)

  !                       Time Group

          call writeH5TimeGroup(file_id, timegrpname, time, &
                                'radH5Field1D', error)

          call writeH5RunInfo(file_id,  time, sz_loc, iL, 'radH5Field1D', error)

          lb=0.0_WP*sLengthOfElmZ2_G  ! Lower and upper bounds...
          ub=NZ2_G*sLengthOfElmZ2_G

          call write1DlimGrp(file_id,limgrpname,lb,ub)
          call write1DuniformMesh(file_id,meshScaledGrpname,lb,ub,(NZ2_G-1),"z2,A_perp radiation field")

          aname="intensityScaled"
          attr_data_string="sqr(aperp_real)+sqr(aperp_imaginary)"
          attr_string_len=len(trim(adjustl(attr_data_string)))

          call addH5derivedVariable(file_id,aname,attr_data_string,error)

          call h5fclose_f(file_id, error)

        end if

      end if



    end subroutine outputH5Field1D2CompSD











!> CreateIntegrated1DFloat(simtime,error)
!! Creates a single integrated file for the 1D datasets
  subroutine CreateIntegrated1DFloat(simtime, sz_loc, iL, error,nslices)

    implicit none

    REAL(kind=WP), intent(in) :: simtime      !< simulation time
    real(kind=wp), intent(in) :: sz_loc        !< zbar local to current undulator module
    integer(kind=ip), intent(in) :: iL        !< lattice element counter
    INTEGER(kind=IP),intent(in) :: nslices       !< Number of slices
    INTEGER(HID_T) :: file_id       !< File identifier
    INTEGER(HID_T) :: attr_id       !< Attribute identifier
    INTEGER(HID_T) :: aspace_id     !< Attribute Dataspace identifier
    INTEGER(HID_T) :: atype_id      !< Attribute Data type identifier
    INTEGER(HID_T) :: group_id      !< Group identifier
    CHARACTER(LEN=4), PARAMETER :: timegrpname = "time"  !< Time Group name
    CHARACTER(LEN=12), PARAMETER :: limgrpname = "globalLimits"  !< Lims Group name
    CHARACTER(LEN=14), PARAMETER :: limgrpnameSI = "globalLimitsSI"  !< Lims Group name
    CHARACTER(LEN=10), PARAMETER :: meshScaledGrpname = "meshScaled" !< Mesh Group name
    CHARACTER(LEN=6), PARAMETER :: meshSIGrpname = "meshSI"  !< SI Mesh Group name
    character(1024_IP) :: filename !< output filename
    REAL(kind=WP), ALLOCATABLE :: limdata (:)  !< dataset containing limits to write
    INTEGER(kind=IP), ALLOCATABLE :: numcelldata (:)  !< Dataset to write with numcells
    integer(kind=ip) :: error !< Local Error flag
    if (tProcInfo_G%qRoot) then
      filename = ( trim(adjustl(zFilename_G)) // '_integrated_' &
        //trim(adjustl(IntegerToString(igwr))) &
        // '.h5' )
      CALL h5open_f(error)
      CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
      CALL writeH5TimeGroup(file_id, timegrpname, simtime, 'intH5field1D', error)
      CALL writeH5RunInfo(file_id,  simtime, sz_loc, iL, 'integratedH5Field1D', error)
! Limits group
!      CALL h5gcreate_f(file_id, limgrpname, group_id, error)
      CALL write1DlimGrp(file_id,limgrpname,0._wp,real((NZ2_G-1),kind=wp)*sLengthOfElmZ2_G)
      CALL write1DlimGrp(file_id,limgrpnameSI,0._wp,real((NZ2_G-1),kind=wp)*sLengthOfElmZ2_G*lc_g)

      CALL write1DuniformMesh(file_id,"intFieldMeshSc",0._wp, &
        real((NZ2_G-1),kind=wp)*sLengthOfElmZ2_G,NZ2_G,"z2,scaled parameter")

      CALL write1DuniformMesh(file_id,"intPtclMeshSc",0._wp, &
        real((NZ2_G-1),kind=wp)*sLengthOfElmZ2_G,nslices,"z2,scaled parameter")


      CALL write1DuniformMesh(file_id,"intCurrMeshSc",0._wp, &
        real((NZ2_G-1),kind=wp)*sLengthOfElmZ2_G,npts_I_G,"z2,scaled parameter")

      CALL write1DuniformMesh(file_id,"intFieldMeshSI",0._wp, &
        real((NZ2_G-1),kind=wp)*sLengthOfElmZ2_G*lc_g,NZ2_g,"z [m], SI parameter")

      CALL write1DuniformMesh(file_id,"intPtclMeshSI",0._wp, &
        real((NZ2_G-1),kind=wp)*sLengthOfElmZ2_G*lc_g,nslices,"z [m], SI parameter")

      CALL write1DuniformMesh(file_id,"intCurrMeshSI",0._wp, &
         real((NZ2_G-1),kind=wp)*sLengthOfElmZ2_G*lc_g,npts_I_G,"z [m], SI parameter")

! Close the file.

      CALL h5fclose_f(file_id, error)
      CALL h5close_f(error)

    end if
  end subroutine   CreateIntegrated1DFloat





!>addH5Field1DFloat() This subroutine writes data

  subroutine addH5Field1DFloat(writeData, dsetname, meshname, zLabels, simtime, &
                               sz_loc, iL, error)

    implicit none

    character(1024_IP) :: filename !< output filename
    integer(kind=ip) :: error !< Local Error flag
    real(kind=wp), intent(in) :: writeData(:) !< data to be written
    character(*), intent(in) :: zLabels  !< Axis labels for plotting
    real(kind=wp), allocatable :: writeDataSI(:) !< data to be written
    CHARACTER(LEN=*), intent(in) :: dsetname  !< Dataset name
    CHARACTER(LEN=*), INTENT(IN) :: meshname !<name of mesh to assign integrated data
    REAL(kind=WP), intent(in) :: simtime      !< simulation time
    real(kind=wp), intent(in) :: sz_loc  !< zbar local to the current undulator module
    integer(kind=ip), intent(in) :: iL !< Lattice element number
    INTEGER(HID_T) :: file_id       !< File identifier
    INTEGER(HID_T) :: dset_id       !< Dataset identifier
    INTEGER(HID_T) :: filespace     !< Dataspace identifier in file
    INTEGER(HID_T) :: dspace_id     !< Dataspace identifier in memory
    INTEGER     ::   rank = 1               !< Dataset rank
    INTEGER(HSIZE_T), DIMENSION(1) :: dims  !< Dataset dimensionality
    INTEGER(HSIZE_T), DIMENSION(1) :: adims !< Attribute dims
    INTEGER(HID_T) :: attr_id       !< Attribute identifier
    INTEGER(HID_T) :: aspace_id     !< Attribute Dataspace identifier
    INTEGER(HID_T) :: atype_id      !< Attribute Data type identifier
    CHARACTER(LEN=40) :: aname   !< Attribute name
    REAL(kind=WP) :: attr_data_double       !< for attrs of type double
    CHARACTER(LEN=100) :: attr_data_string  !< attrs of type string
    INTEGER(HSIZE_T) :: attr_string_len     !< len of attrs of type string
    INTEGER(kind=IP) :: numSpatialDims = 1  !< Attr content,
    CHARACTER(LEN=4), PARAMETER :: timegrpname = "time"  !< Time Group name
    CHARACTER(LEN=12), PARAMETER :: limgrpname = "globalLimits"  !< Lims Group name
    CHARACTER(LEN=14), PARAMETER :: limgrpnameSI = "globalLimitsSI"  !< Lims Group name
!    CHARACTER(LEN=10), PARAMETER :: meshScaledGrpname = "meshScaled" !< Mesh Group name
!    CHARACTER(LEN=6), PARAMETER :: meshSIGrpname = "meshSI"  !< SI Mesh Group name
!    CHARACTER(LEN=15), PARAMETER :: meshScaledGrpname = "meshIntPtclData" !< Mesh Group name
!    CHARACTER(LEN=6), PARAMETER :: meshSIGrpname = "meshIntPtclSI"  !< SI Mesh Group name
    CHARACTER(LEN=40) :: scaleToSIstring !< placeholder for scaling factor strings
    INTEGER     ::  arank = 1               !< Attribute Dataset rank
    if (tProcInfo_G%qRoot) then
      dims = size(writeData) ! Dataset dimensions
      filename = ( trim(adjustl(zFilename_G)) // '_integrated_' &
        //trim(adjustl(IntegerToString(igwr))) &
        // '.h5' )
      CALL h5open_f(error)
      CALL h5fopen_f(filename, H5F_ACC_RDWR_F, file_id, error)
      CALL h5screate_simple_f(rank, dims, filespace, error)
      CALL h5dcreate_f(file_id, trim(adjustl(dsetname)), H5T_NATIVE_DOUBLE, filespace, &
        dset_id, error)
      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, writeData, dims, error)
      CALL h5sclose_f(filespace, error)
! scalar dataset for simpler values
      CALL h5screate_f(H5S_SCALAR_F, aspace_id, error)

      call writeCommonAtts(dset_id, simtime, sz_loc, iL, aspace_id)



      CALL h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)
      aname="vsLabels"
      attr_data_string=trim(adjustl(dsetname))
      attr_string_len=len(trim(adjustl(dsetname)))
      CALL h5tset_size_f(atype_id, attr_string_len, error)
      CALL h5tset_strpad_f(atype_id, H5T_STR_SPACEPAD_F, error)
      CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)
      CALL h5awrite_f(attr_id, atype_id, attr_data_string, adims, error)
      CALL h5aclose_f(attr_id, error)
      CALL addH5StringAttribute(dset_id,"vsType","variable",aspace_id)
      if (size(writeData) .eq. NZ2_G) then
        CALL addH5StringAttribute(dset_id,"vsCentering","nodal",aspace_id)
      else
        CALL addH5StringAttribute(dset_id,"vsCentering","zonal",aspace_id)
      end if
      CALL addH5StringAttribute(dset_id,"vsIndexOrder","compMinorF",aspace_id)
      CALL addH5StringAttribute(dset_id,"vsTimeGroup",timegrpname,aspace_id)
      CALL addH5StringAttribute(dset_id,"vsLimits",limgrpnameSI,aspace_id)
      CALL addH5StringAttribute(dset_id,"vsMesh", meshname, aspace_id)
      CALL addH5StringAttribute(dset_id,"vsAxisLabels",zLabels,aspace_id)
!      aname=trim(adjustl(dsetname)) // "SI"
!      write(scaleToSIstring, '(E16.9)' ) (scalefac)
!      attr_data_string=(trim(adjustl(dsetname)) // "*" // scaleToSIstring)
!      attr_string_len=len(trim(adjustl(attr_data_string)))
!      CALL addH5derivedVariableSI(file_id,aname,attr_data_string,meshname,error)

      CALL h5sclose_f(aspace_id, error)
      CALL h5dclose_f(dset_id, error)

!       CALL h5screate_simple_f(rank, dims, filespace, error)
!       CALL h5dcreate_f(file_id, trim(adjustl(dsetname // "SI")), H5T_NATIVE_DOUBLE, filespace, &
!         dset_id, error)
!       allocate(writeDataSI(size(writeData)))
!       writeDataSI=writeData*scalefac
!       CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, writeDataSI, dims, error)
!       deallocate(writeDataSI)
!       CALL h5sclose_f(filespace, error)
! ! scalar dataset for simpler values
!       CALL h5screate_f(H5S_SCALAR_F, aspace_id, error)
!
!       call writeCommonAtts(dset_id, simtime, sz_loc, iL, aspace_id)
!
! !      CALL addH5FloatAttribute(dset_id, "time", simtime, aspace_id)
! !      CALL addH5FloatAttribute(dset_id, "zbarInter", simtime, aspace_id)
! !      CALL addH5FloatAttribute(dset_id, "zInter", simtime * lg_G, aspace_id)
! !
! !      CALL addH5IntegerAttribute(dset_id, "iCsteps", iCsteps, aspace_id)
! !      CALL addH5IntegerAttribute(dset_id, "istep", istep, aspace_id)
! !
! !      call addH5IntegerAttribute(dset_id, "iUnd_cr", iUnd_cr, aspace_id)
! !      call addH5IntegerAttribute(dset_id, "iChic_cr", iChic_cr, aspace_id)
! !      call addH5IntegerAttribute(dset_id, "iDrift_cr", iDrift_cr, aspace_id)
! !      call addH5IntegerAttribute(dset_id, "iQuad_cr", iQuad_cr, aspace_id)
! !      call addH5IntegerAttribute(dset_id, "iModulation_cr", iModulation_cr, aspace_id)
! !      call addH5IntegerAttribute(dset_id, 'iL', iL, aspace_id)
!
!
!       CALL h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)
!       aname="vsLabels"
!       attr_data_string=trim(adjustl(dsetname))
!       attr_string_len=len(trim(adjustl(dsetname)))
!       CALL h5tset_size_f(atype_id, attr_string_len, error)
!       CALL h5tset_strpad_f(atype_id, H5T_STR_SPACEPAD_F, error)
!       CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)
!       CALL h5awrite_f(attr_id, atype_id, attr_data_string, adims, error)
!       CALL h5aclose_f(attr_id, error)
!       CALL addH5StringAttribute(dset_id,"vsType","variable",aspace_id)
!       if (size(writeData) .eq. NZ2_G) then
!         CALL addH5StringAttribute(dset_id,"vsCentering","nodal",aspace_id)
!       else
!         CALL addH5StringAttribute(dset_id,"vsCentering","zonal",aspace_id)
!       end if
!       CALL addH5StringAttribute(dset_id,"vsIndexOrder","compMinorF",aspace_id)
!       CALL addH5StringAttribute(dset_id,"vsTimeGroup",timegrpname,aspace_id)
!       CALL addH5StringAttribute(dset_id,"vsLimits",limgrpname,aspace_id)
!       CALL addH5StringAttribute(dset_id,"vsMesh",(meshname//"SI"),aspace_id)
!       CALL addH5StringAttribute(dset_id,"vsAxisLabels","z2 [m],"//dsetname//"(SI)",aspace_id)
!       CALL addH5StringAttribute(dset_id,"vsAxisUnits","m,(SI)",aspace_id)
!       CALL h5sclose_f(aspace_id, error)
!       CALL h5dclose_f(dset_id, error)


      CALL h5fclose_f(file_id, error)
      CALL h5close_f(error)
    end if
  end subroutine addH5Field1DFloat



end module hdf5PuffColl
