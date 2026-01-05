! Copyright 2012-2018, University of Strathclyde
! Authors: Jonathan Smith (Tech-X UK Ltd) & Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Jonathan Smith (Tech-X UK Ltd)
!> @brief
!> This module contains the routines for writing the dumps and integrated data
!> individually from each process in hdf5 format - so multiple files are written
!> for each of the field mesh, the macroparticles, and integrated data, and they
!> must be stitched together in post.
!>
!> Routines originally written by Jonathan Smith (Tech-X UK Ltd)

module hdf5PuffID

use paratype
use HDF5
use globals
use ParallelInfoType
use lattice
USE ParallelSetUp
USE ArrayFunctions
USE TypesandConstants
Use avWrite
use hdf5PuffLow

contains

!> Output the electron bean macroparticle 
!! 6D phase space coordinates (plus weight) in Puffin.
!! For one file per rank only at the moment
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

  subroutine outputH5BeamFilesID(time, sz_loc, iL, error)
    
    implicit none
    
    real(kind=wp),intent(in) :: time !< Current time
    real(kind=wp),intent(in) :: sz_loc
    integer(kind=ip), intent(in) :: iL !< Lattice element number
    integer(HID_T) :: file_id        !< File identifier
    integer(HID_T) :: dset_id        !< Dataset identifier 
    integer(HID_T) :: dspace_id      !< Dataspace identifier in memory
    integer(HID_T) :: filespace      !< Dataspace identifier in file
    integer(HID_T) :: attr_id        !< Attribute identifier
    integer(HID_T) :: aspace_id      !< Attribute Dataspace identifier
    integer(HID_T) :: atype_id       !< Attribute Data type identifier
    integer(HID_T) :: group_id       !< Group identifier
!    logical, intent(in) :: qSeparate !<May reinstitute this.
    CHARACTER(LEN=9), PARAMETER :: dsetname = "electrons" !< Dataset name
    CHARACTER(LEN=30) :: aname   !< Attribute name
    character(1024_IP) :: filename
!    logical, intent(inout) :: qOK
!    integer(HSIZE_T), DIMENSION(1) :: dims = (/iGloNumElectrons_G/) ! Dataset dimensions
    integer(HSIZE_T), dimension(2) :: dims   !< dims of ptcl dataset (coords*numelecs)
    integer(HSIZE_T), dimension(2) :: doffset!< Offset for write, could be rank dependent
    integer(HSIZE_T), dimension(2) :: dsize  !< Size of hyperslab to write
    integer     ::  rank = 2                 !< Particle Dataset rank
    integer     ::  arank = 1                !< Attribute rank - 1 is vector
    integer(HSIZE_T), dimension(1) :: adims  !< Attribute dims
    integer(HSIZE_T), dimension(1) :: attr_data_int !< For integer attribs (numdims)
    integer     :: numSpatialDims    !< Attr content, and also num elsewhere  
!assumed 3D sim. May be 1D.
!    TYPE(C_PTR) :: f_ptr
    real(kind=wp) :: attr_data_double
    character(len=100) :: attr_data_string
    character(len=16) :: scaleToSIstring
    integer(HSIZE_T) :: attr_string_len
    character(len=4), parameter :: timegrpname = "time"  ! Group name
    character(len=12), parameter :: limgrpname = "globalLimits"  ! Group name
    real(kind=wp), allocatable :: limdata (:)  ! Data to write
    ! Local vars
    !integer(kind=ip) :: iep
    integer :: error ! Error flag

    if (qONED_G) then
      numSpatialDims=1
    else
      numSpatialDims=3
    end if
!    print*,'Writing electron data on rank', tProcInfo_G%rank

    attr_data_int(1)=numSpatialDims
    adims(1)=1 
    adims = (/1/) 
    dims = (/7,iNumberElectrons_G/) ! Dataset dimensions
    doffset=(/0,0/)
    dsize=(/1,iNumberElectrons_G/)
    attr_data_string="electrons_x,electrons_y,electrons_z,electrons_px," // &
      "electrons_py,electrons_gamma,electrons_weight"
    attr_string_len=94

! Prepare filename

    filename = ( trim(adjustl(zFilename_G)) // '_electrons_' // &
                 trim(adjustl(IntegerToString(tProcInfo_G%Rank))) // &
		 '_' // trim(adjustl(IntegerToString(iCSteps))) // '.h5' )

    CALL h5open_f(error)
!    Print*,'hdf5_puff:outputH5BeamFiles(file opened)'

! Create a new file using default properties.
    CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
!    Print*,'hdf5_puff:outputH5BeamFiles(file created)'

! Create the big dataspace in the file.
    CALL h5screate_simple_f(rank, dims, filespace, error)
!    Print*,'hdf5_puff:outputH5BeamFiles(filespace created)'

! Create the dataset with default properties.
    CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_DOUBLE, filespace, &
       dset_id, error)
!    Print*,'hdf5_puff:outputH5BeamFiles(dataset created)'
    CALL h5sclose_f(filespace, error)
!    Print*,'hdf5_puff:outputH5BeamFiles(filespace closed)'

! Create a smaller space to buffer the data writes
    CALL h5screate_simple_f(rank, dsize, dspace_id, error)
!    Print*,'hdf5_puff:outputH5BeamFiles(memory dataspace allocated)'
! Select hyperslab in the file.
    CALL H5Dget_space_f(dset_id, filespace, error)
    CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, doffset, &
       dsize, error)   
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, sElX_G, dims, error, &
       file_space_id = filespace, mem_space_id = dspace_id)

! End access to the dataset and release resources used by it.
    CALL h5sclose_f(filespace, error) 
!    CALL h5sclose_f(dspace_id, error) 
  
! repeat for some next y dataset
    doffset=(/1,0/)
    CALL H5Dget_space_f(dset_id, filespace, error)
    CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, doffset, &
       dsize, error)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, sElY_G, dims, error, &
       file_space_id = filespace, mem_space_id = dspace_id)
! was       dspace_id, filespace)
    CALL h5sclose_f(filespace, error) 

!
! repeat for some next z dataset
    doffset=(/2,0/)
    CALL H5Dget_space_f(dset_id, filespace, error)
    CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, doffset, &
       dsize, error)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, sElZ2_G, dims, error, &
       file_space_id = filespace, mem_space_id = dspace_id)
! was       dspace_id, filespace)
    CALL h5sclose_f(filespace, error) 

! repeat for some next px dataset
    doffset=(/3,0/)
    CALL H5Dget_space_f(dset_id, filespace, error)
    CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, doffset, &
       dsize, error)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, sElPX_G, dims, error, &
       file_space_id = filespace, mem_space_id = dspace_id)
! was       dspace_id, filespace)
    CALL h5sclose_f(filespace, error) 

! repeat for some next py dataset
    doffset=(/4,0/)
    CALL H5Dget_space_f(dset_id, filespace, error)
    CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, doffset, &
       dsize, error)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, sElPY_G, dims, error, &
       file_space_id = filespace, mem_space_id = dspace_id)
! was       dspace_id, filespace)
    CALL h5sclose_f(filespace, error) 

! repeat for some next gamma dataset (actually beta*gamma)
    doffset=(/5,0/)

    CALL H5Dget_space_f(dset_id, filespace, error)
    CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, doffset, &
       dsize, error)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, sElgam_G, dims, error, &
       file_space_id = filespace, mem_space_id = dspace_id)
! was       dspace_id, filespace)
    CALL h5sclose_f(filespace, error) 
!
! 
! put Chi in the file, slightly redundant as charge on a macroparticle
! doesn't increase or decrease through the simulation. But does make
! Everything self contained. Perhaps we use in future a funky h5 technique
! to point this column at a separate file which holds the data, reducing 
! the size of this column from every written file.
    doffset=(/6,0/)

    CALL H5Dget_space_f(dset_id, filespace, error)
    CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, doffset, &
       dsize, error)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, s_chi_bar_G, dims, error, &
       file_space_id = filespace, mem_space_id = dspace_id)
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

    
!    call addH5IntegerAttribute(dset_id, "iUnd_cr", iUnd_cr, aspace_id)
!    call addH5IntegerAttribute(dset_id, "iChic_cr", iChic_cr, aspace_id)
!    call addH5IntegerAttribute(dset_id, "iDrift_cr", iDrift_cr, aspace_id)
!    call addH5IntegerAttribute(dset_id, "iQuad_cr", iQuad_cr, aspace_id)
!    call addH5IntegerAttribute(dset_id, "iModulation_cr", iModulation_cr, aspace_id)
!    call addH5IntegerAttribute(dset_id, 'iL', iL, aspace_id)
!    
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

! then text attributes
    CALL h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)
    if (attr_string_len .eq. 0) attr_string_len = 1
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
!
! Terminate access to the dataset space, still using the scalar identifier
    CALL h5dclose_f(dset_id, error)

! Write time Group
    CALL writeH5TimeGroup(file_id, timegrpname, time, 'outputH5Beam', error)

! Write run info
    CALL writeH5RunInfo(file_id, time, sz_loc, iL, 'outputH5Beam', error)

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
    limdata(1)=-0.5*NX_G*sLengthOfElmX_G
! Particles inhabit a 3D physical space even for 1D simulations.
!    if (numSpatialDims .GT. 1) then
      limdata(2)=-0.5*NY_G*sLengthOfElmY_G
      limdata(3)=0.0
!    end if
    CALL h5awrite_f(attr_id, atype_id, limdata, adims, error) 
    CALL h5aclose_f(attr_id, error)
    aname="vsUpperBounds"
    CALL h5acreate_f(group_id, aname, atype_id, aspace_id, attr_id, error)
    !Print*,'hdf5_puff:outputH5BeamFiles(upper bounds attribute created)'
    limdata(1)=0.5*NX_G*sLengthOfElmX_G
! Particles inhabit a 3D physical space even for 1D simulations.
!    if (numSpatialDims .GT. 1) then
      limdata(2)=0.5*NY_G*sLengthOfElmY_G
      limdata(3)=real((NZ2_G-1),kind=wp)*sLengthOfElmZ2_G
!    end if
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

! Todo: write an expression for rms beam sizes
! Todo: write an expression for slice emittance

! Close the file.
    CALL h5fclose_f(file_id, error)

!Close the interface
    CALL h5close_f(error)

!    qOK = .true.            
    goto 2000

!     Error Handler - Error log Subroutine in CIO.f90 line 709
1000 call Error_log('Error in hdf5_puff:outputBeamFiles',tErrorLog_G)
    print*,'Error in hdf5_puff:outputBeamFiles'
2000 continue
  end subroutine outputH5BeamFilesID










!> outputH5Field3DID is for writing the full field output.
!! This version dumps on each rank separately.
  subroutine outputH5Field3DID(time, sz_loc, iL, error, nlonglength, dsetname, rawdata, nlo, nhi, chkactiveflag)
    implicit none
    REAL(kind=WP), intent(in) :: time, sz_loc, rawdata(:) !< The data to write
    integer(kind=ip), intent(in) :: iL
    CHARACTER(*), intent(in) :: dsetname !< Dataset name
    INTEGER(kind=IP), intent(in) :: nlonglength !<number of cells in z in this section
    INTEGER(kind=IP), intent(in) :: nlo,nhi !< cell range in z in this raw data selection
    LOGICAL, intent(in) :: chkactiveflag !< flag determines whether to test for the entire field on every rank
    INTEGER(HID_T) :: file_id       !< File identifier
    INTEGER(HID_T) :: dset_id       !< Dataset identifier 
    INTEGER(HID_T) :: dspace_id     !< Dataspace identifier in memory
    INTEGER(HID_T) :: filespace     !< Dataspace identifier in file
    INTEGER(HID_T) :: attr_id       !< Attribute identifier
    INTEGER(HID_T) :: aspace_id     !< Attribute Dataspace identifier
    INTEGER(HID_T) :: atype_id      !< Attribute Data type identifier
    INTEGER(HID_T) :: group_id      !< Group identifier
! may yet need this, but field data is not separated amongst cores
!    logical, intent(in) :: qSeparate
!    CHARACTER(LEN=5), PARAMETER :: dsetname = "aperp"     ! Dataset name
    CHARACTER(LEN=16) :: aname   ! Attribute name
!    character(1024_IP), intent(in) :: zDFName
    character(64_IP) :: filename
    INTEGER(HSIZE_T), DIMENSION(3) :: dims !<no longer includes component
! Data as component*reducedNX*reducedNY*reducedNZ2
! Not described as a parameter, so can prob modify 
! for single component (rank 3 data) like charge
    INTEGER     ::   rank = 3               !< Dataset rank
    INTEGER(HSIZE_T), DIMENSION(1) :: adims !< Attribute dims
    REAL(kind=WP) :: attr_data_double       !< holder of attribute double data
    REAL(kind=WP), DIMENSION(3) :: ub       !< holder of attribute double data
    REAL(kind=WP), DIMENSION(3) :: lb       !< holder of attribute double data
    CHARACTER(LEN=100) :: attr_data_string  !< holder of attribute strings 
    INTEGER(HSIZE_T) :: attr_string_len     !< length of attribute strings
    INTEGER(kind=IP) :: numSpatialDims      !< Attr content,  
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

    if (qUnique .OR. (tProcInfo_G%qRoot)) then
    if (nlonglength.GT.0) then
    if (qONED_G) then
      numSpatialDims=1
      dims = (/1,1,nlonglength/) ! Dataset dimensions
    else
      numSpatialDims=3
      dims = (/nx_g,ny_g,nlonglength/) ! Dataset dimensions
    end if
!    print *,IntegerToString(size(fr_rfield)) // " vs " //trim(adjustl(IntegerToString(Nx_g*ny_g*nlonglength)))

!    Print*,('Spatialdims: ' // trim(IntegerToString(numSpatialDims)))
    filename = (trim(adjustl(zFilename_G)) // '_' // trim(adjustl(dsetname)) &
        // '_' // trim(adjustl(IntegerToString(tProcInfo_G%Rank))) &
        // '_' // trim(adjustl(IntegerToString(iStep))) // '.h5' )
      CALL h5open_f(error)
      CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
      CALL h5screate_simple_f(rank, dims, filespace, error)
!      Print*,'hdf5_puff:outputH5FieldID(filespace created)'
      CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_DOUBLE, filespace, &
       dset_id, error)
!      Print*,'hdf5_puff:outputH5FieldID(dataset created)'
!      Print*,error
      CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, rawdata, dims, error)
!      Print*,'hdf5_puff:outputH5FieldID(write done)'
!      Print*,error
      CALL h5sclose_f(filespace, error)
!      Print*,'hdf5_puff:outputH5FieldID(filespace closed)'
!      Print*,error
      CALL h5screate_f(H5S_SCALAR_F, aspace_id, error)
!      Print*,'hdf5_puff:outputH5Field(scalar space created)'
!      CALL h5tcopy_f(H5T_NATIVE_DOUBLE, atype_id, error)


      call writeCommonAtts(dset_id, time, sz_loc, iL, aspace_id)

!      CALL addH5FloatAttribute(dset_id, "time", time,aspace_id)
!      CALL addH5FloatAttribute(dset_id, "zbarInter", time,aspace_id)
!      CALL addH5FloatAttribute(dset_id, "zInter", time*lg_G,aspace_id)
!
!      call addH5IntegerAttribute(dset_id, "iUnd_cr", iUnd_cr, aspace_id)
!      call addH5IntegerAttribute(dset_id, "iChic_cr", iChic_cr, aspace_id)
!      call addH5IntegerAttribute(dset_id, "iDrift_cr", iDrift_cr, aspace_id)
!      call addH5IntegerAttribute(dset_id, "iQuad_cr", iQuad_cr, aspace_id)
!      call addH5IntegerAttribute(dset_id, "iModulation_cr", iModulation_cr, aspace_id)
!      call addH5IntegerAttribute(dset_id, 'iL', iL, aspace_id)

!      CALL addH5FloatAttribute(dset_id, z, iStep*,aspace_id)
!      aname="time"
!      attr_data_double=time
!      CALL h5acreate_f(dset_id, aname, atype_id, aspace_id, attr_id, error)
!      CALL h5awrite_f(attr_id, atype_id, attr_data_double, adims, error) 
!      CALL h5aclose_f(attr_id, error)
!      CALL h5tclose_f(atype_id, error)
      CALL addH5StringAttribute(dset_id,"vsLabels","aperp_front_realfield",aspace_id)
      CALL addH5StringAttribute(dset_id,"vsType","variable",aspace_id)
      CALL addH5StringAttribute(dset_id,"vsCentering","nodal",aspace_id)
      CALL addH5StringAttribute(dset_id,"vsIndexOrder","compMajorF",aspace_id)
      CALL addH5StringAttribute(dset_id,"vsTimeGroup",timegrpname,aspace_id)
      CALL addH5StringAttribute(dset_id,"vsLimits",limgrpname,aspace_id)
      CALL addH5StringAttribute(dset_id,"vsMesh",meshScaledGrpname,aspace_id)
      CALL addH5StringAttribute(dset_id,"vsAxisLabels","xbar,ybar,z2bar",aspace_id)
      CALL h5dclose_f(dset_id, error)	  
! Time Group 
      CALL writeH5TimeGroup(file_id, timegrpname, time, &
	     'outH5Field3D', error)
      CALL writeH5RunInfo(file_id, time, sz_loc, iL, 'outH5Field3D', error)
      lb(1)=-0.5*NX_G*sLengthOfElmX_G
      lb(2)=-0.5*NY_G*sLengthOfElmY_G
      lb(3)=(nlo-1)*sLengthOfElmZ2_G
      ub(1)=0.5*NX_G*sLengthOfElmX_G
      ub(2)=0.5*NY_G*sLengthOfElmY_G
      ub(3)=nhi*sLengthOfElmZ2_G
      CALL write3DlimGrp(file_id,limgrpname,lb,ub)
      CALL write3DuniformMesh(file_id,meshScaledGrpname,lb,ub,(/nx_g,ny_g,nlonglength/))
      CALL h5fclose_f(file_id, error)
    end if
   end if
  end subroutine outputH5Field3DID

end module hdf5PuffID
