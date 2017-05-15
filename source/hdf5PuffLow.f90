! Copyright 2012-2017, University of Strathclyde
! Authors: Jonathan Smith (Tech-X UK Ltd) & Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Jonathan Smith (Tech-X UK Ltd)
!> @brief
!> This module contains low level writing functions for attribute writing 
!> etc to hdf5 from Puffin.
!>
!> Routines originally written by Jonathan Smith (Tech-X UK Ltd)


module hdf5PuffLow

use paratype
use globals
use ParallelInfoType
use lattice
USE ParallelSetUp
USE ArrayFunctions
USE TypesandConstants
use hdf5

contains

!> Simple wrapper routine to write a string attribute into hdf5
!! Assumed that the padding is set up outside here
!! Assumed that the h5 dataspace is set up outside here.

  subroutine addH5StringAttribute(locHandle,attrName,attrValue,aspace_id)

    implicit none

    integer(HID_T), intent(in) :: locHandle   !< h5 handle of write location
    character(len=*), intent(in) :: attrName  !<attrib name
    character(len=*), intent(in) :: attrValue !<attrib value
    integer(HID_T), intent(in) :: aspace_id   !< Attribute Dataspace identifier
    ! Local vars
    integer(HID_T) :: attr_id                 !< Attribute identifier
    integer(HID_T) :: atype_id                !< Attribute Data type identifier
    integer(HSIZE_T) :: attr_string_len       !< Length of attribute string 
    integer(HSIZE_T), dimension(1) :: adims=(/1/) !< Attribute Data type identifier
    integer :: error                             !< Error flag

!    aname="vsType"
!    attr_data_string="vsVars"

    call h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)

    attr_string_len=len(trim(adjustl(attrValue)))

    call h5tset_size_f(atype_id, attr_string_len, error)

    call h5acreate_f(locHandle, attrName, atype_id, aspace_id, attr_id, error)

    call h5awrite_f(attr_id, atype_id, attrValue, adims, error) 

    call h5aclose_f(attr_id, error)

    call h5tclose_f(atype_id, error)

  end subroutine







!> Simple wrapper routine to write a float attribute into hdf5
!! Assumed that the padding is set up outside here
!! Assumed that the h5 dataspace is set up outside here.

  subroutine addH5FloatAttribute(locHandle,attrName,attrValue,aspace_id)
    
    implicit none

    integer(HID_T), intent(in) :: locHandle   !< h5 handle of write location
    character(LEN=*), intent(in) :: attrName  !<attrib name
    real(kind=wp), intent(in) :: attrValue !<attrib value
    integer(HID_T), intent(in) :: aspace_id   !< Attribute Dataspace identifier
    ! Local vars

    integer(HID_T) :: attr_id                 !< Attribute identifier
    integer(HID_T) :: atype_id                !< Attribute Data type identifier
    integer(HSIZE_T) :: attr_string_len       !< Length of attribute string 
    integer(HSIZE_T), dimension(1) :: adims=(/1/) !< Attribute Data type identifier
    integer :: error                             !< Error flag

!    aname="vsType"
!    attr_data_string="vsVars"

    call h5tcopy_f(H5T_NATIVE_DOUBLE, atype_id, error)
    call h5acreate_f(locHandle, attrName, atype_id, aspace_id, attr_id, error)
    call h5awrite_f(attr_id, atype_id, attrValue, adims, error) 
    call h5aclose_f(attr_id, error)
    call h5tclose_f(atype_id, error)

  end subroutine addH5FloatAttribute






  subroutine addH5IntegerAttribute(locHandle,attrName,attrValue,aspace_id)

    implicit none

    integer(HID_T), intent(in) :: locHandle   !< h5 handle of write location
    character(LEN=*), intent(in) :: attrName  !<attrib name
    integer(kind=IP), intent(in) :: attrValue !<attrib value
    integer(HID_T), intent(in) :: aspace_id   !< Attribute Dataspace identifier

    ! Local vars

    integer(HID_T) :: attr_id                 !< Attribute identifier
    integer(HID_T) :: atype_id                !< Attribute Data type identifier
    integer(HSIZE_T) :: attr_string_len       !< Length of attribute string 
    integer(HSIZE_T), dimension(1) :: adims=(/1/) !< Attribute Data type identifier
    integer :: error                             !< Error flag
!    aname="vsType"
!    attr_data_string="vsVars"

    call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
    call h5acreate_f(locHandle, attrName, atype_id, aspace_id, attr_id, error)
    call h5awrite_f(attr_id, atype_id, attrValue, adims, error) 
    call h5aclose_f(attr_id, error)
    call h5tclose_f(atype_id, error)

  end subroutine addH5IntegerAttribute



!> addH5derivedVariable() add vizschema derived variable
  subroutine addH5derivedVariable(location,varName,expression,error)

    implicit none

    integer(HID_T), intent(in) :: location !< h5 handle of write location
    character(len=*), intent(in) :: varName !<derived var name
    character(len=*), intent(in) :: expression !<expression value

    ! Local vars

    integer(HID_T) :: group_id      !< Group identifier
    integer(HID_T) :: aspace_id     !< Attribute Dataspace identifier
    integer :: error ! Error flag

!               We make a new group

    call h5gcreate_f(location, varName, group_id, error)
    call h5screate_f(H5S_SCALAR_F, aspace_id, error)
    call addH5StringAttribute(group_id,"vsType","vsVars",aspace_id)
    call addH5StringAttribute(group_id,varName,expression,aspace_id)
    call h5sclose_f(aspace_id, error)
    call h5gclose_f(group_id, error)

  end subroutine addH5derivedVariable

!> addH5derivedVariable() add vizschema derived variable
  subroutine addH5derivedVariableSI(location,varName,expression,meshname,error)

    implicit none

    integer(HID_T), intent(in) :: location !< h5 handle of write location
    character(len=*), intent(in) :: varName !<derived var name
    character(len=*), intent(in) :: expression !<expression value
    character(len=*), intent(in) :: meshname !<expression value

    ! Local vars

    integer(HID_T) :: group_id      !< Group identifier
    integer(HID_T) :: aspace_id     !< Attribute Dataspace identifier
    integer :: error ! Error flag

!               We make a new group

    call h5gcreate_f(location, varName, group_id, error)
    call h5screate_f(H5S_SCALAR_F, aspace_id, error)
    call addH5StringAttribute(group_id,"vsType","vsVars",aspace_id)
    call addH5StringAttribute(group_id,"vsMesh",trim(adjustl(meshname // "SI")),aspace_id)
!    call addH5StringAttribute(group_id,"vsLimits","globalLimitsSI",aspace_id)
    call addH5StringAttribute(group_id,varName,expression,aspace_id)
    call h5sclose_f(aspace_id, error)
    call h5gclose_f(group_id, error)

  end subroutine addH5derivedVariableSI




!> Write a 3D array of real, floating point values

  subroutine write3DfloatAttribute(location, aname, valarray)

    implicit none

    integer(HID_T), intent(in) :: location   !< h5 handle of write location
    character(LEN=*), intent(in) :: aname    !<derived var name
    real(kind=wp), intent(in) :: valarray(:) !<the array to be written
    integer(HSIZE_T), dimension(1) :: adims=(/3/)  !< Attribute dims
    integer        ::  arank = 1                !< Attribute rank - 1 is vector
    integer(HID_T) :: aspace_id              !< Attribute Dataspace identifier
    integer(HID_T) :: atype_id                !< Attribute Data type identifier
    integer(HID_T) :: attr_id                 !< Attribute identifier
    integer        :: error                   !< Error flag

    call h5screate_simple_f(arank, adims, aspace_id, error)
    call h5tcopy_f(H5T_NATIVE_DOUBLE, atype_id, error)
    call h5acreate_f(location, aname, atype_id, aspace_id, attr_id, error)
!    Print*,'hdf5_puff:write3DfloatAttribute(' // trim(adjustl(aname)) // ')'
!    ALLOCATE ( limdata(numSpatialDims))
!    limdata(1)=-0.5*(NX_G-1_IP)*sLengthOfElmX_G
!    limdata(2)=-0.5*(NY_G-1_IP)*sLengthOfElmY_G
!    limdata(3)=0.0
    call h5awrite_f(attr_id, atype_id, valarray, adims, error) 
!    Print*,error
    call h5aclose_f(attr_id, error)
!    Print*,error
    call h5tclose_f(atype_id, error)
    call h5sclose_f(aspace_id, error)

  end subroutine write3DfloatAttribute






!> Write a 3D array of integer values

  subroutine write3DintAttribute(location, aname, valarray)

    implicit none

    integer(HID_T), intent(in) :: location   !< h5 handle of write location
    character(LEN=*), intent(in) :: aname    !<derived var name
    integer(kind=IP), intent(in) :: valarray(:) !<the array to be written
    integer(HSIZE_T), dimension(1) :: adims=(/3/)  !< Attribute dims
    integer        ::  arank = 1                !< Attribute rank - 1 is vector
    integer(HID_T) :: aspace_id              !< Attribute Dataspace identifier
    integer(HID_T) :: atype_id                !< Attribute Data type identifier
    integer(HID_T) :: attr_id                 !< Attribute identifier
    integer        :: error                   !< Error flag


    call h5screate_simple_f(arank, adims, aspace_id, error)

    call h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)

    call h5acreate_f(location, aname, atype_id, aspace_id, attr_id, error)

    call h5awrite_f(attr_id, atype_id, valarray, adims, error) 

    call h5aclose_f(attr_id, error)

    call h5tclose_f(atype_id, error)

    call h5sclose_f(aspace_id, error)

  end subroutine write3DintAttribute








!> Subroutine to write 3d limit data

  subroutine write3DlimGrp(location,limgrpname,lb,ub)
    
    implicit none

    integer(HID_T), intent(in) :: location        !< h5 handle of write location
    character(len=*), intent(in) :: limgrpname    !<derived var name
    real(kind=wp), dimension(3), intent(in) :: ub(:), lb(:)     !<Bounds to write
    integer(HID_T) :: group_id      !< Group identifier
    integer        :: error                   !< Error flag
    integer(HID_T) :: aspace_id              !< Attribute Dataspace identifier

    call h5gcreate_f(location, limgrpname, group_id, error)

    call h5screate_f(H5S_SCALAR_F, aspace_id, error)

    call addH5StringAttribute(group_id,"vsType","limits",aspace_id)

    call addH5StringAttribute(group_id,"vsKind","Cartesian",aspace_id)

    call write3DfloatAttribute(group_id, "vsLowerBounds", lb)

    call write3DfloatAttribute(group_id, "vsUpperBounds", ub)

    call h5sclose_f(aspace_id, error)    

    call h5gclose_f(group_id, error)

  end subroutine write3DlimGrp







!> Subroutine to write 1d or limit data

  subroutine write1DlimGrp(location,limgrpname,lb,ub)
    
    implicit none
    
    integer(HID_T), intent(in) :: location        !< h5 handle of write location
    character(len=*), intent(in) :: limgrpname    !<derived var name
    real(kind=wp), intent(in) :: ub, lb           !<Bounds to write
    integer(HID_T) :: group_id                    !< Group identifier
    integer        :: error                       !< Error flag
    integer(HID_T) :: aspace_id                   !< Attribute Dataspace identifier

    call h5gcreate_f(location, limgrpname, group_id, error)

    call h5screate_f(H5S_SCALAR_F, aspace_id, error)

    call addH5StringAttribute(group_id,"vsType","limits",aspace_id)

    call addH5StringAttribute(group_id,"vsKind","Cartesian",aspace_id)

    call addH5FloatAttribute(group_id, "vsLowerBounds", lb,aspace_id)

    call addH5FloatAttribute(group_id, "vsUpperBounds", ub,aspace_id)

    call h5sclose_f(aspace_id, error)    

    call h5gclose_f(group_id, error)

  end subroutine write1DlimGrp






!> Subroutine to write 3d mesh data

  subroutine write3DuniformMesh(location,meshname,lb,ub,numcells)

    implicit none

    integer(HID_T), intent(in) :: location        !< h5 handle of write location
    character(len=*), intent(in) :: meshname    !<derived var name
    real(kind=wp), dimension(3), intent(in) :: ub(:), lb(:)     !<Bounds to write
    integer(kind=ip), dimension(3), intent(in) :: numcells(:) !<the array to be written
    integer(HID_T) :: group_id      !< Group identifier
    integer        :: error                   !< Error flag
    integer(HID_T) :: aspace_id              !< Attribute Dataspace identifier


    call h5gcreate_f(location, meshname, group_id, error)

    call h5screate_f(H5S_SCALAR_F, aspace_id, error)

    call addH5StringAttribute(group_id,"vsType","mesh",aspace_id)

    call addH5StringAttribute(group_id,"vsKind","uniform",aspace_id)

    call addH5StringAttribute(group_id,"vsCentering","nodal",aspace_id)

    call addH5StringAttribute(group_id,"vsIndexOrder","compMajorF",aspace_id)

    call h5sclose_f(aspace_id, error)    

    call write3DfloatAttribute(group_id, "vsLowerBounds", lb)

    call write3DfloatAttribute(group_id, "vsUpperBounds", ub)

    call write3DintAttribute(group_id, "vsStartCell", (/0,0,0/))

    call write3DintAttribute(group_id, "vsNumCells", numcells)

    call h5gclose_f(group_id, error)

  end subroutine write3DuniformMesh





  !> Subroutine to write 1d mesh data

  subroutine write1DuniformMesh(location,meshname,lb,ub,numcells,labels)

    implicit none

    integer(HID_T), intent(in) :: location        !< h5 handle of write location
    character(len=*), intent(in) :: meshname    !<derived var name
    character(len=*), intent(in) :: labels    !<derived var name
    real(kind=wp), intent(in) :: ub, lb   !<Bounds to write
    integer(kind=IP), intent(in) :: numcells !<the array to be written
    integer(kind=IP) :: startcell=0 !<another array to be written
    integer(HID_T) :: group_id      !< Group identifier
    integer        :: error                   !< Error flag
    integer(HID_T) :: aspace_id              !< Attribute Dataspace identifier

    call h5gcreate_f(location, meshname, group_id, error)
    call h5screate_f(H5S_SCALAR_F, aspace_id, error)
    call addH5StringAttribute(group_id,"vsType","mesh",aspace_id)
    call addH5StringAttribute(group_id,"vsKind","uniform",aspace_id)
    call addH5StringAttribute(group_id,"vsCentering","nodal",aspace_id)
    call addH5StringAttribute(group_id,"vsIndexOrder","compMajorF",aspace_id)
    call addH5StringAttribute(group_id,"vsAxisLabels",labels,aspace_id)
    call addH5FloatAttribute(group_id, "vsLowerBounds", lb,aspace_id)
    call addH5FloatAttribute(group_id, "vsUpperBounds", ub,aspace_id)
    call addH5IntegerAttribute(group_id, "vsStartCell", startcell,aspace_id)
    call addH5IntegerAttribute(group_id, "vsNumCells", numcells,aspace_id)

    call h5sclose_f(aspace_id, error)    

    call h5gclose_f(group_id, error)

  end subroutine write1DuniformMesh






  subroutine writeCommonAtts(dset_id, simtime, z_loc, iL, aspace_id)

    integer(HID_T), intent(in) :: dset_id   !< h5 handle of write location
    real(kind=wp), intent(in) :: simtime   !< Current simulation 'time' (zbar)
    real(kind=wp), intent(in) :: z_loc     !< zbar local to current undulator module
    integer(kind=ip), intent(in) :: iL        !< lattice element counter
    integer(HID_T), intent(in) :: aspace_id   !< h5 handle of write location

    CALL addH5FloatAttribute(dset_id, "time", simtime, aspace_id)
    
    call addH5FloatAttribute(dset_id, "zbarTotal", simtime, aspace_id)
    call addH5FloatAttribute(dset_id, "zTotal", simtime * lg_G, aspace_id)

    CALL addH5FloatAttribute(dset_id, "zbarInter", sZi_G, aspace_id)
    CALL addH5FloatAttribute(dset_id, "zInter", sZi_G * lg_G, aspace_id)
    
    call addH5FloatAttribute(dset_id, "zbarLocal", z_loc, aspace_id)
    call addH5FloatAttribute(dset_id, "zLocal", z_loc * lg_G,aspace_id)

    CALL addH5IntegerAttribute(dset_id, "iCsteps", iCsteps, aspace_id)
    CALL addH5IntegerAttribute(dset_id, "istep", istep, aspace_id)
    
    call addH5IntegerAttribute(dset_id, "iUnd_cr", iUnd_cr, aspace_id)
    call addH5IntegerAttribute(dset_id, "iChic_cr", iChic_cr, aspace_id)
    call addH5IntegerAttribute(dset_id, "iDrift_cr", iDrift_cr, aspace_id)
    call addH5IntegerAttribute(dset_id, "iQuad_cr", iQuad_cr, aspace_id)
    call addH5IntegerAttribute(dset_id, "iModulation_cr", iModulation_cr, aspace_id)
    call addH5IntegerAttribute(dset_id, 'iL', iL, aspace_id)  

  end subroutine writeCommonAtts



  subroutine writeRunAtts(dset_id, simtime, z_loc, iL, aspace_id)

    integer(HID_T), intent(in) :: dset_id     !< h5 handle of write location
    real(kind=wp), intent(in) :: simtime      !< Current simulation 'time' (zbar)
    real(kind=wp), intent(in) :: z_loc        !< zbar local to current undulator module
    integer(kind=ip), intent(in) :: iL        !< lattice element counter
    integer(HID_T), intent(in) :: aspace_id   !< h5 handle of write location

    CALL addH5FloatAttribute(dset_id, "time", simtime, aspace_id)
    
    call addH5FloatAttribute(dset_id, "zbarTotal", simtime, aspace_id)
    call addH5FloatAttribute(dset_id, "zTotal", simtime * lg_G, aspace_id)

    CALL addH5FloatAttribute(dset_id, "zbarInter", sZi_G, aspace_id)
    CALL addH5FloatAttribute(dset_id, "zInter", sZi_G * lg_G, aspace_id)
    
    call addH5FloatAttribute(dset_id, "zbarLocal", z_loc, aspace_id)
    call addH5FloatAttribute(dset_id, "zLocal", z_loc * lg_G,aspace_id)

    CALL addH5IntegerAttribute(dset_id, "iCsteps", iCsteps, aspace_id)
    CALL addH5IntegerAttribute(dset_id, "istep", istep, aspace_id)
    
    call addH5IntegerAttribute(dset_id, "iUnd_cr", iUnd_cr, aspace_id)
    call addH5IntegerAttribute(dset_id, "iChic_cr", iChic_cr, aspace_id)
    call addH5IntegerAttribute(dset_id, "iDrift_cr", iDrift_cr, aspace_id)
    call addH5IntegerAttribute(dset_id, "iQuad_cr", iQuad_cr, aspace_id)
    call addH5IntegerAttribute(dset_id, "iModulation_cr", iModulation_cr, aspace_id)
    call addH5IntegerAttribute(dset_id, 'iL', iL, aspace_id)  
    
    call addH5IntegerAttribute(dset_id, 'nX', nX_G, aspace_id)  
    call addH5IntegerAttribute(dset_id, 'nY', nY_G, aspace_id)  
    call addH5IntegerAttribute(dset_id, 'nZ2', nZ2_G, aspace_id)  
    
    call addH5FloatAttribute(dset_id, 'sLengthOfElmX', sLengthOfElmX_G, aspace_id)  
    call addH5FloatAttribute(dset_id, 'sLengthOfElmY', sLengthOfElmY_G, aspace_id)  
    call addH5FloatAttribute(dset_id, 'sLengthOfElmZ2', sLengthOfElmZ2_G, aspace_id)  
    call addH5FloatAttribute(dset_id, 'sStepSize', sStepSize, aspace_id)  
    call addH5IntegerAttribute(dset_id, 'nSteps', nSteps, aspace_id)  
    call addH5FloatAttribute(dset_id, 'rho', sRho_G, aspace_id)  
    call addH5FloatAttribute(dset_id, 'aw', sAw_G, aspace_id)  
    call addH5FloatAttribute(dset_id, 'eta', seta_G, aspace_id)  
    call addH5FloatAttribute(dset_id, 'gamma_r', sGammaR_G, aspace_id)
    call addH5FloatAttribute(dset_id, 'kappa', sKappa_G, aspace_id)  
    call addH5FloatAttribute(dset_id, 'npk_bar', npk_bar_G, aspace_id)  
    call addH5FloatAttribute(dset_id, 'Lg', lg_G, aspace_id)  
    call addH5FloatAttribute(dset_id, 'Lc', lc_G, aspace_id)  
    call addH5FloatAttribute(dset_id, 'lambda_w', lam_w_G, aspace_id)
    call addH5FloatAttribute(dset_id, 'lambda_r', lam_r_G, aspace_id)  
    !call addH5IntegerAttribute(dset_id, 'qOneD', qOneD_G, aspace_id)  
    !call addH5IntegerAttribute(dset_id, 'iL', iL, aspace_id)  
    !call addH5IntegerAttribute(dset_id, 'iL', iL, aspace_id)  
    !call addH5IntegerAttribute(dset_id, 'iL', iL, aspace_id)  
    !call addH5IntegerAttribute(dset_id, 'iL', iL, aspace_id)  
    !call addH5IntegerAttribute(dset_id, 'iL', iL, aspace_id)  
    !call addH5IntegerAttribute(dset_id, 'iL', iL, aspace_id)  
    !call addH5IntegerAttribute(dset_id, 'iL', iL, aspace_id)  
    !call addH5IntegerAttribute(dset_id, 'iL', iL, aspace_id)  
    !call addH5IntegerAttribute(dset_id, 'iL', iL, aspace_id)  

  end subroutine writeRunAtts
  
  
  subroutine writeH5TimeGroup(file_id, timegrpname, simtime, callerstr, error)

    INTEGER(HID_T) :: file_id       ! File identifier
    CHARACTER(LEN=4), intent(in) :: timegrpname  ! Group name
    REAL(kind=WP), intent(in) :: simtime ! Current time
    CHARACTER(LEN=12), intent(in) :: callerstr
    INTEGER(kind=ip) :: error
!
! Local
    INTEGER(HID_T) :: attr_id       ! Attribute identifier
    INTEGER(HID_T) :: aspace_id     ! Attribute Dataspace identifier
    INTEGER(HID_T) :: atype_id      ! Attribute Data type identifier
    INTEGER(HID_T) :: group_id      ! Group identifier
    INTEGER     ::   rank = 1               ! Dataset rank
    INTEGER(HSIZE_T), DIMENSION(1) :: adims ! Attribute dims
    REAL(kind=WP) :: attr_data_double
    CHARACTER(LEN=100) :: attr_data_string
    INTEGER(HSIZE_T) :: attr_string_len
    INTEGER(kind=IP) :: numSpatialDims = 1   ! Attr content,  
    INTEGER     ::  arank = 1               ! Attribute Dataset rank
    CHARACTER(LEN=16) :: aname   ! Attribute name

! with the main dataset done we work on the other groups with attributes
! We make a group
    CALL h5gcreate_f(file_id, timegrpname, group_id, error)
!   Print*,'hdf5_puff:' // callerstr // '(group timegrpname created)'
    CALL h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)
!   Print*,'hdf5_puff:outputH5power(set timegrpname type)'
    CALL h5tset_strpad_f(atype_id, H5T_STR_SPACEPAD_F, error)
!    Print*,'hdf5_puff:outputH5power(string padding enabled)'
    aname="vsType"
    attr_data_string="time"
    attr_string_len=len(trim(adjustl(attr_data_string)))
    CALL h5tset_size_f(atype_id, attr_string_len, error)
!   Print*,'hdf5_puff:outputH5power(set timegrpname size)'
!    Print*,error
    CALL h5screate_f(H5S_SCALAR_F, aspace_id, error)
!    Print*,('hdf5_puff:' // callerstr // '(scalar attr space created)')
!    Print*,error   
    CALL h5acreate_f(group_id, aname, atype_id, aspace_id, attr_id, error)
!    Print*,'hdf5_puff:' // callerstr // '(create timegrp vstype at)'
!    Print*,error
    CALL h5awrite_f(attr_id, atype_id, attr_data_string, adims, error) 
!    Print*,'hdf5_puff:' // callerstr // '(write timegrp vstype attr)'
!    Print*,error
    CALL h5aclose_f(attr_id, error)
!    Print*,'hdf5_puff:' // callerstr // '(close timegrp vstype attr)'
!    Print*,error

    CALL h5tclose_f(atype_id, error)
!    Print*,'hdf5_puff:' // callerstr // '(close timegrpname time attr-type )'
!    Print*,error

    CALL h5tcopy_f(H5T_NATIVE_DOUBLE, atype_id, error)
    aname="vsTime"
    attr_data_double=simtime
    CALL h5acreate_f(group_id, aname, atype_id, aspace_id, attr_id, error)
!    print*,'hdf5_puff:' // callerstr // ' create vstime attribute'
!    Print*,error
    CALL h5awrite_f(attr_id, atype_id, attr_data_double, adims, error) 
!    print*,'hdf5_puff:' // callerstr // ' write vstime attribute'
!    Print*,error
    CALL h5aclose_f(attr_id, error)
    CALL h5tclose_f(atype_id, error)
    CALL h5tcopy_f(H5T_NATIVE_INTEGER, atype_id, error)
    aname="vsStep"
    CALL h5acreate_f(group_id, aname, atype_id, aspace_id, attr_id, error)
    CALL h5awrite_f(attr_id, atype_id, iCSteps, adims, error) 
    CALL h5tclose_f(atype_id, error)
    CALL h5aclose_f(attr_id, error)
!    print*,'hdf5_puff:' // callerstr // ' close vsStep attribute'
    CALL h5sclose_f(aspace_id, error)
    CALL h5gclose_f(group_id, error)
!    Print*,error
!    Print*,'Closing timeGroup'
!    Print*,error

  end subroutine writeH5TimeGroup

!> writeH5RunInfo
!! Routine to write provenance data to the vizschema hdf5 files
!! @param file_id file identifier, location to put provenance data
!! @callerstr allows the passing in of information about what is
!!   asking for the run information to be written (ie parent routine)
   subroutine writeH5RunInfo(file_id, simtime, z_loc, iL, callerstr, error)

    use PuffProvenance

    INTEGER(HID_T), INTENT(in) :: file_id 
    real(kind=wp), intent(in) :: simtime      !< Current simulation 'time' (zbar)
    real(kind=wp), intent(in) :: z_loc        !< zbar local to current undulator module
    integer(kind=ip), intent(in) :: iL        !< lattice element counter
    CHARACTER(LEN=12), intent(in) :: callerstr
    INTEGER(kind=ip) :: error
!
! Local
    INTEGER(HID_T) :: attr_id       !< Attribute identifier
    INTEGER(HID_T) :: aspace_id     !< Attribute Dataspace identifier
    INTEGER(HID_T) :: atype_id      !< Attribute Data type identifier
    INTEGER(HID_T) :: group_id      !< Group identifier
    INTEGER     ::   rank = 1               !< Dataset rank
    INTEGER(HSIZE_T), DIMENSION(1) :: adims !< Attribute dims
    REAL(kind=WP) :: attr_data_double       !< attrib data (type double)
    CHARACTER(LEN=1024) :: attr_data_string !< attrib data (type string)
    INTEGER(HSIZE_T) :: attr_string_len     !< length of attrib string
    INTEGER(kind=IP) :: numSpatialDims = 1  !< Attr content, identifying nu 
    INTEGER     ::  arank = 1               !< Attribute Dataset rank
    CHARACTER(LEN=24) :: aname   ! Attribute name
    character(8)  :: date
    character(10) :: time
    character(5)  :: zone
    integer,dimension(8) :: values
    call date_and_time(date,time,zone,values)
 ! with the main dataset done we work on the other groups with attributes
! We make a group
    CALL h5gcreate_f(file_id, 'runInfo' , group_id, error)
 !   Print*,'hdf5_puff:' // callerstr // '(group runinfo created)'
    CALL h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, error)
    CALL h5tset_strpad_f(atype_id, H5T_STR_SPACEPAD_F, error)
    aname="vsType"
    attr_data_string="runInfo"
    attr_string_len=len(trim(attr_data_string))
    CALL h5tset_size_f(atype_id, attr_string_len, error)
    CALL h5screate_f(H5S_SCALAR_F, aspace_id, error)
!    Print*,('hdf5_puff:' // callerstr // '(scalar attr space created)')
!    Print*,error   
    CALL h5acreate_f(group_id, aname, atype_id, aspace_id, attr_id, error)
    CALL h5awrite_f(attr_id, atype_id, attr_data_string, adims, error) 
    CALL h5aclose_f(attr_id, error)
!    Print*,'hdf5_puff:' // callerstr // '(close timegrpname time attr)'
!    Print*,error
    CALL addH5StringAttribute(group_id,"vsSoftware","PUFFIN",aspace_id)
    
!    write(attr_data_string, '(8i5)') values
!    write(attr_data_string, '(5i4-3i2-3i2) zone (4i3 3i2:3i2:3i2 .4i3)') values
    write(attr_data_string, '(a,a,a,a,a,2x,a,a,a,a,a,1x,a)') date(1:4),'-',date(5:6), &
      '-',date(7:8),time(1:2),':',time(3:4),':',time(5:10), zone
    CALL addH5StringAttribute(group_id,"vsRunDate",attr_data_string,aspace_id)
    CALL GETLOG(attr_data_string)
    CALL addH5StringAttribute(group_id,"vsUser",attr_data_string,aspace_id)
    CALL HOSTNM(attr_data_string)
    CALL addH5StringAttribute(group_id,"vsRunHost",attr_data_string,aspace_id)
    CALL addH5StringAttribute(group_id,"vsBuildConfigDate", timeStamp, aspace_id)

!    attr_data_string='@GIT_BRANCH@ : @GIT_REVISION@' 
    CALL addH5StringAttribute(group_id,"vsSwRevision", gitBranch,aspace_id)

!    attr_data_string='@Puffin_VERSION_MAJOR@.@Puffin_VERSION_MINOR@.@Puffin_VERSION_PATCH@' 
    CALL addH5StringAttribute(group_id,"vsSwVersion", puffVersion,aspace_id)
   
    CALL addH5StringAttribute(group_id,"vsVsVersion","3.0",aspace_id)
    CALL addH5StringAttribute(group_id,"vsFCompiler", fortCompiler, aspace_id)
    CALL addH5StringAttribute(group_id,"vsFCompilerVersion", fortVersion, aspace_id)
    CALL addH5StringAttribute(group_id,"vsFCompilerFlags", fortFlags, aspace_id)
    CALL addH5StringAttribute(group_id,"vsBuildHost", buildHost, aspace_id)
    CALL addH5StringAttribute(group_id,"vsBuildHostType", hostType, aspace_id)

! required Fotran 2003. Not sure if we really want to do this.
    CALL get_command(attr_data_string)
    CALL addH5StringAttribute(group_id,"vsCommandLine",attr_data_string,aspace_id)
! rest I think is less sensitive to FORTRAN version


    CALL addH5StringAttribute(group_id,"vsInputFile",zFilename_G,aspace_id)
    CALL addH5StringAttribute(group_id,"vsBeamFile",zBFile_G,aspace_id)
    CALL addH5StringAttribute(group_id,"vsSeedFile",zSFile_G,aspace_id)
    
    call writeRunAtts(group_id,  simtime, z_loc, iL, aspace_id)
!    aname="vsSeedFile"
!    attr_data_string=zSFile_G
!    attr_string_len=len(attr_data_string)
!    CALL h5tset_size_f(atype_id, attr_string_len, error)
!    CALL h5acreate_f(group_id, aname, atype_id, aspace_id, attr_id, error)
!    CALL h5awrite_f(attr_id, atype_id, attr_data_string, adims, error) 
!    CALL h5aclose_f(attr_id, error)
    CALL h5sclose_f(aspace_id, error)
!    Print*,error
    CALL h5tclose_f(atype_id, error)
    CALL h5gclose_f(group_id, error)
!    Print*,'Closing runInfo'
!    Print*,error

  end subroutine writeH5RunInfo
 
!  subroutine createH5Files(tArrayY, zDFName, zOptionalString, qOK)
!
!    implicit none

! Create "Full" Files - creates either 
! the full data sets for the field and 
! electron phase space.

!    type(cArraySegment), intent(inout) :: tArrayY(:)
!   character(1024_IP), intent(in)   ::   zDFName
!    character(*), intent(in), optional  :: zOptionalString
!    logical, intent(inout) :: qOK

!    integer(kind=ip) :: iap
!    character(1024_IP) :: zFileName
!    logical :: qOptional, qOKL



!    qOK = .false.
!    if (present(zOptionalString)) then
!      if (len(trim(adjustl(zOptionalString))) > 0) then
!        qOptional = .TRUE.
!      end if
!    end if
!     Loop around array segments, creating files
!    do iap = 1, size(tArrayY)
!      if (tArrayY(iap)%qWrite) then
!        if (tProcInfo_G%qRoot) then
!     Prepare filename      
!          zFilename = (trim(adjustl(tArrayY(iap)%zVariable)) // trim(adjustl(zDFName)) // '.h5')
!          if (qOptional) then
!            zFilename = (trim(adjustl(zOptionalString)) // '_' // trim(adjustl(zFilename)) // '.h5')
!          end if
!          call CreateSDDSFile(zFilename, &
!                              tArrayY(iap)%zVariable, &
!                              tArrayY(iap)%tFileType, &
!                              qOKL)    
!        end if
!     end if
!   end do
!     Set error flag and exit
!    qOK = .true.
!    goto 2000
!     Error Handler - Error log Subroutine in CIO.f90 line 709
!1000 call Error_log('Error in sddsPuffin:createFFiles',tErrorLog_G)
!    print*,'Error in sddsPuffin:createFFiles'
!2000 continue
!  end subroutine createH5Files


!> IntegerToString
!! Convert an integer into a string
!! Handy for filename substitution of dump number
!! @param  iInteger    - INPUT  - Integer to convert
FUNCTION IntegerToString(iInteger)

        IMPLICIT NONE
        INTEGER(KIND=IP), INTENT(IN) :: iInteger
        CHARACTER(32_IP) :: IntegerToString
! Define local variables
        CHARACTER(32_IP) :: zCharacter
! Write character to internal file
      write(zCharacter,*) iInteger
! Output without blanks
      IntegerToString = TRIM(ADJUSTL(zCharacter))
!  Set error flag and exit
       GoTo 2000
! Error Handler - Error log Subroutine in CIO.f90 line 709
1000 call Error_log('Error in sddsPuffin:IntegerToString',tErrorLog_G)
      Print*,'Error in sddsPuffin:IntegerToString'
2000 CONTINUE

END FUNCTION IntegerToString
	

end module hdf5PuffLow
