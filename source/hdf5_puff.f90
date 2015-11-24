module hdf5_puff

USE ArrayFunctions
USE TypesandConstants
USE Globals
USE ParallelSetUp
Use avWrite
use paratype
use HDF5

implicit none 

contains


  SUBROUTINE  WriteAttributeData(zDataFileName, &
       iNodes,&
       iNumElectrons, &
       sLengthOfElm, &
       sStepSize, &
       nSteps, &
       sLenEPulse, &
       sWigglerLength, &
       sSigmaGaussian, &
       sA0_Re, &
       sA0_Im, &
       rho,aw,epsilon,gamma_r, &
       kbeta, ff, &
       lam_w, lam_r, &
       l_g, l_c, &
       npk_bar, &
       totalNumberElectrons, &
       nWaveEquations, &
       nElectronEquations, &  
       sZ, &
       iWriteNthSteps, &
       iIntWriteNthSteps, &
       sSeedSigma, &
       qSwitch, &
       fx, &
       fy, &
       qOK)

! Write input data used to create results
!
! zDataFileName      - INPUT  - Data file name
! iNodes             - INPUT  - Number of Nodes
! iNumElectrons      - INPUT  - number of electrons
! sLengthOfElm       - INPUT  - Element length
! sStepSize          - INPUT  - Integration step size
! nSteps             - INPUT  - Number of steps 
! sLenEPulse 	     - INPUT  - L-electron pulse
! sWigglerLength     - INPUT  - Wiggler length
! sSigmaGaussian     - INPUT  - e-pulse sigma
! sA0_Re,            - INPUT  - Initial field value (real)
! sA0_Im,            - INPUT  - Initial field value (imag)
! iTotalNumElectrons - INPUT  - Acutal Number of electrons used
! nWaveEquations     - INPUT  - Number of Wave Equations
! nElectronEquations - INPUT  - Number of Electron Equations
! sZ                 - UPDATE - IN: Starting z position
! iWriteNthSteps     - UPDATE - Steps to write data at
! sSeedSigma         - INPUT  - Sigma of initial seed field
! qSwitch            - UPDATE - Optional if letting electrons
!                               evolve, field evolve,
!                               diffraction,
!                               gauss inital field
! sx0_offset         - INPUT  - Electron offset value
! sy0_offset         - INPUT  - Electron offset value
! qOK                - OUTPUT - Error flag
!

    IMPLICIT NONE


!  LIst of variables to write as attributes available at FssdsPuffin.f90 lines 250 - 375	
!
    CHARACTER(32_IP), INTENT(IN) :: zDataFileName
    INTEGER(KIND=IP), INTENT(IN) :: iNodes(:)
    INTEGER(KIND=IP), INTENT(IN) :: iNumElectrons(:)
    REAL(KIND=WP),    INTENT(IN) :: sLengthOfElm(:)
    REAL(KIND=WP),    INTENT(IN) :: sStepSize
    INTEGER(KIND=IP), INTENT(IN) :: nSteps
    REAL(KIND=WP),    INTENT(IN) :: sLenEPulse(:)   
    REAL(KIND=WP),    INTENT(IN) :: sWigglerLength(:) 
    REAL(KIND=WP),    INTENT(IN) :: sSigmaGaussian(:)
    REAL(KIND=WP),    INTENT(IN) :: sA0_Re   
    REAL(KIND=WP),    INTENT(IN) :: sA0_Im   
    REAL(KIND=WP),    INTENT(IN) :: rho,aw,epsilon,gamma_r
    REAL(KIND=WP),    INTENT(IN) :: kbeta, ff
    real(kind=wp),    intent(in) :: lam_w, lam_r, l_g, l_c
    real(kind=wp),    intent(in) :: npk_bar
    INTEGER(KIND=IPL), INTENT(IN) :: totalNumberElectrons
    INTEGER(KIND=IP), INTENT(IN) :: nWaveEquations    
    INTEGER(KIND=IP), INTENT(IN) :: nElectronEquations
    REAL(KIND=WP),    INTENT(IN) :: sZ
    INTEGER(KIND=IP), INTENT(IN) :: iWriteNthSteps, iIntWriteNthSteps
    REAL(KIND=WP),    INTENT(IN) :: sSeedSigma(:)
    LOGICAL,          INTENT(IN) :: qSwitch(:)
    REAL(KIND=WP),    INTENT(IN) :: fx,fy
  
    LOGICAL,          INTENT(OUT) :: qOK      
!
! Define local variables
! 
! tParamFile   - Write Parameter data to file
! qOKL         - Local error flag
!	
    TYPE(cFileType) :: tParamFile
    LOGICAL         :: qOKL
!********************************************************
! BEGIN:-
! Set error flag to false         
    qOK = .FALSE.    

    If (tProcInfo_G%qROOT) Then

! Open the file to receive data output -
! This subroutine is in IO.f90 line 793
       tParamFile%qFormatted = .TRUE.
!       call InitBasicSDDSFile('Param' // TRIM(zDataFileName),  or some other init for HDF5
!       If (.NOT. qOKL) Goto 1000
    End If 

!  Set error flag and exit         
    qOK = .TRUE.				    
    GoTo 2000     

! Error Handler - Error log Subroutine in CIO.f90 line 709
1000 call Error_log('Error in hdf5_puff:WriteAttributeData',&
          tErrorLog_G)
    Print*,'Error in hdf5_puff:WriteAttributeData'
2000 CONTINUE
  END SUBROUTINE WriteAttributeData


	subroutine wr_h5(sA, sZ, istep, tArrayA, tArrayE, tArrayZ, &
                     iIntWr, iWr, qSep, zDFname, qWriteFull, &
                     qWriteInt, qOK)

    implicit none

! Write Data FileS


    real(kind=wp), intent(in) :: sA(:), sZ
    type(cArraySegment), intent(inout) :: tArrayA(:), tArrayE(:), tArrayZ
    integer(kind=ip), intent(in) :: istep
    integer(kind=ip), intent(in) :: iIntWr, iWr
    character(32_IP), intent(in) :: zDFName
    logical, intent(in) :: qSep
    logical, intent(inout) :: qOK
    integer :: error
    logical :: qWriteInt, qWriteFull
    error = 0
    if (qWriteFull) then

      call outputH5BeamFiles(iStep, error)
      if (error .ne. 0) goto 1000

!      call outputH5Field(sA, tArrayA, iStep, qSep, zDFName, qOKL)
!      if (.not. qOKL) goto 1000

!      call outputH5Z(sZ, tArrayZ, iStep, qSep, zDFName, qOKL)
!      if (.not. qOKL) goto 1000

    end if

!    if (qWriteInt) then
!
!      call writeIntData(sA)
!    
!    end if

!  Set error flag and exit         
    error = 0            
    goto 2000     

! Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call Error_log('Error in hdfPuffin:wr_h5',&
          tErrorLog_G)
    print*,'Error in hdfPuffin:wr_h5'
2000 continue

    end subroutine wr_h5

  subroutine outputH5BeamFiles(iStep, error)


    implicit none

! Output the electron bean macroparticle 
! 6D phase space coordinates in Puffin.
! 
! tArrayE   -      Array describing the 
!                  layout of data in 
!                  sV
!
    INTEGER(HID_T) :: file_id       ! File identifier
    INTEGER(HID_T) :: dset_id       ! Dataset identifier 
    INTEGER(HID_T) :: dspace_id     ! Dataspace identifier in memory
    INTEGER(HID_T) :: filespace     ! Dataspace identifier in file
!    type(cArraySegment), intent(inout) :: tArrayE(:)
    integer(kind=ip), intent(in) :: iStep
!    logical, intent(in) :: qSeparate
    CHARACTER(LEN=9), PARAMETER :: dsetname = "electrons"     ! Dataset name
!    character(32_IP), intent(in) :: zDFName
    character(32_IP) :: filename
!    logical, intent(inout) :: qOK
!    INTEGER(HSIZE_T), DIMENSION(1) :: dims = (/iGloNumElectrons_G/) ! Dataset dimensions
!    INTEGER     ::   rank = 1                        ! Dataset rank
    INTEGER(HSIZE_T), DIMENSION(2) :: dims 
    INTEGER(HSIZE_T), DIMENSION(2) :: doffset! Offset for write
    INTEGER(HSIZE_T), DIMENSION(2) :: dsize ! Size of hyperslab to write
    INTEGER     ::   rank = 2                        ! Dataset rank
! Local vars

    integer(kind=ip) :: iep
    integer :: error ! Error flag
    dims = (/iNumberElectrons_G,6/) ! Dataset dimensions
    doffset=(/0,0/)
    dsize=(/iNumberElectrons_G,1/)
!     Create data files

!      call createH5Files(tArrayE, zDFName, trim(IntegerToString(iStep)), qOKL)
!      if (.not. qOKL) goto 1000
!       do iep = 1, size(tArrayE)

!            if (tArrayE(iep)%qWrite) then
        
!      if (tProcInfo_G%qRoot) then

!     Prepare filename      


    filename = (trim(adjustl(IntegerToString(iStep))) // '_' // &
		 trim(adjustl(IntegerToString(tProcInfo_G%Rank))) // &
		 '_particles.h5' )

    CALL h5open_f(error)
    Print*,'hdf5_puff:outputH5BeamFiles(file opened)'
!
! Create a new file using default properties.
!
    CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
    Print*,'hdf5_puff:outputH5BeamFiles(file created)'
!
! Create the big dataspace in the file.
!
    CALL h5screate_simple_f(rank, dims, filespace, error)
    Print*,'hdf5_puff:outputH5BeamFiles(filespace created)'

!
! Create the dataset with default properties.
!
    CALL h5dcreate_f(file_id, dsetname, H5T_NATIVE_DOUBLE, filespace, &
       dset_id, error)
    Print*,'hdf5_puff:outputH5BeamFiles(dataset created)'
    CALL h5sclose_f(filespace, error)
    Print*,'hdf5_puff:outputH5BeamFiles(filespace closed)'

!
! Create a smaller space to buffer the data writes
!
    CALL h5screate_simple_f(rank, dsize, dspace_id, error)
    Print*,'hdf5_puff:outputH5BeamFiles(memory dataspace allocated)'

! 
! Select hyperslab in the file.
!
    CALL H5Dget_space_f(dset_id, filespace, error)
    CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, doffset, &
       dsize, error)   
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, sElX_G, dims, error, &
       file_space_id = filespace, mem_space_id = dspace_id)
!
! End access to the dataset and release resources used by it.
!
    CALL h5sclose_f(filespace, error) 
!    CALL h5sclose_f(dspace_id, error) 
  
! repeat for some next y dataset
    doffset=(/0,1/)

    CALL H5Dget_space_f(dset_id, filespace, error)
    CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, doffset, &
       dsize, error)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, sElY_G, dims, error, &
       file_space_id = filespace, mem_space_id = dspace_id)
! was       dspace_id, filespace)
    CALL h5sclose_f(filespace, error) 

!
! repeat for some next z dataset
    doffset=(/0,2/)

    CALL H5Dget_space_f(dset_id, filespace, error)
    CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, doffset, &
       dsize, error)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, sElZ2_G, dims, error, &
       file_space_id = filespace, mem_space_id = dspace_id)
! was       dspace_id, filespace)
    CALL h5sclose_f(filespace, error) 


! repeat for some next px dataset
    doffset=(/0,3/)

    CALL H5Dget_space_f(dset_id, filespace, error)
    CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, doffset, &
       dsize, error)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, sElPX_G, dims, error, &
       file_space_id = filespace, mem_space_id = dspace_id)
! was       dspace_id, filespace)
    CALL h5sclose_f(filespace, error) 


! repeat for some next py dataset
    doffset=(/0,4/)

    CALL H5Dget_space_f(dset_id, filespace, error)
    CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, doffset, &
       dsize, error)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, sElPY_G, dims, error, &
       file_space_id = filespace, mem_space_id = dspace_id)
! was       dspace_id, filespace)
    CALL h5sclose_f(filespace, error) 


! repeat for some next gamma dataset (actually beta*gamma)
    doffset=(/0,5/)

    CALL H5Dget_space_f(dset_id, filespace, error)
    CALL h5sselect_hyperslab_f(filespace, H5S_SELECT_SET_F, doffset, &
       dsize, error)
    CALL h5dwrite_f(dset_id, H5T_NATIVE_DOUBLE, sElgam_G, dims, error, &
       file_space_id = filespace, mem_space_id = dspace_id)
! was       dspace_id, filespace)
    CALL h5sclose_f(filespace, error) 
!
! 
!  
    CALL h5dclose_f(dset_id, error)

!
! Terminate access to the data space.
!
    CALL h5sclose_f(dspace_id, error)

!
! Close the file.
!
    CALL h5fclose_f(file_id, error)


!      end if
!		    end if
!      end do



!     Write full 6D electron phase space
!     to file. This will create very large
!     files!!!

!    call wrt_phs_coord(iRe_X_CG, sElX_G, qOKL)
!    call wrt_phs_coord(iRe_Y_CG, sElY_G, qOKL)
!    call wrt_phs_coord(iRe_Z2_CG, sElZ2_G, qOKL)
!    call wrt_phs_coord(iRe_PPerp_CG, sElPX_G, qOKL)
!    call wrt_phs_coord(iIm_PPerp_CG, sElPY_G, qOKL)
!    call wrt_phs_coord(iRe_Gam_CG, sElGam_G, qOKL)
!    if (.not. qOKL) goto 1000
!     size(iRe_X_CG)
!     Set error flag and exit

!    qOK = .true.            
    goto 2000


!     Error Handler - Error log Subroutine in CIO.f90 line 709

1000 call Error_log('Error in hdf5_puff:outputBeamFiles',tErrorLog_G)
    print*,'Error in hdf5_puff:outputBeamFiles'

2000 continue


  end subroutine outputH5BeamFiles

!  subroutine createH5Files(tArrayY, zDFName, zOptionalString, qOK)
!
!    implicit none

! Create "Full" Files - creates either 
! the full data sets for the field and 
! electron phase space.

!    type(cArraySegment), intent(inout) :: tArrayY(:)
!   character(32_IP), intent(in)   ::   zDFName
!    character(*), intent(in), optional  :: zOptionalString
!    logical, intent(inout) :: qOK

!    integer(kind=ip) :: iap
!    character(32_IP) :: zFileName
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


FUNCTION IntegerToString(iInteger)

! Convert an integer into a string

! iInteger    - INPUT  - Integer to convert

! Define variables

        IMPLICIT NONE

        INTEGER(KIND=IP),          INTENT(IN)                   :: iInteger
        CHARACTER(32_IP)                                        :: IntegerToString

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
	
	
end module hdf5_puff

