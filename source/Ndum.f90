module dummyf

USE FFTW_Constants
USE transforms
USE sddsPuffin
USE lattice
USE Stiffness
USE Setup
USE RK4int
use dumpFiles 
use hdf5_puff
use pln_puff


implicit none

contains

subroutine diffractIM(sA, sAr, Ar_local, local_rows, sStep,&
                      frecvs, fdispls, lrecvs, ldispls, &
                      qDiffrctd, qOK)


! Subroutine which diffracts the field, after making the preparations
! necessary when Puffin is within an undukator module.
!
! Lawrence Campbell
! University of Strathclyde
! Jan 2015


  implicit none

  real(kind=wp), intent(inout) :: sA(:)
  real(kind=wp), intent(inout), allocatable :: sAr(:), Ar_local(:)
  real(kind=wp), intent(in) :: sStep
  integer(kind=ip), intent(in) :: local_rows
  integer(kind=ip), intent(in) :: frecvs(:), fdispls(:), lrecvs(:), ldispls(:)
  logical, intent(out) :: qDiffrctd, qOK

  logical :: qOKL


  qOK = .false.
  

  
  DEALLOCATE(sAr)
  CALL innerLA2largeA(Ar_local,sA,lrecvs,ldispls,tTransInfo_G%qOneD)
  DEALLOCATE(Ar_local)

  CALL DiffractionStep(sStep,&
       frecvs,&
       fdispls,&
       sA,&
       qOKL)
  if (.not. qOKL) goto 1000

  ALLOCATE(sAr(2*ReducedNX_G*ReducedNY_G*NZ2_G))
  ALLOCATE(Ar_local(2*local_rows))

  CALL getAlocalFL(sA,Ar_local)

  CALL local2globalA(Ar_local,sAr,mrecvs,mdispls,tTransInfo_G%qOneD)

  qDiffrctd = .true.
  
    


!              Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in diffractIM',tErrorLog_G)

2000 CONTINUE


end subroutine diffractIM




!     ###################################################### 





subroutine writeIM(sA, Ar_local, sZ, &
                   zDataFileName, iStep, iCstep, iWriteNthSteps, &
                   lrecvs, ldispls, &
                   iIntWriteNthSteps, nSteps, qOK)


! Subroutine to write data, making the necessary
! preparations before writing due to the layout
! of data when integrating in the undulator module.
!
! Lawrence Campbell
! University of Strathclyde
! Jan 2015

  implicit none

  real(kind=wp), intent(inout) :: sA(:), Ar_local(:), sZ
  integer(kind=ip), intent(in) :: iStep, iWriteNthSteps, iIntWriteNthSteps, nSteps
  integer(kind=ip), intent(in) :: lrecvs(:), ldispls(:), iCstep
  character(32_IP), intent(in) :: zDataFileName
  logical, intent(inout) :: qOK

  logical :: qOKL, qWriteInt, qWriteFull

  qOK = .false.

  call innerLA2largeA(Ar_local,sA,lrecvs,ldispls,tTransInfo_G%qOneD)

  call int_or_full(istep, iIntWriteNthSteps, iWriteNthSteps, &
                   qWriteInt, qWriteFull, qOK)

  if (wrMeth_G == 'sdds') then

    call wr_sdds(sA, sZ, iCstep, tArrayA, tArrayE, tArrayZ, &
                 iIntWriteNthSteps, iWriteNthSteps, qSeparateStepFiles_G, &
                 zDataFileName, qWriteFull, &
                 qWriteInt, qOK)
! temporarilly commenting as we want files written both ways
!  else if (wrMeth_G == 'hdf5') then
    print*,zDataFileName
    call wr_h5(sA, sZ, iCstep, tArrayA, tArrayE, tArrayZ, &
                 iIntWriteNthSteps, iWriteNthSteps, qSeparateStepFiles_G, &
                 zDataFileName, qWriteFull, &
                 qWriteInt, qOK)

  else 

    call wr_pln()

  end if

!              Set error flag and exit

  qOK = .TRUE.

  goto 2000

1000  call Error_log('Error in writeIM',tErrorLog_G)

2000 continue



end subroutine writeIM



!          ##################################################################



  subroutine int_or_full(istep, iIntWr, iWr, &
                         qWriteInt, qWriteFull, qOK)

    implicit none

!   Figure out whether to write integrated data or 
!   full particle dump


    integer(kind=ip), intent(in) :: istep
    integer(kind=ip), intent(in) :: iIntWr, iWr
    logical, intent(inout) :: qWriteInt, qWriteFull, qOK

    logical ::  qOKL

    qOK = .false.
    
    qWriteInt = .false.
    qWriteFull = .false.

    if ((mod(iStep,iIntWr)==0) .or. (iStep == nSteps) .or. (iStep == 0) ) then

      qWriteInt = .true.

    end if


    if ((mod(iStep,iWr)==0) .or. (iStep == nSteps) .or. (iStep == 0) ) then

      qWriteFull = .true.

    end if


  end subroutine int_or_full


!########################################################################


function qWriteq(iStep, iWriteNthSteps, iIntWriteNthSteps, nSteps)



  implicit none

  logical :: qWriteq
  integer(kind=ip) :: iStep, iWriteNthSteps, iIntWriteNthSteps, nSteps


  if ((mod(iStep,iIntWriteNthSteps)==0) .or. (iStep == nSteps) &
               .or. (mod(iStep,iWriteNthSteps)==0)) then

    qWriteq = .true.

  else

    qWriteq = .false.

  end if

end function qWriteq

end module dummyf
