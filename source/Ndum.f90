module dummyf

USE FFTW_Constants
USE pdiff
USE sddsPuffin
USE lattice
USE Setup
USE RK4int
use dumpFiles 
!use hdf5_puff

use pln_puff
use ParaField

implicit none

contains

subroutine diffractIM(sStep, &
                      qDiffrctd, qOK)


! Subroutine which diffracts the field, after making the preparations
! necessary when Puffin is within an undulator module.
!
! Lawrence Campbell
! University of Strathclyde
! Jan 2015


  implicit none

  real(kind=wp), intent(in) :: sStep
  logical, intent(out) :: qDiffrctd, qOK

  logical :: qOKL


  qOK = .false.
  

!      Change data layout to FFTW -

  call redist2FFTWlt()



  CALL DiffractionStep(sStep,&
       tre_fft, tim_fft,&
       qOKL)
  if (.not. qOKL) goto 1000

  qDiffrctd = .true.
  


!    Change back to wiggler data layout

  call redistbackFFT()


!              Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in diffractIM',tErrorLog_G)

2000 CONTINUE


end subroutine diffractIM




!     ###################################################### 





subroutine writeIM(sZ, &
                   zDataFileName, iStep, iCstep, iWriteNthSteps, &
                   iIntWriteNthSteps, nSteps, qOK)


! Subroutine to write data, making the necessary
! preparations before writing due to the layout
! of data when integrating in the undulator module.
!
! Lawrence Campbell
! University of Strathclyde
! Jan 2015

  implicit none

  real(kind=wp), intent(inout) :: sZ
  integer(kind=ip), intent(in) :: iStep, iWriteNthSteps, iIntWriteNthSteps, nSteps
  integer(kind=ip), intent(in) :: iCstep
  character(32_IP), intent(in) :: zDataFileName
  logical, intent(inout) :: qOK

  integer error

  logical :: qOKL, qWriteInt, qWriteFull

  qOK = .false.


  call int_or_full(istep, iIntWriteNthSteps, iWriteNthSteps, &
                   qWriteInt, qWriteFull, qOK)

  if (qsdds_G) then

    call wr_sdds(sZ, iCstep, tArrayA, tArrayE, tArrayZ, &
                 iIntWriteNthSteps, iWriteNthSteps, qSeparateStepFiles_G, &
                 zDataFileName, qWriteFull, &
                 qWriteInt, qOK)

  end if

  if (qhdf5_G) then

!    call wr_h5(sA, sZ, tArrayA, tArrayE, tArrayZ, &
!                 iIntWriteNthSteps, iWriteNthSteps, qSeparateStepFiles_G, &
!                 zDataFileName, qWriteFull, &
!                 qWriteInt, qOK)

  end if

!  else 

!    call wr_pln()

!  end if

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
