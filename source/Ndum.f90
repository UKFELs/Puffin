module dummyf

!USE FFTW_Constants

USE sddsPuffin
USE lattice
USE RK4int
use dumpFiles
use hdf5_puff
use pln_puff
use ParaField

implicit none

contains


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
  integer(kind=ip) :: nslices
  character(32_IP), intent(in) :: zDataFileName
  logical, intent(inout) :: qOK

  integer error

  logical :: qOKL, qWriteInt, qWriteFull

  qOK = .false.


  call int_or_full(istep, iCsteps, iIntWriteNthSteps, iWriteNthSteps, &
                   qWriteInt, qWriteFull, qOK)

  if (qsdds_G) then

    call wr_sdds(sZ, iCstep, tArrayA, tArrayE, tArrayZ, &
                 iIntWriteNthSteps, iWriteNthSteps, qSeparateStepFiles_G, &
                 zDataFileName, qWriteFull, &
                 qWriteInt, qOK)

  end if

  if (qhdf5_G) then
!     nslices=ceiling( (sLengthOfElmZ2_G*NZ2_G)/(4*pi*srho_g))

    call wr_h5(sZ, tArrayA, tArrayE, tArrayZ, &
                 iIntWriteNthSteps, iWriteNthSteps, qSeparateStepFiles_G, &
                 zDataFileName, qWriteFull, &
                 qWriteInt, nslices, qOK)

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



  subroutine int_or_full(istep, iCsteps, iIntWr, iWr, &
                         qWriteInt, qWriteFull, qOK)

    implicit none

!   Figure out whether to write integrated data or
!   full particle dump


    integer(kind=ip), intent(in) :: istep, iCsteps
    integer(kind=ip), intent(in) :: iIntWr, iWr
    logical, intent(inout) :: qWriteInt, qWriteFull, qOK

    logical ::  qOKL

    qOK = .false.

    qWriteInt = .false.
    qWriteFull = .false.


    if (qInitWrLat_G) then

      if ((mod(iStep,iIntWr)==0) .or. (iStep == nSteps) .or. (iStep == 0) ) then

        qWriteInt = .true.

      end if


      if ((mod(iStep,iWr)==0) .or. (iStep == nSteps) .or. (iStep == 0) ) then

        qWriteFull = .true.

      end if

    else

      if ((mod(iCsteps,iIntWr)==0) .or. (iCsteps == nSteps) .or. (iCsteps == 0) ) then

        qWriteInt = .true.

      end if


      if ((mod(iCsteps,iWr)==0) .or. (iCsteps == nSteps) .or. (iCsteps == 0) ) then

        qWriteFull = .true.

      end if

    end if



  end subroutine int_or_full


!########################################################################


function qWriteq(iStep, iCsteps, iWriteNthSteps, iIntWriteNthSteps, nSteps)



  implicit none

  logical :: qWriteq
  integer(kind=ip) :: iStep, iCsteps, iWriteNthSteps, iIntWriteNthSteps, nSteps


  if (qInitWrLat_G) then

    if ((mod(iStep,iIntWriteNthSteps)==0) .or. (iStep == nSteps) &
                 .or. (mod(iStep,iWriteNthSteps)==0)) then

      qWriteq = .true.

    else

      qWriteq = .false.

    end if

  else

    if ((mod(iCsteps,iIntWriteNthSteps)==0) .or. (iCsteps == nSteps) &
                 .or. (mod(iCsteps,iWriteNthSteps)==0)) then

      qWriteq = .true.

    else

      qWriteq = .false.

    end if

  end if


end function qWriteq

end module dummyf
