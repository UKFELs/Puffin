module dummyf

USE FFTW_Constants
USE transforms
USE sddsPuffin
USE lattice
USE Stiffness
USE Setup
USE RK4int
use dumpFiles 

implicit none

contains

subroutine diffractIM(sA, sAr, Ar_local, local_rows, sV, sStep,&
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
  real(kind=wp), intent(in) :: sV(:)
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
       sV,&
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










subroutine writeIM(sA, Ar_local, sV, sZ, &
                   zDataFileName, iStep, iWriteNthSteps, &
                   lrecvs, ldispls, &
                   iIntWriteNthSteps, nSteps, qWDisp, qOK)


! Subroutine to write data, making the necessary
! preparations before writing due to the layout
! of data when integrating in the undulator module.
!
! Lawrence Campbell
! University of Strathclyde
! Jan 2015

  implicit none

  real(kind=wp), intent(inout) :: sA(:), Ar_local(:), sV(:), sZ
  integer(kind=ip), intent(in) :: iStep, iWriteNthSteps, iIntWriteNthSteps, nSteps
  integer(kind=ip), intent(in) :: lrecvs(:), ldispls(:)
  character(32_IP), intent(in) :: zDataFileName
  logical, intent(inout) :: qWDisp
  logical, intent(inout) :: qOK

  logical :: qOKL

  qOK = .false.

  call innerLA2largeA(Ar_local,sA,lrecvs,ldispls,tTransInfo_G%qOneD)

  call wdfs(sA, sV, sZ, istep, tArrayA, tArrayE, tArrayZ, &
            iIntWriteNthSteps, iWriteNthSteps, qSeparateStepFiles_G, &
            zDataFileName, qWDisp, qOKL)

!              Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in writeIM',tErrorLog_G)

2000 CONTINUE



end subroutine writeIM

















function qWriteq(iStep, iWriteNthSteps, iIntWriteNthSteps, nSteps, &
                 qWDisp)



  implicit none

  logical :: qWriteq
  integer(kind=ip) :: iStep, iWriteNthSteps, iIntWriteNthSteps, nSteps
  logical :: qWDisp


  if ((mod(iStep,iIntWriteNthSteps)==0) .or. (iStep == nSteps) .or. &
               (qWDisp)   .or. (mod(iStep,iWriteNthSteps)==0)) then

    qWriteq = .true.

  else

    qWriteq = .false.

  end if

end function qWriteq

end module dummyf