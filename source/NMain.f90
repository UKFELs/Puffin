! ###############################################
! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause
! ###############################################

!!!!!!!!!!!!!!!!!!! Puffin Version 1.9.0 !!!!!!!!!!!!!!!!!!!

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Main top-level Puffin program. Calls setup routines, then loops around
!> undulator lattice elements, propagating the field and electron beam through
!> each.
!> @param sZ Propagation distance in z through the undulator.
!> @param qOKL Error flag

program main

use transforms
!use sddsPuffin
use lattice
use Setup
use undulator
use initDataType
use Globals

implicit none

real(kind=wp)    :: sZ, szl
integer(kind=ip) :: iL, iLst

logical          :: qOKL

!           Read in data file and initialize system

call init(sZ,qOKL)



call Get_time(start_time)







if ((tProcInfo_G%qRoot) .and. (ioutInfo_G>0)) print*,' starting simulation... '

if (tProcInfo_G%qRoot) OPEN(UNIT=137,FILE='rec.out',STATUS='REPLACE',FORM='FORMATTED')
if (tProcInfo_G%qRoot) WRITE(137,*) ' starting..... '

!!!!!!!!!!!!!!!!!!!!!!!  BEGIN INTEGRATION !!!!!!!!!!!!!!!!!!!!!!!!


iLSt = 1_ip
if (qresume_G) iLst = tInitData_G%iL



do iL = iLst, modNum




  if (iElmType(iL) == iUnd) then

    if ((tProcInfo_G%qRoot) .and. (ioutInfo_G > 0)) then
      print*, 'Simulating undulator module', iUnd_cr
    end if

    call UndSection(iL, sZ)

  else if (iElmType(iL) == iQuad) then

    call Quad(iL)

  else if (iElmType(iL) == iChic) then

    call disperse(iL, sZ)

  else if (iElmType(iL) == iDrift) then

    call driftSection(iL, sZ)
!     FOR WRITING AFTER EACH DRIFT
!    szl = 0.0_wp
!    call wr_cho(sZ, szl, &
!                0_ip, iCsteps, modNum, iWriteNthSteps, &
!                iIntWriteNthSteps, 0_ip, .true., .true., qOKL)

  else if (iElmType(iL) == iModulation) then

    call BModulation(iL)

  else if (iElmType(iL) == iRotation) then

    call bRotation(iL)

  end if



end do




if (qDumpEnd_G) then

  szl = 0.0_wp
  call wr_cho(sZ, szl, &
              0_ip, iCsteps, modNum, iWriteNthSteps, &
              iIntWriteNthSteps, 0_ip, .true., .true., qOKL)

end if



call cleanup(sZ)   !     Clear arrays and stucts used during integration


CLOSE(UNIT=137,STATUS='KEEP')





goto 2000     !       Exit


1000 call Error_log('Error in Main',tErrorLog_G)
print*,'Error in Main'
print*, 'Check error log file for details, ',tErrorLog_G%zFileName
call UnDefineParallelLibrary(qOKL)

2000 continue

if (tProcInfo_G%qRoot) print*,'Exited successfully'

end program main
