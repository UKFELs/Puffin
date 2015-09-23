!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

program main

use FFTW_Constants
use transforms
use sddsPuffin
use lattice
use Stiffness
use Setup
use RK4int
use dumpFiles
use dummyf

!!!!!!!!!!!!!!!!!!! Puffin Version 1.4.4 !!!!!!!!!!!!!!!!!!!
!
! A program for solving an unaveraged 3D FEL system. This 
! parallel MPI code requires the MPI transforms in FFTW v2.5.1.
! The system of equations and numerical solution is presented 
! in:
!
! LT Campbell and BWJ McNeil, Physics of Plasmas 19, 093119 (2012)
!
! Written by Lawrence Campbell, Cynthia Nam, and Dr. Pamela Johnston.
! University of Strathclyde, Glasgow
!
! Contact: lawrence.campbell@strath.ac.uk
!
!                       ARGUMENTS 
!
!
!   sA                    Array containing the values of the
!                         real and imaginary parts of the 
!                         scaled radiation field at the 
!                         radiation field nodes. The radiation
!                         field nodes are arranged in a 3D grid
!                         in x, y and z2. The field values at 
!                         each node are assigned to this 1D
!                         array in order of x, y and z2 (see
!                         documentation). For Nn nodes, sA(1:Nn)
!                         contains the real radiation field 
!                         values and sA(Nn+1:2*Nn) contains
!                         the imaginary field values.
!
!   sZ                    Propagation distance in z through 
!                         the undulator.
!
!   qOKL                  Error flag.

implicit none

real(kind=wp), allocatable  :: sA(:), sAr(:), Ar_local(:)
real(kind=wp)    :: sZ, nextDiff

logical          :: qOKL, qDiffrctd, qWDisp

!           Read in data file and initialize system

call init(sA,sZ,qOKL)

allocate(sAr(2*ReducedNX_G*ReducedNY_G*NZ2_G))
allocate(Ar_local(2*local_rows))

!!!! TEMP - NEEDS TIDIED, SHOULD OPTIMIZE

  if (tTransInfo_G%qOneD) then
     Ar_local(1:local_rows)=sA(fst_row:lst_row)
     Ar_local(local_rows+1:2*local_rows)=&
          sA(fst_row+iNumberNodes_G:lst_row+iNumberNodes_G)
  else
     call getAlocalFL(sA,Ar_local)
  end if

call local2globalA(Ar_local,sAr,mrecvs,mdispls,tTransInfo_G%qOneD)

qDiffrctd = .false.
qWDisp = .false.

if (start_step==1_IP) then

  iCount = 0_IP

else

  iCount = mod(start_step-1_IP,iWriteNthSteps)

end if




if (start_step == 1) then
  sStep = diffStep*0.5_WP ! Integration step size for first diffraction step
  nextDiff = 0.0_WP
else 
  sStep = diffStep
  nextDiff = ceiling(sZ/diffStep) * diffStep
end if



call Get_time(start_time)







if (tProcInfo_G%qRoot) print*,' starting..... '

if (tProcInfo_G%qRoot) OPEN(UNIT=137,FILE='rec.out',STATUS='REPLACE',FORM='FORMATTED')
if (tProcInfo_G%qRoot) WRITE(137,*) ' starting..... '

!!!!!!!!!!!!!!!!!!!!!!!  BEGIN INTEGRATION !!!!!!!!!!!!!!!!!!!!!!!!

do iStep = start_step, nSteps
  





!   First step of split-step method:- field diffraction only

  if (qDiffraction_G) then

    if (iStep==0) then

      call diffractIM(sA, sAr, Ar_local, local_rows, &
                      diffStep*0.5_WP, frecvs, fdispls, lrecvs, ldispls, &
                      qDiffrctd, qOKL)

      nextDiff = nextDiff + diffStep

    end if

  end if






!   Second half of split step method: electron propagation
!                    and field driving.

  if (qElectronsEvolve_G .OR. qFieldEvolve_G &
       .OR. qElectronFieldCoupling_G) then

     call rk4par(sAr,Ar_local,sZ,sStepSize,mrecvs,mdispls,qDiffrctd)

  end if 






!                  Increment z position  
!       (we now have solution at zbar + sStepsize) 

  sZ = sZ + sStepSize











!   diffract field to complete diffraction step

  if (qDiffraction_G) then

    if ((sZ>(nextDiff-sStepsize/100.0_WP)) .or. (iStep == nSteps))  then

      if ((iStep == nSteps) .or.  qWriteq(iStep, iWriteNthSteps, iIntWriteNthSteps, nSteps, &
                   qWDisp) ) then
  
        call diffractIM(sA, sAr, Ar_local, local_rows, &
                        diffStep * 0.5_wp, frecvs, fdispls, lrecvs, ldispls, &
                        qDiffrctd, qOKL)

      else
  
        call diffractIM(sA, sAr, Ar_local, local_rows, &
                        diffStep, frecvs, fdispls, lrecvs, ldispls, &
                        qDiffrctd, qOKL)
  
      end if

      nextDiff = nextDiff + diffStep
  
    end if

  end if














!                 If at end of current undulator module, 
!      propagate electron beam through a dispersive chicane, if present,
!                 and move to the next undulator module.

  if (qMod_G) then
     if (sZ>(zMod(modCount)-sStepsize/100.0_WP)) then

        if (modCount /= ModNum) then

          call disperse(D(modCount),delta(modCount),&
                   modCount,sStepSize,sZ)

        end if



        qWDisp = .true.
        modCount=modCount+1_IP   !      Update module count
     
     end if
  
  end if





!                   Write result to file
 
  iCount = iCount + 1_IP
  





if ( qWriteq(iStep, iWriteNthSteps, iIntWriteNthSteps, nSteps, &
             qWDisp) ) then

  call writeIM(sA, Ar_local, sZ, &
               zDataFileName, iStep, iWriteNthSteps, &
               lrecvs, ldispls, &
               iIntWriteNthSteps, nSteps, qWDisp, qOKL)


  if (qDiffraction_G) then

!             If field diffraction occurred this step, need to complete it....  
!             ...the diffraction only diffracts a half step if data is going
!             to be written (to match up the split-step data)

     if (qDiffrctd) call diffractIM(sA, sAr, Ar_local, local_rows, &
                      diffStep * 0.5_wp, frecvs, fdispls, lrecvs, ldispls, &
                      qDiffrctd, qOKL)

  end if

    if (qWDisp) qWDisp = .false.

  end if


  
  call Get_time(end_time)
  
  if (tProcInfo_G%QROOT ) then
     print*,' finished step ',iStep, end_time-start_time
     WRITE(137,*) ' finished step ',iStep, end_time-start_time
  end if
  







!                Dump data when time comes

  if (mod(iStep,iDumpNthSteps)==0) then
     if (tProcInfo_G%qRoot) PRINT*, 'Dumping data in case of crash'
     
     call innerLA2largeA(Ar_local,sA,lrecvs,ldispls,tTransInfo_G%qOneD)
     
     if (qDump_G) call DUMPDATA(sA,tProcInfo_G%rank,NX_G*NY_G*NZ2_G,&
          iNumberElectrons_G,sZ,istep,tArrayA(1)%tFileType%iPage)
  end if






  if (modCount > ModNum) EXIT


end do   ! End of integration loop







call cleanup(sA, sZ)   !     Clear arrays and stucts used during integration


CLOSE(UNIT=137,STATUS='KEEP') 





goto 2000     !       Exit

            
1000 call Error_log('Error in Main',tErrorLog_G)
print*,'Error in Main'
print*, 'Check error log file for details, ',tErrorLog_G%zFileName
call UnDefineParallelLibrary(qOKL)

2000 continue

if (tProcInfo_G%qRoot) print*,'Exited successfully'

end program main


