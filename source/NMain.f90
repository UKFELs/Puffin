!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

PROGRAM main

USE FFTW_Constants
USE transforms
USE DataWrite
USE lattice
USE Stiffness
USE Setup
USE RK4int

!!!!!!!!!!!!!!!!!!!Puffin Version 1.4.0 !!!!!!!!!!!!!!!!!!!
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
!   sV                    Contains electron macroparticle 
!                         phase space coordinates. If there
!                         are Nm Macroparticles, then this
!                         array is of length 6*Nm. The first
!                         n=1:Nm values describe the nth 
!                         macroparticle's coordinate in the
!                         x dimension, and the n=Nm+1:2*Nm
!                         values contain the coordinates in
!                         the y dimension and so on, in order
!                         of x, y, z2, px, py, and p2.
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

IMPLICIT NONE

REAL(KIND=WP), ALLOCATABLE  :: sV(:)
REAL(KIND=WP), ALLOCATABLE  :: sA(:), sAr(:), Ar_local(:)
REAL(KIND=WP)    :: sZ, nextDiff

LOGICAL          :: qOKL, qDiffrctd

!           Read in data file and initialize system

CALL init(sA,sV,sZ,qOKL)

ALLOCATE(sAr(2*ReducedNX_G*ReducedNY_G*NZ2_G))
ALLOCATE(Ar_local(2*local_rows))

!!!! TEMP - NEEDS TIDIED, SHOULD OPTIMIZE

  IF (tTransInfo_G%qOneD) THEN
     Ar_local(1:local_rows)=sA(fst_row:lst_row)
     Ar_local(local_rows+1:2*local_rows)=&
          sA(fst_row+iNumberNodes_G:lst_row+iNumberNodes_G)
  ELSE
     CALL getAlocalFL(sA,Ar_local)
  END IF

CALL local2globalA(Ar_local,sAr,mrecvs,mdispls,tTransInfo_G%qOneD)

qDiffrctd = .false.

iCount = 0_IP

sStep = diffStep*0.5_WP ! Integration step size for first diffraction step
nextDiff = 0.0_WP

CALL Get_time(start_time)







IF (tProcInfo_G%qRoot) print*,' starting..... '

IF (tProcInfo_G%qRoot) OPEN(UNIT=137,FILE='rec.out',STATUS='REPLACE',FORM='FORMATTED')
IF (tProcInfo_G%qRoot) WRITE(137,*) ' starting..... '

!!!!!!!!!!!!!!!!!!!!!!!  BEGIN INTEGRATION !!!!!!!!!!!!!!!!!!!!!!!!

DO iStep = start_step, nSteps
  





!   First step of split-step method:- field diffraction only

  IF (qDiffraction_G) THEN

    IF (sZ>(nextDiff-sStepsize/100.0_WP)) THEN
    

      IF(iStep == nSteps) sStep = diffStep*0.5_WP

      DEALLOCATE(sAr)
      CALL innerLA2largeA(Ar_local,sA,lrecvs,ldispls,tTransInfo_G%qOneD)
      DEALLOCATE(Ar_local)

      CALL DiffractionStep(sStep,&
           frecvs,&
           fdispls,&
           sV,&
           sA,&
           qOKL)
     
      ALLOCATE(sAr(2*ReducedNX_G*ReducedNY_G*NZ2_G))
      ALLOCATE(Ar_local(2*local_rows))

      CALL getAlocalFL(sA,Ar_local)

      CALL local2globalA(Ar_local,sAr,mrecvs,mdispls,tTransInfo_G%qOneD)

      IF(iStep==start_step) sStep = diffStep
      
      qDiffrctd = .true.

      nextDiff = nextDiff + diffStep

    END IF

  END IF









!   Second half of split step method: electron propagation
!                    and field driving.

  IF (qElectronsEvolve_G .OR. qFieldEvolve_G &
       .OR. qElectronFieldCoupling_G) THEN

     CALL rk4par(sV,sAr,Ar_local,sZ,sStepSize,mrecvs,mdispls,qDiffrctd)

  END IF 







!                  Increment z position  
!       (we now have solution at zbar + sStepsize) 

  sZ = sZ + sStepSize






!                 If at end of current undulator module, 
!      propagate electron beam through a dispersive chicane, if present,
!                 and move to the next undulator module.

  IF (qMod_G) THEN
     IF (sZ>(zMod(modCount)-sStepsize/100.0_WP)) THEN

        IF (modCount /= ModNum) THEN

          CALL disperse(sV,D(modCount),delta(modCount),&
                   modCount,sStepSize,sZ)

        END IF




        !            Write data if not already going to

        IF ((iCount /= iWriteNthSteps).AND.&
             (iStep /= nSteps)) THEN
             
           CALL innerLA2largeA(Ar_local,sA,lrecvs,ldispls,tTransInfo_G%qOneD)
             
           CALL WriteData(qSeparateStepFiles_G,&
                zDataFileName,tArrayZ,tArrayA,&
                tArrayE,&
                iStep,sZ,sA,sV,.FALSE.,qFormattedFiles_G,&
                qOKL)	

        END IF
  


        modCount=modCount+1_IP   !      Update module count
     
     END IF
  
  END IF





!                   Write result to file
 
  iCount = iCount + 1_IP
  IF ((iCount == iWriteNthSteps).OR.&
       (iStep == nSteps)) THEN
     iCount = 0_IP
     
     CALL innerLA2largeA(Ar_local,sA,lrecvs,ldispls,tTransInfo_G%qOneD)
     
     CALL WriteData(qSeparateStepFiles_G,&
          zDataFileName,tArrayZ,tArrayA,tArrayE,&
          iStep,sZ,sA,sV,.FALSE.,qFormattedFiles_G,&
          qOKL)
  END IF
  
  CALL Get_time(end_time)
  
  IF (tProcInfo_G%QROOT ) THEN
     print*,' finished step ',iStep, end_time-start_time
     WRITE(137,*) ' finished step ',iStep, end_time-start_time
  END IF
  







!                Dump data when time comes

  IF (mod(iStep,iDumpNthSteps)==0) THEN
     IF (tProcInfo_G%qRoot) PRINT*, 'Dumping data in case of crash'
     
     CALL innerLA2largeA(Ar_local,sA,lrecvs,ldispls,tTransInfo_G%qOneD)
     
     if (qDump_G) CALL DUMPDATA(sA,sV,tProcInfo_G%rank,NX_G*NY_G*NZ2_G,&
          iNumberElectrons_G,sZ,istep,tArrayA(1)%tFileType%iPage)
  END IF






  IF (modCount > ModNum) EXIT


END DO   ! End of integration loop







CALL cleanup(sA,sV,sZ)   !     Clear arrays and stucts used during integration


CLOSE(UNIT=137,STATUS='KEEP') 





GOTO 2000     !       Exit

            
1000 CALL Error_log('Error in Main',tErrorLog_G)
PRINT*,'Error in Main'
PRINT*, 'Check error log file for details, ',tErrorLog_G%zFileName
CALL UnDefineParallelLibrary(qOKL)

2000 CONTINUE

IF (tProcInfo_G%qRoot) PRINT*,'Exited successfully'

END PROGRAM main
