!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

module undulator



!  use paratype
!  use ParallelSetUp
!  use globals
!  use TransformInfoType
!  use stiffness

use FFTW_Constants
use transforms
use sddsPuffin
use lattice
use Stiffness
use RK4int
use dumpFiles
use dummyf


implicit none


contains


  subroutine UndSection(iM, sA, sZ)


    implicit none

! nPeriods        -      Number of periods in the module
! nStepsPerPeriod -      Number of steps per undulator period
!                        to be used in the integration
! iM          -      Which module is this?
! sV              -      6D electron data, arranged according
!                 -      to ArrayFunctions.f90
! sA              -      Field array
! sZ              -      zbar

    integer(kind=ip), intent(in) :: iM
    real(kind=wp), intent(inout) :: sZ, sA(:)


! Local args

    real(kind=wp), allocatable  :: sAr(:), Ar_local(:)
    integer(kind=ip) :: iPer, iS ! Loop index - period counter
    integer(kind=ip) :: nW
    integer(kind=ip) :: iSteps4Diff
    real(kind=wp) :: delz_D, nextDiff
    logical :: qFirst, qLast, qDiffrctd, qWDisp
    logical :: qWPF
    logical :: qWIF
    logical :: qOKL


!    stepsize = delmz(iM)
    
!    iUndType_G = iDetail(iM)




!    nW = nPeriods(iM)
!    nSPP = nStepsPerPeriod(iM)

!     Need to match into undulator

  call initUndulator(iUnd_cr, sZ)
  call matchIn(sZ)




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

  start_step = 1_ip  ! ...TEMP...

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




!     #####
!     Begin integration through undulator





  do iStep = start_step, nSteps
  

  iCsteps = iCsteps + 1_ip



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







!call diffractNT(sA, iStep)



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





!                   Write result to file
 
    iCount = iCount + 1_IP
  





  if ( qWriteq(iStep, iWriteNthSteps, iIntWriteNthSteps, nSteps, &
               qWDisp) ) then
  
    call writeIM(sA, Ar_local, sZ, &
                 zDataFileName, iStep, iCsteps, iWriteNthSteps, &
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
       print*,' finished step ',iCsteps, end_time-start_time
       WRITE(137,*) ' finished step ',iCsteps, end_time-start_time
    end if
  







!                Dump data when time comes

    if (mod(iStep,iDumpNthSteps)==0) then
       if (tProcInfo_G%qRoot) PRINT*, 'Dumping data in case of crash'
       
       call innerLA2largeA(Ar_local,sA,lrecvs,ldispls,tTransInfo_G%qOneD)
       
       if (qDump_G) call DUMPDATA(sA,tProcInfo_G%rank,NX_G*NY_G*NZ2_G,&
            iNumberElectrons_G,sZ,istep,tArrayA(1)%tFileType%iPage)
    end if






!    if (modCount > ModNum) EXIT


  end do   ! End of integration loop



  call matchOut(sZ)


  deallocate(sAr)
  deallocate(Ar_local)


  iUnd_cr = iUnd_cr + 1_ip

end subroutine UndSection


































































































!     iSteps4Diff = round(real(nSPP,kind=wp) * sDFrac)

!     ! if (mod(iSteps4Diff,2) /= 0) iSteps4Diff = iSteps4Diff + 1_ip   ! if not even, force even






!     delz_D = stepsize * iSteps4Diff   !   Diffraction step size










!     CALL Get_time(start_time)


! !       First diffraction half-step

!     if (qDiffraction_G) then

!       call DiffractionStep(delz_D/2.0_WP, frecvs, fdispls, sV, sA, qOKL)

!       nextDiff = nextDiff + delz_D/2.0_WP

!     end if


! !       Fill smaller sAr array, before main loop

!     call AllocDrA(sAr, Ar_local)

!     call fillDrA(sA, sAr, Ar_local)


!     do iPer = 1, nW ! loop around undulator periods



      
!       do iS = 1, nSPP   

! !                    RK4 for electrons and field source

!         call rk4par(sV, sAr, Ar_local, sZ, stepsize, mrecvs, mdispls) ! zbar now incremented in RK4


! !                        Diffraction step

!         if (sZ > nextDiff - stepSize / 100.0_WP) then 

!           call diffraction(sA, sAr, Ar_local, sV, delz_D) ! Diffract full step

!           nextDiff = nextDiff + delz_D

!         end if

!         call Get_time(end_time)
  
!         if (tProcInfo_G%QROOT ) then
!            print*, 'MODULE ', iM, ': finished step ',iS, ' in undulator period ', &
!                                 iPer, ' after ', end_time-start_time, ' seconds.'
!            write(126,*) 'MODULE ', iM, ': finished step ',iS, ' in undulator period ', &
!                                 iPer, ' after ', end_time-start_time, ' seconds.'
!         end if

!         ! Add writing in here later for the sub-period writing options

!       end do



!       if ((mod(iPer,iWNUP4Part_G) == 0_IP) .or. (mod(iPer,iWNUP4Int_G) == 0_IP)) then
      
!         if (mod(iPer,iWNUP4Part_G) == 0_IP) qWPF = .true.
!         if (mod(iPer,iWNUP4Part_G) == 0_IP) qWIF = .true.

!         if (qWPF .and. qDiffraction_G) then
!           ! get data back to "full" sA
!               deallocate(sAr)
!               call innerLA2largeA(Ar_local,sA,lrecvs,ldispls,tTransInfo_G%qOneD)
!               deallocate(Ar_local)

!         end if

!         call wdfs(sA, sV, sZ, iM, iPer, tArrayA, tArrayE, tArrayZ, qWPF, qWIF, & 
!                   qSeparateStepFiles_G, .true., zDataFileName, qOKL)


!         if (qWPF .and. qDiffraction_G) then
!           ! Get sA back to smaller reduced size arrays

!           call AllocDrA(sAr, Ar_local)
!           call fillDrA(sA, sAr, Ar_local)

!         end if



!         qWPF = .false.
!         qWIF = .false.
  



!       end if

!     end do

! !            Last diffraction half step

!     deallocate(sAr)
!     call innerLA2largeA(Ar_local,sA,lrecvs,ldispls,tTransInfo_G%qOneD)
!     deallocate(Ar_local)

!     call DiffractionStep(delz_D/2.0_WP, frecvs, fdispls, sV, sA, qOKL)


! !     "Match out" of undulator....

!     call matchOut(sV, sZ)


! !!!!!!
! !
! !        SETUP FOR ABOVE....
! !
! !!!!!!


  


! !!!!!!   END SETUP





! !!!!!!   Undulator section complete

!   end subroutine UndSection


! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! !   subroutine UndPeriod(delz, iSteps4Diff, &
! !                        nSteps, qSubWrite, &
! !                        sA, sV, sZ)

! !     implicit none

! ! ! Integrate electrons and field over one undulator period
! ! ! using split-step fourier method.
! ! !
! ! ! Note that the electron and field driving integration
! ! ! stepsize and the field diffraction stepsizes are
! ! ! not, in general, the same size. We have a diffraction
! ! ! stepsize and an electron/field source step size.
! ! !
! ! !
! ! !
! ! ! qSubWrite         If writing data on a sub-period scale


! !     real(kind=wp), intent(in) :: delz
! !     integer(kind=ip), intent(in) :: iSteps4Diff
! !     integer(kind=ip), intent(in) :: nSteps
! !     logical, intent(in) :: qSubWrite
! !     real(kind=wp), intent(inout) :: sA(:), sV(:), sZ

! ! !  Local args:

! !     real(kind=wp) :: delz_D
! !     real(kind=wp), allocatable :: sAr(:), Ar_local(:)


! ! !    iWCount = 0 ! Writing count






! ! !       Fill smaller sAr array, before main loop

! !     call AllocDrA(sAr, Ar_local)

! !     call fillDrA(sA, sAr, Ar_local)



! !     do iS = 1, nSteps ! loop around steps to integrate over one complete period
  
! ! !   Half of split step method: electron propagation
! ! !                and field driving.

! !           if (qElectronsEvolve_G .or. qFieldEvolve_G &
! !              .or. qElectronFieldCoupling_G) then
      
! !              call rk4par(sV, sAr, Ar_local, sZ, sStepSize, mrecvs, mdispls)
      
! !           end if 


! ! !            Diffraction step

! !           if ((mod(iStep + iStepsPerDiff / 2_IP, iStepsPerDiff) == 0) &
! !                         .and. (iStep /= nsteps - iStepsPerDiff / 2_IP)) then
          
! !               if (qDiffraction_G) call diffraction(sA, sAr, Ar_local, sV, delz_D) ! Diffract full step
        
! !           end if


! ! !            Write data if required

! !           if (writin-time) WriteData(prefix 'modNum_periodNum_stepNum')

! !     end do


! ! !            Last diffraction half step

! !     deallocate(sAr)
! !     call innerLA2largeA(Ar_local,sA,lrecvs,ldispls,tTransInfo_G%qOneD)
! !     deallocate(Ar_local)

! !     if (qDiffraction_G) call DiffractionStep(delz_D/2.0_WP, frecvs, fdispls, &
! !                                              sV, sA, qOKL) ! Final half-step


! !   end subroutine UndPeriod


! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!   subroutine diffraction(sA, sAr, Ar_local, sV, dz)

!     implicit none

! ! Fill the local and global field arrays

!     real(kind=wp), intent(inout) :: sA, sV
!     real(kind=wp), allocatable, intent(inout) ::  sAr, Ar_local
!     real(kind=wp), intent(in) :: dz  


!     deallocate(sAr)
!     call innerLA2largeA(Ar_local,sA,lrecvs,ldispls,tTransInfo_G%qOneD)
!     deallocate(Ar_local)

!     call DiffractionStep(dz,&
!          frecvs,&
!          fdispls,&
!          sV,&
!          sA,&
!          qOKL)
   
!     call AllocDrA(sAr, Ar_local)

!     call fillDrA(sA, sAr, Ar_local)



!   end subroutine diffraction


















!   subroutine AllocDrA(sAr, Ar_local)

!     implicit none

! ! Allocates the local and global field arrays 
! ! used for the field driving stages.

!     real(kind=wp), allocatable, intent(inout) :: sAr(:), Ar_local(:)


!     allocate(sAr(2*ReducedNX_G*ReducedNY_G*NZ2_G))
!     allocate(Ar_local(2*local_rows))


!   end subroutine AllocDrA




!   subroutine fillDrA(sA, sAr, Ar_local)

!   implicit none

! ! Fill the local and global field arrays

!   real(kind=wp), intent(inout) :: sA(:)
!   real(kind=wp), allocatable, intent(inout) ::  sAr(:), Ar_local(:)


!   if (tTransInfo_G%qOneD) then

!      Ar_local(1:local_rows)=sA(fst_row:lst_row)

!      Ar_local(local_rows+1:2*local_rows)=&
!           sA(fst_row+iNumberNodes_G:lst_row+iNumberNodes_G)

!   else

!      call getAlocalFL(sA,Ar_local)

!   end if

!   call local2globalA(Ar_local,sAr,mrecvs,mdispls,tTransInfo_G%qOneD)


!   end subroutine fillDrA


end module undulator
