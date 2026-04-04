! ###############################################
! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause
! ###############################################

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

module puffin_mod
   implicit none
contains
   subroutine puffin_main(input_file_name, qOK)
      use transforms
      use lattice
      use Setup, only: init, cleanup
      use undulator
      use initDataType
      use Globals
      use IO, only: tErrorLog_G, log_error
      use GlobalTypes, only: tFieldMesh, tFELFrame, tSimulationFlags, &
                             tOutputConfig, tLatticeElements
      use AdapterGlobals, only: PopulateFieldMeshFromGlobals, &
                                PopulateFELFrameFromGlobals, &
                                PopulateSimulationFlagsFromGlobals, &
                                PopulateOutputConfigFromGlobals, &
                                PopulateLatticeElementsFromGlobals

      implicit none

      character(1024_IP), intent(in) :: input_file_name
      logical, intent(out) :: qOK
      real(kind=wp)    :: sZ, szl
      integer(kind=ip) :: iL, iLst
      logical          :: qOKL

      ! Simulation-lifetime derived types - owned here, passed to all element routines
      type(tFieldMesh)       :: mesh
      type(tFELFrame)        :: frame
      type(tSimulationFlags) :: flags
      type(tOutputConfig)    :: output
      type(tLatticeElements) :: latt

      qOK = .false.
!           Read in data file and initialize system

      call init(input_file_name, sZ, qOKL)
      if (.not. qOKL) then
         call log_error('Error during initialization', tErrorLog_G)
         print*, 'Error during initialization, check error log for details, ', tErrorLog_G%zFileName
         goto 1000
      end if

      ! Populate simulation-lifetime types once from globals set during init().
      ! These are passed as arguments to all element subroutines.
      call PopulateFieldMeshFromGlobals(mesh)
      call PopulateFELFrameFromGlobals(frame)
      call PopulateSimulationFlagsFromGlobals(flags)
      call PopulateOutputConfigFromGlobals(output)
      call PopulateLatticeElementsFromGlobals(latt)

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

            call UndSection(iL, sZ, mesh, frame, flags, output, latt)

         else if (iElmType(iL) == iQuad) then

            call Quad(iL, latt, frame, flags)

         else if (iElmType(iL) == iChic) then

            call disperse(iL, sZ, latt, frame, flags)

         else if (iElmType(iL) == iDrift) then

            call driftSection(iL, sZ, latt, frame, flags)
!     FOR WRITING AFTER EACH DRIFT
!    szl = 0.0_wp
!    call wr_cho(sZ, szl, &
!                0_ip, iCsteps, modNum, iWriteNthSteps, &
!                iIntWriteNthSteps, 0_ip, .true., .true., qOKL)

         else if (iElmType(iL) == iModulation) then

            call BModulation(iL, latt)

         end if

      end do

      if (qDumpEnd_G) then
         szl = 0.0_wp
         call wr_cho(sZ, szl, &
            0_ip, iCsteps, modNum, iWriteNthSteps, &
            iIntWriteNthSteps, 0_ip, .true., .true., qOKL)
      end if

      call cleanup(sZ)   !     Clear arrays and stucts used during integration

      close(UNIT=137,STATUS='KEEP')

      qOK = .true.
      goto 2000     !       Exit

1000  call log_error('Error in Main',tErrorLog_G)
      print*,'Error in Main'
      print*, 'Check error log file for details, ',tErrorLog_G%zFileName

2000  continue

      if (tProcInfo_G%qRoot) print*,'Exited successfully'

   end subroutine puffin_main

end module puffin_mod
