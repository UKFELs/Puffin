!> Recompute the golden reduction constants for the big 3D integration test
!! from an existing pair of Puffin output files.
!!
!! testMPIIntegration3DBig.pf pins the final field and beam down with a handful
!! of reductions hard-coded in its parameter block. Those numbers have to come
!! from somewhere, and the obvious route -- run the test, let it print what it
!! computed, paste that back in -- costs a full 9-minute simulation just to read
!! two files that already exist on disk.
!!
!! This program does the reading and the arithmetic on their own. Point it at
!! the final aperp and electrons dumps of a run and it prints the parameter
!! block ready to paste. It deliberately uses the same H5in calls and the same
!! reduction expressions as the test, compiled the same way, so the sums are
!! accumulated in the same order and the constants match bit for bit rather
!! than merely closely.
!!
!! Usage (single rank -- the H5in readers do their work on root):
!!
!!   mpiexec -n 1 ./harvest_big_reference <aperp_file> <electrons_file>
!!
!! The step-count and file-index constants are not produced here; read those off
!! the dumps as described in the header of testMPIIntegration3DBig.pf.

program harvest_big_reference

  use MPI
  use puffin_kinds
  use puffin_mpiInfo, only: tProcInfo_G
  use ParallelSetUp, only: InitializeProcessors
  use H5in, only: getMacroparticleCount, readH5BeamDataOntoRootProcess, &
                  getNX, getNY, getNZ2, readH5FieldDataOntoRootProcess3D

  implicit none

  character(len=1024) :: fieldFile, electronFile
  integer :: error, nArgs
  logical :: qOKL
  integer(kind=ip) :: nX, nY, nZ2, nMPs
  integer(kind=8) :: nField
  real(kind=WP), allocatable :: rfield(:), ifield(:)
  real(kind=WP), allocatable :: sElX(:), sElY(:), sElZ2(:), sElPX(:), sElPY(:), &
                                sElGam(:), chi_bar(:)
  real(kind=WP) :: sumAbsR, sumAbsI, intens, maxAmp
  real(kind=WP) :: rmsX, rmsY, rmsZ2, rmsPX, rmsPY, rmsGam, sumChi

  ! Must match goldenFmt in testMPIIntegration3DBig.pf, so the values printed
  ! here round-trip exactly into the parameter block.
  character(len=*), parameter :: goldenFmt = '(1x, a, " = ", es25.17e3, "_WP")'

  call MPI_INIT(error)
  call InitializeProcessors(tProcInfo_G, qOKL)

  nArgs = command_argument_count()
  if (nArgs < 2) then
    if (tProcInfo_G%qRoot) then
      print *, 'usage: harvest_big_reference <aperp_file> <electrons_file>'
    end if
    call MPI_FINALIZE(error)
    stop 1
  end if

  call get_command_argument(1, fieldFile)
  call get_command_argument(2, electronFile)

! --- Field ----------------------------------------------------------------

  nX  = getNX(trim(fieldFile))
  nY  = getNY(trim(fieldFile))
  nZ2 = getNZ2(trim(fieldFile))

  if (tProcInfo_G%qRoot) then
    nField = int(nX, 8) * int(nY, 8) * int(nZ2, 8)
    print *, 'Field mesh (nX, nY, nZ2):', nX, nY, nZ2
  else
    nField = 0_8
  end if

  allocate(rfield(nField), ifield(nField))

  call readH5FieldDataOntoRootProcess3D(trim(fieldFile), rfield, ifield, nX, nY, nZ2)

  if (tProcInfo_G%qRoot) then

    sumAbsR = sum(abs(rfield))
    sumAbsI = sum(abs(ifield))
    intens  = sum(rfield*rfield + ifield*ifield)
    maxAmp  = sqrt(maxval(rfield*rfield + ifield*ifield))

    print *, '--- GOLDEN REFERENCE VALUES (field) ---'
    write(*, goldenFmt) 'SHCSE_BIG_FIELD_SUMABSR', sumAbsR
    write(*, goldenFmt) 'SHCSE_BIG_FIELD_SUMABSI', sumAbsI
    write(*, goldenFmt) 'SHCSE_BIG_FIELD_INTENS ', intens
    write(*, goldenFmt) 'SHCSE_BIG_FIELD_MAXAMP ', maxAmp

  end if

  deallocate(rfield, ifield)

! --- Beam -----------------------------------------------------------------

  nMPs = getMacroparticleCount(trim(electronFile))
  if (.not. tProcInfo_G%qRoot) nMPs = 0

  allocate(sElX(nMPs), sElY(nMPs), sElZ2(nMPs), sElPX(nMPs), sElPY(nMPs), &
           sElGam(nMPs), chi_bar(nMPs))

  call readH5BeamDataOntoRootProcess(trim(electronFile), sElX, sElY, sElZ2, &
    sElPX, sElPY, sElGam, chi_bar, nMPs)

  if (tProcInfo_G%qRoot) then

    rmsX   = sqrt(sum(sElX*sElX)     / real(nMPs, WP))
    rmsY   = sqrt(sum(sElY*sElY)     / real(nMPs, WP))
    rmsZ2  = sqrt(sum(sElZ2*sElZ2)   / real(nMPs, WP))
    rmsPX  = sqrt(sum(sElPX*sElPX)   / real(nMPs, WP))
    rmsPY  = sqrt(sum(sElPY*sElPY)   / real(nMPs, WP))
    rmsGam = sqrt(sum(sElGam*sElGam) / real(nMPs, WP))
    sumChi = sum(chi_bar)

    print *, '--- GOLDEN REFERENCE VALUES (electrons) ---'
    print *, 'SHCSE_BIG_NMPS = ', nMPs
    write(*, goldenFmt) 'SHCSE_BIG_RMS_X  ', rmsX
    write(*, goldenFmt) 'SHCSE_BIG_RMS_Y  ', rmsY
    write(*, goldenFmt) 'SHCSE_BIG_RMS_Z2 ', rmsZ2
    write(*, goldenFmt) 'SHCSE_BIG_RMS_PX ', rmsPX
    write(*, goldenFmt) 'SHCSE_BIG_RMS_PY ', rmsPY
    write(*, goldenFmt) 'SHCSE_BIG_RMS_GAM', rmsGam
    write(*, goldenFmt) 'SHCSE_BIG_SUM_CHI', sumChi

  end if

  deallocate(sElX, sElY, sElZ2, sElPX, sElPY, sElGam, chi_bar)

  call MPI_FINALIZE(error)

end program harvest_big_reference
