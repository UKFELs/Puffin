! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Fortran Type description and subroutines controlling the field mesh
!> parallelism in Puffin.

module ParaField


use paratype
use globals
use ParallelSetUp
use TransformInfoType
use gtop2
use filetype
use createSDDS
use sddsROutput
use sddsSetup


implicit none

private

type, public :: pMesh

  real(kind=wp), allocatable :: fr_rfield(:,:,:), bk_rfield(:,:,:), ac_rfield(:,:,:), &
                                fr_ifield(:,:,:), bk_ifield(:,:,:), ac_ifield(:,:,:), &
                                tre_fft(:,:,:), tim_fft(:,:,:)

  real(kind=wp), allocatable :: tmp_A(:,:,:)

  integer(kind=ip), allocatable :: recvs_pf(:), displs_pf(:), recvs_ff(:), &
                                   displs_ff(:), recvs_ef(:), displs_ef(:)

  integer(kind=ip), allocatable :: recvs_ppf(:), displs_ppf(:), recvs_fpf(:), &
                                   displs_fpf(:), recvs_epf(:), displs_epf(:)

  integer(kind=ip) :: fz2, ez2, lTr, bz2, fbuffLen, fbuffLenM, tllen, mainlen, &
                      fz2_GGG, ez2_GGG

  integer(kind=ip) :: ffs, ffe, tlflen, ees, eee, tlelen, tlflen_glob, tlelen_glob, &
                      tlflen4arr, tlelen4arr, ffs_GGG, ffe_GGG, ees_GGG, eee_GGG

!!!   For parallel algorithm to deal with over-compression...

  integer(kind=ip), allocatable :: lrank_v(:), rrank_v(:,:), &
                                   lrfromwhere(:)

  integer(kind=ip) :: nsnds_bf, nrecvs_bf

  logical :: qUnique

  integer(kind=ip), allocatable :: ac_ar(:,:), ff_ar(:,:), ee_ar(:,:), &
                                   ft_ar(:,:)


  integer(kind=ip) :: iParaBas   ! Basis for parallelism - options below:

  logical :: qStart_new


contains

  procedure, public :: getLocalFieldIndices
  procedure, public :: UpdateGlobalPow
  procedure, private :: getFStEnd, calcBuff
  procedure, private :: rearrElecs, getFrBk

end type

! Options for parallelism

  integer(kind=ip), parameter :: iElectronBased=1, &
                                 iFieldBased = 2, &
                                 iFFTW_based = 3



contains


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Sets up the parallel field mesh, distributed over MPI processes.
!> If already initialized, this subroutine will then redistribute
!> the field mesh according to the current state of the system.
!> @param[in] tScale Custom Fortran type describing coordinate scaling.
!> @param[in] tMPI Custom Fortran type to hold MPI info.
!> @param[in] sdz Estimate the additional mesh size needed for propagating
!> the beam a distance of sdz * lambda_w. The mesh layout calculated by this
!> will then be valid for that propagation distance. (sdz expressed in units
!> of a 'base' undulator period)

	subroutine getLocalFieldIndices(self, tScale, tMPI, sdz)

    use typempicomm
    use typeScale

    implicit none

    class(pMesh), intent(inout) :: self
    type(fScale), intent(in) :: tScale
    type(fMPIComm), intent(in) :: tMPI
    real(kind=wp), intent(in) :: sdz

    real(kind=wp), allocatable :: sp2(:), fr_rfield_old(:), &
                                  fr_ifield_old(:), &
                                  bk_rfield_old(:), &
                                  bk_ifield_old(:), &
                                  ac_rfield_old(:), &
                                  ac_ifield_old(:)

    integer(kind=ip) :: locN, ij
    integer(kind=ip) :: fz2_r, gath_v
    integer :: req, error, lrank, rrank
    integer sendstat(MPI_STATUS_SIZE)
    integer statr(MPI_STATUS_SIZE)



    integer(kind=ip) :: fz2_OLD, ez2_OLD, lTr_OLD, bz2_OLD, &
                        fbuffLen_OLD, fbuffLenM_OLD, tllen_OLD, &
                        mainlen_OLD

    integer(kind=ip) :: ffs_OLD, ffe_OLD, ees_OLD, eee_OLD

    integer(kind=ip), allocatable :: ee_ar_old(:,:), &
                                     ff_ar_old(:,:), &
                                     ac_ar_old(:,:)



    integer(kind=ip) :: tnjdlz2


    integer(kind=ipl) :: sendbuff, recvbuff
    integer recvstat(MPI_STATUS_SIZE)

    if (self%qStart_new) then

      self%iParaBas = iFieldBased

      call self%getFStEnd(tMPI)

      self%bz2 = self%ez2

      self%tlflen_glob = 0
      self%tlflen = 0
      self%tlflen4arr = 1
      self%ffs = 0
      self%ffe = 0


      self%tlelen_glob = 0
      self%tlelen = 0
      self%tlelen4arr = 1
      self%ees = 0
      self%eee = 0

      allocate(self%ee_ar(tMPI%size, 3))
      allocate(self%ff_ar(tMPI%size, 3))
      allocate(self%ac_ar(tMPI%size, 3))


      call setupLayoutArrs(tMPI, self%mainlen, self%fz2, self%ez2, self%ac_ar)
      call setupLayoutArrs(tMPI, self%tlflen, self%ffs, self%ffe, self%ff_ar)
      call setupLayoutArrs(tMPI, self%tlelen, self%ees, self%eee, self%ee_ar)

      allocate(self%fr_rfield(self%tlflen4arr*ntrnds_G), &
                 self%fr_ifield(self%tlflen4arr*ntrnds_G))
      allocate(self%bk_rfield(self%tlelen4arr*ntrnds_G), &
               self%bk_ifield(self%tlelen4arr*ntrnds_G))

      allocate(self%ac_rfield(self%mainlen*ntrnds_G), &
               self%ac_ifield(self%mainlen*ntrnds_G))


      self%ac_rfield = 0_wp
      self%ac_ifield = 0_wp

      self%fr_rfield = 0_wp
      self%fr_ifield = 0_wp
      self%bk_rfield = 0_wp
      self%bk_ifield = 0_wp

      self%qStart_new = .false.

      self%iParaBas = iElectronBased
      self%qUnique = .true.

    else

      deallocate(self%recvs_pf, self%displs_pf, self%tmp_A)
      deallocate(self%recvs_ff, self%displs_ff, self%recvs_ef, self%displs_ef)
      deallocate(self%lrank_v, self%lrfromwhere)
      deallocate(self%rrank_v)

      deallocate(self%recvs_ppf, self%displs_ppf)
      deallocate(self%recvs_fpf, self%displs_fpf, self%recvs_epf, self%displs_epf)

    end if



  allocate(ee_ar_old(tMPI%size, 3))
  allocate(ff_ar_old(tMPI%size, 3))
  allocate(ac_ar_old(tMPI%size, 3))

  ee_ar_old = self%ee_ar
  ff_ar_old = self%ff_ar
  ac_ar_old = self%ac_ar



  call self%getFStEnd(tMPI)    ! Define new 'active' region







  call setupLayoutArrs(tMPI, self%mainlen, self%fz2, self%ez2, self%ac_ar)





  !if (iParaBas /= iFFTW_based) then

  if (self%qUnique) call self%rearrElecs(tMPI)   ! Rearrange electrons



  call self%calcBuff(tMPI, 4.0_wp * pi * tScale%rho * sdz)  ! Calculate buffers
!  call calcBuff(4.0_wp)  ! Calculate buffers

 ! else

!    bz2 = ez2

  !end if




  call self%getFrBk(tMPI)  ! Get surrounding nodes


  call setupLayoutArrs(tMPI, self%tlflen, self%ffs, self%ffe, self%ff_ar)
  call setupLayoutArrs(tMPI, self%tlelen, self%ees, self%eee, self%ee_ar)


  if (.not. self%qUnique) then

    self%ac_ar(1,1) = self%mainlen
    self%ac_ar(1,2) = self%fz2
    self%ac_ar(1,3) = self%ez2
    self%ac_ar(2:tMPI%size,:) = 0

  end if


!  print*, 'trying to get layout of main as', fz2, ez2, bz2

!print*, 'AC_AR_OLD', ac_ar_old


! for each front, active and back (9 calls??)


  allocate(fr_rfield_old(size(fr_rfield)), fr_ifield_old(size(fr_ifield)))
  allocate(bk_rfield_old(size(bk_rfield)), bk_ifield_old(size(bk_ifield)))
  allocate(ac_rfield_old(size(ac_rfield)), ac_ifield_old(size(ac_ifield)))

  fr_rfield_old = self%fr_rfield
  fr_ifield_old = self%fr_ifield
  bk_rfield_old = self%bk_rfield
  bk_ifield_old = self%bk_ifield
  ac_rfield_old = self%ac_rfield
  ac_ifield_old = self%ac_ifield

  deallocate(self%ac_rfield, self%ac_ifield)
  deallocate(self%fr_rfield, self%fr_ifield)
  deallocate(self%bk_rfield, self%bk_ifield)

  allocate(self%fr_rfield(self%tlflen4arr*ntrnds_G), &
           self%fr_ifield(self%tlflen4arr*ntrnds_G))
  allocate(self%bk_rfield(self%tlelen4arr*ntrnds_G), &
           self%bk_ifield(self%tlelen4arr*ntrnds_G))
  allocate(self%ac_rfield(self%tllen*ntrnds_G), &
           self%ac_ifield(self%tllen*ntrnds_G))

  self%ac_rfield = 0_wp
  self%ac_ifield = 0_wp

  self%bk_rfield = 0_wp
  self%bk_ifield = 0_wp
  self%fr_rfield = 0_wp
  self%fr_ifield = 0_wp


  call redist2new2(tMPI, ff_ar_old, self%ff_ar, fr_rfield_old, self%fr_rfield)
  call redist2new2(tMPI, ff_ar_old, self%ff_ar, fr_ifield_old, self%fr_ifield)

  call redist2new2(tMPI, ee_ar_old, self%ff_ar, bk_rfield_old, self%fr_rfield)
  call redist2new2(tMPI, ee_ar_old, self%ff_ar, bk_ifield_old, self%fr_ifield)

  call redist2new2(tMPI, ac_ar_old, self%ff_ar, ac_rfield_old, self%fr_rfield)
  call redist2new2(tMPI, ac_ar_old, self%ff_ar, ac_ifield_old, self%fr_ifield)





  call redist2new2(tMPI, ff_ar_old, self%ee_ar, fr_rfield_old, self%bk_rfield)
  call redist2new2(tMPI, ff_ar_old, self%ee_ar, fr_ifield_old, self%bk_ifield)

  call redist2new2(tMPI, ee_ar_old, self%ee_ar, bk_rfield_old, self%bk_rfield)
  call redist2new2(tMPI, ee_ar_old, self%ee_ar, bk_ifield_old, self%bk_ifield)


  call redist2new2(tMPI, ac_ar_old, self%ee_ar, ac_rfield_old, self%bk_rfield)
  call redist2new2(tMPI, ac_ar_old, self%ee_ar, ac_ifield_old, self%bk_ifield)

!  call mpi_finalize(error)
!  stop





  call redist2new2(tMPI, ff_ar_old, self%ac_ar, fr_rfield_old, self%ac_rfield)
  call redist2new2(tMPI, ff_ar_old, self%ac_ar, fr_ifield_old, self%ac_ifield)

  call redist2new2(tMPI, ee_ar_old, self%ac_ar, bk_rfield_old, self%ac_rfield)
  call redist2new2(tMPI, ee_ar_old, self%ac_ar, bk_ifield_old, self%ac_ifield)

  call redist2new2(tMPI, ac_ar_old, self%ac_ar, ac_rfield_old, self%ac_rfield)
  call redist2new2(tMPI, ac_ar_old, self%ac_ar, ac_ifield_old, self%ac_ifield)




  deallocate(ff_ar_old, &
             ee_ar_old, &
             ac_ar_old)


  deallocate(ac_rfield_old, ac_ifield_old)
  deallocate(fr_rfield_old, fr_ifield_old)
  deallocate(bk_rfield_old, bk_ifield_old)


  if (.not. self%qUnique) then

    call MPI_Bcast(self%ac_rfield, self%tllen*ntrnds_G, &
                   mpi_double_precision, 0, &
                   tMPI%comm, error)

    call MPI_Bcast(self%ac_ifield, self%tllen*ntrnds_G, &
                   mpi_double_precision, 0, &
                   tMPI%comm, error)
  end if


! then deallocate old fields

! then redist electrons for new layout


!$$$$$$$$$$$$$$$$$%%%^^^^^^^^^FFFFFFFFFFFFFFFFF




!  #######################################################################
!     Get gathering arrays - only used to gather active field sections
!     back to GLOBAL field (the full field array on each process...)
!     Will NOT be needed later on....
!     ...and should now ONLY be used for data writing while testing...



      if (tMPI%rank /= tMPI%size-1) then
        gath_v = self%tlflen*ntrnds_G !-1
      else
        gath_v = self%tlflen*ntrnds_G
      end if


      allocate(self%recvs_ff(tMPI%size), self%displs_ff(tMPI%size))
      call getGathArrs(gath_v, self%recvs_ff, self%displs_ff)





      if (tMPI%rank /= tMPI%size-1) then
        gath_v = self%mainlen*ntrnds_G ! -1
      else
        gath_v = self%mainlen*ntrnds_G
      end if

      allocate(self%recvs_pf(tMPI%size), self%displs_pf(tMPI%size))
      call getGathArrs(gath_v, self%recvs_pf, self%displs_pf)



      if (tMPI%rank /= tMPI%size-1) then
        gath_v = self%tlelen*ntrnds_G !-1
      else
        gath_v = self%tlelen*ntrnds_G
      end if

      allocate(self%recvs_ef(tMPI%size), self%displs_ef(tMPI%size))
      call getGathArrs(gath_v, self%recvs_ef, self%displs_ef)









!  #######################################################################
!     Get gathering arrays - only used to gather active field sections
!   THESE ARE FOR POWER



      if (tMPI%rank /= tMPI%size-1) then
        gath_v = self%tlflen !-1
      else
        gath_v = self%tlflen
      end if


      allocate(self%recvs_fpf(tMPI%size), self%displs_fpf(tMPI%size))
      self%recvs_fpf = 0
      self%displs_fpf = 0
      call getGathArrs(gath_v, self%recvs_fpf, self%displs_fpf)





      if (tMPI%rank /= tMPI%size-1) then
        gath_v = self%mainlen ! -1
      else
        gath_v = self%mainlen
      end if

      allocate(self%recvs_ppf(tMPI%size), self%displs_ppf(tMPI%size))
      self%recvs_ppf = 0
      self%displs_ppf = 0
      call getGathArrs(gath_v, self%recvs_ppf, self%displs_ppf)



      if (tMPI%rank /= tMPI%size-1) then
        gath_v = self%tlelen !-1
      else
        gath_v = self%tlelen
      end if

      allocate(self%recvs_epf(tMPI%size), self%displs_epf(tMPI%size))
      self%recvs_epf = 0
      self%displs_epf = 0
      call getGathArrs(gath_v, self%recvs_epf, self%displs_epf)

!  #######################################################################

  call mpi_barrier(tMPI%comm, error)

!!!!!!          Update number of macroparticles on each process after
!!!!!!                rearranging everything in the subroutine:

    IF (tMPI%rank == tMPI%size-1) THEN
       rrank = 0
       lrank = tMPI%rank-1
    ELSE IF (tMPI%rank==0) THEN
       rrank = tMPI%rank+1
       lrank = tMPI%size-1
    ELSE
       rrank = tMPI%rank+1
       lrank = tMPI%rank-1
    END IF


    procelectrons_G(1) = iNumberElectrons_G

    sendbuff = iNumberElectrons_G
    recvbuff = iNumberElectrons_G

    DO ij=2,tMPI%size
       CALL MPI_ISSEND( sendbuff,1,MPI_INT_HIGH,rrank,&
            0,tMPI%comm,req,error )
       CALL MPI_RECV( recvbuff,1,MPI_INT_HIGH,lrank,&
            0,tMPI%comm,recvstat,error )
       CALL MPI_WAIT( req,sendstat,error )
       procelectrons_G(ij) = recvbuff
       sendbuff=recvbuff
    END DO




    if (self%qUnique) then

      allocate(self%tmp_A(maxval(self%lrank_v)*ntrndsi_G))

    else
      allocate(self%tmp_A(self%tllen*ntrndsi_G))
    end if


      ! allocate(tmp_A(fbuffLenM))

     tmp_A = 0_wp

!      print*, tProcInfo_G%rank, ' set up with bounds of ', fz2, ez2, bz2! , &

      qPArrOK_G = .true.
!      'with a buffer length of ', fbuffLen, 'and a total length of ', tllen
      !print*, tProcInfo_G%rank, ' and size of sA (over 2) is ', size(sA) / 2

    end subroutine getLocalFieldIndices







  subroutine inner2Outer(inner_ra, inner_ia)

    implicit none

    real(kind=wp), contiguous, intent(in) :: inner_ra(:), inner_ia(:)

    integer(kind=ip) :: iz, ssti, ssei, iy, sst, sse
    integer(kind=ip) :: nxout, nyout ! should be made global and calculated

    nxout = (nx_g - nspindx)/2
    nyout = (ny_g - nspindy)/2


    do iz = fz2, bz2

      do iy = 1, nspinDY

        sst = (iz - (fz2-1)-1)*ntrnds_G + &
                         nx_G*(nyout+(iy-1)) + &
                         nxout + 1

        sse = sst + nspinDX - 1

        ssti = (iz - (fz2-1)-1)*ntrndsi_G + &
                         nspinDX*(iy-1) + 1
        ssei = ssti + nspinDX - 1

        ac_rfield(sst:sse) = inner_ra(ssti:ssei)
        ac_ifield(sst:sse) = inner_ia(ssti:ssei)

      end do

    end do


  end subroutine inner2Outer





  subroutine outer2Inner(inner_ra, inner_ia)


    implicit none

    real(kind=wp), contiguous, intent(out) :: inner_ra(:), inner_ia(:)

    integer(kind=ip) :: iz, sst, sse, ssti, ssei
    integer(kind=ip) :: nxout, nyout, iy ! should be made global and calculated

    nxout = (nx_g - nspindx)/2
    nyout = (ny_g - nspindy)/2

    do iz = fz2, bz2

      do iy = 1, nspinDY

        sst = (iz - (fz2-1)-1)*ntrnds_G + &
                         nx_G*(nyout+(iy-1)) + &
                         nxout + 1

        sse = sst + nspinDX - 1

        ssti = (iz - (fz2-1)-1)*ntrndsi_G + &
                         nspinDX*(iy-1) + 1
        ssei = ssti + nspinDX - 1

        inner_ra(ssti:ssei) = ac_rfield(sst:sse)
        inner_ia(ssti:ssei) = ac_ifield(sst:sse)

      end do

    end do


  end subroutine outer2Inner


  subroutine getInNode()

  real(kind=wp) :: sminx, smaxx, sminy, smaxy
  integer(kind=ip) :: iminx, imaxx, iminy, imaxy, &
                      inBuf

  integer :: error
  logical :: qOKL

  inBuf = 3_ip

  if (iNumberElectrons_G > 0_ipl) then

    smaxx = abs(maxval(sElX_G))
    sminx = abs(minval(sElX_G))

    smaxy = abs(maxval(sElY_G))
    sminy = abs(minval(sElY_G))

    imaxx = ceiling(smaxx / sLengthOfElmX_G)
    iminx = floor(sminx / sLengthOfElmX_G)

    imaxy = ceiling(smaxy / sLengthOfElmY_G)
    iminy = floor(sminy / sLengthOfElmY_G)

    nspinDX = maxval((/imaxx,iminx/)) + inBuf
    nspinDY = maxval((/imaxy,iminy/)) + inBuf

    nspinDX = nspinDX * 2
    nspinDY = nspinDY * 2

  else   ! arbitrary minimum in case of zero MPs...

    nspinDX = 2_ipl
    nspinDY = 2_ipl

  end if

  if (mod(nx_g, 2) .ne. mod(nspinDX, 2) ) then

    nspinDX =  nspinDX + 1

  end if

  if (mod(ny_g, 2) .ne. mod(nspinDY, 2) ) then

    nspinDY =  nspinDY + 1

  end if



  call mpi_allreduce(MPI_IN_PLACE, nspinDX, 1, mpi_integer, &
                   mpi_max, tProcInfo_G%comm, error)

  call mpi_allreduce(MPI_IN_PLACE, nspinDY, 1, mpi_integer, &
                   mpi_max, tProcInfo_G%comm, error)

  if (nspinDX > nx_g) then
    print*, 'ERROR, x grid not large enough'
    print*, 'nspinDX = ', nspinDX
    call StopCode(qOKL)
  end if


  if (nspinDY > ny_g) then
    print*, 'ERROR, y grid not large enough'
    print*, 'nspinDY = ', nspinDY
    call StopCode(qOKL)
  end if


  ntrndsi_G = nspinDX * nspinDY

  qInnerXYOK_G = .true.

  end subroutine getInNode



!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Get local number of nodes and start and global indices of start and end 
!> points.
!> @param[in] ndpts Total number of nodes in mesh.
!> @param[in] numproc Total number of MPI processes.
!> @param[in] rank Rank of local MPI process.
!> @param[out] locN Local number of nodes of the global mesh which will be 
!> stored on this process.
!> @param[out] local_start Global starting index of the mesh stored on this
!> process.
!> @param[out] local_start Global ending index of the mesh stored on this
!> process.

  subroutine divNodes(ndpts, numproc, rank, &
                      locN, local_start, local_end)

    integer(kind=ip), intent(in) :: ndpts, numproc, rank

    integer(kind=ip), intent(out) :: locN
    integer(kind=ip), intent(out) :: local_start, local_end

!          LOCAL ARGS

    real(kind=wp) :: frac
    integer(kind=ip) :: lowern, highern, remainder


    frac = real(ndpts)/real(numproc)
    lowern = FLOOR(frac)
    highern = CEILING(frac)
    remainder = MOD(ndpts,numproc)

    if (remainder==0) then
       locN = lowern
    else
       if (rank < remainder) then
          locN = highern
       else
          locN = lowern
       end if
    end if


!     Calculate local start and end values.

    if (rank >= remainder) then

      local_start = (remainder*highern) + ((rank-remainder) * lowern) + 1
      local_end = local_start + locN - 1

    else

      local_start = rank*locN + 1
      local_end = local_start + locN - 1

    end if


  end subroutine divNodes

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Calculate the overlaps of data in the parallel mesh during the redistribution
!> stage, so that the algorithm knows where to send and/or recieve data to/from.
!> So e.g. f_send(1,1:3) will hold number of nodes to send from local front 
!> array to front array of rank=0, and the local start and end positions of what
!> is being sent from the local start array, respectively.
!> @param[in] tMPI Custom Fortran type to hold MPI info.
!> @param[in] iso Start index of range of mesh on this process.
!> @param[in] ieo End index of range of mesh on this process.
!> @param[in] f_ar Description of current layout of mesh.
!> @param[out] f_send Description of where to send the data.

  subroutine golaps(tMPI, iso, ieo, f_ar, f_send)

    use typempicomm

! inputs

    type(fMPIComm), intent(in) :: tMPI
    integer(kind=ip), intent(in)  :: iso, ieo
    integer(kind=ip), intent(in)  :: f_ar(:,:)
    integer(kind=ip), intent(out) :: f_send(:,:)

! local args

    integer(kind=ip) :: iproc


!    print*, iso, ieo, size(f_send)

    do iproc = 0, tMPI%size-1

      f_send(iproc+1,1) = 0
      f_send(iproc+1,2) = 0
      f_send(iproc+1,3) = 0

      if (f_ar(iproc+1,1) > 0) then

        if ((ieo >= f_ar(iproc+1,2)) .and. (ieo <= f_ar(iproc+1,3) ) )  then ! end node between limits

          f_send(iproc+1,3) = ieo

          if (iso < f_ar(iproc+1,2)) f_send(iproc+1,2) = f_ar(iproc+1,2)    ! front node before first limit

          if (iso >= f_ar(iproc+1,2)) f_send(iproc+1,2) = iso   ! front node after first limit

          f_send(iproc+1,1) = f_send(iproc+1,3) - f_send(iproc+1,2) + 1

        else if ( (ieo >= f_ar(iproc+1,3)) .and.  (iso <= f_ar(iproc+1,3)))  then ! end node after last limit

          f_send(iproc+1, 3) = f_ar(iproc+1,3)

          if (iso < f_ar(iproc+1,2)) f_send(iproc+1,2) = f_ar(iproc+1,2)    ! front node before first limit

          if (iso >= f_ar(iproc+1,2)) f_send(iproc+1,2) = iso   ! front node after first limit

          f_send(iproc+1,1) = f_send(iproc+1,3) - f_send(iproc+1,2) + 1

        else      ! no overlap...

          f_send(iproc+1,1) = 0
          f_send(iproc+1,2) = 0
          f_send(iproc+1,3) = 0

        end if

      end if

    end do


  end subroutine golaps


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Subroutine to setup the 'buffer' region at the end of the parallel field section
!> on this process. This is calculated by estimating how much will be needed by 
!> the electrons currently on the process. By calculating p2, one may estimate 
!> the size of the domain required in z2 to hold the electron macroparticles over 
!> a distance dz through the undulator.
!> @param[in] tMPI Custom Fortran type to hold MPI info.
!> @param[in] dz Distance in zbar the mesh distribution is desired to be valid for.

  subroutine calcBuff(this, tMPI, dz)

    class(pMesh), intent(inout) :: this
    type(fMPIComm), intent(in) :: tMPI
    real(kind=wp), intent(in) :: dz

    real(kind=wp), allocatable :: sp2(:)
    real(kind=wp) :: bz2_len
    integer(kind=ip) :: yip, ij, bz2_globm, ctrecvs, cpolap, dum_recvs
    integer(kind=ip), allocatable :: drecar(:)
    integer :: error, req

    integer statr(MPI_STATUS_SIZE)
    integer sendstat(MPI_STATUS_SIZE)

    ! get buffer location

    if (iNumberElectrons_G > 0_ipl) then

      allocate(sp2(iNumberElectrons_G))

      call getP2(sp2, sElGam_G, sElPX_G, sElPY_G, sEta_G, sGammaR_G, sAw_G)

      bz2_len = dz  ! distance in zbar until next rearrangement
      bz2_len = maxval(sElZ2_G + bz2_len * sp2)  ! predicted length in z2 needed needed in buffer for beam

!    print*, 'bz2 length is...', bz2_len
!    print*, 'max p2 is ', maxval(sp2)

      deallocate(sp2)

    else

      bz2_len = (this%ez2 + 2_ip) * sLengthOfElmZ2_G  ! Just have 2 node boundary for no macroparticles

    end if

!    print*, tMPI%rank, 'is inside calcBuff, with buffer length', bz2_len

!    bz2 = ez2 + nint(4 * 4 * pi * sRho_G / sLengthOfElmZ2_G)   ! Boundary only 4 lambda_r long - so can only go ~ 3 periods

    this%bz2 = nint(bz2_len / sLengthOfElmZ2_G)  ! node index of final node in boundary

    if (this%bz2 > nz2_G) this%bz2 = nz2_G

! Find global bz2...

    call mpi_allreduce(this%bz2, bz2_globm, 1, mpi_integer, mpi_max, &
                    tMPI%comm, error)

    if (tMPI%rank == tMPI%size-1) then

      print*, this%bz2
      this%bz2 = bz2_globm

    else

      if (this%bz2 <= this%ez2) this%bz2 = this%ez2 + 1  ! For sparse beam!!

    end if

    if (.not. this%qUnique) then

      this%bz2 = bz2_globm
      this%ez2 = this%bz2
      this%mainlen = this%ez2 - this%fz2 + 1_ip
      this%fbufflen = 0
      this%tllen = this%mainlen

      allocate(this%lrank_v(1), this%rrank_v(1,1), this%lrfromwhere(1))

    else

      this%fbuffLen = this%bz2 - (this%ez2 + 1_ip) + 1_ip  ! Local buffer length, NOT including the ez2 node
      this%tllen = this%bz2 - this%fz2 + 1     ! local total length, including buffer





      if (tMPI%rank == tMPI%size-1_ip) then

        this%mainlen = this%tllen
        this%fbuffLen = 0
        this%ez2 = this%bz2

      end if



      call setupLayoutArrs(tMPI, this%mainlen, this%fz2, this%ez2, this%ac_ar)  ! readjust ac_ar with new ez2 for last process


      this%ez2_GGG = this%ac_ar(tMPI%size, 3)


      ! count overlap over how many processes....


  !!!   NOW NEED TO RECALC AC_AR TO TAKE INTO ACCOUNT POSSIBLY ADJUSTED
  !!!   BOUNDS ON LAST PROCESS....???

      cpolap = 0

      do ij = 0,tMPI%size-1

        if  ( (ij > tMPI%rank) .and. (this%bz2 >= this%ac_ar(ij+1, 2)) ) then

          cpolap = cpolap + 1_ip

        end if

      end do



      if (tMPI%rank /= tMPI%size-1) then
        allocate(this%rrank_v(cpolap, 3))
        !allocate(isnd2u(tMPI%size))
      else
        allocate(this%rrank_v(1, 3))
        this%rrank_v = 1
        !allocate(isnd2u(tMPI%size))
      end if

      yip = 0
      this%nsnds_bf = cpolap
      !isnd2u = 0




  !  Count how much I'm sending to each process I'm bounding over...

      if (tMPI%rank /= tMPI%size-1) then

        do ij = 0, tMPI%size - 1
  !print*, ij
          if  (   (ij > tMPI%rank) .and. (this%bz2 >= this%ac_ar(ij+1, 2)) ) then

            yip = yip + 1_ip

            rrank_v(yip,2) = this%ac_ar(ij+1, 2)

            if (this%bz2 > this%ac_ar(ij+1, 3)) then
              this%rrank_v(yip,3) = this%ac_ar(ij+1, 3)
            else
              this%rrank_v(yip,3) = this%bz2
            end if

            this%rrank_v(yip,1) = this%rrank_v(yip,3) - this%rrank_v(yip,2) + 1_ip

          end if

        end do



      !  send numbers I'm sending to the processes to let them know:

        yip = 0

        do ij = tMPI%rank + 1, tMPI%size-1

          yip = yip + 1

          if (ij - tMPI%rank <= cpolap) then

            !send rrank_v(yip, 1) to tMPI%rank + yip

            call mpi_issend(this%rrank_v(yip, 1), 1, mpi_integer, &
                     tMPI%rank + yip, 0, tMPI%comm, &
                     req, error)

          else

            !send 0 to tMPI%rank + yip
            call mpi_issend(0, 1, mpi_integer, &
                     tMPI%rank + yip, 0, tMPI%comm, &
                     req, error)

          end if

        end do

      end if




      allocate(drecar(tMPI%rank))
      ctrecvs = 0

      if (tMPI%rank /= 0) then

        do ij =  0, tMPI%rank - 1

          ! dum_recvs from ij

          call mpi_recv(dum_recvs, 1, mpi_integer, ij, &
                  0, tMPI%comm, statr, error)

          drecar(ij+1) = dum_recvs

          if (dum_recvs > 0) ctrecvs = ctrecvs + 1

        end do

      end if

      if (tMPI%rank /= tMPI%size-1) then
        call mpi_wait(req, sendstat, error)
      end if

      if (tMPI%rank /= 0) then

        allocate(this%lrank_v(ctrecvs))
        allocate(this%lrfromwhere(ctrecvs))

      else

        allocate(this%lrank_v(1))
        allocate(this%lrfromwhere(1))
        this%lrank_v = 1
        this%lrfromwhere = 1

      end if

      this%nrecvs_bf = ctrecvs

      if (tMPI%rank /= 0) then

        !nrecvs2me = count(drecar > 0)

        yip = 0

        do ij = 0, tMPI%rank - 1


          if (drecar(ij+1) > 0) then

            yip = yip+1
            this%lrank_v(yip) = drecar(ij+1)
            this%lrfromwhere(yip) = ij  ! rank recieving info from

          end if

        end do

      end if

    end if

  end subroutine calcBuff

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Retrieves the global start and end of the current 'active' portion of the 
!> field mesh, and the local start and end, excluding boundaries needed for
!> propagation. The global node indices are numbered from 1 -> number of nodes 
!> in z2. The transverse size of the mesh is not taken into account in this 
!> subroutine - the parallel mesh distribution is determined by temporal/longitudinal 
!> z2 coordinate only. This active portion of the mesh is determined either by 
!> the electron beam (so the active mesh is defined to contain the electron
!> beam), known as 'electron based', or it just defines the total field mesh as
!>  'active' - this is 'field based'. In each case, the field mesh is evenly 
!> divided across each process in the longitudinal/temporal z2 direction.
!> Furthermore, if the electron based parallelism is used, then if the 'active'
!> mesh is too small to be efficiently parallelised for the number of processes
!> used, then the FULL active mesh will be kept on each process. Note that this
!> subroutine does not actually physically send the field nodes to the MPI
!> processes - it calculates the mesh indices which should belong to each process.
!> It does not calculate the size or indices of the boundaries which will be 
!> required on each MPI process to allow a beam to be propagated. Nor does
!> this subroutine calculate the parallel distribution of the 'front'
!> and 'back' sections of the field mesh which surround the 'active' section.
!> @param[in] tMPI Custom Fortran type to hold MPI info.

  subroutine getFStEnd(self, tMPI)

    use typempicomm

    class(pMesh), intent(inout) :: self
    type(fMPIComm), intent(in) :: tMPI

    real(kind=wp), allocatable :: sp2(:)
    integer(kind=ip) :: fz2_act, ez2_act

    integer :: error
    integer(kind=ip) :: n_act_g
    integer(kind=ip) :: rbuff

! get global start and end nodes for the active region

! (find min and max electron z2's)

    if (self%iParaBas == iElectronBased) then

      fz2_act = minval(ceiling(sElZ2_G / sLengthOfElmZ2_G))   ! front z2 node in 'active' region

      CALL mpi_allreduce(fz2_act, rbuff, 1, mpi_integer, &
               mpi_min, tMPI%comm, error)

      fz2_act = rbuff
      self%fz2_GGG = fz2_act

      ez2_act = maxval(ceiling(sElZ2_G / sLengthOfElmZ2_G) + 1)

!      print*, tMPI%rank, 'and I have ez2_act = ', ez2_act

      CALL mpi_allreduce(ez2_act, rbuff, 1, mpi_integer, &
               mpi_max, tMPI%comm, error)

      ez2_act = rbuff
      self%ez2_GGG = ez2_act

!print*, 'ez2_act = ', ez2_act

    else if (iParaBas == iFieldBased) then    !    FIELD based - also used for initial steps...

      fz2_act = 1_ip
      ez2_act = NZ2_G
      self%fz2_GGG = 1_ip
      self%ez2_GGG = 1_ip

    else

      print*, 'NO BASIS FOR PARALLELISM SELECTED!!!'

    end if

    n_act_g = ez2_act - fz2_act + 1_ip

! get local start and end nodes for the active region

! (use divNodes)


    if (n_act_g < 2*tMPI%size) then ! If too many nodes

      self%qUnique = .false.

      print*, 'So WHY AM I HERE, WITH nz2 = ', nz2_G
      print*, 'n_act_g = ', n_act_g
      print*, 'fz2_act = ', fz2_act
      print*, 'ez2_act = ', ez2_act


      self%fz2 = self%fz2_GGG
      self%ez2 = self%ez2_GGG

      self%mainlen = n_act_g

    else

      self%qUnique = .true.

      call divNodes(n_act_g, tMPI%size, tMPI%rank, &
                    self%tllen, self%fz2, self%ez2)

      self%fz2 = self%fz2 + fz2_act - 1_ip
      self%ez2 = self%ez2 + fz2_act - 1_ip

      self%mainlen = self%ez2 - self%fz2 + 1_ip     ! local length, NOT including buffer

    end if

  end subroutine getFStEnd


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Get array indices of front and back nodes depending on active region nodes.
!> @param[in] tMPI Custom Fortran type to hold MPI info.

  subroutine getFrBk(self, tMPI)

    class(pMesh), intent(inout) :: self
    type(fMPIComm), intent(in) :: tMPI
    integer(kind=ip) :: efz2_MG, ebz2_MG
    integer :: error

      if (tMPI%qRoot) efz2_MG = self%fz2 - 1_ip

      call MPI_BCAST(efz2_MG, 1, mpi_integer, 0, &
                      tMPI%comm,error)



      if (efz2_MG < 1) then

!     then there is no front section of the field...

        self%tlflen_glob = 0
        self%tlflen = 0
        self%tlflen4arr = 1
        self%ffs = 0
        self%ffe = 0
        self%ffs_GGG = 0
        self%ffe_GGG = 0


      else if (efz2_MG > 0) then

        self%tlflen_glob = efz2_MG

        call divNodes(efz2_MG,tMPI%size, &
                      tMPI%rank, &
                      self%tlflen, self%ffs, self%ffe)

        self%tlflen4arr = self%tlflen

        self%ffs_GGG = 1
        self%ffe_GGG = self%efz2_MG


      end if


      CALL MPI_ALLGATHER(self%tlflen, 1, MPI_INTEGER, &
              self%ff_ar(:,1), 1, MPI_INTEGER, &
              tMPI%comm, error)

      CALL MPI_ALLGATHER(self%ffs, 1, MPI_INTEGER, &
              self%ff_ar(:,2), 1, MPI_INTEGER, &
              tMPI%comm, error)

      CALL MPI_ALLGATHER(self%ffe, 1, MPI_INTEGER, &
              ff_ar(:,3), 1, MPI_INTEGER, &
              tMPI%comm, error)




!    print*, 'FRONT ARRAY IS ', ff_ar




      ! get rightmost bz2 ...(last process)
      ! ebz2_MG - extreme back z2 node of active region plus 1

      if (tMPI%rank == tMPI%size-1) ebz2_MG = self%bz2 + 1


      call MPI_BCAST(ebz2_MG,1, mpi_integer, tMPI%size-1, &
                      tMPI%comm, error)



      if (ebz2_MG > NZ2_G) then

!     then there is no back section of the field...

        self%tlelen_glob = 0
        self%tlelen = 0
        self%tlelen4arr = 1
        self%ees = 0
        self%eee = 0

      else if (ebz2_MG < nz2_G + 1) then

        tlelen_glob = nz2_G - ebz2_MG + 1

!        print*, 'I get the tlelen_glob to be ', tlelen_glob

        call divNodes(tlelen_glob, tMPI%size, &
                      tMPI%rank, &
                      self%tlelen, self%ees, self%eee)

        self%ees = self%ees + ebz2_MG - 1
        self%eee = self%eee + ebz2_MG - 1

        self%ees_GGG = ebz2_MG
        self%eee_GGG = NZ2_G

        self%tlelen4arr = self%tlelen

!        print*, '...and the start nd end of the back to be', ees, eee

      end if

      CALL MPI_ALLGATHER(self%tlelen, 1, MPI_INTEGER, &
              self%ee_ar(:,1), 1, MPI_INTEGER, &
              tMPI%comm, error)

      CALL MPI_ALLGATHER(self%ees, 1, MPI_INTEGER, &
              self%ee_ar(:,2), 1, MPI_INTEGER, &
              tMPI%comm, error)

      CALL MPI_ALLGATHER(self%eee, 1, MPI_INTEGER, &
              self%ee_ar(:,3), 1, MPI_INTEGER, &
              tMPI%comm, error)

  end subroutine getFrBk


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Alternative subroutine to redistribute the field values in field_old to
!> field_new. The layout of the field in field_old is described in old_dist,
!> and the layout of the new field is described in new_dist. This subroutine
!> uses mpi_alltoallv, in contrast to redist2new, which uses mpi the sends and 
!> recvs. This routine is usually faster.
!> @param[in] tMPI Custom Fortran type to hold MPI info.
!> @param[in] old_dist Description of distribution of data of field_old across
!> MPI processes.
!> @param[in] old_dist Description of distribution of data of field_new across
!> MPI processes.
!> @param[in] field_old Field values are to be sent from this array
!> @param[in] field _new Field values are to be sent to this array

  subroutine redist2new2(tMPI, old_dist, new_dist, field_old, field_new)

    use typempicomm

! inputs

    type(fMPIComm), intent(in) :: tMPI
    integer(kind=ip), intent(in) :: old_dist(:,:), new_dist(:,:)
    real(kind=wp), intent(inout) :: field_old(:), field_new(:)

! local

    integer(kind=ip) :: iproc_s, iproc_r
    integer(kind=ip) :: st_ind_new, ed_ind_new, &
                        st_ind_old, ed_ind_old, &
                        nbase, obase
    integer(kind=ip), allocatable :: send_ptrs(:,:), recv_ptrs(:,:)

    integer(kind=ip), allocatable :: sdispls(:), rdispls(:)


    integer(kind=ip), allocatable :: nsends(:), nrecvs(:)
    integer :: error, req, ij
    integer statr(MPI_STATUS_SIZE)
    integer sendstat(MPI_STATUS_SIZE)



    allocate(send_ptrs(tMPI%size, 3))
    allocate(recv_ptrs(tMPI%size, 3))

    allocate(sdispls(tMPI%size))
    allocate(rdispls(tMPI%size))

    allocate(nsends(tMPI%size), nrecvs(tMPI%size))

    call golaps(tMPI, old_dist(tMPI%rank+1,2), &
                old_dist(tMPI%rank+1,3), &
                new_dist, send_ptrs)

    call golaps(tMPI, new_dist(tMPI%rank+1,2), &
                new_dist(tMPI%rank+1,3), &
                old_dist, recv_ptrs)



    nsends = send_ptrs(:,1) * ntrnds_G
    nrecvs = recv_ptrs(:,1) * ntrnds_G


    sdispls = ( (send_ptrs(:,2) - (old_dist(tMPI%rank+1, 2) - 1) ) - 1_ip) * ntrnds_G

    rdispls = ( (recv_ptrs(:,2) - (new_dist(tMPI%rank+1, 2) - 1) ) - 1_ip) * ntrnds_G

    do ij = 1, tMPI%size

      if (send_ptrs(ij,1) == 0) then

        if (ij-1 == 0) then

          sdispls(ij) = 0_ip

        else

          sdispls(ij) = sdispls(ij-1)! + nsends(ij-1)

        end if

      end if

    end do


    do ij = 1, tMPI%size

      if (recv_ptrs(ij,1) == 0) then

        if (ij-1 == 0) then

          rdispls(ij) = 0_ip

        else

          rdispls(ij) = rdispls(ij-1)! + nrecvs(ij-1)

        end if

      end if

    end do

    call mpi_alltoallv(field_old, nsends, sdispls, mpi_double_precision, &
                       field_new, nrecvs, rdispls, mpi_double_precision, &
                       tMPI%comm, error)


    deallocate(nsends, nrecvs)
    deallocate(send_ptrs, recv_ptrs)
    deallocate(sdispls, rdispls)

  end subroutine redist2new2

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Subroutine to redistribute the field values in field_old to field_new. The 
!> layout of the field in field_old is described in old_dist, and the layout of 
!> the new field is described in new_dist.
!> @param[in] tMPI Custom Fortran type to hold MPI info.
!> @param[in] old_dist Description of distribution of data of field_old across
!> MPI processes.
!> @param[in] old_dist Description of distribution of data of field_new across
!> MPI processes.
!> @param[in] field_old Field values are to be sent from this array
!> @param[in] field _new Field values are to be sent to this array

  subroutine redist2new(tMPI, old_dist, new_dist, field_old, field_new)

    use typempicomm

    ! inputs

    type(fMPIComm), intent(in) :: tMPI
    integer(kind=ip), intent(in) :: old_dist(:,:), new_dist(:,:)
    real(kind=wp), intent(inout) :: field_old(:), field_new(:)

    ! local

    integer(kind=ip) :: iproc_s, iproc_r
    integer(kind=ip) :: st_ind_new, ed_ind_new, &
                        st_ind_old, ed_ind_old, &
                        nbase, obase

    integer(kind=ip), allocatable :: send_ptrs(:,:)

    integer :: error, req
    integer statr(MPI_STATUS_SIZE)
    integer sendstat(MPI_STATUS_SIZE)

    allocate(send_ptrs(tMPI%size, 3))

    ! calc overlaps from MPI process 'iproc_s',
    ! then loop round, if size_olap>0 then if
    ! rank==iproc_s send, else if rank==iproc_r
    ! recv, unless iproc_r==iproc_s then just
    ! direct assignment


!    if (tMPI%qroot) print*, 'NOW, for new dist described by ', new_dist,'....', &
!                               ' and old dist of ', old_dist

    do iproc_s = 0, tMPI%size-1   !  maybe do iproc_s = rank, rank-1 (looped round....)

      call golaps(tMPI, old_dist(iproc_s+1,2), old_dist(iproc_s+1,3), new_dist, send_ptrs)

!      if (tMPI%qroot) print*, 'olaps are ', send_ptrs, 'for old nodes ', old_dist(iproc_s+1,2), &
!          'to', old_dist(iproc_s+1,3)

!      call mpi_barrier(tMPI%comm, error)

      do iproc_r = 0, tMPI%size-1

        if (send_ptrs(iproc_r+1,1) > 0 ) then

          if ((tMPI%rank == iproc_r) .and. (iproc_r == iproc_s) ) then

            ! assign directly

            obase = old_dist(iproc_r+1, 2) - 1
            nbase = new_dist(iproc_r+1, 2) - 1

            st_ind_new = send_ptrs(iproc_r+1, 2) - nbase
            st_ind_new = (st_ind_new - 1)* ntrnds_G + 1

            ed_ind_new = send_ptrs(iproc_r+1, 3) - nbase
            ed_ind_new = ed_ind_new * ntrnds_G

            st_ind_old = send_ptrs(iproc_r+1, 2) - obase
            st_ind_old = (st_ind_old - 1) * ntrnds_G + 1

            ed_ind_old = send_ptrs(iproc_r+1, 3) - obase
            ed_ind_old = ed_ind_old * ntrnds_G


!            print*, 'AD st_ind_old = ', st_ind_old, ed_ind_old
!            print*, 'AD st_ind_new = ', st_ind_new, ed_ind_new

            field_new(st_ind_new:ed_ind_new) = field_old(st_ind_old:ed_ind_old)

          else

            obase = old_dist(iproc_s+1, 2) - 1
            nbase = new_dist(iproc_r+1, 2) - 1

            st_ind_new = send_ptrs(iproc_r+1, 2) - nbase
            st_ind_new = (st_ind_new - 1)* ntrnds_G + 1

            ed_ind_new = send_ptrs(iproc_r+1, 3) - nbase
            ed_ind_new = ed_ind_new * ntrnds_G

            st_ind_old = send_ptrs(iproc_r+1, 2) - obase
            st_ind_old = (st_ind_old - 1) * ntrnds_G + 1

            ed_ind_old = send_ptrs(iproc_r+1, 3) - obase
            ed_ind_old = ed_ind_old * ntrnds_G


            if (tMPI%rank == iproc_s) then

!              print*, 'SD st_ind_old = ', st_ind_old, ed_ind_old, size(field_old), &
!              send_ptrs(iproc_r+1,1)

              call mpi_issend(field_old(st_ind_old:ed_ind_old), &
                          send_ptrs(iproc_r+1,1)*ntrnds_G, &
                          mpi_double_precision, iproc_r, 0, tMPI%comm, req, error)

!              print*, 'SENDING', field_old(st_ind_old:ed_ind_old)

            else if (tMPI%rank == iproc_r) then

!              print*, 'SD st_ind_new = ', st_ind_new, ed_ind_new, size(field_new), &
!                           send_ptrs(iproc_r+1,1)

              call mpi_recv(field_new(st_ind_new:ed_ind_new), &
                            send_ptrs(iproc_r+1,1)*ntrnds_G, &
                            mpi_double_precision, iproc_s, 0, tMPI%comm, statr, error)

!              print*, 'RECIEVED', field_new(st_ind_new:ed_ind_new)

            end if

            !if (tMPI%rank == iproc_s) call mpi_wait( req,sendstat,error )
  !          call mpi_barrier(tMPI%comm, error)

   !         call mpi_finalize(error)
   !         stop


          end if

        end if

      end do

!    if (tMPI%rank == iproc_s) call mpi_wait( req,sendstat,error )

    end do

    deallocate(send_ptrs)

    call mpi_barrier(tMPI%comm, error)

   end subroutine redist2new



!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Subroutine to setup the arrays storing the size, start and end pointers of 
!> the local field arrays.
!> @param[in] tMPI Custom Fortran type to hold MPI info.
!> @param[inout] len Length of the local portion of the mesh.
!> @param[inout] st_ind Starting index (global) of the mesh on THIS MPI process.
!> @param[inout] ed_ind Ending index (global) of the mesh on THIS MPI process.
!> @param[inout] arr Array to describe the structure of the parallel layout.

  subroutine setupLayoutArrs(tMPI, len, st_ind, ed_ind, arr)

    use typempicomm

    type(fMPIComm), intent(in) :: tMPI
    integer(kind=ip), intent(inout) :: len, st_ind, ed_ind, arr(:,:)
    integer :: error


    CALL MPI_ALLGATHER(len, 1, MPI_INTEGER, &
            arr(:,1), 1, MPI_INTEGER, &
            tProcInfo_G%comm, error)

    CALL MPI_ALLGATHER(st_ind, 1, MPI_INTEGER, &
            arr(:,2), 1, MPI_INTEGER, &
            tProcInfo_G%comm, error)

    CALL MPI_ALLGATHER(ed_ind, 1, MPI_INTEGER, &
            arr(:,3), 1, MPI_INTEGER, &
            tProcInfo_G%comm, error)

  end subroutine setupLayoutArrs

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Rearranges electron macroparticles according to radiation mesh paralle layout.
!> @param[in] tMPI Custom Fortran type to hold MPI info.

  subroutine rearrElecs(self, tMPI)

  use typempicomm

  class(pMesh), intent(inout) :: self
  type(fMPIComm), intent(in) :: tMPI

  integer :: error
  integer(kind=ip) :: iproc, iproc_r, iproc_s
  integer(kind=ip), allocatable :: cnt2proc(:), &
                                   inds4sending(:)
  real(kind=wp), allocatable :: sElZ2_OLD(:), sElX_OLD(:), &
                                sElY_OLD(:), sElGam_OLD(:), &
                                sElPX_OLD(:), sElPY_OLD(:), &
                                s_chi_bar_OLD(:), &
                                s_Normalised_chi_OLD(:), &
                                tmp4sending(:)

  integer(kind=ip) :: icds, new_sum, offe, offs, frmroot


    ! p_nodes type calc from jRHS...

    ! send_count = count(p_nodes == rank)
    ! where(p_nodes == rank)

! %%%%%%%%%%%%%%%%%%%
    ! so get p_nodes_loc

    allocate(cnt2proc(tProcInfo_G%size))
    cnt2proc = 0

!    do imp = 1,iNumberElectrons_G

!      where ( (sElZ2_G(imp) >= (sLengthOfElmZ2_G * (ac_ar(:,2)-1))) .and. &
!                  (sElZ2_G(imp) >= (sLengthOfElmZ2_G * (ac_ar(:,3)-1) )) )

!        cnt2proc = cnt2proc + 1

!      end where

!    end do

! OR
    do iproc = 0, tProcInfo_G%size-1

      icds = count(sElZ2_G > (sLengthOfElmZ2_G * (self%ac_ar(iproc+1, 2)-1)) .and. &
                    (sElZ2_G <= (sLengthOfElmZ2_G * self%ac_ar(iproc+1, 3))) )

      cnt2proc(iproc+1) = icds   ! amount I'm sending to iproc

      call mpi_reduce(cnt2proc(iproc+1), new_sum, 1, &
                     mpi_integer, mpi_sum, iproc, tProcInfo_G%comm, error)

    end do


    call mpi_barrier(tProcInfo_G%comm, error)

!    print*, 'cnt2proc = ', cnt2proc


!    call mpi_finalize(error)
!    stop

!    do iproc = 0, tProcInfo_G%size-1

      ! mpi sum to iproc total coming to it
      ! cnt2proc summed to new_sum

!    end do

    allocate(sElZ2_OLD(iNumberElectrons_G))
    sElZ2_OLD = sElZ2_G

    deallocate(sElZ2_G)

    allocate(sElZ2_G(new_sum))



    allocate(sElGam_OLD(iNumberElectrons_G))
    sElGam_OLD = sElGam_G

    deallocate(sElGam_G)

    allocate(sElGam_G(new_sum))



    allocate(sElX_OLD(iNumberElectrons_G))
    sElX_OLD = sElX_G

    deallocate(sElX_G)

    allocate(sElX_G(new_sum))



    allocate(sElY_OLD(iNumberElectrons_G))
    sElY_OLD = sElY_G

    deallocate(sElY_G)

    allocate(sElY_G(new_sum))



    allocate(sElPX_OLD(iNumberElectrons_G))
    sElPX_OLD = sElPX_G

    deallocate(sElPX_G)

    allocate(sElPX_G(new_sum))



    allocate(sElPY_OLD(iNumberElectrons_G))
    sElPY_OLD = sElPY_G

    deallocate(sElPY_G)

    allocate(sElPY_G(new_sum))



    allocate(s_chi_bar_OLD(iNumberElectrons_G))
    s_chi_bar_OLD = s_chi_bar_G

    deallocate(s_chi_bar_G)

    allocate(s_chi_bar_G(new_sum))



    iNumberElectrons_G = new_sum
!    print*, 'new num elecs is ', new_sum

!    allocate(frmroot(tProcInfo_G%size))
    allocate(tmp4sending(maxval(cnt2proc)))
    allocate(inds4sending(maxval(cnt2proc)))

    inds4sending = 0
    tmp4sending = 0

    offs=0
    offe=0

    do iproc_s = 0, tProcInfo_G%size-1

    ! mpi scatter from iproc_s to others

      frmroot = 0

      call mpi_scatter(cnt2proc, 1, mpi_integer, frmroot, &
                       1, mpi_integer, iproc_s, tProcInfo_G%comm, &
                       error)

        ! mpi_send electrons - z2, gamma, etc


    call mpi_barrier(tProcInfo_G%comm, error)



      if (tProcInfo_G%rank == iproc_s) then

        do iproc_r = 0, tProcInfo_G%size-1


          if (cnt2proc(iproc_r+1) > 0) then

            if (iproc_r == iproc_s) then

              ! Assign locally

              offs = offe + 1
              offe = offs + cnt2proc(iproc_r+1) - 1

              call getinds(inds4sending(1:cnt2proc(iproc_r+1)), &
                    sElZ2_OLD, &
                    (sLengthOfElmZ2_G * (self%ac_ar(iproc_r+1,2)-1)), &
                    (sLengthOfElmZ2_G * self%ac_ar(iproc_r+1,3)) )

              !print*, inds4sending(1:cnt2proc(iproc_r+1))
              !print*, 'cnt2proc again:', cnt2proc(iproc_r+1)

              sElZ2_G(offs:offe) = sElZ2_OLD(inds4sending(1:cnt2proc(iproc_r+1)))
              sElGam_G(offs:offe) = sElGam_OLD(inds4sending(1:cnt2proc(iproc_r+1)))
              sElPX_G(offs:offe) = sElPX_OLD(inds4sending(1:cnt2proc(iproc_r+1)))
              sElPY_G(offs:offe) = sElPY_OLD(inds4sending(1:cnt2proc(iproc_r+1)))
              sElX_G(offs:offe) = sElX_OLD(inds4sending(1:cnt2proc(iproc_r+1)))
              sElY_G(offs:offe) = sElY_OLD(inds4sending(1:cnt2proc(iproc_r+1)))
              s_chi_bar_G(offs:offe) = s_chi_bar_OLD(inds4sending(1: &
                                                   cnt2proc(iproc_r+1)))


            else

              ! SEND to iproc_r

              call getinds(inds4sending(1:cnt2proc(iproc_r+1)), &
                    sElZ2_OLD, &
                    (sLengthOfElmZ2_G * (self%ac_ar(iproc_r+1,2)-1) ), &
                    (sLengthOfElmZ2_G * self%ac_ar(iproc_r+1,3)) )

!              tmp4sending(1:cnt2proc(iproc_r+1)) = sElZ2_OLD((1:cnt2proc(iproc_r+1)))

!              call mpi_issend(tmp4sending(1:cnt2proc(iproc_r+1)), cnt2proc(iproc_r+1), mpi_real, &
!                               iproc_r, tProcInfo_G%comm, req, error)

              call sendArrPart(sElZ2_OLD, inds4sending(1:cnt2proc(iproc_r+1)), &
                               cnt2proc(iproc_r+1), &
                               tmp4sending, iproc_r)

              call sendArrPart(sElGam_OLD, inds4sending(1:cnt2proc(iproc_r+1)), &
                               cnt2proc(iproc_r+1), &
                               tmp4sending, iproc_r)

              call sendArrPart(sElPX_OLD, inds4sending(1:cnt2proc(iproc_r+1)), &
                               cnt2proc(iproc_r+1), &
                               tmp4sending, iproc_r)

              call sendArrPart(sElPY_OLD, inds4sending(1:cnt2proc(iproc_r+1)), &
                               cnt2proc(iproc_r+1), &
                               tmp4sending, iproc_r)

              call sendArrPart(sElX_OLD, inds4sending(1:cnt2proc(iproc_r+1)), &
                               cnt2proc(iproc_r+1), &
                               tmp4sending, iproc_r)

              call sendArrPart(sElY_OLD, inds4sending(1:cnt2proc(iproc_r+1)), &
                               cnt2proc(iproc_r+1), &
                               tmp4sending, iproc_r)

              call sendArrPart(s_chi_bar_OLD, inds4sending(1:cnt2proc(iproc_r+1)), &
                               cnt2proc(iproc_r+1), &
                               tmp4sending, iproc_r)

            end if

          end if

        end do


      else

        if (frmroot>0) then


          offs = offe + 1
          offe = offs + frmroot - 1


          call recvArrPart(sElZ2_G, frmroot, &
                           offs, offe, iproc_s)

          call recvArrPart(sElGam_G, frmroot, &
                           offs, offe, iproc_s)

          call recvArrPart(sElPX_G, frmroot, &
                           offs, offe, iproc_s)

          call recvArrPart(sElPY_G, frmroot, &
                           offs, offe, iproc_s)

          call recvArrPart(sElX_G, frmroot, &
                           offs, offe, iproc_s)

          call recvArrPart(sElY_G, frmroot, &
                           offs, offe, iproc_s)

          call recvArrPart(s_chi_bar_G, frmroot, &
                           offs, offe, iproc_s)

        end if

      end if

    end do

! count send_to_each_process

! loop over processes

! call mpi_scatter(root=iproc, send_to_each_process, to recv_buff)

! if recv_buff>0 call mpi_recv(from iproc)

! loop lor_proc over processes
! if send_to_each_process(lor_proc) > 0  call mpi_send(to lor_proc)

    deallocate(cnt2proc)
    deallocate(tmp4sending)
    deallocate(inds4sending)
!    deallocate(frmroot)



    deallocate(sElZ2_OLD, sElX_OLD, &
               sElY_OLD, sElGam_OLD, &
               sElPX_OLD, sElPY_OLD, &
               s_chi_bar_OLD)



  end subroutine rearrElecs




!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Subroutine to return the indices of elements in the array
!> which lie between the upper and lower bounds specified.
!> @param[out] inds Indices of 'array' which contains values which lie within
!> the specified range.
!> @param[in] array The array of values to be queried.
!> @param[in] lower The lower bound (non-inclusive) of the range.
!> @param[in] lower The upper bound (inclusive) of the range.

  subroutine getinds(inds, array, lower, upper)

    integer(kind=ip), intent(out) :: inds(:)
    real(kind=wp), intent(in) :: array(:)
    real(kind=wp), intent(in) :: lower, upper

    integer(kind=ip) :: nx, ij, co

    nx = size(array)
    co = 0

    do ij = 1, nx

      if ((array(ij) > lower) .and. (array(ij) <= upper) ) then
        co = co+1
        inds(co) = ij
      end if

    end do

  end subroutine getinds


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Subroutine to send part of an array specified by the indices in 'inds', from
!> this MPI process to destination MPI process iproc.
!> @param[in] array The array to be sent.
!> @param[in] inds The indices of 'array' to be sent.
!> @param[in] cnt Total number of indices to be sent from 'array'
!> @param[in] tmparray Temp/scratch array to hold the selected indices from 
!> 'array' in during sending. Should be at least of size 'cnt'
!> @param[inout] iproc MPI process to send data to

  subroutine sendArrPart(array, inds, cnt, tmparray, iproc)

    real(kind=wp), intent(in) :: array(:)
    real(kind=wp), intent(inout) :: tmparray(:)
    integer(kind=ip), intent(in) :: inds(:)
    integer(kind=ip), intent(in) :: cnt
    integer(kind=ip), intent(inout) :: iproc

    integer :: req, error
    integer :: sendstat(MPI_STATUS_SIZE)

    tmparray(1:cnt) = array(inds)

    call mpi_issend(tmparray(1:cnt), cnt, mpi_double_precision, &
                   iproc, 0, tProcInfo_G%comm, req, error)

    call mpi_wait( req,sendstat,error )

  end subroutine sendArrPart

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Subroutine to recieve an array of reals (double precision) from another MPI
!> process, to a contiguous location specified by 'st_ind' and 'ed_ind' in a 
!> possibly larger array.
!> this MPI process to destination MPI process iproc.
!> @param[inout] array The array which the recieved information will be placed.
!> @param[inout] cnt The size of the array being recieved.
!> @param[inout] iproc MPI process the info is coming from.
!> @param[in] st_ind Starting index of 'array' into which the info is placed
!> @param[in] ed_ind Last index of 'array' into which info is placed

  subroutine recvArrPart(array, cnt, st_ind, ed_ind, iproc)

    real(kind=wp), intent(inout) :: array(:)
    integer(kind=ip), intent(inout) :: cnt, iproc
    integer(kind=ip), intent(in) :: st_ind, ed_ind

    integer :: error
    integer statr(MPI_STATUS_SIZE)


    call mpi_recv(array(st_ind:ed_ind), cnt, &
                  mpi_double_precision, iproc, &
                  0, tProcInfo_G%comm, statr, error)

  end subroutine recvArrPart


!!!!~#####################################################################################



  subroutine redist2FFTWlt()

    implicit none

    integer(kind=ip) :: tmpfz2, tmpez2, tmpmainlen, &
                        tmpbz2, tmptllen, tmpfz2_act, &
                        tmpez2_act






    tmpfz2 = tTransInfo_G%loc_z2_start + 1
    tmpez2 = tTransInfo_G%loc_z2_start + &
              tTransInfo_G%loc_nz2

    tmpmainlen = tTransInfo_G%loc_nz2
    tmpbz2 = tmpez2
    tmptllen = tmpmainlen
    tmpfz2_act = 1
    tmpez2_act = nz2_G


    allocate(tre_fft(tmpmainlen*ntrnds_G), &
             tim_fft(tmpmainlen*ntrnds_G))


    tre_fft = 0_wp
    tim_fft = 0_wp


    allocate(ft_ar(tProcInfo_G%size, 3))
    call setupLayoutArrs(tmpmainlen, tmpfz2, tmpez2, ft_ar)

!    print*, 'fft array layout is ', ft_ar


    call redist2new2(ff_ar, ft_ar, fr_rfield, tre_fft)
    call redist2new2(ff_ar, ft_ar, fr_ifield, tim_fft)


    call redist2new2(ee_ar, ft_ar, bk_rfield, tre_fft)
    call redist2new2(ee_ar, ft_ar, bk_ifield, tim_fft)



    call redist2new2(ac_ar, ft_ar, ac_rfield, tre_fft)
    call redist2new2(ac_ar, ft_ar, ac_ifield, tim_fft)


  end subroutine redist2FFTWlt






  subroutine redistbackFFT()


    implicit none

    call redist2new2(ft_ar, ff_ar, tre_fft, fr_rfield)
    call redist2new2(ft_ar, ff_ar, tim_fft, fr_ifield)


    call redist2new2(ft_ar, ee_ar, tre_fft, bk_rfield)
    call redist2new2(ft_ar, ee_ar, tim_fft, bk_ifield)



    call redist2new2(ft_ar, ac_ar, tre_fft, ac_rfield)
    call redist2new2(ft_ar, ac_ar, tim_fft, ac_ifield)


    deallocate(tre_fft, tim_fft)
    deallocate(ft_ar)



  end subroutine redistbackFFT


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Sync the Power calculated from each process to one array. Each process
!> will have calc'd its own back, front, and active arrays.
!> @param[in] tProcInfo Custom type to hold MPI info.
!> @param[in] fpow Power in the local processes front mesh section
!> @param[in] apow Power in the local processes active mesh section
!> @param[in] bpow Power in the local processes back mesh section
!> @param[inout] gpow Power gathered to one array here, on the root process.
!> @param[out] qOK Error flag for Puffin.

    subroutine UpdateGlobalPow(self, tProcInfo, fpow, apow, bpow, gpow)

      class(pMesh), intent(in) :: self
      real(kind=wp), intent(in) :: fpow(:), apow(:), bpow(:)
      real(kind=wp), intent(inout) :: gpow(:)
      type(fMPIComm), intent(out)	   :: tProcInfo

      integer(kind=ip) :: gath_v

      real(kind=wp), allocatable :: A_local(:), powi(:)

      integer error

!          Sync power from front field

      gpow=0.0_wp



      if (self%ffe_GGG > 0) then

        if (tProcInfo%rank /= tProcInfo%size-1) then
          gath_v = self%tlflen  !-1
        else
          gath_v = self%tlflen
        end if




        allocate(A_local(gath_v))

        A_local = 0_wp

        A_local(1:gath_v) = fpow(1:gath_v)
        !A_local(gath_v+1:gath_v*2) = fr_ifield(1:gath_v)

        call gather1A(A_local, gpow(self%ffs_GGG:self%ffe_GGG), &
                gath_v, self%ffe_GGG - self%ffs_GGG + 1, &
                  self%recvs_fpf, self%displs_fpf)

        deallocate(A_local)

      end if



!          Sync power from active field

      if (tProcInfo%rank /= tProcInfo%size-1) then
        gath_v = self%mainlen !-1
      else
        gath_v = self%mainlen
      end if

      allocate(A_local(gath_v))
      allocate(powi(self%ez2_GGG - self%fz2_GGG + 1_ip))

      A_local = 0_wp
      powi = 0_wp

      A_local(1:gath_v) = apow(1:gath_v)

      if (self%qUnique) then 
        
        call gather1A(A_local, powi, &
                      gath_v, self%fz2_GGG - self%ez2_GGG + 1_ip, &
                      self%recvs_ppf, self%displs_ppf)

        gpow(fz2_GGG:ez2_GGG) = powi(:)

      else
        
        gpow(self%fz2_GGG:self%ez2_GGG) = apow(1:gath_v)
        
      end if
        
      deallocate(A_local)
      deallocate(powi)



!          Sync power from back field

      if (eee_GGG < nz2_G+1_ip) then

        if (tProcInfo%rank /= tProcInfo%size-1_ip) then
          gath_v = self%tlelen !-1
        else
          gath_v = self%tlelen
        end if

        allocate(A_local(gath_v))
        allocate(powi(self%eee_GGG - self%ees_GGG + 1_ip))

        A_local = 0_wp
        powi = 0_wp

        A_local(1:gath_v) = bpow(1:gath_v)

        call gather1A(A_local, powi, &
                      gath_v, self%eee_GGG - self%ees_GGG + 1_ip, &
                      self%recvs_epf, self%displs_epf)

        gpow(self%ees_GGG:self%eee_GGG) = powi(:)

        deallocate(A_local)
        deallocate(powi)

      end if

    end subroutine UpdateGlobalPow


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Send dadz from buffer to MPI process on the right. Data is added to array in 
!> next process, not written over.
!> @param[in] tMPI Custom Fortran type to hold MPI info.
!> @param[in] dadz_r Real part of dadz
!> @param[in] dadz_i Imaginary part of dadz

    subroutine upd8da(tMPI, dadz_r, dadz_i)

      real(kind=wp), contiguous, intent(inout) :: dadz_r(:), dadz_i(:)

      integer :: req, error
      integer(kind=ip) :: ij, si, sst, sse
      integer statr(MPI_STATUS_SIZE)
      integer sendstat(MPI_STATUS_SIZE)


      if (qUnique) then



        tmp_A = 0_wp
        !snd_A = 0_wp


   !           si = rrank_v(1, 1)
   !         sst = rrank_v(1, 2)
   !         sse = rrank_v(1, 3)
   !         call mpi_barrier(tProcInfo_G%comm, error)
   !         print*, tProcInfo_G%rank, 'here, sending nodes :', (sst - (fz2-1)-1)*ntrnds_G + 1, &
   !                       'to', (sse-(fz2-1))*ntrnds_G , 'to rank', tProcInfo_G%rank+1

        if (tProcInfo_G%rank /= tProcInfo_G%size-1) then

  !        send to rank+1

          !do ij = tProcInfo_G%rank + 1, tProcInfo_G%size-1
           do ij = 1, nsnds_bf

            si = rrank_v(ij, 1)
            sst = rrank_v(ij, 2)
            sse = rrank_v(ij, 3)


  !          call mpi_issend(dadz_r((ez2+1)-(fz2-1) + ofst :bz2-(fz2-1)), si, &
  !                    mpi_double_precision, &
  !                    tProcInfo_G%rank+1, 0, tProcInfo_G%comm, req, error)


!            print*, tProcInfo_G%rank, 'here, sending nodes :', (sst - (fz2-1)-1)*ntrnds_G + 1, &
!                          'to', (sse-(fz2-1))*ntrnds_G , 'to rank', tProcInfo_G%rank+ij, &
!                          ' and size dadz_r = ', size(dadz_r), ' and si =  ', si

            sst = (sst - (fz2-1)-1)*ntrndsi_G + 1
            sse = (sse-(fz2-1))*ntrndsi_G
            si = si*ntrndsi_G


            call mpi_issend(dadz_r(sst:sse), &
                      si, &
                      mpi_double_precision, &
                      tProcInfo_G%rank+ij, 0, tProcInfo_G%comm, req, error)


          end do


        end if

    !          call mpi_barrier(tProcInfo_G%comm, error)
     !         print*, tProcInfo_G%rank, 'here, recving nodes :', 1, &
      !                    'to', lrank_v(1)*ntrndsi_G , 'from rank', lrfromwhere(1)


        if (tProcInfo_G%rank /= 0) then

  !       rec from rank-1

          do ij = 1, nrecvs_bf


!              print*, tProcInfo_G%rank, 'here, recving nodes :', 1, &
!                          'to', lrank_v(ij)*ntrndsi_G , 'from rank', lrfromwhere(ij), &
!                          ' and size dadz_r = ', size(dadz_r)

            CALL mpi_recv( tmp_A(1:lrank_v(ij)*ntrndsi_G), &
                   lrank_v(ij)*ntrndsi_G, &
                   mpi_double_precision, &
            	     lrfromwhere(ij), 0, tProcInfo_G%comm, statr, error )

            dadz_r(1:lrank_v(ij)*ntrndsi_G) = dadz_r(1:lrank_v(ij)*ntrndsi_G) &
                                           + tmp_A(1:lrank_v(ij)*ntrndsi_G)

          end do

        end if

        if (tProcInfo_G%rank /= tProcInfo_G%size-1) call mpi_wait( req,sendstat,error )







        tmp_A = 0_wp

        if (tProcInfo_G%rank /= tProcInfo_G%size-1) then

  !        send to rank+1

          ! do ij = tProcInfo_G%rank + 1, tProcInfo_G%size-1
          do ij = 1, nsnds_bf

            si = rrank_v(ij, 1)
            sst = rrank_v(ij, 2)
            sse = rrank_v(ij, 3)


            sst = (sst - (fz2-1)-1)*ntrndsi_G + 1
            sse = (sse-(fz2-1))*ntrndsi_G
            si = si*ntrndsi_G

  !          call mpi_issend(dadz_r((ez2+1)-(fz2-1) + ofst :bz2-(fz2-1)), si, &
  !                    mpi_double_precision, &
  !                    tProcInfo_G%rank+1, 0, tProcInfo_G%comm, req, error)

            call mpi_issend(dadz_i(sst:sse), &
                      si, &
                      mpi_double_precision, &
                      tProcInfo_G%rank+ij, 0, tProcInfo_G%comm, req, error)


          end do

        end if



        if (tProcInfo_G%rank /= 0) then

  !       rec from rank-1

          do ij = 1, nrecvs_bf

            CALL mpi_recv( tmp_A(1:lrank_v(ij)*ntrndsi_G), &
                   lrank_v(ij)*ntrndsi_G, &
                   mpi_double_precision, &
                   lrfromwhere(ij), 0, tProcInfo_G%comm, statr, error )

            dadz_i(1:lrank_v(ij)*ntrndsi_G) = dadz_i(1:lrank_v(ij)*ntrndsi_G) &
                                           + tmp_A(1:lrank_v(ij)*ntrndsi_G)

          end do

        end if


        if (tProcInfo_G%rank /= tProcInfo_G%size-1) call mpi_wait( req,sendstat,error )

        !dadz_i(1:fbuffLenM) = dadz_i(1:fbuffLenM) + tmp_A


      else


        call mpi_reduce(dadz_r, tmp_A, mainlen*ntrndsi_G, &
                        mpi_double_precision, &
                        mpi_sum, 0, tProcInfo_G%comm, &
                        error)

        dadz_r = tmp_A


        call mpi_reduce(dadz_i, tmp_A, mainlen*ntrndsi_G, &
                        mpi_double_precision, &
                        mpi_sum, 0, tProcInfo_G%comm, &
                        error)

        dadz_i = tmp_A

      end if

    end subroutine upd8da





!  ###################################################





    subroutine upd8a(ac_rl, ac_il)

      implicit none

    ! Send sA from buffer to process on the left
    ! Data in 'buffer' on the left is overwritten.

      real(kind=wp), contiguous, intent(inout) :: ac_rl(:), ac_il(:)

      integer(kind=ip) :: req, error, ij, si, sst, sse
      integer statr(MPI_STATUS_SIZE)
      integer sendstat(MPI_STATUS_SIZE)

!      real(kind=wp), allocatable :: tstf(:), tstf2(:)



      if (qUnique) then


        if (tProcInfo_G%rank /= 0) then

  !       rec from rank-1

          do ij = 1, nrecvs_bf

            CALL mpi_issend( ac_rl(1:lrank_v(ij)*ntrndsi_G), &
                   lrank_v(ij)*ntrndsi_G, &
                   mpi_double_precision, &
                   lrfromwhere(ij), 0, tProcInfo_G%comm, req, error )

          end do

        end if







        if (tProcInfo_G%rank /= tProcInfo_G%size-1) then

  !        send to rank+1

          !do ij = tProcInfo_G%rank + 1, tProcInfo_G%size-1
           do ij = 1, nsnds_bf

            si = rrank_v(ij, 1)
            sst = rrank_v(ij, 2)
            sse = rrank_v(ij, 3)

            sst = (sst - (fz2-1)-1)*ntrndsi_G + 1
            sse = (sse-(fz2-1))*ntrndsi_G
            si = si*ntrndsi_G

  !          call mpi_issend(dadz_r((ez2+1)-(fz2-1) + ofst :bz2-(fz2-1)), si, &
  !                    mpi_double_precision, &
  !                    tProcInfo_G%rank+1, 0, tProcInfo_G%comm, req, error)

            call mpi_recv(ac_rl(sst:sse), &
                      si, &
                      mpi_double_precision, &
                      tProcInfo_G%rank+ij, 0, tProcInfo_G%comm, statr, error)


          end do


        end if




        if (tProcInfo_G%rank /= 0) then

  !       rec from rank-1

          do ij = 1, nrecvs_bf

            CALL mpi_issend( ac_il(1:lrank_v(ij)*ntrndsi_G), &
                   lrank_v(ij)*ntrndsi_G, &
                   mpi_double_precision, &
                   lrfromwhere(ij), 0, tProcInfo_G%comm, req, error )

          end do

        end if







        if (tProcInfo_G%rank /= tProcInfo_G%size-1) then

  !        send to rank+1

          !do ij = tProcInfo_G%rank + 1, tProcInfo_G%size-1
           do ij = 1, nsnds_bf

            si = rrank_v(ij, 1)
            sst = rrank_v(ij, 2)
            sse = rrank_v(ij, 3)

            sst = (sst - (fz2-1)-1)*ntrndsi_G + 1
            sse = (sse-(fz2-1))*ntrndsi_G
            si = si*ntrndsi_G

  !          call mpi_issend(dadz_r((ez2+1)-(fz2-1) + ofst :bz2-(fz2-1)), si, &
  !                    mpi_double_precision, &
  !                    tProcInfo_G%rank+1, 0, tProcInfo_G%comm, req, error)

            call mpi_recv(ac_il( sst:sse ), &
                      si, &
                      mpi_double_precision, &
                      tProcInfo_G%rank+ij, 0, tProcInfo_G%comm, statr, error)


          end do


        end if


        if (tProcInfo_G%rank /= 0) call mpi_wait( req,sendstat,error )



      else


        call MPI_Bcast(ac_rl, tllen*ntrndsi_G, &
                       mpi_double_precision, 0, &
                       tProcInfo_G%comm, error)

        call MPI_Bcast(ac_il, tllen*ntrndsi_G, &
                       mpi_double_precision, 0, &
                       tProcInfo_G%comm, error)


      end if

    end subroutine upd8a




!      allocate(sp2(size(sElGam_G)))

!      call getP2(sp2, sElGam_G, sElPX_G, sElPY_G, sEta_G, sGammaR_G, saw_G)

!      lTr = maxval(sElZ2_G + sp2)

!     bz2 = ez2 + nint(4 * 4 * pi * sRho_G / sLengthOfElmZ2_G)   ! Boundary only 4 lambda_r long - so can only go ~ 3 periods



!     print*, 'INSIDE GETLOCALFIELDINDICES, SIZE OF SA AT 1 IS ', size(sA)



! !     Communicate starting node index of process to the left, as we
! !     need these to match up with the left processes bz2...

!     if (tProcInfo_G%rank /= 0) then

! !        send to rank-1

!       print*, 'sending fz2 of ', fz2
!       call mpi_issend(fz2, 1, mpi_integer, tProcInfo_G%rank-1, 0, &
!            tProcInfo_G%comm, req, error)


!     end if


!     if (tProcInfo_G%rank /= tProcInfo_G%size-1) then

! !       rec from rank+1

!       CALL mpi_recv( fz2_r, 1, mpi_long, tProcInfo_G%rank+1, 0, &
!         tProcInfo_G%comm, statr, error )

!     end if

!     if (tProcInfo_G%rank /= 0) call mpi_wait( req,sendstat,error )
!     if (tProcInfo_G%rank==0)  print*, 'received fz2_r of ', fz2_r


!     print*, 'INSIDE GETLOCALFIELDINDICES, SIZE OF SA AT 2 IS ', size(sA), &
!       ' FOR PROCESSOR ', tProcInfo_G%rank


!     if (tProcInfo_G%rank == 0) fz2 = 1

! !     Last process contains full rest of the field, for now...

!     if (tProcInfo_G%rank == tProcInfo_G%size-1) bz2 = size(sA) / 2
!     if (tProcInfo_G%rank == tProcInfo_G%size-1) ez2 = bz2





!     if (tProcInfo_G%rank /= tProcInfo_G%size-1) then

!     if (fz2_r /= ez2) then

!       print*, 'WARNING, fixing parallel field bounds... on proc', tProcInfo_G%rank, &
!               ' ez2 was ', ez2, ' is now ', fz2_r
!       ez2 = fz2_r

!     end if

!     end if

!     fbuffLen = bz2 - ez2 + 1  ! Local buffer length, including the ez2 node
!     tllen = bz2 - fz2 + 1     ! local total length, including buffer
!     mainlen = ez2 - fz2 + 1     ! local length, NOT including buffer




!     print*, 'INSIDE GETLOCALFIELDINDICES, SIZE OF SA AT 3 IS ', size(sA), &
!         ' FOR PROCESSOR ', tProcInfo_G%rank


! !     Send buffer length to process on the right - as the right process
! !     will be updating out local 'buffer' region

!     fbuffLenM = 1

!     if (tProcInfo_G%rank /= tProcInfo_G%size-1) then

! !        send to rank+1

!       call mpi_issend(fbuffLen, 1, mpi_integer, tProcInfo_G%rank+1, 0, tProcInfo_G%comm, req, error)

!     end if

!     if (tProcInfo_G%rank /= 0) then

! !       rec from rank-1

!       CALL mpi_recv( fbuffLenM,1,MPI_INTEGER,tProcInfo_G%rank-1,0,tProcInfo_G%comm,statr,error )

!     end if

!     if (tProcInfo_G%rank /= tProcInfo_G%size-1) call mpi_wait( req,sendstat,error )




!     print*, 'INSIDE GETLOCALFIELDINDICES, SIZE OF SA AT 4 IS ', size(sA), &
!             ' FOR PROCESSOR ', tProcInfo_G%rank








end module ParaField
