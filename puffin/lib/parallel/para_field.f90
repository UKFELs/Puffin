! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Module controlling the field mesh parallelism in Puffin

module ParaField

use puffin_kinds, only: WP, IPL, IP
use globals, only: NX_G, NY_G, NZ2_G, ntrnds_G, ntrndsi_G, nspinDX, nspinDY, sLengthOfElmX_G, &
  sLengthOfElmY_G, sLengthOfElmZ2_G, fieldMesh, iTemporal, iPeriodic, s_chi_bar_G, &
  procelectrons_G, iNumberElectrons_G, sElX_G, sElY_G, sElZ2_G, sElPX_G, sElPY_G, sElGam_G, &
  ioutInfo_G, pi
use ParallelSetUp, only: MPI_INT_HIGH, stopcode, gather1a, getgatharrs
use puffin_mpiInfo, only: tProcInfo_G
use puffin_fftwInfo, only: tTransInfo_G
use gtop2, only: getp2, getp2avg
use GlobalTypes, only: tSimulationFlags, tFELFrame, tFieldValues
use mpi, only: MPI_ALLGATHER, mpi_allreduce, mpi_alltoallv, mpi_barrier, MPI_Bcast, &
  mpi_double_precision, MPI_IN_PLACE, mpi_integer, MPI_ISSEND, mpi_max, mpi_min, MPI_RECV, &
  mpi_reduce, mpi_scatter, MPI_STATUS_SIZE, mpi_sum, MPI_WAIT, MPI_WAITALL

implicit none (type, external)
private

public :: getinnode, getlocalfieldindices, inner2outer, &
           ioutInfo_G, iTemporal, outer2inner, pupd8, redist2fftwlt, &
           redistbackfft, tTransInfo_G, upd8a, upd8da, updateglobalpow


!  The field data, and the decomposition indices that give it meaning, now live
!  in ctx%field (type tFieldValues) and are passed in to the routines below.
!  What remains module state here is MPI scratch belonging to the transfers
!  themselves, not to the field. See UKFELs/Puffin#107.

real(kind=wp), allocatable :: tmp_A(:)

integer(kind=ip), allocatable :: recvs_pf(:), displs_pf(:), recvs_ff(:), &
                                 displs_ff(:), recvs_ef(:), displs_ef(:)

integer(kind=ip), allocatable :: recvs_ppf(:), displs_ppf(:), recvs_fpf(:), &
                                 displs_fpf(:), recvs_epf(:), displs_epf(:)

!!!   For parallel algorithm to deal with over-compression...

integer(kind=ip), allocatable :: lrank_v(:), rrank_v(:,:), &
                                 lrfromwhere(:)

integer(kind=ip) :: nsnds_bf, nrecvs_bf

! if fz2 to ez2 overlaps, give warning - but not fail...
! No, fz2 to ez2 will only overlap if less nodes than procs...
! in which case, we share ALL nodes...but do this later...
! and can use MPI_ALLGATHER or whatever as before (but on
! field%ac_r and field%ac_i rather than sA)

! if boundary overlaps next process, then process will send
! buffer to more than one process...so loop around lrank_v
! and rrank_v etc




! Options for field%iParaBas, the basis for the parallel decomposition

integer(kind=ip), parameter :: iElectronBased=1, &
                               iFieldBased = 2, &
                               iFFTW_based = 3

contains


        subroutine getLocalFieldIndices(sdz, flags, frame, field, pqSq)

    implicit none (type, external)

!     Setup local field pointers. These describe how the field is
!     parallelized. For now, only set up constant field barriers to
!     test a short 1D run. Field boundaries are decided by the
!     positions of electrons on adjecent processes, to ensure no
!     overlap (except at the 'boundaries').
!
!     Then can get more sophisticated...
!         1) Call more often to rearrange the grid and
!            provide a 'moving frame' for the electrons.
!
!         2) Can define bounds as averages between processes,
!            or start to share electrons between processes.

    real(kind=wp), intent(in) :: sdz
    type(tSimulationFlags), intent(inout) :: flags
    type(tFELFrame), intent(in) :: frame
    type(tFieldValues), intent(inout) :: field
    real(kind=wp), intent(in), optional :: pqSq   ! passed in averaged mode only - see calcBuff

    real(kind=wp), allocatable :: fr_rfield_old(:), &
                                  fr_ifield_old(:), &
                                  bk_rfield_old(:), &
                                  bk_ifield_old(:), &
                                  ac_rfield_old(:), &
                                  ac_ifield_old(:)

    integer(kind=ip) :: ij
    integer(kind=ip) :: gath_v
    integer :: req, error, lrank, rrank
    integer :: sendstat(MPI_STATUS_SIZE)





    integer(kind=ip), allocatable :: ee_ar_old(:,:), &
                                     ff_ar_old(:,:), &
                                     ac_ar_old(:,:)





    INTEGER(KIND=IPL) :: sendbuff, recvbuff
    INTEGER :: recvstat(MPI_STATUS_SIZE)

    real(kind=wp) :: lenz2

!    print*, 'INSIDE GETLOCALFIELDINDICES, SIZE OF SA IS ', size(sA)



!!!!!!&&*(&*(&CD*(S)))  INITIALIZING ONLY FOR TESTING!!!! WILL ONLY
!                       WORK WITH TEST CASE!!!!!

    if (field%qStart_new) then

      field%iParaBas = iFieldBased

      call getFStEnd(field)

      field%bz2 = field%ez2

      field%tlflen_glob = 0
      field%tlflen = 0
      field%tlflen4arr = 1
      field%ffs = 0
      field%ffe = 0


      field%tlelen_glob = 0
      field%tlelen = 0
      field%tlelen4arr = 1
      field%ees = 0
      field%eee = 0

      allocate(field%ee_ar(tProcInfo_G%size, 3))
      allocate(field%ff_ar(tProcInfo_G%size, 3))
      allocate(field%ac_ar(tProcInfo_G%size, 3))


      call setupLayoutArrs(field%mainlen, field%fz2, field%ez2, field%ac_ar)
      call setupLayoutArrs(field%tlflen, field%ffs, field%ffe, field%ff_ar)
      call setupLayoutArrs(field%tlelen, field%ees, field%eee, field%ee_ar)


      allocate(field%fr_r(field%tlflen4arr*ntrnds_G), &
                 field%fr_i(field%tlflen4arr*ntrnds_G))
      allocate(field%bk_r(field%tlelen4arr*ntrnds_G), &
               field%bk_i(field%tlelen4arr*ntrnds_G))

      allocate(field%ac_r(field%mainlen*ntrnds_G), &
               field%ac_i(field%mainlen*ntrnds_G))


      field%ac_r = 0_wp
      field%ac_i = 0_wp

      field%fr_r = 0_wp
      field%fr_i = 0_wp
      field%bk_r = 0_wp
      field%bk_i = 0_wp

      field%qStart_new = .false.

      if (fieldMesh == iTemporal) field%iParaBas = iElectronBased
      field%qUnique = .true.

    else

      deallocate(recvs_pf, displs_pf, tmp_A)
      deallocate(recvs_ff, displs_ff, recvs_ef, displs_ef)
      deallocate(lrank_v, lrfromwhere)
      deallocate(rrank_v)

      deallocate(recvs_ppf, displs_ppf)
      deallocate(recvs_fpf, displs_fpf, recvs_epf, displs_epf)

    end if


!!!   APPLY PERIODIC BOUNDS - PROBABLY A BETTER PLACE FOR THIS....

    if (FieldMesh == iPeriodic) then
      lenz2 = sLengthOfElmZ2_G * real((nz2_G - 1_ip), kind=wp )
      sElZ2_G = sElZ2_G - (real(floor(sElZ2_G / lenz2), kind=wp) * lenz2 )
    end if

  allocate(ee_ar_old(tProcInfo_G%size, 3))
  allocate(ff_ar_old(tProcInfo_G%size, 3))
  allocate(ac_ar_old(tProcInfo_G%size, 3))

  ee_ar_old = field%ee_ar
  ff_ar_old = field%ff_ar
  ac_ar_old = field%ac_ar



  call getFStEnd(field)    ! Define new 'active' region

  call setupLayoutArrs(field%mainlen, field%fz2, field%ez2, field%ac_ar)

  if (field%qUnique) call rearrElecs(field)   ! Rearrange electrons

! Branch on the argument, not on flags%period_averaged: init calls this
! before the flags are populated.

  if (present(pqSq)) then
    call calcBuff(4 * pi * frame%rho * sdz, frame%eta, frame%gamma_ref, &
                  frame%aw, field, pqSq)  ! Calculate buffers
  else
    call calcBuff(4 * pi * frame%rho * sdz, frame%eta, frame%gamma_ref, &
                  frame%aw, field)  ! Calculate buffers
  end if

  call getFrBk(field)  ! Get surrounding nodes

  call setupLayoutArrs(field%tlflen, field%ffs, field%ffe, field%ff_ar)
  call setupLayoutArrs(field%tlelen, field%ees, field%eee, field%ee_ar)


  if (.not. field%qUnique) then

    field%ac_ar(1,1) = field%mainlen
    field%ac_ar(1,2) = field%fz2
    field%ac_ar(1,3) = field%ez2
    field%ac_ar(2:tProcInfo_G%size,:) = 0

  end if



  allocate(fr_rfield_old(size(field%fr_r)), fr_ifield_old(size(field%fr_i)))
  allocate(bk_rfield_old(size(field%bk_r)), bk_ifield_old(size(field%bk_i)))
  allocate(ac_rfield_old(size(field%ac_r)), ac_ifield_old(size(field%ac_i)))

  fr_rfield_old = field%fr_r
  fr_ifield_old = field%fr_i
  bk_rfield_old = field%bk_r
  bk_ifield_old = field%bk_i
  ac_rfield_old = field%ac_r
  ac_ifield_old = field%ac_i

  deallocate(field%ac_r, field%ac_i)
  deallocate(field%fr_r, field%fr_i)
  deallocate(field%bk_r, field%bk_i)

  allocate(field%fr_r(field%tlflen4arr*ntrnds_G), &
           field%fr_i(field%tlflen4arr*ntrnds_G))
  allocate(field%bk_r(field%tlelen4arr*ntrnds_G), &
           field%bk_i(field%tlelen4arr*ntrnds_G))
  allocate(field%ac_r(field%tllen*ntrnds_G), &
           field%ac_i(field%tllen*ntrnds_G))

  field%ac_r = 0_wp
  field%ac_i = 0_wp

  field%bk_r = 0_wp
  field%bk_i = 0_wp
  field%fr_r = 0_wp
  field%fr_i = 0_wp


  call redist2new2(ff_ar_old, field%ff_ar, fr_rfield_old, field%fr_r)
  call redist2new2(ff_ar_old, field%ff_ar, fr_ifield_old, field%fr_i)

  call redist2new2(ee_ar_old, field%ff_ar, bk_rfield_old, field%fr_r)
  call redist2new2(ee_ar_old, field%ff_ar, bk_ifield_old, field%fr_i)

  call redist2new2(ac_ar_old, field%ff_ar, ac_rfield_old, field%fr_r)
  call redist2new2(ac_ar_old, field%ff_ar, ac_ifield_old, field%fr_i)





  call redist2new2(ff_ar_old, field%ee_ar, fr_rfield_old, field%bk_r)
  call redist2new2(ff_ar_old, field%ee_ar, fr_ifield_old, field%bk_i)

  call redist2new2(ee_ar_old, field%ee_ar, bk_rfield_old, field%bk_r)
  call redist2new2(ee_ar_old, field%ee_ar, bk_ifield_old, field%bk_i)


  call redist2new2(ac_ar_old, field%ee_ar, ac_rfield_old, field%bk_r)
  call redist2new2(ac_ar_old, field%ee_ar, ac_ifield_old, field%bk_i)

!  call mpi_finalize(error)
!  stop





  call redist2new2(ff_ar_old, field%ac_ar, fr_rfield_old, field%ac_r)
  call redist2new2(ff_ar_old, field%ac_ar, fr_ifield_old, field%ac_i)

  call redist2new2(ee_ar_old, field%ac_ar, bk_rfield_old, field%ac_r)
  call redist2new2(ee_ar_old, field%ac_ar, bk_ifield_old, field%ac_i)

  call redist2new2(ac_ar_old, field%ac_ar, ac_rfield_old, field%ac_r)
  call redist2new2(ac_ar_old, field%ac_ar, ac_ifield_old, field%ac_i)




  deallocate(ff_ar_old, &
             ee_ar_old, &
             ac_ar_old)


  deallocate(ac_rfield_old, ac_ifield_old)
  deallocate(fr_rfield_old, fr_ifield_old)
  deallocate(bk_rfield_old, bk_ifield_old)


  if (.not. field%qUnique) then

    call MPI_Bcast(field%ac_r, field%tllen*ntrnds_G, &
                   mpi_double_precision, 0, &
                   tProcInfo_G%comm, error)

    call MPI_Bcast(field%ac_i, field%tllen*ntrnds_G, &
                   mpi_double_precision, 0, &
                   tProcInfo_G%comm, error)
  end if


! then deallocate old fields

! then redist electrons for new layout




!  #######################################################################
!     Get gathering arrays - only used to gather active field sections
!     back to GLOBAL field (the full field array on each process...)
!     Will NOT be needed later on....
!     ...and should now ONLY be used for data writing while testing...



      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then
        gath_v = field%tlflen*ntrnds_G !-1
      else
        gath_v = field%tlflen*ntrnds_G
      end if


      allocate(recvs_ff(tProcInfo_G%size), displs_ff(tProcInfo_G%size))
      call getGathArrs(gath_v, recvs_ff, displs_ff)





      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then
        gath_v = field%mainlen*ntrnds_G ! -1
      else
        gath_v = field%mainlen*ntrnds_G
      end if

      allocate(recvs_pf(tProcInfo_G%size), displs_pf(tProcInfo_G%size))
      call getGathArrs(gath_v, recvs_pf, displs_pf)



      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then
        gath_v = field%tlelen*ntrnds_G !-1
      else
        gath_v = field%tlelen*ntrnds_G
      end if

      allocate(recvs_ef(tProcInfo_G%size), displs_ef(tProcInfo_G%size))
      call getGathArrs(gath_v, recvs_ef, displs_ef)









!  #######################################################################
!     Get gathering arrays - only used to gather active field sections
!   THESE ARE FOR POWER



      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then
        gath_v = field%tlflen !-1
      else
        gath_v = field%tlflen
      end if


      allocate(recvs_fpf(tProcInfo_G%size), displs_fpf(tProcInfo_G%size))
      recvs_fpf = 0
      displs_fpf = 0
      call getGathArrs(gath_v, recvs_fpf, displs_fpf)





      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then
        gath_v = field%mainlen ! -1
      else
        gath_v = field%mainlen
      end if

      allocate(recvs_ppf(tProcInfo_G%size), displs_ppf(tProcInfo_G%size))
      recvs_ppf = 0
      displs_ppf = 0
      call getGathArrs(gath_v, recvs_ppf, displs_ppf)



      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then
        gath_v = field%tlelen !-1
      else
        gath_v = field%tlelen
      end if

      allocate(recvs_epf(tProcInfo_G%size), displs_epf(tProcInfo_G%size))
      recvs_epf = 0
      displs_epf = 0
      call getGathArrs(gath_v, recvs_epf, displs_epf)




!  #######################################################################



      ! Allocate back, front and active fields....commented out!
      ! ONLY USING ACTIVE FIELD FOR NOW TO CHECK SCALING...TO SEE
      ! IF IT'S WORTH PERSUING THIS METHOD
!      allocate(field%fr_r(field%tlflen), field%bk_r(field%tlelen), &
!               field%fr_i(field%tlflen), field%bk_i(field%tlelen))

!      allocate(field%ac_r(field%tllen), &
!               field%ac_i(field%tllen))

!      field%ac_r = sA(field%fz2:field%bz2)
!      field%ac_i = sA(field%fz2 + NZ2_G:field%bz2 + NZ2_G)

!      print*, 'INSIDE GETLOCALFIELDINDICES, SIZE OF SA AT 5 IS ', size(sA), &
!              ' FOR PROCESSOR ', tProcInfo_G%rank


  call mpi_barrier(tProcInfo_G%comm, error)

    IF (tProcInfo_G%rank == tProcInfo_G%size-1) THEN
       rrank = 0
       lrank = tProcInfo_G%rank-1
    ELSE IF (tProcInfo_G%rank==0) THEN
       rrank = tProcInfo_G%rank+1
       lrank = tProcInfo_G%size-1
    ELSE
       rrank = tProcInfo_G%rank+1
       lrank = tProcInfo_G%rank-1
    END IF


    procelectrons_G(1) = iNumberElectrons_G

    sendbuff = iNumberElectrons_G
    recvbuff = iNumberElectrons_G

    if (tProcInfo_G%size > 1_ip) then

      do ij=2,tProcInfo_G%size
         call MPI_ISSEND( sendbuff,1,MPI_INT_HIGH,rrank,&
              0,tProcInfo_G%comm,req,error )
         call MPI_RECV( recvbuff,1,MPI_INT_HIGH,lrank,&
              0,tProcInfo_G%comm,recvstat,error )
         call MPI_WAIT( req,sendstat,error )
         procelectrons_G(ij) = recvbuff
         sendbuff=recvbuff
      end do

    end if



    if (field%qUnique) then

      allocate(tmp_A(maxval(lrank_v)*ntrndsi_G))

    else
      allocate(tmp_A(field%tllen*ntrndsi_G))
    end if

     tmp_A = 0_wp


      call pupd8(field)

      flags%parallel_arrays_ok = .true.

    end subroutine getLocalFieldIndices


!  ###################################################


    subroutine UpdateGlobalPow(fpow, apow, bpow, gpow, field)

      real(kind=wp), intent(inout) :: fpow(:), apow(:), bpow(:), gpow(:)
      type(tFieldValues), intent(in) :: field

      integer(kind=ip) :: gath_v

      real(kind=wp), allocatable :: A_local(:), powi(:)


      gpow=0.0_wp

      if ((field%ffe_GGG - field%ffs_GGG) > 0) then

        if (tProcInfo_G%rank /= tProcInfo_G%size-1) then
          gath_v = field%tlflen  !-1
        else
          gath_v = field%tlflen
        end if




        allocate(A_local(gath_v))

        A_local = 0_wp

        A_local(1:gath_v) = fpow(1:gath_v)

        call gather1A(A_local, gpow(field%ffs_GGG:field%ffe_GGG), &
                gath_v, field%ffe_GGG - field%ffs_GGG + 1, &
                  recvs_fpf, displs_fpf)


        deallocate(A_local)

      end if



      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then
        gath_v = field%mainlen !-1
      else
        gath_v = field%mainlen
      end if




      allocate(A_local(gath_v))
      allocate(powi(field%ez2_GGG - field%fz2_GGG + 1))

      A_local = 0_wp
      powi = 0_wp

      A_local(1:gath_v) = apow(1:gath_v)

      if (field%qUnique) then
        call gather1A(A_local, powi, &
                       gath_v, field%fz2_GGG - field%ez2_GGG + 1, &
                       recvs_ppf, displs_ppf)
      else
        powi = A_local
      end if


      gpow(field%fz2_GGG:field%ez2_GGG) = powi(:)
      deallocate(A_local)
      deallocate(powi)


      if ( (field%eee_GGG - field%ees_GGG) > 0) then

        if (tProcInfo_G%rank /= tProcInfo_G%size-1) then
          gath_v = field%tlelen !-1
        else
          gath_v = field%tlelen
        end if




        allocate(A_local(gath_v))
        allocate(powi(field%eee_GGG - field%ees_GGG + 1))


          A_local = 0_wp
          powi = 0_wp

          A_local(1:gath_v) = bpow(1:gath_v)

          call gather1A(A_local, powi, &
                         gath_v, field%eee_GGG - field%ees_GGG + 1, recvs_epf, displs_epf)


          gpow(field%ees_GGG:field%eee_GGG) = powi(:)

          deallocate(A_local)
          deallocate(powi)

      end if


    end subroutine UpdateGlobalPow

!  ###################################################




    subroutine upd8da(dadz_r, dadz_i, field)

    ! Send dadz from buffer to MPI process on the right
    ! Data is added to array in next process, not
    ! written over.

      real(kind=wp), contiguous, intent(inout) :: dadz_r(:), dadz_i(:)
      type(tFieldValues), intent(in) :: field

      integer :: req, error
      integer(kind=ip) :: ij, si, sst, sse
      integer :: statr(MPI_STATUS_SIZE)
      integer :: sendstat(MPI_STATUS_SIZE)

!     One request per outstanding issend - a single scalar handle would
!     be overwritten by each iteration, leaking every request but the last.

      integer, allocatable :: reqs(:), sendstats(:,:)

      real(kind=wp), allocatable :: Abounds(:)

      if (field%qUnique) then

        if (tProcInfo_G%rank /= tProcInfo_G%size-1) then
          allocate(reqs(nsnds_bf), sendstats(MPI_STATUS_SIZE, nsnds_bf))
        end if



        tmp_A = 0_wp

        if (tProcInfo_G%rank /= tProcInfo_G%size-1) then

!        send to rank+1

           do ij = 1, nsnds_bf

            si = rrank_v(ij, 1)
            sst = rrank_v(ij, 2)
            sse = rrank_v(ij, 3)

            sst = (sst - (field%fz2-1)-1)*ntrndsi_G + 1
            sse = (sse-(field%fz2-1))*ntrndsi_G
            si = si*ntrndsi_G


            call mpi_issend(dadz_r(sst:sse), &
                      si, &
                      mpi_double_precision, &
                      tProcInfo_G%rank+ij, 0, tProcInfo_G%comm, reqs(ij), error)


          end do


        end if


        if (tProcInfo_G%rank /= 0) then

!       rec from rank-1

          do ij = 1, nrecvs_bf

            CALL mpi_recv( tmp_A(1:lrank_v(ij)*ntrndsi_G), &
                   lrank_v(ij)*ntrndsi_G, &
                   mpi_double_precision, &
                     lrfromwhere(ij), 0, tProcInfo_G%comm, statr, error )

            dadz_r(1:lrank_v(ij)*ntrndsi_G) = dadz_r(1:lrank_v(ij)*ntrndsi_G) &
                                           + tmp_A(1:lrank_v(ij)*ntrndsi_G)

          end do

        end if

        if (tProcInfo_G%rank /= tProcInfo_G%size-1) call mpi_waitall( nsnds_bf, &
                                                    reqs, sendstats, error )







        tmp_A = 0_wp

        if (tProcInfo_G%rank /= tProcInfo_G%size-1) then

  !        send to rank+1

          do ij = 1, nsnds_bf

            si = rrank_v(ij, 1)
            sst = rrank_v(ij, 2)
            sse = rrank_v(ij, 3)


            sst = (sst - (field%fz2-1)-1)*ntrndsi_G + 1
            sse = (sse-(field%fz2-1))*ntrndsi_G
            si = si*ntrndsi_G

            call mpi_issend(dadz_i(sst:sse), &
                      si, &
                      mpi_double_precision, &
                      tProcInfo_G%rank+ij, 0, tProcInfo_G%comm, reqs(ij), error)


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


        if (tProcInfo_G%rank /= tProcInfo_G%size-1) call mpi_waitall( nsnds_bf, &
                                                    reqs, sendstats, error )

        if (tProcInfo_G%rank /= tProcInfo_G%size-1) deallocate(reqs, sendstats)


      else


        call mpi_reduce(dadz_r, tmp_A, field%mainlen*ntrndsi_G, &
                        mpi_double_precision, &
                        mpi_sum, 0, tProcInfo_G%comm, &
                        error)

        dadz_r = tmp_A


        call mpi_reduce(dadz_i, tmp_A, field%mainlen*ntrndsi_G, &
                        mpi_double_precision, &
                        mpi_sum, 0, tProcInfo_G%comm, &
                        error)

        dadz_i = tmp_A

      end if


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  PERIODIC BOUNDS - EXPERIMENTAL


      if (FieldMesh == iPeriodic) then

        if (.not. field%qUnique) then

!!!! IF PERIODIC

          si = ntrndsi_G * (field%bz2PB + 1_ip)
          sst = ((field%tllen - (field%bz2PB+1_ip) ) * ntrndsi_G) + 1_ip
          sse = field%tllen * ntrndsi_G
          if (ioutInfo_G > 0) print*, size(dadz_r), field%tllen, field%mainlen

          if (ioutInfo_G > 0) print*, "IM NOT UNIQUE"

          dadz_r(1:si) = dadz_r(1:si) + dadz_r(sst:sse)
          dadz_i(1:si) = dadz_i(1:si) + dadz_i(sst:sse)


        else



          si = ntrndsi_G * (field%bz2PB + 1_ip)
          sst = ((field%tllen - (field%bz2PB + 1_ip) ) * ntrndsi_G) + 1_ip
          sse = field%tllen * ntrndsi_G

          if (tProcInfo_G%rank == tProcInfo_G%size-1_ip) then

              call mpi_issend(si, 1, mpi_integer, 0_ip, 0, &
                              tProcInfo_G%comm, req, error)

          end if

          if (tProcInfo_G%rank == 0) then

              call mpi_recv( si, 1, mpi_integer, tProcInfo_G%size-1_ip, 0, &
                             tProcInfo_G%comm, statr, error )

          end if

          if (tProcInfo_G%rank == tProcInfo_G%size-1_ip) then

!           Complete the size handshake above before reusing req - rank 0
!           has posted its matching recv by now.

            call mpi_wait( req,sendstat,error )

            call mpi_issend(dadz_r(sst:sse), &
                            si, &
                            mpi_double_precision, &
                            0_ip, 0, &
                            tProcInfo_G%comm, req, error)

          end if


          if (tProcInfo_G%rank == 0) then

            allocate(Abounds(si))

            call mpi_recv( Abounds, si, mpi_double_precision, &
                     tProcInfo_G%size-1_ip, 0, tProcInfo_G%comm, &
                     statr, error )

            dadz_r(1:si) = dadz_r(1:si) + Abounds

          end if


          if (tProcInfo_G%rank == tProcInfo_G%size-1) then

            call mpi_wait( req,sendstat,error )
            call mpi_issend(dadz_i(sst:sse), si, mpi_double_precision, &
                            0_ip, 0, tProcInfo_G%comm, req, error)

          end if

          if (tProcInfo_G%rank == 0) then

            call mpi_recv( Abounds, si, mpi_double_precision, &
                      tProcInfo_G%size-1_ip, 0, tProcInfo_G%comm, &
                      statr, error )

            dadz_i(1:si) = dadz_i(1:si) + Abounds

            deallocate(Abounds)

          end if


          if (tProcInfo_G%rank == tProcInfo_G%size-1) then

            call mpi_wait( req,sendstat,error )

          end if




          if (tProcInfo_G%rank == 0_ip) then

            call mpi_issend(dadz_r(1:si), si, mpi_double_precision, &
                            tProcInfo_G%size-1_ip, 0, &
                            tProcInfo_G%comm, req, error)

          end if



          if (tProcInfo_G%rank == tProcInfo_G%size-1_ip) then

            call mpi_recv( dadz_r(sst:sse), si, mpi_double_precision, &
                     0, 0, tProcInfo_G%comm, statr, error )

          end if


          if (tProcInfo_G%rank == 0_ip) then

            call mpi_wait( req,sendstat,error )
            call mpi_issend(dadz_i(1:si), si, mpi_double_precision, &
                            tProcInfo_G%size-1_ip, 0, &
                            tProcInfo_G%comm, req, error)

          end if


          if (tProcInfo_G%rank == tProcInfo_G%size-1_ip) then

            call mpi_recv( dadz_i(sst:sse), si, mpi_double_precision, &
                     0, 0, tProcInfo_G%comm, statr, error )

          end if


          if (tProcInfo_G%rank == 0_ip) then

            call mpi_wait( req,sendstat,error )

          end if

        end if  ! end periodic mesh for field%qUnique

      end if   ! End synch'ing for periodic mesh...

    end subroutine upd8da


!  ###################################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Update the periodic boundary buffer for the periodic case

!> Periodic-mesh wraparound of the active slab. Takes the field object rather
!> than the two arrays: it is only ever called on ac_r/ac_i, and passing those
!> alongside the object they belong to would alias an intent(inout) dummy
!> against part of the same object.

    subroutine pupd8(field)

      type(tFieldValues), intent(inout) :: field

      integer :: req, error
      integer(kind=ip) :: si, sst, sse
      integer :: statr(MPI_STATUS_SIZE)
      integer :: sendstat(MPI_STATUS_SIZE)

      if (FieldMesh == iPeriodic) then

        if (field%qUnique) then

          si = ntrnds_G * (field%bz2PB + 1_ip)
          sst = ((field%tllen - (field%bz2PB + 1_ip) ) * ntrnds_G) + 1_ip
          sse = field%tllen * ntrnds_G

          if (tProcInfo_G%rank == 0_ip) then

            call mpi_issend(field%ac_r(1:si), si, mpi_double_precision, &
                            tProcInfo_G%size-1_ip, 0, &
                            tProcInfo_G%comm, req, error)

          end if



          if (tProcInfo_G%rank == tProcInfo_G%size-1_ip) then

            call mpi_recv( field%ac_r(sst:sse), si, mpi_double_precision, &
                     0, 0, tProcInfo_G%comm, statr, error )

          end if


          if (tProcInfo_G%rank == 0_ip) then

            call mpi_wait( req,sendstat,error )
            call mpi_issend(field%ac_i(1:si), si, mpi_double_precision, &
                            tProcInfo_G%size-1_ip, 0, &
                            tProcInfo_G%comm, req, error)

          end if


          if (tProcInfo_G%rank == tProcInfo_G%size-1_ip) then

            call mpi_recv( field%ac_i(sst:sse), si, mpi_double_precision, &
                     0, 0, tProcInfo_G%comm, statr, error )

          end if


          if (tProcInfo_G%rank == 0_ip) then

            call mpi_wait( req,sendstat,error )

          end if

        end if

      end if

    end subroutine pupd8

!  ###################################################





    subroutine upd8a(ac_rl, ac_il, field)

      implicit none (type, external)

    ! Send sA from buffer to process on the left
    ! Data in 'buffer' on the left is overwritten.

      real(kind=wp), contiguous, intent(inout) :: ac_rl(:), ac_il(:)
      type(tFieldValues), intent(in) :: field

      integer(kind=ip) :: req, error, ij, si, sst, sse
      integer :: statr(MPI_STATUS_SIZE)
      integer :: sendstat(MPI_STATUS_SIZE)

!     One request per outstanding issend - a single scalar handle would
!     be overwritten by each iteration, leaking every request but the last.

      integer, allocatable :: reqs(:), sendstats(:,:)

!      real(kind=wp), allocatable :: tstf(:), tstf2(:)



      if (field%qUnique) then

        if (tProcInfo_G%rank /= 0) then
          allocate(reqs(nrecvs_bf), sendstats(MPI_STATUS_SIZE, nrecvs_bf))
        end if


        if (tProcInfo_G%rank /= 0) then

  !       rec from rank-1

          do ij = 1, nrecvs_bf

            CALL mpi_issend( ac_rl(1:lrank_v(ij)*ntrndsi_G), &
                   lrank_v(ij)*ntrndsi_G, &
                   mpi_double_precision, &
                   lrfromwhere(ij), 0, tProcInfo_G%comm, reqs(ij), error )

          end do

        end if







        if (tProcInfo_G%rank /= tProcInfo_G%size-1) then

  !        send to rank+1

          !do ij = tProcInfo_G%rank + 1, tProcInfo_G%size-1
           do ij = 1, nsnds_bf

            si = rrank_v(ij, 1)
            sst = rrank_v(ij, 2)
            sse = rrank_v(ij, 3)

            sst = (sst - (field%fz2-1)-1)*ntrndsi_G + 1
            sse = (sse-(field%fz2-1))*ntrndsi_G
            si = si*ntrndsi_G

  !          call mpi_issend(dadz_r((field%ez2+1)-(field%fz2-1) + ofst :field%bz2-(field%fz2-1)), si, &
  !                    mpi_double_precision, &
  !                    tProcInfo_G%rank+1, 0, tProcInfo_G%comm, req, error)

            call mpi_recv(ac_rl(sst:sse), &
                      si, &
                      mpi_double_precision, &
                      tProcInfo_G%rank+ij, 0, tProcInfo_G%comm, statr, error)


          end do


        end if


        if (tProcInfo_G%rank /= 0) call mpi_waitall( nrecvs_bf, reqs, &
                                                     sendstats, error )




        if (tProcInfo_G%rank /= 0) then

  !       rec from rank-1

          do ij = 1, nrecvs_bf

            CALL mpi_issend( ac_il(1:lrank_v(ij)*ntrndsi_G), &
                   lrank_v(ij)*ntrndsi_G, &
                   mpi_double_precision, &
                   lrfromwhere(ij), 0, tProcInfo_G%comm, reqs(ij), error )

          end do

        end if







        if (tProcInfo_G%rank /= tProcInfo_G%size-1) then

  !        send to rank+1

          !do ij = tProcInfo_G%rank + 1, tProcInfo_G%size-1
           do ij = 1, nsnds_bf

            si = rrank_v(ij, 1)
            sst = rrank_v(ij, 2)
            sse = rrank_v(ij, 3)

            sst = (sst - (field%fz2-1)-1)*ntrndsi_G + 1
            sse = (sse-(field%fz2-1))*ntrndsi_G
            si = si*ntrndsi_G

  !          call mpi_issend(dadz_r((field%ez2+1)-(field%fz2-1) + ofst :field%bz2-(field%fz2-1)), si, &
  !                    mpi_double_precision, &
  !                    tProcInfo_G%rank+1, 0, tProcInfo_G%comm, req, error)

            call mpi_recv(ac_il( sst:sse ), &
                      si, &
                      mpi_double_precision, &
                      tProcInfo_G%rank+ij, 0, tProcInfo_G%comm, statr, error)


          end do


        end if


        if (tProcInfo_G%rank /= 0) then
          call mpi_waitall( nrecvs_bf, reqs, sendstats, error )
          deallocate(reqs, sendstats)
        end if



      else


        call MPI_Bcast(ac_rl, field%tllen*ntrndsi_G, &
                       mpi_double_precision, 0, &
                       tProcInfo_G%comm, error)

        call MPI_Bcast(ac_il, field%tllen*ntrndsi_G, &
                       mpi_double_precision, 0, &
                       tProcInfo_G%comm, error)


      end if


!   -----     OLD

!      if (tProcInfo_G%rank /= 0) then
!
!!        send to rank-1
!
!        call mpi_issend(ac_rl(1:field%fbuffLenM), field%fbuffLenM, mpi_double_precision, &
!                           tProcInfo_G%rank-1, 0, tProcInfo_G%comm, req, error)
!
!      end if
!
!
!
!      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then
!
!!       rec from rank+1
!
!        CALL mpi_recv( ac_rl((field%ez2+1)-(field%fz2-1):field%bz2-(field%fz2-1)), field%fbuffLen, mpi_double_precision, &
!                    tProcInfo_G%rank+1, 0, tProcInfo_G%comm, statr, error )
!
!      end if
!
!
!
!
!
!      if (tProcInfo_G%rank /= 0) call mpi_wait( req,sendstat,error )
!
!
!
!      if (tProcInfo_G%rank /= 0) then
!
!!        send to rank-1
!
!        call mpi_issend(ac_il(1:field%fbuffLenM), field%fbuffLenM, mpi_double_precision, &
!                tProcInfo_G%rank-1, 0, tProcInfo_G%comm, req, error)
!
!      end if
!
!      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then
!
!!       rec from rank+1
!
!        CALL mpi_recv( ac_il((field%ez2+1)-(field%fz2-1):field%bz2-(field%fz2-1)), field%fbuffLen, mpi_double_precision, &
!               tProcInfo_G%rank+1, 0, tProcInfo_G%comm, statr, error )
!
!      end if
!
!      if (tProcInfo_G%rank /= 0) call mpi_wait( req,sendstat,error )
!
!   -----     OLD



    end subroutine upd8a






  subroutine inner2Outer(inner_ra, inner_ia, field)


    implicit none (type, external)

    real(kind=wp), contiguous, intent(in) :: inner_ra(:), inner_ia(:)
    type(tFieldValues), intent(inout) :: field

    integer(kind=ip) :: iz, ssti, ssei, iy, sst, sse
    integer(kind=ip) :: nxout, nyout ! should be made global and calculated

    nxout = (nx_g - nspindx)/2
    nyout = (ny_g - nspindy)/2


    do iz = field%fz2, field%bz2

      do iy = 1, nspinDY

        sst = (iz - (field%fz2-1)-1)*ntrnds_G + &
                         nx_G*(nyout+(iy-1)) + &
                         nxout + 1

        sse = sst + nspinDX - 1

        ssti = (iz - (field%fz2-1)-1)*ntrndsi_G + &
                         nspinDX*(iy-1) + 1
        ssei = ssti + nspinDX - 1

        field%ac_r(sst:sse) = inner_ra(ssti:ssei)
        field%ac_i(sst:sse) = inner_ia(ssti:ssei)

      end do

    end do


  end subroutine inner2Outer





  subroutine outer2Inner(inner_ra, inner_ia, field)


    implicit none (type, external)

    real(kind=wp), contiguous, intent(out) :: inner_ra(:), inner_ia(:)
    type(tFieldValues), intent(in) :: field

    integer(kind=ip) :: iz, sst, sse, ssti, ssei
    integer(kind=ip) :: nxout, nyout, iy ! should be made global and calculated

    nxout = (nx_g - nspindx)/2
    nyout = (ny_g - nspindy)/2

    do iz = field%fz2, field%bz2

      do iy = 1, nspinDY

        sst = (iz - (field%fz2-1)-1)*ntrnds_G + &
                         nx_G*(nyout+(iy-1)) + &
                         nxout + 1

        sse = sst + nspinDX - 1

        ssti = (iz - (field%fz2-1)-1)*ntrndsi_G + &
                         nspinDX*(iy-1) + 1
        ssei = ssti + nspinDX - 1

        inner_ra(ssti:ssei) = field%ac_r(sst:sse)
        inner_ia(ssti:ssei) = field%ac_i(sst:sse)

      end do

    end do


  end subroutine outer2Inner


  subroutine getInNode(flags)

  type(tSimulationFlags), intent(inout) :: flags

  real(kind=wp) :: sminx, smaxx, sminy, smaxy
  integer(kind=ip) :: iminx, imaxx, iminy, imaxy, &
                      inBuf

  integer :: error

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

    nspinDX = maxval([imaxx,iminx]) + inBuf
    nspinDY = maxval([imaxy,iminy]) + inBuf

    nspinDX = nspinDX * 2
    nspinDY = nspinDY * 2

  else   ! arbitrary minimum in case of zero MPs...

    nspinDX = 2_ipl
    nspinDY = 2_ipl

  end if

  if (mod(nx_g, 2) /= mod(nspinDX, 2) ) then

    nspinDX =  nspinDX + 1

  end if

  if (mod(ny_g, 2) /= mod(nspinDY, 2) ) then

    nspinDY =  nspinDY + 1

  end if



  call mpi_allreduce(MPI_IN_PLACE, nspinDX, 1, mpi_integer, &
                   mpi_max, tProcInfo_G%comm, error)

  call mpi_allreduce(MPI_IN_PLACE, nspinDY, 1, mpi_integer, &
                   mpi_max, tProcInfo_G%comm, error)

  if (nspinDX > nx_g) then
    print*, "ERROR, x grid not large enough"
    print*, "nspinDX = ", nspinDX
    call StopCode()
  end if


  if (nspinDY > ny_g) then
    print*, "ERROR, y grid not large enough"
    print*, "nspinDY = ", nspinDY
    call StopCode()
  end if


  ntrndsi_G = nspinDX * nspinDY

  flags%inner_xy_ok = .true.

  end subroutine getInNode





  subroutine divNodes(ndpts, numproc, rank, &
                      locN, local_start, local_end)

! Get local number of nodes and start and global
! indices of start and end points.
!
!           ARGUMENTS

    integer(kind=ip), intent(in) :: ndpts, numproc, rank

    integer(kind=ip), intent(out) :: locN
    integer(kind=ip), intent(out) :: local_start, local_end

!          LOCAL ARGS

    real(kind=wp) :: frac
    integer(kind=ip) :: lowern, highern, remainder


    frac = REAL(ndpts)/REAL(numproc)
    lowern = FLOOR(frac)
    highern = CEILING(frac)
    remainder = MOD(ndpts,numproc)

    IF (remainder==0) THEN
       locN = lowern
    ELSE
       IF (rank < remainder) THEN
          locN = highern
       ELSE
          locN = lowern
       END IF
    END IF


!     Calculate local start and end values.

    IF (rank >= remainder) THEN

      local_start = (remainder*highern) + ((rank-remainder) * lowern) + 1
      local_end = local_start + locN - 1

    ELSE

      local_start = rank*locN + 1
      local_end = local_start + locN - 1

    END IF


  end subroutine divNodes















  subroutine golaps(iso, ieo, f_ar, f_send)

!     Calculate so e.g. f_send(1,1:3) holds number of nodes to c
!     send from local front array to front array of rank=0, and
!     the local start and end positions of what is being sent
!     from the local start array, respectively

! inputs

    integer(kind=ip), intent(in)  :: iso, ieo
    integer(kind=ip), intent(in)  :: f_ar(:,:)
    integer(kind=ip), intent(out) :: f_send(:,:)

! local args

    integer(kind=ip) :: iproc


!    print*, iso, ieo, size(f_send)

    do iproc = 0,tProcInfo_G%size-1

      f_send(iproc+1,1) = 0
      f_send(iproc+1,2) = 0
      f_send(iproc+1,3) = 0

      if (f_ar(iproc+1,1) > 0) then

        ! end node between limits
        if ((ieo >= f_ar(iproc+1,2)) .and. (ieo <= f_ar(iproc+1,3) ) )  then

          f_send(iproc+1,3) = ieo

          ! front node before first limit
          if (iso < f_ar(iproc+1,2)) f_send(iproc+1,2) = f_ar(iproc+1,2)

          if (iso >= f_ar(iproc+1,2)) f_send(iproc+1,2) = iso   ! front node after first limit

          f_send(iproc+1,1) = f_send(iproc+1,3) - f_send(iproc+1,2) + 1

        ! end node after last limit
        else if ( (ieo >= f_ar(iproc+1,3)) .and.  (iso <= f_ar(iproc+1,3)))  then

          f_send(iproc+1, 3) = f_ar(iproc+1,3)

          ! front node before first limit
          if (iso < f_ar(iproc+1,2)) f_send(iproc+1,2) = f_ar(iproc+1,2)

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

















  subroutine calcBuff(dz, sEta, sGammaR, sAw, field, pqSq)

! Subroutine to setup the 'buffer' region
! at the end of the parallel field section
! on this process.
!
! This is calculated by estimating how much
! will be needed by the electrons currently on the
! process. By calculating p2, one may estimate the
! size of the domain required in z2 to hold
! the electron macroparticles over a distance
! dz through the undulator.

    real(kind=wp), intent(in) :: dz, sEta, sGammaR, sAw
    type(tFieldValues), intent(inout) :: field

! Present only in the averaged mode: the period average of the undulator
! quiver |pperp|^2.  There sElPX_G/sElPY_G hold only the slow part of pperp,
! and would otherwise predict the drift-space p2 - roughly half the
! in-undulator value - and so too small a buffer.  Absent, this routine is
! exactly as it was, so the unaveraged path is untouched.

    real(kind=wp), intent(in), optional :: pqSq
    real(kind=wp), allocatable :: sp2(:)

    real(kind=wp) :: bz2_len
    integer(kind=ip) :: yip, ij, bz2_globm, ctrecvs, cpolap, dum_recvs, &
                        maxbz2PB, bz2last
    integer(kind=ip), allocatable :: drecar(:)
    integer :: error, req, nsnd_hs
    integer(kind=ip) :: izero_hs

!   One request per outstanding issend - a single scalar handle would
!   be overwritten by each iteration, leaking every request but the last.

    integer, allocatable :: reqs(:), sendstats(:,:)

    integer :: statr(MPI_STATUS_SIZE)
    integer :: sendstat(MPI_STATUS_SIZE)

    ! get buffer location

    if (iNumberElectrons_G > 0_ipl) then

      allocate(sp2(iNumberElectrons_G))

      if (present(pqSq)) then
        call getP2Avg(sp2, sElGam_G, sElPX_G, sElPY_G, sEta, sGammaR, sAw, pqSq)
      else
        call getP2(sp2, sElGam_G, sElPX_G, sElPY_G, sEta, sGammaR, sAw)
      end if

      bz2_len = dz  ! distance in zbar until next rearrangement
      ! predicted length in z2 needed needed in buffer for beam
      bz2_len = maxval(sElZ2_G + bz2_len * sp2)

!    print*, 'field%bz2 length is...', bz2_len
!    print*, 'max p2 is ', maxval(sp2)

      deallocate(sp2)

    else

      bz2_len = (field%ez2 + 2_ip) * sLengthOfElmZ2_G  ! Just have 2 node boundary for no macroparticles

    end if

!    print*, tProcInfo_G%rank, 'is inside calcBuff, with buffer length', bz2_len

!    field%bz2 = field%ez2 + nint(4 * 4 * pi * sRho_G / sLengthOfElmZ2_G)
!    Boundary only 4 lambda_r long - so can only go ~ 3 periods

!   Node index of the final node in the boundary. The furthest particle
!   interpolates onto nodes floor(z2/dz2) + 1 and + 2, and getInterps_* need
!   its lower node to lie below bz2, so bz2 must reach floor(z2/dz2) + 2.
!   Rounding to the nearest node leaves the buffer up to 1.5 cells short.
!   That is harmless while the slippage over a redistribution interval spans
!   many cells, as on a mesh resolving the carrier, but on the coarse envelope
!   mesh of the averaged mode it is a fraction of a cell and the
!   rearrangement fails outright - so the averaged mode uses the exact bound.
!   The unaveraged path keeps nint: changing the buffer there changes the
!   parallel layout, and with it the summation order, so the e2e goldens
!   would no longer match at 1e-10.

    if (present(pqSq)) then
      field%bz2 = floor(bz2_len / sLengthOfElmZ2_G, kind=ip) + 2_ip
    else
      field%bz2 = nint(bz2_len / sLengthOfElmZ2_G)
    end if

    if (fieldMesh == iPeriodic) then

      field%bz2PB = 0_ip

      if (field%bz2 > nz2_G) then

        field%bz2PB = field%bz2 - nz2_G
!        if (field%bz2PB >= field%fz2) field%qUnique = .false.
        !field%bz2 = nz2_G

      end if

    else

      if (field%bz2 > nz2_G) field%bz2 = nz2_G
      field%bz2PB = 0_ip

    end if


! Find global bz2...

    call mpi_allreduce(field%bz2, bz2_globm, 1, mpi_integer, mpi_max, &
                    tProcInfo_G%comm, error)

    if (tProcInfo_G%rank == tProcInfo_G%size-1) then

!      print*, field%bz2, field%bz2PB
      field%bz2 = bz2_globm

    else

      if (field%bz2 <= field%ez2) field%bz2 = field%ez2 + 1  ! For sparse beam!!

    end if


    if (fieldMesh == iPeriodic) then

!      print*, '1', field%bz2

      if (bz2_globm > nz2_G) then
        field%bz2PB = bz2_globm - nz2_G
      else
        field%bz2PB = 3_ip
      end if

! tell last process what the max periodic boundary is...

!      maxbz2PB = 1_ip

      if (tProcInfo_G%qRoot) maxbz2PB = field%mainlen - 1_ip

!      print*, 'field%bz2PB', field%bz2PB

      call mpi_bcast(maxbz2PB, 1, mpi_integer, 0, tProcInfo_G%comm, error)

!      maxbz2PB = 10

      if (field%bz2PB > maxbz2PB) then

        field%bz2PB = maxbz2PB

      end if

! if, for any other process, bz2 goes bigger than bz2 on the last process,
! then it will have to reduce its own bz2 to be OK

      if (tProcInfo_G%rank == tProcInfo_G%size-1) field%bz2 = field%ez2 + field%bz2PB

!      print*, 'field%bz2 = ', field%bz2
!      print*, 'field%bz2PB = ', field%bz2PB
!      print*, 'field%ez2 = ', field%ez2

      bz2last = field%bz2

      call mpi_bcast(bz2last, 1, mpi_integer, tProcInfo_G%size-1, tProcInfo_G%comm, error)

      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then
        if (field%bz2 > nz2_g) then
          field%bz2 = nz2_g
        end if
      end if

      bz2_globm = bz2last

    end if


    if (.not. field%qUnique) then

      field%bz2 = bz2_globm
      field%ez2 = field%bz2
      field%mainlen = field%ez2-field%fz2+1
      field%fbuffLen = 0
      field%tllen = field%mainlen

      allocate(lrank_v(1), rrank_v(1,1), lrfromwhere(1))

    else

      field%fbuffLen = field%bz2 - (field%ez2+1) + 1  ! Local buffer length, NOT including the field%ez2 node
      field%tllen = field%bz2 - field%fz2 + 1     ! local total length, including buffer





      if (tProcInfo_G%rank == tProcInfo_G%size-1) then

        if (fieldMesh == iPeriodic) then

          !field%mainlen = field%tllen
!          print*, 'field%mainlen', field%mainlen
!          print*, 'field%tllen', field%tllen
!          print*, 'nz2_g', nz2_g
!          print*, 'field%bz2PB', field%bz2PB
!          print*, 'field%bz2', field%bz2
          field%tllen = field%mainlen + field%bz2PB
          field%fbuffLen = field%bz2PB
          field%ez2 = nz2_g !field%bz2

        else

          field%mainlen = field%tllen
          field%fbuffLen = 0
          field%ez2 = field%bz2

        end if

      end if



      ! readjust ac_ar with new ez2 for last process
      call setupLayoutArrs(field%mainlen, field%fz2, field%ez2, field%ac_ar)


      field%ez2_GGG = field%ac_ar(tProcInfo_G%size, 3)


      ! count overlap over how many processes....


  !!!   NOW NEED TO RECALC AC_AR TO TAKE INTO ACCOUNT POSSIBLY ADJUSTED
  !!!   BOUNDS ON LAST PROCESS....???

      cpolap = 0

      do ij = 0,tProcInfo_G%size-1

        if  ( (ij > tProcInfo_G%rank) .and. (field%bz2 >= field%ac_ar(ij+1, 2)) ) then

          cpolap = cpolap + 1

        end if

      end do



      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then
        allocate(rrank_v(cpolap, 3))
        !allocate(isnd2u(tProcInfo_G%size))
      else
        allocate(rrank_v(1, 3))
        rrank_v = 1
        !allocate(isnd2u(tProcInfo_G%size))
      end if

      yip = 0
      nsnds_bf = cpolap
      !isnd2u = 0




  !  Count how much I'm sending to each process I'm bounding over...

      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then

        do ij = 0,tProcInfo_G%size-1
  !print*, ij
          if  (   (ij > tProcInfo_G%rank) .and. (field%bz2 >= field%ac_ar(ij+1, 2)) ) then

            yip = yip + 1

            rrank_v(yip,2) = field%ac_ar(ij+1, 2)

            if ((fieldMesh == iPeriodic) .and.  (ij == tProcInfo_G%size-1_ip)) then

              rrank_v(yip,3) = field%bz2

            else

              if (field%bz2 > field%ac_ar(ij+1, 3)) then
                rrank_v(yip,3) = field%ac_ar(ij+1, 3)
              else
                rrank_v(yip,3) = field%bz2
              end if

            end if

            rrank_v(yip,1) = rrank_v(yip,3) - rrank_v(yip,2) + 1

          end if

        end do



      !  send numbers I'm sending to the processes to let them know:

        yip = 0

        nsnd_hs = tProcInfo_G%size - 1 - tProcInfo_G%rank
        allocate(reqs(nsnd_hs), sendstats(MPI_STATUS_SIZE, nsnd_hs))

        do ij = tProcInfo_G%rank + 1, tProcInfo_G%size-1

          yip = yip + 1

          if (ij - tProcInfo_G%rank <= cpolap) then

            !send rrank_v(yip, 1) to tProcInfo_G%rank + yip

            call mpi_issend(rrank_v(yip, 1), 1, mpi_integer, &
                     tProcInfo_G%rank + yip, 0, tProcInfo_G%comm, &
                     reqs(yip), error)

          else

            !send 0 to tProcInfo_G%rank + yip
            izero_hs = 0_ip
            call mpi_issend(izero_hs, 1, mpi_integer, &
                     tProcInfo_G%rank + yip, 0, tProcInfo_G%comm, &
                     reqs(yip), error)

          end if

        end do

      end if

      allocate(drecar(tProcInfo_G%rank))
      ctrecvs = 0

      if (tProcInfo_G%rank /= 0) then

        do ij =  0, tProcInfo_G%rank - 1

          ! dum_recvs from ij

          call mpi_recv(dum_recvs, 1, mpi_integer, ij, &
                  0, tProcInfo_G%comm, statr, error)

          drecar(ij+1) = dum_recvs

          if (dum_recvs > 0) ctrecvs = ctrecvs + 1

        end do

      end if

      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then
        call mpi_waitall(nsnd_hs, reqs, sendstats, error)
        deallocate(reqs, sendstats)
      end if

      if (tProcInfo_G%rank /= 0) then

        allocate(lrank_v(ctrecvs))
        allocate(lrfromwhere(ctrecvs))

      else

        allocate(lrank_v(1))
        allocate(lrfromwhere(1))
        lrank_v = 1
        lrfromwhere = 1

      end if

      nrecvs_bf = ctrecvs

      if (tProcInfo_G%rank /= 0) then

        !nrecvs2me = count(drecar > 0)

        yip = 0

        do ij = 0, tProcInfo_G%rank - 1


          if (drecar(ij+1) > 0) then

            yip = yip+1
            lrank_v(yip) = drecar(ij+1)
            lrfromwhere(yip) = ij  ! rank recieving info from

          end if

        end do

      end if

    end if


!    call mpi_barrier(tProcInfo_G%comm, error)
!    print*, 'SENT AND RECVD BOSS!!!'
!    print*, tProcInfo_G%rank, 'has lrank_v = ', lrank_v!, size(lrank_v), ctrecvs
!    print*, tProcInfo_G%rank, 'has rrank_v = ', rrank_v
!    print*, tProcInfo_G%rank, 'has lrfromwhere = ', lrfromwhere
!    print*, tProcInfo_G%rank, 'has nrecvs_bf = ', nrecvs_bf
!    print*, tProcInfo_G%rank, 'has nsnds_bf = ', nsnds_bf


!    call mpi_finalize(error)
!    stop

!!  !!!!!!! !!!!!  AND NOW EZ2 OF LAST PROCESS SHOULD EQUAL *GLOBAL* BZ2!!!
!!
!!
!!
!!  !!!!!!! !!!!!  SO EQUALS GLOBAL MAXIMUM SO NO ACCIDENTAL OVERLAP INTO BANDIT TERRITORY!!!
!!
!!
!!
!!  !!!!!!! !!!!!  THIS SHOULD PROBABLY BE SET BEFORE THE SEND AND RECV ARRAYS ARE SET UP!!!!



!  ----      OLD
!     Send buffer length to process on the right - as the right process
!     will be updating out local 'buffer' region

!    field%fbuffLenM = 1
!
!    if (tProcInfo_G%rank /= tProcInfo_G%size-1) then
!
!!        send to rank+1
!
!      call mpi_issend(field%fbuffLen, 1, mpi_integer, tProcInfo_G%rank+1, 4, &
!             tProcInfo_G%comm, req, error)
!
!    end if
!
!    if (tProcInfo_G%rank /= 0) then
!
!!       rec from rank-1
!
!      CALL mpi_recv( field%fbuffLenM,1,MPI_INTEGER,tProcInfo_G%rank-1,4, &
!             tProcInfo_G%comm,statr,error )
!
!!      call mpi_wait( statr,sendstat,error )
!
!    end if
!
!    if (tProcInfo_G%rank /= tProcInfo_G%size-1) call mpi_wait( req,sendstat,error )
!
!
!
!
!    call mpi_barrier(tProcInfo_G%comm, error)
!
!    print* , tProcInfo_G%rank, 'is inside calcBuff, with field%fz2, field%ez2, field%bz2 of = ', field%fz2, field%ez2, field%bz2, &
!    'and lens of ', field%mainlen, field%tllen, field%fbuffLen, field%fbuffLenM
!    -----    OLD

!    call mpi_finalize(error)
!    stop


  end subroutine calcBuff










  subroutine getFStEnd(field)

    type(tFieldValues), intent(inout) :: field

    integer(kind=ip) :: fz2_act, ez2_act

    integer :: error
    integer(kind=ip) :: n_act_g
    integer(kind=ip) :: rbuff

! get global start and end nodes for the active region


! (find min and max electron z2's)



! Defensive default: only the iElectronBased/iFieldBased branches below
! set fz2_act/ez2_act; the invalid-basis branch prints an error but
! otherwise leaves them undefined.
    fz2_act = 0_ip
    ez2_act = 0_ip

    if (field%iParaBas == iElectronBased) then

      fz2_act = minval(ceiling(sElZ2_G / sLengthOfElmZ2_G))   ! front z2 node in 'active' region

!      print*, tProcInfo_G%rank, 'and I have fz2_act = ', fz2_act

      CALL mpi_allreduce(fz2_act, rbuff, 1, mpi_integer, &
               mpi_min, tProcInfo_G%comm, error)

!      print*, tProcInfo_G%rank, 'and then my reduced fz2_act = ', rbuff



      fz2_act = rbuff
      field%fz2_GGG = fz2_act

!print*, tProcInfo_G%rank, 'and so my fz2_act remains = ', fz2_act

!print*, 'fz2_act = ', fz2_act

      ez2_act = maxval(ceiling(sElZ2_G / sLengthOfElmZ2_G) + 1)

!      print*, tProcInfo_G%rank, 'and I have ez2_act = ', ez2_act

      CALL mpi_allreduce(ez2_act, rbuff, 1, mpi_integer, &
               mpi_max, tProcInfo_G%comm, error)

      ez2_act = rbuff
      field%ez2_GGG = ez2_act

!print*, 'ez2_act = ', ez2_act

    else if (field%iParaBas == iFieldBased) then    !    FIELD based - also used for initial steps...

      fz2_act = 1_ip
      ez2_act = NZ2_G
      field%fz2_GGG = 1
      field%ez2_GGG = 1

    else

      print*, "NO BASIS FOR PARALLELISM SELECTED!!!"

    end if

    n_act_g = ez2_act - fz2_act + 1


    !print*, tProcInfo_G%rank, 'and then my fz2_act is STILL = ', fz2_act




! get local start and end nodes for the active region

! (use divNodes)


    if (n_act_g < 2*tProcInfo_G%size) then ! If too many nodes

      field%qUnique = .false.

      if (ioutInfo_G > 0) then
        print*, "So WHY AM I HERE, WITH nz2 = ", nz2_G
        print*, "n_act_g = ", n_act_g
        print*, "fz2_act = ", fz2_act
        print*, "ez2_act = ", ez2_act
      end if


      field%fz2 = field%fz2_GGG
      field%ez2 = field%ez2_GGG

      field%mainlen = n_act_g

    else

      field%qUnique = .true.

      call divNodes(n_act_g, tProcInfo_G%size, tProcInfo_G%rank, &
                    field%tllen, field%fz2, field%ez2)

      field%fz2 = field%fz2 + fz2_act - 1
      field%ez2 = field%ez2 + fz2_act - 1

      field%mainlen = field%ez2 - field%fz2 + 1     ! local length, NOT including buffer

    end if

!    print*, 'n_act_g was ', n_act_g
!    print*, 'local now ', field%tllen, field%fz2, field%ez2, fz2_act





  end subroutine getFStEnd



  subroutine getFrBk(field)

! Get array indices of front and back nodes
! depending on active region nodes

    type(tFieldValues), intent(inout) :: field

    integer(kind=ip) :: efz2_MG, ebz2_MG
    integer :: error

      if (tProcInfo_G%qRoot) efz2_MG = field%fz2 - 1

      call MPI_BCAST(efz2_MG,1, mpi_integer, 0, &
                      tProcInfo_G%comm,error)



      if (fieldMesh == iPeriodic) then

        field%tlflen_glob = 0
        field%tlflen = 0
        field%tlflen4arr = 1
        field%ffs = 0
        field%ffe = 0
        field%ffs_GGG = 0
        field%ffe_GGG = 0

      else

        if (efz2_MG < 1) then

!     then there is no front section of the field...

          field%tlflen_glob = 0
          field%tlflen = 0
          field%tlflen4arr = 1
          field%ffs = 0
          field%ffe = 0
          field%ffs_GGG = 0
          field%ffe_GGG = 0

        else if (efz2_MG > 0) then

          field%tlflen_glob = efz2_MG

          call divNodes(efz2_MG,tProcInfo_G%size, &
                        tProcInfo_G%rank, &
                        field%tlflen, field%ffs, field%ffe)

          field%tlflen4arr = field%tlflen

          field%ffs_GGG = 1
          field%ffe_GGG = efz2_MG


        end if

      end if

      CALL MPI_ALLGATHER(field%tlflen, 1, MPI_INTEGER, &
              field%ff_ar(:,1), 1, MPI_INTEGER, &
              tProcInfo_G%comm, error)

      CALL MPI_ALLGATHER(field%ffs, 1, MPI_INTEGER, &
              field%ff_ar(:,2), 1, MPI_INTEGER, &
              tProcInfo_G%comm, error)

      CALL MPI_ALLGATHER(field%ffe, 1, MPI_INTEGER, &
              field%ff_ar(:,3), 1, MPI_INTEGER, &
              tProcInfo_G%comm, error)




!    print*, 'FRONT ARRAY IS ', field%ff_ar




      ! get rightmost bz2 ...(last process)
      ! ebz2_MG - extreme back z2 node of active region plus 1

      if (tProcInfo_G%rank == tProcInfo_G%size-1) ebz2_MG = field%bz2 + 1


      call MPI_BCAST(ebz2_MG,1, mpi_integer, tProcInfo_G%size-1, &
                      tProcInfo_G%comm,error)



      if (fieldMesh == iPeriodic) then

        field%tlelen_glob = 0
        field%tlelen = 0
        field%tlelen4arr = 1
        field%ees = 0
        field%eee = 0
        field%ees_GGG = 0
        field%eee_GGG = 0

      else

        if (ebz2_MG > NZ2_G) then

!     then there is no back section of the field...

          field%tlelen_glob = 0
          field%tlelen = 0
          field%tlelen4arr = 1
          field%ees = 0
          field%eee = 0

        else if (ebz2_MG < nz2_G + 1) then

          field%tlelen_glob = nz2_G - ebz2_MG + 1

!        print*, 'I get the field%tlelen_glob to be ', field%tlelen_glob

          call divNodes(field%tlelen_glob,tProcInfo_G%size, &
                        tProcInfo_G%rank, &
                        field%tlelen, field%ees, field%eee)

          field%ees = field%ees + ebz2_MG - 1
          field%eee = field%eee + ebz2_MG - 1

          field%ees_GGG = ebz2_MG
          field%eee_GGG = NZ2_G

          field%tlelen4arr = field%tlelen

!        print*, '...and the start nd end of the back to be', field%ees, field%eee

        end if

      end if

      CALL MPI_ALLGATHER(field%tlelen, 1, MPI_INTEGER, &
              field%ee_ar(:,1), 1, MPI_INTEGER, &
              tProcInfo_G%comm, error)

      CALL MPI_ALLGATHER(field%ees, 1, MPI_INTEGER, &
              field%ee_ar(:,2), 1, MPI_INTEGER, &
              tProcInfo_G%comm, error)

      CALL MPI_ALLGATHER(field%eee, 1, MPI_INTEGER, &
              field%ee_ar(:,3), 1, MPI_INTEGER, &
              tProcInfo_G%comm, error)

!        print*, '...so back array = ', field%ee_ar



  end subroutine getFrBk


  subroutine redist2new2(old_dist, new_dist, field_old, field_new)

  implicit none (type, external)

! Alternative subroutine to redistribute the field values in field_old
! to field_new. The layout of the field in field_old is
! described in old_dist, and the layout of the new field
! is described in new_dist. This subroutine uses mpi_alltoallv, in
! contrast to redist2new which uses mpi the sends and recvs

! inputs

    integer(kind=ip), intent(in) :: old_dist(:,:), new_dist(:,:)
    real(kind=wp), intent(inout) :: field_old(:), field_new(:)

! local

    integer(kind=ip), allocatable :: send_ptrs(:,:), recv_ptrs(:,:)

    integer(kind=ip), allocatable :: sdispls(:), rdispls(:)


    integer(kind=ip), allocatable :: nsends(:), nrecvs(:)
    integer :: error, ij



    allocate(send_ptrs(tProcInfo_G%size, 3))
    allocate(recv_ptrs(tProcInfo_G%size, 3))

    allocate(sdispls(tProcInfo_G%size))
    allocate(rdispls(tProcInfo_G%size))

    allocate(nsends(tProcInfo_G%size), nrecvs(tProcInfo_G%size))

    call golaps(old_dist(tProcInfo_G%rank+1,2), &
                old_dist(tProcInfo_G%rank+1,3), &
                new_dist, send_ptrs)

    call golaps(new_dist(tProcInfo_G%rank+1,2), &
                new_dist(tProcInfo_G%rank+1,3), &
                old_dist, recv_ptrs)



    nsends = send_ptrs(:,1) * ntrnds_G
    nrecvs = recv_ptrs(:,1) * ntrnds_G

!            nbase = new_dist(iproc_r+1, 2) - 1

!            st_ind_new = send_ptrs(iproc_r+1, 2) - nbase
!            st_ind_new = (st_ind_new - 1)* ntrnds_G + 1



!    send_ptrs(iproc_r+1, 2) - nbase



    sdispls = ( (send_ptrs(:,2) - (old_dist(tProcInfo_G%rank+1, 2) - 1) ) - 1_ip) * ntrnds_G


!    do ij = 1, tProcInfo_G%size

!      if ((ij == 1) .and. () )

!      sdispls(ij) = ( (send_ptrs(ij,2) - (new_dist(ij, 2) - 1) ) - 1_ip) * ntrnds_G

!    end do



    rdispls = ( (recv_ptrs(:,2) - (new_dist(tProcInfo_G%rank+1, 2) - 1) ) - 1_ip) * ntrnds_G

    do ij = 1, tProcInfo_G%size

      if (send_ptrs(ij,1) == 0) then

        if (ij-1 == 0) then

          sdispls(ij) = 0_ip

        else

          sdispls(ij) = sdispls(ij-1)! + nsends(ij-1)

        end if

      end if

    end do


    do ij = 1, tProcInfo_G%size

      if (recv_ptrs(ij,1) == 0) then

        if (ij-1 == 0) then

          rdispls(ij) = 0_ip

        else

          rdispls(ij) = rdispls(ij-1)! + nrecvs(ij-1)

        end if

      end if

    end do


!    print*, tProcInfo_G%rank, 'nrecvs = ', nrecvs
!    print*, tProcInfo_G%rank, 'rdispls = ', rdispls
!    print*, tProcInfo_G%rank, 'recv_ptrs(:,2) = ', recv_ptrs(:,2)
!    print*, tProcInfo_G%rank, 'nsends = ', nsends
!    print*, tProcInfo_G%rank, 'sdispls = ', sdispls
!    print*, tProcInfo_G%rank, 'send_ptrs(:,2) = ', send_ptrs(:,2)

!    call mpi_barrier(tProcInfo_G%comm, error)

    call mpi_alltoallv(field_old, nsends, sdispls, mpi_double_precision, &
                       field_new, nrecvs, rdispls, mpi_double_precision, &
                       tProcInfo_G%comm, error)


    deallocate(nsends, nrecvs)
    deallocate(send_ptrs, recv_ptrs)
    deallocate(sdispls, rdispls)

!    print*, 'called me NOW'

!    call mpi_barrier(tProcInfo_G%comm, error)
!    call mpi_finalize(error)
!    stop

  end subroutine redist2new2










  subroutine redist2new(old_dist, new_dist, field_old, field_new)


! Subroutine to redistribute the field values in field_old
! to field_new. The layout of the field in field_old is
! described in old_dist, and the layout of the new field
! is described in new_dist.

    ! inputs

    integer(kind=ip), intent(in) :: old_dist(:,:), new_dist(:,:)
    real(kind=wp), intent(inout) :: field_old(:), field_new(:)

    ! local

    integer(kind=ip) :: iproc_s, iproc_r
    integer(kind=ip) :: st_ind_new, ed_ind_new, &
                        st_ind_old, ed_ind_old, &
                        nbase, obase
    integer(kind=ip), allocatable :: send_ptrs(:,:)

    integer :: error, req
    integer :: statr(MPI_STATUS_SIZE)



    allocate(send_ptrs(tProcInfo_G%size, 3))

    ! calc overlaps from MPI process 'iproc_s',
    ! then loop round, if size_olap>0 then if
    ! rank==iproc_s send, else if rank==iproc_r
    ! recv, unless iproc_r==iproc_s then just
    ! direct assignment


!    if (tProcInfo_G%qroot) print*, 'NOW, for new dist described by ', new_dist,'....', &
!                               ' and old dist of ', old_dist

    do iproc_s = 0, tProcInfo_G%size-1   !  maybe do iproc_s = rank, rank-1 (looped round....)

      call golaps(old_dist(iproc_s+1,2), old_dist(iproc_s+1,3), new_dist, send_ptrs)

!      if (tProcInfo_G%qroot) print*, 'olaps are ', send_ptrs, 'for old nodes ', &
!          old_dist(iproc_s+1,2), 'to', old_dist(iproc_s+1,3)

!      call mpi_barrier(tProcInfo_G%comm, error)

      do iproc_r = 0, tProcInfo_G%size-1

        if (send_ptrs(iproc_r+1,1) > 0 ) then

          if ((tProcInfo_G%rank == iproc_r) .and. (iproc_r == iproc_s) ) then

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


            if (tProcInfo_G%rank == iproc_s) then

!              print*, 'SD st_ind_old = ', st_ind_old, ed_ind_old, size(field_old), &
!              send_ptrs(iproc_r+1,1)

              call mpi_issend(field_old(st_ind_old:ed_ind_old), &
                          send_ptrs(iproc_r+1,1)*ntrnds_G, &
                          mpi_double_precision, iproc_r, 0, tProcInfo_G%comm, req, error)

!              print*, 'SENDING', field_old(st_ind_old:ed_ind_old)

            else if (tProcInfo_G%rank == iproc_r) then

!              print*, 'SD st_ind_new = ', st_ind_new, ed_ind_new, size(field_new), &
!                           send_ptrs(iproc_r+1,1)

              call mpi_recv(field_new(st_ind_new:ed_ind_new), &
                            send_ptrs(iproc_r+1,1)*ntrnds_G, &
                            mpi_double_precision, iproc_s, 0, tProcInfo_G%comm, statr, error)

!              print*, 'RECIEVED', field_new(st_ind_new:ed_ind_new)

            end if

            !if (tProcInfo_G%rank == iproc_s) call mpi_wait( req,sendstat,error )
  !          call mpi_barrier(tProcInfo_G%comm, error)

   !         call mpi_finalize(error)
   !         stop


          end if

        end if

      end do

!    if (tProcInfo_G%rank == iproc_s) call mpi_wait( req,sendstat,error )

    end do

    deallocate(send_ptrs)

    call mpi_barrier(tProcInfo_G%comm, error)

   end subroutine redist2new




  subroutine setupLayoutArrs(len, st_ind, ed_ind, arr)

!
! Subroutine to setup the arrays storing the size, start and
! end pointers of the local field arrays.
!

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



  subroutine rearrElecs(field)

  implicit none (type, external)

  type(tFieldValues), intent(in) :: field

  integer :: error
  integer(kind=ip) :: iproc, iproc_r, iproc_s
  integer(kind=ip), allocatable :: cnt2proc(:), &
                                   inds4sending(:)
  real(kind=wp), allocatable :: sElZ2_OLD(:), sElX_OLD(:), &
                                sElY_OLD(:), sElGam_OLD(:), &
                                sElPX_OLD(:), sElPY_OLD(:), &
                                s_chi_bar_OLD(:), &
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

!      where ( (sElZ2_G(imp) >= (sLengthOfElmZ2_G * (field%ac_ar(:,2)-1))) .and. &
!                  (sElZ2_G(imp) >= (sLengthOfElmZ2_G * (field%ac_ar(:,3)-1) )) )

!        cnt2proc = cnt2proc + 1

!      end where

!    end do

! OR
    do iproc = 0, tProcInfo_G%size-1

      icds = count(sElZ2_G > (sLengthOfElmZ2_G * (field%ac_ar(iproc+1, 2)-1)) .and. &
                    (sElZ2_G <= (sLengthOfElmZ2_G * field%ac_ar(iproc+1, 3))) )

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
                    (sLengthOfElmZ2_G * (field%ac_ar(iproc_r+1,2)-1)), &
                    (sLengthOfElmZ2_G * field%ac_ar(iproc_r+1,3)) )

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
                    (sLengthOfElmZ2_G * (field%ac_ar(iproc_r+1,2)-1) ), &
                    (sLengthOfElmZ2_G * field%ac_ar(iproc_r+1,3)) )

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

                !       call mpi_issend(field%fz2, 1, mpi_integer, tProcInfo_G%rank-1, 0, &
!            tProcInfo_G%comm, req, error)


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






  subroutine getinds(inds, array, lower, upper)

  ! getInds
  !
  ! Subroutine to return the indices of elements in the array
  ! which lie between the upper and lower bounds.
  !
  !
  !
  !

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



  subroutine sendArrPart(array, inds, cnt, tmparray, iproc)

!
! Subroutine to send part of an array specified by
! the indices in 'inds'.
!
! cnt - count of the number of elements to be sent
! inds - integer array of the indices of array to be sent
! array - real number array of values, of which only part will be
!         sent as specified in inds
! tmparray - An array used to temporarily store the values of
!            'array' to be sent - should be AT LEAST of size
!            cnt, but may be larger.
!


    real(kind=wp), intent(in) :: array(:)
    real(kind=wp), intent(inout) :: tmparray(:)
    integer(kind=ip), intent(in) :: inds(:)
    integer(kind=ip), intent(in) :: cnt
    integer(kind=ip), intent(in) :: iproc

    integer :: req, error
    integer :: sendstat(MPI_STATUS_SIZE)

    tmparray(1:cnt) = array(inds)

    call mpi_issend(tmparray(1:cnt), cnt, mpi_double_precision, &
                   iproc, 0, tProcInfo_G%comm, req, error)

    call mpi_wait( req,sendstat,error )

  end subroutine sendArrPart


  subroutine recvArrPart(array, cnt, st_ind, ed_ind, iproc)

! Recv section of array, specified by start and end
! indices st_ind and ed_ind.
!
!

    real(kind=wp), intent(inout) :: array(:)
    integer(kind=ip), intent(inout) :: cnt
    integer(kind=ip), intent(in) :: iproc
    integer(kind=ip), intent(in) :: st_ind, ed_ind

    integer :: error
    integer :: statr(MPI_STATUS_SIZE)


    call mpi_recv(array(st_ind:ed_ind), cnt, &
                  mpi_double_precision, iproc, &
                  0, tProcInfo_G%comm, statr, error)

  end subroutine recvArrPart


!!!!~#####################################################################################



  subroutine redist2FFTWlt(field)

    implicit none (type, external)

    type(tFieldValues), intent(inout) :: field

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


    allocate(field%tre_fft(tmpmainlen*ntrnds_G), &
             field%tim_fft(tmpmainlen*ntrnds_G))


    field%tre_fft = 0_wp
    field%tim_fft = 0_wp


    allocate(field%ft_ar(tProcInfo_G%size, 3))
    call setupLayoutArrs(tmpmainlen, tmpfz2, tmpez2, field%ft_ar)

!    print*, 'fft array layout is ', field%ft_ar


    call redist2new2(field%ff_ar, field%ft_ar, field%fr_r, field%tre_fft)
    call redist2new2(field%ff_ar, field%ft_ar, field%fr_i, field%tim_fft)


    call redist2new2(field%ee_ar, field%ft_ar, field%bk_r, field%tre_fft)
    call redist2new2(field%ee_ar, field%ft_ar, field%bk_i, field%tim_fft)



    call redist2new2(field%ac_ar, field%ft_ar, field%ac_r, field%tre_fft)
    call redist2new2(field%ac_ar, field%ft_ar, field%ac_i, field%tim_fft)


  end subroutine redist2FFTWlt






  subroutine redistbackFFT(field)

    implicit none (type, external)

    type(tFieldValues), intent(inout) :: field

    integer :: req, error
    integer(kind=ip) :: si, sst, sse
    integer :: statr(MPI_STATUS_SIZE)
    integer :: sendstat(MPI_STATUS_SIZE)

    call redist2new2(field%ft_ar, field%ff_ar, field%tre_fft, field%fr_r)
    call redist2new2(field%ft_ar, field%ff_ar, field%tim_fft, field%fr_i)


    call redist2new2(field%ft_ar, field%ee_ar, field%tre_fft, field%bk_r)
    call redist2new2(field%ft_ar, field%ee_ar, field%tim_fft, field%bk_i)



    call redist2new2(field%ft_ar, field%ac_ar, field%tre_fft, field%ac_r)
    call redist2new2(field%ft_ar, field%ac_ar, field%tim_fft, field%ac_i)


    deallocate(field%tre_fft, field%tim_fft)
    deallocate(field%ft_ar)


    if (fieldMesh == iPeriodic) then

      si = (nx_g * ny_g) * (field%bz2PB + 1_ip)
      sst = ((field%tllen - (field%bz2PB+1_ip) ) * (nx_g * ny_g)) + 1_ip
      sse = field%tllen * (nx_g * ny_g)

      if (tProcInfo_G%rank == 0_ip) then

        call mpi_issend(field%ac_r(1:si), &
                        si, &
                        mpi_double_precision, &
                        tProcInfo_G%size-1_ip, 0, &
                        tProcInfo_G%comm, req, error)

      end if



      if (tProcInfo_G%rank == tProcInfo_G%size-1_ip) then

        call mpi_recv( field%ac_r(sst:sse), &
                 si, mpi_double_precision, &
                 0, 0, tProcInfo_G%comm, &
                 statr, error )

      end if


      if (tProcInfo_G%rank == 0_ip) then

        call mpi_wait( req,sendstat,error )
        call mpi_issend(field%ac_i(1:si), &
                        si, &
                        mpi_double_precision, &
                        tProcInfo_G%size-1_ip, 0, &
                        tProcInfo_G%comm, req, error)

      end if




      if (tProcInfo_G%rank == tProcInfo_G%size-1_ip) then

        call mpi_recv( field%ac_i(sst:sse), &
                 si, mpi_double_precision, &
                 0, 0, tProcInfo_G%comm, &
                 statr, error )

      end if



      if (tProcInfo_G%rank == 0_ip) then

        call mpi_wait( req,sendstat,error )

      end if

    end if

  end subroutine redistbackFFT



end module ParaField
