

module ParaField


use paratype
use globals
use ParallelSetUp

implicit none

real(kind=wp), allocatable :: fr_rfield(:), bk_rfield(:), ac_rfield(:), &
                              fr_ifield(:), bk_ifield(:), ac_ifield(:)

real(kind=wp), allocatable :: tmp_A(:)

integer(kind=ip), allocatable :: recvs_pf(:), displs_pf(:)

integer(kind=ip) :: fz2, ez2, lTr, bz2, fbuffLen, fbuffLenM, tllen, mainlen

contains


	subroutine getLocalFieldIndices(sdz, sA)

      implicit none

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

      real(kind=wp), intent(in) :: sdz, sA(:)

      real(kind=wp), allocatable :: sp2(:)

      integer(kind=ip) :: fz2_r, req, error, gath_v
      integer sendstat(MPI_STATUS_SIZE)
      integer statr(MPI_STATUS_SIZE)


      print*, 'INSIDE GETLOCALFIELDINDICES, SIZE OF SA IS ', size(sA)

      fz2 = minval(ceiling(sElZ2_G / sLengthOfElmZ2_G))

      ez2 = maxval(ceiling(sElZ2_G / sLengthOfElmZ2_G) + 1) 

!      allocate(sp2(size(sElGam_G)))

!      call getP2(sp2, sElGam_G, sElPX_G, sElPY_G, sEta_G, sGammaR_G, saw_G)

!      lTr = maxval(sElZ2_G + sp2)

      bz2 = ez2 + nint(4 * 4 * pi * sRho_G / sLengthOfElmZ2_G)   ! Boundary only 4 lambda_r long - so can only go ~ 3 periods



      print*, 'INSIDE GETLOCALFIELDINDICES, SIZE OF SA AT 1 IS ', size(sA)



      if (tProcInfo_G%rank /= 0) then

!        send to rank-1

        print*, 'sending fz2 of ', fz2
        call mpi_issend(fz2, 1, mpi_integer, tProcInfo_G%rank-1, 0, &
        	 tProcInfo_G%comm, req, error)


      end if



      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then

!       rec from rank+1

        CALL mpi_recv( fz2_r, 1, mpi_long, tProcInfo_G%rank+1, 0, &
        	tProcInfo_G%comm, statr, error )  

      end if

      if (tProcInfo_G%rank /= 0) call mpi_wait( req,sendstat,error )
      if (tProcInfo_G%rank==0)  print*, 'received fz2_r of ', fz2_r


      print*, 'INSIDE GETLOCALFIELDINDICES, SIZE OF SA AT 2 IS ', size(sA), &
        ' FOR PROCESSOR ', tProcInfo_G%rank


      if (tProcInfo_G%rank == 0) fz2 = 1

      if (tProcInfo_G%rank == tProcInfo_G%size-1) bz2 = size(sA) / 2
      if (tProcInfo_G%rank == tProcInfo_G%size-1) ez2 = bz2





      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then

      if (fz2_r /= ez2) then

        print*, 'WARNING, fixing parallel field bounds... on proc', tProcInfo_G%rank, &
                ' ez2 was ', ez2, ' is now ', fz2_r
        ez2 = fz2_r

      end if

      end if

      fbuffLen = bz2 - ez2 + 1  ! Local buffer length, including the ez2 node
      tllen = bz2 - fz2 + 1     ! local total length, including buffer
      mainlen = ez2 - fz2 + 1     ! local length, NOT including buffer




      print*, 'INSIDE GETLOCALFIELDINDICES, SIZE OF SA AT 3 IS ', size(sA), &
        ' FOR PROCESSOR ', tProcInfo_G%rank


      fbuffLenM = 1

      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then

!        send to rank+1

        call mpi_issend(fbuffLen, 1, mpi_integer, tProcInfo_G%rank+1, 0, tProcInfo_G%comm, req, error)

      end if

      if (tProcInfo_G%rank /= 0) then

!       rec from rank-1

        CALL mpi_recv( fbuffLenM,1,MPI_INTEGER,tProcInfo_G%rank-1,0,tProcInfo_G%comm,statr,error )  

      end if

      if (tProcInfo_G%rank /= tProcInfo_G%size-1) call mpi_wait( req,sendstat,error )




      print*, 'INSIDE GETLOCALFIELDINDICES, SIZE OF SA AT 4 IS ', size(sA), &
              ' FOR PROCESSOR ', tProcInfo_G%rank





      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then
      	gath_v = mainlen-1
      else
      	gath_v = mainlen
      end if

      allocate(recvs_pf(tProcInfo_G%size), displs_pf(tProcInfo_G%size))
      call getGathArrs(gath_v, recvs_pf, displs_pf)


      ! Allocate back, front and active fields....commented out!
      ! ONLY USING ACTIVE FIELD FOR NOW TO CHECK SCALING...TO SEE
      ! IF IT'S WORTH PERSUING THIS METHOD
      !allocate(fr_rfield(tllen), bk_rfield(tllen), ac_rfield(tllen), &
      !         fr_ifield(tllen), bk_ifield(tllen), ac_ifield(tllen))

      allocate(ac_rfield(tllen), &
               ac_ifield(tllen))

      ac_rfield = sA(fz2:bz2)
      ac_ifield = sA(fz2 + NZ2_G:bz2 + NZ2_G)

      print*, 'INSIDE GETLOCALFIELDINDICES, SIZE OF SA AT 5 IS ', size(sA), &
              ' FOR PROCESSOR ', tProcInfo_G%rank



      allocate(tmp_A(fbuffLenM))

      tmp_A = 0_wp

      print*, tProcInfo_G%rank, ' set up with bounds of ', fz2, ez2, bz2, &
      'with a buffer length of ', fbuffLen, 'and a total length of ', tllen
      print*, tProcInfo_G%rank, ' and size of sA (over 2) is ', size(sA) / 2

    end subroutine getLocalFieldIndices


!  ###################################################


    subroutine UpdateGlobalField(sA)

      real(kind=wp), intent(inout) :: sA(:)

      real(kind=wp), allocatable :: A_local(:)

      integer(kind=ip) :: gath_v

      integer error


      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then
      	gath_v = mainlen-1
      else
        gath_v = mainlen
      end if




      allocate(A_local(gath_v * 2))

      A_local = 0_wp

      A_local(1:gath_v) = ac_rfield(1:gath_v)
      A_local(gath_v+1:gath_v*2) = ac_ifield(1:gath_v)

      call gather2A(A_local, sA, gath_v, NZ2_G, recvs_pf, displs_pf)



      deallocate(A_local)

    end subroutine UpdateGlobalField




!  ###################################################  




    subroutine upd8da(dadz_r, dadz_i)

    ! Send dadz from buffer to MPI process on the right
    ! Data is added to array in next process, not
    ! written over.

      real(kind=wp), intent(inout) :: dadz_r(:), dadz_i(:)

      integer(kind=ip) :: req, error
      integer statr(MPI_STATUS_SIZE)
      integer sendstat(MPI_STATUS_SIZE)


      tmp_A = 0_wp

      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then

!        send to rank+1

        call mpi_issend(dadz_r(ez2-(fz2-1):bz2-(fz2-1)), fbuffLen, mpi_double_precision, &
              tProcInfo_G%rank+1, 0, tProcInfo_G%comm, req, error)

      end if

      if (tProcInfo_G%rank /= 0) then

!       rec from rank-1

        CALL mpi_recv( tmp_A, fbuffLenM, mpi_double_precision, &
        	     tProcInfo_G%rank-1, 0, tProcInfo_G%comm, statr, error )  

      end if

      if (tProcInfo_G%rank /= tProcInfo_G%size-1) call mpi_wait( req,sendstat,error )

      dadz_r(1:fbuffLenM) = dadz_r(1:fbuffLenM) + tmp_A





      tmp_A = 0_wp


      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then

!        send to rank+1

        call mpi_issend(dadz_i(ez2-(fz2-1):bz2-(fz2-1)), fbuffLen, mpi_double_precision, &
        	     tProcInfo_G%rank+1, 0, tProcInfo_G%comm, req, error)

      end if

      if (tProcInfo_G%rank /= 0) then

!       rec from rank-1

        CALL mpi_recv( tmp_A, fbuffLenM, mpi_double_precision, &
        	    tProcInfo_G%rank-1, 0, tProcInfo_G%comm, statr, error )  

      end if

      if (tProcInfo_G%rank /= tProcInfo_G%size-1) call mpi_wait( req,sendstat,error )

      dadz_i(1:fbuffLenM) = dadz_i(1:fbuffLenM) + tmp_A



    end subroutine upd8da





!  ###################################################





    subroutine upd8a(ac_rl, ac_il)

      implicit none

    ! Send sA from buffer to process on the left
    ! Data in 'buffer' on the left is overwritten.

      real(kind=wp), intent(inout) :: ac_rl(tllen), ac_il(tllen)

      integer(kind=ip) :: req, error
      integer statr(MPI_STATUS_SIZE)
      integer sendstat(MPI_STATUS_SIZE)

      real(kind=wp), allocatable :: tstf(:), tstf2(:)




      if (tProcInfo_G%rank /= 0) then

!        send to rank-1

        call mpi_issend(ac_rl(1:fbuffLenM), fbuffLenM, mpi_double_precision, &
                           tProcInfo_G%rank-1, 0, tProcInfo_G%comm, req, error)

      end if



      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then

!       rec from rank+1

        CALL mpi_recv( ac_rl(ez2-(fz2-1):bz2-(fz2-1)), fbuffLen, mpi_double_precision, &
                    tProcInfo_G%rank+1, 0, tProcInfo_G%comm, statr, error )  

      end if





      if (tProcInfo_G%rank /= 0) call mpi_wait( req,sendstat,error )



      if (tProcInfo_G%rank /= 0) then

!        send to rank-1

        call mpi_issend(ac_il(1:fbuffLenM), fbuffLenM, mpi_double_precision, &
                tProcInfo_G%rank-1, 0, tProcInfo_G%comm, req, error)

      end if

      if (tProcInfo_G%rank /= tProcInfo_G%size-1) then

!       rec from rank+1

        CALL mpi_recv( ac_il(ez2-(fz2-1):bz2-(fz2-1)), fbuffLen, mpi_double_precision, &
               tProcInfo_G%rank+1, 0, tProcInfo_G%comm, statr, error )  

      end if

      if (tProcInfo_G%rank /= 0) call mpi_wait( req,sendstat,error )



    end subroutine upd8a




end module ParaField





