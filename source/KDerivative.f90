!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Module which calculates d/dz of the local electron and field variables, and 
!> then sums the global regions together.

module Derivative

! Module to calculate derivative required to integrate
! using rk4

use rhs
use ParaField

implicit none

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!> Subroutine which calculate d/dz of the local electron and field 
!> variables, and then sums the global regions together.
!> @param sz position in undulator module.
!> @param sAr real field 
!> @param sAi imaginary field 
!> @param sx electron macroparticles' x position


  subroutine derivs(sz, sAr, sAi, sEAr, sEAi, sx, sy, sz2, spr, spi, sp2, &
                    sdx, sdy, sdz2, sdpr, sdpi, sdp2, sdAr, sdAi, sEdAr, sEdAi)

  implicit none

! External subroutine returning dydz at z for use with RK4
!
!                 ARGUMENTS
!
! sz      INPUT     value of z to evaluate derivative at
! sy      INPUT     Value of y at this z
! sdydz   OUTPUT    Derivative of z and y

    real(kind=wp), intent(in)  :: sz
    real(kind=wp), intent(in)  :: sAr(:), sAi(:)
    real(kind=wp), intent(inout)  :: sEAr(:,:), sEAi(:,:)

    real(kind=wp), intent(in)  :: sx(:), sy(:), sz2(:), &
                                  spr(:), spi(:), sp2(:)

    real(kind=wp), intent(inout)  :: sdx(:), sdy(:), sdz2(:), &
                                  sdpr(:), sdpi(:), sdp2(:)

    real(kind=wp), intent(inout) :: sdAr(:), sdAi(:)
    real(kind=wp), intent(inout) :: sEdAr(:,:), sEdAi(:,:)

!                 LOCAL ARGS
!
! qOKL      Local error flag
! sb        Vector holding the right hand sides

!    real(kind=wp), allocatable :: ldadz(:)
    integer(kind=ip) :: error, iArEr
    logical :: qOKL

!    allocate(LDADz(ReducedNX_G*ReducedNY_G*NZ2_G*2))
!    ldadz = 0.0_WP   ! Local dadz (MPI)




    sEAr(:,:) = 0.0_wp
    sEAi(:,:) = 0.0_wp

    sEdAr(:,:) = 0.0_wp
    sEdAi(:,:) = 0.0_wp

    call popElms(sEAr, sEAi, sAr, sAi)


!     Get RHS of field eqn and d/dz of electron variables

    CALL getrhs(sz, &
                sEAr, sEAi, &
                sx, sy, sz2, &
                spr, spi, sp2, &
                sdx, sdy, sdz2, &
                sdpr, sdpi, sdp2, &
                sEdAr, sEdAi, &
                qOKL)


!    reduce field element representation to nodal:-

      call red2Vnodes(sEdAr, sEdAi, sdAr, sdAi)



!    update fields in buffers

      call upd8da(sdAr, sdAi)



!     Check to see if parallel field setup OK...

      if (.not. qPArrOK_G) then
        iArEr = 1_ip
      else
        iArEr = 0_ip
      end if

      call mpi_allreduce(mpi_in_place, iArEr, 1, &
            MPI_INTEGER, &
            MPI_SUM, MPI_COMM_WORLD, error)

      if (iArEr > 0_ip) then
        qPArrOK_G = .false.
        if (tProcInfo_G%qRoot) then
          print*, 'electron outside parallel bounds!'
          print*, 'Emergency redistribute!!!'
          print*, 'If this happens often, then &
                & it is possible the parallel &
                & tuning parameters are inefficient, &
                & and not suitable...'
        end if
      end if



      if (.not. qInnerXYOK_G) then
        iArEr = 1_ip
      else
        iArEr = 0_ip
      end if

      call mpi_allreduce(mpi_in_place, iArEr, 1, &
            MPI_INTEGER, &
            MPI_SUM, MPI_COMM_WORLD, error)

      if (iArEr > 0_ip) then
        qInnerXYOK_G = .false.
        if (tProcInfo_G%qRoot) then
          print*, 'electron outside transverse bounds!'
          print*, 'Emergency redistribute!!!'
        end if
      end if


!    scatter dadz to local MPI process (split amongst processors)

!    CALL scatter2Loc(sda, ldadz, local_rows, &
!         ReducedNX_G*ReducedNY_G*NZ2_G, mrecvs, mdispls, 0)


!    DEALLOCATE(LDADz)

    GOTO 2000

!     Error Handler

1000 CALL Error_log('Error in Derivative:derivs',tErrorLog_G)
    PRINT*,'Error in Derivative:derivs'
2000 CONTINUE

  END SUBROUTINE derivs


!> @author 
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Reduce the 8*nelement array -> nnodes array
!> @param[in] sEdar - dAdz_real in element representation
!> @param[in] sEdai - dAdz_imag in element representation
!> @param[out] sdai - dAdz_real in nodal representation
!> @param[out] sdai - dAdz_imag in nodal representation

  subroutine red2Vnodes(sEdar, sEdai, sdar, sdai)

    real(kind=wp), intent(in) :: sEdar(:,:), sEdai(:,:)

    real(kind=wp), intent(out) :: sdar(:), sdai(:)

    integer(kind=ip) :: iNode, iElm0, tllen43D, nelms, &
                        ielmx, ielmy, ielmz, ielm, &
                        nelmsx, nelmsy, nelmsz2, nelmstrans, &
                        nnodesx, nnodesy, nnodestrans, ibnode

    integer(kind=ip) :: node_d(8)

    nelmsx = nspindx-1
    nelmsy = nspindy-1
    nelmsz2 = tllen-1
    nelmstrans = nelmsx * nelmsy

    nnodesx = nspindx
    nnodesy = nspindy
    nnodestrans = nnodesx * nnodesy    

    node_d = (/0, 1, nnodesx, nnodesx+1, nnodestrans, &
                  nnodestrans+1, nnodestrans + nnodesx, &
                      nnodestrans + nnodesx + 1/)

!    tllen43D = tllen * ntrndsi_G

!       'base' element is non-existent at first!!

   


    do ielmz = 1, nelmsz2
      do ielmy = 1, nelmsy

        !  SHOULD be vectorizable

        !$OMP SIMD
        do ielmx = 1, nelmsx


          ielm = ((ielmz-1) * nelmstrans) + ((ielmy-1) * nelmsx) + ielmx
          ibnode = ((ielmz-1) * nnodestrans) + ((ielmy-1) * nnodesx) + ielmx        ! 'base' node
      
          sdar(ibnode + node_d(1)) = sEdar(1, ielm) + sdar(ibnode + node_d(1))
          sdar(ibnode + node_d(2)) = sEdar(2, ielm) + sdar(ibnode + node_d(2))
          sdar(ibnode + node_d(3)) = sEdar(3, ielm) + sdar(ibnode + node_d(3))
          sdar(ibnode + node_d(4)) = sEdar(4, ielm) + sdar(ibnode + node_d(4))
          sdar(ibnode + node_d(5)) = sEdar(5, ielm) + sdar(ibnode + node_d(5))
          sdar(ibnode + node_d(6)) = sEdar(6, ielm) + sdar(ibnode + node_d(6))
          sdar(ibnode + node_d(7)) = sEdar(7, ielm) + sdar(ibnode + node_d(7))
          sdar(ibnode + node_d(8)) = sEdar(8, ielm) + sdar(ibnode + node_d(8))

        end do
      end do
    end do


    do ielmz = 1, nelmsz2
      do ielmy = 1, nelmsy

        !  SHOULD be vectorizable

        !$OMP SIMD
        do ielmx = 1, nelmsx


          ielm = ((ielmz-1) * nelmstrans) + ((ielmy-1) * nelmsx) + ielmx
          ibnode = ((ielmz-1) * nnodestrans) + ((ielmy-1) * nnodesx) + ielmx        ! 'base' node
      
          sdai(ibnode + node_d(1)) = sEdai(1, ielm) + sdai(ibnode + node_d(1))
          sdai(ibnode + node_d(2)) = sEdai(2, ielm) + sdai(ibnode + node_d(2))
          sdai(ibnode + node_d(3)) = sEdai(3, ielm) + sdai(ibnode + node_d(3))
          sdai(ibnode + node_d(4)) = sEdai(4, ielm) + sdai(ibnode + node_d(4))
          sdai(ibnode + node_d(5)) = sEdai(5, ielm) + sdai(ibnode + node_d(5))
          sdai(ibnode + node_d(6)) = sEdai(6, ielm) + sdai(ibnode + node_d(6))
          sdai(ibnode + node_d(7)) = sEdai(7, ielm) + sdai(ibnode + node_d(7))
          sdai(ibnode + node_d(8)) = sEdai(8, ielm) + sdai(ibnode + node_d(8))

        end do
      end do
    end do

   ! belongs to elements

!   ielm0
!   ielm0 + 1
!   ielm0 + nelmsx
!   ielm0 + nelmsx + 1
!   ielm0 + nelmstrans
!   ielm0 + nelmstrans + 1
!   ielm0 + nelmstrans + nelmsx
!   ielm0 + nelmstrans + nelmsx + 1
! first prime element, which is non-existent, is then

!    elm0 = 1 - nelmstrans - nelmsx + 1


!  ... SO ... is nelms = (nx+1)*(ny+1)*(nz+1) for a 'guard' region below? Seems ... unneccesarry...???



!  -------   ALTERNATIVE 'NODE CENTRIC' VERSION - AVOIDS RACE CONDITIONS
!  -------   WITH OPENMP....MAY BE BETTER....REQUIRES BUFFER ELEMENTS AROUND
!  -------   NODE SYSTEM, SO HAVE ESSENTIALLY DEAD NODES AND MORE COMPUTATION
!  -------   THOUGH....

!    elm0 = 1
!
!    do iNode = 1, tllen43D
!
!      sdar(inode) = sEdar(8, ielm0) + sdar(inode)
!      sdar(inode) = sEdar(7, ielm0 + 1) + sdar(inode)
!      sdar(inode) = sEdar(6, ielm0 + nelmsx) + sdar(inode)
!      sdar(inode) = sEdar(5, ielm0 + nelmsx + 1) + sdar(inode)
!      sdar(inode) = sEdar(4, ielm0 + nelmstrans) + sdar(inode)
!      sdar(inode) = sEdar(3, ielm0 + nelmstrans + 1) + sdar(inode)
!      sdar(inode) = sEdar(2, ielm0 + nelmstrans + nelmsx) + sdar(inode)
!      sdar(inode) = sEdar(1, ielm0 + nelmstrans + nelmsx + 1) + sdar(inode)
!
!      sdai(inode) = sEdai(8, ielm0) + sdai(inode)
!      sdai(inode) = sEdai(7, ielm0 + 1) + sdai(inode)
!      sdai(inode) = sEdai(6, ielm0 + nelmsx) + sdai(inode)
!      sdai(inode) = sEdai(5, ielm0 + nelmsx + 1) + sdai(inode)
!      sdai(inode) = sEdai(4, ielm0 + nelmstrans) + sdai(inode)
!      sdai(inode) = sEdai(3, ielm0 + nelmstrans + 1) + sdai(inode)
!      sdai(inode) = sEdai(2, ielm0 + nelmstrans + nelmsx) + sdai(inode)
!      sdai(inode) = sEdai(1, ielm0 + nelmstrans + nelmsx + 1) + sdai(inode)
!      
!      !8 of 1st elm
!      !7 of 2nd elm
!      !6 of 3rd elm
!      !5 of 4th elm
!      !4 of 5th elm
!      !3 of 6th elm
!      !2 of 7th elm
!      !1 of 8th elm
!
!      iElm = iElm + 1
!
!    end do


  end subroutine red2Vnodes




!> @author 
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Populate the 8*nelement array from nnodes array
!> @param[out] sEdar - dAdz_real in element representation
!> @param[out] sEdai - dAdz_imag in element representation
!> @param[in] sdai - dAdz_real in nodal representation
!> @param[in] sdai - dAdz_imag in nodal representation

  subroutine popElms(sEdar, sEdai, sdar, sdai)

    real(kind=wp), intent(out) :: sEdar(:,:), sEdai(:,:)

    real(kind=wp), intent(in) :: sdar(:), sdai(:)

    integer(kind=ip) :: iNode, iElm0, tllen43D, nelms, &
                        ielmx, ielmy, ielmz, ielm, &
                        nelmsx, nelmsy, nelmsz2, nelmstrans, &
                        nnodesx, nnodesy, nnodestrans, ibnode

    integer(kind=ip) :: node_d(8)

    nelmsx = nspindx-1
    nelmsy = nspindy-1
    nelmsz2 = tllen-1
    nelmstrans = nelmsx * nelmsy

    nnodesx = nspindx
    nnodesy = nspindy
    nnodestrans = nnodesx * nnodesy    
    
    node_d = (/0, 1, nnodesx, nnodesx+1, nnodestrans, &
                  nnodestrans+1, nnodestrans + nnodesx, &
                      nnodestrans + nnodesx + 1/)

!    tllen43D = tllen * ntrndsi_G

!       'base' element is non-existent at first!!


    do ielmz = 1, nelmsz2
      do ielmy = 1, nelmsy

        !  SHOULD be vectorizable

        !$OMP SIMD
        do ielmx = 1, nelmsx


          ielm = ((ielmz-1) * nelmstrans) + ((ielmy-1) * nelmsx) + ielmx
          ibnode = ((ielmz-1) * nnodestrans) + ((ielmy-1) * nnodesx) + ielmx        ! 'base' node
        
          sEdar(1, ielm) = sdar(ibnode + node_d(1))
          sEdar(2, ielm) = sdar(ibnode + node_d(2))
          sEdar(3, ielm) = sdar(ibnode + node_d(3))
          sEdar(4, ielm) = sdar(ibnode + node_d(4))
          sEdar(5, ielm) = sdar(ibnode + node_d(5))
          sEdar(6, ielm) = sdar(ibnode + node_d(6))
          sEdar(7, ielm) = sdar(ibnode + node_d(7))
          sEdar(8, ielm) = sdar(ibnode + node_d(8))

        end do
      end do
    end do



    do ielmz = 1, nelmsz2
      do ielmy = 1, nelmsy

        !  SHOULD be vectorizable

        !$OMP SIMD
        do ielmx = 1, nelmsx


          ielm = ((ielmz-1) * nelmstrans) + ((ielmy-1) * nelmsx) + ielmx
          ibnode = ((ielmz-1) * nnodestrans) + ((ielmy-1) * nnodesx) + ielmx        ! 'base' node
        
          sEdai(1, ielm) = sdai(ibnode + node_d(1))
          sEdai(2, ielm) = sdai(ibnode + node_d(2))
          sEdai(3, ielm) = sdai(ibnode + node_d(3))
          sEdai(4, ielm) = sdai(ibnode + node_d(4))
          sEdai(5, ielm) = sdai(ibnode + node_d(5))
          sEdai(6, ielm) = sdai(ibnode + node_d(6))
          sEdai(7, ielm) = sdai(ibnode + node_d(7))
          sEdai(8, ielm) = sdai(ibnode + node_d(8))

        end do
      end do
    end do
    
    
     ! belongs to elements

  !   ielm0
  !   ielm0 + 1
  !   ielm0 + nelmsx
  !   ielm0 + nelmsx + 1
  !   ielm0 + nelmstrans
  !   ielm0 + nelmstrans + 1
  !   ielm0 + nelmstrans + nelmsx
  !   ielm0 + nelmstrans + nelmsx + 1
  ! first prime element, which is non-existent, is then

  !    elm0 = 1 - nelmstrans - nelmsx + 1


  !  ... SO ... is nelms = (nx+1)*(ny+1)*(nz+1) for a 'guard' region below? Seems ... unneccesarry...???



  !  -------   ALTERNATIVE 'NODE CENTRIC' VERSION - AVOIDS RACE CONDITIONS
  !  -------   WITH OPENMP....MAY BE BETTER....REQUIRES BUFFER ELEMENTS AROUND
  !  -------   NODE SYSTEM, SO HAVE ESSENTIALLY DEAD NODES AND MORE COMPUTATION
  !  -------   THOUGH....

  !    elm0 = 1
  !
  !    do iNode = 1, tllen43D
  !
  !      sdar(inode) = sEdar(8, ielm0) + sdar(inode)
  !      sdar(inode) = sEdar(7, ielm0 + 1) + sdar(inode)
  !      sdar(inode) = sEdar(6, ielm0 + nelmsx) + sdar(inode)
  !      sdar(inode) = sEdar(5, ielm0 + nelmsx + 1) + sdar(inode)
  !      sdar(inode) = sEdar(4, ielm0 + nelmstrans) + sdar(inode)
  !      sdar(inode) = sEdar(3, ielm0 + nelmstrans + 1) + sdar(inode)
  !      sdar(inode) = sEdar(2, ielm0 + nelmstrans + nelmsx) + sdar(inode)
  !      sdar(inode) = sEdar(1, ielm0 + nelmstrans + nelmsx + 1) + sdar(inode)
  !
  !      sdai(inode) = sEdai(8, ielm0) + sdai(inode)
  !      sdai(inode) = sEdai(7, ielm0 + 1) + sdai(inode)
  !      sdai(inode) = sEdai(6, ielm0 + nelmsx) + sdai(inode)
  !      sdai(inode) = sEdai(5, ielm0 + nelmsx + 1) + sdai(inode)
  !      sdai(inode) = sEdai(4, ielm0 + nelmstrans) + sdai(inode)
  !      sdai(inode) = sEdai(3, ielm0 + nelmstrans + 1) + sdai(inode)
  !      sdai(inode) = sEdai(2, ielm0 + nelmstrans + nelmsx) + sdai(inode)
  !      sdai(inode) = sEdai(1, ielm0 + nelmstrans + nelmsx + 1) + sdai(inode)
  !      
  !      !8 of 1st elm
  !      !7 of 2nd elm
  !      !6 of 3rd elm
  !      !5 of 4th elm
  !      !4 of 5th elm
  !      !3 of 6th elm
  !      !2 of 7th elm
  !      !1 of 8th elm
  !
  !      iElm = iElm + 1
  !
  !    end do


end subroutine popElms
    
    
    



END MODULE Derivative
