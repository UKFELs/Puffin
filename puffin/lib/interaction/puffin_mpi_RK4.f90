! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

module RK4int

   use puffin_mpiInfo
   use Globals
   use Derivative
   use IO
   use ParaField

   implicit none

   REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: dadz_r0, dadz_i0
   REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: dadz_r1, dadz_i1
   REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: dadz_r2, dadz_i2

   REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: A_localtr0, A_localti0
   REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: A_localtr1, A_localti1
   REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: A_localtr2, A_localti2
   REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: A_localtr3, A_localti3

   REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: ac_rfield_in, ac_ifield_in

   REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: dxdx, dydx, dz2dx, dpxdx, dpydx, dpz2dx


   REAL(KIND=WP), DIMENSION(:), ALLOCATABLE :: dxm, dxt, xt    ! *t is 'temp', for use in next rhs call...
   REAL(KIND=WP), DIMENSION(:), ALLOCATABLE :: dym, dyt, yt
   REAL(KIND=WP), DIMENSION(:), ALLOCATABLE :: dpxm, dpxt, pxt
   REAL(KIND=WP), DIMENSION(:), ALLOCATABLE :: dpym, dpyt, pyt
   REAL(KIND=WP), DIMENSION(:), ALLOCATABLE :: dz2m, dz2t, z2t
   REAL(KIND=WP), DIMENSION(:), ALLOCATABLE :: dpz2m, dpz2t, pz2t

contains

   subroutine rk4par(sZ,h,qD)

      implicit none
!
! Perform 4th order Runge-Kutta integration, tailored
! to Puffin and its method of parallelization:
! This is NOT a general, all-purpose RK4 routine, it
! is specific to Puffin. Includes MPI_gathers and
! scatters etc between calculation of derivatives for
! use with the parallel field derivative.
!
!                ARGUMENTS
!
! y       INPUT/OUTPUT   Electron values
! SA      INPUT/OUTPUT   Field values
! x       INPUT          Propagation distance zbar
! h       INPUT          Step size in zbar

!  REAL(KIND=WP),  DIMENSION(:), INTENT(INOUT) :: sA, A_local
      REAL(KIND=WP),  INTENT(IN)                  :: sZ
      REAL(KIND=WP),                INTENT(IN)  :: h
      LOGICAL, INTENT(INOUT) :: qD

!               LOCAL ARGS
!
! h6         Step size divided by 6
! hh         Half of the step size
! xh         x position incremented by half a step
! dym        Intermediate derivatives
! dyt        Intermediate derivatives
! yt         Incremental solution
! dAdx       Field derivative
! dydx       Electron derivatives

      INTEGER(KIND=IP) :: iy,idydx,iyout,i,p
      REAL(KIND=WP)    :: h6, hh, szh
      !REAL(KIND=WP), DIMENSION(size(y)) :: dym, dyt, yt




      REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: dAdx
      REAL(KIND=WP), DIMENSION(:),ALLOCATABLE :: A_localt
      INTEGER(KIND=IP) :: error, trans

!    Transverse nodes

      trans = (NX_G)*(NY_G)

!    Step sizes

      hh = h * 0.5_WP
      h6 = h / 6.0_WP
      szh = sz + hh



      dadz_r0 = 0_wp
      dadz_r1 = 0_wp
      dadz_r2 = 0_wp
      dadz_i0 = 0_wp
      dadz_i1 = 0_wp
      dadz_i2 = 0_wp

      A_localtr0 = 0_wp
      A_localtr1 = 0_wp
      A_localtr2 = 0_wp
      A_localtr3 = 0_wp
      A_localti0 = 0_wp
      A_localti1 = 0_wp
      A_localti2 = 0_wp
      A_localti3 = 0_wp



      xt = 0_wp
      yt = 0_wp
      z2t = 0_wp
      pxt = 0_wp
      pyt = 0_wp
      pz2t = 0_wp

      dxdx = 0.0_wp
      dydx = 0.0_wp
      dz2dx = 0.0_wp
      dpxdx = 0.0_wp
      dpydx = 0.0_wp
      dpz2dx = 0.0_wp

      dxm = 0.0_wp
      dxt = 0.0_wp
      dym = 0.0_wp
      dyt = 0.0_wp
      dpxm = 0.0_wp
      dpxt = 0.0_wp
      dpym = 0.0_wp
      dpyt = 0.0_wp
      dz2m = 0.0_wp
      dz2t = 0.0_wp
      dpz2m = 0.0_wp
      dpz2t = 0.0_wp



      A_localtr0 = ac_rfield_in
      A_localti0 = ac_ifield_in


!if (count(abs(ac_rfield) > 1.0E2) > 0) print*, 'HELP IM RUBBUSH AT START I habve ', &
!              (count(abs(ac_rfield) > 1.0E2) > 0), 'bigger than 100...'

!  allocate(DADx(2*local_rows))
!  allocate(A_localt(2*local_rows))

!    A_local from A_big

      if (qD) then

!    if (tTransInfo_G%qOneD) then
!       A_local(1:local_rows)=sA(fst_row:lst_row)
!       A_local(local_rows+1:2*local_rows)=&
!            sA(fst_row+iNumberNodes_G:lst_row+iNumberNodes_G)
!    ELSE
!       CALL getAlocalFS(sA,A_local)
!    END if
!
         qD = .false.
!
      end if

!    First step
!  iy = size(sElX_G)
!  idydx = size(dxdx)

!    Get derivatives

      call derivs(sZ, A_localtr0, A_localti0, &
         sElX_G, sElY_G, sElZ2_G, sElPX_G, sElPY_G, sElGam_G, &
         dxdx, dydx, dz2dx, dpxdx, dpydx, dpz2dx, &
         dadz_r0, dadz_i0)

!call mpi_finalize(error)
!stop

!  allocate(dAm(2*local_rows),dAt(2*local_rows))
      !print*, dpydx

!    Increment local electron and field values

      if (qPArrOK_G) then

!$OMP PARALLEL WORKSHARE
         xt = sElX_G      +  hh*dxdx
         yt = sElY_G      +  hh*dydx
         z2t = sElZ2_G    +  hh*dz2dx
         pxt = sElPX_G    +  hh*dpxdx
         pyt = sElPY_G    +  hh*dpydx
         pz2t = sElGam_G  +  hh*dpz2dx

         A_localtr1 = A_localtr0 + hh * dadz_r0
         A_localti1 = A_localti0 + hh * dadz_i0
!$OMP END PARALLEL WORKSHARE

!    Update large field array with new values
!  call local2globalA(A_localt,sA,recvs,displs,tTransInfo_G%qOneD)

         call upd8a(A_localtr1, A_localti1)

      end if



!    Second step
!    Get derivatives

      if (qPArrOK_G) &
         call derivs(szh, A_localtr1, A_localti1, &
         xt, yt, z2t, pxt, pyt, pz2t, &
         dxt, dyt, dz2t, dpxt, dpyt, dpz2t, &
         dadz_r1, dadz_i1)





!    Incrementing with newest derivative value...

      if (qPArrOK_G) then
!$OMP PARALLEL WORKSHARE
         xt = sElX_G      +  hh*dxt
         yt = sElY_G      +  hh*dyt
         z2t = sElZ2_G    +  hh*dz2t
         pxt = sElPX_G    +  hh*dpxt
         pyt = sElPY_G    +  hh*dpyt
         pz2t = sElGam_G  +  hh*dpz2t

         A_localtr2 = A_localtr0 + hh * dadz_r1
         A_localti2 = A_localti0 + hh * dadz_i1
!$OMP END PARALLEL WORKSHARE
!    Update full field array

!  call local2globalA(A_localt,sA,recvs,displs,tTransInfo_G%qOneD)

         call upd8a(A_localtr2, A_localti2)

      end if

!    Third step
!    Get derivatives


      if (qPArrOK_G) &
         call derivs(szh, A_localtr2, A_localti2, &
         xt, yt, z2t, pxt, pyt, pz2t, &
         dxm, dym, dz2m, dpxm, dpym, dpz2m, &
         dadz_r2, dadz_i2)

!    Incrementing

      if (qPArrOK_G) then
!$OMP PARALLEL WORKSHARE
         xt = sElX_G      +  h * dxm
         yt = sElY_G      +  h * dym
         z2t = sElZ2_G    +  h * dz2m
         pxt = sElPX_G    +  h * dpxm
         pyt = sElPY_G    +  h * dpym
         pz2t = sElGam_G  +  h * dpz2m

         A_localtr3 = A_localtr0 + h * dadz_r2
         A_localti3 = A_localti0 + h * dadz_i2
!$OMP END PARALLEL WORKSHARE
!  call local2globalA(A_localt, sA, recvs, displs, tTransInfo_G%qOneD)

         call upd8a(A_localtr3, A_localti3)

!$OMP PARALLEL WORKSHARE
         dxm = dxt + dxm
         dym = dyt + dym
         dz2m = dz2t + dz2m
         dpxm = dpxt + dpxm
         dpym = dpyt + dpym
         dpz2m = dpz2t + dpz2m

         dadz_r2 = dadz_r1 + dadz_r2
         dadz_i2 = dadz_i1 + dadz_i2

         dadz_r1 = 0_wp
         dadz_i1 = 0_wp
!$OMP END PARALLEL WORKSHARE
      end if

!    Fourth step

      szh = sz + h


!    Get derivatives

      if (qPArrOK_G) &
         call derivs(szh, A_localtr3, A_localti3, &
         xt, yt, z2t, pxt, pyt, pz2t, &
         dxt, dyt, dz2t, dpxt, dpyt, dpz2t, &
         dadz_r1, dadz_i1)


!    Accumulate increments with proper weights

      if (qPArrOK_G) then
!$OMP PARALLEL WORKSHARE
         sElX_G    = sElX_G   + h6 * ( dxdx   + dxt   + 2.0_WP * dxm  )
         sElY_G    = sElY_G   + h6 * ( dydx   + dyt   + 2.0_WP * dym  )
         sElZ2_G   = sElZ2_G  + h6 * ( dz2dx  + dz2t  + 2.0_WP * dz2m )
         sElPX_G   = sElPX_G  + h6 * ( dpxdx  + dpxt  + 2.0_WP * dpxm )
         sElPY_G   = sElPY_G  + h6 * ( dpydx  + dpyt  + 2.0_WP * dpym )
         sElGam_G  = sElGam_G + h6 * ( dpz2dx + dpz2t + 2.0_WP * dpz2m)

         ac_rfield_in = ac_rfield_in + h6 * (dadz_r0 + dadz_r1 + 2.0_WP * dadz_r2)
         ac_ifield_in = ac_ifield_in + h6 * (dadz_i0 + dadz_i1 + 2.0_WP * dadz_i2)
!$OMP END PARALLEL WORKSHARE
!  if (count(abs(dadz_r0) > 0.0_wp) <= 0) print*, 'HELP IM TOO RUBBUSH'

!  if (count(abs(ac_rfield) > 0.0_wp) <= 0) print*, 'HELP IM RUBBUSH'


!if (count(abs(ac_rfield) > 1.0E2) > 0) print*, 'HELP IM RUBBUSH for I habve ', &
!              count(abs(ac_rfield) > 1.0E2) , 'bigger than 100...', &
!              'and I am', tProcInfo_G%rank



         call upd8a(ac_rfield_in, ac_ifield_in)

      end if

!  call local2globalA(A_local,sA,recvs,displs,tTransInfo_G%qOneD)

!    Deallocating temp arrays

!  deallocate(dAm,dAt,A_localt)

!  deallocate(DADx)

!  deallocate(dadz_r0, dadz_i0)
!  deallocate(dadz_r1, dadz_i1)
!  deallocate(dadz_r2, dadz_i2)
!
!  deallocate(A_localtr0, A_localti0)
!  deallocate(A_localtr1, A_localti1)
!  deallocate(A_localtr2, A_localti2)
!  deallocate(A_localtr3, A_localti3)
!
!  deallocate(DxDx)
!  deallocate(DyDx)
!  deallocate(DpxDx)
!  deallocate(DpyDx)
!  deallocate(Dz2Dx)
!  deallocate(Dpz2Dx)

!   Set error flag and exit

      GOTO 2000

!   Error Handler - Error log Subroutine in CIO.f90 line 709

1000  CALL log_error('Error in MathLib:rk4',tErrorLog_G)
      PRINT*,'Error in MathLib:rk4'
2000  CONTINUE

   end subroutine rk4par







   subroutine allact_rk4_arrs()

      integer(kind=ip) :: tllen43D

      tllen43D = tllen * ntrndsi_G

      allocate(DxDx(iNumberElectrons_G))
      allocate(DyDx(iNumberElectrons_G))
      allocate(DpxDx(iNumberElectrons_G))
      allocate(DpyDx(iNumberElectrons_G))
      allocate(Dz2Dx(iNumberElectrons_G))
      allocate(Dpz2Dx(iNumberElectrons_G))


      allocate(dadz_r0(tllen43D), dadz_i0(tllen43D))
      allocate(dadz_r1(tllen43D), dadz_i1(tllen43D))
      allocate(dadz_r2(tllen43D), dadz_i2(tllen43D))

      allocate(A_localtr0(tllen43D), A_localti0(tllen43D))
      allocate(A_localtr1(tllen43D), A_localti1(tllen43D))
      allocate(A_localtr2(tllen43D), A_localti2(tllen43D))
      allocate(A_localtr3(tllen43D), A_localti3(tllen43D))

      allocate(ac_rfield_in(tllen43D), ac_ifield_in(tllen43D))

      allocate(dxm(iNumberElectrons_G), &
         dxt(iNumberElectrons_G), xt(iNumberElectrons_G))
      allocate(dym(iNumberElectrons_G), &
         dyt(iNumberElectrons_G), yt(iNumberElectrons_G))
      allocate(dpxm(iNumberElectrons_G), &
         dpxt(iNumberElectrons_G), pxt(iNumberElectrons_G))
      allocate(dpym(iNumberElectrons_G), &
         dpyt(iNumberElectrons_G), pyt(iNumberElectrons_G))
      allocate(dz2m(iNumberElectrons_G), &
         dz2t(iNumberElectrons_G), z2t(iNumberElectrons_G))
      allocate(dpz2m(iNumberElectrons_G), &
         dpz2t(iNumberElectrons_G), pz2t(iNumberElectrons_G))

      allocate(dadz_w(iNumberElectrons_G))

      call outer2Inner(ac_rfield_in, ac_ifield_in)

      qInnerXYOK_G = .true.

   end subroutine allact_rk4_arrs




   subroutine deallact_rk4_arrs()

      call inner2Outer(ac_rfield_in, ac_ifield_in)

      deallocate(ac_rfield_in, ac_ifield_in)

      deallocate(dadz_r0, dadz_i0)
      deallocate(dadz_r1, dadz_i1)
      deallocate(dadz_r2, dadz_i2)

      deallocate(A_localtr0, A_localti0)
      deallocate(A_localtr1, A_localti1)
      deallocate(A_localtr2, A_localti2)
      deallocate(A_localtr3, A_localti3)

      deallocate(DxDx)
      deallocate(DyDx)
      deallocate(DpxDx)
      deallocate(DpyDx)
      deallocate(Dz2Dx)
      deallocate(Dpz2Dx)

      deallocate(dxm, &
         dxt, xt)
      deallocate(dym, &
         dyt, yt)
      deallocate(dpxm, &
         dpxt, pxt)
      deallocate(dpym, &
         dpyt, pyt)
      deallocate(dz2m, &
         dz2t, z2t)
      deallocate(dpz2m, &
         dpz2t, pz2t)

      deallocate(dadz_w)

   end subroutine deallact_rk4_arrs


!subroutine RK4_inc



!end subroutine RK4_inc

! Note - the dxdz ad intermediates should all be global,
! if allocating outside of RK4 routine.
!
! They should be passed through to rhs / derivs, and
! be local in there, I think....
!
! All those vars being passed into rhs? GLOBAL.
! Only the arrays should be global.
! Same for the equations module...
!
! Scoop out preamble of rhs, defining temp vars.
! These can all be defined outside this routine to make
! it more readable
!
! label consistently - *_g for main global,
!                      *_rg for rhs global,
!                      *_dg for diffraction global
!
! Check 3D und eqns - are they general? i.e. can kx and ky be anything?
!
! Lj, field4elec (real and imag) and dp2f should be global?
!
! Interface for private var access??
! So have dp2f, field4elec and Lj private arrays in the equations module,
! and interface dp2f and field4elec to rhs.f90 to alter them...
! OR alter them through a subroutine....
! SO Lj is common to both field and e eqns...
! whereas field4elec and dp2f are e only.
! So only making Lj and dp2f private to eqns for now
! In fact, they are just globallay defined ATM until
! we get this working....
! Rename eqns to electron eqns or something...
!
! Make rhs / eqns vars global
! fix dp2f interface in rhs.f90 ... DONE
! only allocate / calc dp2f when it will be used!
!

end module rk4int
