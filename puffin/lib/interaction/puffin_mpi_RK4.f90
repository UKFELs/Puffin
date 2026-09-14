! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

module RK4int

   use puffin_mpiInfo, only: ip
   use Globals, only: NX_G, NY_G, ntrndsi_G, iNumberElectrons_G, sElX_G, sElY_G, sElZ2_G, &
     sElPX_G, sElPY_G, sElGam_G, dadz_w, WP
   use Derivative, only: derivs
   use IO, only: tErrorLog_G, log_error
   use ParaField, only: upd8a, inner2outer, outer2inner
   use GlobalTypes, only: tSimulationContext, tFieldValues, tRK4Workspace

   implicit none (type, external)
private

public :: allact_rk4_arrs, deallact_rk4_arrs, rk4par

!  The integration scratch that used to sit here as module arrays now lives in
!  a tRK4Workspace, passed in by UndSection. The field half of it is indexed by
!  envelope, so a second polarisation is another element of work%env rather than
!  another set of module arrays. See UKFELs/Puffin#107 and the W3 section of
!  AVERAGED_MODE_ROADMAP.md.

contains

   subroutine rk4par(sZ, h, qD, ctx, work)

      implicit none (type, external)
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
      type(tSimulationContext), intent(inout) :: ctx
      type(tRK4Workspace), intent(inout) :: work

!               LOCAL ARGS
!
! h6         Step size divided by 6
! hh         Half of the step size
! xh         x position incremented by half a step
! work%dym        Intermediate derivatives
! work%dyt        Intermediate derivatives
! work%yt         Incremental solution
! dAdx       Field derivative
! work%dydx       Electron derivatives

      REAL(KIND=WP)    :: h6, hh, szh
      !REAL(KIND=WP), DIMENSION(size(y)) :: work%dym, work%dyt, work%yt




      INTEGER(KIND=IP) :: trans

!    Transverse nodes

      trans = (NX_G)*(NY_G)

!    Step sizes

      hh = h * 0.5_WP
      h6 = h / 6.0_WP
      szh = sz + hh



      work%env(1)%dadz_r(:,0) = 0_wp
      work%env(1)%dadz_r(:,1) = 0_wp
      work%env(1)%dadz_r(:,2) = 0_wp
      work%env(1)%dadz_i(:,0) = 0_wp
      work%env(1)%dadz_i(:,1) = 0_wp
      work%env(1)%dadz_i(:,2) = 0_wp

      work%env(1)%A_r(:,0) = 0_wp
      work%env(1)%A_r(:,1) = 0_wp
      work%env(1)%A_r(:,2) = 0_wp
      work%env(1)%A_r(:,3) = 0_wp
      work%env(1)%A_i(:,0) = 0_wp
      work%env(1)%A_i(:,1) = 0_wp
      work%env(1)%A_i(:,2) = 0_wp
      work%env(1)%A_i(:,3) = 0_wp



      work%xt = 0_wp
      work%yt = 0_wp
      work%z2t = 0_wp
      work%pxt = 0_wp
      work%pyt = 0_wp
      work%pz2t = 0_wp

      work%dxdx = 0.0_wp
      work%dydx = 0.0_wp
      work%dz2dx = 0.0_wp
      work%dpxdx = 0.0_wp
      work%dpydx = 0.0_wp
      work%dpz2dx = 0.0_wp

      work%dxm = 0.0_wp
      work%dxt = 0.0_wp
      work%dym = 0.0_wp
      work%dyt = 0.0_wp
      work%dpxm = 0.0_wp
      work%dpxt = 0.0_wp
      work%dpym = 0.0_wp
      work%dpyt = 0.0_wp
      work%dz2m = 0.0_wp
      work%dz2t = 0.0_wp
      work%dpz2m = 0.0_wp
      work%dpz2t = 0.0_wp



      work%env(1)%A_r(:,0) = work%env(1)%in_r
      work%env(1)%A_i(:,0) = work%env(1)%in_i


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
!  idydx = size(work%dxdx)

!    Get derivatives

      call derivs(sZ, work%env(1)%A_r(:,0), work%env(1)%A_i(:,0), &
         sElX_G, sElY_G, sElZ2_G, sElPX_G, sElPY_G, sElGam_G, &
         work%dxdx, work%dydx, work%dz2dx, work%dpxdx, work%dpydx, work%dpz2dx, &
         work%env(1)%dadz_r(:,0), work%env(1)%dadz_i(:,0), ctx)

!call mpi_finalize(error)
!stop

!  allocate(dAm(2*local_rows),dAt(2*local_rows))
      !print*, work%dpydx

!    Increment local electron and field values

      if (ctx%flags%parallel_arrays_ok) then

!$OMP PARALLEL WORKSHARE
         work%xt = sElX_G      +  hh*work%dxdx
         work%yt = sElY_G      +  hh*work%dydx
         work%z2t = sElZ2_G    +  hh*work%dz2dx
         work%pxt = sElPX_G    +  hh*work%dpxdx
         work%pyt = sElPY_G    +  hh*work%dpydx
         work%pz2t = sElGam_G  +  hh*work%dpz2dx

         work%env(1)%A_r(:,1) = work%env(1)%A_r(:,0) + hh * work%env(1)%dadz_r(:,0)
         work%env(1)%A_i(:,1) = work%env(1)%A_i(:,0) + hh * work%env(1)%dadz_i(:,0)
!$OMP END PARALLEL WORKSHARE

!    Update large field array with new values
!  call local2globalA(A_localt,sA,recvs,displs,tTransInfo_G%qOneD)

         call upd8a(work%env(1)%A_r(:,1), work%env(1)%A_i(:,1), ctx%field)

      end if



!    Second step
!    Get derivatives

      if (ctx%flags%parallel_arrays_ok) then
        call derivs(szh, work%env(1)%A_r(:,1), work%env(1)%A_i(:,1), &
         work%xt, work%yt, work%z2t, work%pxt, work%pyt, work%pz2t, &
         work%dxt, work%dyt, work%dz2t, work%dpxt, work%dpyt, work%dpz2t, &
         work%env(1)%dadz_r(:,1), work%env(1)%dadz_i(:,1), ctx)
      end if





!    Incrementing with newest derivative value...

      if (ctx%flags%parallel_arrays_ok) then
!$OMP PARALLEL WORKSHARE
         work%xt = sElX_G      +  hh*work%dxt
         work%yt = sElY_G      +  hh*work%dyt
         work%z2t = sElZ2_G    +  hh*work%dz2t
         work%pxt = sElPX_G    +  hh*work%dpxt
         work%pyt = sElPY_G    +  hh*work%dpyt
         work%pz2t = sElGam_G  +  hh*work%dpz2t

         work%env(1)%A_r(:,2) = work%env(1)%A_r(:,0) + hh * work%env(1)%dadz_r(:,1)
         work%env(1)%A_i(:,2) = work%env(1)%A_i(:,0) + hh * work%env(1)%dadz_i(:,1)
!$OMP END PARALLEL WORKSHARE
!    Update full field array

!  call local2globalA(A_localt,sA,recvs,displs,tTransInfo_G%qOneD)

         call upd8a(work%env(1)%A_r(:,2), work%env(1)%A_i(:,2), ctx%field)

      end if

!    Third step
!    Get derivatives


      if (ctx%flags%parallel_arrays_ok) then
        call derivs(szh, work%env(1)%A_r(:,2), work%env(1)%A_i(:,2), &
         work%xt, work%yt, work%z2t, work%pxt, work%pyt, work%pz2t, &
         work%dxm, work%dym, work%dz2m, work%dpxm, work%dpym, work%dpz2m, &
         work%env(1)%dadz_r(:,2), work%env(1)%dadz_i(:,2), ctx)
      end if

!    Incrementing

      if (ctx%flags%parallel_arrays_ok) then
!$OMP PARALLEL WORKSHARE
         work%xt = sElX_G      +  h * work%dxm
         work%yt = sElY_G      +  h * work%dym
         work%z2t = sElZ2_G    +  h * work%dz2m
         work%pxt = sElPX_G    +  h * work%dpxm
         work%pyt = sElPY_G    +  h * work%dpym
         work%pz2t = sElGam_G  +  h * work%dpz2m

         work%env(1)%A_r(:,3) = work%env(1)%A_r(:,0) + h * work%env(1)%dadz_r(:,2)
         work%env(1)%A_i(:,3) = work%env(1)%A_i(:,0) + h * work%env(1)%dadz_i(:,2)
!$OMP END PARALLEL WORKSHARE
!  call local2globalA(A_localt, sA, recvs, displs, tTransInfo_G%qOneD)

         call upd8a(work%env(1)%A_r(:,3), work%env(1)%A_i(:,3), ctx%field)

!$OMP PARALLEL WORKSHARE
         work%dxm = work%dxt + work%dxm
         work%dym = work%dyt + work%dym
         work%dz2m = work%dz2t + work%dz2m
         work%dpxm = work%dpxt + work%dpxm
         work%dpym = work%dpyt + work%dpym
         work%dpz2m = work%dpz2t + work%dpz2m

         work%env(1)%dadz_r(:,2) = work%env(1)%dadz_r(:,1) + work%env(1)%dadz_r(:,2)
         work%env(1)%dadz_i(:,2) = work%env(1)%dadz_i(:,1) + work%env(1)%dadz_i(:,2)

         work%env(1)%dadz_r(:,1) = 0_wp
         work%env(1)%dadz_i(:,1) = 0_wp
!$OMP END PARALLEL WORKSHARE
      end if

!    Fourth step

      szh = sz + h


!    Get derivatives

      if (ctx%flags%parallel_arrays_ok) then
        call derivs(szh, work%env(1)%A_r(:,3), work%env(1)%A_i(:,3), &
         work%xt, work%yt, work%z2t, work%pxt, work%pyt, work%pz2t, &
         work%dxt, work%dyt, work%dz2t, work%dpxt, work%dpyt, work%dpz2t, &
         work%env(1)%dadz_r(:,1), work%env(1)%dadz_i(:,1), ctx)
      end if


!    Accumulate increments with proper weights

      if (ctx%flags%parallel_arrays_ok) then
!$OMP PARALLEL WORKSHARE
         sElX_G    = sElX_G   + h6 * ( work%dxdx   + work%dxt   + 2.0_WP * work%dxm  )
         sElY_G    = sElY_G   + h6 * ( work%dydx   + work%dyt   + 2.0_WP * work%dym  )
         sElZ2_G   = sElZ2_G  + h6 * ( work%dz2dx  + work%dz2t  + 2.0_WP * work%dz2m )
         sElPX_G   = sElPX_G  + h6 * ( work%dpxdx  + work%dpxt  + 2.0_WP * work%dpxm )
         sElPY_G   = sElPY_G  + h6 * ( work%dpydx  + work%dpyt  + 2.0_WP * work%dpym )
         sElGam_G  = sElGam_G + h6 * ( work%dpz2dx + work%dpz2t + 2.0_WP * work%dpz2m)

         work%env(1)%in_r = work%env(1)%in_r + h6 * (work%env(1)%dadz_r(:,0) + work%env(1)%dadz_r(:,1) + 2.0_WP * work%env(1)%dadz_r(:,2))
         work%env(1)%in_i = work%env(1)%in_i + h6 * (work%env(1)%dadz_i(:,0) + work%env(1)%dadz_i(:,1) + 2.0_WP * work%env(1)%dadz_i(:,2))
!$OMP END PARALLEL WORKSHARE
!  if (count(abs(work%env(1)%dadz_r(:,0)) > 0.0_wp) <= 0) print*, 'HELP IM TOO RUBBUSH'

!  if (count(abs(ac_rfield) > 0.0_wp) <= 0) print*, 'HELP IM RUBBUSH'


!if (count(abs(ac_rfield) > 1.0E2) > 0) print*, 'HELP IM RUBBUSH for I habve ', &
!              count(abs(ac_rfield) > 1.0E2) , 'bigger than 100...', &
!              'and I am', tProcInfo_G%rank



         call upd8a(work%env(1)%in_r, work%env(1)%in_i, ctx%field)

      end if

!  call local2globalA(A_local,sA,recvs,displs,tTransInfo_G%qOneD)

!    Deallocating temp arrays

!  deallocate(dAm,dAt,A_localt)

!  deallocate(DADx)

!  deallocate(work%env(1)%dadz_r(:,0), work%env(1)%dadz_i(:,0))
!  deallocate(work%env(1)%dadz_r(:,1), work%env(1)%dadz_i(:,1))
!  deallocate(work%env(1)%dadz_r(:,2), work%env(1)%dadz_i(:,2))
!
!  deallocate(work%env(1)%A_r(:,0), work%env(1)%A_i(:,0))
!  deallocate(work%env(1)%A_r(:,1), work%env(1)%A_i(:,1))
!  deallocate(work%env(1)%A_r(:,2), work%env(1)%A_i(:,2))
!  deallocate(work%env(1)%A_r(:,3), work%env(1)%A_i(:,3))
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

      CALL log_error("Error in MathLib:rk4",tErrorLog_G)
      PRINT*,"Error in MathLib:rk4"
2000  CONTINUE

   end subroutine rk4par







   subroutine allact_rk4_arrs(field, work)

      type(tFieldValues), intent(in) :: field
      type(tRK4Workspace), intent(inout) :: work

      integer(kind=ip) :: tllen43D, nEnv, ie

      tllen43D = field%tllen * ntrndsi_G

!     One radiation envelope. An elliptical undulator or a harmonic band makes
!     this 2 or more, and nothing below changes except this count (W3, #129).

      nEnv = 1_ip

      allocate(work%env(nEnv))

      do ie = 1, nEnv
        allocate(work%env(ie)%dadz_r(tllen43D, 0:2), work%env(ie)%dadz_i(tllen43D, 0:2))
        allocate(work%env(ie)%A_r(tllen43D, 0:3), work%env(ie)%A_i(tllen43D, 0:3))
        allocate(work%env(ie)%in_r(tllen43D), work%env(ie)%in_i(tllen43D))
      end do

      allocate(work%dxdx(iNumberElectrons_G))
      allocate(work%dydx(iNumberElectrons_G))
      allocate(work%dpxdx(iNumberElectrons_G))
      allocate(work%dpydx(iNumberElectrons_G))
      allocate(work%dz2dx(iNumberElectrons_G))
      allocate(work%dpz2dx(iNumberElectrons_G))

      allocate(work%dxm(iNumberElectrons_G), &
         work%dxt(iNumberElectrons_G), work%xt(iNumberElectrons_G))
      allocate(work%dym(iNumberElectrons_G), &
         work%dyt(iNumberElectrons_G), work%yt(iNumberElectrons_G))
      allocate(work%dpxm(iNumberElectrons_G), &
         work%dpxt(iNumberElectrons_G), work%pxt(iNumberElectrons_G))
      allocate(work%dpym(iNumberElectrons_G), &
         work%dpyt(iNumberElectrons_G), work%pyt(iNumberElectrons_G))
      allocate(work%dz2m(iNumberElectrons_G), &
         work%dz2t(iNumberElectrons_G), work%z2t(iNumberElectrons_G))
      allocate(work%dpz2m(iNumberElectrons_G), &
         work%dpz2t(iNumberElectrons_G), work%pz2t(iNumberElectrons_G))

      allocate(dadz_w(iNumberElectrons_G))

      call outer2Inner(work%env(1)%in_r, work%env(1)%in_i, field)

   end subroutine allact_rk4_arrs




   subroutine deallact_rk4_arrs(field, work)

      type(tFieldValues), intent(inout) :: field
      type(tRK4Workspace), intent(inout) :: work

      integer(kind=ip) :: ie

!     The inner mesh is a working copy; write it back to the authoritative
!     outer mesh before letting it go.

      do ie = 1, size(work%env)
        call inner2Outer(work%env(ie)%in_r, work%env(ie)%in_i, field)
        deallocate(work%env(ie)%in_r, work%env(ie)%in_i)
        deallocate(work%env(ie)%dadz_r, work%env(ie)%dadz_i)
        deallocate(work%env(ie)%A_r, work%env(ie)%A_i)
      end do

      deallocate(work%env)

      deallocate(work%dxdx)
      deallocate(work%dydx)
      deallocate(work%dpxdx)
      deallocate(work%dpydx)
      deallocate(work%dz2dx)
      deallocate(work%dpz2dx)

      deallocate(work%dxm, &
         work%dxt, work%xt)
      deallocate(work%dym, &
         work%dyt, work%yt)
      deallocate(work%dpxm, &
         work%dpxt, work%pxt)
      deallocate(work%dpym, &
         work%dpyt, work%pyt)
      deallocate(work%dz2m, &
         work%dz2t, work%z2t)
      deallocate(work%dpz2m, &
         work%dpz2t, work%pz2t)

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
