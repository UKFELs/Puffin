! ###############################################
! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause
! ###############################################

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> This module contains subroutines for calculating the radiation diffraction 
!> step.

module PDiff

use paratype
use parallelinfotype
use transforminfotype
use transforms
use masks
use Globals
use IO
use parafield

use, intrinsic :: iso_c_binding

implicit none



contains

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Setup parallel data structures for diffraction step, call diffraction 
!> routine, then return data to nominal parallel distribution for RK4 
!> integration.
!> @param[in] sStep Diffraction step size - distance to propagate field over.
!> @param[out] qDiffrctd Whether field has been diffracted.
!> @param[out] qOK Error flag.

subroutine diffractIM(sStep, &
                      qDiffrctd, qOK)

  implicit none

  real(kind=wp), intent(in) :: sStep
  logical, intent(out) :: qDiffrctd, qOK

  logical :: qOKL


  qOK = .false.


!      Change data layout to FFTW -

  call redist2FFTWlt()



  CALL DiffractionStep(sStep,&
       tre_fft, tim_fft,&
       qOKL)
  if (.not. qOKL) goto 1000

  qDiffrctd = .true.



!    Change back to wiggler data layout

  call redistbackFFT()


!              Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in diffractIM',tErrorLog_G)

2000 CONTINUE


end subroutine diffractIM




!     ######################################################


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Multiplies Fourier Transformed field by complex exponential to diffract the
!> field. To diffract field, do \f$ A_\bot(\bar{z}+\Delta \bar{z}) = 
!> A_\bot(\bar{z}) \exp(\frac{i \Delta \bar{z} (k_x^2 + k_y^2) }{2 k_{z2}}) \f$ :
!> see LT Campbell and BWJ McNeil, Physics of Plasmas 19, 093119 (2012)
!> @param[in] h Diffraction step size \f$ \Delta \bar{z} \f$
!> @param[out] qOK Error flag.

subroutine multiplyexp(h,qOK)

  implicit none

  real(kind=wp), intent(in) :: h

  logical, intent(out) :: qOK

  complex(kind=wp) :: posI            !< Imaginary unit
  
  integer(kind=IP) :: ind,   &        !< index 
                      x_inc, &        !< loop index for nodes in x
                      y_inc, &        !< loop index for nodes in y
                      z2_inc          !< loop index for nodes in z2
                      
  integer(kind=IP) :: loc_nz2         !< Local number of z2 nodes
  
  real(kind=wp) :: cutoff, &          !< Frequency cutoff for high pass filter
                   delz2              !< Mesh spacing in z2

!------------------------------------------------------
!                      Begin

  qOK = .false.

  posI=CMPLX(0.0,1.0,KIND=WP)
  delz2=sLengthOfElmZ2_G
  cutoff=2.0_WP*pi*sfilt/(REAL(NZ2_G,KIND=WP)*delz2)
  if (fieldMesh == iPeriodic) cutoff = 0.0_wp
  loc_nz2 = tTransInfo_G%loc_nz2

!      Main loop, multiply FT field by exp factor

  do z2_inc=0,loc_nz2-1_IP
     do y_inc=0,NY_G-1_IP
        do x_inc=0,NX_G-1_IP

           !ind=x_inc+y_inc*NX_G+z2_inc*NX_G*NY_G

           if ((kz2_loc_G(z2_inc)>cutoff) .or. &
                (kz2_loc_G(z2_inc)<-cutoff)) then

              if (kz2_loc_G(z2_inc)/=0.0_WP) then

                Afftw(x_inc+1,y_inc+1,z2_inc+1) = &
                           exp(posI*h*(kx_G(x_inc)**2 + &
                                     ky_G(y_inc)**2) / &
                                (2.0_WP*kz2_loc_G(z2_inc))) * &
                           Afftw(x_inc+1,y_inc+1,z2_inc+1)

              end if

           else

              if (qFilter) Afftw(x_inc+1,y_inc+1,z2_inc+1) = &
                         CMPLX(0.0, 0.0, C_DOUBLE_COMPLEX)

           end if

        end do
     end do
  end do

!              Set error flag and exit

  qOK = .true.

  GOTO 2000

1000  call Error_log('Error in transforms:RearrangeExp',tErrorLog_G)

2000 continue

end subroutine multiplyexp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Subroutine to perform free space radiation field diffraction
!> in the dimensionless scaled notation.
!> Diffraction step algorithm described in
!> LT Campbell and BWJ McNeil, Physics of Plasmas 19, 093119 (2012)
!> @param[in] h Diffraction step size \f$ \Delta \bar{z} \f$
!> @param[inout] sAr Real part of \f$ A_\bot \f$
!> @param[inout] sAi Imaginary part of \f$ A_\bot \f$
!> @param[out] qOK Error flag.
!> @param ntrh Number of nodes in transverse mesh = nx * ny
!> @param ix Counter for mesh nodes in x
!> @param iy Counter for mesh nodes in y
!> @param iz Counter for mesh nodes in z2
!> @param qOKL local error flag
!> @param error Error integer for MPI calls

SUBROUTINE DiffractionStep(h, sAr, sAi, qOK)

  IMPLICIT NONE
!
! Subroutine to perform free space radiation field diffraction
! in the dimensionless scaled notation.
! Diffraction step algorithm described in
! LT Campbell and BWJ McNeil, Physics of Plasmas 19, 093119 (2012)

  real(kind=wp), intent(in)      ::   h
  real(kind=wp), dimension(:), intent(inout)  :: sAr, sAi
  logical, intent(out)  ::  qOK

  integer(kind=ip) :: ntrh, ix, iy, iz
  logical :: qOKL
  integer :: error

!                      Begin

  qOK = .false.

!     Allocate arrays and get distributed FT of field.
!     Transforming from A(x,y,z2,zbar) to A(kx,ky,kz2,zbar)


  if (tProcInfo_G%QROOT ) then
    print*,' inside diffraction... ',iCsteps, end_time-start_time
  end if


  call Get_time(tr_time_s)


!  ALLOCATE(sA_local(0:tTransInfo_G%TOTAL_LOCAL_SIZE-1))
!  ALLOCATE(work(0:tTransInfo_G%TOTAL_LOCAL_SIZE-1))

  call Get_time(tr_time_e)

  if (tProcInfo_G%QROOT ) then
    print*,' allocating arrays took... ', tr_time_e-tr_time_s
  end if


!  sA_local = 0.0_wp
  ntrh = NX_G * NY_G

  do iz = 1, tTransInfo_G%loc_nz2
    do iy = 1, NY_G
      do ix = 1, NX_G

        Afftw(ix,iy,iz) = CMPLX(sAr(ix + nx_g*(iy-1) + ntrh*(iz-1)), &
                     sAi(ix + nx_g*(iy-1) + ntrh*(iz-1)), C_DOUBLE_COMPLEX)

      end do
    end do
  end do

  call Get_time(tr_time_e)

  if (tProcInfo_G%QROOT ) then
    print*,' assigning data took... ', tr_time_e-tr_time_s
  end if

  call Transform(tTransInfo_G%fplan, &
       Afftw, &
       qOKL)


  call Get_time(tr_time_e)  ! ...timing info

  if (tProcInfo_G%QROOT ) then
    print*,' forward transform took... ', tr_time_e-tr_time_s
  end if

!    Multiply field by the exp factor to obtain A(kx,ky,kz2,zbar+h)

  call MultiplyExp(h,qOKL)


  call Get_time(tr_time_e)

  if (tProcInfo_G%QROOT ) then
    print*,' multiply exp took... ', tr_time_e-tr_time_s
  end if


!   Perform the backward Fourier transform to obtain A(x,y,z2,zbar+h)

  call Transform(tTransInfo_G%bplan, &
       Afftw, &
       qOKL)


  call Get_time(tr_time_e)

  if (tProcInfo_G%QROOT ) then
    print*,' back transform took... ', tr_time_e-tr_time_s
  end if

!      Scale the field data to normalize transforms

  Afftw = Afftw/ffact


!      Now solve for the absorbing boundary layer

  call AbsorptionStep(Afftw, h, ffact)

  call Get_time(tr_time_e)

  if (tProcInfo_G%QROOT ) then
    print*,' absorption step took... ', tr_time_e-tr_time_s
  end if


!      assign data back to real and imag parts for integration
!                        through undulator

  do iz = 1, tTransInfo_G%loc_nz2
    do iy = 1, NY_G
      do ix = 1, NX_G

        sAr(ix + nx_g*(iy-1) + ntrh*(iz-1)) = real(Afftw(ix,iy,iz), kind=wp)
        sAi(ix + nx_g*(iy-1) + ntrh*(iz-1)) = aimag(Afftw(ix,iy,iz))

      end do
    end do
  end do

!              Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in transforms:DiffractionStep',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE DiffractionStep

!***********************************************************


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> This subroutine implements a boundary region
!> in the x, y and z2 directions.The boundary 
!> layer absorbs the outgoing radiation to 
!> minimize the reflections of the diffracted
!> radiation.
!> @param[inout] sAl Complex field \f$ A_\bot \f$
!> @param[in] h Diffraction step size \f$ \Delta \bar{z} \f$
!> @param[in] ffact Absorption coefficient

SUBROUTINE AbsorptionStep(sAl,h,ffact)

! This subroutine implements a boundary region
! in the x, y and z2 directions. The method used
! is similar to e.g. REF.
!
! The boundary layer absorbs the outgoing radiation
! to minimize the reflections of the diffracted
! radiation.
!
! Written by
! Dr L.T. Campbell
! University of Hamburg
! Oct 2013



!
! APPLY ABSORPTION FIRST IN THE TRANSVERSE DIRECTION,
! THEN IN THE LONGITUDINAL.
!
! SO, FIRST, TRANSVERSE MASK, THEN FT, THEN:
!
!          NEW_Af = Af * EXP(delz*beta*(-kx-ky)/kz2)
!
! i.e. EXPONENTIALLY DECREASE FREQUENCY COMPONENTS
! DEPENDANT ON *BOTH* TRANSVERSE AND LONGITUDINAL
! WAVENUMBER.
!
! THEN FFT BACK TO A.
!
! THEN APPLY LONGITUDINAL MASK, AND FFT AGAIN
!
! THEN ABSORB LONGITUDINAL PROP FROM DIFFRACTION
!
!
!
!                ARGUMENTS

  complex(C_DOUBLE_COMPLEX), pointer, intent(inout) :: sAl(:,:,:)

  REAL(KIND=WP), INTENT(IN) :: h,ffact

!               LOCAL ARGS

  REAL(KIND=WP), allocatable :: mask(:), mask_z2(:)
  COMPLEX(KIND=WP), allocatable :: sAnb(:,:,:)
  COMPLEX(KIND=WP) :: posI
  INTEGER(KIND=IP) :: iz2, x_inc, y_inc, z2_inc, ind, ix, iy
  INTEGER(KIND=IP) :: loc_nz2
  integer :: error
  LOGICAL :: qOKL

! ############################

  loc_nz2 = tTransInfo_G%loc_nz2

! ****************************

  allocate(mask(NX_G*NY_G), mask_z2(tTransInfo_G%loc_nz2))
  allocate(sAnb(nx_g, ny_g, loc_nz2))

! ****************************



  posI=CMPLX(0.0,1.0,KIND=WP)

  CALL getMask(NX_G, NY_G, sLengthOfElmX_G, sLengthOfElmY_G, &
               NBX_G, NBY_G, mask)



!  Now also using boundary in z2....so mask in z2 is....

  IF (loc_nz2 > 0) THEN

    mask_z2 = getZ2Mask(sLengthOfElmZ2_G, nZ2_G, tTransInfo_G%loc_nz2,   &
                         nBZ2_G, tTransInfo_G%loc_z2_start)

    if (fieldMesh == iPeriodic) mask_z2 = 0.0_wp

!!!!!      sAl is local      !!!!!
!!!!!      goes from 0,total_local_size     !!!!!!!



    do iz2 = 1_IP, loc_nz2
      do iy = 1_ip, ny_g
        do ix = 1_ip, nx_g

          sAnb(ix, iy, iz2) = (1.0_WP - (mask(ix+((iy-1)*nx_g)) +  &
                  ( mask_z2(iz2) *  (1.0_WP - mask(ix+((iy-1)*nx_g) ) ) ) ) ) * &
                  sAl(ix,iy,iz2)

!    sAnb(NX_G*NY_G*iz2 : NX_G*NY_G*(iz2+1_IP) - 1_IP) =   (1.0_WP - mask_z2(iz2)) * sAnb(NX_G*NY_G*iz2 : NX_G*NY_G*(iz2+1_IP) - 1_IP)

          sAl(ix, iy, iz2) = (mask(ix+((iy-1)*nx_g))   +  &
                  ( mask_z2(iz2) *  (1.0_WP - mask(ix+((iy-1)*nx_g) ) ) ) ) * &
                  sAl(ix,iy,iz2)

        end do
      end do
    end do

  end if

!     FFT sAb

  CALL Transform(tTransInfo_G%fplan, sAl, qOKL)

!     Apply filter, by decreasing fourier coefficients

  do z2_inc=0,loc_nz2-1_IP
    do y_inc=0,NY_G-1_IP
      do x_inc=0,NX_G-1_IP

        if (kz2_loc_G(z2_inc)/=0.0_WP) then

              !  sAl(ind)=exp(-posI*h*(kx_G(x_inc)**2 + &
              !             ky_G(y_inc)**2) / &
              !             (2.0_WP*kz2_loc_G(z2_inc)))*sAl(ind)

           sAl(x_inc+1,y_inc+1,z2_inc+1) = exp(-h*sBeta_G*(abs(kx_G(x_inc)) + &
                          abs(ky_G(y_inc))) / &
                          (sqrt(abs(2.0_WP * kz2_loc_G(z2_inc))))) * &
                          sAl(x_inc+1,y_inc+1,z2_inc+1)

!          sAl(ind) = exp(-h*sBeta_G) * sAl(ind)

        end if

      end do
    end do
  end do

!     Inverse FFT

  call transform(tTransInfo_G%bplan, sAl, qOKL)

!CALL MPI_BARRIER(tProcInfo_G%comm,error)

!  IF (.NOT. qOKL) GOTO 1000

!     Scale the field data to normalize transforms

  sAl = sAl / ffact

!     Recombine masked field around boundary with remainder

  sAl = sAl + sAnb


  deallocate(mask, mask_z2)
  deallocate(sAnb)

END SUBROUTINE AbsorptionStep


! PUT IN ANOTHER FILE
!**************************************************
!**************************************************
!**************************************************
!**************************************************

!**************************************************


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Sets field to zero behind electron beam.
!> @param[inout] sA Complex field \f$ A_\bot \f$
!> @param[in] ffact Absorption coefficient

SUBROUTINE clearA(sA, qOK)

! qOK       OUT      Error flag; if .false. error has occured

  IMPLICIT NONE

  REAL(KIND=WP),INTENT(INOUT) :: sA(:)
  LOGICAL, INTENT(OUT) :: qOK
  INTEGER(KIND=IP) :: error,iz2A,nA,trans
  REAL(KIND=WP) :: loc_max,glo_max

!------------------------------------------------------
!                      Begin

  qOK = .FALSE.

! Set field to zero behind electrons...

! Find furthest back electron
  loc_max = MAXVAL(sElZ2_G)

  CALL MPI_ALLREDUCE(loc_max,glo_max,1,MPI_DOUBLE_PRECISION,&
       MPI_MAX,MPI_COMM_WORLD,error)

! Get corresponding node and increment by 1
  iz2A  = ceiling(glo_max/sLengthOfElmZ2_G)  + 2_IP

  trans=NX_G*NY_G
  nA=size(sA)/2

! Force trailing nodes = 0
  if (seedend<nA) then
    if (iz2A > seedend) then !if electron has passed end of seed
      if (iz2A < nA) then
        sA(((iz2A-1)*trans+1):nA)=0.0_WP !real
        sA(((iz2A-1)*trans+1+nA):2*nA)=0.0_WP !imag
      end if
    else
      sA(((seedend-1)*trans+1):nA)=0.0_WP !real
      sA(((seedend-1)*trans+1+nA):2*nA)=0.0_WP !imag
    end if
  end if
!Other initial condition is z2(0)=0
  sA(1:trans)=0.0_WP
  sA(nA+1:nA+trans)=0.0_WP

!              Set error flag and exit

  qOK = .TRUE.

  GOTO 2000

1000  CALL Error_log('Error in transforms:clearA',tErrorLog_G)

2000 CONTINUE

END SUBROUTINE clearA

!**************************************************

end module PDiff
