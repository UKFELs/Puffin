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
!> Module for calculating Fourier Transforms, and interfacing with FFTW.


module transforms

use ParallelInfoType
use TransformInfoType
!use FFTW_Constants
use Globals
use IO
use masks

use, intrinsic :: iso_c_binding
implicit none

!INCLUDE 'fftw3-mpi.f03'

real(kind=wp) :: tr_time_s, tr_time_e
type(C_PTR) :: cdata
complex(C_DOUBLE_COMPLEX), pointer :: Afftw(:,:,:)


!INCLUDE 'mpif.h'

contains


!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Top-level subroutine to create FFTW plans for Puffin. The subroutine
!> decides whether to call the 1D or 3D FFTW plan creation routines.
!> @param[in] nnodes Integer array of length 3, containing the number of
!> nodes in the radiation field nodes in x, y, and z2, corresponding to
!> element 1, 2, and 3 of the array respectively.
!> @param[out] qOK Logical error flag, == .true.
!> for successful completion of subroutine.

subroutine getTransformPlans4FEL(nnodes,qmeasure,qOK)

  implicit none

  integer(kind=ip), intent(in) :: nnodes(3)
  logical, intent(in) :: qmeasure
  logical, intent(out) :: qOK

!                 LOCAL ARGS
!
! qOKL - Local Logical error checker.

  logical :: qOKL
  integer error

!                   Begin

  qOK = .FALSE.

!     Determine if 1D or 3D transform is needed

  if (nnodes(iX_CG) == 1 .and. nnodes(iY_CG) == 1) then
     tTransInfo_G%qOneD = .true.
     !CALL getTransformPlans_OneD(nnodes(iZ2_CG),qOKL)
     qOKL = .true.
  else
     tTransInfo_G%qOneD = .false.
     call getTransformPlans_MultiD(nnodes,3,qmeasure,qOKL)
  end if

  if (.not. qOKL) goto 1000

  qOK = .true.
  goto 2000

1000  call Error_log('Error in transforms:getTransformPlans4FEL',tErrorLog_G)

2000 continue

end subroutine getTransformPlans4FEL


!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Subroutine to create 3-dimensional forward
!> and backward FFTW-MPI plans for Fourier transforms
!> of data of a specified size, and get the sizes of
!> the local data distribution before and after the
!> transform.
!> @param[in] sizes Integer array of length 3,
!> containing the number of nodes in the field
!> mesh in x, y and z2, respectively. (Use
!> the number of nodes in x and y = 1 if nDims = 1)
!> @param[in] nDims Number of dimensions in the field
!> mesh.
!> @param[out] qOK Logical error flag, == .true.
!> for successful completion of subroutine.

subroutine getTransformPlans_MultiD(sizes,nDims,qMeasure,qOK)

  implicit none

!
!                  ARGUMENTS
!
! sizes       IN   Array of size (3) describing the
!                  number of field nodes in x, y,
!                  and z2 respectively
! qOK         OUT  Error flag

  integer, intent(in) :: sizes(3)
  integer, intent(in) :: nDims
  logical, intent(in) :: qMeasure
  logical, intent(out) :: qOK


  integer(C_INTPTR_T) :: L, M, N, local_N, local_j_offset, &
                         alloc_local


!                         Begin

  qOK = .FALSE.


!                  Init FFTW-MPI

  call fftw_mpi_init()

  L = int(sizes(iX_CG), C_INTPTR_T)
  M = int(sizes(iY_CG), C_INTPTR_T)
  N = int(sizes(iZ2_CG), C_INTPTR_T)

!      Get the local sizes for the distribution of the transform data.


  alloc_local = fftw_mpi_local_size_3d(N, M, L, &
                                    tProcInfo_G%comm, &
                                    local_N, local_j_offset)


  tTransInfo_G%loc_nz2 = int(local_N, kind=ip)

  tTransInfo_G%loc_z2_start = &
                   int(local_j_offset, kind=ip)

  tTransInfo_G%total_local_size = &
                   int(alloc_local, kind=ip)


!     Allocate data

  cdata = fftw_alloc_complex(alloc_local)

!     Assign to fortran pointer

  call c_f_pointer(cdata, Afftw, [L,M,local_N])



!     Create plans

  if (tProcInfo_G%qroot) print*, 'Creating FFTW3 plans'

  if (qDiffraction_G) then
    if (qMeasure) then

      tTransInfo_G%fplan = fftw_mpi_plan_dft_3d(N, M, L, &
                            Afftw, Afftw, tProcInfo_G%comm, &
                            FFTW_FORWARD, FFTW_MEASURE)


      tTransInfo_G%bplan = fftw_mpi_plan_dft_3d(N, M, L, &
                            Afftw, Afftw, tProcInfo_G%comm, &
                            FFTW_BACKWARD, FFTW_MEASURE)

    else
      
      tTransInfo_G%fplan = fftw_mpi_plan_dft_3d(N, M, L, &
                            Afftw, Afftw, tProcInfo_G%comm, &
                            FFTW_FORWARD, FFTW_ESTIMATE)


      tTransInfo_G%bplan = fftw_mpi_plan_dft_3d(N, M, L, &
                            Afftw, Afftw, tProcInfo_G%comm, &
                            FFTW_BACKWARD, FFTW_ESTIMATE)      


    end if
  end if

  if (tProcInfo_G%qroot) print*, 'Created FFTW3 plans'
  if (tProcInfo_G%qroot) print*,  ''
  if (tProcInfo_G%qroot) print*, '***********************'

  qOK = .true.
  goto 2000

1000  call Error_log('Error in transforms:getTransformPlans_MultiD',tErrorLog_G)

2000 continue

end subroutine getTransformPlans_MultiD

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Top-level subroutine to clear the FFTW transform
!> plans from memory. It chooses whether to call
!> the equivalent 1D or 3D routines.
!> @param[out] qOK Logical error flag, == .true.
!> for successful completion of subroutine.

subroutine clearTransformPlans(qOK)

  implicit none

  logical, intent(out) :: qOK

!                     LOCAL ARGS
!
! qOKL       Local error flag

  logical :: qOKL

!                      Begin

  qOK = .false.

!     Identify 1D or 3D case and call appropriate subroutine

  if (tTransInfo_G%qOneD) then
!     CALL clearTransformPlans_OneD(qOKL)
  else
     call clearTransformPlans_ThreeD(qOKL)
  end if

  if (.not. qOKL) goto 1000

! Set error flag and exit

  qOK = .true.

  goto 2000

1000  call Error_log('Error in transforms:clearTransformPlans',tErrorLog_G)

2000 continue

end subroutine clearTransformPlans




!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Subroutine to delete the 3D FFTW plans from
!> memory.
!> @param[out] qOK Logical error flag, == .true.
!> for successful completion of subroutine.

subroutine clearTransformPlans_ThreeD(qOK)

  implicit none
!
! Subroutine to destroy multi-dimensional FFTW plans.
! Calls FFTW supplied subroutine.
!
!           ARGUMENTS
!
! qOK            error flag; .false. if error occurs
!
  logical, intent(out)  :: qOK

!                  Begin
  qOK = .false.


!       multi-D fftw plan deallocation

  call fftw_destroy_plan(tTransInfo_G%fplan)
  call fftw_destroy_plan(tTransInfo_G%bplan)

  call fftw_free(cdata)

  qOK = .TRUE.

  goto 2000

1000  call Error_log('Error in transforms:clearTransformPlans_ThreeD',tErrorLog_G)

2000 continue

end subroutine clearTransformPlans_ThreeD

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Top-level subroutine to call a FFTW transform.
!> This subroutine chooses whether to call a
!> 3D or a 1D tranform (can be backwards or
!> forwards transform).
!> @param[in] plan C pointer to FFTW3 plan.
!> @param[inout] local_in Real array containing the
!> untransformed field on input, and output as
!> the fourier transformed field. Should be distrbuted
!> amongst MPI ranks according to the FFTW3-MPI
!> layout specified in the planning stage.
!> @param[out] qOK Logical error flag, == .true.
!> for successful completion of subroutine.

subroutine Transform(plan, &
     local_in, &
     qOK)

  implicit none

  type(C_PTR), intent(inout) :: plan

  complex(C_DOUBLE_COMPLEX), pointer, intent(inout) :: local_in(:,:,:)

  logical, intent(out) :: qOK

!                 LOCAL ARGS
!
!       qOKL                  Local error flag

  logical :: qOKL

!                     Begin

  qOK = .FALSE.

!       Select 1D or 3D case and execute transform

  if (tTransInfo_G%qOneD) then
     !call Transform_OneD(plan,work,local_in,qOKL)
  else
     call Transform_MultiD(plan,local_in,qOKL)
  end if

  if (.not. qOKL) goto 1000

!              Set error flag and exit

  qOK = .TRUE.

  goto 2000

1000  call Error_log('Error in transforms:Transform',tErrorLog_G)

2000 continue

end subroutine Transform

!******************************************************

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Subroutine to perform the FFTW3 transform,
!> on the input array, according to the supplied
!> FFTW3 plan.
!> @param[in] plan C pointer to FFTW3 plan.
!> @param[inout] local_in Real array containing the
!> untransformed field on input, and output as
!> the fourier transformed field. It is NOT  transposed,
!> and is returned in normal order. See FFTW3 manual
!> for details.
!> @param[out] qOK Logical error flag, == .true.
!> for successful completion of subroutine.

subroutine Transform_MultiD(plan, local_in, qOK)

  implicit none

  type(C_PTR), intent(inout) :: plan

  complex(C_DOUBLE_COMPLEX), pointer, intent(inout) :: local_in(:,:,:)

  logical, intent(out) :: qOK


!                       Begin

  qOK = .FALSE.

!            Perform fourier transform

  call fftw_mpi_execute_dft(plan, local_in, local_in)

!              Set error flag and exit

  qOK = .true.

  goto 2000

1000  call Error_log('Error in transforms:Transform_MultiD',tErrorLog_G)

2000 continue

end subroutine Transform_MultiD

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> This subroutine calculates the k-vectors
!> at the field mesh nodes in x, y and z2
!> in Fourier space. These are global arrays,
!> and are not input or output. They are \f$ \bar{k}_x \f$,
!> \f$ \bar{k}_y \f$ and \f$ \bar{k}_{z2} \f$ used in the
!> diffraction step equation described in the Puffin manual.
!> the FFT's are performed in parallel with MPI, distrbuted across
!> the \f$ \bar{z}_2 \f$ dimension, and the !> \f$ \bar{k}_{z2} \f$
!> vectors are thus distributed amongst MPI ranks accordingly.
!> This subroutine also calculates the parallel
!> layout of the field data for the transform steps
!> for the purposes of MPI_allgathering. The layout
!> is stored in the recvs and displs arrays (see MPI
!> documentation, especially the recvs and displs arrays
!> typically used in MPI_ALLGATHERV, etc).
!> @param[inout] recvs Integer array of length equal to
!> the number of MPI ranks being used. Blank on
!> input, on output it describes the array
!> 'recieve counts.' Describes parallel field layout
!> for Fourier transform step. See MPI documentation,
!> especially MPI_ALLGATHERV etc.
!> @param[inout] displs Integer array of length equal to
!> the number of MPI ranks being used. Blank on
!> input, on output it describes the array
!> 'displacements' on each MPI rank. Describes parallel
!> field layout for Fourier transform step. See MPI
!> documentation, especially MPI_ALLGATHERV etc.
!> @param[out] qOK Logical error flag, == .true.
!> for successful completion of subroutine.

subroutine GetKValues(recvs,displs,qOK)

  implicit none

  integer(kind=ip),intent(inout) :: recvs(:),displs(:)
  logical, intent(out) :: qOK

!                   Local vars

  integer(kind=ip)  :: maxx, maxy, maxz2, i
  integer(kind=ip), dimension(:), allocatable  :: nx
  integer(kind=ip), dimension(:), allocatable  :: ny
  integer(kind=ip), dimension(:), allocatable  :: nz2
  real(KIND=WP), dimension(:), allocatable :: kz2_loc
  real(KIND=WP)  :: slengthX, sLengthY,sLengthZ2, pi
  integer(kind=ip) :: loc_z2_start,loc_nz2
  integer(kind=ip) :: error, trans

!                      Begin

  qOK = .FALSE.

  pi=4.0_WP*atan(1.0_WP)

!         Calculate the Full length along x,y,z2

  slengthX  =  real((NX_G),KIND=WP) * sLengthOfElmX_G
  slengthY  =  real((NY_G),KIND=WP) * sLengthOfElmY_G
  slengthZ2 =  real((NZ2_G),KIND=WP) * sLengthOfElmZ2_G

! Calculate maximum x, y and z2 values for the n arrays
! Arrays go from (0:N/2-1) then (-N/2:-1)

  maxx = ceiling(real(NX_G, kind=wp) / 2.0_wp)
  maxy = ceiling(real(NY_G, kind=wp) / 2.0_wp)
  maxz2 = ceiling(real(NZ2_G, kind=wp) / 2.0_wp)

!         Calculate nx, ny and nz2 values...
!         nx and ny are calculated in full by all
!         processes.
!         nz2 is calculated locally in segments

!              If 1D  use transpose order.
  if ( tTransInfo_G%qOneD) then
    loc_z2_start = tTransInfo_G%loc_z2_start_aft_trans
    loc_nz2 = tTransInfo_G%loc_nz2_aft_trans
  else
!       If not use normal order tranforms and get kx and ky
    loc_z2_start = tTransInfo_G%loc_z2_start
    loc_nz2 = tTransInfo_G%loc_nz2

    allocate(nx(0:NX_G-1))
    allocate(ny(0:NY_G-1))

    do i = 0,NX_G-1
      if ( i >= maxx ) then
        nx(i) = i - NX_G
      else
        nx(i) = i
      end if
    end do

    do i = 0,NY_G-1
      if (i >= maxy ) then
        ny(i) = i - NY_G
      else
        ny(i) = i
      end if
    end do

    kx_G=2.0_WP * pi * real(nx, kind=wp) / sLengthX
    ky_G=2.0_WP * pi * real(ny, kind=wp) / sLengthY
    deallocate(nx,ny)
  end if

!     Kz2 vals calculated in parallel according to FFTW distribution

  if (loc_nz2/=0) then
    allocate(nz2(0:loc_nz2-1))
    do i = 0,loc_nz2-1
      if ( loc_z2_start + i >= maxz2) then
        nz2(i) = loc_z2_start + i - NZ2_G
      else
        nz2(i) = loc_z2_start + i
      end if
    end do

    kz2_loc_G=2.0_WP*pi*real(nz2, kind=wp) / sLengthZ2

    deallocate(nz2)
  end if

!        Get gathering arrays for global all_to_all operations

  trans = NX_G*NY_G
  call getGathArrs(trans*tTransInfo_G%loc_nz2,recvs,displs)

!              Set error flag and exit

  qOK = .true.

  goto 2000

1000  call Error_log('Error in transforms:GetKValues',tErrorLog_G)

2000 continue

end subroutine GetKValues

end module Transforms
