! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell & Jonathan Smith (Tech-X UK Ltd)
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> This module contains routines for calculating the reduced or integrated data
!> for Puffin output. This module also writes this data, in the SDDS case.

module avwrite

use paratype
use arrayfunctions
use globals
use functions
use ParallelSetUp
use parafield


implicit none

contains

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Subroutine to calculate the radiation power. The power
!> is calculated from the global distributed radiation field arrays.
!> @params[out] power Array containing the calculated power
!> as a function of z2. The power is calculated on an equispaced
!> 1D mesh at nodes equal to the nodes in z2 of the radiation field mesh.
!> So e.g. the power node separation is sLengthOfElmZ2_G.

  subroutine gPowerP(power)

    implicit none


    real(kind=wp), intent(out) :: power(:)  !< Returned power array

    real(kind=wp), allocatable :: fr_power(:), &  !< Power in 'front' field section
                                  bk_power(:), &  !< Power in 'back' field section
                                  ac_power(:)     !< Power in 'active' field section

    integer :: error  !< Error flag for MPI routines

    allocate(ac_power(mainlen), fr_power(tlflen4arr), bk_power(tlelen4arr))

    if ((ffe_GGG > 0) .and. (tlflen > 0) ) then

      call gPower(fr_rfield, fr_ifield, fr_power)

    end if

    call gPower(ac_rfield(1:mainlen*ntrnds_G), ac_ifield(1:mainlen*ntrnds_G), ac_power)

    if ((ees_GGG < nz2_G) .and. (tlelen > 0) ) then

      call gPower(bk_rfield, bk_ifield, bk_power)

    end if

    call UpdateGlobalPow(fr_power, ac_power, bk_power, power)

    deallocate(fr_power, ac_power, bk_power)

  end subroutine gPowerP

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> This subroutine sets up array structures to be used in the calculation of
!> the power.
!> @params lx Length of field mesh in x
!> @params ly Length of field mesh in y

  subroutine initPowerCalc()

    real(kind=wp) :: lx, &
                     ly

    if (allocated(x_ax_G)) then
      deallocate(x_ax_G)
    end if

    if (allocated(y_ax_G)) then
      deallocate(y_ax_G)
    end if

    allocate(x_ax_G(NX_G), y_ax_G(NY_G))

    if ( .not. ((NX_G == 1_IP) .and. (NY_G == 1_IP)) ) then

      lx = sLengthOfElmX_G * (NX_G-1)
      ly = sLengthOfElmY_G * (NY_G-1)

      x_ax_G = linspace(-lx/2.0_WP, lx/2.0_WP, NX_G)
      y_ax_G = linspace(-ly/2.0_WP, ly/2.0_WP, NY_G)

    end if

  end subroutine initPowerCalc




!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> This subroutine fetches the temporal Power
!> from the 3D or 1D field mesh. It chooses the appropriate
!> 1D or 3D power calculation based on the mesh.
!> @params[in] rfield Real component of A_perp
!> @params[in] ifield Imag component of A_perp
!> @params[out] power Temporal power

  subroutine gPower(rfield, ifield, power)

    implicit none

    real(kind=wp), intent(in) :: rfield(:), &
                                 ifield(:)

    real(kind=wp), intent(out) :: power(:)

    power = 0.0_wp     ! init

    if ((NX_G == 1_IP) .and. (NY_G == 1_IP)) then

      call fPower_1D(rfield, ifield, power)

    else

      call fPower_3D(rfield,ifield,x_ax_G,y_ax_G,power)

    end if

  end subroutine gPower

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> This subroutine fetches the temporal Power
!> from the 1D field
!> @params[in] rfield Real component of A_perp
!> @params[in] ifield Imag component of A_perp
!> @params[out] power Temporal power

  subroutine fPower_1D(rfield, ifield, power)

    real(kind=wp), intent(in) :: rfield(:), ifield(:)
    real(kind=wp), intent(out) :: power(:)

    power = abs(rfield)**2.0_WP + abs(ifield)**2.0_WP

  end subroutine fPower_1D


!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> This subroutine fetches the temporal Power
!> from the 3D field
!> @params[in] rfield Real component of A_perp
!> @params[in] ifield Imag component of A_perp
!> @params[in] xaxis Coordinates of mesh points in x
!> @params[in] yaxis Coordinates of mesh points in y
!> @params[out] power Temporal power


  subroutine fPower_3D(rfield,ifield,xaxis,yaxis, power)

    real(kind=wp), intent(in) :: rfield(:), ifield(:), &
                                 xaxis(:), yaxis(:)

    real(kind=wp), intent(out) :: power(:)

    real(kind=wp), allocatable :: intens(:), intens2(:,:)
    integer(kind=ip) :: i, bt, et, ntr, nx, ny, nz2, nno

    integer :: error

    nx = NX_G
    ny = NY_G

    ntr = nx * ny ! Num of transverse nodes

    nz2 = size(power)

    allocate(intens(nx*ny), intens2(nx,ny))

    do i = 1, nZ2

      bt = (i-1) * ntr + 1_IP
      et = i * ntr

      intens = abs(rfield(bt:et))**2.0_WP + abs(ifield(bt:et))**2.0_WP

      intens2 = reshape(intens, (/nx,ny/))

      power(i) = m_trapz2D(xaxis, yaxis, intens2)

    end do

    deallocate(intens,intens2)

  end subroutine fPower_3D



!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Simple 2D (x-y) integration over equispaced 2D mesh using
!> trapezoidal rule.
!> @params[in] x Coordinates of mesh nodes in x
!> @params[in] y Coordinates of mesh nodes in y
!> @params[in] fxy 2D function to integrate (values on the mesh)

  real function m_trapz2D(x, y, fxy)

    implicit none

    real(kind=wp), dimension(:) :: x,y
    real(kind=wp), dimension(:,:) :: fxy
    integer(kind=ip) :: xe, ye, i, j
    real(kind=wp), allocatable :: cul(:)

    allocate(cul(size(y)))

    ye = size(y)

    do i = 1, ye
      cul(i) = m_trapz(x, fxy(:,i))
    end do

    m_trapz2D = m_trapz(y,cul)

    deallocate(cul)

  end function m_trapz2D

!> @author
!> Lawrence Campbell,
!> University of Strathclyde, 
!> Glasgow, UK
!> @brief
!> Simple 1D integration over function, using
!> trapezoidal rule.
!> @params[in] x Coordinates of mesh nodes in x
!> @params[in] y Function y(x) at mesh points

  real function m_trapz(x, y, lower, upper)

    implicit none

    real(kind=wp), dimension(:) :: x,y
    integer(kind=ip), optional :: lower, upper
    integer(kind=ip) :: l, u, i

    if (present(lower)) then
      l = lower
    else
      l = 1
    end if

    if (present(upper)) then
      u = upper
    else
      u = size(x)
    end if

    m_trapz = 0.0_WP

    do i = l,u-1
      m_trapz = m_trapz + (x(i+1)-x(i))*(y(i+1)+y(i))/2.0
    end do

  end function m_trapz






  subroutine getCurrNpts(sam_len, npts)

    real(kind=wp), intent(in) :: sam_len
    integer(kind=ip), intent(out) :: npts

    npts = ceiling(sLengthOfElmZ2_G*(NZ2_G-1)/sam_len)  + 1_ip ! round up over length of system

  end subroutine getCurrNpts




!> Calculate the current through interpolating the charge into bins

  subroutine getCurr(sam_len, Iarray)

    use typesAndConstants

    real(kind=wp), intent(in) :: sam_len !< length of bins in z2
    real(kind=wp), intent(inout) :: Iarray(:) !< data containing the current info

    integer(kind=ip) :: ij, inl, inu, inlpb, inupb !<electron indices over which to integrate
    real(kind=wp) :: li1, li2, locz2 !<interpolation fractions

    Iarray = 0.0_wp   ! initialize

    if (fieldMesh == iPeriodic) then

      do ij = 1, size(sElX_G)

        !   Array indices
        inl = ceiling(sElZ2_G(ij)/sam_len)
        inu = inl + 1_ip

        if (inu >= npts_I_G) then
          inuPB = inu - floor(real(inu,kind=wp)/ real(npts_I_G, kind=wp)) * npts_I_G + 1
        else
          inuPB = inu
        end if
  
        if (inl >= npts_I_G) then
          inlPB = inl - floor(real(inl,kind=wp)/real(npts_I_G,kind=wp)) * npts_I_G + 1
        else
          inlPB = inl
        end if

!      if ((inu > npts_I_G) .or. (inl<=0)) then
        if (inl<=0) then
           print*, 'NODES OUTSIDE BOUNDS'
           stop
        end if
       
       
        ! Interpolation fractions
        locz2 = sElZ2_G(ij) - real((inl-1_ip),kind=wp) * sam_len
        li2 = locz2 / sam_len
        li1 = 1_wp - li2

        if ((li2 < 0.0_wp) .or. (li1<0.0_wp)) then
          print*, 'Unable to calculate correct interpolation fraction'
          print*, 'Particle coords'
          print*, sElX_G(ij)
          print*, sElY_G(ij)
          print*, sElZ2_G(ij)
          print*, 'Interps are negative!'
          STOP
        end if

        ! interpolate onto current mesh
        Iarray(inlPB) = li1 * s_chi_bar_G(ij) + Iarray(inlPB)
        Iarray(inuPB) = li2 * s_chi_bar_G(ij) + Iarray(inuPB)

      end do

    else

      do ij = 1, size(sElX_G)

        !   Array indices
        inl = ceiling(sElZ2_G(ij)/sam_len)
        inu = inl + 1

        if ((inu > npts_I_G) .or. (inl<=0)) then
          print*, 'NODES OUTSIDE BOUNDS'
          STOP
        end if

        ! Interpolation fractions
        locz2 = sElZ2_G(ij) - real((inl-1_ip),kind=wp) * sam_len
        li2 = locz2 / sam_len
        li1 = 1_wp - li2

        if ((li2 < 0.0_wp) .or. (li1<0.0_wp)) then
          print*, 'Unable to calculate correct interpolation fraction'
          print*, 'Particle coords'
          print*, sElX_G(ij)
          print*, sElY_G(ij)
          print*, sElZ2_G(ij)
          print*, 'Interps are negative!'
          STOP
        end if

        ! interpolate onto current mesh
        Iarray(inl) = li1 * s_chi_bar_G(ij) + Iarray(inl)
        Iarray(inu) = li2 * s_chi_bar_G(ij) + Iarray(inu)

      end do

    end if

    call sum2RootArr(Iarray, size(Iarray), 0)

    Iarray = Iarray * npk_bar_G    ! N_e at each node
    if (qOneD_G) Iarray = Iarray * ata_G
    Iarray = Iarray * q_e / sam_len    ! dQ / dz2
    Iarray = Iarray * c / lc_G         ! dQ / dt

  end subroutine getCurr

!> getSliceTwiss computes twiss parameters
!! in universal coordinates - so need scaling
!! to get back to SI
  subroutine getSliceTwiss(nslices,slicetrim,aveX,aveY,avePX,avePY, &
     sdX,sdY,sdpx,sdpy,eX,eY,ax,ay,bx,by, &
     aveGamma,aveDgamma,b1,b2,b3,b4,b5,sdata)
    integer(kind=ip), intent(in) :: nslices
    real(kind=wp), intent(in) :: slicetrim
    real(kind=wp), intent(out), DIMENSION(nslices) :: aveX,aveY,avePX,avePY,aveGamma,aveDgamma
    real(kind=wp), intent(out), DIMENSION(nslices) :: sdX, sdY, sdpx, sdpy, eX, eY, ax, ay, bx, by
    real(kind=wp), intent(out), DIMENSION(nslices) :: b1,b2,b3,b4,b5,sdata
!    real(kind=wp), intent(out) :: sdX(nslices)
!    real(kind=wp), intent(out) :: sdY(nslices)
!    real(kind=wp), intent(out) :: eX(:)
!    real(kind=wp), intent(out) :: eY(:)
!    real(kind=wp), intent(out) :: aX(:)
!    real(kind=wp), intent(out) :: aY(:)
!    real(kind=wp), intent(out) :: bX(:)
!    real(kind=wp), intent(out) :: bY(:)
!    real(kind=wp), intent(out) :: aveGamma(nslices)
!    real(kind=wp), intent(out) :: aveDgamma(:)
    integer(kind=ip),parameter :: ncoord=6
    integer(kind=ip) :: ipc,ic1,ic2,is !< particle,coord,slice index
    real(kind=wp) :: sliceSizeZ2
    real(kind=wp),DIMENSION(nslices) :: b1r,b2r,b3r,b4r,b5r
    real(kind=wp),DIMENSION(nslices) :: b1i,b2i,b3i,b4i,b5i

    real(kind=wp) :: meanX(nslices), meanY(nslices), &
                     meanGam(nslices), meanPX(nslices), &
                     meanPY(nslices), &
                     meanXX(nslices), meanXPX(nslices), &
                     meanXZ2(nslices), meanXGam(nslices), &
                     meanXPY(nslices), meanXY(nslices), &
                     meanYY(nslices), meanYZ2(nslices), &
                     meanYPX(nslices), meanYPY(nslices), &
                     meanYGam(nslices), &
                     meanPXPX(nslices), meanPYPY(nslices), &
                     meanGamGam(nslices)
                     
    integer :: error


    aveX = 0.0_wp  ! initialize
    aveY = 0.0_wp  ! initialize
    avepX = 0.0_wp  ! initialize
    avepY = 0.0_wp  ! initialize
    sdX = 0.0_wp   ! initialize
    sdY = 0.0_wp   ! initialize
    sdpX = 0.0_wp   ! initialize
    sdpY = 0.0_wp   ! initialize
    eX = 0.0_wp    ! initialize
    eY = 0.0_wp    ! initialize
    aX = 0.0_wp    ! initialize
    aY = 0.0_wp    ! initialize
    bX = 0.0_wp    ! initialize
    bY = 0.0_wp    ! initialize
    aveGamma = 0.0_wp   ! initialize
    aveDgamma = 0.0_wp   ! initialize
    sdata = 0.0_wp   ! initialize

    meanX = 0.0_wp
    meanY = 0.0_wp
    meanGam = 0.0_wp
    meanPX = 0.0_wp
    meanPY = 0.0_wp
    meanXX = 0.0_wp
    meanXPX = 0.0_wp
    meanXZ2 = 0.0_wp
    meanXGam = 0.0_wp
    meanXPY = 0.0_wp
    meanXY = 0.0_wp
    meanYY = 0.0_wp
    meanYZ2 = 0.0_wp
    meanYPX = 0.0_wp
    meanYPY = 0.0_wp
    meanYGam = 0.0_wp
    meanPXPX = 0.0_wp
    meanPYPY = 0.0_wp
    meanGamGam = 0.0_wp
!
! Should create a parameter hno (harmonic number)
! and loop.
!

    b1r = 0.0_wp   ! initialize
    b2r = 0.0_wp   ! initialize
    b3r = 0.0_wp   ! initialize
    b4r = 0.0_wp   ! initialize
    b5r = 0.0_wp   ! initialize
    b1i = 0.0_wp   ! initialize
    b2i = 0.0_wp   ! initialize
    b3i = 0.0_wp   ! initialize
    b4i = 0.0_wp   ! initialize
    b5i = 0.0_wp   ! initialize
!    sliceSizeZ2=(sLengthOfElmZ2_G*NBZ2)/(nslices-1)
!    sliceSizeZ2=((sLengthOfElmZ2_G*NZ2_G)-slicetrim)/(nslices)
    sliceSizeZ2=4*pi*srho_g
!    print *,"evaluating slices of size",4*pi*srho_g,sliceSizeZ2,slicetrim
    if (iNumberElectrons_G > 0_ipl) then


    do ipc = 1, iNumberElectrons_G

      is = ceiling(sElZ2_G(ipc)/sliceSizeZ2)

      if (fieldMesh == iPeriodic) then

        if (is>=nslices) then
          is = is - (floor(real(is, kind=wp) / real(nslices, kind=wp)) * nslices)+1
        end if

        if (is <1) then
          print*,"slice index, is, out of bounds in slice computation"
          goto 1000
        end if
      
      else

        if ((is>nslices) .or. (is <1)) then
          print*,"slice index, is, out of bounds in slice computation"
          goto 1000
        end if

      end if

!      if (mod(ip,10000) .eq. 0) then
!        print*,"at particle ",ip
!      end if
      sdata(is)=sdata(is)+s_chi_bar_G(ipc)
!      do ic1 = 1,ncoord
!        select case (ncoord)
!          case (1) csdata(ic1,is)=s_chi_bar_G(ip)*sX_G
!        !! Would be tidier, but sadly our data is not structured nicely for this
!      end do
      meanX(is)=meanX(is)+s_chi_bar_G(ipc)*sElX_G(ipc)
      meanY(is)=meanY(is)+s_chi_bar_G(ipc)*sElY_G(ipc)
      meanPX(is)=meanPX(is)+s_chi_bar_G(ipc)*sElpX_G(ipc)
      meanPY(is)=meanPY(is)-s_chi_bar_G(ipc)*sElpY_G(ipc)
      meanGam(is)=meanGam(is)+s_chi_bar_G(ipc)*sElGam_G(ipc)
      
      
      
      meanXX(is)=meanXX(is)+s_chi_bar_G(ipc)*sElX_G(ipc)*sElX_G(ipc)
      meanXY(is)=meanXY(is)+s_chi_bar_G(ipc)*sElX_G(ipc)*sElY_G(ipc)
      meanXZ2(is)=meanXZ2(is)+s_chi_bar_G(ipc)*sElX_G(ipc)*sElz2_G(ipc)
      meanXPX(is)=meanXPX(is)+s_chi_bar_G(ipc)*sElX_G(ipc)*sElpX_G(ipc)
      meanXPY(is)=meanXPY(is)-s_chi_bar_G(ipc)*sElX_G(ipc)*sElpy_G(ipc)
      meanXGam(is)=meanXGam(is)+s_chi_bar_G(ipc)*sElX_G(ipc)*sElgam_G(ipc)
      
      meanYY(is)=meanYY(is)+s_chi_bar_G(ipc)*sElY_G(ipc)*sElY_G(ipc)
      meanYZ2(is)=meanYZ2(is)+s_chi_bar_G(ipc)*sElY_G(ipc)*sElz2_G(ipc)
      meanYPX(is)=meanYPX(is)+s_chi_bar_G(ipc)*sElY_G(ipc)*sElpX_G(ipc)
      meanYPY(is)=meanYPY(is)-s_chi_bar_G(ipc)*sElY_G(ipc)*sElpy_G(ipc)
      meanYGam(is)=meanYGam(is)+s_chi_bar_G(ipc)*sElY_G(ipc)*sElgam_G(ipc)
      
      meanPXPX(is)=meanPXPX(is)+s_chi_bar_G(ipc)*sElpX_G(ipc)*sElpX_G(ipc)      
      meanPYPY(is)=meanPYPY(is)+s_chi_bar_G(ipc)*sElpY_G(ipc)*sElpY_G(ipc)      
      meanGamGam(is)=meanGamGam(is)+s_chi_bar_G(ipc)*sElgam_G(ipc)*sElgam_G(ipc)


      b1r(is) = b1r(is) + s_chi_bar_G(ipc)*cos(sElz2_G(ipc)/(2*sRho_G)) &
                          * ( (is*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )

      if (fieldMesh /= iPeriodic) b1r(is+1) = b1r(is+1) + &
                    s_chi_bar_G(ipc)*cos(sElz2_G(ipc)/(2*sRho_G)) &
                          * ( -((is-1)*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )

      b1i(is) = b1i(is) + s_chi_bar_G(ipc)*sin(sElz2_G(ipc)/(2*sRho_G)) &
                          * ( (is*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )

      if (fieldMesh /= iPeriodic) b1i(is+1) = b1i(is+1) + &
                             s_chi_bar_G(ipc)*sin(sElz2_G(ipc)/(2*sRho_G)) &
                          * ( -((is-1)*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )


!      b1r(is)=b1r(is)+s_chi_bar_G(ip)*cos(sElz2_G(ip)/(2*sRho_G))
!      b1i(is)=b1i(is)+s_chi_bar_G(ip)*sin(sElz2_G(ip)/(2*sRho_G))

!      b2r(is)=b2r(is)+s_chi_bar_G(ip)*cos(sElz2_G(ip)/(4*sRho_G))



!      b2i(is)=b2i(is)+s_chi_bar_G(ip)*sin(sElz2_G(ip)/(4*sRho_G))


      b2r(is) = b2r(is) + s_chi_bar_G(ipc)*cos(2.0_wp*sElz2_G(ipc)/(2.0_wp*sRho_G)) &
                          * ( (is*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )

      if (fieldMesh /= iPeriodic) b2r(is+1) = b2r(is+1) + &
                          s_chi_bar_G(ipc)*cos(2.0_wp*sElz2_G(ipc)/(2.0_wp*sRho_G)) &
                          * ( -((is-1)*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )

      b2i(is) = b2i(is) + s_chi_bar_G(ipc)*sin(2.0_wp*sElz2_G(ipc)/(2.0_wp*sRho_G)) &
                          * ( (is*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )

      if (fieldMesh /= iPeriodic) b2i(is+1) = b2i(is+1) + &
                       s_chi_bar_G(ipc)*sin(2.0_wp*sElz2_G(ipc)/(2.0_wp*sRho_G)) &
                          * ( -((is-1)*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )



      b3r(is) = b3r(is) + s_chi_bar_G(ipc)*cos(3.0_wp*sElz2_G(ipc)/(2.0_wp*sRho_G)) &
                          * ( (is*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )

      if (fieldMesh /= iPeriodic) b3r(is+1) = b3r(is+1) + &
                         s_chi_bar_G(ipc)*cos(3.0_wp*sElz2_G(ipc)/(2.0_wp*sRho_G)) &
                          * ( -((is-1)*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )

      b3i(is) = b3i(is) + s_chi_bar_G(ipc)*sin(3.0_wp*sElz2_G(ipc)/(2.0_wp*sRho_G)) &
                          * ( (is*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )

      if (fieldMesh /= iPeriodic) b3i(is+1) = b3i(is+1) + &
                s_chi_bar_G(ipc)*sin(3.0_wp*sElz2_G(ipc)/(2.0_wp*sRho_G)) &
                          * ( -((is-1)*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )





      b4r(is) = b4r(is) + s_chi_bar_G(ipc)*cos(4.0_wp*sElz2_G(ipc)/(2.0_wp*sRho_G)) &
                          * ( (is*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )

      if (fieldMesh /= iPeriodic) b4r(is+1) = b4r(is+1) + &
                     s_chi_bar_G(ipc)*cos(4.0_wp*sElz2_G(ipc)/(2.0_wp*sRho_G)) &
                          * ( -((is-1)*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )

      b4i(is) = b4i(is) + s_chi_bar_G(ipc)*sin(4.0_wp*sElz2_G(ipc)/(2.0_wp*sRho_G)) &
                          * ( (is*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )

      if (fieldMesh /= iPeriodic) b4i(is+1) = b4i(is+1) + &
                   s_chi_bar_G(ipc)*sin(4.0_wp*sElz2_G(ipc)/(2.0_wp*sRho_G)) &
                          * ( -((is-1)*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )




      b5r(is) = b5r(is) + s_chi_bar_G(ipc)*cos(5.0_wp*sElz2_G(ipc)/(2.0_wp*sRho_G)) &
                          * ( (is*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )

      if (fieldMesh /= iPeriodic) b5r(is+1) = b5r(is+1) + &
                      s_chi_bar_G(ipc)*cos(5.0_wp*sElz2_G(ipc)/(2.0_wp*sRho_G)) &
                          * ( -((is-1)*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )

      b5i(is) = b5i(is) + s_chi_bar_G(ipc)*sin(5.0_wp*sElz2_G(ipc)/(2.0_wp*sRho_G)) &
                          * ( (is*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )

      if (fieldMesh /= iPeriodic) b5i(is+1) = b5i(is+1) + &
                      s_chi_bar_G(ipc)*sin(5.0_wp*sElz2_G(ipc)/(2.0_wp*sRho_G)) &
                          * ( -((is-1)*sliceSizeZ2 - sElz2_G(ipc)) / sliceSizeZ2 )







!      b3r(is)=b3r(is)+s_chi_bar_G(ip)*cos(sElz2_G(ip)/(6*sRho_G))
!      b3i(is)=b3i(is)+s_chi_bar_G(ip)*sin(sElz2_G(ip)/(6*sRho_G))
!      b4r(is)=b4r(is)+s_chi_bar_G(ip)*cos(sElz2_G(ip)/(8*sRho_G))
!      b4i(is)=b4i(is)+s_chi_bar_G(ip)*sin(sElz2_G(ip)/(8*sRho_G))
!      b5r(is)=b5r(is)+s_chi_bar_G(ip)*cos(sElz2_G(ip)/(10*sRho_G))
!      b5i(is)=b5i(is)+s_chi_bar_G(ip)*sin(sElz2_G(ip)/(10*sRho_G))
!    call sum2RootArr(cs2data(, size(cs2data), 0)
1000    end do
      end if
! print*,"Bringing arrays onto rank0"


    call sum2RootArr(sdata, nslices, 0)
    call sum2RootArr(meanX, nslices, 0)
    call sum2RootArr(meanY, nslices, 0)
    call sum2RootArr(meanPX, nslices, 0)
    call sum2RootArr(meanPY, nslices, 0)
    call sum2RootArr(meanGam, nslices, 0)
    call sum2RootArr(meanXX, nslices, 0)
    call sum2RootArr(meanXPX, nslices, 0)
    call sum2RootArr(meanPXPX, nslices, 0)
    call sum2RootArr(meanPYPY, nslices, 0)
    call sum2RootArr(meanYPY, nslices, 0)
    call sum2RootArr(meanYY, nslices, 0)
    call sum2RootArr(meanGamGam, nslices, 0)
    call sum2RootArr(b1r, size(b1r), 0)
    call sum2RootArr(b1i, size(b1i), 0)
    call sum2RootArr(b2r, size(b2r), 0)
    call sum2RootArr(b2i, size(b2i), 0)
    call sum2RootArr(b3r, size(b3r), 0)
    call sum2RootArr(b3i, size(b3i), 0)
    call sum2RootArr(b4r, size(b4r), 0)
    call sum2RootArr(b4i, size(b4i), 0)
    call sum2RootArr(b5r, size(b5r), 0)
    call sum2RootArr(b5i, size(b5i), 0)

!! All ranks calculate, but correct data is now only on rank0.

    if (tProcInfo_G%qRoot) then
    Do is=1,nslices
      if (sdata(is)>0.0_wp) then
        aveX(is)=meanX(is)/sdata(is)
        aveY(is)=meanY(is)/sdata(is)
        avepX(is)=meanPX(is)/sdata(is)
        avepY(is)=meanPY(is)/sdata(is)
        aveGamma(is)=meanGam(is)/sdata(is)
        sdx(is)= (meanXX(is)/sdata(is)) - (meanX(is) / sdata(is) )**2.0_wp
        sdy(is)= (meanYY(is)/sdata(is)) - (meanY(is) / sdata(is) )**2.0_wp
        sdpx(is)=(meanPXPX(is)/sdata(is)) - (meanPX(is) / sdata(is) )**2.0_wp
        sdpy(is)=(meanPYPY(is)/sdata(is)) - (meanPY(is) / sdata(is) )**2.0_wp
        avedgamma(is) = (meanGamGam(is)/sdata(is)) - (meanGam(is)/sdata(is))**2.0_wp
        if (tprocinfo_g%qroot) then
!          print*, "ex terms for slice "
!          print*, is
!          print*,cs2data(1,1,is)
!          print*,cs2data(4,4,is)
!          print*,cs2data(1,4,is)
!          print*,cs2data(1,1,is)*cs2data(4,4,is)
!          print*,cs2data(1,4,is)**2
        end if
        
        ex(is) = sdx(is) * sdpx(is) - &
                (meanxpx(is) / sdata(is) - (aveX(is)*avepX(is)) ) 

        ey(is) = sdy(is) * sdpy(is) - &
                (meanypy(is) / sdata(is) - (aveY(is)*avepY(is)) ) 

        b1(is)=sqrt(b1r(is)**2+b1i(is)**2)/sliceSizeZ2
        b2(is)=sqrt(b2r(is)**2+b2i(is)**2)/sliceSizeZ2
        b3(is)=sqrt(b3r(is)**2+b3i(is)**2)/sliceSizeZ2
        b4(is)=sqrt(b4r(is)**2+b4i(is)**2)/sliceSizeZ2
        b5(is)=sqrt(b5r(is)**2+b5i(is)**2)/sliceSizeZ2
        
        
      else
        aveX(is)=0._wp
        aveY(is)=0._wp
        avePX(is)=0._wp
        avePY(is)=0._wp
        aveGamma(is)=0._wp
        sdX(is)=0._wp
        sdY(is)=0._wp
        sdpX(is)=0._wp
        sdpY(is)=0._wp
        eX(is)=0._wp
        eY(is)=0._wp
        avedgamma(is)=0._wp
        b1(is)=0._wp
        b2(is)=0._wp
        b3(is)=0._wp
        b4(is)=0._wp
        b5(is)=0._wp
      end if
    end do

  where (sdx<0.0_wp) sdx = 0.0_wp
  where (sdy<0.0_wp) sdy = 0.0_wp
  where (sdpx<0.0_wp) sdpx = 0.0_wp
  where (sdpy<0.0_wp) sdpy = 0.0_wp
  where (avedgamma<0.0_wp) avedgamma = 0.0_wp
  where (ex<0.0_wp) ex = 0.0_wp
  where (ey<0.0_wp) ey = 0.0_wp
  
  sdx = sqrt(sdx)
  sdy = sqrt(sdy)
  sdpx = sqrt(sdpx)
  sdpy = sqrt(sdpy)
  avedgamma = sqrt(avedgamma)
  ex = sqrt(ex)
  ey = sqrt(ey)
  
  end if

  call mpi_barrier(tProcInfo_G%comm, error)

  end subroutine getSliceTwiss

  subroutine getBunchingFundamental(nslices,bunching)
    integer(kind=ip), intent(in) :: nslices
    real(kind=wp), intent(out) :: bunching(:)
    bunching = 0.0_wp    ! initialize
  end subroutine getBunchingFundamental


end module avwrite
