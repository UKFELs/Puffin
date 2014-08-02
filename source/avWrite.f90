!************* THIS HEADER MUST NOT BE REMOVED *******************!
!** Copyright 2013, Lawrence Campbell and Brian McNeil.         **!
!** This program must not be copied, distributed or altered in  **!
!** any way without the prior permission of the above authors.  **!
!*****************************************************************!


module avwrite

use paratype
use arrayfunctions
use globals
use functions
use sddsROutput



implicit none

contains





  subroutine writeIntData(sA, sV)

    implicit none

! Outputs integrated data e.g. power, current
! bunching, etc

    real(kind=wp), intent(in) :: sA(:), sV(:)

    call oPower(sA)

  end subroutine writeIntData










  subroutine oPower(sA)

    implicit none

! This subroutine retrieves the power in z2 and
! outputs it to a file.
!
! inputs

    real(kind=wp), intent(in) :: sA(:)

! local vars
!
! wfield          array used to hold the field in 3D form

!    complex(kind=wp), allocatable :: wfield(:,:,:)
    real(kind=wp), allocatable :: power(:)

    allocate(power(nz2_g))

!    allocate(wfield(nx,ny,nz2))

!    wfield = complex(reshape(sA(1:nnodes),(/nx,ny,nz2/)), &
!                reshape(sA(nnodes+1:2*nnodes),(/nx,ny,nz2/)))

    PRINT*,'GETTING POWER *******'

    call gPower(sA, power)

    PRINT*,'GOT POWER *******'

    call writePower(power,tPowF)

    deallocate(power)

  end subroutine oPower












  subroutine writePower(power,powFType)

    implicit none

! This subroutine appends the power at the current
! step to the SDDS power file.
!
! inputs

    real(kind=wp), intent(in) :: power(:)
    type(cFileType), intent(inout) :: powFType

    integer(kind=ip) :: nnz2
    logical :: qOKL


    nnz2 = size(power)

    if (tProcInfo_G%qRoot) then 

      call OutputIntegrationData(powFType,&
              power,nnz2,qOKL)

    end if  

  end subroutine writePower







  subroutine initPFile(powFType, qForm)

    implicit none

! This subroutine initializes the SDDS power file
!
! inputs

    type(cFileType), intent(inout) :: powFType
    logical, intent(in) :: qForm

    character(32_IP) :: fname, vname
    logical :: qOKL

    real(kind=wp) :: lx, ly

    powFType%qformatted = qForm
    powFType%zFileName = 'power.sdds' !  filename
    vname = 'power' !  SDDS variable name


    if (tProcInfo_G%qRoot) then

      call SetUpDataFile(powFType%zFileName, powFType%qformatted, &
                         vname, powFType, qOKL)

    end if
  

    allocate(x_ax_G(NX_G), y_ax_G(NY_G))

    if ( .not. ((NX_G == 1_IP) .and. (NY_G == 1_IP)) ) then
    
      lx = sLengthOfElmX_G * (NX_G-1)
      ly = sLengthOfElmY_G * (NY_G-1)

      x_ax_G = linspace(-lx/2.0_WP, lx/2.0_WP, NX_G)
      y_ax_G = linspace(-ly/2.0_WP, ly/2.0_WP, NY_G)

    end if

  end subroutine initPFile
























  subroutine gPower(field, power)

    implicit none

! This subroutine fetches the temporal Power
! from the 3D field
!
!       ARGUMENTS

    real(kind=wp), intent(in) :: field(:)

    real(kind=wp), intent(out) :: power(:)
    


    if ((NX_G == 1_IP) .and. (NY_G == 1_IP)) then

      PRINT*,'WE ARE 1D!!!! *******'

      call fPower_1D(field, power)

    else 

      call fPower_3D(field,x_ax_G,y_ax_G,power)

    end if

  end subroutine gPower













  subroutine fPower_1D(field, power)

    real(kind=wp), intent(in) :: field(:)
    real(kind=wp), intent(out) :: power(:)

    integer(kind=ip) :: nno

    nno = size(field) / 2_IP
   
    PRINT*,'GOT NNO *******'

    power = abs(field(1:nno))**2.0_WP + abs(field(nno+1:2*nno))**2.0_WP

    PRINT*,'GOT POWER 1 *******'

  end subroutine fPower_1D





  subroutine fPower_3D(field,xaxis,yaxis, power)

! This subroutine fetches the temporal Power
! from the 3D field
!
!       ARGUMENTS

    real(kind=wp), intent(in) :: field(:), &
                                 xaxis(:), yaxis(:)

    real(kind=wp), intent(out) :: power(:)
    
    real(kind=wp), allocatable :: intens(:), intens2(:,:)
    integer(kind=ip) :: i, bt, et, ntr, nx, ny, nz2, nno


    nx = NX_G
    ny = NY_G
    nz2 = NZ2_G

    ntr = nx * ny ! Num of transverse nodes

    nno = size(field) / 2_IP ! total num of field nodes

    allocate(intens(nx*ny), intens2(nx,ny))

    !print*, 'starting loop round trans slices'

    do i = 1, nZ2

      bt = (i-1) * ntr + 1_IP 
      et = i * ntr

      intens = abs(field(bt:et))**2.0_WP + abs(field(bt+nno:et+nno))**2.0_WP

      intens2 = reshape(intens, (/nx,ny/))
      power(i) = m_trapz2D(xaxis, yaxis, intens2)

    end do

    !print*, 'end of fPower_3D'

    deallocate(intens,intens2)

  end subroutine fPower_3D

























  real function m_trapz2D(x, y, fxy)

    implicit none

! Integration over x and then y, using trapezoidal rule
!
!           ARGUMENTS

    real(kind=wp), dimension(:) :: x,y
    real(kind=wp), dimension(:,:) :: fxy
    integer(kind=ip) :: xe, ye, i, j
    real(kind=wp), allocatable :: cul(:)

    allocate(cul(size(x)))

    xe = size(x)

    do i = 1, xe
      cul(i) = m_trapz(y, fxy(i,:))
    end do

    m_trapz2D = m_trapz(x,cul)

    deallocate(cul)
        
  end function m_trapz2D










  real function m_trapz(x, y, lower, upper)

    implicit none

! Integration of y(x) by trapezoidal rule
!
!           ARGUMENTS

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

end module avwrite