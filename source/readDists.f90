module readDists


! This module was created to read in the dist files used by Puffin
!
! Dr Lawrence Campbell
! Center for Free Electron Laser Science (CFEL) 
! c/o DESY, building 99, Luruper Chaussee 149
! 22761 Hamburg
! Germany
! 
! Nov 2013

USE paratype
USE parallelInfoType

implicit none

contains


subroutine readPartDists(fname, z2m, gam_m, xm, ym, pxm, pym, &
	                       gam_d, pxd, pyd, Ne, nZ2)

  implicit none

  character(*), intent(in) :: fname

  integer(kind=ip), intent(in) :: nZ2

  real(kind=wp), intent(inout) :: z2m(:), & 
                               pxm(:), pym(:), xm(:), ym(:), &
                               gam_m(:), gam_d(:), pxd(:), pyd(:), &
                               Ne(:)

!              Local args

  real(kind=wp) :: npk, dz2

  real(kind=wp) :: eta, rho
  integer(kind=ip) :: ios, fid, k

!  px0 = pxoffset
!  py0 = pyoffset

  fid = 168

  open(unit=fid, file=fname, iostat=ios, &
       action='READ', position='REWIND')
  if  (ios/=0_ip) stop "Error opening file unit fid"


!     Read in blank lines

  call readBlanks(fid, 2)

!     Read in header

  call readDistHeader(fid, eta, rho, npk, dZ2)

  call readBlanks(fid, 4)  

  do k = 1,NZ2

    !        Read in z2, gamma, x, y, px, py etc...


    call readLine(fid, z2m(k), gam_m(k), xm(k), ym(k), pxm(k), pym(k), &
                  gam_d(k), pxd(k), pyd(k), Ne(k))



  end do

  close(unit=fid, iostat=ios, status="KEEP")
  if ( ios /= 0 ) stop "Error closing file unit fid"
  
end subroutine readPartDists


!!!!!!!!!!!!!!!!!!!!!!!!!


subroutine readDistHeader(fid, eta, rho, npk, dz2)

! Reads the header of the dist files

  implicit none

  real(kind=wp), intent(out) :: eta, rho, npk, dz2
  real(kind=wp) :: aw, lw, lr, Ipk 

  integer(kind=ip), intent(in) :: fid

  character(96) :: dum1, dum2, dum3, dum4, dum5, &
                   dum6, dum7, dum8 , dum9, dum10, dum11

  read(UNIT=fid, FMT=*) dum1, dum2, dz2, dum3, dum4, aw, dum5, dum6, lw, &
                        dum7, dum8, lr, dum9, dum10, dum11, Ipk

  read(UNIT=fid, FMT=*) dum1, dum2, eta, dum3, dum4, rho, dum5, dum6, npk

end subroutine readDistHeader

!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine getHeaders(fnames, npk, dz2, nZ2)

  character(*), intent(in) :: fnames(:)
  real(kind=wp), intent(out) :: npk, dz2(:)
  integer(kind=ip), intent(out) :: nZ2(:)

  real(kind=wp) :: rho, eta   ! dummy for now
  integer(kind=ip) :: ios, fid, ib, nbeams

  fid = 169

  nbeams = size(dZ2)

  do ib = 1, nbeams

    open(unit=fid, file=fnames(ib), iostat=ios, &
         action='READ', position='REWIND')
    if  (ios /= 0_IP) stop "Error closing file unit 169"

    call readBlanks(fid, 2)

    call readDistHeader(fid, eta, rho, npk, dz2(ib))

    close(unit=fid, status="KEEP")
    if ( ios /= 0_IP ) stop "Error closing file unit 169"

  end do    


  nZ2(:) = 5000_IP    ! TEMP, THIS SHOULD BE READ IN 
                     ! BUT YOU MUST CHANGE THE FILE
                     ! FORMAT

end subroutine getHeaders  
!!!!!!!!!!!!!!!!!!!!!!!!!!


subroutine readBlanks(fid,Nl)

! Read in Nl blank or unwanted lines in a file, 
! usually to advance to a desired position within 
! a file.

  implicit none

  integer(kind=ip), intent(in) :: fid,Nl

  integer(kind=ip) :: ln

  do ln = 1,Nl

    read(UNIT=fid, FMT=*)

  end do

end subroutine readBlanks


!!!!!!!!!!!!!!!!!!!!!!!!!!


subroutine readLine(fid, z2, gam, x, y, px, py, gam_d, pxd, pyd, Ne)

! Reads in one 'line' of the dist file. Note that a line is 1
! complete sequence of data, and not 1 actual line in the file.
!
!                  ARGUMENTS

  implicit none

  real(kind=wp), intent(inout) :: z2, gam, x, y, px, py, gam_d, pxd, pyd, Ne
  integer(kind=ip), intent(in) :: fid

  read(UNIT=fid, FMT=*) z2, gam, x, y, px, py, gam_d, pxd

  read(UNIT=fid, FMT=*) pyd, Ne


end subroutine readLine

!!!!!!!!!!!!!!!!!!!!!!!!!!

end module readDists