! Copyright 2012-2017, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

module cwrites

use paratype
use typempicomm


integer(kind=ip), allocatable :: wrarray(:)
logical :: qWrArray_G

contains 


  subroutine getWrArray(fname)

    character(*), intent(in) :: fname

!                LOCAL VARS

  INTEGER(KIND=IP)   :: i,ios,nw,error,ri

  integer(kind=ip) :: nwrts

  integer(kind=ip) :: cnt, cntw
  character(40) :: ztest


    cntw = 0_ip
    cnt = 0_ip

    nwrts = numOfWrites(fname)

    allocate(wrarray(nwrts))

      open(168,FILE=fname, IOSTAT=ios, STATUS='OLD', ACTION='READ', POSITION ='REWIND')

      if (ios /= 0) then
        print*, 'iostat = ', ios
        stop "OPEN(input file) not performed correctly, IOSTAT /= 0"
      end if


      do 

        read (168,*, IOSTAT=ios) ztest  ! probe the line

        if (ios < 0) then  ! if reached end of file:-

          if (tProcInfo_G%qroot) print*, "Reached end of file!! (for the second time)"
          !print*, "Turns out you had ", cnt, "lines in the file!!"
          !print*, "Turns out you had ", cntq, "quads in the file!! in lines ", lineq
          !print*, "Turns out you had ", cntu, "undulators in the file!!"

          exit

        else if (ios > 0) then

          print*, 'THIS LINE HAS NOTHING FOR ME', ios
          exit
          cnt = cnt + 1

        else

          if (ztest(1:2) == 'WR') then

            backspace(168)

            cntw = cntw + 1

            read (168,*, IOSTAT=ios) ztest, wrarray(cntw)  ! read step to write at

          end if

          cnt = cnt + 1
          !print*, 'hi'

        end if

      end do    

      close(168, STATUS='KEEP')



  end subroutine getWrArray





  function numOfWrites(fname)

!     Function to count the number of each type of element
!     in the lattice file
!
!                ARGUMENTS

  integer(kind=ip)          :: numOfWrites
  character(*), intent(in)  :: fname

!                LOCAL ARGS

  integer :: ios
  integer(kind=ip) :: cnt, cntw
  character(40) :: ztest

  ztest = ''
  cnt = 0
  cntw = 0


  open(168,FILE=fname, IOSTAT=ios, STATUS='OLD', ACTION='READ', POSITION ='REWIND')
  if (ios /= 0) then
    print*, 'iostat = ', ios
    stop "OPEN(input file) not performed correctly, IOSTAT /= 0"
  end if

  do 

    read (168,*, IOSTAT=ios) ztest  ! probe the line

    if (ios < 0) then  ! if reached end of file:-

      if (tProcInfo_G%qroot)  print*, "Reached end of the writing file!!"

      if (tProcInfo_G%qroot)  print*, "Turns out you had ", cntw, "writes in the file!!"

      exit

    else if (ios > 0) then

      print*, 'THIS LINE HAS NOTHING FOR ME'
      cnt = cnt + 1
      stop

    else

      if (ztest(1:2) == 'WR') then

        cntw = cntw + 1
!        print*, 'quad number ', cntq, ' has params ', quad1, quad2
      end if
      cnt = cnt + 1
      !print*, 'hi'

    end if

  end do

  close(168, STATUS='KEEP')

  numOfWrites = cntw


  end function numOfWrites

end module cwrites

