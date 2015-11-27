module MASPin


  subroutine readMASPfile(zFile)

    character(*) :: zFile


    integer(kind=ip) :: nMPs, nMPsLoc
    integer :: fid
    
    real(kind=wp) :: dummy1, dummy2, dummy3


    fid = 132

    ! Split into local num per process

!   Don't care about globLen, etc, only care about numbers of
!   macroparticles...

    call splitBeam(nMPs, dummy1, tProcInfo_G%size, tProcInfo_G%rank, &
                   nMPsLoc, dummy2, dummy3)



    ! Allocate local MP arrays

    allocate(sElX_G(nMPsLoc),   &
             sElY_G(nMPsLoc),   &
             sElZ2_G(nMPsLoc),  &
             sElPX_G(nMPsLoc),  &
             sElPY_G(nMPsLoc),  &
             sElGam_G(nMPsLoc), &
             s_chi_bar_G(nMPsLoc), &
             s_Normalised_chi_G(nMPsLoc))



    ! read file

    do ir = 0,tProcInfo_G%size-1

      if (ir == tProcInfo_G%rank) then

        OPEN(UNIT=fid,FILE=zFile,IOSTAT=ios,&
             ACTION='READ',POSITION='REWIND')   
  
        do ij = 1,nBlanks 
        	READ() 
        end do

        do ij = 1,nMPsLoc

          read(UNIT=fid, FMT=*) sElX_G(ij), sElY_G(ij), &
                                sElPX_G(ij), sElPY_G(ij), &
                                sElGam_G(ij), sElZ2_G(ij), &
                                s_chi_bar_G(ij)

        end do

      end if

    end do

    ! exit

  end subroutine readMASPfile


end module MASPin

