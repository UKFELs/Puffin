program puffin

    use puffin_mod, only: puffin_main
    use puffin_kinds, only: ip
    use IO
    use MPI
    use puffin_mpiInfo, only: tProcInfo_G

    character(1024_IP) :: input_file_name
    integer(KIND=IP)    :: error, provided
    logical :: qOK

!     Read in input file name
!     (input on command line as variable at runtime)

    call getarg(1,input_file_name)
    call MPI_INIT_THREAD(MPI_THREAD_FUNNELED, provided, error)
    call puffin_main(input_file_name, qOK)
    if (.not. qOK) then
      if (tProcInfo_G%qroot) print*, 'Puffin simulation failed, check error log for details, ', tErrorLog_G%zFileName
    else
      if (tProcInfo_G%qroot) print*, 'Puffin simulation completed'
    end if
    call MPI_FINALIZE(error)

end program puffin
