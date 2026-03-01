program puffin

    use puffin_mod, only: puffin_main
    use puffin_kinds, only: ip
    use IO
    use MPI

    character(1024_IP) :: input_file_name
    integer(KIND=IP)    :: error, provided
    logical :: qOK

!     Read in input file name
!     (input on command line as variable at runtime)

    call getarg(1,input_file_name)
    call MPI_INIT_THREAD(MPI_THREAD_FUNNELED, provided, error)
    call puffin_main(input_file_name, qOK)
    call MPI_FINALIZE(error)
    if (.not. qOK) then
      print*, 'Puffin simulation failed, check error log for details, ', tErrorLog_G%zFileName
    else
      print*, 'Puffin simulation completed successfully.'
    end if

end program puffin
