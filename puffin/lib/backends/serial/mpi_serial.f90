! Copyright 2012-2018, University of Strathclyde
! Authors: Lawrence T. Campbell
! License: BSD-3-Clause

!> @author
!> Lawrence Campbell,
!> University of Strathclyde,
!> Glasgow, UK
!> @brief
!> Serial replacement for the MPI Fortran module.
!>
!> This file is compiled ONLY when Puffin is configured with
!> -DENABLE_PARALLEL=OFF. It provides a module called `mpi` which exports
!> exactly the subset of the MPI-3 Fortran ("mpi" module) API that Puffin
!> uses, implemented for a communicator of size 1. Every `use mpi, only: ...`
!> in the rest of Puffin then resolves to this module instead of the real
!> one, so no MPI library is needed to build or run, and not a single call
!> site in the physics, setup or IO code has to change.
!>
!> The contract is deliberately narrow: these routines reproduce what real
!> MPI does on a one-rank communicator, and nothing more. Anything that can
!> only be meaningful with more than one rank (a send to a rank other than
!> ourselves, a receive with no matching message already posted) is a
!> programming error here and stops the code with a diagnostic rather than
!> silently returning wrong numbers. That is intentional: a serial build
!> that quietly skipped a genuine communication would produce plausible but
!> wrong physics.
!>
!> Self-communication IS supported, because Puffin genuinely uses it. With
!> periodic field boundaries, rank 0 and rank `size-1` are the same process
!> when size == 1, so the periodic wrap-around in parafield posts an issend
!> to itself and then receives it. Those messages are buffered in a small
!> FIFO below, which is how real MPI copes with the same pattern.
!>
!> Buffer arguments are declared `type(*), dimension(..)` (assumed-type,
!> assumed-rank) so that one implementation serves every type and rank that
!> Puffin passes, mirroring the type-agnostic `<type> buf(*)` of the real
!> Fortran bindings. Payloads are moved as raw bytes, the count being taken
!> from the `count` and `datatype` arguments exactly as MPI defines it.

module mpi

  use, intrinsic :: iso_c_binding, only: c_loc, c_f_pointer, c_associated, c_int8_t
  use, intrinsic :: iso_fortran_env, only: int64, real64, error_unit

  implicit none (type, external)
private

  public :: MPI_ALLGATHER, MPI_ALLGATHERV, MPI_ALLREDUCE, MPI_ALLTOALLV, MPI_BARRIER, MPI_BCAST, &
             MPI_CHARACTER, MPI_COMM_RANK, MPI_COMM_SIZE, MPI_COMM_WORLD, MPI_DOUBLE_PRECISION, &
             MPI_FINALIZE, MPI_IN_PLACE, MPI_INFO_NULL, MPI_INIT, MPI_INIT_THREAD, &
             MPI_Initialized, MPI_INTEGER, MPI_ISSEND, MPI_LOGICAL, MPI_MAX, MPI_MIN, &
             MPI_PROC_NULL, MPI_REAL, MPI_RECV, MPI_REDUCE, MPI_REQUEST_NULL, MPI_SCATTER, &
             MPI_SCATTERV, MPI_SEND, MPI_STATUS_SIZE, MPI_SUM, MPI_THREAD_FUNNELED, &
             MPI_THREAD_MULTIPLE, MPI_THREAD_SERIALIZED, MPI_THREAD_SINGLE, MPI_UNDEFINED, &
             MPI_WAIT, MPI_WAITALL, MPI_Wtime


!     Communicator, and the handles that go with it. The values are
!     arbitrary; nothing outside this module may interpret them.

  integer, parameter :: MPI_COMM_WORLD = 1
  integer, parameter :: MPI_INFO_NULL = 0
  integer, parameter :: MPI_REQUEST_NULL = -1
  integer, parameter :: MPI_PROC_NULL = -2
  integer, parameter :: MPI_UNDEFINED = -32766
  integer, parameter :: MPI_STATUS_SIZE = 6

!     Datatype handles. Each handle is its own extent in bytes, which is
!     all a one-rank implementation ever needs to know about a datatype.
!     They are distinct values so that dt_size can reject an unknown one.

  integer, parameter :: MPI_CHARACTER = 1
  integer, parameter :: MPI_INTEGER = 4
  integer, parameter :: MPI_LOGICAL = 104
  integer, parameter :: MPI_REAL = 204
  integer, parameter :: MPI_DOUBLE_PRECISION = 308

!     Reduction operations. On one rank a reduction is the identity, so
!     these are never inspected; they exist so call sites still compile.

  integer, parameter :: MPI_SUM = 1001
  integer, parameter :: MPI_MAX = 1002
  integer, parameter :: MPI_MIN = 1003

!     Thread support levels, in the order MPI defines them.

  integer, parameter :: MPI_THREAD_SINGLE = 0
  integer, parameter :: MPI_THREAD_FUNNELED = 1
  integer, parameter :: MPI_THREAD_SERIALIZED = 2
  integer, parameter :: MPI_THREAD_MULTIPLE = 3

!     MPI_IN_PLACE is identified by its address, exactly as the real
!     implementations do, so it must be a variable rather than a constant.

  integer, save, target :: MPI_IN_PLACE = -1


!     Buffered messages posted to ourselves, awaiting a matching receive.
!     Puffin's deepest self-exchange is a handful of messages, so a plain
!     growable FIFO is ample.

  type :: pending_msg
    integer :: tag = 0
    integer(kind=c_int8_t), allocatable :: payload(:)
  end type pending_msg

  integer, parameter :: initial_queue_size = 8

  type(pending_msg), allocatable, save :: msg_queue(:)
  integer, save :: n_queued = 0

contains


!> Byte extent of one element of the given datatype handle.

  integer function dt_size(datatype)

    implicit none (type, external)

    integer, intent(in) :: datatype

    select case (datatype)
    case (MPI_CHARACTER)
      dt_size = 1
    case (MPI_INTEGER, MPI_REAL)
      dt_size = 4
    case (MPI_LOGICAL)
      dt_size = 4
    case (MPI_DOUBLE_PRECISION)
      dt_size = 8
    case default
      write (error_unit, '(a,i0)') &
        "serial MPI stub: unknown datatype handle ", datatype
      error stop 1
    end select

  end function dt_size


!> Abort with a diagnostic. Used for the operations that cannot be given a
!> meaning on a single rank; reaching one of these means the caller assumed
!> a genuinely distributed communicator.

  subroutine serial_abort(what)

    implicit none (type, external)

    character(len=*), intent(in) :: what

    write (error_unit, '(a)') "serial MPI stub: " // trim(what)
    write (error_unit, '(a)') &
      "This build has MPI disabled (-DENABLE_PARALLEL=OFF) and runs on one rank."
    error stop 1

  end subroutine serial_abort


!> Copy nbytes bytes from sendbuf (starting soff bytes in) to recvbuf
!> (starting roff bytes in). This is the whole of a one-rank collective:
!> every gather, scatter, reduction and all-to-all reduces to it.

  subroutine copy_bytes(sendbuf, soff, recvbuf, roff, nbytes)

    implicit none (type, external)

    type(*), dimension(..), contiguous, target, intent(in)    :: sendbuf
    type(*), dimension(..), contiguous, target, intent(inout) :: recvbuf
    integer, intent(in) :: soff, roff, nbytes

    integer(kind=c_int8_t), pointer :: sb(:), rb(:)

    if (nbytes <= 0) return

    call c_f_pointer(c_loc(sendbuf), sb, [soff + nbytes])
    call c_f_pointer(c_loc(recvbuf), rb, [roff + nbytes])

    rb(roff+1:roff+nbytes) = sb(soff+1:soff+nbytes)

  end subroutine copy_bytes


!> .true. if buf is MPI_IN_PLACE, tested by address as MPI requires.

  logical function is_in_place(buf)

    implicit none (type, external)

    type(*), dimension(..), contiguous, target, intent(in) :: buf

    is_in_place = c_associated(c_loc(buf), c_loc(MPI_IN_PLACE))

  end function is_in_place


!-----------------------------------------------------------------------
!     Startup, shutdown and process information
!-----------------------------------------------------------------------

  subroutine MPI_INIT(ierror)

    implicit none (type, external)

    integer, intent(out) :: ierror

    ierror = 0

  end subroutine MPI_INIT


  subroutine MPI_INIT_THREAD(required, provided, ierror)

    implicit none (type, external)

    integer, intent(in)  :: required
    integer, intent(out) :: provided
    integer, intent(out) :: ierror

!     A serial run trivially satisfies any threading level asked for: there
!     is only ever one thread making "MPI" calls.

    provided = required
    ierror = 0

  end subroutine MPI_INIT_THREAD


  subroutine MPI_Initialized(flag, ierror)

    implicit none (type, external)

    logical, intent(out) :: flag
    integer, intent(out) :: ierror

    flag = .true.
    ierror = 0

  end subroutine MPI_Initialized


  subroutine MPI_FINALIZE(ierror)

    implicit none (type, external)

    integer, intent(out) :: ierror

    if (allocated(msg_queue)) deallocate(msg_queue)
    n_queued = 0
    ierror = 0

  end subroutine MPI_FINALIZE


  subroutine MPI_COMM_RANK(comm, rank, ierror)

    implicit none (type, external)

    integer, intent(in)  :: comm
    integer, intent(out) :: rank
    integer, intent(out) :: ierror

    rank = 0
    ierror = 0

  end subroutine MPI_COMM_RANK


  subroutine MPI_COMM_SIZE(comm, size, ierror)

    implicit none (type, external)

    integer, intent(in)  :: comm
    integer, intent(out) :: size
    integer, intent(out) :: ierror

    size = 1
    ierror = 0

  end subroutine MPI_COMM_SIZE


!> Wall-clock seconds, matching MPI_Wtime's contract that only differences
!> between two calls are meaningful.

  real(kind=real64) function MPI_Wtime()

    implicit none (type, external)

    integer(kind=int64) :: ticks, rate

    call system_clock(count=ticks, count_rate=rate)

    if (rate > 0_int64) then
      MPI_Wtime = real(ticks, real64) / real(rate, real64)
    else
      MPI_Wtime = 0.0_real64
    end if

  end function MPI_Wtime


  subroutine MPI_BARRIER(comm, ierror)

    implicit none (type, external)

    integer, intent(in)  :: comm
    integer, intent(out) :: ierror

    ierror = 0

  end subroutine MPI_BARRIER


!-----------------------------------------------------------------------
!     Collectives
!-----------------------------------------------------------------------

!> On one rank a reduction returns its own contribution unchanged, so this
!> is a copy - or nothing at all, when the caller reduces in place.

  subroutine MPI_ALLREDUCE(sendbuf, recvbuf, count, datatype, op, comm, ierror)

    implicit none (type, external)

    type(*), dimension(..), contiguous, target, intent(in)    :: sendbuf
    type(*), dimension(..), contiguous, target, intent(inout) :: recvbuf
    integer, intent(in)  :: count, datatype, op, comm
    integer, intent(out) :: ierror

    ierror = 0
    if (is_in_place(sendbuf)) return

    call copy_bytes(sendbuf, 0, recvbuf, 0, count * dt_size(datatype))

  end subroutine MPI_ALLREDUCE


  subroutine MPI_REDUCE(sendbuf, recvbuf, count, datatype, op, root, comm, ierror)

    implicit none (type, external)

    type(*), dimension(..), contiguous, target, intent(in)    :: sendbuf
    type(*), dimension(..), contiguous, target, intent(inout) :: recvbuf
    integer, intent(in)  :: count, datatype, op, root, comm
    integer, intent(out) :: ierror

    ierror = 0
    if (is_in_place(sendbuf)) return

!     The only rank is the root, so the result always lands here.

    call copy_bytes(sendbuf, 0, recvbuf, 0, count * dt_size(datatype))

  end subroutine MPI_REDUCE


!> Broadcast from the only rank to itself: the buffer already holds the
!> value, so there is nothing to move.

  subroutine MPI_BCAST(buffer, count, datatype, root, comm, ierror)

    implicit none (type, external)

    type(*), dimension(..), contiguous, target, intent(inout) :: buffer
    integer, intent(in)  :: count, datatype, root, comm
    integer, intent(out) :: ierror

    ierror = 0

  end subroutine MPI_BCAST


  subroutine MPI_ALLGATHER(sendbuf, sendcount, sendtype, &
                           recvbuf, recvcount, recvtype, comm, ierror)

    implicit none (type, external)

    type(*), dimension(..), contiguous, target, intent(in)    :: sendbuf
    type(*), dimension(..), contiguous, target, intent(inout) :: recvbuf
    integer, intent(in)  :: sendcount, sendtype, recvcount, recvtype, comm
    integer, intent(out) :: ierror

    ierror = 0

    call copy_bytes(sendbuf, 0, recvbuf, 0, sendcount * dt_size(sendtype))

  end subroutine MPI_ALLGATHER


  subroutine MPI_ALLGATHERV(sendbuf, sendcount, sendtype, &
                            recvbuf, recvcounts, displs, recvtype, comm, ierror)

    implicit none (type, external)

    type(*), dimension(..), contiguous, target, intent(in)    :: sendbuf
    type(*), dimension(..), contiguous, target, intent(inout) :: recvbuf
    integer, intent(in)  :: sendcount, sendtype, recvtype, comm
    integer, intent(in)  :: recvcounts(*), displs(*)
    integer, intent(out) :: ierror

    ierror = 0

!     displs is measured in elements of recvtype, as MPI defines it.

    call copy_bytes(sendbuf, 0, recvbuf, displs(1) * dt_size(recvtype), &
                    sendcount * dt_size(sendtype))

  end subroutine MPI_ALLGATHERV


  subroutine MPI_SCATTER(sendbuf, sendcount, sendtype, &
                         recvbuf, recvcount, recvtype, root, comm, ierror)

    implicit none (type, external)

    type(*), dimension(..), contiguous, target, intent(in)    :: sendbuf
    type(*), dimension(..), contiguous, target, intent(inout) :: recvbuf
    integer, intent(in)  :: sendcount, sendtype, recvcount, recvtype, root, comm
    integer, intent(out) :: ierror

    ierror = 0

    call copy_bytes(sendbuf, 0, recvbuf, 0, recvcount * dt_size(recvtype))

  end subroutine MPI_SCATTER


  subroutine MPI_SCATTERV(sendbuf, sendcounts, displs, sendtype, &
                          recvbuf, recvcount, recvtype, root, comm, ierror)

    implicit none (type, external)

    type(*), dimension(..), contiguous, target, intent(in)    :: sendbuf
    type(*), dimension(..), contiguous, target, intent(inout) :: recvbuf
    integer, intent(in)  :: sendtype, recvcount, recvtype, root, comm
    integer, intent(in)  :: sendcounts(*), displs(*)
    integer, intent(out) :: ierror

    ierror = 0

    call copy_bytes(sendbuf, displs(1) * dt_size(sendtype), recvbuf, 0, &
                    recvcount * dt_size(recvtype))

  end subroutine MPI_SCATTERV


  subroutine MPI_ALLTOALLV(sendbuf, sendcounts, sdispls, sendtype, &
                           recvbuf, recvcounts, rdispls, recvtype, comm, ierror)

    implicit none (type, external)

    type(*), dimension(..), contiguous, target, intent(in)    :: sendbuf
    type(*), dimension(..), contiguous, target, intent(inout) :: recvbuf
    integer, intent(in)  :: sendtype, recvtype, comm
    integer, intent(in)  :: sendcounts(*), sdispls(*), recvcounts(*), rdispls(*)
    integer, intent(out) :: ierror

    ierror = 0

!     The only exchange is the block we send to ourselves.

    call copy_bytes(sendbuf, sdispls(1) * dt_size(sendtype), &
                    recvbuf, rdispls(1) * dt_size(recvtype), &
                    sendcounts(1) * dt_size(sendtype))

  end subroutine MPI_ALLTOALLV


!-----------------------------------------------------------------------
!     Point to point
!
!     Only self-communication is possible here. Puffin uses it for the
!     periodic field wrap-around, where rank 0 and rank size-1 coincide.
!     Sends buffer their payload; receives consume the oldest buffered
!     message carrying a matching tag.
!-----------------------------------------------------------------------

!> Buffer a message destined for ourselves.

  subroutine enqueue(buf, count, datatype, dest, tag)

    implicit none (type, external)

    type(*), dimension(..), contiguous, target, intent(in) :: buf
    integer, intent(in) :: count, datatype, dest, tag

    type(pending_msg), allocatable :: grown(:)
    integer :: nbytes

    if (dest == MPI_PROC_NULL) return

    if (dest /= 0) call serial_abort( &
      "a message was sent to a rank other than 0, which does not exist.")

    if (.not. allocated(msg_queue)) then
      allocate(msg_queue(initial_queue_size))
    else if (n_queued == size(msg_queue)) then
      allocate(grown(2 * size(msg_queue)))
      grown(1:n_queued) = msg_queue(1:n_queued)
      call move_alloc(grown, msg_queue)
    end if

    nbytes = count * dt_size(datatype)

    n_queued = n_queued + 1
    msg_queue(n_queued)%tag = tag
    allocate(msg_queue(n_queued)%payload(max(nbytes, 0)))

    if (nbytes > 0) call copy_bytes(buf, 0, msg_queue(n_queued)%payload, 0, nbytes)

  end subroutine enqueue


  subroutine MPI_ISSEND(buf, count, datatype, dest, tag, comm, request, ierror)

    implicit none (type, external)

    type(*), dimension(..), contiguous, target, intent(in) :: buf
    integer, intent(in)  :: count, datatype, dest, tag, comm
    integer, intent(out) :: request
    integer, intent(out) :: ierror

    call enqueue(buf, count, datatype, dest, tag)

!     Buffering completes the send immediately, so the request is already
!     satisfied by the time it is handed back and MPI_WAIT has nothing to do.

    request = MPI_REQUEST_NULL
    ierror = 0

  end subroutine MPI_ISSEND


  subroutine MPI_SEND(buf, count, datatype, dest, tag, comm, ierror)

    implicit none (type, external)

    type(*), dimension(..), contiguous, target, intent(in) :: buf
    integer, intent(in)  :: count, datatype, dest, tag, comm
    integer, intent(out) :: ierror

    call enqueue(buf, count, datatype, dest, tag)
    ierror = 0

  end subroutine MPI_SEND


  subroutine MPI_RECV(buf, count, datatype, source, tag, comm, status, ierror)

    implicit none (type, external)

    type(*), dimension(..), contiguous, target, intent(inout) :: buf
    integer, intent(in)  :: count, datatype, source, tag, comm
    integer, intent(out) :: status(MPI_STATUS_SIZE)
    integer, intent(out) :: ierror

    integer :: i, match, nbytes

    status = 0
    ierror = 0

    if (source == MPI_PROC_NULL) return

    if (source /= 0) call serial_abort( &
      "a message was expected from a rank other than 0, which does not exist.")

    match = 0
    do i = 1, n_queued
      if (msg_queue(i)%tag == tag) then
        match = i
        exit
      end if
    end do

    if (match == 0) call serial_abort( &
      "a receive was posted with no matching message already sent. " // &
      "In a real MPI run this would have been satisfied by another rank.")

    nbytes = min(count * dt_size(datatype), size(msg_queue(match)%payload))

    if (nbytes > 0) call copy_bytes(msg_queue(match)%payload, 0, buf, 0, nbytes)

!     Consume the message, preserving the order of the ones behind it.

    deallocate(msg_queue(match)%payload)
    do i = match, n_queued - 1
      msg_queue(i) = msg_queue(i+1)
    end do
    n_queued = n_queued - 1

  end subroutine MPI_RECV


!> Sends complete when posted here, so waiting is a no-op.

  subroutine MPI_WAIT(request, status, ierror)

    implicit none (type, external)

    integer, intent(inout) :: request
    integer, intent(out)   :: status(MPI_STATUS_SIZE)
    integer, intent(out)   :: ierror

    request = MPI_REQUEST_NULL
    status = 0
    ierror = 0

  end subroutine MPI_WAIT


  subroutine MPI_WAITALL(count, requests, statuses, ierror)

    implicit none (type, external)

    integer, intent(in)    :: count
    integer, intent(inout) :: requests(*)
    integer, intent(out)   :: statuses(MPI_STATUS_SIZE, *)
    integer, intent(out)   :: ierror

    integer :: i

    do i = 1, count
      requests(i) = MPI_REQUEST_NULL
      statuses(:, i) = 0
    end do

    ierror = 0

  end subroutine MPI_WAITALL

end module mpi
