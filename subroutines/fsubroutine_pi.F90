!
!        Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
!        Affiliation:    University College London
!        Date:           2023.05.25 - 
!
!        Description:
!


subroutine fortran_subprogram_pi( comm, task_id ) bind(C,name="fortran_subprogram_pi")

  use iso_c_binding 
  use mpi

  implicit none

  ! subroutine args 
  integer(c_int), intent(inout) :: comm
  integer(c_int), intent(in) :: task_id
 
  ! same with main variables
  integer :: rank, size, ierr
  integer, parameter :: root = 0
  integer, parameter :: iter_max  = 16
  integer, parameter :: N = 100000000

  integer :: i, iter

  integer :: N_local, N_inside = 0, N_total_inside
  double precision :: x, y, pi_local, pi_total, t_start, t_end, pi_res
  character(len=40) :: start_timestamp, end_timestamp, filename,temp
  double precision :: mpi_tstart, mpi_tend, mpi_elapsed_t
  character(len=100) :: string

  double precision, allocatable :: res(:)

  allocate(res(iter_max))

  call MPI_Comm_rank(comm, rank, ierr)
  call MPI_Comm_size(comm, size, ierr)

  if( rank == 0 ) then
    write(*,'(a,I4,I14)') "FORTRAN> check task_id,comm: ", task_id, comm
  end if 

  call random_seed()

  mpi_tstart = MPI_Wtime()
  start_timestamp = fgetCurrentDateTime()

  ! filename set
  write(temp,*) task_id
  write(filename,'(a)') adjustl("task_id_"//trim(adjustl(temp)))

  ! call sflag(rank,1,task_id)

  ! file open
  if ( rank == 0 ) then
    open(unit=1,file=filename,status='replace')
  end if 

  N_local = N / size

  ! loop
  do iter = 1, iter_max

    do i = 1, N_local

      call random_number(x)
      call random_number(y)

      if ( sqrt(x*x + y*y) <= 1.0 ) then
        N_inside = N_inside + 1
      end if

    end do

    call MPI_Reduce(N_inside,N_total_inside,1,MPI_INT,MPI_SUM,root,comm,ierr)
    pi_local = 4.0 * real(N_inside) / real(N_local)
    call MPI_Reduce(pi_local,pi_total,1,MPI_DOUBLE_PRECISION,MPI_SUM,root,comm,ierr)

    res(iter) = pi_total
    N_inside = 0

  end do

  pi_res = 0
  do iter = 1, iter_max
    pi_res = pi_res + res(iter)
  end do
  pi_total = pi_res / real(iter_max)

  ! final record
  pi_res = pi_total/real(size)
  ! write(*,'(f8.6)') pi_total/real(size)
  mpi_tend = MPI_Wtime()
  end_timestamp = fgetCurrentDateTime()
  mpi_elapsed_t = mpi_tend - mpi_tstart

  ! file close
  if ( rank == 0 ) then
    write(1,'(a, a, a, a, a, I4, a, I4 )',advance="no") "start: ", &
        trim(adjustl(start_timestamp)), " end: ", trim(adjustl(end_timestamp)), &
        " task_id: ", task_id, " n_tasks: ", size 
    ! write(1,'(a, a, a, a )') "start: ", start_timestamp, " end: ", end_timestamp
    write(1,'(" result: ", f12.10, " elapsed_time: ", f12.8)') pi_res, mpi_elapsed_t
    close(1)
  end if

  deallocate(res)

  ! practically the main subroutine is done
  ! ---------------------------------------------------------------------

contains

  function fgetCurrentDateTime() result(dateTimeStr)
    character(len=40) :: dateTimeStr
    integer :: ierr
    character(len=19) :: timeStr
    character(len=21) :: dateStr

    call date_and_time(date=dateStr, time=timeStr)
    dateTimeStr = trim(dateStr) // ' ' // trim(timeStr)

    return
  end function fgetCurrentDateTime

  subroutine sflag( rank, flag, task_id )
    integer, intent(in) :: rank,flag,task_id
    if( rank == 0 ) then
      write(*,'(a,I4,I4)') "F> flag task_id: ", flag, task_id
    end if
  end subroutine

end subroutine fortran_subprogram_pi
