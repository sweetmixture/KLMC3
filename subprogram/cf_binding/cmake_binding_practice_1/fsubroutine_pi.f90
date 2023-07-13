subroutine fortran_subprogram_pi( comm, task_id ) bind ( C, name="f_foo" )

  use iso_c_binding
  use mpi

  implicit none

  ! subroutine args 
  integer(c_int), intent(inout) :: comm
  integer(c_int), intent(inout) :: task_id
  double precision :: mpi_time
 
  ! same with main variables
  integer :: rank, size, ierr, fortInt

  if( rank == 0 ) then
    write(*,'(a,I10)') "sizeof(task_id):", sizeof(task_id)
    write(*,'(a,I10)') "sizeof(fortInt):", sizeof(fortInt)
    write(*,'(a,I10)') "task_id : ", task_id
  end if

  call MPI_Comm_rank(comm,rank,ierr)
  call MPI_Comm_size(comm,size,ierr)

  mpi_time = MPI_Wtime()

  if ( rank == 0 ) then
    write(*,'(a,f10.6)')   "F> this is subroutine : ", mpi_time
    write(*,'(a,I4,a,I4)') "F> size: ", size, " rank:", rank

    call sflag( rank, 1)
    ! write(*,'(a,I4)')      "F> task_id: ", task_id
    write(*,'(a)')      "F> task_id: ", task_id
    call sflag( rank, 2)
  end if

  task_id = task_id + 1


contains

  subroutine sflag( rank, flag )

    integer, intent(in) :: rank,flag

    if( rank == 0 ) then
      write(*,'(a,I4)') "F> flag: ", flag
    end if

  end subroutine

end subroutine fortran_subprogram_pi
