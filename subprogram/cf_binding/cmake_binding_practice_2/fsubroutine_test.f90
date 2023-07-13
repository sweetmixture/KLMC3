subroutine fortran_subprogram_pi( comm, task_id ) bind ( C, name="f_foo" )

  use iso_c_binding
  use mpi

  implicit none

  ! subroutine args 
  integer(c_int), intent(inout) :: comm, task_id
  double precision :: mpi_time
 
  ! same with main variables
  integer :: rank, size, ierr

  call MPI_Comm_rank(comm,rank,ierr)
  call MPI_Comm_size(comm,size,ierr)

  mpi_time = MPI_Wtime()

  if ( rank == 0 ) then
    write(*,'(a,I10)') "F> comm : ", comm
    write(*,'(a,f20.10)') "F> this is subroutine : ", mpi_time
    write(*,'(a,I4,a,I4)') "F> size: ", size, " rank:", rank
    write(*,'(a,I4)') "F> task_id: ", task_id
  end if

  task_id = task_id + 1

end subroutine fortran_subprogram_pi
