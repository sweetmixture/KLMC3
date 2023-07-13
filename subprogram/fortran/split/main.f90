program main

  use mpi

  implicit none

  integer :: size, rank, ierr, task_id
  integer :: BaseComm

  ! character(len=MPI_MAX_OBJECT_NAME) :: name
  ! integer :: name_length

  BaseComm = MPI_COMM_WORLD
  write(*,*) BaseComm

  call MPI_Init(ierr)
  call MPI_Comm_size(BaseComm,size,ierr)
  call MPI_Comm_rank(BaseComm,rank,ierr)

  ! write(*,'("rank / size :", I2, a3, I2)') rank, " / ", size

  ! call MPI_Comm_get_name(BaseComm,name,name_length)
  ! write(*,'(A20,I10)') name,name_length

  task_id = 15

  call fortran_subprogram_pi(BaseComm,task_id)


  call MPI_Finalize(ierr)

end program
