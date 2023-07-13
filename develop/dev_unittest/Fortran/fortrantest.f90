program test

  implicit none

  integer, save :: stdout = 36

  ! character(len=32) :: stdout = "stdout"

  write(stdout,'(A)') "banana is fruit"
  call flush(stdout)

end program
