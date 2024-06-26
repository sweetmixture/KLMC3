!
!       Author:         Woongkyu Jee / woong.jee.16@ucl.ac.uk
!       Affiliation:    University College London
!       Date:           2023.05.25 - 
!
!       Description:
!
!

subroutine gulpklmc( MPI_comm_klmc, klmc_task_iopath, klmc_task_id, klmc_worker_id ) bind (C,name="gulpklmc")
  !
  ! wkjee - KLMC development 07/2023
  !
  use datatypes,  only : i4
  use parallel,   only : ioproc
  use klmc
  use iso_c_binding
  use mpi
  implicit none
  ! able to use 'gulp_klmc_iopath' module from modules.F90 (added)


  ! MPI_comm_klmc				: communicator passed by KLMC taskfarm
  ! klmc_task_iopath			: path to read/write gulpt input/output(standard *gout)
  ! klmc_task_id				: task_id
  ! klmc_worker_id				: taskfarm worker id

  integer*4,                     intent(in) :: MPI_comm_klmc
  integer,                       intent(in) :: klmc_task_id
  integer,                       intent(in) :: klmc_worker_id

  ! this works on Archer2 Cray ftn
  ! character(kind=c_char,len=512), intent(in) :: klmc_task_iopath

  ! wkjee - refactoring for intel extension - 21/07/23
  character(kind=c_char), dimension(*), intent(in) :: klmc_task_iopath

  character(len=512) klmc_task_iopath_dummy
  
  ! iopath_length - string length of the file path
  integer iopath_length
  integer ierr
  integer rank
  integer cpu_count

  integer(i4)        :: iret
  integer(i4)        :: klmc_link
  integer*4          :: MPI_comm_togulp
  
  ! timing
  character(len=40) :: start_timestamp, end_timestamp
  double precision  :: mpi_tstart, mpi_tend, mpi_elapsed_t

  ! wkjee - do not touch this block . related to gulp default ============
  iret = 0
  klmc_link = 0
  MPI_comm_togulp = MPI_comm_klmc
  ! end wkjee ============================================================

  call MPI_Comm_rank(MPI_comm_togulp,rank,ierr)
  call MPI_Comm_size(MPI_comm_togulp,cpu_count,ierr)

  ! wkjee - refactoring for intel compiler extension - 21/07/23
  !       - important: iopath_length - remove the terminating zero '0' at the end 
  !       - length error c string / fortran string size must be in match, 07/23
  !       - intel extension 07/23
  klmc_task_iopath_dummy = ""
  iopath_length = 0
  do 
    if(klmc_task_iopath(iopath_length+1) == C_NULL_CHAR) exit
    iopath_length = iopath_length + 1
    klmc_task_iopath_dummy(iopath_length:iopath_length) = klmc_task_iopath(iopath_length)
  end do

  gulp_klmc_iopath = klmc_task_iopath_dummy(1:iopath_length)

  ! passing taskfarm info
  gulp_klmc_task_id = klmc_task_id
  gulp_klmc_worker_id = klmc_worker_id

  ! wallclock time measure
  mpi_tstart = MPI_Wtime()
  start_timestamp = fgetCurrentDateTime()

!  if ( rank .eq. 0 ) then
!    write(*,'(A)')    "==========================================================="
!    write(*,'(A,I4)') " (D) in gulpklmc: task_id                : ", klmc_task_id
!    write(*,'(A,I4)') " (D) in gulpklmc: klmc_task_iopath_length: ", iopath_length
!    write(*,'(A,A)')  " (D) in gulpklmc: KLMC klmc_task_iopath  : ", klmc_task_iopath_dummy
!    write(*,'(A,A)')  " (D) in gulpklmc: KLMC gulp_klmc_iopath  : ", gulp_klmc_iopath 
!    write(*,'(A,A)')  " (D) in gulpklmc: start_timestamp        : ", trim(adjustl(start_timestamp))
!  end if

!======================
! Launch GULP-KLMC 
!======================

  call gulpklmc_initmax

  call gulpmain(iret, klmc_link, MPI_comm_togulp)

!======================
! Finish GULP-KLMC run 
!======================

  call gulpfinish

  ! 06/2024
  ! memory debugging
  ! call gulpklmc_deallocate 
  call gulpklmc_deallocate_all

!=====================
! Print out log
!=====================
  mpi_tend = MPI_Wtime()
  end_timestamp = fgetCurrentDateTime()
  mpi_elapsed_t = mpi_tend - mpi_tstart

!  if ( rank .eq. 0 ) then
!    write(*,'(A,A,A,A,A,F24.6,A,I4,A,I4,A,I4,A,A)') &
!    "KLMC_FINALIZE(gulpklmc)> start: ", &
!    trim(adjustl(start_timestamp)), &
!    " end: ", trim(adjustl(end_timestamp)), &
!    " elapsed_time: ", mpi_elapsed_t, &
!    " cpu_count: ", cpu_count, &
!    " task_id: ", klmc_task_id, &
!    " worker_id: ", klmc_worker_id, &
!    " io_path: ", gulp_klmc_iopath
!  end if

  return

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
  
!    subroutine sflag( rank, flag, task_id )
!      integer, intent(in) :: rank,flag,task_id
!      if( rank == 0 ) then
!        write(*,'(a,I4,I4)') "F> flag task_id: ", flag, task_id
!      end if
!    end subroutine

end subroutine gulpklmc
