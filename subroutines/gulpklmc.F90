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
  ! able to use 'gulp_klmc_iopath' module from modules.F90 (added)
  ! include 'mpif.h'
  ! able to use 'gulp_klmc_iopath' module from modules.F90 (added)
  implicit none


  ! MPI_comm_klmc				: communicator passed by KLMC taskfarm
  ! klmc_task_iopath				: path to read/write gulpt input/output(standard *gout)
  ! klmc_task_id				: task_id
  ! klmc_worker_id				: taskfarm worker id

  integer*4,          intent(in) :: MPI_comm_klmc
  integer,            intent(in) :: klmc_task_id
  integer,            intent(in) :: klmc_worker_id
  character(len=512), intent(in) :: klmc_task_iopath
  
  ! iopath_length - string length of the file path
  integer iopath_length
  integer ierr
  integer rank
  integer cpu_count

  integer(i4)        :: iret
  integer(i4)        :: klmc_link
  ! klmc_link .eq. ichemsh_link
  ! integer(i4)      :: ichemsh_link
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

  ! wkjee - setup iopath (possibly a message sent from the taskfarm <master>)
  ! from module klmc in modules.F90 (GULP main source), attribute<save>

  ! passing the working directory - the directory must include 'gulp_klmc.gin' (standard gulp input file)
  ! klmc_task_iopath = "/work/e05/e05/wkjee/Software/gulp-6.1.2/Src/Custom/path_test"
  ! gulp_klmc_iopath = ""
  ! gulp_klmc_iopath = klmc_task_iopath(1:60)
  ! gulp_klmc_iopath = klmc_task_iopath
  ! gulp_klmc_iopath = ""

  ! wkjee - solid working without bulshit ending character
  ! <IMPORTANT> iopath_length - remove the terminating zero '0' at the end 
  iopath_length = len_trim(klmc_task_iopath)-1
  gulp_klmc_iopath = klmc_task_iopath(1:iopath_length)
  ! length error - wkjee 10 July 2023 Mon - c string / fortran string size must be in match

  ! passing taskfarm info
  gulp_klmc_task_id = klmc_task_id
  gulp_klmc_worker_id = klmc_worker_id

  ! wallclock time measure
  mpi_tstart = MPI_Wtime()
  start_timestamp = fgetCurrentDateTime()

  if ( rank .eq. 0 ) then
    ! klmc_task_iopath : inlcude the ending "\0" character from 'C'
    write(*,'(A)')    "========================================================================"
    write(*,'(A,I4)') " (D) in gulpklmc: task_id                : ", klmc_task_id
    write(*,'(A,I4)') " (D) in gulpklmc: klmc_task_iopath_length: ", len_trim(klmc_task_iopath)-1
    write(*,'(A,A)')  " (D) in gulpklmc: KLMC klmc_task_iopath  : ", klmc_task_iopath
    write(*,'(A,A)')  " (D) in gulpklmc: KLMC gulp_klmc_iopath  : ", gulp_klmc_iopath 
    write(*,'(A,A)')  " (D) in gulpklmc: start_timestamp        : ", trim(adjustl(start_timestamp))
  end if

!======================
! Launch GULP-KLMC 
!======================

  ! wkjee tell this fresh run - reset gulp internal configuration parameters
  lklmcfreshrun = .true.

  call gulpmain(iret, klmc_link, MPI_comm_togulp)

!======================
! Finish GULP-KLMC run 
!======================

  call gulpfinish

!=====================
! Print out log
!=====================
  mpi_tend = MPI_Wtime()
  end_timestamp = fgetCurrentDateTime()
  mpi_elapsed_t = mpi_tend - mpi_tstart

  if ( rank .eq. 0 ) then
    write(*,'(A,A,A,A,A,F12.8,A,I4,A,I4,A,I4,A,A)') "KLMC_FINALIZE(gulpklmc)> start: ", trim(adjustl(start_timestamp)), " end: ", trim(adjustl(end_timestamp)), " elapsed_time: ", mpi_elapsed_t, &
    " cpu_count: ", cpu_count, " task_id: ", klmc_task_id, " worker_id: ", klmc_worker_id, " io_path: ", gulp_klmc_iopath
  end if

  ! wkjee - experiment
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
  
    subroutine sflag( rank, flag, task_id )
      integer, intent(in) :: rank,flag,task_id
      if( rank == 0 ) then
        write(*,'(a,I4,I4)') "F> flag task_id: ", flag, task_id
      end if
    end subroutine

end subroutine gulpklmc
