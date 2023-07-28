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

  integer*4,                     intent(in) :: MPI_comm_klmc
  integer,                       intent(in) :: klmc_task_id
  integer,                       intent(in) :: klmc_worker_id
  ! character(kind=c_char),        intent(in) :: klmc_task_iopath(512)

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

  ! wkjee - refactoring for intel compiler extension - 21/07/23
  klmc_task_iopath_dummy = ""
  iopath_length = 0
  do 
    if(klmc_task_iopath(iopath_length+1) == C_NULL_CHAR) exit
    iopath_length = iopath_length + 1
    klmc_task_iopath_dummy(iopath_length:iopath_length) = klmc_task_iopath(iopath_length)
  end do

  ! refactoring intel extension - 21/07/23
  ! iopath_length = len_trim(klmc_task_iopath)-1
  gulp_klmc_iopath = klmc_task_iopath_dummy(1:iopath_length)
  ! length error - wkjee 10 July 2023 Mon - c string / fortran string size must be in match
  ! refactoring intel extension - 21/07/23

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
    ! refactoring intel extension - 21/07/23
    ! write(*,'(A,I4)') " (D) in gulpklmc: klmc_task_iopath_length: ", len_trim(klmc_task_iopath)-1
    ! write(*,'(A,A)')  " (D) in gulpklmc: KLMC klmc_task_iopath  : ", klmc_task_iopath
    write(*,'(A,I4)') " (D) in gulpklmc: klmc_task_iopath_length: ", iopath_length
    write(*,'(A,A)')  " (D) in gulpklmc: KLMC klmc_task_iopath  : ", klmc_task_iopath_dummy
    write(*,'(A,A)')  " (D) in gulpklmc: KLMC gulp_klmc_iopath  : ", gulp_klmc_iopath 
    write(*,'(A,A)')  " (D) in gulpklmc: start_timestamp        : ", trim(adjustl(start_timestamp))
  end if

!======================
! Launch GULP-KLMC 
!======================

  ! wkjee tell this fresh run - reset gulp internal configuration parameters

! lklmcfreshrun = .true.
! lklmc_maxat            = .true.
! lklmc_maxatloc         = .true.
! lklmc_maxatot          = .true.
! lklmc_maxbond          = .true.
! lklmc_maxbondq         = .true.
! lklmc_maxccspec        = .true.
! lklmc_maxcfg           = .true.
! lklmc_maxconnect       = .true.
! lklmc_maxdef           = .true.
! lklmc_maxeamden        = .true.
! lklmc_maxeamfnspec     = .true.
! lklmc_maxeamspec       = .true.
! lklmc_maxedipspec      = .true.
! lklmc_maxfgrad         = .true.
! lklmc_maxfit           = .true.
! lklmc_maxfor           = .true.
! lklmc_maxfstrain       = .true.
! lklmc_maxgcmcmol       = .true.
! lklmc_maxlambda        = .true.
! lklmc_maxlib           = .true.
! lklmc_maxmcswaps       = .true.
! lklmc_maxmcswapspec    = .true.
! lklmc_maxmctrans       = .true.
! lklmc_maxmol           = .true.
! lklmc_maxnboa          = .true.
! lklmc_maxnboo          = .true.
! lklmc_maxnbopot        = .true.
! lklmc_maxnboq0         = .true.
! lklmc_maxnboq          = .true.
! lklmc_maxnbor          = .true.
! lklmc_maxnboz          = .true.
! lklmc_maxnebreplicatot = .true.
! lklmc_maxnppa          = .true.
! lklmc_maxnpts          = .true.
! lklmc_maxobs           = .true.
! lklmc_maxone           = .true.
! lklmc_maxplanepot      = .true.
! lklmc_maxpot           = .true.
! lklmc_maxqrange        = .true.
! lklmc_maxr1at          = .true.
! lklmc_maxreaxffspec    = .true.
! lklmc_maxreaxffval3    = .true.
! lklmc_maxregion        = .true.
! lklmc_maxsix           = .true.
! lklmc_maxspcellbo      = .true.
! lklmc_maxspcell        = .true.
! lklmc_maxspec          = .true.
! lklmc_maxtdfield       = .true.
! lklmc_maxtempramp      = .true.
! lklmc_maxthb           = .true.
! lklmc_maxtitle         = .true.
! lklmc_maxneighk        = .true.
! lklmc_maxpdfcfg        = .true.

  call gulpklmc_initmax

  call gulpmain(iret, klmc_link, MPI_comm_togulp)

!======================
! Finish GULP-KLMC run 
!======================

  call gulpfinish

  ! 25/07 added wkjee tmp
  ! call reinitialise

!=====================
! Print out log
!=====================
  mpi_tend = MPI_Wtime()
  end_timestamp = fgetCurrentDateTime()
  mpi_elapsed_t = mpi_tend - mpi_tstart

  if ( rank .eq. 0 ) then
    write(*,'(A,A,A,A,A,F16.8,A,I4,A,I4,A,I4,A,A)') "KLMC_FINALIZE(gulpklmc)> start: ", trim(adjustl(start_timestamp)), " end: ", trim(adjustl(end_timestamp)), " elapsed_time: ", mpi_elapsed_t, &
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
