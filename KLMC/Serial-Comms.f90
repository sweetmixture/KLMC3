MODULE Comms
  IMPLICIT none
  PRIVATE
  
  INTEGER,SAVE,PUBLIC      :: nprocs
  INTEGER,SAVE,PUBLIC      :: nprocs_in_farm
  INTEGER,SAVE,PUBLIC      :: my_id
  INTEGER,SAVE,PUBLIC      :: my_new_id
  INTEGER,SAVE,PUBLIC      :: farm_id
  INTEGER,SAVE,PUBLIC      :: comm_handle
  INTEGER,SAVE,PUBLIC      :: my_rank
  INTEGER,SAVE,PUBLIC      :: my_new_rank
  LOGICAL,SAVE,PUBLIC      :: ROOT 
  INTEGER,SAVE,PUBLIC      :: comm_group
  INTEGER,SAVE,PUBLIC      :: MY_COMM,COMM_FARM
  INTEGER,SAVE,PUBLIC      :: NUM_FARMS
  LOGICAL,SAVE,PUBLIC      :: master_alone=.FALSE.

  PUBLIC comms_initialize
  PUBLIC comms_finalize
  PUBLIC comms_bcast_integer
  PUBLIC comms_bcast_real
  PUBLIC comms_bcast_character
  PUBLIC comms_bcast_logical
  PUBLIC comms_copy_to_root
  PUBLIC comms_copy_to_master
! PUBLIC comms_scatter
  PUBLIC comms_create_farm

  PUBLIC MPI_BARRIER

! INTERFACE comms_bcast
!    MODULE PROCEDURE comms_bcast_integer
!    MODULE PROCEDURE comms_bcast_real
!    MODULE PROCEDURE comms_bcast_character
!    MODULE PROCEDURE comms_bcast_logical
! END INTERFACE

  CONTAINS

! subroutines:
! comms_initialize comms_create_farm comms_finalize
! comms_bcast_integer comms_bcast_real comms_bcast_character comms_bcast_logical
! comms_copy_to_root comms_copy_to_master

!================================================================================!
!                              INITIALISE                                        !
!================================================================================!   
  SUBROUTINE comms_initialize
    USE Config
    IMPLICIT none

    MY_COMM = 1
    MASTER_PROC = .TRUE.
    nprocs = 1
    NUM_PROCS_IN_FARM = 1
    RETURN
  END SUBROUTINE comms_initialize
!================================================================================!
!                              TASK FARM                                         !
!================================================================================!   
  SUBROUTINE comms_create_farm
    IMPLICIT NONE

    RETURN
  END SUBROUTINE comms_create_farm
!================================================================================!
!                              BOARDCAST INTEGER                                 !
!================================================================================!   
  SUBROUTINE comms_bcast_integer(array,length)
    IMPLICIT none    
    INTEGER, INTENT(IN) :: array, length
   
    RETURN
  END SUBROUTINE comms_bcast_integer
!================================================================================!
!                              BOARDCAST REAL                                    !
!================================================================================!   
  SUBROUTINE comms_bcast_real(array,length,IN_COMM)
    IMPLICIT none
    INTEGER, INTENT(IN) :: array, length, IN_COMM

    RETURN
  END SUBROUTINE comms_bcast_real
!================================================================================!
!                              BOARDCAST CHARACTER                               !
!================================================================================!   
  SUBROUTINE comms_bcast_character(array,length)
    IMPLICIT none
    INTEGER, INTENT(IN) :: array, length
  
    RETURN
  END SUBROUTINE comms_bcast_character
!================================================================================!
!                              BOARDCAST LOGICAL                                 !
!================================================================================!   
  SUBROUTINE comms_bcast_logical(array,length,IN_COMM)
    IMPLICIT none
    INTEGER, INTENT(IN) :: array, length, IN_COMM

    RETURN
  END SUBROUTINE comms_bcast_logical
!================================================================================!
!                              COPY TO ROOT                                      !
!================================================================================!   
  SUBROUTINE comms_copy_to_root(in_pop)
    USE Config
    IMPLICIT none
    TYPE(cluster), INTENT(IN), DIMENSION(:) :: in_pop

    RETURN
  END SUBROUTINE comms_copy_to_root
!================================================================================!
!                              COPY TO MASTER_PROC                                    !
!================================================================================!   
  SUBROUTINE comms_copy_to_master(in_pop)
    USE Config
    IMPLICIT none
    TYPE(cluster), INTENT(IN), DIMENSION(:) :: in_pop

    RETURN
  END SUBROUTINE comms_copy_to_master
!================================================================================!
!                              fINALISE                                          !
!================================================================================!   
  SUBROUTINE comms_finalize
    IMPLICIT none

    RETURN
  END SUBROUTINE comms_finalize
!================================================================================!
!                              MPI BARRIER                                       !
!================================================================================!   
  SUBROUTINE MPI_BARRIER(MY_COMM,in_error)
    IMPLICIT none
    INTEGER :: MY_COMM, in_error

    RETURN
  END SUBROUTINE MPI_BARRIER
!================================================================================!
END MODULE Comms
