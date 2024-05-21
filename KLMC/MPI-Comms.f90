! -*- mode: F90 ; mode: font-lock ; column-number-mode: true               -*-!
!=============================================================================!
!                           Comms.f90                                         !
!=============================================================================!
!=============================================================================!
! Written by Matthew Farrow      04/09/09                                     !
! Modified   10/10/11                                                         !
!=============================================================================!

MODULE comms
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
   LOGICAL,save,PUBLIC      :: MASTER_PROC
   INTEGER,SAVE,PUBLIC      :: comm_group
   INTEGER,SAVE,PUBLIC      :: MY_COMM,COMM_FARM
   INTEGER,SAVE,PUBLIC      :: NUM_FARMS
   LOGICAL,SAVE,PUBLIC      :: master_alone=.FALSE.


   PUBLIC comms_initialize
   PUBLIC comms_finalize
   PUBLIC comms_bcast
   PUBLIC comms_copy_to_root
   PUBLIC comms_copy_to_master
 ! PUBLIC comms_scatter
   PUBLIC comms_create_farm

   INCLUDE "mpif.h"

   INTERFACE comms_bcast
     MODULE PROCEDURE comms_bcast_integer
     MODULE PROCEDURE comms_bcast_real
     MODULE PROCEDURE comms_bcast_character
     MODULE PROCEDURE comms_bcast_logical
     !MODULE PROCEDURE comms_bcast_complex
  END INTERFACE

CONTAINS

  SUBROUTINE comms_initialize
    IMPLICIT none
    INTEGER :: ierr,i,j

    call MPI_Init(ierr)
    IF (ierr /= MPI_success) STOP 'Error in MPI_init'
    
    !Duplicate MY_COMM - good programming practice
    CALL MPI_COMM_DUP(MPI_COMM_WORLD,MY_COMM,ierr)
    IF (ierr/=MPI_SUCCESS) STOP 'Error in duplicating MY_COMM'
    !Get total number of processors within MPI world
    CALL MPI_comm_size(MY_COMM,nprocs,ierr)
    !Get rank on each different processor within MPI world (0 to nprocs-1)
    CALL MPI_comm_rank(MY_COMM,my_id,ierr)
    
    my_rank = my_id + 1
    IF (my_rank == 1) THEN
       MASTER_PROC= .TRUE.
    ELSE
       MASTER_PROC= .FALSE.
    END IF
    RETURN
  END SUBROUTINE comms_initialize
!==========================================================================================!
!                              TASK FARM                                                   !
!==========================================================================================!   
  SUBROUTINE comms_create_farm
    USE Config, only : NUM_PROCS_IN_FARM
    IMPLICIT NONE
    INTEGER   :: ierr,i,j

    !Check to make sure number of processors chosen divides nicely into number of task farms wanted
    IF (NUM_PROCS_IN_FARM == 0) THEN
       WRITE(*,*) 'Requested zero procs in farm!' 
       WRITE(*,*) 'Enabled default of two processors per farm'
       NUM_PROCS_IN_FARM = 1
    END IF

    IF ( MOD(nprocs,NUM_PROCS_IN_FARM) /=0 ) THEN
       !See if can have one proc for MASTER_PROCand divide remaining
       IF (MOD( (nprocs-1),NUM_PROCS_IN_FARM) /=0 ) THEN
          IF(MASTER) THEN
             WRITE(*,*) 'Must choose processor number a multiple of NUM_PROCS_IN_FARM'
             WRITE(*,*) 'Num processors:',nprocs,'NUM_PROCS_IN_FARM:',NUM_PROCS_IN_FARM
             CALL MPI_FINALIZE(ierr)
             CALL MPI_ABORT(my_comm,1,ierr)
          END IF
       ELSE
          master_alone = .TRUE.
          NUM_FARMS =  1 +  ((nprocs-1)/NUM_PROCS_IN_FARM)
       END IF
    ELSE
       NUM_FARMS = nprocs/NUM_PROCS_IN_FARM
    END IF

    IF (MASTER) THEN
       WRITE(*,*) 'There are',NUM_FARMS,'farms'
       IF(master_alone) WRITE(*,*) 'Master is alone'
    END IF

    IF (master_alone) THEN 
       IF ( MOD(my_rank,NUM_PROCS_IN_FARM) /= 0 ) THEN
          
          IF (MASTER) THEN
             farm_id = 1
          ELSE
             farm_id = 1 + NINT(REAL(my_rank/NUM_PROCS_IN_FARM) )
          END IF
       ELSE
          farm_id = 1 + (my_rank/NUM_PROCS_IN_FARM)
       END IF
         ELSE
       IF ( MOD(my_rank,NUM_PROCS_IN_FARM) /= 0 ) THEN
          farm_id = 1 + NINT(REAL(my_rank/NUM_PROCS_IN_FARM) )
       ELSE
          farm_id = my_rank/NUM_PROCS_IN_FARM
       END IF
    END IF
   
    !Split MY_COMM (MPI_COMM_WORLD) into farms
    CALL MPI_COMM_SPLIT(MY_COMM,farm_id,my_rank-1,COMM_FARM,ierr)
    
   !Get total number of processors within task farm
   CALL MPI_comm_size(COMM_FARM,nprocs_in_farm,ierr)
   IF (ierr/=MPI_SUCCESS) STOP 'Error in size of new communicator'
   CALL MPI_comm_rank(COMM_FARM,my_new_id, ierr)
   IF (ierr/=MPI_SUCCESS) STOP 'Error in creating finding rank'
  
   !Set masters of each task farm   
   my_new_rank = my_new_id + 1
   IF (my_new_rank == 1) THEN
      ROOT = .TRUE.
      WRITE(*,*) 'Root node:',my_rank
   ELSE
      ROOT = .FALSE.
   END IF
   
   !WRITE(*,*) 'Processor',my_rank,'in farm',farm_id,'with rank',my_new_rank
   !CALL MPI_BARRIER(MY_COMM,ierr)

   RETURN
 END SUBROUTINE comms_create_farm
 !==============================================================
 SUBROUTINE comms_bcast_integer(array,length)
   use Config, only : stderr
   IMPLICIT none    
   
   INTEGER, intent(inout)  :: array  ! Data to be sent
   INTEGER, intent(in)     :: length ! The number of elements to send

   INTEGER                 :: ierror ! Error flag

   !Call MPI_bcast to transfer data
   call MPI_bcast(array,length,MPI_INTEGER,0,my_comm,ierror)
   
   IF (ierror.ne.MPI_success) THEN
      WRITE(stderr,*) 'Error comms_bcast_integer: MPI_bcast failed'
      call comms_finalize
      STOP
   END IF
   RETURN
 END SUBROUTINE comms_bcast_integer
 !==============================================================
 SUBROUTINE comms_bcast_real(array,length,IN_COMM)
    USE Config, only : dp,stderr
    IMPLICIT none
    REAL(kind=dp),intent(inout)  :: array  ! Data to be sent
    INTEGER, intent(in)          :: length ! The number of elements to send
    INTEGER                      :: ierror ! Error flag
    INTEGER,INTENT(IN)           :: IN_COMM !Communicator to broadcast over
  
    !Call MPI_bcast to transfer data
    CALL MPI_bcast(array,length,MPI_DOUBLE_PRECISION,0,IN_COMM,ierror)
   
    IF (ierror.ne.MPI_success) THEN
       WRITE(stderr,*) 'Error comms_bcast_integer: MPI_bcast failed'
       CALL comms_finalize
       STOP
    END IF
    RETURN
 END SUBROUTINE comms_bcast_real
 !==============================================================
 SUBROUTINE comms_bcast_character(array,length)
   use Config, only : stderr
   IMPLICIT none
   CHARACTER,intent(inout)  :: array  ! Data to be sent
   INTEGER, intent(in)      :: length ! The number of elements to send

   INTEGER                  :: ierror ! Error flag

   !Call MPI_bcast to transfer data
   call MPI_bcast(array,length,MPI_CHARACTER,0,my_comm,ierror)
  
   IF (ierror.ne.MPI_success) THEN
      WRITE(stderr,*) 'Error comms_bcast_character: MPI_bcast failed'
      call comms_finalize
      STOP
   END IF
   RETURN
 END SUBROUTINE comms_bcast_character
 !==============================================================
 SUBROUTINE comms_bcast_logical(array,length,IN_COMM)
    USE Config, only : stderr
    IMPLICIT none
    LOGICAL, INTENT(inout)  :: array   ! Data to be sent
    INTEGER, INTENT(IN)     :: length  ! The number of elements to send
    INTEGER                 :: ierror  ! Error flag
    INTEGER,INTENT(IN)      :: IN_COMM !Communicator to use for broadcast 
    !Call MPI_bcast to transfer data
    CALL  MPI_bcast(array,length,MPI_LOGICAL,0,IN_COMM,ierror)
    IF (ierror.ne.MPI_success) THEN
       WRITE(stderr,*) 'Error comms_bcast_logical: MPI_bcast failed'
       CALL comms_finalize
       STOP
    END IF
    RETURN
 END SUBROUTINE comms_bcast_logical
 !==============================================================
SUBROUTINE comms_copy_to_root(in_pop)
  USE Config
  IMPLICIT none

  TYPE(cluster), INTENT(INOUT), DIMENSION(:) :: in_pop
  INTEGER :: i,ierror
  !Assign the positions of each processor to the TEMP array
  DO i = my_rank,N_POPULATION,nprocs
     TEMP_POP(i)%atoms = in_pop(i)%atoms 
     TEMP_POP(i)%energy = in_pop(i)%energy
     TEMP_POP(i)%selected = in_pop(i)%selected
  END DO
  !Now reset all the clusters' energy and positions
  in_pop(:)%energy     = 0.0_dp
  in_pop(:)%selected   = .FALSE.
  DO i = 1,N_POPULATION
     in_pop(i)%atoms(:)%x = 0.0_dp
     in_pop(i)%atoms(:)%y = 0.0_dp
     in_pop(i)%atoms(:)%z = 0.0_dp   
  END DO
  !Copy back the positions - now have array JUST of positions, energy etc.. on each processor
  DO i = my_rank,N_POPULATION,nprocs
     in_pop(i)%energy = TEMP_POP(i)%energy
     in_pop(i)%atoms  = TEMP_POP(i)%atoms 
     in_pop(i)%selected = TEMP_POP(i)%selected
  END DO

  !Finally zero the TEMP array
  TEMP_POP(:)%energy = 0.0_dp
  TEMP_POP(:)%selected = .FALSE.
 
  DO i = 1,N_POPULATION
     TEMP_POP(i)%atoms(:)%x = 0.00_dp
     TEMP_POP(i)%atoms(:)%y = 0.00_dp
     TEMP_POP(i)%atoms(:)%z = 0.00_dp
  END DO

  !Copy the cluster information back to root node
  CALL MPI_REDUCE(in_pop(:)%energy,TEMP_POP(:)%energy,N_POPULATION,MPI_DOUBLE_PRECISION,MPI_SUM,0,comm_farm,ierror)
  CALL MPI_REDUCE(in_pop(:)%selected,TEMP_POP(:)%selected,N_POPULATION,MPI_LOGICAL,MPI_LOR,0,comm_farm,ierror)
  
  in_pop(:)%energy   = TEMP_POP(:)%energy
  in_pop(:)%selected = TEMP_POP(:)%selected

  !Positional information
  DO i = 1,N_POPULATION
     CALL MPI_REDUCE(in_pop(i)%atoms(:)%x,TEMP_POP(i)%atoms(:)%x,MAX_ATOMS,MPI_DOUBLE_PRECISION,MPI_SUM,0,comm_farm,ierror)
     CALL MPI_REDUCE(in_pop(i)%atoms(:)%y,TEMP_POP(i)%atoms(:)%y,MAX_ATOMS,MPI_DOUBLE_PRECISION,MPI_SUM,0,comm_farm,ierror)
     CALL MPI_REDUCE(in_pop(i)%atoms(:)%z,TEMP_POP(i)%atoms(:)%z,MAX_ATOMS,MPI_DOUBLE_PRECISION,MPI_SUM,0,comm_farm,ierror)
     in_pop(i)%atoms(:)%x = TEMP_POP(i)%atoms(:)%x
     in_pop(i)%atoms(:)%y = TEMP_POP(i)%atoms(:)%y
     in_pop(i)%atoms(:)%z = TEMP_POP(i)%atoms(:)%z
  END DO
  
RETURN
END SUBROUTINE comms_copy_to_root
 !==============================================================
!Copy populations data to the MASTER_PROCprocessor
SUBROUTINE comms_copy_to_master(in_pop)
  USE Config
  IMPLICIT none

  TYPE(cluster), INTENT(INOUT), DIMENSION(:) :: in_pop
  INTEGER :: i,ierror

  !Assign the positions of each processor to the TEMP array
  DO i = my_rank,N_POPULATION,nprocs
     TEMP_POP(i)%atoms    = in_pop(i)%atoms 
     TEMP_POP(i)%energy   = in_pop(i)%energy
     TEMP_POP(i)%selected = in_pop(i)%selected
  END DO

  !Now reset all the clusters' energy and positions
  in_pop(:)%energy     = 0.0_dp
  in_pop(:)%selected   = .FALSE.

  DO i = 1,N_POPULATION
     in_pop(i)%atoms(:)%x = 0.0_dp
     in_pop(i)%atoms(:)%y = 0.0_dp
     in_pop(i)%atoms(:)%z = 0.0_dp
  END DO
  !Copy back the positions - now have array JUST of positions, energy etc.. on each processor
  DO i = my_rank,N_POPULATION,nprocs
     in_pop(i)%energy   = TEMP_POP(i)%energy
     in_pop(i)%atoms    = TEMP_POP(i)%atoms 
     in_pop(i)%selected = TEMP_POP(i)%selected
  END DO
  
  !Zero the TEMP array
  TEMP_POP(:)%energy = 0.0_dp
  TEMP_POP(:)%selected = .FALSE.
  DO i = 1,N_POPULATION
     TEMP_POP(i)%atoms(:)%x = 0.00_dp
     TEMP_POP(i)%atoms(:)%y = 0.00_dp
     TEMP_POP(i)%atoms(:)%z = 0.00_dp
  END DO
  !Copy the cluster information back to root node
  CALL MPI_REDUCE(in_pop(:)%energy,TEMP_POP(:)%energy,N_POPULATION,MPI_DOUBLE_PRECISION,MPI_SUM,0,MY_COMM,ierror)
  CALL MPI_REDUCE(in_pop(:)%selected,TEMP_POP(:)%selected,N_POPULATION,MPI_LOGICAL,MPI_LOR,0,MY_COMM,ierror)

  in_pop(:)%energy   = TEMP_POP(:)%energy
  in_pop(:)%selected = TEMP_POP(:)%selected

  !Positional information
  DO i = 1,N_POPULATION
     CALL MPI_REDUCE(in_pop(i)%atoms(:)%x,TEMP_POP(i)%atoms(:)%x,MAX_ATOMS,MPI_DOUBLE_PRECISION,MPI_SUM,0,MY_COMM,ierror)
     CALL MPI_REDUCE(in_pop(i)%atoms(:)%y,TEMP_POP(i)%atoms(:)%y,MAX_ATOMS,MPI_DOUBLE_PRECISION,MPI_SUM,0,MY_COMM,ierror)
     CALL MPI_REDUCE(in_pop(i)%atoms(:)%z,TEMP_POP(i)%atoms(:)%z,MAX_ATOMS,MPI_DOUBLE_PRECISION,MPI_SUM,0,MY_COMM,ierror)
     in_pop(i)%atoms(:)%x = TEMP_POP(i)%atoms(:)%x
     in_pop(i)%atoms(:)%y = TEMP_POP(i)%atoms(:)%y
     in_pop(i)%atoms(:)%z = TEMP_POP(i)%atoms(:)%z
  END DO

RETURN
END SUBROUTINE comms_copy_to_master
!=====================================================================================!
!!$SUBROUTINE comms_scatter(in_pop)
!!$  USE Config
!!$
!!$  TYPE(cluster), INTENT(INOUT), DIMENSION(:) :: in_pop
!!$  INTEGER :: i,ierror
!!$
!!$  TEMP_POP = in_pop
!!$  
!!$  !Send data from root to all the other nodes
!!$  CALL MPI_Scatter(in_pop(:)%energy,N_POPULATION, MPI_DOUBLE_PRECISION,& 
!!$       & TEMP_POP(:)%energy,N_POPULATION, MPI_DOUBLE_PRECISION,0, &
!!$       & MY_COMM,ierror)
!!$  CALL MPI_Scatter(in_pop(:)%selected,N_POPULATION, MPI_LOGICAL,& 
!!$       & TEMP_POP(:)%selected,N_POPULATION, MPI_LOGICAL,0, &
!!$       & MY_COMM,ierror)
!!$  
!!$
!!$  DO i = 1,N_POPULATION
!!$     CALL MPI_Scatter(in_pop(i)%atoms(:)%x,N_ATOMS, MPI_DOUBLE_PRECISION,& 
!!$          & TEMP_POP(i)%atoms(:)%x,N_ATOMS, MPI_DOUBLE_PRECISION,0, &
!!$          & MY_COMM,ierror)
!!$     CALL MPI_Scatter(in_pop(i)%atoms(:)%y,N_ATOMS, MPI_DOUBLE_PRECISION,& 
!!$          & TEMP_POP(i)%atoms(:)%y,N_ATOMS, MPI_DOUBLE_PRECISION,0, &
!!$          & MY_COMM,ierror)
!!$     CALL MPI_Scatter(in_pop(i)%atoms(:)%z,N_ATOMS, MPI_DOUBLE_PRECISION,& 
!!$          & TEMP_POP(i)%atoms(:)%z,N_ATOMS, MPI_DOUBLE_PRECISION,0, &
!!$          & MY_COMM,ierror)
!!$  END DO
!!$
!!$!  in_pop = TEMP_POP
!!$
!!$  RETURN
!!$END SUBROUTINE comms_scatter
!=====================================================================================!
 SUBROUTINE comms_finalize
   use Config, only : stderr
   IMPLICIT none
   INTEGER :: ierr
   call MPI_finalize(ierr)
   IF (ierr /= MPI_success) THEN
          write(*,*) 'Error comms_finalise: MPI_finalize failed.'
          call MPI_abort(my_comm,1,ierr)
       END IF
   RETURN
 END SUBROUTINE comms_finalize
!=======================================================================!
END MODULE comms
