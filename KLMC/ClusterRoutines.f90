MODULE ClusterRoutines

    USE Config
    USE Format
    USE Library
    USE Timer
    USE File,        ONLY : generateAimsInput, generateGulpInput, generateVaspInput, &
                            generateNWChemInput,logMessage, readPotential,removeClusterFiles, &
                            retrieveAimsData, retrieveData,retrieveVaspData,retrieveNWChemData, &
                            RUN_AIMS, RUN_GULP, RUN_VASP,RUN_NWCHEM, peek, RUN_DMOL, makeFolder, &
                            outputCAR, readOutMol, outputXYZ

    USE UnitCell,    ONLY : distance_3, lattice_c2v_3D, translate_3,checkCell

    USE Utilities,   ONLY : getHashKey, findHashKeyMatch, comparePMOI, standardiseclustercoordinates, &
                            getClusterPMOI, compareHashKeys, getClusterAtomsSepSq
    USE Grid

    IMPLICIT NONE

!==========================================================================================!
CONTAINS

! subroutines: checkCell enforceContainer evaluate getMadelungPot inclusionZone
!              relaxAtoms relaxSprings
!
!  functions: areIdentical collapsed dspecies forwardJump fragmented
!             getAtomicMass getEnergy getGnorm getTraceInertia
!             notValidEnergy notValidCluster alignCoreShell

!==========================================================================================!

SUBROUTINE evaluate_single(inout_cluster,in_i)
    TYPE(cluster),DIMENSION(:), INTENT(INOUT) :: inout_cluster
    TYPE(cluster),DIMENSION(1)                :: tmp_cluster
    INTEGER, INTENT(IN)  :: in_i

    tmp_cluster(1) = inout_cluster(in_i)
    CALL evaluate(tmp_cluster)
    inout_cluster(in_i) = tmp_cluster(1) 

END SUBROUTINE


SUBROUTINE evaluate_single_x(inout_cluster)

    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    TYPE(cluster), DIMENSION(1)  :: tmp_cluster

    INTEGER :: ide, idx

    DO ide = 1, N_DEF_ENERGY

      tmp_cluster(1) = inout_cluster

      tmp_cluster(1)%edefn=ide
      tmp_cluster(1)%id(1:1)=CLUSTER_PREFIX(ide)

      CALL evaluate(tmp_cluster)
      inout_cluster = tmp_cluster(1)

      IF (inout_cluster%edefn==0) THEN
        DO idx = ide,1,-1
          inout_cluster%id(1:1)=CLUSTER_PREFIX(idx)
          CALL removeClusterFiles(inout_cluster)
        ENDDO

        inout_cluster%id(1:1)=INITIAL_PREFIX
      ENDIF

      IF (inout_cluster%edefn /= ide) THEN
        inout_cluster%relaxFailed = .TRUE.

      ELSEIF (getEnergy(inout_cluster) > R_REFINE_THRESHOLD(ide)) THEN
        inout_cluster%energy(0) = alignCoreShell(inout_cluster)

        inout_cluster%relaxFailed = .TRUE.
      ELSE
        inout_cluster%energy(0) = getEnergy(inout_cluster)
      ENDIF

    ENDDO

    IF (.NOT. inout_cluster%relaxFailed) THEN
      ! Recentre cluster to geometrical centre?
      IF (L_RECENTRE_AFTER_EVAL) THEN
        CALL recentreCluster(inout_cluster)
      ENDIF

      ! Standardise
      IF (L_CMPR_PMOI) THEN
        CALL standardiseClusterCoordinates(inout_cluster)

        CALL getClusterPMOI(inout_cluster)
      ENDIF
    ENDIF

END SUBROUTINE 

!==========================================================================================!
! Routine to replace cluster atoms and rescale their positions
!==========================================================================================!
SUBROUTINE dataMineCluster(inout_cluster)

  TYPE(cluster), INTENT(INOUT) :: inout_cluster
  INTEGER :: I, J
  REAL(KIND=DBL) :: newxc, newyc, newzc, xc1, yc1, zc1

  IF (DM_ATOMS_SWAP_CNT .LT. 1) THEN
    RETURN
  ENDIF

  DO I = 1, DM_ATOMS_SWAP_CNT
    DO J = 1, N_ATOMS
      IF (TRIM(inout_cluster%atoms(J)%symbol) .EQ. TRIM(DM_ATOMS_FROM(I))) THEN
        inout_cluster%atoms(J)%symbol = TRIM(DM_ATOMS_TO(I))

        ! How about charge?
      ENDIF
    ENDDO
  ENDDO

  ! Rescaling the cluster
  CALL rescaleCluster(inout_cluster, DM_COORD_COEF)

  IF (DM_RECENTRE) THEN
    CALL recentreCluster(inout_cluster)
  ENDIF

END SUBROUTINE dataMineCluster

!==========================================================================================!
! A routine to rescale a cluster
!==========================================================================================!
SUBROUTINE rescaleCluster(inout_cluster, scalingCoef)
  TYPE(cluster), INTENT(INOUT) :: inout_cluster
  REAL(KIND=DBL), INTENT(IN)   :: scalingCoef

  INTEGER :: I
  REAL(KIND=DBL) :: newxc, newyc, newzc, xc1, yc1, zc1

  ! Ideally should rescale out from CoM st GA mating still okay, but for now . . . .
  ! Let's fix the first atom and scale the coordinates of others!
  xc1 = inout_cluster%atoms(1)%xc
  yc1 = inout_cluster%atoms(1)%yc
  zc1 = inout_cluster%atoms(1)%zc

  DO I = 2, N_ATOMS
    newxc = (inout_cluster%atoms(I)%xc - xc1) * scalingCoef
    newyc = (inout_cluster%atoms(I)%yc - yc1) * scalingCoef
    newzc = (inout_cluster%atoms(I)%zc - zc1) * scalingCoef

    inout_cluster%atoms(I)%xc = newxc + xc1
    inout_cluster%atoms(I)%yc = newyc + yc1
    inout_cluster%atoms(I)%zc = newzc + zc1
  ENDDO

END SUBROUTINE rescaleCluster

!==========================================================================================!



SUBROUTINE printHashkeys(inout_cluster)
  TYPE(cluster),DIMENSION(:), INTENT(INOUT) :: inout_cluster
  INTEGER :: i, clustersLen
  CHARACTER(LEN=200) :: outStr

  clustersLen = SIZE(inout_cluster)

  DO i = 1, clustersLen
    WRITE(outStr, *) i, inout_cluster(i)%id, inout_cluster(i)%edefn, inout_cluster(i)%hashkey1st, '  ', &
                     inout_cluster(i)%hashkey
    print *, outStr
  ENDDO

END SUBROUTINE printHashkeys


SUBROUTINE re_evaluate(inout_cluster)
    TYPE(cluster),DIMENSION(:), INTENT(INOUT) :: inout_cluster
    TYPE(cluster),DIMENSION(:),ALLOCATABLE    :: tmp_cluster
    INTEGER   :: i,j,count

    count = 0
    DO i = 1,SIZE(inout_cluster)
       IF ((inout_cluster(i)%edefn == 0) .AND. (inout_cluster(i)%status .NE. 0)) count = count+1
    END DO  

    IF (count > 0 ) THEN

       ALLOCATE(tmp_cluster(1:count))
       j = 0
       DO i = 1,size(inout_cluster)
          IF ((inout_cluster(i)%edefn == 0) .AND. (inout_cluster(i)%status .NE. 0)) THEN
             j = j + 1
             tmp_cluster(j)= inout_cluster(i)
          END IF
       END DO   
       CALL evaluate(tmp_cluster)

       j = 0
       DO i = 1,size(inout_cluster)
          IF ((inout_cluster(i)%edefn == 0) .AND. (inout_cluster(i)%status .NE. 0)) THEN
             j = j + 1
             inout_cluster(i)= tmp_cluster(j)
          END IF
       END DO   

      DEALLOCATE(tmp_cluster)
    END IF  
END SUBROUTINE 

SUBROUTINE evaluate(inout_cluster)

    TYPE(cluster),DIMENSION(:), INTENT(INOUT) :: inout_cluster
    INTEGER :: ide,idx, i, clustersLen
    CHARACTER(LEN=N_HASH_LEN) :: hashkey

    clustersLen = SIZE(inout_cluster)

    DO ide=1,N_DEF_ENERGY
      inout_cluster(:)%edefn=ide
      inout_cluster(:)%id(1:1)=CLUSTER_PREFIX(ide)

      CALL relaxAtoms(inout_cluster(:))

      DO i = 1,clustersLen
        IF (inout_cluster(i)%edefn==0) THEN
          DO idx=ide,1,-1
            inout_cluster(i)%id(1:1)=CLUSTER_PREFIX(idx)

            CALL removeClusterFiles(inout_cluster(i))
          ENDDO

          inout_cluster(i)%id(1:1)=INITIAL_PREFIX
        ENDIF

        IF (inout_cluster(i)%edefn/=ide) THEN
          inout_cluster(i)%relaxFailed = .TRUE.

        ELSEIF (getEnergy(inout_cluster(i)) > R_REFINE_THRESHOLD(ide)) THEN
          inout_cluster(i)%energy(0) = alignCoreShell(inout_cluster(i))

          inout_cluster(i)%relaxFailed = .TRUE.
        ELSE
          inout_cluster(i)%energy(0) = getEnergy(inout_cluster(i))
        ENDIF
      ENDDO
    ENDDO

    DO i = 1, clustersLen
      IF (.NOT. inout_cluster(i)%relaxFailed) THEN
        ! Recentre cluster to geometrical centre?
        IF (L_RECENTRE_AFTER_EVAL) THEN
          CALL recentreCluster(inout_cluster(i))
        ENDIF

        ! Standardise
        IF (L_CMPR_PMOI) THEN
          CALL standardiseClusterCoordinates(inout_cluster(i))

          CALL getClusterPMOI(inout_cluster(i))
        ENDIF
      ENDIF
    ENDDO


END SUBROUTINE evaluate

!==========================================================================================!

REAL(KIND=DBL) FUNCTION getEnergy(in_cluster,chosen_ide)
    TYPE(cluster), INTENT(IN) :: in_cluster
    INTEGER, INTENT(IN), OPTIONAL :: chosen_ide

    IF (PRESENT(chosen_ide)) THEN
      getEnergy=in_cluster%energy(chosen_ide)
    ELSEIF (in_cluster%edefn == 0) THEN
      getEnergy=R_ENERGY_ZERO
    ELSEIF (in_cluster%edefn .GT. MAX_E_DEFN) THEN
      getEnergy=R_ENERGY_ZERO
    ELSEIF (in_cluster%edefn == -1) THEN
      getEnergy=in_cluster%energy(0)
    ELSE
      getEnergy = in_cluster%energy(in_cluster%edefn)
    ENDIF

END FUNCTION getEnergy

!==========================================================================================!

REAL(KIND=DBL) FUNCTION getGnorm(in_cluster,chosen_ide)
    TYPE(cluster), INTENT(IN) :: in_cluster
    INTEGER, INTENT(IN), OPTIONAL :: chosen_ide
    INTEGER :: ide

    IF (PRESENT(chosen_ide)) THEN
      ide = chosen_ide
    ELSE
      ide = in_cluster%edefn
    ENDIF

    IF (ide==0) THEN
      getGnorm=1000.0

    ELSEIF (ide== -1) THEN
      getGnorm=0.0

    ELSE
      getGnorm=in_cluster%gnorm(ide)
    ENDIF
END FUNCTION getGnorm

!==========================================================================================!

!  Julian Gale, NRI, Curtin University, July 2007
FUNCTION getTraceInertia(in_cluster)
  TYPE(CLUSTER), INTENT(IN) :: in_cluster
  REAL(KIND=DBL) :: getTraceInertia
  REAL(KIND=DBL) :: trace, dx, dy, dz, mi
  REAL(KIND=DBL) :: totalmass, xcom, ycom, zcom
  INTEGER :: i

  trace = 0.0
! Find centre of mass
  xcom = 0.0
  ycom = 0.0
  zcom = 0.0
  totalmass = 0.0
  DO i = 1, N_ATOMS
     mi = getAtomicMass(in_cluster%atoms(i)%symbol)
     totalmass = totalmass + mi
     xcom = xcom + in_cluster%atoms(i)%xc * mi
     ycom = ycom + in_cluster%atoms(i)%yc * mi
     zcom = zcom + in_cluster%atoms(i)%zc * mi
  END DO
  xcom = xcom / totalmass
  ycom = ycom / totalmass
  zcom = zcom / totalmass
!
!  Calculate
!  tensor
  DO i = 1, N_ATOMS
     mi = getAtomicMass(in_cluster%atoms(i)%symbol)
     dx = in_cluster%atoms(i)%xc - xcom
     dy = in_cluster%atoms(i)%yc - ycom
     dz = in_cluster%atoms(i)%zc - zcom
     trace = trace + mi*(dx*dx + dy*dy + dz*dz)
   END DO
   getTraceInertia = trace
END FUNCTION getTraceInertia

!==========================================================================================!

FUNCTION getAtomicMass(str)
  CHARACTER(LEN=*), INTENT(IN)  :: str
  REAL(KIND=DBL) :: getAtomicMass
  INTEGER :: i
  DO i=1, 107  !Formerly maxele = max number of elements
     IF(INDEX(atsym(i),str) /= 0) EXIT
  END DO
  getAtomicMass = atmass(i)
END FUNCTION getAtomicMass

!==========================================================================================!

SUBROUTINE relaxAtoms(inout_cluster)
    TYPE(cluster), DIMENSION(:),INTENT(INOUT) :: inout_cluster
    REAL(KIND=DBL) :: grad_left
    REAL(KIND=DP), DIMENSION(1:N_POPULATION)    :: energy
    CHARACTER(LEN=100),DIMENSION(1:N_POPULATION) :: filename
    CHARACTER(LEN=1) :: temp=''
    INTEGER :: i,ide,n
    INTEGER,DIMENSION(1:N_POPULATION) :: ierr
    CHARACTER(LEN=100) :: filenameExt
    CHARACTER(LEN=100) :: outFileMessage

    ide=inout_cluster(1)%edefn
    temp=inout_cluster(1)%id(1:1)
    filename(1)=TRIM(RELAXED_FOLDER)//TRIM(inout_cluster(1)%id)

    IF (DEF_ENERGY(ide) == 'V') THEN

      PRINT *, "TLA: FAILCHECK HAS NOT BEEN IMPLEMENTED YET"
      PRINT *, "TLA: CHECK STATUS NOT IMPLEMENTED"

      DO i = 1,SIZE(inout_cluster)
        filename(i)=TRIM(RELAXED_FOLDER)//TRIM(inout_cluster(i)%id)
        CALL generateVaspInput(inout_cluster(i),TRIM(inout_cluster(i)%id)) 
        WRITE(*,*) 'Refining ',inout_cluster(i)%id,' with VASP'
      END DO
      
      CALL RUN_VASP(inout_cluster(:)%id,energy(:),ierr(:))
    
      DO i = 1,SIZE(inout_cluster)
        inout_cluster(i)%energy(ide)=energy(i)
        IF ((ierr(i) /= 0).OR.(notValidEnergy(inout_cluster(i)))) THEN
          !VASP DID NOT COMPLETE OR FAILED
          WRITE(stderr,*)'VASP did not complete or failed for ',TRIM(inout_cluster(i)%id)
        ELSE IF (.NOT. L_ENERGY_ONLY ) THEN
         CALL retrieveVaspData(TRIM(inout_cluster(i)%id),inout_cluster(i) )
        END IF
      END DO

    ELSE IF (DEF_ENERGY(ide) == 'G') THEN

      DO i = 1,SIZE(inout_cluster)

        ! check whether previus relaxations have failed
        IF ((inout_cluster(i)%relaxFailed) .OR. (inout_cluster(i)%status .EQ. 0) .OR. &
          & (L_USE_TOP_ANALYSIS .AND. (inout_cluster(i)%hashkeyMatchNo .NE. 0))) THEN
          CYCLE
        ENDIF

        filename(i)=TRIM(RELAXED_FOLDER)//TRIM(inout_cluster(i)%id)

        DO n = 1, N_RELAXATION_ATTEMPTS

          CALL generateGulpInput(ide, inout_cluster(i), TRIM(inout_cluster(i)%id))
          CALL RUN_GULP(TRIM(inout_cluster(i)%id), temp, energy(i), grad_left,ierr(i))

          IF (DEBUG_LEVEL > 100) WRITE(*,*) "ierr=",ierr(i)

          inout_cluster(i)%energy(ide)=energy(i)
          inout_cluster(i)%gnorm(ide)=grad_left

          IF (ierr(i) == -1) THEN
            WRITE(stderr,*) 'GULP crashed and burned for ',TRIM(inout_cluster(i)%id), &
                     ' on attempt ',TRIM(intToChar(n))
            inout_cluster(i)%edefn=ide-1
            inout_cluster(i)%status = 0
            EXIT

          ELSEIF (ierr(i) < -1 .AND. n == N_RELAXATION_ATTEMPTS) THEN

            WRITE(stderr,*) 'GULP requires more cycles for ',TRIM(inout_cluster(i)%id)
            CALL printStringColumns('GULP requires more cycles for',TRIM(inout_cluster(i)%id))
            WRITE(stdsee,*) 'GULP requires more cycles for ',TRIM(inout_cluster(i)%id)
            inout_cluster(i)%edefn=ide-1
            inout_cluster(i)%status = 0
            EXIT

          ELSEIF (ierr(i) < -1 .AND. .NOT. L_ENERGY_ONLY) THEN
            CALL retrieveData(inout_cluster(i),filename(i)) ! and try again

          ELSEIF ((ierr(i) > 0 .AND. grad_left > R_GNORM_TOL).OR.(notValidEnergy(inout_cluster(i)))) THEN
            WRITE(stderr,*) 'GULP run did not complete for ',TRIM(inout_cluster(i)%id)
            inout_cluster(i)%edefn=ide-1
            inout_cluster(i)%status = 0
            EXIT

          ELSE

            IF (.NOT.L_ENERGY_ONLY) THEN
              CALL retrieveData(inout_cluster(i),filename(i))
            ENDIF

            ! getting the hashkey. If the job is solid solutions ignore, since the structure might contain vacancies
            IF (L_USE_TOP_ANALYSIS) THEN
              inout_cluster(i)%hashkey = getHashKey(inout_cluster(i), hashRadius)
            ENDIF

            EXIT
          ENDIF
        ENDDO
      ENDDO

    ELSEIF (DEF_ENERGY(ide) == 'A') THEN


      PRINT *, "TLA: FAILCHECK HAS NOT BEEN IMPLEMENTED YET"
      PRINT *, "TLA: CHECK STATUS NOT IMPLEMENTED"

      CALL generateAimsInput(inout_cluster(1), TRIM(inout_cluster(1)%id))
      IF (.NOT. collapsed(inout_cluster(1),0.40_dp)) THEN ! this should be done elsewhere (smw)
       WRITE(*,*) 'Refining ',inout_cluster(1)%id,' with FHI-aims'
       CALL RUN_AIMS(TRIM(inout_cluster(1)%id),energy(1),ierr(1))
       inout_cluster(1)%energy(ide)=energy(1)
       WRITE(*,*) 'Candidate ',inout_cluster(1)%id,' converged with energy ',energy(1)
       IF ((ierr(1) /= 0).OR.(notValidEnergy(inout_cluster(1)))) THEN
          !FHI-aims RUN DID NOT COMPLETE SUCCESSFULLY
          WRITE(stderr,*) 'FHI-aims did not complete for ',TRIM(inout_cluster(1)%id) 
          inout_cluster(1)%edefn=ide-1
       ELSE IF (.NOT. L_ENERGY_ONLY ) THEN
           CALL retrieveAimsData( TRIM(inout_cluster(1)%id),inout_cluster(1) )
       END IF
      ELSE
         WRITE(stderr,*) 'Cluster:',TRIM(inout_cluster(1)%id),'has collapsed, & 
            skipping FHI-aims calculation'
            inout_cluster(1)%edefn=ide-1
      END IF 

    ELSEIF (DEF_ENERGY(ide) == 'N') THEN

      PRINT *, "TLA: FAILCHECK HAS NOT BEEN IMPLEMENTED YET"
      PRINT *, "TLA: CHECK STATUS NOT IMPLEMENTED"

      CALL generateNWChemInput(inout_cluster(1),TRIM(inout_cluster(1)%id)) 
      WRITE(*,*) 'Refining ',inout_cluster(1)%id,' with NWChem'
      CALL RUN_NWCHEM(TRIM(inout_cluster(1)%id),energy(1),ierr(1))
      inout_cluster(1)%energy(ide)=energy(1)
      IF ((ierr(1) /= 0).OR.(notValidEnergy(inout_cluster(1)))) THEN
        !NWCHEM DID NOT COMPLETE OR FAILED
        WRITE(stderr,*)'NWChem did not complete or failed for ',TRIM(inout_cluster(1)%id)
      ELSE IF (.NOT. L_ENERGY_ONLY ) THEN
         CALL retrieveNWChemData(TRIM(inout_cluster(1)%id),inout_cluster(1) )
      END IF

    ELSEIF (DEF_ENERGY(ide) == 'D') THEN

      DO i = 1,SIZE(inout_cluster)
        ! check whether the previous relaxations have failed

        IF ((inout_cluster(i)%relaxFailed) .OR. (inout_cluster(i)%status .EQ. 0) .OR. &
          & (L_USE_TOP_ANALYSIS .AND. (inout_cluster(i)%hashkeyMatchNo .NE. 0))) THEN
          CYCLE
        ENDIF

        IF (L_APPLY_PRESSURE) THEN
          filenameExt = TRIM(inout_cluster(i)%id) // "_PRESS"
        ELSE
          filenameExt = TRIM(inout_cluster(i)%id)
        ENDIF

        filename(i) = TRIM(RELAXED_FOLDER) // TRIM(filenameExt)

        CALL outputCAR(inout_cluster(i), "Generated by KLMC", TRIM(filename(i)), L_APPLY_PRESSURE)

        ! Execute DMol3
        CALL RUN_DMOL(filenameExt, ierr(i), ide, .FALSE.)

        IF (DEBUG_LEVEL > 100) WRITE(*,*) "ierr=", ierr(i)

        IF (ierr(i) /= 0) THEN
          WRITE(stderr, *) 'DMol3 did not complete or failed for ', TRIM(inout_cluster(i)%id), "ierr=", ierr(i)

          inout_cluster(i)%edefn = ide - 1
          inout_cluster(i)%status = 0
        ELSE

          IF (L_APPLY_PRESSURE) THEN
            filenameExt = TRIM(inout_cluster(i)%id)
            filename(i) = TRIM(RELAXED_FOLDER) // TRIM(filenameExt)

            CALL outputCAR(inout_cluster(i), "Generated by KLMC", filename(i))

            ! Execute DMol3
            CALL RUN_DMOL(filenameExt, ierr(i), ide, .TRUE.)

            IF (DEBUG_LEVEL > 100) WRITE(*,*) "ierr=", ierr(i)

            IF (ierr(i) /= 0) THEN
              WRITE(stderr, *) 'DMol(3) did not complete or failed for ',TRIM(inout_cluster(i)%id)

              inout_cluster(i)%edefn = ide - 1
              inout_cluster(i)%status = 0
            ENDIF
          ENDIF

          ! Retrieve data
          CALL readOutMol(inout_cluster(i), TRIM(filename(i)))

          IF (notValidEnergy(inout_cluster(i))) THEN
            WRITE(stderr, *) 'Energy is not valid. DMol3 did not complete or failed for ',TRIM(inout_cluster(i)%id)

            inout_cluster(i)%edefn=ide-1
            inout_cluster(i)%status = 0
          ENDIF

          ! getting the hashkey
          IF (L_USE_TOP_ANALYSIS) THEN
            inout_cluster(i)%hashkey = TRIM(getHashKey(inout_cluster(i), hashRadius))

          ENDIF

        ENDIF

      ENDDO

    ELSEIF (DEF_ENERGY(ide) == 'X') THEN

      outFileMessage = "Generated by KLMC DM"

      DO i = 1,SIZE(inout_cluster)
        ! edefn=-1 means that a cluster was not optimised, but should not be discarded
        inout_cluster(i)%edefn = -1

        filenameExt = TRIM(inout_cluster(i)%id)
        filename(i) = TRIM(RELAXED_FOLDER) // TRIM(filenameExt)

        IF (TRIM(DM_NOEVAL_OUT_FORMAT) .EQ. 'car') THEN
          CALL outputCAR(inout_cluster(i), outFileMessage, filename(i))

        ELSEIF (TRIM(DM_NOEVAL_OUT_FORMAT) .EQ. 'xyz') THEN
          CALL outputXYZ(inout_cluster(i), outFileMessage, filename(i))

        ENDIF

      ENDDO

    ELSE
      WRITE(stderr,*) DEF_ENERGY(ide),'is an undefined energy definition'
      STOP
    ENDIF

  RETURN
END SUBROUTINE relaxAtoms

!==========================================================================================!

SUBROUTINE relaxSprings(inout_cluster)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    REAL(KIND=DBL) :: energy,gnorm
    CHARACTER(LEN=60) :: filename=''
    INTEGER :: n_gulp_run,ierr
    inout_cluster%edefn=1
!   inout_cluster%id(1:1)='C'
    filename=TRIM(RELAXED_FOLDER)//TRIM(inout_cluster%id)
    n_gulp_run=1

    CALL generateGulpInput(n_gulp_run, inout_cluster, TRIM(inout_cluster%id))
    CALL RUN_GULP(inout_cluster%id,'temp',energy,gnorm,ierr)
    CALL retrieveData(inout_cluster, filename)
    CALL readPotential(inout_cluster,'temp')
END SUBROUTINE relaxSprings

!==========================================================================================!

SUBROUTINE getMadelungPot(inout_cluster)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    REAL(KIND=DBL) :: energy,gnorm
    INTEGER :: i, n_gulp_run, ikey, ierr
    CHARACTER(LEN=60) :: filename=''
    filename=TRIM(RELAXED_FOLDER)//TRIM(inout_cluster%id)
    n_gulp_run=1

    toploop: DO i = 1, SIZE(MASTER_TOP)
      IF (LEN_TRIM(MASTER_TOP(i)) < 1) EXIT toploop
      IF (index(MASTER_TOP(i),'#').ne.0) THEN
      ELSE
        ikey=i
        MASTER_KEY=''
        MASTER_KEY=MASTER_TOP(i)
        MASTER_TOP(i)=''
        MASTER_TOP(i)='pot'
        EXIT toploop
      ENDIF
    END DO toploop

    CALL generateGulpInput(n_gulp_run, inout_cluster, TRIM(inout_cluster%id))
    CALL RUN_GULP(inout_cluster%id,'temp',energy,gnorm,ierr)
    inout_cluster%edefn  = n_gulp_run
    CALL retrieveData(inout_cluster, filename)
    CALL readPotential(inout_cluster, 'temp')

    MASTER_TOP(ikey)=''
    MASTER_TOP(ikey)=MASTER_KEY
END SUBROUTINE getMadelungPot

!==========================================================================================!

SUBROUTINE inclusionZone(inout_cluster) ! called after random displacements applied
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    REAL(KIND=DBL) :: r0
    INTEGER :: i

    DO i=1, N_ATOMS
        r0 = inout_cluster%atoms(i)%xc - R_CENTRE_CLUSTER(1) 
        IF (r0 > R_MAX_CLUSTER_BOUNDARY(1)) inout_cluster%atoms(i)%xc = &
            R_CENTRE_CLUSTER(1) + R_MAX_CLUSTER_BOUNDARY(1)
        r0 = inout_cluster%atoms(i)%xc - R_CENTRE_CLUSTER(1) 
        IF (r0 < -R_MAX_CLUSTER_BOUNDARY(1)) inout_cluster%atoms(i)%xc = &
            R_CENTRE_CLUSTER(1) - R_MAX_CLUSTER_BOUNDARY(1)
        r0 = inout_cluster%atoms(i)%yc - R_CENTRE_CLUSTER(2) 
        IF (r0 > R_MAX_CLUSTER_BOUNDARY(2)) inout_cluster%atoms(i)%yc = &
            R_CENTRE_CLUSTER(2) + R_MAX_CLUSTER_BOUNDARY(2)
        r0 = inout_cluster%atoms(i)%yc - R_CENTRE_CLUSTER(2) 
        IF (r0 < -R_MAX_CLUSTER_BOUNDARY(2)) inout_cluster%atoms(i)%yc = &
            R_CENTRE_CLUSTER(2) - R_MAX_CLUSTER_BOUNDARY(2)
        r0 = inout_cluster%atoms(i)%zc - R_CENTRE_CLUSTER(3) 
        IF (r0 > R_MAX_CLUSTER_BOUNDARY(3)) inout_cluster%atoms(i)%zc = &
            R_CENTRE_CLUSTER(3) + R_MAX_CLUSTER_BOUNDARY(3)
        r0 = inout_cluster%atoms(i)%zc - R_CENTRE_CLUSTER(3) 
        IF (r0 < -R_MAX_CLUSTER_BOUNDARY(3)) inout_cluster%atoms(i)%zc = &
            R_CENTRE_CLUSTER(3) - R_MAX_CLUSTER_BOUNDARY(3)
    END DO
END SUBROUTINE inclusionZone

!==========================================================================================!

SUBROUTINE enforceContainer(inout_cluster, in_atom)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    INTEGER, INTENT(IN), OPTIONAL :: in_atom
    INTEGER :: i, j, ncheck

    IF (PRESENT(in_atom)) THEN
      j = in_atom
      ncheck = 1
    ELSE
      j=1
      ncheck = N_ATOMS
    ENDIF
    
    DO i=1, ncheck
       IF (inout_cluster%atoms(j)%xc > R_MAX_CLUSTER_BOUNDARY(1)) THEN
           inout_cluster%atoms(j)%xc = R_MAX_CLUSTER_BOUNDARY(1)
       ELSEIF (inout_cluster%atoms(j)%xc < -R_MAX_CLUSTER_BOUNDARY(1)) THEN
           inout_cluster%atoms(j)%xc = -R_MAX_CLUSTER_BOUNDARY(1)
       END IF
       IF (inout_cluster%atoms(j)%yc > R_MAX_CLUSTER_BOUNDARY(2)) THEN
           inout_cluster%atoms(j)%yc = R_MAX_CLUSTER_BOUNDARY(2)
       ELSEIF (inout_cluster%atoms(j)%yc < -R_MAX_CLUSTER_BOUNDARY(2)) THEN
           inout_cluster%atoms(j)%yc = -R_MAX_CLUSTER_BOUNDARY(2)
       END IF
       IF (inout_cluster%atoms(j)%zc > R_MAX_CLUSTER_BOUNDARY(3)) THEN
           inout_cluster%atoms(j)%zc = R_MAX_CLUSTER_BOUNDARY(3)
       ELSEIF (inout_cluster%atoms(j)%zc < -R_MAX_CLUSTER_BOUNDARY(3)) THEN
           inout_cluster%atoms(j)%zc = -R_MAX_CLUSTER_BOUNDARY(3)
       END IF
       j = j + 1
    ENDDO

    RETURN
END SUBROUTINE enforceContainer

!==========================================================================================!

LOGICAL FUNCTION notValidEnergy(in_cluster,comprehensive_check)
    TYPE(cluster), INTENT(IN) :: in_cluster
    LOGICAL, INTENT(IN), OPTIONAL :: comprehensive_check
    REAL(KIND=DBL) :: energy
    INTEGER :: ide0, ide

    IF (PRESENT(comprehensive_check)) THEN
      ide0 = 1
    ELSE
      ide0 = in_cluster%edefn
    ENDIF

    notValidEnergy = .TRUE.
    IF (in_cluster%edefn == 0) RETURN

    DO ide = ide0, in_cluster%edefn
      energy = getEnergy(in_cluster,ide)
      IF(energy < R_ENERGY_MIN_THRESHOLD(ide)) THEN
        WRITE(stderr,*)'WARNING cluster has energy lower than ',R_ENERGY_MIN_THRESHOLD(ide)
        RETURN
      ELSEIF(energy > R_ENERGY_MAX_THRESHOLD(ide)) THEN
        WRITE(stderr,*)'WARNING cluster has energy greater than ',R_ENERGY_MAX_THRESHOLD(ide)
        RETURN
      ENDIF
    ENDDO
    notValidEnergy = .FALSE.
    RETURN
END FUNCTION notValidEnergy

!==========================================================================================!
! Geometrical check of a cluster: if it is not collapsed, two species are not too close,
!   not fragmented, or do not have zero-coordination atoms
!==========================================================================================!
LOGICAL FUNCTION clusterGeometricalCheck(in_cluster, log_step)
  TYPE(cluster), INTENT(IN)     :: in_cluster
  INTEGER, INTENT(IN), OPTIONAL :: log_step

  LOGICAL :: reject
  CHARACTER*50 :: reason
  REAL(KIND=DBL) :: energy

  reject = .FALSE.
  reason = TRIM(in_cluster%id)

  ! Check if the cluster has collapsed
  IF (collapsed(in_cluster, R_COLLAPSE, USE_VARIABLE_RAD_COLL)) THEN ! cluster has collapsed
    reject = .TRUE.
    reason = TRIM(reason) // ': Cluster has collapsed!'
    energy = getEnergy(in_cluster)

  ! Check if the cluster has two like-charged species that are too close
  ELSEIF ((.NOT. L_AIMS_RUN) .AND. (.NOT. L_VASP_RUN) .AND. (.NOT. L_NWCHEM_RUN) .AND. &
    (dspecies(in_cluster,R_SPECIES))) THEN

    reject = .TRUE.
    reason = TRIM(reason) // ': Transfer of electrons!'
    energy = getEnergy(in_cluster)

  ! Check if the cluster is fragmented using two routines
  ELSEIF ((CHECK_FRAGMENTATION) .AND. fragmented_old(in_cluster, R_FRAGMENT, USE_VARIABLE_RAD_FRAG)) THEN
    IF (fragMatt(in_cluster, R_FRAGMENT, USE_VARIABLE_RAD_FRAG)) THEN
      reject = .TRUE.
      reason = TRIM(reason) // ': A fragmented cluster!'
      energy = getEnergy(in_cluster)
    ENDIF

  ! Check if the cluster has zero-coordination atoms
  ELSEIF (CHECK_ZERO_COORDINATION .AND. checkZeroCoordination(in_cluster, R_FRAGMENT, USE_VARIABLE_RAD_FRAG)) THEN
    reject = .TRUE.
    reason = TRIM(reason) // ': Cluster has zero coordinated atoms'
    energy = getEnergy(in_cluster)

  ENDIF

  IF (reject) THEN
    CALL printLog("clusterGeometricalCheck", reason, 11)
  ENDIF

  clusterGeometricalCheck = reject

  RETURN
END FUNCTION clusterGeometricalCheck

  !==========================================================================================!
  ! Geometrical check of a cluster's atom: if it is not collapsed, two species are not too close,
  !   not fragmented, or do not have zero-coordination atoms
  !==========================================================================================!
  LOGICAL FUNCTION clusterGeometricalCheckSingle(in_cluster, checkAtom, varyCollDist, varyFragDist)
    TYPE(cluster), INTENT(IN) :: in_cluster
    INTEGER,       INTENT(IN) :: checkAtom
    LOGICAL,       INTENT(IN) :: varyCollDist, varyFragDist

    CHARACTER*50 :: reason
    LOGICAL :: reject, fragmented
    INTEGER :: i, checkTo
    REAL(KIND=DBL) :: collDist, collDist2, dspeDist2, fragDist, fragDist2, sep2

    reject = .FALSE.
    reason = TRIM(in_cluster%id)

    collDist2 = R_COLLAPSE ** 2
    dspeDist2 = R_SPECIES ** 2
    fragDist2 = R_FRAGMENT ** 2

    fragmented = .TRUE.

    checkTo = checkAtom-1

    DO i = 1, checkTo

      sep2 = getClusterAtomsSepSq(in_cluster, checkAtom, i)

      ! Check if atoms are not too close
      IF (varyCollDist) THEN
        CALL getCollapsedRadius(in_cluster%atoms(checkAtom)%symbol, in_cluster%atoms(i)%symbol, collDist)
        collDist2 = collDist ** 2
      ENDIF

      ! Atoms are too cloose
      IF (sep2 .LT. collDist2) THEN
        reason = TRIM(reason) // ': Cluster has collapsed!'
        reject = .TRUE.

        EXIT
      ENDIF

      IF ((.NOT. L_AIMS_RUN) .AND. (.NOT. L_VASP_RUN) .AND. (.NOT. L_NWCHEM_RUN)) THEN
        IF (sep2 .LT. dspeDist2) THEN
          reason = TRIM(reason) // ': Transfer of electrons!'
          reject = .TRUE.

          EXIT
        ENDIF
      ENDIF

      ! Check if atoms are not fragmented
      IF (fragmented .AND. (CHECK_FRAGMENTATION .OR. CHECK_ZERO_COORDINATION)) THEN
        IF (varyFragDist) THEN
          CALL getFragmentRadius(in_cluster%atoms(checkAtom)%symbol, in_cluster%atoms(i)%symbol, fragDist)
          fragDist2 = fragDist ** 2
        ENDIF

        IF (sep2 < fragDist2) THEN
          fragmented = .FALSE.
        ENDIF
      ENDIF
    ENDDO

    IF (.NOT. reject .AND. fragmented) THEN
      reason = TRIM(reason) // ': A fragmented cluster!'
      reject = .TRUE.
    ENDIF

    IF (reject) THEN
      CALL printLog("clusterGeometricalCheckSingle", reason, 11)
    ENDIF

    clusterGeometricalCheckSingle = reject

    RETURN
  END FUNCTION clusterGeometricalCheckSingle

!==========================================================================================!

LOGICAL FUNCTION notValidCluster(in_cluster,log_step)
    TYPE(cluster), INTENT(IN) :: in_cluster
    INTEGER, INTENT(IN), OPTIONAL :: log_step
    LOGICAL :: reject
    INTEGER :: log_stepX
    REAL(KIND=DBL) :: energy
    CHARACTER*24 :: reason

    reject = .FALSE.
    reason = ''

    IF (.NOT. PRESENT(log_step)) THEN
      log_stepX = 0
    ELSE
      log_stepX = log_step
    ENDIF

    IF ((in_cluster%edefn == 0) .OR. (in_cluster%edefn .GT. MAX_E_DEFN)) THEN ! energy of cluster not defined
      reject = .TRUE.
      reason = 'Energy not defined!'
      energy = ZERO

      CALL logMessage(log_stepX, energy, reason)

    ELSEIF (in_cluster%status .EQ. 0) THEN
      reject = .TRUE.
      reason = 'Status 0!'
      energy = ZERO

      CALL logMessage(log_stepX, energy, reason)

    ELSEIF (N_PBC == 0) THEN
      ! Geometrical check of a cluster
      reject = clusterGeometricalCheck(in_cluster, log_stepX)

    ELSEIF (N_PBC == 3) THEN
    ! PERIODIC SYSTEM ROUTINES*************************
      CALL checkCell(in_cluster,reject)
      IF (reject) THEN
        reason='Structure -> 2D!'
        energy=getEnergy(in_cluster)

      ELSEIF (collapsed(in_cluster,R_COLLAPSE)) THEN ! structure has collapsed
        reject=.true.
        reason='Structure collapsed!'
        energy=getEnergy(in_cluster)

      ENDIF

      IF (reject) THEN
        CALL logMessage(log_stepX,energy,reason)
      ENDIF
    ENDIF

    IF (reject) THEN
      notValidCluster = .TRUE.
    ELSE
      notValidCluster = .FALSE.
    ENDIF

    RETURN
END FUNCTION notValidCluster

!==========================================================================================!
! Same as notValidCluster, but does not check if energy was evaluated
!==========================================================================================!
LOGICAL FUNCTION notValidClusterNoEChk(in_cluster,log_step)
  TYPE(cluster), INTENT(IN) :: in_cluster
  INTEGER, INTENT(IN), OPTIONAL :: log_step
  LOGICAL :: reject
  INTEGER :: log_stepX
  REAL(KIND=DBL) :: energy
  CHARACTER*24 :: reason

  reject = .FALSE.
  reason = ''

  IF (.NOT. PRESENT(log_step)) THEN
    log_stepX = 0
  ELSE
    log_stepX = log_step
  ENDIF

  IF (in_cluster%status .EQ. 0) THEN
    reject = .TRUE.
    reason = 'Status 0!'
    energy = ZERO

    CALL logMessage(log_stepX, energy, reason)

  ELSEIF (N_PBC == 0) THEN
    ! Geometrical check of a cluster
    reject = clusterGeometricalCheck(in_cluster, log_stepX)

  ELSEIF (N_PBC == 3) THEN
    ! PERIODIC SYSTEM ROUTINES*************************
    CALL checkCell(in_cluster,reject)
    IF (reject) THEN
      reason='Structure -> 2D!'
      energy=getEnergy(in_cluster)
    ELSEIF (collapsed(in_cluster,R_COLLAPSE)) THEN ! structure has collapsed
      reject=.true.
      reason='Structure collapsed!'
      energy=getEnergy(in_cluster)
    ENDIF

    CALL logMessage(log_stepX, energy,reason)
  ENDIF

  IF (reject) THEN
    notValidClusterNoEChk = .TRUE.
  ELSE
    notValidClusterNoEChk = .FALSE.
  ENDIF

  RETURN
END FUNCTION notValidClusterNoEChk


!==========================================================================================!
! A subroutine to check clusters initial hashkey against other clusters' initial hashkey
!==========================================================================================!
LOGICAL FUNCTION chkDplWithIniHashkeys(inout_cluster, in_clusters)
  TYPE(cluster), INTENT(INOUT) :: inout_cluster
  TYPE(cluster),DIMENSION(:), INTENT(INOUT) :: in_clusters

  INTEGER :: clustersLen, i
  LOGICAL :: hashEqual
  CHARACTER(LEN=100) :: message

  hashEqual = .FALSE.
  clustersLen = SIZE(in_clusters)

  DO i = 1, clustersLen
    hashEqual = compareHashKeys(inout_cluster, in_clusters(i), .TRUE.)

    IF (hashEqual) THEN

      IF (((JOB_TYPE == 2) .OR. (JOB_TYPE == 9)) .AND. GA_GEN_STATS) THEN
        in_clusters(i)%ga_stats%gaNoOfOccur = in_clusters(i)%ga_stats%gaNoOfOccur + 1
      ENDIF

      WRITE(message, *) "Clusters ", TRIM(inout_cluster%id), ' initial configuration is topolically equal to clusters ', &
        TRIM(in_clusters(i)%id)
      CALL printLog("chkDplWithIniHashkeys", message, 10)

      EXIT
    ENDIF
  ENDDO

  chkDplWithIniHashkeys = hashEqual

  RETURN
END FUNCTION chkDplWithIniHashkeys

!==========================================================================================!
! Compares two clusters based on energies and moment of inertia
!==========================================================================================!
LOGICAL FUNCTION areIdentical(cluster1, cluster2)
  TYPE(cluster), INTENT(IN) :: cluster1
  TYPE(cluster), INTENT(IN) :: cluster2

  REAL(KIND=DBL) :: trace1, trace2
  LOGICAL :: foundDuplicate, hashkeyMatch
  INTEGER :: ide

  CHARACTER(LEN=150) :: message

  ide = cluster1%edefn

  foundDuplicate = .FALSE.

  IF ((cluster1%edefn .EQ. 0) .OR. (cluster2%edefn .EQ. 0)) THEN
    foundDuplicate = .FALSE.

  ELSEIF ((cluster1%edefn .GT. MAX_E_DEFN) .OR. (cluster2%edefn .GT. MAX_E_DEFN)) THEN
    foundDuplicate = .FALSE.

  ! Comparing hashkeys
  ELSEIF (((L_USE_TOP_ANALYSIS) .AND. (compareHashKeys(cluster1, cluster2, .FALSE.)))) THEN
    foundDuplicate = .TRUE.

  ! Comparing pmoi's
  ELSEIF (((L_CMPR_PMOI) .AND. (comparePMOI(cluster1, cluster2)))) THEN
    foundDuplicate = .TRUE.

  ! Comparing energies
  ELSEIF ((ide == cluster2%edefn) .AND. &
          ((ABS(cluster1%energy(ide) - cluster2%energy(ide)) < R_ENERGY_TOLERANCE))) THEN

    foundDuplicate = .TRUE.
  ENDIF

  areIdentical = foundDuplicate

END FUNCTION areIdentical

!==========================================================================================!

LOGICAL FUNCTION dspecies(in_cluster,in_acc)
    TYPE(cluster), INTENT(IN) :: in_cluster
    REAL(KIND=DBL), INTENT(IN) :: in_acc
    REAL(KIND=DBL) :: compn_min,compn_max,r2,dx,dy,dz
    LOGICAL :: lbreak
    INTEGER :: n, m

    compn_max=in_acc
    compn_min=0.57735*in_acc

    lbreak=.false.
    IF (in_acc > 0.0) THEN
     atomsloop1: DO n=1,N_ATOMS-1
      atomsloop2: DO m=n+1,N_ATOMS 
        IF (in_cluster%atoms(n)%symbol.eq.in_cluster%atoms(m)%symbol) THEN
          ! .OR. product of charges is positive THEN
          IF(ABS(in_cluster%atoms(n)%xc-in_cluster%atoms(m)%xc) < compn_min)THEN
          IF(ABS(in_cluster%atoms(n)%yc-in_cluster%atoms(m)%yc) < compn_min)THEN
          IF(ABS(in_cluster%atoms(n)%zc-in_cluster%atoms(m)%zc) < compn_min)THEN
            lbreak=.TRUE.
            EXIT atomsloop2
          ENDIF 
          ENDIF 
          ENDIF 
        ENDIF 
      ENDDO atomsloop2
      IF (lbreak) EXIT atomsloop1
     ENDDO atomsloop1
    ENDIF

    IF (.not.lbreak) THEN
     r2=in_acc*in_acc
     atomsloop3: DO n=1,N_ATOMS-1
      atomsloop4: DO m=n+1,N_ATOMS 
        IF (in_cluster%atoms(n)%symbol.eq.in_cluster%atoms(m)%symbol) THEN
          ! .OR. product of charges is positive THEN
          dx = in_cluster%atoms(n)%xc-in_cluster%atoms(m)%xc
          IF (ABS(dx) < compn_max) THEN
            dy = in_cluster%atoms(n)%yc-in_cluster%atoms(m)%yc
            IF (ABS(dy) < compn_max) THEN
              dz = in_cluster%atoms(n)%zc-in_cluster%atoms(m)%zc
              IF (ABS(dz) < compn_max) THEN
                IF ( (dx*dx+dy*dy+dz*dz).lt.r2 ) THEN
                  !WRITE(stderr,*)'Closest distance between atoms:',SQRT(dx**2 + dy**2 + dz**2)
                  lbreak=.TRUE.
                  EXIT atomsloop4
                ENDIF
              ENDIF 
            ENDIF 
          ENDIF 
        ENDIF 
      ENDDO atomsloop4
      IF (lbreak) EXIT atomsloop3
     ENDDO atomsloop3
    ENDIF

    dspecies=lbreak
END FUNCTION dspecies

!==========================================================================================!

LOGICAL FUNCTION collapsed(in_cluster,in_acc, varyCollDist)
    TYPE(cluster), INTENT(IN) :: in_cluster
    REAL(KIND=DBL), INTENT(IN) :: in_acc
    LOGICAL, OPTIONAL, INTENT(IN) :: varyCollDist
    REAL(KIND=DBL) :: r2,dx,dy,dz,f1(3),f2(3),radius
    REAL(KIND=DBL) :: compn_min,compn_max, pairDist
    LOGICAL :: lbreak
    INTEGER :: n, m

    LOGICAL :: varyCollDistx

    IF (PRESENT(varyCollDist)) THEN
      varyCollDistx = varyCollDist
    ELSE
      varyCollDistx = .FALSE.
    ENDIF

    lbreak=.false.

    IF (N_PBC == 0) THEN

      compn_max=in_acc
      compn_min=0.57735*in_acc

      IF ((in_acc > 0.0) .OR. (varyCollDistx)) THEN

       atomsloop1: DO n=1,N_ATOMS-1
!       IF (in_cluster%atoms(n)%cstype /= 'x') THEN
        atomsloop2: DO m=n+1,N_ATOMS 

          IF (varyCollDistx) THEN
            CALL getCollapsedRadius(in_cluster%atoms(n)%symbol, in_cluster%atoms(m)%symbol, pairDist)
            compn_min = 0.57735 * pairDist
            compn_max = pairDist

          ENDIF

!        IF (in_cluster%atoms(m)%cstype /= 'x') THEN
          IF (ABS(in_cluster%atoms(n)%xc-in_cluster%atoms(m)%xc) < compn_min) THEN
          IF (ABS(in_cluster%atoms(n)%yc-in_cluster%atoms(m)%yc) < compn_min) THEN
          IF (ABS(in_cluster%atoms(n)%zc-in_cluster%atoms(m)%zc) < compn_min) THEN
            lbreak=.TRUE.
            EXIT atomsloop2
          ENDIF 
          ENDIF 
          ENDIF 
!        ENDIF
        ENDDO atomsloop2
        IF (lbreak) EXIT atomsloop1
!       ENDIF
       ENDDO atomsloop1
      ENDIF

      IF (.not.lbreak) THEN
       r2=in_acc*in_acc
       atomsloop3: DO n=1,N_ATOMS-1
        atomsloop4: DO m=n+1,N_ATOMS

          IF (varyCollDistx) THEN
            CALL getCollapsedRadius(in_cluster%atoms(n)%symbol, in_cluster%atoms(m)%symbol, pairDist)

            r2 = pairDist * pairDist
          ENDIF

          dx = in_cluster%atoms(n)%xc-in_cluster%atoms(m)%xc
          IF (ABS(dx) < compn_max) THEN
            dy = in_cluster%atoms(n)%yc-in_cluster%atoms(m)%yc
            IF (ABS(dy) < compn_max) THEN
              dz = in_cluster%atoms(n)%zc-in_cluster%atoms(m)%zc
              IF (ABS(dz) < compn_max) THEN
                IF ( (dx*dx+dy*dy+dz*dz).lt.r2 ) THEN

                  lbreak=.TRUE.
                  EXIT atomsloop4
                ENDIF
              ENDIF 
            ENDIF 
          ENDIF 
        ENDDO atomsloop4
        IF (lbreak) EXIT atomsloop3
       ENDDO atomsloop3
      ENDIF

    ELSEIF (N_PBC == 3) THEN

    CALL lattice_c2v_3D(in_cluster%box,VECTOR)

     atomsloop31: DO n=1,N_ATOMS-1
!     IF (in_cluster%atoms(n)%cstype /= 'x') THEN
       f1(1)=in_cluster%atoms(n)%xf
       f1(2)=in_cluster%atoms(n)%yf
       f1(3)=in_cluster%atoms(n)%zf

       atomsloop32: DO m=n+1,N_ATOMS 
!       IF (in_cluster%atoms(m)%cstype /= 'x') THEN
         f2(1)=in_cluster%atoms(m)%xf
         f2(2)=in_cluster%atoms(m)%yf
         f2(3)=in_cluster%atoms(m)%zf

        CALL translate_3(VECTOR,f1,f2)

        radius = in_acc
        IF (varyCollDistx) THEN
          CALL getCollapsedRadius(in_cluster%atoms(n)%symbol, in_cluster%atoms(m)%symbol, radius)
        ENDIF

        IF (distance_3(VECTOR,f1,f2) < radius) THEN
          lbreak=.TRUE.
          EXIT atomsloop32
        ENDIF

!       ENDIF
       ENDDO atomsloop32

       IF (lbreak) EXIT atomsloop31
!     ENDIF
     ENDDO atomsloop31
    ENDIF

    collapsed=lbreak
END FUNCTION collapsed

!==========================================================================================!
! Check if there are-zero coordinated atoms
!   It uses the fragmentation radius as the coordination distance.
!==========================================================================================!
LOGICAL FUNCTION checkZeroCoordination(in_cluster, coordist, varyCoorDist)

  TYPE(cluster), INTENT(IN) :: in_cluster
  REAL(KIND=DBL), INTENT(IN) :: coordist
  LOGICAL, OPTIONAL, INTENT(IN) :: varyCoorDist

  LOGICAL :: varyCoorDistx, zeroCoordExists
  REAL(KIND=DBL) :: coordist2, dx2, dy2, dz2, dist2
  INTEGER :: i, j
  INTEGER, DIMENSION(:), ALLOCATABLE :: coordination

  zeroCoordExists = .FALSE.

  ALLOCATE(coordination(N_ATOMS))

  DO i = 1, N_ATOMS
    coordination(i) = 0
  ENDDO

  coordist2 = coordist**2

  IF (PRESENT(varyCoorDist)) THEN
   varyCoorDistx = varyCoorDist
  ELSE
   varyCoorDistx = .FALSE.
  ENDIF

  DO i = 1, N_ATOMS-1
    DO j = i+1, N_ATOMS

      dx2 = (in_cluster%atoms(i)%xc - in_cluster%atoms(j)%xc)**2
      dy2 = (in_cluster%atoms(i)%yc - in_cluster%atoms(j)%yc)**2
      dz2 = (in_cluster%atoms(i)%zc - in_cluster%atoms(j)%zc)**2
      dist2 = dx2 + dy2 + dz2

      IF (varyCoorDistx) THEN
        ! uses the fragmentation radius as the coordination distance
        CALL getFragmentRadius(in_cluster%atoms(i)%symbol, in_cluster%atoms(j)%symbol, coordist2)
        coordist2 = coordist2 * coordist2
      ENDIF

      ! two atoms have a bond
      IF (dist2 <= coordist2) THEN
        coordination(i) = coordination(i) + 1
        coordination(j) = coordination(j) + 1
      ENDIF

    ENDDO
  ENDDO

  ! check for zero-coordinated atoms
  DO i = 1, N_ATOMS
    IF (coordination(i) .EQ. 0) THEN
      zeroCoordExists = .TRUE.
      EXIT
    ENDIF
  ENDDO

  DEALLOCATE(coordination)

  checkZeroCoordination = zeroCoordExists
END FUNCTION checkZeroCoordination

!==========================================================================================!

LOGICAL FUNCTION fragmented_old(in_cluster,in_acc, varyFragDist)
    TYPE(cluster), INTENT(IN) :: in_cluster
    REAL(KIND=DBL), INTENT(IN) :: in_acc
    LOGICAL, OPTIONAL, INTENT(IN) :: varyFragDist
    INTEGER :: nfrag(MAX_ATOMS)
    REAL(KIND=DBL) :: dx,dy,dz,dr2,compn_min,compn_max
    REAL(KIND=DP)  :: dist,min_dist, pairDist
    INTEGER :: n, m, na
    LOGICAL :: METALLIC,lbreak, varyFragDistx
	
	METALLIC=.FALSE.
    fragmented_old = .FALSE.
	
    IF (N_PBC /= 0) RETURN ! only works for clusters

    IF (PRESENT(varyFragDist)) THEN
      varyFragDistx = varyFragDist
    ELSE
      varyFragDistx = .FALSE.
    ENDIF

    IF ((in_acc > 0.0) .OR. (varyFragDistx)) THEN
      compn_min=0.57735*in_acc
      compn_max=in_acc
      dr2=in_acc*in_acc

      DO n=1,N_ATOMS
        nfrag(n)=n
      ENDDO

      atomsloop1: DO n=1,N_ATOMS

        lbreak=.true.
        DO m=n+1,N_ATOMS

          IF (nfrag(n).eq.nfrag(m)) THEN ! already part of same fragment
            lbreak=.false.

          ELSE

            !IF ( in_cluster%atoms(n)%symbol.ne.in_cluster%atoms(m)%symbol &
            !  .or. METALLIC) THEN

            dx=ABS(in_cluster%atoms(n)%xc-in_cluster%atoms(m)%xc)
            dy=ABS(in_cluster%atoms(n)%yc-in_cluster%atoms(m)%yc)
            dz=ABS(in_cluster%atoms(n)%zc-in_cluster%atoms(m)%zc)

            IF (varyFragDistx) THEN
              CALL getFragmentRadius(in_cluster%atoms(n)%symbol, in_cluster%atoms(m)%symbol, pairDist)

              compn_min = 0.57735 * pairDist
              compn_max = pairDist
              dr2 = pairDist * pairDist
            ENDIF

            IF ( dx.gt.compn_max .OR. dy.gt.compn_max .OR. dz.gt.compn_max) THEN
              CONTINUE
            ELSEIF ((dx.lt.compn_min.AND.dy.lt.compn_min.AND.dz.lt.compn_min) &
              .OR. ((dx.lt.compn_max.AND.dy.lt.compn_max.AND.dz.lt.compn_max) &
              .AND.( dx*dx+dy*dy+dz*dz.lt.dr2 ))) THEN
              lbreak=.false.
              IF (nfrag(n).lt.nfrag(m)) THEN
                DO na=1,m-1
                  IF (nfrag(na).eq.nfrag(m)) nfrag(na)=nfrag(n)
                ENDDO
                DO na=m+1,N_ATOMS
                  IF (nfrag(na).eq.nfrag(m)) nfrag(na)=nfrag(n)
                ENDDO
                nfrag(m)=nfrag(n)
              ELSE
                DO na=1,n-1
                  IF (nfrag(na).eq.nfrag(n)) nfrag(na)=nfrag(m)
                ENDDO
                DO na=n+1,N_ATOMS
                  IF (nfrag(na).eq.nfrag(n)) nfrag(na)=nfrag(m)
               ENDDO
                nfrag(n)=nfrag(m)
             ENDIF
            ENDIF
         ENDIF
        ENDDO
        IF (nfrag(n).eq.n.and.lbreak) THEN
         fragmented_old=.TRUE.
        EXIT atomsloop1
       ENDIF
     ENDDO atomsloop1
     atomsloop2: DO n=1,N_ATOMS
        IF (fragmented_old) EXIT atomsloop2
        IF (nfrag(n).ne.1) fragmented_old=.TRUE.
      ENDDO atomsloop2
    ENDIF

END FUNCTION fragmented_old
!==========================================================================================!

LOGICAL FUNCTION fragMatt(in_struct,fragdist, varyFragDist)
   TYPE(cluster),INTENT(IN)      :: in_struct
   REAL(KIND=dp),INTENT(IN)      :: fragdist
   LOGICAL, OPTIONAL, INTENT(IN) :: varyFragDist

   INTEGER                      :: i,j,k,file_unit=99
   INTEGER                      :: count,loop
   REAL(KIND=dp)                :: sum_x,sum_y,sum_z,dist
   REAL(KIND=dp),DIMENSION(1:3) :: start_pos
   REAL(KINd=dp)                :: dx,dy,dz,fragdist2
   LOGICAL,DIMENSION(1:MAX_ATOMS) :: clusterCheck 
   LOGICAL                      :: workToDo,checkflag
   REAL(KIND=dp),ALLOCATABLE,DIMENSION(:,:) :: work_array
   INTEGER,ALLOCATABLE,DIMENSION(:) :: int_array

   LOGICAL                      :: varyFragDistx

   fragdist2= fragdist**2
   workToDo = .FALSE.
   fragMatt = .TRUE.

   IF (PRESENT(varyFragDist)) THEN
     varyFragDistx = varyFragDist
   ELSE
     varyFragDistx = .FALSE.
   ENDIF

   count = 0
   DO i = 1,N_ATOMS
      dx = (in_struct%atoms(i)%xc - in_struct%atoms(1)%xc)**2
      dy = (in_struct%atoms(i)%yc - in_struct%atoms(1)%yc)**2
      dz = (in_struct%atoms(i)%zc - in_struct%atoms(1)%zc)**2
      dist = dx + dy + dz

      IF (varyFragDistx) THEN
        CALL getFragmentRadius(in_struct%atoms(i)%symbol, in_struct%atoms(1)%symbol, fragdist2)
        fragdist2 = fragdist2 * fragdist2
      ENDIF

      IF (dist <= fragdist2) THEN
         !All is well                                                                                   
         clusterCheck(i) = .FALSE.
      ELSE
         !Outside radius, could be a fragment                                                           
         count = count + 1
         clusterCheck(i) = .TRUE.
         workToDo  = .TRUE.
      END IF
   END DO

   IF (count == 0) THEN
      fragMatt = .FALSE.
      RETURN
   END IF

   !Now need to check whether work is to be done                                                        
   IF (workToDo) THEN
     ! WRITE(*,*) 'There are',count,'atoms outside COM fragmentation radius'
      checkflag = .TRUE.

      ALLOCATE(work_array(1:count,1:N_ATOMS))
      ALLOCATE(int_array(1:count))
      work_array(:,:) = 0.0_dp
      int_array(:) = 0
      k = 0

      DO i = 1,N_ATOMS
         !Found an atoms outside radius                                                                 
         IF (clusterCheck(i)) THEN
            IF (k > count) STOP 'k > count!'
            k = k+1
            DO j = 1,N_ATOMS
               IF (i /= j) THEN
                  dx = (in_struct%atoms(j)%xc - in_struct%atoms(i)%xc)**2
                  dy = (in_struct%atoms(j)%yc - in_struct%atoms(i)%yc)**2
                  dz = (in_struct%atoms(j)%zc - in_struct%atoms(i)%zc)**2
                  dist = dx + dy + dz
                  work_array(k,j) = dist
                  int_array(k) = i
                END IF
            END DO
         END IF
      END DO
      ! Now see if any atoms in the array are close to a COM radius                                    
      DO loop = 1,count-1
         DO j = 1,count
            DO i = 1,N_ATOMS

               IF (varyFragDistx) THEN
                 CALL getFragmentRadius(in_struct%atoms(int_array(j))%symbol, in_struct%atoms(i)%symbol, fragdist2)
                 fragdist2 = fragdist2 * fragdist2
               ENDIF

               !Is the atom inside COM sphere?                                                            
               IF (work_array(j,i) <= fragdist2) THEN
                  !If near other atom inside COM radius then not fragmented                              
                  IF (.NOT. clusterCheck(i)) THEN
                     !WRITE(*,*) 'Atom',int_array(j),'close to',i                                        
                      DO k = 1,N_ATOMS

                         IF (varyFragDistx) THEN
                           CALL getFragmentRadius(in_struct%atoms(int_array(j))%symbol, in_struct%atoms(k)%symbol, fragdist2)
                           fragdist2 = fragdist2 * fragdist2
                         ENDIF

                         IF(work_array(j,k) <= fragdist2) THEN
                            clusterCheck(k) =.FALSE.
                         END IF
                      END DO
                   END IF
                END IF
            END DO
         END DO
      END DO   
      DEALLOCATE(work_array)
      DEALLOCATE(int_array)
    ELSE
       fragMatt=.FALSE.
    END IF

    IF (fragMatt) THEN
       DO i = 1,N_ATOMS
           IF (clusterCheck(i)) THEN
              fragMatt = .TRUE.

              IF (DEBUG_LEVEL > 10) THEN
                WRITE (stderr,*) "fragMatt: ", in_struct%id
                WRITE(stderr,*)'Atom:',i
                WRITE(stderr,*) in_struct%atoms(i)%symbol,   &
                        in_struct%atoms(i)%xc,&
                        in_struct%atoms(i)%yc,&
                        in_struct%atoms(i)%zc
                WRITE(stderr,*) 'is fragmented in:'
                DO j= 1,N_ATOMS
                   WRITE(stderr,*) in_struct%atoms(j)%symbol,   &
                           in_struct%atoms(j)%xc,&
                           in_struct%atoms(j)%yc,&
                           in_struct%atoms(j)%zc
                END DO
              ENDIF

              EXIT
           ELSE
              fragMatt = .FALSE.
           END IF
       END DO
    END IF

END FUNCTION fragMatt

FUNCTION forwardJump(in_cluster1, in_cluster2, chkInvalidClusters)
    REAL(KIND=DBL) :: forwardJump
    TYPE(cluster), INTENT(IN) :: in_cluster1, in_cluster2
    LOGICAL, OPTIONAL, INTENT(IN) :: chkInvalidClusters

    REAL(KIND=DBL) :: denergy, infinity
    INTEGER :: ide1, ide2
    LOGICAL :: chkInvalidClustersX

    IF (PRESENT(chkInvalidClusters)) THEN
      chkInvalidClustersX = chkInvalidClusters
    ELSE
      chkInvalidClustersX = .FALSE.
    ENDIF

    infinity = 1000000.0_DP

    ide1 = in_cluster1%edefn
    ide2 = in_cluster2%edefn

    ! Sorting also clusters with ide = 0
    IF (chkInvalidClustersX) THEN
      ! If both are invalid
      IF ((ide1 == 0) .AND. (ide2 == 0)) THEN
        denergy = ZERO

      ! If one of the clusters has higher ide, that one is better
      ELSEIF(ide1 /= ide2) THEN
        denergy = (ide1 - ide2) * infinity
      ELSE
        denergy = getEnergy(in_cluster2) - getEnergy(in_cluster1)
      ENDIF
    ELSE
      ! Sorting in the oldway

      IF ((ide1 == 0) .OR. (ide2 == 0)) THEN
        denergy = ZERO

      ELSEIF (ide1 /= ide2) THEN
        denergy = (ide1-ide2)*infinity
      ELSE
        denergy = getEnergy(in_cluster2)-getEnergy(in_cluster1)
      ENDIF
    ENDIF

    forwardJump = denergy
END FUNCTION forwardJump

!==========================================================================================!

FUNCTION alignCoreShell(in_cluster)
    REAL(KIND=DBL) :: alignCoreShell
    TYPE(cluster), INTENT(IN) :: in_cluster
    INTEGER :: ide

    alignCoreShell = getEnergy(in_cluster)
    IF (L_SHELLS_1ST.AND.L_SHELLS_2ND) RETURN

    ide = in_cluster%edefn
    IF (ide > 2) RETURN

    IF (L_SHELLS_2ND.AND.ide==1) THEN
      alignCoreShell = R_SHELL_CORE_OFFSET + alignCoreShell
    ELSEIF (L_SHELLS_1ST.AND.ide==2) THEN
      alignCoreShell = R_SHELL_CORE_OFFSET + alignCoreShell
    ENDIF

END FUNCTION alignCoreShell

  !==========================================================================================!
  ! A routine to recentre a cluster
  !==========================================================================================!
  SUBROUTINE recentreCluster(inout_cluster)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster

    REAL(KIND=DBL) :: x_min, y_min, z_min, x_max, y_max, z_max, compn
    REAL(KIND=DBL) :: x_medium, y_medium, z_medium
    INTEGER :: i

    x_min =  inout_cluster%atoms(1)%xc
    y_min =  inout_cluster%atoms(1)%yc
    z_min =  inout_cluster%atoms(1)%zc

    x_max =  x_min
    y_max =  y_min
    z_max =  z_min

    DO i = 2, N_ATOMS
      compn = inout_cluster%atoms(i)%xc
      IF ( compn < x_min ) x_min = compn
      IF ( compn > x_max ) x_max = compn

      compn = inout_cluster%atoms(i)%yc
      IF ( compn < y_min ) y_min = compn
      IF ( compn > y_max ) y_max = compn

      compn = inout_cluster%atoms(i)%zc
      IF ( compn < z_min ) z_min = compn
      IF ( compn > z_max ) z_max = compn
    END DO

    x_medium = 0.5*(x_min + x_max)
    y_medium = 0.5*(y_min + y_max)
    z_medium = 0.5*(z_min + z_max)

    DO i = 1, N_ATOMS
      inout_cluster%atoms(i)%xc  = inout_cluster%atoms(i)%xc - x_medium
      inout_cluster%atoms(i)%yc  = inout_cluster%atoms(i)%yc - y_medium
      inout_cluster%atoms(i)%zc  = inout_cluster%atoms(i)%zc - z_medium
    END DO

  END SUBROUTINE recentreCluster

END MODULE ClusterRoutines
