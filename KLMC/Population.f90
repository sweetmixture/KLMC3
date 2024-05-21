MODULE Population

    USE Config
    USE Format
    USE Library
    USE ClusterRoutines, ONLY : notValidCluster, areIdentical, forwardJump, evaluate, evaluate_single, &
                                notValidClusterNoEChk, getEnergy, evaluate_single_x, printHashkeys, chkDplWithIniHashkeys
    USE Environment,     ONLY : findRestartData, getRestartData, reportItemDone
    USE File,            ONLY : inputCluster, outputCluster, peek, makeFolder, removeClusterFiles, outputCAR, outputXYZ
    USE Moveclass,       ONLY : getRandomAtoms, mutate, randomiseCell
    USE Utilities,       ONLY : getHashkeyRadius, wipeGAStatsSingle, getClusterNumber, getHashKey, &
                                addStructureToHashKeyLib
    USE Timer

    IMPLICIT NONE

    SAVE
    TYPE(cluster), PUBLIC :: MASTER_CLUSTER
    TYPE(cluster), PUBLIC, ALLOCATABLE, DIMENSION(:) :: POP     !Cluster population array
    TYPE(cluster), PUBLIC, ALLOCATABLE, DIMENSION(:) :: ELITE   !Cluster elitists
    REAL(KIND=DBL), PUBLIC :: ENERGY_MIN=0.

!==========================================================================================!
CONTAINS

! subroutines: initialiseCluster initialisePopulation 
!              insertIntoPop 
!              makeExtinct
!              peekIntoPopulation
!              removeDuplicates repopulate restartWalker
!              saveElites sortPopulation 
!              validateClusters 
!              writeElite writeElites 
!
!  functions:  

SUBROUTINE enforceIniPopSize(inout_pop)
  TYPE(cluster), INTENT(INOUT), DIMENSION(:) :: inout_pop

  INTEGER :: i, popLen, counter, duplCnt, minPop
  TYPE(cluster) :: newCluster
  LOGICAL :: invalidCluster, duplicateCluster
  CHARACTER(LEN=100) :: message

  popLen = SIZE(inout_pop)
  counter = getNoOfClusters(inout_pop)

  i = 1
  DO WHILE ((counter .LT. N_POPULATION) .AND. (i .LE. GA_INI_POP_ATTEMPTS))

    CALL increment(N_INDIVIDUALS)
    CALL initialiseCluster(newCluster)
    CALL getRandomAtoms(newCluster)

    IF (((JOB_TYPE == 2) .OR. (JOB_TYPE == 9)) .AND. GA_GEN_STATS) THEN
      newCluster%ga_stats%gaOrigin = "IREPOP"
      newCluster%ga_stats%gaGenNo = GA_ITER
    ENDIF

    ! initial hashkey
    IF (L_USE_TOP_ANALYSIS) THEN
      newCluster%hashkey1st = TRIM(getHashKey(newCluster, hashRadius))

      duplicateCluster = chkDplWithIniHashkeys(newCluster, inout_pop)

      IF (duplicateCluster) THEN
        CONTINUE
      ENDIF
    ENDIF

    CALL evaluate_single_x(newCluster)

    IF (JOB_TYPE == 2) THEN
      invalidCluster = notValidCluster(newCluster, GA_ITER)
    ELSE
      invalidCluster = notValidCluster(newCluster)
    ENDIF

    IF (.NOT. invalidCluster) THEN
      counter = counter + 1
      inout_pop(counter) = newCluster

      CALL sortPopulation(inout_pop, counter)

      duplCnt = removeDuplicatesX(inout_pop, counter)

      counter = getNoOfClusters(inout_pop)
    ENDIF

    IF (MOD(i, 10) == 0) THEN
      WRITE(message, *) "Populating: " // TRIM(ADJUSTL(intToStr(i))) // " Unique clusters: ", &
        TRIM(ADJUSTL(intToStr(counter)))

      CALL printLog("enforceIniPopSize", message, 5)
    ENDIF

    i = i + 1
  ENDDO

  minPop = NINT(DBLE(N_POPULATION) * GA_ENFORCE_MIN_SIZE)

  WRITE(message, *) "Generated " // TRIM(ADJUSTL(intToStr(counter))) // " unique initial clusters."
  CALL printLog("enforceIniPopSize", message, 0)
  CALL printOut("enforceIniPopSize", message, 0)

  IF (counter .LT. minPop) THEN
    WRITE(message, *) "Generated " // TRIM(ADJUSTL(intToStr(counter))) // " unique initial clusters, but " // &
      "minimum initial population: " // TRIM(ADJUSTL(intToStr(minPop))) // ". Terminating."

    CALL printLog("enforceIniPopSize", message, 0)
    CALL printOut("enforceIniPopSize", message, 0)

    STOP
  ENDIF

  WRITE(message, *) "Generated " // TRIM(ADJUSTL(intToStr(counter))) // " unique initial cluster, with a requirement of " // &
      "minimum initial population: " // TRIM(ADJUSTL(intToStr(minPop))) // "."
  CALL printLog("enforceIniPopSize", message, 0)

END SUBROUTINE enforceIniPopSize

!==========================================================================================!
! A function to return a number of valid members in an array
!==========================================================================================!
INTEGER FUNCTION getNoOfClusters(in_pop)
  TYPE(cluster), INTENT(IN), DIMENSION(:) :: in_pop
  INTEGER :: i, popLen

  getNoOfClusters = 0
  popLen = SIZE(in_pop)

  DO i = 1, popLen
    IF (in_pop(i)%edefn .NE. 0) THEN
      getNoOfClusters = getNoOfClusters + 1
    ENDIF
  ENDDO

END FUNCTION getNoOfClusters

!==========================================================================================!
! A function to return a number of valid elite members
!==========================================================================================!
INTEGER FUNCTION getNoOfElites()

  INTEGER :: elitesLen, i, counter

  elitesLen = SIZE(ELITE)
  counter = 0

  DO i = 1, elitesLen
    IF (ELITE(i)%edefn .NE. 0) THEN
      counter = counter + 1
    ENDIF
  ENDDO

  getNoOfElites = counter

END FUNCTION getNoOfElites

!==========================================================================================!

SUBROUTINE initialiseCluster(new_cluster,l_empty)
  TYPE(cluster), INTENT(OUT) :: new_cluster
  LOGICAL, INTENT(IN), OPTIONAL :: l_empty

  new_cluster = MASTER_CLUSTER
  CALL setDefaults(new_cluster)
  IF(PRESENT(l_empty)) THEN
    new_cluster%id = INITIAL_PREFIX//'0'
  END IF
END SUBROUTINE initialiseCluster

!==========================================================================================!

SUBROUTINE initialisePopulation()
    REAL(KIND=DBL), DIMENSION(3) :: r   !Random number 3-D array
    INTEGER :: i,j,k
    CHARACTER(LEN=100) :: message

    IF (.NOT. ALLOCATED(POP)) ALLOCATE(POP(N_POPULATION), STAT=ALLOCATE_STAT)

    CALL findRestartData() ! define N_SEEDS and L_RESTART

    !IF (L_RESTART) THEN CALL read in data from RESTART_FOLDER/DATA using readControl(../...)

    DO i = 1, SIZE(POP)

      CALL initialiseCluster(POP(i))

      IF (i <= N_SEEDS) THEN
        CALL increment(N_INDIVIDUALS)
        POP(i)%id = 'Z'//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))
        CALL getRestartData(POP(i),i)
        CALL printStringColumns('Seed with a predefined structure in',TRIM(RESTART_FOLDER))

      ELSEIF (L_RESTART) THEN
        CALL restartPop(POP(i),i)
        CALL printStringColumns('Seed with previous population in',TRIM(RESTART_FOLDER)// &
                                                        'POP/'//TRIM(intToChar(i))//'.can')
      ELSEIF (JOB_TYPE /= 7) THEN
        CALL increment(N_INDIVIDUALS)
        CALL getRandomAtoms(POP(i))

        CALL printStringColumns('Using random data for',TRIM(POP(i)%id))

        IF (((JOB_TYPE == 2) .OR. (JOB_TYPE == 9)) .AND. GA_GEN_STATS) THEN
          POP(i)%ga_stats%gaOrigin = "RANDOM"
          POP(i)%ga_stats%gaGenNo = GA_ITER
        ENDIF
      ENDIF

      message = 'Data ready for candidate ' // TRIM(intToChar(i)) // ' in initial POP'
      CALL printLog("initialisePopulation", message, 1)

      IF ((JOB_TYPE == 2) .AND. (L_USE_TOP_ANALYSIS)) THEN
        ! getting the hash radius

        IF (i == 1) THEN
          IF (hashRadius .EQ. 0.0) THEN
            hashRadius = getHashkeyRadius(POP(i), N_ATOMS)
          ENDIF
        ENDIF

        ! getting initial hashkeys
        IF (hashRadius .NE. 0.0) THEN
          POP(i)%hashkey1st = TRIM(getHashKey(POP(i), hashRadius))
        ENDIF
      ENDIF
    ENDDO

    IF ( JOB_TYPE == 2) THEN !GA - NEED ELITE
     IF (.NOT. ALLOCATED(ELITE)) ALLOCATE(ELITE(N_ELITE_POP), STAT=ALLOCATE_STAT)

     DO i = 1, SIZE(ELITE)
       CALL initialiseCluster(ELITE(i),.TRUE.)
     END DO

     IF (L_RESTART) THEN
       CALL restartElites

     ELSE
       CALL makeFolder(TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'POP')
       CALL makeFolder(TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'ELITES')

!       DO i = 1, SIZE(ELITE)
!         CALL increment(N_INDIVIDUALS)
!         ELITE(i)%id = 'Z'//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))
!         CALL getRandomAtoms(ELITE(i), .TRUE.)
!
!         ! getting initial hashkeys
!         IF (hashRadius .NE. 0.0) THEN
!           ELITE(i)%hashkey1st = TRIM(getHashKey(ELITE(i), hashRadius))
!         ENDIF
!
!         IF (GA_GEN_STATS) THEN
!           ELITE(i)%ga_stats%gaOrigin = "RANDOM"
!           ELITE(i)%ga_stats%gaGenNo = GA_ITER
!         ENDIF
!
!         message = 'Data ready for candidate ' // TRIM(intToChar(i)) // ' in elite POP'
!         CALL printLog("initialisePopulation", message, 1)
!       ENDDO
     ENDIF
    ENDIF
    CALL printHeaderCentred('')

END SUBROUTINE initialisePopulation

!==========================================================================================!

SUBROUTINE peekIntoPopulation(enforce)
    LOGICAL, OPTIONAL, INTENT(IN) :: enforce
    INTEGER :: i
    IF ((PRESENT(enforce).AND.enforce).OR.(DEBUG_LEVEL > 10)) THEN
      DO i = 1, SIZE(POP)
        CALL peek(POP(i))
      END DO
    ENDIF
END SUBROUTINE peekIntoPopulation

!==========================================================================================!

SUBROUTINE sortPopulation(inout_pop, fixedSize)
    TYPE(cluster), INTENT(INOUT), DIMENSION(:) :: inout_pop
    INTEGER, INTENT(IN), OPTIONAL :: fixedSize

    TYPE(cluster) :: temp
!   INTEGER :: ide
    INTEGER :: n, i, j, k

    IF (PRESENT(fixedSize)) THEN
      n = fixedSize
    ELSE
      n = SIZE(inout_pop)
    ENDIF

!   ide=N_DEF_ENERGY
!   DO i=1, n
!     DO j=n, i+1,-1
!       IF (inout_pop(i)%energy(ide) > inout_pop(j)%energy(ide)) THEN
!         temp = inout_pop(i)
!         inout_pop(i) = inout_pop(j)
!         inout_pop(j) = temp
!       END IF
!     END DO
!   END DO

    DO i = 1, n-1

      k = i
      DO j = i+1, n
        IF (forwardJump(inout_pop(k),inout_pop(j), .TRUE.) < ZERO) THEN ! E(k) > E(j)
          k=j
        ENDIF
      END DO

      IF (k > i) THEN ! k has changed
        temp = inout_pop(i)
        inout_pop(i) = inout_pop(k)
        inout_pop(k) = temp
      END IF
    END DO

    RETURN
END SUBROUTINE sortPopulation

!==========================================================================================!

SUBROUTINE insertIntoPop(inout_pop,in_cluster)

! subroutine for inserting a cluster into a current population
! entries in pop should already be ranked (entry 1 => current GM)
! insert cluster if unique and better than current worst

    TYPE(cluster), INTENT(INOUT), DIMENSION(:) :: inout_pop
    TYPE(cluster), INTENT(IN) :: in_cluster

    TYPE(cluster) :: temp
    INTEGER :: ide
    INTEGER :: n_pop, i, in_here
    REAL(KIND=DBL) :: e

    ide = N_DEF_ENERGY ! only interested in final landscape
    IF (in_cluster%edefn /= ide) RETURN

    n_pop = SIZE(inout_pop)
    e = in_cluster%energy(ide)
    in_here=0
    DO i=1, n_pop
      IF (inout_pop(i)%energy(ide) > e) THEN
        in_here=i
        EXIT
      ENDIF
    END DO

!   if (in_here>1) then
!    print*,'DEBUG',in_here,in_cluster%energy(ide), &
!                 inout_pop(in_here-1)%energy(ide),inout_pop(in_here)%energy(ide)
!   else
!    print*,'DEBUG',in_here,in_cluster%energy(ide),inout_pop(in_here)%energy(ide)
!   endif

    IF (in_here > 0) THEN

      IF (areIdentical(inout_pop(in_here),in_cluster)) THEN
!       print*,'DEBUG replace twin with slightly better half'
        inout_pop(in_here) = in_cluster

      ELSEIF (in_here .LE. 1) THEN
        DO i=n_pop,in_here+1,-1
          inout_pop(i) = inout_pop(i-1)
        END DO
        inout_pop(in_here) = in_cluster

      ELSEIF ((in_here > 1) .AND. &
        areIdentical(inout_pop(in_here-1), in_cluster)) THEN
!       print*,'DEBUG no point inserting this duplicate candidate'

      ELSE
!       print*,'DEBUG insert new candidate and shift higher ranked entries'
        DO i=n_pop,in_here+1,-1
          inout_pop(i) = inout_pop(i-1)
        END DO
        inout_pop(in_here) = in_cluster
      ENDIF
    ENDIF
    RETURN
END SUBROUTINE insertIntoPop

!==========================================================================================!

SUBROUTINE removeDuplicates(inout_pop)
  TYPE(cluster), INTENT(INOUT), DIMENSION(:) :: inout_pop
  INTEGER :: i, j, n_pop, k, x, y, ide1, ide2

  TYPE(cluster) :: tempCluster
  CHARACTER(LEN=100) :: message

  n_pop = SIZE(inout_pop)

  outer: DO i = n_pop, 1, -1
    IF (inout_pop(i)%edefn /= 0) THEN

      inner:DO j = i-1, 1, -1

        IF (inout_pop(j)%edefn .EQ. 0) THEN
          CYCLE inner
        ENDIF

        IF(areIdentical(inout_pop(i), inout_pop(j))) THEN

          WRITE(message, *) "Cluster ", TRIM(inout_pop(i)%id), " is identical to ", TRIM(inout_pop(j)%id)
          CALL printLog("removeDuplicates", message, 5)

          ! remove duplicates by resetting the energy
          ! we should keep the one which HAS A LOWER ENERGY

          ide1 = inout_pop(i)%edefn
          ide2 = inout_pop(j)%edefn

          IF (ide1 .EQ. ide2) THEN

            IF (inout_pop(i)%energy(ide1) .LT. inout_pop(j)%energy(ide2)) THEN
              x = j
              y = i
            ELSE
              x = i
              y = j
            ENDIF

          ELSE IF (ide1 .GT. ide2) THEN
            x = j
            y = i
          ELSE
            x = i
            y = j
          ENDIF

          ! Saving the number of occurances
          IF (((JOB_TYPE == 2) .OR. (JOB_TYPE == 9)) .AND. GA_GEN_STATS) THEN
            inout_pop(y)%ga_stats%gaNoOfOccur = inout_pop(y)%ga_stats%gaNoOfOccur + &
                                                inout_pop(x)%ga_stats%gaNoOfOccur

            CALL addStructureToHashKeyLib(inout_pop(x))
          ENDIF

          inout_pop(x)%status = 0
          inout_pop(x)%edefn = 0
          CALL removeClusterFiles(inout_pop(x))

          ! moving everything up by one and placing the cluster at the bottom
          tempCluster = inout_pop(x)

          DO k = x, n_pop-1
            inout_pop(k) = inout_pop(k+1)
          ENDDO

          inout_pop(n_pop) = tempCluster

          CYCLE outer
        END IF
      END DO inner
    ENDIF
  END DO outer

END SUBROUTINE removeDuplicates

!==========================================================================================!
INTEGER FUNCTION removeDuplicatesX(inout_pop, fixedSize)

  TYPE(cluster), INTENT(INOUT), DIMENSION(:) :: inout_pop
  INTEGER, INTENT(IN), OPTIONAL :: fixedSize

  INTEGER :: i, j, n_pop, k, x, y, ide1, ide2

  TYPE(cluster) :: tempCluster

  CHARACTER(LEN=100) :: message

  removeDuplicatesX = 0

  IF (PRESENT(fixedSize)) THEN
    n_pop = fixedSize
  ELSE
    n_pop = SIZE(inout_pop)
  ENDIF

  outer:DO i=n_pop, 1, -1
    IF (inout_pop(i)%edefn /= 0) THEN

      inner:DO j=i-1, 1, -1

        IF (inout_pop(j)%edefn /= 0) THEN

          IF(areIdentical(inout_pop(i), inout_pop(j))) THEN

            WRITE(message, *) "Cluster ", TRIM(inout_pop(i)%id), " is identical to ", TRIM(inout_pop(j)%id)
            CALL printLog("removeDuplicatesX", message, 5)

            ! remove duplicates by resetting the energy
            ! we should keep the one which HAS A LOWER ENERGY

            ide1 = inout_pop(i)%edefn
            ide2 = inout_pop(j)%edefn

            IF (ide1 .EQ. ide2) THEN

              IF (inout_pop(i)%energy(ide1) .LT. inout_pop(j)%energy(ide2)) THEN
                x = j
                y = i
              ELSE
                x = i
                y = j
              ENDIF

            ELSE IF (ide1 .GT. ide2) THEN
              x = j
              y = i
            ELSE
              x = i
              y = j
            ENDIF

            ! Saving the number of occurances
            IF (((JOB_TYPE == 2) .OR. (JOB_TYPE == 9)) .AND. GA_GEN_STATS) THEN
              inout_pop(y)%ga_stats%gaNoOfOccur = inout_pop(y)%ga_stats%gaNoOfOccur + &
                                                  inout_pop(x)%ga_stats%gaNoOfOccur

              CALL addStructureToHashKeyLib(inout_pop(x))
            ENDIF

            inout_pop(x)%status = 0
            inout_pop(x)%edefn = 0
            CALL removeClusterFiles(inout_pop(x))

            removeDuplicatesX = removeDuplicatesX + 1

            ! moving everything up by one and placing the cluster at the bottom
            tempCluster = inout_pop(x)

            DO k = x, n_pop-1
              inout_pop(k) = inout_pop(k+1)
            ENDDO

            inout_pop(n_pop) = tempCluster
          END IF
        END IF
      END DO inner
    ENDIF
  END DO outer

END FUNCTION removeDuplicatesX

!==========================================================================================!

SUBROUTINE repopulate(inout_pop)

    USE moveclass, only : getRandom
    TYPE(cluster), INTENT(INOUT), DIMENSION(:) :: inout_pop
    INTEGER :: i, n_pop, n_elite, n_mutations, attemptCnt, repopType
    LOGICAL :: repopSuccess
    REAL(KIND=DBL) :: r

    TYPE(cluster) :: temp_cluster

    n_pop = SIZE(inout_pop)
    !n_elite = SIZE(ELITE)
    n_elite = getNoOfElites()

    IF (N_ATOMS < 5) THEN
      n_mutations = 1
    ELSEIF (N_ATOMS < 9) THEN
      n_mutations = 2
    ELSEIF (N_ATOMS < 13) THEN
      n_mutations = 3
    ELSEIF (N_ATOMS < 17) THEN
      n_mutations = 4
    ELSE
      n_mutations = 5
    ENDIF

    DO i = 1, n_pop

      IF((inout_pop(i)%edefn .EQ. 0) .OR. (inout_pop(i)%status .EQ. 0)) THEN

        CALL removeClusterFiles(inout_pop(i))
        CALL increment(N_INDIVIDUALS)

        CALL RANDOM_NUMBER(r)

        CALL wipeGAStatsSingle(inout_pop(i))

        repopSuccess = .FALSE.
        attemptCnt = 0

        repopType = 0

        DO WHILE ((.NOT. repopSuccess) .AND. (attemptCnt < 10000))

          IF (r < R_REINSERT_ELITES_RATIO) THEN
            ! fill with a mutated random elite

            temp_cluster = ELITE(getRandom(1, n_elite))

            CALL wipeGAStatsSingle(temp_cluster)

            temp_cluster = mutate(temp_cluster, n_mutations)

            repopType = 1

          ELSE ! replace with new random (foreign) structure
            ! = inout_pop(i)

            CALL initialiseCluster(temp_cluster)

            !CALL randomiseCell(temp_cluster)
            CALL getRandomAtoms(temp_cluster)

            repopType = 2
          END IF

          ! Geometrical checking
          IF (notValidClusterNoEChk(temp_cluster)) THEN
            repopSuccess = .FALSE.
          ELSE
            repopSuccess = .TRUE.
          ENDIF

          attemptCnt = attemptCnt + 1
        ENDDO

        IF (.NOT. repopSuccess) THEN
          temp_cluster%status = 0
        ELSE
          temp_cluster%status = 1
        ENDIF

        temp_cluster%id = 'Y'//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))
        temp_cluster%edefn = 0
        temp_cluster%energy(:) = R_ENERGY_ZERO
        temp_cluster%hashkey = UNDEFINED_HASHKEY
        temp_cluster%hashkeyMatchNo = 0
        temp_cluster%status = 1
        temp_cluster%relaxFailed = .FALSE.

        IF (((JOB_TYPE == 2) .OR. (JOB_TYPE == 9)) .AND. GA_GEN_STATS) THEN

          IF (repopType == 1) THEN
            temp_cluster%ga_stats%gaOrigin = "REPOPM"

          ELSEIF (repopType == 2) THEN
            temp_cluster%ga_stats%gaOrigin = "REPOPR"
          ELSE
            temp_cluster%ga_stats%gaOrigin = "REPOP-"
          ENDIF

          temp_cluster%ga_stats%gaGenNo = GA_ITER
          temp_cluster%ga_stats%gaRepopAttempts = attemptCnt

        ENDIF

        IF (repopSuccess) THEN
          CALL evaluate_single_x(temp_cluster)
        ENDIF
        inout_pop(i) = temp_cluster

      END IF
    END DO
END SUBROUTINE repopulate

!==========================================================================================!

SUBROUTINE validateClusters(inout_clusters,log_step)
    TYPE(cluster), INTENT(INOUT) :: inout_clusters(:)
    INTEGER, INTENT(IN), OPTIONAL :: log_step
    INTEGER :: i

    IF (PRESENT(log_step)) THEN
      DO i = 1, SIZE(inout_clusters)
        IF (inout_clusters(i)%edefn /= 0) THEN
          IF (notValidCluster(inout_clusters(i),log_step)) THEN
            CALL removeClusterFiles(inout_clusters(i))
            inout_clusters(i)%edefn = 0
          ENDIF
        ENDIF
      END DO

    ELSE

      DO i = 1, SIZE(inout_clusters)
        IF (inout_clusters(i)%edefn /= 0) THEN
          IF (notValidCluster(inout_clusters(i))) THEN

            CALL removeClusterFiles(inout_clusters(i))
            inout_clusters(i)%edefn = 0
          ENDIF
        ENDIF

      END DO
    ENDIF

END SUBROUTINE validateClusters

!==========================================================================================!

SUBROUTINE saveElites(in_pop)
  TYPE(cluster),INTENT(IN),DIMENSION(:) :: in_pop
  INTEGER :: i, n_pop, n_elite,ide
  CHARACTER(LEN=100) :: message

  ide = N_DEF_ENERGY ! use final landscape

  n_pop = SIZE(in_pop)
  n_elite = SIZE(ELITE) ! N_ELITE_POP

  DO i=1, n_pop
    IF (in_pop(i)%edefn == ide) THEN
      IF (ELITE(n_elite)%energy(ide) > in_pop(i)%energy(ide)) THEN
        CALL insertIntoPop(ELITE,in_pop(i))
      END IF
    END IF
  END DO

  WRITE(message, *) 'ELITE:'
  CALL printOut("saveElites", message, 0)

  DO i = 1, n_elite
    WRITE(message, *) ELITE(i)%id, ELITE(i)%energy(ide)
    CALL printOut("saveElites", message, 0)
  END DO

  RETURN
END SUBROUTINE saveElites

!==========================================================================================!

SUBROUTINE restartWalker(inout_cluster,out_successful)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    LOGICAL, INTENT(OUT), OPTIONAL :: out_successful
    LOGICAL :: l_old_walker
    INQUIRE(FILE=TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'walker.can',EXIST=l_old_walker)
    IF (l_old_walker) THEN
      CALL printBanner('now reading in the position of the last walker')
      CALL inputCluster(inout_cluster,TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'walker')
    ENDIF
    IF (PRESENT(out_successful)) out_successful = l_old_walker
END SUBROUTINE restartWalker

!==========================================================================================!

SUBROUTINE restartPop(out_cluster,in_file_number)
    TYPE(cluster), INTENT(OUT) :: out_cluster
    INTEGER, INTENT(IN) :: in_file_number
    CALL inputCluster(out_cluster,TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'POP/' &
                                                //TRIM(intToChar(in_file_number)))
END SUBROUTINE restartPop

!==========================================================================================!

SUBROUTINE restartElites
    CHARACTER(LEN=MAX_DIRECTORY_WIDTH) :: filename=''
    INTEGER :: i

    filename = TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'ELITES/'

    DO i = 1, N_ELITE_POP
      CALL inputCluster(ELITE(i),TRIM(filename)//TRIM(intToChar(i)))
      CALL printStringColumns('Seed with previous population in',TRIM(filename)// & 
                                                        TRIM(intToChar(i))//'.can')
    END DO
END SUBROUTINE restartElites

!===============================================================================================!

SUBROUTINE readElites
    INTEGER :: i
    DO i = 1, N_ELITE_POP
      CALL readElite(ELITE(i),i)
    END DO
END SUBROUTINE readElites

!===============================================================================================!

SUBROUTINE readElite(out_cluster,in_counter)
    TYPE(cluster), INTENT(OUT) :: out_cluster
    INTEGER, INTENT(IN), OPTIONAL :: in_counter
    CHARACTER(LEN=MAX_DIRECTORY_WIDTH) :: filename=''
    INTEGER :: filenumber
    filenumber = 0
    IF (PRESENT(in_counter)) filenumber = in_counter
    filename = TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'ELITES/'//TRIM(intToChar(filenumber))
    CALL inputCluster(out_cluster,filename)
END SUBROUTINE readElite

!===============================================================================================!

SUBROUTINE writeWalker(in_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    CALL outputCluster(in_cluster,TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'walker')
END SUBROUTINE writeWalker

!===============================================================================================!

SUBROUTINE writePopulation
    CHARACTER(LEN=MAX_DIRECTORY_WIDTH) :: filename=''
    INTEGER :: i
    filename = TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'POP/'
    DO i = 1, SIZE(POP)
      CALL outputCluster(POP(i),TRIM(filename)//TRIM(intToChar(i)))
    END DO
END SUBROUTINE writePopulation

!===============================================================================================!

SUBROUTINE writeElites
    INTEGER :: i
    DO i = 1, N_ELITE_POP
      CALL writeElite(ELITE(i),i)
    END DO
!   CALL writeElitesTable
END SUBROUTINE writeElites

!===============================================================================================!

SUBROUTINE writeElite(in_cluster,in_counter)
    TYPE(cluster), INTENT(IN) :: in_cluster
    INTEGER, INTENT(IN), OPTIONAL :: in_counter
    CHARACTER(LEN=MAX_DIRECTORY_WIDTH) :: filename=''
    INTEGER :: filenumber
    filenumber = 0
    IF (PRESENT(in_counter)) filenumber = in_counter
    filename = TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'ELITES/'//TRIM(intToChar(filenumber))
    CALL outputCluster(in_cluster,filename)
END SUBROUTINE writeElite

!==========================================================================================!
! A routine to save the current population of GA
!===============================================================================================!

SUBROUTINE writeGAPop(in_pop)
  TYPE(cluster), INTENT(IN), DIMENSION(:) :: in_pop

  INTEGER :: i
  CHARACTER(LEN=MAX_DIRECTORY_WIDTH) :: filename = ''

  CHARACTER(LEN=100) :: outFileMessage = "Generated by KLMC"

  IF (GA_SAVE_POP_FREQ .EQ. 0) RETURN

  filename = TRIM(RELAXED_FOLDER) // 'POP/'
  CALL makeFolder(filename)

  IF (MOD(GA_ITER, GA_SAVE_POP_FREQ) .EQ. 0 ) THEN
    DO i = 1, SIZE(in_pop)
      IF (TRIM(GA_SAVE_POP_OUT_FORMAT) .EQ. 'car') THEN
        CALL outputCAR(in_pop(i), outFileMessage, TRIM(filename) // &
          TRIM(intToChar(i)) // "_" // TRIM(POP(i)%id) // "_" // TRIM(intToChar(POP(i)%edefn)))

      ELSEIF (TRIM(GA_SAVE_POP_OUT_FORMAT) .EQ. 'xyz') THEN
        CALL outputXYZ(in_pop(i), outFileMessage, TRIM(filename) // &
          TRIM(intToChar(i)) // "_" // TRIM(POP(i)%id) // "_" // TRIM(intToChar(POP(i)%edefn)))
      ENDIF
    END DO
  ENDIF

END SUBROUTINE writeGAPop

!===============================================================================================!

SUBROUTINE makeExtinct
    INTEGER :: i
    DO i = 1, SIZE(POP)
      IF (N_SEEDS > 0) THEN
        CALL reportItemDone(i)
        CALL decrement(N_SEEDS)
      ENDIF
    ENDDO
    DEALLOCATE(POP, STAT=ALLOCATE_STAT)
END SUBROUTINE makeExtinct

!==========================================================================================!

END MODULE Population
