MODULE GeneticAlgorithm

  USE Config
  USE Format
  USE Library
  USE ClusterRoutines, ONLY : getEnergy, evaluate,re_evaluate, forwardJump, printHashkeys
  USE Environment,     ONLY : writeRestart
  USE File,            ONLY : displayGULPcounters, peek, printBestSet
  USE Moveclass,       ONLY : crossover, getRandom, mutate
  USE Master,          ONLY : updateTopClusters
  USE Population,      ONLY : POP, ENERGY_MIN, peekIntoPopulation, &
                              removeDuplicates, repopulate, saveElites, sortPopulation, &
                              validateClusters, writeElites, writePopulation, writeGAPop, enforceIniPopSize
  USE UnitCell,        ONLY : rectifyCell
  USE Utilities,       ONLY : getValidClustersLen, wipeGAStats, getHashKey, addStructureToHashKeyLib
  USE TIMER

  IMPLICIT NONE

  INTEGER,ALLOCATABLE,dimension(:) :: SELECTED
  TYPE(cluster), PUBLIC, ALLOCATABLE, DIMENSION(:) :: CHILDREN  

!==========================================================================================!
CONTAINS

! routines for Genetic or Evolutionary Algorithm
!
! subroutines: applyCrossover applyMutation 
!              evolve replacePopulation
!              runGeneticAlgorithm
!              selectionTournament
!
!  functions:  

!==========================================================================================!

  SUBROUTINE runGeneticAlgorithm
    IMPLICIT NONE
    INTEGER :: i
    TYPE(cluster),DIMENSION(1:N_POPULATION) :: local_cluster
    CHARACTER(LEN=MAX_FOLDER_WIDTH) :: TMP_FOLDER

    TMP_FOLDER = RELAXED_FOLDER

    WRITE(stdsee,*) 'Using Genetic Algorithm'
    !Run GULP and/or ab initio code on initial clusters

    DO i = 1, N_POPULATION
       local_cluster(i) = POP(i)
    END DO   

    CALL evaluateInitialPopulation(local_cluster)
    !report fail / success here?

    DO i = 1, N_POPULATION
       CALL rectifyCell(local_cluster(i))
       POP(i) = local_cluster(i)
    END DO

    GA_ITER = 0
    CALL printHeaderCentred('Initial population evaluation completed')
    CALL peekIntoPopulation

    CALL validateClusters(POP) ! remove duff clusters
    CALL removeDuplicates(POP)
    CALL sortPopulation(POP)

    CALL enforceIniPopSize(POP)

    IF (getEnergy(POP(1)) < ENERGY_MIN) ENERGY_MIN=getEnergy(POP(1))
    CALL printHeaderCentred('Lowest cost function in initial population = '//TRIM(realToChar(ENERGY_MIN)))
    CALL printHeaderCentred(' ')

    CALL saveElites(POP)
    CALL updateTopClusters(POP)

    ! Generating GA statistics
    IF (GA_GEN_STATS) THEN
      CALL gaGenerateStatistics(POP)
    ENDIF

    RELAXED_FOLDER = TMP_FOLDER

    CALL evolve ! Perform GA
    CALL writeRestart
    CALL writePopulation
    CALL writeElites


    IF (L_USE_TOP_ANALYSIS .AND. GA_GEN_STATS) THEN
      DO i = 1, N_POPULATION
        IF (POP(i)%edefn /= 0) THEN
          CALL addStructureToHashKeyLib(POP(i))
        ENDIF
      ENDDO
    ENDIF

    IF (L_GULP_RUN) CALL displayGULPcounters

    RETURN
END SUBROUTINE runGeneticAlgorithm

!==========================================================================================!

  SUBROUTINE evaluateInitialPopulation(inout_cluster)
    TYPE(cluster), DIMENSION(:),INTENT(INOUT) :: inout_cluster

    RELAXED_FOLDER = TRIM(RELAXED_FOLDER) // '0/'

    CALL SYSTEM('mkdir '//TRIM(RELAXED_FOLDER))

    CALL evaluate(inout_cluster)

  END SUBROUTINE evaluateInitialPopulation

!==========================================================================================!

  SUBROUTINE evolve
    USE Config
    TYPE(cluster),DIMENSION(1:N_POPULATION) :: in_cluster
    INTEGER :: gen,i
    CHARACTER(LEN=MAX_FOLDER_WIDTH) :: TMP_FOLDER
    CHARACTER(LEN=100) :: message

    ALLOCATE(CHILDREN(1:N_POPULATION), STAT=ALLOCATE_STAT)
    IF (ALLOCATE_STAT /= 0) WRITE(stderr,*) 'Error allocating children array!'
    ALLOCATE(SELECTED(1:N_POPULATION), STAT=ALLOCATE_STAT)
    IF (ALLOCATE_STAT /= 0) WRITE(stderr,*) 'Error allocating selected array!'

    !setup Children arrays
    CHILDREN = POP

    !Start the GA loop over N generation
    TMP_FOLDER = RELAXED_FOLDER

    DO gen = 1, N_GENERATIONS

       GA_ITER = gen

       WRITE(message, *) 'Genetic Algorithm iteration ', gen
       CALL printOut("evolve", message, 0)

       RELAXED_FOLDER = TRIM(TMP_FOLDER)//TRIM(intToChar(gen))//'/'

       CALL SYSTEM('mkdir '//TRIM(RELAXED_FOLDER))

       CALL selectionTournament      ! to fill SELECTED(1:N_POPULATION)

       CALL applyCrossover           ! to fill CHILDREN(1:N_POPULATION)

       CALL applyMutation            ! mutate CHILDREN (change diversity)

       IF (DEBUG_LEVEL > 5) CALL printHeaderCentred('New Children Population Completed')

       DO i = 1, N_POPULATION
          in_cluster(i) = CHILDREN(i)

          ! Initial hashkeys
          IF (L_USE_TOP_ANALYSIS) THEN
            in_cluster(i)%hashkey1st = TRIM(getHashKey(in_cluster(i), hashRadius))
          ENDIF
       END DO   

       CALL re_evaluate(in_cluster)

       DO i = 1, N_POPULATION   
          CALL rectifyCell(in_cluster(i))
          CHILDREN(i) = in_cluster(i)
       END DO

       CALL validateClusters(CHILDREN) ! remove duff children

       CALL saveElites(CHILDREN)

       CALL replacePopulation        ! CHILDREN merged into CURRENT POP 

       IF (DEBUG_LEVEL > 5) THEN
         CALL printHeaderCentred('New Population')
         CALL peekIntoPopulation
       ENDIF
 
       CALL validateClusters(POP,gen)

       CALL sortPopulation(POP)

       IF (MOD(gen,N_SAVE_FREQUENCY)==0) THEN
         CALL writeRestart
         CALL writePopulation
         CALL writeElites
         CALL printHeaderCentred('Population saved on generation'//TRIM(intToChar(gen)))
         CALL peekIntoPopulation ! could also CALL displaySummary
       END IF

       IF (ENERGY_MIN-R_ENERGY_TOLERANCE < R_GLOBAL_MIN_VALUE) THEN
         CALL printHeaderCentred('Global minimum located! Exiting on generation'//TRIM(intToChar(gen)))
         EXIT
       ENDIF

       CALL updateTopClusters(POP)

       ! Generating GA statistics
       IF (GA_GEN_STATS) THEN
         CALL gaGenerateStatistics(POP)
       ENDIF

       ! Saves the current population
       CALL writeGAPop(POP)

       RELAXED_FOLDER = TMP_FOLDER
    END DO


    IF (INT_MAX_BEST > 0) THEN
       CALL printBestSet(0)
    ENDIF

    DEALLOCATE(CHILDREN)
    DEALLOCATE(SELECTED)
    RETURN
END SUBROUTINE evolve

  !==========================================================================================!
  ! Generates statistics for a GA generation
  !==========================================================================================!
  SUBROUTINE gaGenerateStatistics(in_pop, prefix)
    TYPE(cluster), INTENT(IN), DIMENSION(:) :: in_pop
    CHARACTER(LEN=1), INTENT(IN), OPTIONAL :: prefix

    INTEGER :: n, i, out_unit=26, out_error=1
    TYPE(GA_STATS) :: cGaStat
    CHARACTER(LEN=1) :: comma = ','

    IF(PRESENT(prefix)) THEN
      OPEN(UNIT=out_unit, FILE=TRIM(RELAXED_FOLDER)//'gaStatistics'//TRIM(prefix)//'.csv', &
        STATUS='REPLACE', ACTION='WRITE', IOSTAT=out_error, IOMSG=ERROR_MSG)
    ELSE
      OPEN(UNIT=out_unit, FILE=TRIM(RELAXED_FOLDER)//'gaStatistics'// TRIM(intToChar(GA_ITER)) //'.csv', &
        STATUS='REPLACE', ACTION='WRITE', IOSTAT=out_error, IOMSG=ERROR_MSG)
    ENDIF

    n = SIZE(in_pop)

    WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) 'ClusterNo,ClusterID,hashkey,edefn,status,' // &
      'Energy,rdmAttempts,crossAttempts,parent1,parent2,mutateAttempts,mutateSwap,mutateDisplace,gaRepopAttempts,' // &
      'origin,gaGenNo,gaNoOfOccur'

    DO i = 1, n
      cGaStat = in_pop(i)%ga_stats

      WRITE(UNIT=out_unit,FMT='(I5,A1,A5,A1,A27,A1,I5,A1,I5,A1,ES14.7,A1,I5,A1,I5,A1,A10,A1,A10,A1,I5,A1,I5,A1,I5,A1,'// &
            'I5,A1,A6,A1,I5,A1,I5,A1,I5)', IOSTAT=out_error, IOMSG=ERROR_MSG) &
        i, comma, in_pop(i)%id, comma, in_pop(i)%hashkey, comma, in_pop(i)%edefn, comma, in_pop(i)%status, comma, &
        in_pop(i)%energy(in_pop(i)%edefn), comma, cGaStat%gaRandomAttempts, comma, cGaStat%gaCrossAttempts, comma, &
        cGaStat%gaCrossParent1, comma, cGaStat%gaCrossParent2, comma, cGaStat%gaMutateAttempts, comma, &
        cGaStat%gaMutateSwapCnt, comma, cGaStat%gaMutateDisplCnt, comma, cGaStat%gaRepopAttempts, comma, &
        cGaStat%gaOrigin, comma, cGaStat%gaGenNo, comma, cGaStat%gaNoOfOccur
    ENDDO

    CLOSE(out_unit)

    RETURN

  END SUBROUTINE gaGenerateStatistics

  !=====================================================================================!

  SUBROUTINE replacePopulation
    INTEGER :: offset, m, n, o, lenPop, lenChildren

    IF (DEBUG_LEVEL > 5) CALL printHeaderCentred('About to replace Population')
    CALL peekIntoPopulation

    CALL removeDuplicates(CHILDREN) ! mark by setting edefn to zero
!   CALL repopulate(CHILDREN)       ! delay refilling with mutations / mutated elites
    CALL sortPopulation(CHILDREN)   ! valid children ranked at the front of array

    IF (getEnergy(CHILDREN(1)) < ENERGY_MIN) ENERGY_MIN=getEnergy(CHILDREN(1))

    IF(R_POP_REPLACEMENT_RATIO>0.95) THEN  ! 100% replacement
      POP = CHILDREN
    ELSE
!      CALL removeDuplicates(POP)   ! mark by setting edefn to zero
!      CALL repopulate(POP)         ! delay refilling with mutations / mutated elites

      CALL sortPopulation(POP)     ! valid adults ranked at the front of array

      lenChildren = getValidClustersLen(CHILDREN)
      lenPop      = getValidClustersLen(POP)

      offset = INT(R_POP_REPLACEMENT_RATIO * N_POPULATION)

      m = MIN(offset, lenChildren)
      n = m + lenPop

      IF (n .LE. N_POPULATION) THEN
        POP(lenPop+1:n) = CHILDREN(1:m)

      ELSE
        o = N_POPULATION - m + 1

        POP(o:N_POPULATION) = CHILDREN(1:m)
      ENDIF
    END IF

    IF (DEBUG_LEVEL > 5) CALL printHeaderCentred('Inital replacement of Population completed')
    CALL peekIntoPopulation

    CALL sortPopulation(POP)

    CALL removeDuplicates(POP)

    CALL repopulate(POP)

    CALL sortPopulation(POP)

    CALL removeDuplicates(POP)

    CALL sortPopulation(POP)

    IF (DEBUG_LEVEL > 5) CALL printHeaderCentred('replace Population completed')
    CALL peekIntoPopulation

    IF (getEnergy(POP(1)) < ENERGY_MIN) ENERGY_MIN=getEnergy(POP(1))

END SUBROUTINE replacePopulation

!=====================================================================================!
! deterministic tournament ie. selection pressure probability=1
SUBROUTINE selectionTournament
  INTEGER :: champion
  INTEGER :: x, y, contender, actualPopLen

  actualPopLen = getValidClustersLen(POP)

  DO x = 1, actualPopLen
    champion = x

    DO y=2, N_TOURNAMENT_SIZE
      contender = getRandom(1, actualPopLen)

      IF (forwardJump(POP(contender),POP(champion)) > ZERO) THEN
        champion = contender
      ENDIF
    END DO

    SELECTED(x) = champion

  END DO

  RETURN
END SUBROUTINE selectionTournament

!=====================================================================================!

SUBROUTINE applyCrossover
  INTEGER :: x, actualPopLen

  actualPopLen = getValidClustersLen(POP)

  DO x = 1, actualPopLen

    CALL increment(N_INDIVIDUALS)

    CHILDREN(x) = crossover(POP(SELECTED(x)), POP(getRandom(1, actualPopLen)), &
                           GA_CROSS_CHECK, GA_CROSS_ATTEMPTS)

    IF (((JOB_TYPE == 2) .OR. (JOB_TYPE == 9)) .AND. GA_GEN_STATS) THEN
      CHILDREN(x)%ga_stats%gaOrigin = "CROSSO"
      CHILDREN(x)%ga_stats%gaGenNo = GA_ITER
    ENDIF

    CHILDREN(x)%id = INITIAL_PREFIX // TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))
  END DO
END SUBROUTINE applyCrossover

!=====================================================================================!

SUBROUTINE applyMutation
  INTEGER :: x, actualPopLen
  REAL(KIND=DBL) :: performMutation, performSelfcrossover

  CHARACTER(LEN=10) :: tempId

  actualPopLen = getValidClustersLen(POP)

  DO x = 1, actualPopLen
    !CALL increment(N_INDIVIDUALS) is applied before in applyCrossover

    IF (CHILDREN(x)%status .EQ. 0) THEN
      CYCLE
    ENDIF

    CALL RANDOM_NUMBER(performMutation)

    IF (performMutation < GA_MUTATION_RATIO) THEN
      CALL RANDOM_NUMBER(performSelfcrossover)

      IF (performSelfcrossover < GA_MUT_SELFCROSS_RATIO) THEN
        tempId = CHILDREN(x)%id
        CHILDREN(x) = crossover(CHILDREN(x), CHILDREN(x), GA_CROSS_CHECK, GA_CROSS_ATTEMPTS)
        CHILDREN(x)%id = tempId

        IF (((JOB_TYPE == 2) .OR. (JOB_TYPE == 9)) .AND. GA_GEN_STATS) THEN
          CHILDREN(x)%ga_stats%gaOrigin = "MUTCRS"
          CHILDREN(x)%ga_stats%gaGenNo = GA_ITER
        ENDIF
      ELSE
        CHILDREN(x) = mutate(CHILDREN(x))

        IF (((JOB_TYPE == 2) .OR. (JOB_TYPE == 9)) .AND. GA_GEN_STATS) THEN
          CHILDREN(x)%ga_stats%gaOrigin = "MUTATE"
          CHILDREN(x)%ga_stats%gaGenNo = GA_ITER
        ENDIF
      ENDIF
    ENDIF

  END DO
END SUBROUTINE applyMutation

END MODULE GeneticAlgorithm
