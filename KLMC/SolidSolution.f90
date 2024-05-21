MODULE SolidSolutions

    USE Config
    USE Format
    USE Library
    USE ClusterRoutines, ONLY : getenergy, evaluate, clusterGeometricalCheck
    USE Environment,     ONLY : writeRestart
    USE File,            ONLY : makeFolder, logEnergies, logMessage, logSuccess, printBestSet, &
                                printTrdf, removeClusterFiles, removeArcFiles, outputXYZ_SS, readXYZ_SS, &
                                removeClusterFilesX, generateGulpInput
    USE Master,          ONLY : removeKeyword, updateBestSet
    USE MonteCarlo,      ONLY : metropolis
    USE Moveclass,       ONLY : mixSolution, randomiseSolution
    USE Population,      ONLY : ENERGY_MIN, MASTER_CLUSTER, initialiseCluster, restartWalker, writeWalker
    USE RdfRoutines,     ONLY : computeRDF, initialiseRDF, initialiseTrdf, resetRDF, restartTrdf, updateTrdf
    USE Utilities,       ONLY : getHashKey, getHashkeyRadius, findHashKeyMatch, getCountOfFiles, getListOfFiles
    USE TIMER

    IMPLICIT NONE

    SAVE
    TYPE(Trdf), PUBLIC, ALLOCATABLE, DIMENSION(:) :: TRDF_DATA  !Total RDF data
    TYPE(rdf), PUBLIC, ALLOCATABLE, DIMENSION(:) :: RDF_NEW     !RDF data

CONTAINS


  !==========================================================================================
  ! A routine to initialize solid solutions module
  !==========================================================================================
  SUBROUTINE SSInitialize(ssPrevHashkeysOccur)
    INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: ssPrevHashkeysOccur

    INTEGER :: cntFiles, cntFiles2, i
    CHARACTER(LEN=MAX_DIRECTORY_WIDTH), ALLOCATABLE, DIMENSION(:) :: files
    LOGICAL :: statsImport

    SS_UNIQUE_CNT = 0

    IF (L_USE_TOP_ANALYSIS) THEN
      ALLOCATE(GEN_HASHKEYS(N_MAX_BH_STEPS))
      GEN_HASHKEYS_CNT = 0
    ENDIF

    IF (SS_IMPORT_LIB) THEN
      ! Check whether the statistics file exists

      statsImport = SSGetPrevHashkeysFromStats(SS_LIB_DIR, TRIM(SS_HASHKEYS_STATS_FILE) // '.csv', ssPrevHashkeysOccur)

      IF (.NOT. statsImport) THEN
        ! Looking for structures from the previous run
        CALL getCountOfFiles(SS_LIB_DIR, SS_FILEIN_PREFIX, SS_FILEIN_SUFFIX, cntFiles)

        ALLOCATE(PREV_HASHKEYS(cntFiles))
        ALLOCATE(files(cntFiles))

        ! Reading the files from the previous run
        CALL getListOfFiles(files, SS_LIB_DIR, SS_FILEIN_PREFIX, SS_FILEIN_SUFFIX, cntFiles2)

        ! Getting the hashkeys from the previous run
        CALL SSGetPrevHashkeys(SS_LIB_DIR, files, cntFiles2)

        IF (SS_STATS) THEN
          ALLOCATE(ssPrevHashkeysOccur(PREV_HASHKEYS_CNT))

          DO i = 1, PREV_HASHKEYS_CNT
            ssPrevHashkeysOccur(i) = 1
          ENDDO
        ENDIF

        DEALLOCATE(files)
      ENDIF
    ELSE
      ! Well, this is not great solution, but for now it should do
      ALLOCATE(ssPrevHashkeysOccur(0))
    ENDIF

  END SUBROUTINE SSInitialize

  !==========================================================================================
  ! A routine to finilize solid solutions module
  !==========================================================================================
  SUBROUTINE SSFinilize()
    IF (L_USE_TOP_ANALYSIS) THEN
      DEALLOCATE(GEN_HASHKEYS)
    ENDIF

    IF (SS_IMPORT_LIB) THEN
      DEALLOCATE(PREV_HASHKEYS)
    ENDIF
  END SUBROUTINE SSFinilize

  !==========================================================================================
  ! A routine to get the hashkeys of the structures from the previous SS runs
  !==========================================================================================
  SUBROUTINE SSGetPrevHashkeys(directory, files, filesCnt)
    CHARACTER(LEN=*),               INTENT(IN) :: directory
    CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: files
    INTEGER,                        INTENT(IN) :: filesCnt

    INTEGER :: i
    CHARACTER(LEN=MAX_DIRECTORY_WIDTH) :: filePath
    TYPE(cluster) :: ssCluster
    REAL(KIND=DP) :: hashRadius
    CHARACTER(LEN=N_HASH_LEN) :: hashKey

    DO i = 1, filesCnt
      filePath = TRIM(directory) // TRIM(files(i))

      ! Initialising default structure
      ssCluster = MASTER_CLUSTER
      CALL initialiseCluster(ssCluster)

      ! Reading in the cluster
      CALL readXYZ_SS(ssCluster, filePath)

      ! Getting the hashkey
      hashRadius = getHashkeyRadius(ssCluster, N_ATOMS) + HASHKEY_RADIUS_CONST
      hashKey = getHashKey(ssCluster, hashRadius)

      PREV_HASHKEYS_CNT = PREV_HASHKEYS_CNT + 1
      PREV_HASHKEYS(PREV_HASHKEYS_CNT) = hashKey
    ENDDO

  END SUBROUTINE SSGetPrevHashkeys

  !==========================================================================================
  ! A function to get the hashkeys of the structures from the previous SS runs form the statistics file
  !==========================================================================================
  LOGICAL FUNCTION SSGetPrevHashkeysFromStats(directory, file, ssPrevHashkeysOccur)
    CHARACTER(LEN=*), INTENT(IN) :: directory, file
    INTEGER, ALLOCATABLE, DIMENSION(:), INTENT(INOUT) :: ssPrevHashkeysOccur

    CHARACTER(LEN=MAX_DIRECTORY_WIDTH) :: filePath
    LOGICAL :: readSuccess
    INTEGER :: in_error=1, in_unit=26, countHashkeys, lineCnt
    CHARACTER(LEN=200) :: line
    CHARACTER(LEN=1) :: tmp1, tmp2, tmp3
    INTEGER :: hashkeyCnt
    CHARACTER(LEN=N_HASH_LEN) :: hashKey


    countHashkeys = 0
    readSuccess = .FALSE.
    filePath = TRIM(directory) // TRIM(file)

    ! Getting the number of hashkeys in the file
    OPEN(UNIT=in_unit, FILE=TRIM(filePath), STATUS='OLD', IOSTAT=in_error, IOMSG=ERROR_MSG)
    IF(in_error == 0) THEN
      DO
        READ (in_unit, FMT='(A200)', iostat=in_error) line

        IF ( in_error /= 0 ) THEN
          CLOSE(in_unit)
          EXIT
        ENDIF

        line = TRIM(line)

        IF (len(TRIM(line)) .GT. 0) THEN
          countHashkeys = countHashkeys + 1
        ENDIF

      END DO
    ELSE
      SSGetPrevHashkeysFromStats = readSuccess
      RETURN
    END IF

    CLOSE(in_unit)

    ! removing the header line
    countHashkeys = countHashkeys - 1

    IF (countHashkeys .LE. 0) THEN
      SSGetPrevHashkeysFromStats = readSuccess
      RETURN
    ENDIF

    ! reading the hashkeys
    ALLOCATE(PREV_HASHKEYS(countHashkeys))

    IF (SS_STATS) THEN
      ALLOCATE(ssPrevHashkeysOccur(countHashkeys))
    ENDIF

    countHashkeys = 0
    OPEN(UNIT=in_unit, FILE=TRIM(filePath), STATUS='OLD', IOSTAT=in_error, IOMSG=ERROR_MSG)
    IF(in_error == 0) THEN
      DO
        READ (in_unit, FMT='(A200)', iostat=in_error) line

        IF ( in_error /= 0 ) THEN
          CLOSE(in_unit)
          EXIT
        ENDIF

        line = TRIM(line)
        IF (len(TRIM(line)) .GT. 0) THEN
          countHashkeys = countHashkeys + 1
        ENDIF

        IF (countHashkeys > 1) THEN
          READ (line, FMT='(A1,A1,A27,A1,I5)', iostat=in_error) tmp1, tmp2, hashKey, tmp3, hashkeyCnt

          PREV_HASHKEYS(countHashkeys-1) = hashKey
          ssPrevHashkeysOccur(countHashkeys-1) = hashkeyCnt
          PREV_HASHKEYS_CNT = countHashkeys-1
        ENDIF

      END DO
    ELSE
      SSGetPrevHashkeysFromStats = readSuccess
      RETURN
    END IF

    CLOSE(in_unit)

    SSGetPrevHashkeysFromStats = .TRUE.

  END FUNCTION SSGetPrevHashkeysFromStats

  !==========================================================================================
  ! Generates a statistics file at the of solid solutions run
  !==========================================================================================
  SUBROUTINE SSGenStats(ssIDs, ssEnergies, ssHashkeysIDs, ssHashkeysOccur, ssPrevHashkeysOccur, prefix)
    CHARACTER(LEN=10), ALLOCATABLE, DIMENSION(:), INTENT(IN) :: ssIDs
    REAL(KIND=DBL), ALLOCATABLE, DIMENSION(:),    INTENT(IN) :: ssEnergies
    INTEGER, ALLOCATABLE, DIMENSION(:),           INTENT(IN) :: ssHashkeysIDs, ssHashkeysOccur, ssPrevHashkeysOccur
    CHARACTER(LEN=*), OPTIONAL,                   INTENT(IN) :: prefix

    INTEGER :: i, out_unit=26, out_error=1
    CHARACTER(LEN=1) :: comma = ','
    CHARACTER(LEN=100) :: fileName

    ! Printing out statistics with respect to clusters
    IF (PRESENT(prefix)) THEN
      fileName = 'ssStatistics' // TRIM(prefix) // '.csv'
    ELSE
      fileName = 'ssStatistics.csv'
    ENDIF

    OPEN(UNIT=out_unit, FILE=TRIM(RELAXED_FOLDER) // TRIM(fileName), &
        STATUS='REPLACE', ACTION='WRITE', IOSTAT=out_error, IOMSG=ERROR_MSG)

    IF(out_error == 0) THEN
      IF (L_USE_TOP_ANALYSIS) THEN
        WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) 'ClusterNo,ClusterID,Energy,Hashkey'
      ELSE
        WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) 'ClusterNo,ClusterID,Energy'
      ENDIF

      ! Saving statistics with respect to newly generated clusters (hashkeys)
      DO i = 1, GEN_HASHKEYS_CNT
        IF (L_USE_TOP_ANALYSIS) THEN
          WRITE(UNIT=out_unit,FMT='(I5,A1,A5,A1,ES14.7,A1,A27)', IOSTAT=out_error, IOMSG=ERROR_MSG) &
            i, comma, ssIDs(i), comma, ssEnergies(i), comma, GEN_HASHKEYS(ssHashkeysIDs(i))
        ELSE
          WRITE(UNIT=out_unit,FMT='(I5,A1,A5,A1,ES14.7)', IOSTAT=out_error, IOMSG=ERROR_MSG) &
            i, comma, ssIDs(i), comma, ssEnergies(i)
        ENDIF

        IF (out_error .NE. 0) THEN
          CLOSE(out_unit)
          CALL print2All('SSGenStats', ERROR_MSG, 0)
          EXIT
        ENDIF
      ENDDO
    ELSE
      CALL print2All('SSGenStats', ERROR_MSG, 0)
    ENDIF

    CLOSE(out_unit)

    ! Printing out statistics with respect to hashkeys
    IF (L_USE_TOP_ANALYSIS) THEN
      IF (PRESENT(prefix)) THEN
        fileName = TRIM(SS_HASHKEYS_STATS_FILE) // TRIM(prefix) // '.csv'
      ELSE
        fileName = TRIM(SS_HASHKEYS_STATS_FILE) // '.csv'
      ENDIF

      OPEN(UNIT=out_unit, FILE=TRIM(RELAXED_FOLDER) // TRIM(fileName), &
        STATUS='REPLACE', ACTION='WRITE', IOSTAT=out_error, IOMSG=ERROR_MSG)

      IF(out_error == 0) THEN
        WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) 'TYPE,Hashkey,Occurance'

        ! Previously found hashkeys
        DO i = 1, PREV_HASHKEYS_CNT
          WRITE(UNIT=out_unit,FMT='(A1,A1,A27,A1,I5)', IOSTAT=out_error, IOMSG=ERROR_MSG) &
            'P', comma, PREV_HASHKEYS(i), comma, ssPrevHashkeysOccur(i)
        ENDDO

        ! Newly found hashkeys
        DO i = 1, GEN_HASHKEYS_CNT
          WRITE(UNIT=out_unit,FMT='(A1,A1,A27,A1,I5)', IOSTAT=out_error, IOMSG=ERROR_MSG) &
            'N', comma, GEN_HASHKEYS(i), comma, ssHashkeysOccur(i)
        ENDDO
      ELSE
        CALL print2All('SSGenStats', ERROR_MSG, 0)
      ENDIF

      CLOSE(out_unit)
    ENDIF

  END SUBROUTINE SSGenStats

  !==========================================================================================
  !
  !==========================================================================================
  SUBROUTINE runSolidSolutions
    INTEGER :: step=1, bad_steps=0, no_GM_update=0, n_exchanges, ide
    REAL(KIND=DBL) :: energy_new, energy_old, energy_low
    REAL(KIND=DBL) :: r_random=0., diff=0.
    REAL(KIND=DBL) :: kB = 1.380658E-23 / 1.60217733E-19
    TYPE(cluster),DIMENSION(1) :: cluster_old(1), cluster_new(1)
    CHARACTER(LEN=200) :: message=''
    CHARACTER(LEN=60) :: filename=''
    CHARACTER(LEN=20) :: MC_TYPE=''
    LOGICAL :: reject, accept=.FALSE., pbc, old_walker_found=.FALSE.
    REAL(KIND=DBL) :: kT, zero=0.0_DP, LOCAL_SIGMA
    REAL(KIND=DP) :: hashRadius
    CHARACTER(LEN=N_HASH_LEN) :: hashKey
    INTEGER :: duplicates, hashKeyIdx, i, badGeometry

    REAL(KIND=DBL), ALLOCATABLE, DIMENSION(:) :: ssEnergies
    CHARACTER(LEN=10), ALLOCATABLE, DIMENSION(:) :: ssIDs
    INTEGER, ALLOCATABLE, DIMENSION(:) :: ssHashkeysIDs, ssHashkeysOccur, ssPrevHashkeysOccur

    CALL SSInitialize(ssPrevHashkeysOccur)

    IF (SS_STATS) THEN
      ALLOCATE(ssEnergies(N_MAX_BH_STEPS))
      ALLOCATE(ssIDs(N_MAX_BH_STEPS))
      ALLOCATE(ssHashkeysIDs(N_MAX_BH_STEPS))

      ALLOCATE(ssHashkeysOccur(N_MAX_BH_STEPS))

      DO i = 1, N_MAX_BH_STEPS
        ssHashkeysOccur(i) = 0
      ENDDO
    ENDIF

    duplicates = 0
    badGeometry = 0

    ide = 0
    energy_low = R_ENERGY_ZERO           ! Best energy found on this current run 
    energy_old = R_ENERGY_ZERO           ! Energy found from last accepted step
    energy_new = R_ENERGY_ZERO           ! Energy of the newly created configuration
    ENERGY_MIN = R_BEST_ENERGIES(1)      ! Energy global minimum 
    kT = R_TEMPERATURE * kB              ! Save time and do multiplication now

    pbc=(index(UNIT_CELL,'cell').ne.0.OR.index(UNIT_CELL,'vector').ne.0)
    LOCAL_SIGMA = R_RDF_SIGMA

    !   L_ENERGY_ONLY=.TRUE. !retrieve only energy unless within computeRDF
    IF (BH_METHOD == 'FIXED') THEN       ! RANDOM CONTINUOUS HOPPING WITHOUT LOCAL OPT
       L_ENERGY_ONLY=.TRUE.
       L_GULP_OPTI=.FALSE.
       CALL removeKeyword('opti')         ! Ensure no relaxation when calling GULP
    ENDIF

    IF (L_COMPUTE_RDF) THEN
       ALLOCATE(RDF_NEW(1))
       ALLOCATE(TRDF_DATA(2))             ! Second element for gnuplot data
    ENDIF

    L_RESTART = (index(ID_BEST(1),'x').eq.0)
    IF (L_COMPUTE_RDF .AND. L_RESTART) THEN
       CALL initialiseRDF(RDF_NEW(1))
       R_RDF_SIGMA = zero
       CALL restartTrdf(RDF_NEW(1),TRDF_DATA(1))
       R_RDF_SIGMA = LOCAL_SIGMA
       CALL restartTrdf(RDF_NEW(1),TRDF_DATA(2))
    ENDIF

    IF (L_RESTART .AND. L_COMPUTE_RDF .AND. N_MAX_BH_STEPS == 0) THEN !mcsteps
       ! do not add anymore statistics to RDF data and jump to end of subroutine
    ELSE !mcsteps
      cluster_old(1) = MASTER_CLUSTER         ! initial solid solution taken from Master File

      reject = .TRUE.
      i = 0

      CALL initialiseCluster(cluster_new(1))  ! set defaults for next solid solution


      IF (L_RANDOM_START) THEN
        step=0
        CALL logMessage(step,zero,' Randomising start')
        CALL increment(N_INDIVIDUALS)    ! id for new configuration (could comment out this call)

        IF (SS_CHECK_GEOMETRY) THEN
          reject = .TRUE.

          DO WHILE ((reject) .AND. (i < N_ATTEMPTS_RNDM_STRUCT))
            CALL randomiseSolution(cluster_old(1), cluster_new(1))
            cluster_old(1) = cluster_new(1)
            reject = clusterGeometricalCheck(cluster_new(1))
            i = i + 1
          ENDDO

          IF (reject) THEN
            WRITE(message, *) 'Cannot create initial structure by ', N_ATTEMPTS_RNDM_STRUCT, 'attempts.'
            CALL print2All("runSolidSolutions", message, 10)
            STOP
          ENDIF
        ELSE
          CALL randomiseSolution(cluster_old(1), cluster_new(1))
          reject = .FALSE.
        ENDIF

      ELSEIF (RESTART_FOLDER /= '') THEN
         CALL restartWalker(cluster_old(1),old_walker_found)
      ENDIF

      ! If SS_SKIP_EVALUATION flag is on, energy is ignored but we still need output files from GULP
      IF (.NOT. SS_SKIP_EVALUATION) THEN
        CALL evaluate(cluster_old)
        energy_old = getEnergy(cluster_old(1))
        IF (energy_old < energy_low) energy_low = energy_old
      ELSE
        IF (.NOT. SS_NO_FILES) THEN
          CALL generateGulpInput(0, cluster_old(1), TRIM(cluster_old(1)%id))
        ENDIF
        energy_old = R_ENERGY_ZERO
      ENDIF

      IF (((.NOT.L_RANDOM_START).AND.L_RESTART).OR.(old_walker_found)) THEN
         ! do not update and therefore bias statistics with Master file

      ELSEIF (INT_MAX_BEST > 0 .AND. energy_old < R_BEST_EMAX) THEN
         ! Keep a record (hard copy) of the best configurations found
         CALL updateBestSet(cluster_old(1))
      ENDIF

      IF (L_COMPUTE_RDF .AND. .NOT. L_RESTART) THEN
         CALL initialiseRDF(RDF_NEW(1))
         CALL computeRDF(RDF_NEW(1),TRIM(RELAXED_FOLDER)//TRIM(cluster_old(1)%id))
         CALL initialiseTrdf(RDF_NEW(1),TRDF_DATA(1))
         CALL initialiseTrdf(RDF_NEW(1),TRDF_DATA(2))
      ENDIF

      IF (L_COMPUTE_RDF .AND. L_RANDOM_START) THEN
         CALL resetRDF(RDF_NEW(1))
         CALL computeRDF(RDF_NEW(1),TRIM(RELAXED_FOLDER)//TRIM(cluster_new(1)%id))
         R_RDF_SIGMA = zero
         CALL updateTrdf(RDF_NEW(1),TRDF_DATA(1))
         R_RDF_SIGMA = LOCAL_SIGMA
         CALL updateTrdf(RDF_NEW(1),TRDF_DATA(2))
      ENDIF

      CALL printHeaderLine
      IF (old_walker_found) THEN
         CALL printBanner('now starting from PREVIOUS solid solution: E ='//realToChar(energy_old))
      ELSEIF (energy_old < ENERGY_MIN) THEN
         ENERGY_MIN = energy_low
         CALL printBanner('now starting from NEW GM solid solution: E ='//realToChar(energy_old))
      ELSE
         CALL printBanner('now starting FIRST solid solution from E ='//realToChar(energy_old))
      ENDIF

      IF (RESTART_FOLDER /= '') THEN ! open restart folder
         CALL makeFolder(TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER))
      ENDIF

      IF (L_USE_TOP_ANALYSIS) THEN
        hashRadius = getHashkeyRadius(cluster_new(1), N_ATOMS) + HASHKEY_RADIUS_CONST

        WRITE(message, *) 'Hashkey radius = ', hashRadius
        CALL printLog("runSolidSolutions", message, 10)
      ENDIF

      mcsteps: DO step=1, N_MAX_BH_STEPS

         WRITE(message, *) 'Starting step ', TRIM(intToChar(step))
         CALL printOut("runSolidSolutions", message, 0)
         CALL printLog("runSolidSolutions", message, 0)

         CALL increment(N_INDIVIDUALS) ! id for new configuration

         ! Choose moveclass
         IF (BH_ACCEPT == 'RANDOMISE') THEN !randomise cluster
           WRITE(message, *) 'Step ', TRIM(intToChar(step)), ' Randomising solution.'
           CALL printLog("runSolidSolutions", message, 10)

           MC_TYPE='Randomised    '
           CALL randomiseSolution(MASTER_CLUSTER, cluster_new(1))

         ELSE
           MC_TYPE='Solution mixed'
           CALL logMessage(step,energy_new,' Mixing solution 1')
           n_exchanges=(randomInteger(1,N_MC_EXCHANGE))
           CALL mixSolution(n_exchanges,cluster_old(1), cluster_new(1))
         END IF

         ! Checking geometry
         IF (SS_CHECK_GEOMETRY) THEN
           reject = clusterGeometricalCheck(cluster_new(1))

           IF (reject) THEN
             badGeometry = badGeometry + 1
           ENDIF
         ELSE
           reject = .FALSE.
         ENDIF

         ! Check here the initial configuration using the hashkey
         IF (.NOT. reject .AND. (L_USE_TOP_ANALYSIS)) THEN
           hashKey = getHashKey(cluster_new(1), hashRadius)

           ! Checking whether the structure has been previously generated (from previous run)
           hashKeyIdx = findHashKeyMatch(PREV_HASHKEYS, PREV_HASHKEYS_CNT, hashKey)
           IF (hashKeyIdx .GE. 1) THEN
             reject = .TRUE.

             WRITE(message, *) 'Step ', TRIM(intToChar(step)), ' #key: ', TRIM(hashKey), &
              ' has been found before in the previous run. SKIPPING the structure.'
             CALL printLog("runSolidSolutions", message, 5)

             duplicates = duplicates + 1

             ssPrevHashkeysOccur(hashKeyIdx) = ssPrevHashkeysOccur(hashKeyIdx) + 1
           ENDIF

           IF (.NOT. reject) THEN
             hashKeyIdx = findHashKeyMatch(GEN_HASHKEYS, GEN_HASHKEYS_CNT, hashKey)

             ! Checking whether the structure has been previously generated
             IF (hashKeyIdx .GE. 1) THEN
               reject = .TRUE.

               WRITE(message, *) 'Step ', TRIM(intToChar(step)), ' #key: ', TRIM(hashKey), &
                ' has been found before. SKIPPING the structure.'
               CALL printLog("runSolidSolutions", message, 5)

               duplicates = duplicates + 1
               ssHashkeysIDs(step) = hashKeyIdx
               ssHashkeysOccur(hashKeyIdx) = ssHashkeysOccur(hashKeyIdx) + 1
             ELSE

               GEN_HASHKEYS_CNT = GEN_HASHKEYS_CNT + 1
               GEN_HASHKEYS(GEN_HASHKEYS_CNT) = hashKey

               IF (SS_STATS) THEN
                 ssHashkeysIDs(step) = GEN_HASHKEYS_CNT
                 ssHashkeysOccur(GEN_HASHKEYS_CNT) = 1
               ENDIF
             ENDIF
           ENDIF
         ENDIF

         ! Evaluate energy
         IF ((.NOT. reject)) THEN
           IF (.NOT. SS_SKIP_EVALUATION) THEN
             WRITE(message, *) 'Step ', TRIM(intToChar(step)), ' Evaluating.'
           ELSE
             WRITE(message, *) 'Step ', TRIM(intToChar(step)), ' Generating output files.'
           ENDIF

           CALL printLog("runSolidSolutions", message, 5)

           IF (.NOT. SS_SKIP_EVALUATION) THEN
             CALL evaluate(cluster_new)
             reject = (cluster_new(1)%edefn == 0)
           ELSE
             IF (.NOT. SS_NO_FILES) THEN
               CALL generateGulpInput(0, cluster_new(1), TRIM(cluster_new(1)%id))
             ENDIF
           ENDIF
         ENDIF

         IF (reject) THEN ! Reject cluster as energy not defined
           WRITE(message, *) 'Step ', TRIM(intToChar(step)), ' Energy not defined!'
           CALL printLog("runSolidSolutions", message, 10)

           IF (SS_STATS) THEN
             ssEnergies(step) = 0.0
             ssIDs(step) = cluster_new(1)%id
           ENDIF
         ELSE

           IF (.NOT. SS_SKIP_EVALUATION) THEN
             energy_new = getEnergy(cluster_new(1))
           ELSE
             energy_new = R_ENERGY_ZERO
           ENDIF

           IF (SS_STATS) THEN
             ssEnergies(step) = energy_new
             ssIDs(step) = cluster_new(1)%id
           ENDIF

           WRITE(message, *) 'Step ', TRIM(intToChar(step)), ' E = ', energy_new
           CALL printLog("runSolidSolutions", message, 10)

      !        Now perform checks on suitability of cluster
      !        IF (L_GULP_OPTI .AND. collapsed(cluster_new(1),R_COLLAPSE)) THEN   ! Reject cluster as it has collapsed
      !          CALL removeClusterFiles(cluster_new(1))
      !          IF(pbc)CALL removeArcFiles(cluster_new(1))
      !          CALL logMessage(step,energy_new,' Cluster has collapsed! ') ! rejects if a solid (fractionals!!)
      !          reject=.true.
      !        ELSEIF (L_GULP_OPTI .AND. dspecies(cluster_new(1),R_SPECIES)) THEN ! Reject as two like-charged species too close
      !          CALL removeClusterFiles(cluster_new(1))
      !          IF(pbc)CALL removeArcFiles(cluster_new(1))
      !          CALL logMessage(step,energy_new,' Transfer of electrons! ')
      !          reject=.true.
      !        ENDIF
         ENDIF

         IF (reject) THEN
           energy_new = R_ENERGY_ZERO
           bad_steps = bad_steps + 1
           no_GM_update = no_GM_update + 1

           ! Delete files of failed attempts
           IF (SS_DELETE_FAILED) THEN
             CALL removeClusterFilesX(cluster_new(1))
           ENDIF

         ELSE !
           diff = getEnergy(cluster_old(1),0) - getEnergy(cluster_new(1),0) ! includes possible c-s offset

           IF (L_COMPUTE_RDF) THEN
             CALL resetRDF(RDF_NEW(1))
             CALL computeRDF(RDF_NEW(1), TRIM(RELAXED_FOLDER)//TRIM(cluster_new(1)%id))
             R_RDF_SIGMA = zero
             CALL updateTrdf(RDF_NEW(1), TRDF_DATA(1))
             R_RDF_SIGMA = LOCAL_SIGMA
             CALL updateTrdf(RDF_NEW(1), TRDF_DATA(2))
           ENDIF

      !        Keep a record (hard copy) of the best configurations found
           IF (INT_MAX_BEST > 0 .AND. energy_new < R_BEST_EMAX) THEN
             CALL updateBestSet(cluster_new(1))
           ENDIF

      !        Report good news or tidy up hard disk
           IF (energy_new < energy_low) THEN
             CALL logMessage(step, energy_new,'Lower energy solution found')
             CALL logSuccess(step, energy_new, MC_TYPE, R_BH_STEPSIZE)
             energy_low = energy_new

           ! remove hard copy of current configuration to reduce disk usage
           ELSEIF (SS_KEEP_ONLY_LOW .AND. (.NOT. SS_SKIP_EVALUATION)) THEN
             CALL removeClusterFiles(cluster_new(1))
             IF (pbc) CALL removeArcFiles(cluster_new(1))
           ENDIF

           IF (BH_ACCEPT == 'METROPOLIS') THEN
             accept = metropolis(cluster_new(1)%id, diff, kT)
           ELSE
             IF (SS_SKIP_EVALUATION) THEN
               accept = .TRUE.
             ELSE
               accept = diff > 0.0000000
             ENDIF
           END IF

           ! If step accepted then old cluster has a new location on landscape
           IF ((accept .AND. ABS(diff) > 0.000000000001) .OR. SS_SKIP_EVALUATION) THEN
             cluster_old(1) = cluster_new(1)
             energy_old = energy_new
             IF (RESTART_FOLDER /= '') THEN
               CALL writeWalker(cluster_old(1))
             ENDIF
           ELSE
             bad_steps = bad_steps + 1 ! update the counter for unsuccessful steps
           END IF

           no_GM_update = no_GM_update + 1
           IF (energy_old < ENERGY_MIN) THEN
             ENERGY_MIN = energy_old
             CALL printBanner('New global minimum E ='//TRIM(realToChar(ENERGY_MIN))// &
                                   ' after '//TRIM(intToChar(no_GM_update))//' step(s)')
             no_GM_update = 0 ! reset counter for number of bad consecutive steps
           ENDIF
         END IF

         IF (.NOT. SS_SKIP_EVALUATION) THEN
           CALL logEnergies(cluster_new(1), energy_old, ENERGY_MIN)
         ENDIF

         ! Change temperature?
         !IF (BH_METHOD == 'OSCIL') THEN
         !    IF (BH_ACCEPT == 'METROPOLIS') THEN
         !      IF ( mod(step,N_STEPS_HIGH_T) == 0 )THEN
         !        write(*,*)'Starting the downhill search after step ',step,energy_old
         !        filename = TRIM(RELAXED_FOLDER)//'MC-HIGH-'//TRIM(intToChar(step))
         !        message = 'Low  Point'
         !        CALL outputXYZ(cluster_old(1), message, filename)
         !        BH_ACCEPT = 'QUENCH'
         !      ENDIF
         !    ELSE ! (BH_ACCEPT == 'QUENCH') THEN
         !      IF ( mod(step,N_STEPS_LOW_T) == 0 )THEN
         !        write(*,*)'Turning back up the heat after step ',step,energy_old
         !        filename = TRIM(RELAXED_FOLDER)//'MC-LOW-'//TRIM(intToChar(step))
         !        message = 'High Point'
         !        CALL outputXYZ(cluster_old(1), message, filename)
         !        BH_ACCEPT = 'METROPOLIS'
         !      ENDIF
         !    ENDIF
         !ELSEIF (BH_METHOD == 'FIXED') THEN
         !    IF ( mod(step,N_TEMPERATURE) == 0 )THEN
         !        write(*,*)'Temperature change ',step,kT/kB,energy_old
         !        filename = TRIM(RELAXED_FOLDER)//'MC-TEMP-'//TRIM(intToChar(step))
         !        message = 'T  Changed'
         !        CALL outputXYZ(cluster_old(1), message, filename)
         !        kT = kT * SA_TEMP_SCALE
         !    ENDIF
         !ENDIF

        IF (RESTART_FOLDER /= '') THEN
          CALL writeRestart
        ENDIF

        IF (SS_STATS .AND. (SS_STATS_BACKUP .GT. 0)) THEN

          IF (MOD(step, SS_STATS_BACKUP) .EQ. 0) THEN
            CALL SSGenStats(ssIDs, ssEnergies, ssHashkeysIDs, ssHashkeysOccur, ssPrevHashkeysOccur, &
              TRIM(intToChar(step)))
          ENDIF
        ENDIF
      END DO mcsteps
    ENDIF !mcsteps

    IF (L_COMPUTE_RDF) THEN
      R_RDF_SIGMA = zero
      CALL printTrdf(TRDF_DATA(1),'D-RDF')
      R_RDF_SIGMA = LOCAL_SIGMA
      CALL printTrdf(TRDF_DATA(2),'T-RDF')
      DEALLOCATE(RDF_NEW)
      DEALLOCATE(TRDF_DATA)
    ENDIF

    IF (INT_MAX_BEST > 0) THEN
      CALL printBestSet(0)
    ENDIF

    IF (MASTER_PROC) THEN
      CALL printBanner('Despite '//TRIM(intToChar(bad_steps))//' bad step(s),'// &
        ' final energy = '//TRIM(realToChar(energy_low)))
      CALL printBanner('Global Energy Minimum = '//TRIM(realToChar(ENERGY_MIN)))
    ENDIF

    ! Generating statistics
    IF (SS_STATS) THEN
      CALL SSGenStats(ssIDs, ssEnergies, ssHashkeysIDs, ssHashkeysOccur, ssPrevHashkeysOccur)
    ENDIF

    ! Cleaning the remporary arrays
    IF (L_USE_TOP_ANALYSIS) THEN
      WRITE(message, *) 'With topological analysis ', TRIM(intToChar(duplicates)), ' duplicates were removed'
      CALL printLog("runSolidSolutions", message, 0)
    ENDIF

    IF (SS_CHECK_GEOMETRY) THEN
      WRITE(message, *) 'With geometrical check ', TRIM(intToChar(badGeometry)), ' clusters were removed'
      CALL printLog("runSolidSolutions", message, 0)
    ENDIF

    IF (SS_STATS) THEN
      DEALLOCATE(ssEnergies)
      DEALLOCATE(ssIDs)
      DEALLOCATE(ssHashkeysIDs)
      DEALLOCATE(ssHashkeysOccur)
      DEALLOCATE(ssPrevHashkeysOccur)
    ENDIF

    CALL SSFinilize()

    MASTER_CLUSTER = cluster_old(1)
  END SUBROUTINE runSolidSolutions

END MODULE SolidSolutions
