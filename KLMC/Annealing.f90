MODULE Annealing

    USE Config
    USE Format
    USE Library
    USE ClusterRoutines, ONLY : evaluate, getEnergy, notValidCluster
    USE Environment,     ONLY : writeRestart
    USE File,            ONLY : displayGULPcounters, makeFolder, printBestSet, removeArcFiles, &
                                removeClusterFiles
    USE Population,      ONLY : ENERGY_MIN, POP, initialiseCluster, restartWalker, writeWalker
    USE Master,          ONLY : foundKeyword, insertKeyword, moveBestSet, removeKeyword, updateBestSet
    USE MonteCarlo,      ONLY : runMonteCarlo

    IMPLICIT NONE

CONTAINS

SUBROUTINE runAnnealing
    INTEGER :: ikey, kid, temp, mc, runner, success
    REAL(KIND=DBL) :: energy_new(1), energy_old(1), current_temp
    TYPE(cluster),DIMENSION(1) :: cluster_old, cluster_new
    CHARACTER(LEN=16) :: MC_TYPE=''
    LOGICAL :: restarting

    energy_old(1) = R_ENERGY_ZERO                   ! Energy of last accepted step
    energy_new(1) = R_ENERGY_ZERO                   ! Energy of new configuration
    MC_TYPE='Annealing       '                   ! Default moveclass

    ikey = foundKeyword('opti')
    IF (ikey == 0) THEN
      ikey = foundKeyword('')
      CALL insertKeyword('opti',ikey)
    ENDIF
    MASTER_KEY = MASTER_TOP(ikey)
    CALL removeKeyword('opti',ikey)
    L_GULP_OPTI=.FALSE.
    L_ENERGY_ONLY=.TRUE.

    CALL initialiseCluster(cluster_old(1))          ! Initialise using defaults values from MASTER
    CALL initialiseCluster(cluster_new(1))          ! Initialise using defaults values from MASTER

    CALL printHeaderLine
    CALL printBeginBanner
    CALL printHeaderCentred('')

    cluster_old(1) = POP(1)
    IF (POP(1)%id(1:1) == 'Z') THEN
      CALL printHeaderCentred('now seeding SA from cluster '//TRIM(cluster_old(1)%id))
    ELSE
      CALL printHeaderCentred('now starting SA from random cluster '//TRIM(cluster_old(1)%id))
    ENDIF
    IF (RESTART_FOLDER /= '') THEN
      CALL restartWalker(cluster_old(1),restarting) ! start at location of previous walker?
    ENDIF

    energy_new(1) = getEnergy(cluster_old(1))
    CALL evaluate(cluster_old)
    energy_old(1) = getEnergy(cluster_old(1))

    ENERGY_MIN = energy_old(1)                      ! Energy global minimum

    IF (restarting) THEN
      CALL printHeaderCentred('energy of starting configuration = '//TRIM(realToChar(energy_old(1))))
      CALL printHeaderCentred('although energy from input file  = '//TRIM(realToChar(energy_new(1))))
    ELSE
      CALL printHeaderCentred('with an initial energy of '//TRIM(realToChar(energy_old(1)))//' eV')
      IF (RESTART_FOLDER /= '') THEN             ! open restart folder
        CALL makeFolder(TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER))
      ENDIF
    ENDIF
    CALL printEndBanner

    current_temp = R_TEMPERATURE
    DO temp = 2, N_MC_DONE
      current_temp = current_temp * SA_TEMP_SCALE
    ENDDO

    temperature: DO temp = N_MC_DONE, N_TEMPERATURE

      WRITE(stdsee,*)temp,' starting temperature ',current_temp

      runner = 0
      DO mc = 1, N_SAMPLE_PTS

        L_GULP_OPTI=.FALSE.
        L_ENERGY_ONLY=.TRUE.
        N_MAX_MC_STEPS = N_THRESHOLD_STEPS
        R_TEMPERATURE = current_temp

        CALL runMonteCarlo(cluster_old(1),success)

        energy_old(1) = getEnergy(cluster_old(1))
        CALL printBanner('Holding energy = '//TRIM(realToChar(energy_old(1)))//'eV')
        N_MAX_MC_STEPS = N_QUENCH_STEPS
        R_TEMPERATURE = ZERO

        DO kid = 1, N_RUNNERS

          runner = runner + 1
          cluster_new(1) = cluster_old(1)
          WRITE(stdsee,*)' starting runner ',runner

          IF (N_MAX_MC_STEPS > 0) THEN
            CALL runMonteCarlo(cluster_new(1),success)
          ENDIF

          CALL increment(N_INDIVIDUALS) ! change id for new configuration
          cluster_new(1)%id = 'Z'//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))

          L_GULP_OPTI=.TRUE.
          L_ENERGY_ONLY=.FALSE.
          MASTER_TOP(ikey) = MASTER_KEY

          CALL evaluate(cluster_new)
          energy_new(1) = getEnergy(cluster_new(1))
          IF (energy_new(1) < ENERGY_MIN) ENERGY_MIN = energy_new(1)

          IF (.NOT.notValidCluster(cluster_new(1),runner)) THEN
            CALL updateBestSet(cluster_new(1))
          ENDIF

          CALL removeClusterFiles(cluster_new(1))
          IF ( (L_GULP_RUN) .AND. (N_PBC == 3) ) CALL removeArcFiles(cluster_new(1))

          L_GULP_OPTI=.FALSE.
          L_ENERGY_ONLY=.TRUE.
          CALL removeKeyword('opti',ikey)

        ENDDO

      ENDDO

      CALL printBestSet(0)
      CALL moveBestSet(temp)

      IF (RESTART_FOLDER /= '') THEN
        CALL writeRestart
        CALL writeWalker(cluster_old(1))
      ENDIF

      current_temp = current_temp * SA_TEMP_SCALE

    END DO temperature

    CALL printBanner('Simulated Annealing Completed')

    IF (L_GULP_RUN) CALL displayGULPcounters

  RETURN
END SUBROUTINE runAnnealing

END MODULE Annealing
