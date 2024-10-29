MODULE ScanBox

    USE Config
    USE Library
    USE ClusterRoutines, ONLY : collapsed, dspecies, evaluate, getEnergy, getGnorm
    USE Environment,     ONLY : findRestartData, getRestartData, reportItemDone
    USE File,            ONLY : logEnergies, logEnergy, logMessage, logSuccess, printBestSet, &
                                removeArcFiles, removeClusterFiles
    USE Population,      ONLY : ENERGY_MIN, MASTER_CLUSTER, initialiseCluster
    USE Master,          ONLY : removeKeyword, updateBestSet
    USE MonteCarlo,      ONLY : metropolis
    USE Moveclass,       ONLY : randomiseLocation, rotateCluster, translateCluster

    IMPLICIT NONE

CONTAINS

SUBROUTINE runScanBox
    INTEGER :: step=1, counter=0, n_exchanges
    REAL(KIND=DBL) :: energy_new(1), energy_old(1)
    REAL(KIND=DBL) :: r_random=0., diff=0., original_stepsize=0.
    REAL(KIND=DBL) :: kB = 1.380658E-23 / 1.60217733E-19
    TYPE(cluster),DIMENSION(1) :: cluster_old, cluster_new
    CHARACTER(LEN=100) :: message=''
    CHARACTER(LEN=60) :: filename=''
    CHARACTER(LEN=20) :: MC_TYPE=''
    LOGICAL :: reject, accept=.FALSE., pbc, restart=.FALSE.
    REAL(KIND=DBL) :: kT, zero=0.0_DP, gnorm
    INTEGER :: scan_no=0, total_scans=0, nloc=0

    energy_old(1) = R_ENERGY_ZERO
    energy_new(1) = R_ENERGY_ZERO
    CALL initialiseCluster(cluster_old(1)) ! allocate memory and set defaults
    CALL initialiseCluster(cluster_new(1)) ! allocate memory and set defaults

    pbc=(index(UNIT_CELL,'cell').ne.0.OR.index(UNIT_CELL,'vector').ne.0)
    IF (BH_METHOD == 'FIXED') THEN    ! RANDOM CONTINUOUS HOPPING WITHOUT LOCAL OPT
      L_ENERGY_ONLY=.TRUE.
      L_GULP_OPTI=.FALSE.
      CALL removeKeyword('opti')      ! Ensure no relaxation when calling GULP
    ENDIF
    kT = R_TEMPERATURE * kB           ! Save time and do multiplication now
    original_stepsize = R_BH_STEPSIZE ! Later use this to define local MC moves
    ENERGY_MIN=energy_old(1)             ! Need to explicitly reset for muliple jobs

    CALL findRestartData()            ! define N_SEEDS and find out which to scan
    total_scans = N_SEEDS

    restart = (total_scans /= 0)
    IF (.NOT.restart) total_scans = 1

    nscans: DO scan_no = 1, total_scans
      IF (restart) THEN
        CALL getRestartData(MASTER_CLUSTER,scan_no)
        CALL logMessage(nloc,ENERGY_MIN,'Initial R location')
        CALL increment(N_INDIVIDUALS)
        CALL randomiseLocation(MASTER_CLUSTER, cluster_old(1))
        MASTER_CLUSTER = cluster_old(1) ! master config with new cluster in box
      ELSE
        cluster_old(1) = MASTER_CLUSTER ! initial starting point taken from Master File
      ENDIF

      mcsteps: DO step=1, N_MAX_BH_STEPS
        nloc = nloc +1
        WRITE(6,*) ' Starting position ',step,' for cluster ',scan_no

        ! Increase dynamic stepsize
        IF (counter > N_BH_DYNAMIC_THRESHOLD) THEN
          counter = 0 !reset counter
          R_BH_STEPSIZE = R_BH_STEPSIZE + original_stepsize
          CALL logMessage(nloc,energy_new(1),' Increasing dynamic stepsize ')
        ENDIF

        CALL increment(N_INDIVIDUALS) ! get id for new configuration

        ! Choose moveclass and apply to generate new configuration
        IF (BH_ACCEPT == 'RANDOMISE') THEN !randomise location of cluster
          MC_TYPE='Randomised    '
          CALL logMessage(nloc,ENERGY_MIN,'Randomise location')
          CALL rotateCluster(MASTER_CLUSTER, cluster_new(1))
          MASTER_CLUSTER = cluster_new(1) ! original location was initially random
          CALL randomiseLocation(MASTER_CLUSTER, cluster_new(1))
        ELSE
          MC_TYPE='MC location   '
          CALL logMessage(nloc,ENERGY_MIN,' MC location      ')
          CALL translateCluster(cluster_old(1), cluster_new(1))
        ENDIF

        CALL evaluate(cluster_new)
        energy_new(1) = getEnergy(cluster_new(1))
        gnorm = getGnorm(cluster_new(1))

        reject=.false.  ! Now perform checks on suitability of cluster

        IF (gnorm < -1.5) THEN ! Reject - GULP did not finish 2nd run
          CALL logMessage(nloc,energy_new(1),' 2nd Gulp run didnt finish!')
          reject=.true.
        ELSEIF (gnorm < -0.5) THEN ! Reject - GULP did not finish 1st run
          CALL logMessage(nloc,energy_new(1),' Gulp did not finish fully!')
          reject=.true.
        ELSEIF (cluster_new(1)%edefn == 0) THEN ! Reject cluster as energy not defined
          CALL logMessage(nloc,R_ENERGY_ZERO,'        Energy not defined!')
          reject=.true.
        ELSEIF (L_GULP_OPTI .AND. collapsed(cluster_new(1),R_COLLAPSE)) THEN   ! Cluster has collapsed
          CALL logMessage(nloc,energy_new(1),'      Cluster has collapsed!') 
          reject=.true.
        ELSEIF (L_GULP_OPTI .AND. dspecies(cluster_new(1),R_SPECIES)) THEN ! like-charges too close
          CALL logMessage(nloc,energy_new(1),'      Transfer of electrons!')
          reject=.true.
        ENDIF

        IF (reject) THEN
          energy_new(1)=R_ENERGY_ZERO
          CALL removeClusterFiles(cluster_new(1))
          IF(pbc)CALL removeArcFiles(cluster_new(1))
        ENDIF

        ! Keep a record (hard copy) of the best configurations found
        IF (INT_MAX_BEST > 0 .AND. energy_new(1) < R_BEST_EMAX) THEN
          CALL updateBestSet(cluster_new(1))
        ENDIF

        ! Report any good news
        ! Otherwise simply remove hard copy of current configuration to reduce disk usage
        IF (energy_new(1) < ENERGY_MIN) THEN
          CALL logMessage(nloc, energy_new(1),'Lower energy position found')
          CALL logSuccess(nloc, energy_new(1), MC_TYPE, R_BH_STEPSIZE)
          ENERGY_MIN = energy_new(1)
          R_BH_STEPSIZE = original_stepsize   ! reset the stepsize
          counter = 0 !reset 1st counter
        ELSE
          CALL removeClusterFiles(cluster_new(1))
          IF(pbc)CALL removeArcFiles(cluster_new(1))
        ENDIF

        IF (reject) THEN
          counter = counter + 1
        ELSE
          diff = energy_old(1) - energy_new(1)

          IF (BH_ACCEPT == 'METROPOLIS') THEN
            IF ( ABS(R_SHELL_CORE_OFFSET) > 0.00000001 ) THEN
              IF ( cluster_new(1)%edefn < cluster_old(1)%edefn ) THEN
                diff = diff + R_SHELL_CORE_OFFSET ! +'ve is downhill
              ENDIF
            ENDIF
            accept=metropolis(cluster_new(1)%id,diff,kT)
          ELSE
            accept = diff > 0.0000000
          ENDIF

          ! If step accepted then old cluster has a new location on landscape
          IF (accept .AND. ABS(diff) > 0.000000000001 ) THEN
            cluster_old(1) = cluster_new(1)
            energy_old(1) = energy_new(1)
            counter = 0  ! step down-hill accepted (position on landscape changed)
          ELSE
            counter = counter + 1 ! update the counter for unsuccessful steps
          ENDIF
        ENDIF

        IF (BH_ACCEPT == 'METROPOLIS' .OR. BH_METHOD == 'OSCIL') THEN
          CALL logEnergies(cluster_new(1),energy_old(1),ENERGY_MIN)
        ELSE
          CALL logEnergy(cluster_new(1),ENERGY_MIN) ! ENERGY_MIN=energy_old(1)
        ENDIF

        ! Change temperature?
        !IF (BH_METHOD == 'OSCIL') THEN
        !    IF (BH_ACCEPT == 'METROPOLIS') THEN
        !      IF ( mod(step,N_STEPS_HIGH_T) == 0 )THEN
        !        write(*,*)'Starting the downhill search after step ',step,energy_old(1)
        !        filename = TRIM(RELAXED_FOLDER)//'MC-HIGH-'//TRIM(intToChar(step))
        !        message = 'Low  Point'
        !        CALL outputXYZ(cluster_old(1), message, filename)
        !        BH_ACCEPT = 'QUENCH'
        !      ENDIF
        !    ELSE ! (BH_ACCEPT == 'QUENCH') THEN
        !      IF ( mod(step,N_STEPS_LOW_T) == 0 )THEN
        !        write(*,*)'Turning back up the heat after step ',step,energy_old(1)
        !        filename = TRIM(RELAXED_FOLDER)//'MC-LOW-'//TRIM(intToChar(step))
        !        message = 'High Point'
        !        CALL outputXYZ(cluster_old(1), message, filename)
        !        BH_ACCEPT = 'METROPOLIS'
        !      ENDIF
        !    ENDIF
        !ELSEIF (BH_METHOD == 'FIXED') THEN
        !    IF ( mod(step,N_TEMPERATURE) == 0 )THEN
        !        write(*,*)'Temperature change ',step,kT/kB,energy_old(1)
        !        filename = TRIM(RELAXED_FOLDER)//'MC-TEMP-'//TRIM(intToChar(step))
        !        message = 'T  Changed'
        !        CALL outputXYZ(cluster_old(1), message, filename)
        !        kT = kT * SA_TEMP_SCALE
        !    ENDIF
        !ENDIF

      ENDDO mcsteps

      CALL reportItemDone(scan_no)
    ENDDO nscans

    !scan_no = 0
    !CALL reportItemDone(scan_no) ! close scans-done (unit 27)

    IF (INT_MAX_BEST > 0) THEN
      CALL printBestSet(0)
    ENDIF

    MASTER_CLUSTER = cluster_old(1)
END SUBROUTINE runScanBox

END MODULE ScanBox
