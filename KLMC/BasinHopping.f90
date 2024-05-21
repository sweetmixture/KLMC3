MODULE BasinHopping

    USE Config
    USE Format
    USE Library
    USE ClusterRoutines, ONLY : evaluate, getEnergy, notValidCluster
    USE Environment,     ONLY : writeRestart
    USE File,            ONLY : displayGULPcounters, logEnergies, logEnergy, logMessage, logSuccess, &
                                makeFolder, outputXYZ, printBestSet, removeArcFiles, removeClusterFiles, &
                                peek
    USE Population,      ONLY : ENERGY_MIN, POP, initialiseCluster, restartWalker, writeWalker
    USE Master,          ONLY : removeKeyword, updateBestSet
    USE MonteCarlo,      ONLY : metropolis
    USE Moveclass,       ONLY : moveAtoms, mutateCluster, randomiseCell, randomiseCluster, rotateCluster, &
                                swapCations, swapAtoms, translateCluster, twistCluster
    USE UnitCell,        ONLY : xftoxc,checkCell
    USE Utilities,       ONLY : getHashKey, getHashkeyRadius, findEnergyMatch

    IMPLICIT NONE

CONTAINS

SUBROUTINE runBasinHopping
    INTEGER :: step=1, counter=0, counter1=0, i
    REAL(KIND=DBL) :: energy_new, energy_old, energy_low
    REAL(KIND=DBL) :: r_random=0., diff=0., original_stepsize=0.
    REAL(KIND=DBL) :: kB = 1.380658E-23 / 1.60217733E-19
    CHARACTER(LEN=16) :: MC_TYPE=''
    TYPE(cluster),DIMENSION(1) :: cluster_old, cluster_new
    CHARACTER(LEN=100) :: message=''
    CHARACTER(LEN=60) :: filename=''
    CHARACTER(LEN=34) :: message1=''
    CHARACTER(LEN=34) :: message2=''

    LOGICAL :: reject, accept=.FALSE., old_config, random_moveclass
    REAL(KIND=DBL) :: kT, gnorm

    ENERGY_MIN = R_BEST_ENERGIES(1)      ! Energy global minimum
    energy_low = R_ENERGY_ZERO           ! Best energy found on this run
    energy_old = R_ENERGY_ZERO           ! Energy of last accepted step
    energy_new = R_ENERGY_ZERO           ! Energy of new configuration
    kT = R_TEMPERATURE * kB              ! Save time and do multiplication now
    original_stepsize = R_BH_STEPSIZE    ! Save in case R_BH_STEPSIZE is modified
    MC_TYPE='Monte Carlo     '           ! Default moveclass
    random_moveclass = .FALSE.

    IF (N_PBC == 0) THEN
      PROB_SWAP_CATIONS = 1.0 - PROB_SWAP_CATIONS
      PROB_SWAP_ATOMS = PROB_SWAP_CATIONS - PROB_SWAP_ATOMS
      PROB_MUTATE_CLUSTER = PROB_SWAP_ATOMS - PROB_MUTATE_CLUSTER
      PROB_TWIST_CLUSTER =  PROB_MUTATE_CLUSTER - PROB_TWIST_CLUSTER
    ELSEIF (N_PBC == 3) THEN
      PROB_SWAP_CATIONS = 1.0 - PROB_SWAP_CATIONS
      PROB_SWAP_ATOMS = PROB_SWAP_CATIONS - PROB_SWAP_ATOMS
    ENDIF

    CALL initialiseCluster(cluster_old(1))  ! Initialise using defaults values from MASTER
    CALL initialiseCluster(cluster_new(1))  ! Initialise using defaults values from MASTER

    cluster_old(1) = POP(1)
    IF (RESTART_FOLDER /= '') THEN
      CALL restartWalker(cluster_old(1))    ! Restart: start at location of previous walker
      cluster_new(1) = cluster_old(1)
      energy_new = getEnergy(cluster_new(1))
    ENDIF

    !Do we have restart data? if so, structures are labelled "Z", otherwise
    !labelled "X" until first optimised. 
    old_config=(cluster_old(1)%id(1:1) == 'Z')

    IF (BH_METHOD == 'FIXED') THEN       ! RANDOM CONTINUOUS HOPPING WITHOUT LOCAL OPT
      L_ENERGY_ONLY=.TRUE.
      L_GULP_OPTI=.FALSE.
      CALL removeKeyword('opti')         ! Ensure no relaxation when calling GULP
    ENDIF

    IF (hashRadius .EQ. 0.0) THEN
       hashRadius = getHashkeyRadius(cluster_old(1), N_ATOMS)
    ENDIF

    CALL evaluate(cluster_old)
    energy_old = getEnergy(cluster_old(1))

    IF (energy_old < R_BEST_EMAX) CALL updateBestSet(cluster_old(1))

    CALL printHeaderLine
    IF (old_config) THEN
      diff = getEnergy(cluster_old(1),0) - getEnergy(cluster_new(1),0) ! includes possible core-shell offset
      IF (ABS(diff) > R_BEST_EDIF) THEN
        CALL printBanner('now starting search with '//TRIM(cluster_old(1)%id)// &
                         ' even though energy changed by '//TRIM(realToChar(diff)))
      ELSE
        CALL printBanner('now starting search from cluster '//TRIM(cluster_old(1)%id)// &
                         ' with energy '//TRIM(realToChar(diff))//'eV')
      ENDIF
      IF (energy_old < ENERGY_MIN) ENERGY_MIN = energy_old
    ELSE
      CALL printBanner('now starting search from random point '//TRIM(cluster_old(1)%id)// &
                         ' with energy '//TRIM(realToChar(diff))//'eV')
      IF (INT_MAX_BEST > 0 .AND. energy_old < R_BEST_EMAX) THEN
        CALL updateBestSet(cluster_old(1))
      ENDIF
    ENDIF

    IF (RESTART_FOLDER /= '') THEN ! open restart folder
      CALL makeFolder(TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER))
    ENDIF
    IF (OUTPUT_LEVEL < 6) THEN
      message1=', so removing associated I/O files'
    ELSE
      message1=', but keeping associated I/O files'
    ENDIF
    IF (OUTPUT_LEVEL < 2) THEN
      message2=', and removed associated I/O files'
    ELSE
      message2='.   Will keep associated I/O files'
    ENDIF

    counter = 0
    counter1 = 0

    mcsteps: DO step=2, N_MAX_BH_STEPS

        WRITE(stdsee,*)' Starting step',step

        ! Increase dynamic stepsize
        IF (counter > N_BH_DYNAMIC_THRESHOLD) THEN
          counter = 0 !reset counter
          R_BH_STEPSIZE = R_BH_STEPSIZE + original_stepsize
          CALL logMessage(step,energy_new,' Increasing dynamic stepsize ')
        END IF

        ! Start up other moveclass operators
        IF (.NOT.L_MC_STEPS_ONLY) THEN
          IF ( counter > N_BH_MOVECLASS_THRESHOLD &
           .OR. R_BH_STEPSIZE > 3.5*original_stepsize) THEN
            CALL logMessage(step,energy_new,' Run tricks: not just simple MC moveclass')
            R_BH_STEPSIZE = original_stepsize
            random_moveclass = .TRUE.
          ENDIF
        ENDIF

        CALL increment(N_INDIVIDUALS) ! change id for new configuration
        reject=.FALSE.

        ! Choose moveclass and apply
        IF(BH_ACCEPT == 'RANDOMISE') THEN !randomise cluster
            MC_TYPE='Randomised    '
            CALL randomiseCluster(cluster_old(1), cluster_new(1))
        !END IF
        
        ELSE IF( N_PBC == 0) THEN ! Cluster Routines Required

           IF (L_MC_STEPS_ONLY) THEN
              CALL moveAtoms(cluster_old(1), cluster_new(1))

           ELSEIF( L_ABOVE_SURFACE ) THEN
              CALL RANDOM_NUMBER(r_random)
              IF (r_random < PROB_TRANS_SURFACE) THEN
                 MC_TYPE='Cluster push  '
                 CALL translateCluster(cluster_old(1), cluster_new(1))
              ELSEIF (r_random < PROB_TRANS_SURFACE + PROB_ROTATE_SURFACE) THEN
                 MC_TYPE='Cluster spin  '
                 CALL rotateCluster(cluster_old(1), cluster_new(1))
              ELSE
                 MC_TYPE='Monte Carlo   '
                 CALL moveAtoms(cluster_old(1), cluster_new(1))
              ENDIF

           ELSEIF( random_moveclass ) THEN
              CALL RANDOM_NUMBER(r_random)
              IF (r_random > PROB_SWAP_CATIONS) THEN
                 MC_TYPE='Cations switch'
                 CALL logMessage(step,energy_new,' Switching cations')
                 CALL swapCations(cluster_old(1), cluster_new(1))
              ELSEIF (r_random > PROB_SWAP_ATOMS) THEN
                 MC_TYPE='Atoms   switch'
                 CALL logMessage(step,energy_new,' Switching atoms')
                 CALL swapAtoms(cluster_old(1), cluster_new(1))
              ELSEIF (r_random > PROB_MUTATE_CLUSTER) THEN
                 MC_TYPE='Cluster mutate'
                 CALL logMessage(step,energy_new,' Mutating cluster')
                 CALL mutateCluster(cluster_old(1), cluster_new(1))
              ELSEIF (r_random > PROB_TWIST_CLUSTER) THEN
                 MC_TYPE='Cluster  twist'
                 CALL logMessage(step,energy_new,' Twisting cluster')
                 CALL twistCluster(cluster_old(1), cluster_new(1))
              ELSE
                 MC_TYPE='Monte Carlo   '
                 CALL moveAtoms(cluster_old(1), cluster_new(1))
              ENDIF

           ELSE
              MC_TYPE='Monte Carlo   '

              CALL moveAtoms(cluster_old(1), cluster_new(1))
           ENDIF

        ELSEIF( N_PBC == 3) THEN ! Crystal Routines Required
        ! IF (L_MC_STEPS_ONLY .AND. .NOT.random_moveclass) THEN
           IF (L_MC_STEPS_ONLY) THEN
              MC_TYPE='Monte Carlo     '
              CALL moveAtoms(cluster_old(1), cluster_new(1))
          !ELSEIF (random_moveclass) THEN
            ELSE
               CALL RANDOM_NUMBER(r_random)
               IF (r_random > PROB_SWAP_CATIONS) THEN
                  MC_TYPE='Cations switch  '
                  CALL logMessage(step,energy_new,' Switching cations')
                  CALL swapCations(cluster_old(1), cluster_new(1))
               ELSEIF (r_random > PROB_SWAP_ATOMS) THEN
                  MC_TYPE='Atoms   switch  '
                  CALL logMessage(step,energy_new,' Switching atoms')
                  CALL swapAtoms(cluster_old(1), cluster_new(1))
               ELSE
                  MC_TYPE='Monte Carlo     '
                  CALL moveAtoms(cluster_old(1), cluster_new(1))
               ENDIF
            ENDIF

          CALL RANDOM_NUMBER(r_random)
          IF (r_random < PROB_MC_UNITCELL) THEN
             MC_TYPE(16:16)='+'
             reject = .TRUE.
             DO WHILE (reject)
                CALL randomiseCell(cluster_new(1))
                CALL checkCell(cluster_new(1),reject)
                IF (reject) THEN
                   WRITE(stderr,*)'Unit cell tried to become unphysical on step ',step
                   WRITE(stderr,*) 'Retrying...'
                   cluster_new(1)%box=cluster_old(1)%box
                END IF
             END DO  
             CALL xftoxc(cluster_new(1))
          ENDIF
        ELSE ! Other routines required
          WRITE(stderr,*)'Basin hopping needs updating' !1D and 2D Routines Required
          STOP
        ENDIF

        CALL evaluate(cluster_new)
        energy_new = getEnergy(cluster_new(1))

        reject=(notValidCluster(cluster_new(1),step))

        IF (reject) THEN
          energy_new=R_ENERGY_ZERO
          accept=.FALSE.
        ELSE

          ! When using topological analysis, energies should be compared at the same level of relaxation
          IF ((L_USE_TOP_ANALYSIS) .AND. (cluster_new(1)%hashkeyMatchNo .NE. 0)) THEN
            diff = getEnergy(cluster_old(1),cluster_new(1)%hashkeyMatchIter) - &
              & getEnergy(cluster_new(1),cluster_new(1)%hashkeyMatchIter) ! +'ve => downhill
          ELSE
            diff = getEnergy(cluster_old(1),0) - getEnergy(cluster_new(1),0) ! +'ve => downhill
          ENDIF

          IF (BH_ACCEPT == 'METROPOLIS') THEN
            accept=metropolis(cluster_new(1)%id,diff,kT)
          ELSE
            accept = diff > dZERO
          ENDIF

          IF (INT_MAX_BEST > 0 .AND. energy_new < R_BEST_EMAX) THEN
            CALL updateBestSet(cluster_new(1))
          ENDIF
        ENDIF

        IF (accept) THEN
          counter = 0                         ! reset counter
          R_BH_STEPSIZE = original_stepsize   ! reset stepsize
          random_moveclass = .FALSE.

          IF (energy_new < energy_low) THEN
            CALL logMessage(step, energy_new,' Lower energy cluster found')
            CALL logSuccess(step, energy_new, MC_TYPE, R_BH_STEPSIZE)
            energy_low = energy_new
          ELSE
!           report step accepted
          ENDIF

          cluster_old(1) = cluster_new(1)
          energy_old = energy_new
          IF (RESTART_FOLDER /= '') THEN
            CALL writeWalker(cluster_old(1))
          ENDIF

          IF (diff > dZERO) THEN
            IF (OUTPUT_LEVEL/=0) CALL printHeaderCentred('Accepted downhill step, '// &
                           TRIM(intToChar(step))//'.   Will keep associated I/O files')
          ELSE
            IF (OUTPUT_LEVEL/=0) CALL printHeaderCentred('Accepted step, '// &
                                                 TRIM(intToChar(step))//TRIM(message2))
            IF (OUTPUT_LEVEL < 2) CALL removeClusterFiles(cluster_new(1))
          ENDIF

          IF (energy_old < ENERGY_MIN) THEN
            ENERGY_MIN = energy_old
            CALL printBanner('New global minimum E ='//TRIM(realToChar(ENERGY_MIN))// &
                                                    ' on step '//TRIM(intToChar(step)))
          ENDIF

        ELSE

          counter = counter + 1
          IF (OUTPUT_LEVEL/=0) CALL printHeaderCentred('Rejected step, '// &
                                                 TRIM(intToChar(step))//TRIM(message1))
          CALL removeClusterFiles(cluster_new(1))

        ENDIF

        IF (BH_ACCEPT == 'METROPOLIS' .OR. BH_METHOD == 'OSCIL') THEN
            CALL logEnergies(cluster_new(1),energy_old,ENERGY_MIN)
        ELSE
            CALL logEnergy(cluster_new(1),ENERGY_MIN) ! ENERGY_MIN=energy_old
        ENDIF

        ! Change temperature?
        IF (BH_METHOD == 'OSCIL') THEN
            IF (BH_ACCEPT == 'METROPOLIS') THEN
              IF ( mod(step,N_STEPS_HIGH_T) == 0 )THEN
                write(*,*)'Starting the downhill search after step ',step,energy_old
                filename = TRIM(RELAXED_FOLDER)//'MC-HIGH-'//TRIM(intToChar(step))
                message = 'Low  Point'
                CALL outputXYZ(cluster_old(1), message, filename)
                BH_ACCEPT = 'QUENCH'
              ENDIF
            ELSE ! (BH_ACCEPT == 'QUENCH') THEN
              IF ( mod(step,N_STEPS_LOW_T) == 0 )THEN
                write(*,*)'Turning back up the heat after step ',step,energy_old
                filename = TRIM(RELAXED_FOLDER)//'MC-LOW-'//TRIM(intToChar(step))
                message = 'High Point'
                CALL outputXYZ(cluster_old(1), message, filename)
                BH_ACCEPT = 'METROPOLIS'
              ENDIF
            ENDIF
        ELSEIF (BH_METHOD == 'FIXED') THEN
            IF ( mod(step,N_TEMPERATURE) == 0 )THEN
                write(*,*)'Temperature change ',step,kT/kB,energy_old
                filename = TRIM(RELAXED_FOLDER)//'MC-TEMP-'//TRIM(intToChar(step))
                message = 'T  Changed'
                CALL outputXYZ(cluster_old(1), message, filename)
                kT = kT * SA_TEMP_SCALE
            ENDIF
        ENDIF

        IF (RESTART_FOLDER /= '') THEN
          CALL writeRestart
        ENDIF

    END DO mcsteps

    IF (N_PBC == 0) THEN
      PROB_TWIST_CLUSTER =  PROB_TWIST_CLUSTER - PROB_MUTATE_CLUSTER
      PROB_MUTATE_CLUSTER = PROB_MUTATE_CLUSTER - PROB_SWAP_ATOMS
      PROB_SWAP_ATOMS = PROB_SWAP_ATOMS - PROB_SWAP_CATIONS
      PROB_SWAP_CATIONS = PROB_SWAP_CATIONS - 1.0
    ELSEIF (N_PBC == 3) THEN
      PROB_SWAP_ATOMS = PROB_SWAP_ATOMS - PROB_SWAP_CATIONS
      PROB_SWAP_CATIONS = PROB_SWAP_CATIONS - 1.0
    ENDIF

    CALL printBanner('Basin Hopping Completed')

    IF (INT_MAX_BEST < 0) THEN
      CALL printBestSet(0)
    ENDIF

    IF (L_GULP_RUN) CALL displayGULPcounters

    POP(1) = cluster_old(1)
  RETURN
END SUBROUTINE runBasinHopping

END MODULE BasinHopping
