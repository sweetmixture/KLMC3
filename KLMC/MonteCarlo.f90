MODULE MonteCarlo

    USE Config
    USE Format
    USE Library
    USE ClusterRoutines, ONLY : evaluate, getEnergy, notValidCluster
    USE File,            ONLY : logEnergies, logMessage, logMetropolis, logSuccess, &
                                removeArcFiles, removeClusterFiles
    USE Moveclass,       ONLY : moveAtoms, randomiseCell
    USE Population,      ONLY : ENERGY_MIN
    USE UnitCell,        ONLY : xftoxc

    IMPLICIT NONE

    SAVE
    TYPE(cluster), PUBLIC, ALLOCATABLE, DIMENSION(:) :: cluster_new  ! new position

!==========================================================================================!
CONTAINS

! routines for performing simple MC and Metropolis
!
! subroutines: runMonteCarlo
!
!  functions:  metropolis

!==========================================================================================!

SUBROUTINE runMonteCarlo(cluster_old,out_success,in_threshold)
    TYPE(CLUSTER), INTENT(INOUT) :: cluster_old
    INTEGER, INTENT(OUT) :: out_success
    REAL(KIND=DBL), INTENT(IN), OPTIONAL :: in_threshold

    INTEGER :: step, success, failure
    REAL(KIND=DBL) :: energy_new, energy_old, energy_low, energy_bad, energy_1st
    REAL(KIND=DBL) :: r_random=0., diff=0.
    REAL(KIND=DBL) :: kT, kB = 1.380658E-23 / 1.60217733E-19
    CHARACTER(LEN=16) :: MC_TYPE=''
    LOGICAL :: accept, L_QUENCH, L_THRESHOLD

    ALLOCATE(cluster_new(1))

    failure = 0                            ! Number of failed configs
    success = 0                            ! Number of succesful steps
    energy_old = getEnergy(cluster_old)    ! Energy of last accepted step
    energy_new = R_ENERGY_ZERO             ! Energy of new configuration
    energy_low = energy_old                ! Best energy found on this run
    energy_bad = energy_old                ! Worst energy found on this run
    energy_1st = energy_old                ! First energy

    ! Energy global minimum
    IF (R_BEST_ENERGIES(1) < ENERGY_MIN) ENERGY_MIN = R_BEST_ENERGIES(1)
    IF (energy_old < ENERGY_MIN) ENERGY_MIN = energy_old

    IF (PRESENT(in_threshold)) THEN
      L_THRESHOLD = .TRUE.
      MC_TYPE='MC-THRESHOLD    '
    ELSE
      L_THRESHOLD = .FALSE.
      L_QUENCH = (R_TEMPERATURE < 1.0e-10) ! Quench or Metropolis?
      IF (L_QUENCH) THEN
        MC_TYPE='MC-QUENCH       '
      ELSE
        MC_TYPE='MC-METROPOLIS   '
        kT = R_TEMPERATURE * kB            ! Save time and do multiplication now
      ENDIF
    ENDIF

    mcsteps: DO step=1, N_MAX_MC_STEPS

        IF (DEBUG_LEVEL > 0) WRITE(stdsee,*)' Starting MC step',step

        CALL increment(N_INDIVIDUALS) ! change id for new configuration

        IF( N_PBC == 0) THEN ! Cluster Routines Required
          CALL moveAtoms(cluster_old, cluster_new(1))
        ELSEIF( N_PBC == 3) THEN ! Crystal Routines Required
          CALL moveAtoms(cluster_old, cluster_new(1))
          CALL RANDOM_NUMBER(r_random)
          IF (r_random < PROB_MC_UNITCELL) THEN
            MC_TYPE(16:16)='+'
            CALL randomiseCell(cluster_new(1))
            CALL xftoxc(cluster_new(1))
          ELSE
            MC_TYPE(16:16)=' '
          ENDIF
        ELSE ! Other routines required
          WRITE(stderr,*)'Basin hopping needs updating' !1D and 2D Routines Required
          STOP
        ENDIF

        CALL evaluate(cluster_new(:))
        energy_new = getEnergy(cluster_new(1))

        IF (notValidCluster(cluster_new(1),step)) THEN
          energy_new=R_ENERGY_ZERO
          accept=.FALSE.
          failure = failure + 1
        ELSE
          IF (L_THRESHOLD) THEN
            accept = (energy_new < in_threshold)
          ELSE
            diff = getEnergy(cluster_old,0) - getEnergy(cluster_new(1),0) ! +'ve => downhill
            IF (L_QUENCH) THEN
              accept=(diff > ZERO)
            ELSE
              accept=metropolis(cluster_new(1)%id,diff,kT)
            ENDIF
          ENDIF
        ENDIF

        IF (accept) THEN

          IF (energy_new > energy_bad) THEN
            energy_bad = energy_new
          ENDIF

          IF (energy_new < energy_low) THEN
            CALL logMessage(step, energy_new,' Lower energy cluster found')
            CALL logSuccess(step, energy_new, MC_TYPE, R_TEMPERATURE)
            energy_low = energy_new
          ELSE
!           report step accepted
          ENDIF

          cluster_old = cluster_new(1)
          energy_old = energy_new
          success = success + 1

          IF (energy_old < ENERGY_MIN) THEN
            ENERGY_MIN = energy_old
            CALL printBanner('New global minimum E ='//TRIM(realToChar(ENERGY_MIN))// &
                                                 ' on MC step '//TRIM(intToChar(step)))
          ELSE
            CALL removeClusterFiles(cluster_new(1))
            IF ( (L_GULP_RUN) .AND. (N_PBC == 3) ) CALL removeArcFiles(cluster_new(1))
          ENDIF

        ELSE

          CALL removeClusterFiles(cluster_new(1))
          IF ( (L_GULP_RUN) .AND. (N_PBC == 3) ) CALL removeArcFiles(cluster_new(1))

        ENDIF

        CALL logEnergies(cluster_new(1),energy_low,energy_bad)

    END DO mcsteps

    IF (OUTPUT_LEVEL > 0) THEN
      CALL printBeginBanner
      IF (L_THRESHOLD) THEN
      CALL printHeaderCentred('MC end: with Lid = '//TRIM(realToChar(in_threshold))// &
      ' after '//TRIM(intToChar(success))//'/'//TRIM(intToChar(N_MAX_MC_STEPS))//' steps')
      ELSE
      CALL printHeaderCentred('MC end: with T = '//TRIM(realToChar(R_TEMPERATURE))// &
      ' after '//TRIM(intToChar(success))//'/'//TRIM(intToChar(N_MAX_MC_STEPS))//' steps')
      ENDIF
      CALL printHeaderCentred('E(start) = '//TRIM(realToChar(energy_1st))// &
                             ' E(end) = '//TRIM(realToChar(energy_old)))
      CALL printHeaderCentred('E(high) =  '//TRIM(realToChar(energy_bad))// &
                             ' E(low) = '//TRIM(realToChar(energy_low)))
      IF (failure > 0)CALL printHeaderCentred('Number of crazy steps = '//intToChar(failure))
      CALL printEndBanner
    ENDIF

    out_success = success
    DEALLOCATE(cluster_new)

    RETURN
END SUBROUTINE runMonteCarlo

!==========================================================================================!

LOGICAL FUNCTION metropolis(in_id,in_diff,in_kT)
    CHARACTER(LEN=*), INTENT(IN) :: in_id
    REAL(KIND=DBL), INTENT(IN) :: in_diff, in_kT
    REAL(KIND=DBL) :: probmetro, r_random
    LOGICAL :: accept

    IF (in_kT < 0.0000000001 ) THEN
       probmetro = 1.0 
       accept = .true.
       r_random = 2.0
       metropolis = accept
    ELSE

    IF ( ABS(in_diff) < dZERO ) THEN
      probmetro = 1.0 ! no point computing exponential fn
      accept = .true.
      r_random = 2.0 ! code to imply nochange
    ELSEIF ( in_diff > dZERO ) THEN
      probmetro = 1.0 ! no point computing exponential fn
      accept = .true.
      r_random = 5.0 ! code to imply accept downhill step
    ELSE
      CALL RANDOM_NUMBER(r_random)
      probmetro = EXP(in_diff/in_kT)
      accept = probmetro > r_random
    ENDIF
   
    END IF

    CALL logMetropolis(in_id,in_diff,probmetro,r_random,accept)

    metropolis=accept
END FUNCTION metropolis

!==========================================================================================!

END MODULE MonteCarlo
