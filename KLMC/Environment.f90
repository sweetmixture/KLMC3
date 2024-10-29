MODULE Environment

    USE Config
    USE Format
    USE Library
    USE TIMER
    USE Comms,   ONLY : comms_finalize, master_alone, MPI_BARRIER, MY_COMM, my_rank, nprocs, NUM_FARMS
    USE File,    ONLY : makeFolder, readXYZ, retrieveData, inputCluster, readCAR, inquireDirectory

    USE Atoms,   ONLY : lookForAtom

    IMPLICIT NONE

SAVE

    CHARACTER(LEN=MASTER_FILE_WIDTH),PUBLIC,DIMENSION(MAX_N_SEEDS):: restart_file=''

!==========================================================================================!
CONTAINS

! routines used to set up the environment
!
! subroutines: displayControl displayLandscapeDefn displayMainHeader
!              extinctArrays finalise
!              findRestartData getRestartData
!              initialise initialiseArrays initialiseEnvironment
!              readControl readJobs readRestart
!              reportItemDone writeRestart

!==========================================================================================!

SUBROUTINE initialise()
    INTEGER :: i
!   ASSUME MAX_E_DEFN = 3 

    DO I=1, MAX_E_DEFN
      IF (I == 1) THEN
        CLUSTER_PREFIX(1)='A'
      ELSEIF (I == 2) THEN
        CLUSTER_PREFIX(2)='B'
      ELSEIF (I == 3) THEN
        CLUSTER_PREFIX(3)='C'
      ELSEIF (I == 4) THEN
        CLUSTER_PREFIX(4)='D'
      ELSEIF (I == 5) THEN
        CLUSTER_PREFIX(5)='E'
      ELSEIF (I == 6) THEN
        CLUSTER_PREFIX(6)='F'
      ELSE
        STOP "Environment->initialise: MAX_E_DEFN > 6"
      ENDIF
    ENDDO

    DO i=1,MAX_E_DEFN
      R_REFINE_THRESHOLD(i)=R_ENERGY_ZERO
      R_ENERGY_MIN_THRESHOLD(i)=-1000000000.0
      R_ENERGY_MAX_THRESHOLD(i)= 1000000000.0
    ENDDO
    R_GLOBAL_MIN_VALUE = -1000000000.0
    K_TOL(1)=0.0000001_DP
    K_TOL(2)=0.0000001_DP
    R_MAX_CLUSTER_BOUNDARY(1)=10.0
    R_MAX_CLUSTER_BOUNDARY(2)=10.0
    R_MAX_CLUSTER_BOUNDARY(3)=10.0
    N_INDIVIDUALS = 0              ! unique number for individuals generated
    N_RELAXATIONS = 0              ! total number of geometry relaxation calls
    N_EVALUATIONS = 0              ! total objective function evaluation calls
    DO i=1,MASTER_FILE_WIDTH
      MASTER_TOP(1)(i:i)=' '
    ENDDO
    DO i=1,6
      MASTER_CELL(i)=-1.0
    ENDDO
END SUBROUTINE initialise

!==========================================================================================!

SUBROUTINE finalise
   CHARACTER*30 :: info
   info=''
   BACKSPACE(stderr)
   READ(stderr,*)info
   IF (index(info,'-KLMC-error-file-').ne.0) THEN
     CLOSE(stderr,status='delete')
   ELSE
     WRITE(stdsee,*) 'Errors were detected! Check error file.'
     CALL printBanner('Errors were detected! Check error file KLMC.err!')
     CLOSE(stderr,status='keep')
   ENDIF

   CLOSE(stdsee)

   RETURN
END SUBROUTINE finalise

!==========================================================================================!

SUBROUTINE initialiseArrays()
    IF (.NOT. ALLOCATED(ID_BEST)) ALLOCATE(ID_BEST(INT_MAX_BEST+1), STAT=ALLOCATE_STAT_1)
    IF (.NOT. ALLOCATED(IDE_BEST)) ALLOCATE(IDE_BEST(INT_MAX_BEST), STAT=ALLOCATE_STAT_2)
    IF (.NOT. ALLOCATED(R_BEST_ENERGIES)) ALLOCATE(R_BEST_ENERGIES(INT_MAX_BEST), STAT=ALLOCATE_STAT_R)
    IF (.NOT. ALLOCATED(N_BEST_ENERGIES)) ALLOCATE(N_BEST_ENERGIES(INT_MAX_BEST), STAT=ALLOCATE_STAT_N)

    IF (N_DEF_ENERGY > 0 .AND. .NOT. ALLOCATED(A_BEST_ENERGIES)) THEN
      ALLOCATE(A_BEST_ENERGIES(INT_MAX_BEST), STAT=ALLOCATE_STAT_A)
    ENDIF
    IF (N_DEF_ENERGY > 1 .AND. .NOT. ALLOCATED(B_BEST_ENERGIES)) THEN
      ALLOCATE(B_BEST_ENERGIES(INT_MAX_BEST), STAT=ALLOCATE_STAT_B)
    ENDIF
    IF (N_DEF_ENERGY > 2 .AND. .NOT. ALLOCATED(C_BEST_ENERGIES)) THEN
      ALLOCATE(C_BEST_ENERGIES(INT_MAX_BEST), STAT=ALLOCATE_STAT_C)
    ENDIF
    IF (N_DEF_ENERGY > 3 .AND. .NOT. ALLOCATED(D_BEST_ENERGIES)) THEN
      ALLOCATE(D_BEST_ENERGIES(INT_MAX_BEST), STAT=ALLOCATE_STAT_D)
    ENDIF
    IF (N_DEF_ENERGY > 4 .AND. .NOT. ALLOCATED(E_BEST_ENERGIES)) THEN
      ALLOCATE(E_BEST_ENERGIES(INT_MAX_BEST), STAT=ALLOCATE_STAT_E)
    ENDIF
    IF (N_DEF_ENERGY > 5 .AND. .NOT. ALLOCATED(F_BEST_ENERGIES)) THEN
      ALLOCATE(F_BEST_ENERGIES(INT_MAX_BEST), STAT=ALLOCATE_STAT_F)
    ENDIF

    IF (L_USE_TOP_ANALYSIS) THEN
      IF (.NOT. ALLOCATED(BEST_HASHKEYS)) ALLOCATE(BEST_HASHKEYS(INT_MAX_BEST+1), STAT=ALLOCATE_STAT_HASH)
      IF (.NOT. ALLOCATED(BLACKLIST_HASHKEYS)) ALLOCATE(BLACKLIST_HASHKEYS(N_MAX_BH_STEPS), STAT=ALLOCATE_STAT_HASH)
    ENDIF

END SUBROUTINE initialiseArrays

!==========================================================================================!

SUBROUTINE extinctArrays
    DEALLOCATE(ID_BEST, STAT=ALLOCATE_STAT_1)
    DEALLOCATE(IDE_BEST, STAT=ALLOCATE_STAT_2)
    DEALLOCATE(R_BEST_ENERGIES, STAT=ALLOCATE_STAT_R)
    DEALLOCATE(N_BEST_ENERGIES, STAT=ALLOCATE_STAT_N)

    DEALLOCATE(A_BEST_ENERGIES, STAT=ALLOCATE_STAT_A)
    DEALLOCATE(B_BEST_ENERGIES, STAT=ALLOCATE_STAT_B)
    DEALLOCATE(C_BEST_ENERGIES, STAT=ALLOCATE_STAT_C)
    DEALLOCATE(D_BEST_ENERGIES, STAT=ALLOCATE_STAT_D)
    DEALLOCATE(E_BEST_ENERGIES, STAT=ALLOCATE_STAT_E)
    DEALLOCATE(F_BEST_ENERGIES, STAT=ALLOCATE_STAT_F)

    IF (L_USE_TOP_ANALYSIS) THEN
      DEALLOCATE(BEST_HASHKEYS, STAT=ALLOCATE_STAT_HASH)
      DEALLOCATE(BLACKLIST_HASHKEYS, STAT=ALLOCATE_STAT_HASH)
    ENDIF

END SUBROUTINE extinctArrays

!==========================================================================================!

SUBROUTINE displayMainHeader()
    CALL printHeaderLine
    CALL printHeaderLine
    CALL printBannerLine('****')
    CALL printBannerLine('**')
    CALL printHeaderCentred('/   *    *   *                      '// &
                            '               *     *   *****   \')
    CALL printHeaderCentred('/    *   *    *                      '// &
                            '               **   **  *     *   \')
    CALL printHeaderCentred(':     *  *     *         KNOWLEDGE LED'// &
                            ' MASTER CODE   * * * *  *          :')
    CALL printHeaderCentred('|     ***      *                      '// &
                            '               *  *  *  *          |')
    CALL printHeaderCentred(':     *  *     *               version'// &
                            ' 1.00          *     *  *          :')
    CALL printHeaderCentred('\    *   *    *                      '// &
                            '               *     *  *     *   /')
    CALL printHeaderCentred('\   *    *   *******                '// &
                            '               *     *   *****   /')
    CALL printBannerLine('**')
    CALL printBannerLine('****')
    CALL printHeaderLine
    CALL printHeaderLine
    CALL printBannerLine('*')
    CALL printHeaderCentred('AUTHORS')
    CALL printHeaderCentred('Scott Woodley')
    CALL printHeaderCentred('Kathleen Lonsdale Materials Chemistry')
    CALL printBannerLine('*')
    CALL printHeaderCentred('Matthew Farrow (Sept 2011 - )')
    CALL printHeaderCentred('Kathleen Lonsdale Materials Chemistry')
    CALL printBannerLine('*')
    CALL printHeaderCentred('Yee Chow (Oct 2010 - Sept 2011)')
    CALL printHeaderCentred('Molecular Modelling & Materials Science')
    CALL printHeaderCentred('UCL Industrial Doctorate Centre')
    CALL printBannerLine('*')
    CALL printHeaderCentred('Tomas Lazauskas (Sept 2014 - )')
    CALL printHeaderCentred('Kathleen Lonsdale Materials Chemistry')
    CALL printBannerLine('*')
    CALL printHeaderCentred('CONTRIBUTIONS FROM')
    CALL printHeaderCentred('Julian Gale (Curtin University of Technology, Australia)')
    CALL printHeaderCentred('Arnulf Mobius (Leibniz Institute, Dresden)')
    CALL printHeaderCentred('Christian Schon (Max Planck Institute, Stuttgart)')
    CALL printHeaderCentred('Alexey Sokol (Kathleen Lonsdale Materials Chemistry, UCL)')
    CALL printHeaderCentred('Iain Bethune (Edinburgh Parallel Computing Centre, Scotland)')
    CALL printBannerLine('*')
    CALL printHeaderCentred('MODULES INCLUDE')
    CALL printHeaderCentred('Stochastic Cluster Optimisation based on Thermodynamical Techniques')
    CALL printBannerLine('*')
    CALL printHeaderCentred('INTERFACES WITH')
    CALL printHeaderCentred('GULP, https://projects.ivec.org/gulp/')
    CALL printHeaderCentred('FHI-AIMS, https://aimsclub.fhi-berlin.mpg.de/')
    CALL printBannerLine('*')
    CALL printHeaderLine
    CALL printHeaderCentred('*  Last modified 1 April 2013  *')
    CALL printHeaderLine
    CALL printEmptyLine
END SUBROUTINE displayMainHeader

!==========================================================================================!

SUBROUTINE displayLandscapeDefn()
    INTEGER :: i,l
    CALL printBanner('ENERGY LANDSCAPE DEFINITION','start')
    CALL printHeaderCentred('')

    IF(N_DEF_ENERGY == 1) THEN
      IF (DEF_ENERGY(1) == 'G') CALL printHeaderCentred('will only call GULP during exploration of energy landscape')
      IF (DEF_ENERGY(1) == 'A') CALL printHeaderCentred('will only call AIMS during exploration of energy landscape')
      IF (DEF_ENERGY(1) == 'V') CALL printHeaderCentred('will only call VASP during exploration of energy landscape')
      IF (DEF_ENERGY(1) == 'N') CALL printHeaderCentred('will only call NWChem during exploration of energy landscape')
      IF (DEF_ENERGY(1) == 'D') CALL printHeaderCentred('will only call DMOL3 during exploration of energy landscape')
      CALL printHeaderCentred('')
      CALL printHeaderCentred('minimum acceptable energy: '//TRIM(realToChar(R_ENERGY_MIN_THRESHOLD(1)))//' eV')
      CALL printHeaderCentred('maximum acceptable energy: '//TRIM(realToChar(R_ENERGY_MAX_THRESHOLD(1)))//' eV')
    ELSE
      IF (DEF_ENERGY(1) == 'G') CALL printHeaderCentred('will initially call GULP during exploration of energy landscape')
      IF (DEF_ENERGY(1) == 'A') CALL printHeaderCentred('will initially call AIMS during exploration of energy landscape')
      IF (DEF_ENERGY(1) == 'V') CALL printHeaderCentred('will initially call VASP during exploration of energy landscape')
      IF (DEF_ENERGY(1) == 'N') CALL printHeaderCentred('will initially call NWChem during exploration of energy landscape')
      IF (DEF_ENERGY(1) == 'D') CALL printHeaderCentred('will initially call DMOL3 during exploration of energy landscape')
      CALL printHeaderCentred('')
      CALL printHeaderCentred('minimum acceptable energy: '//TRIM(realToChar(R_ENERGY_MIN_THRESHOLD(1)))//' eV')
      CALL printHeaderCentred('maximum acceptable energy: '//TRIM(realToChar(R_ENERGY_MAX_THRESHOLD(1)))//' eV')
      DO i=2,N_DEF_ENERGY
        CALL printHeaderCentred('iff energy less than: '//TRIM(realToChar(R_REFINE_THRESHOLD(i-1)))//' eV, then')
        CALL printHeaderCentred('')
        IF (DEF_ENERGY(i) == 'G') CALL printHeaderCentred('will further refine by calling GULP (switch energy landscape)')
        IF (DEF_ENERGY(i) == 'A') CALL printHeaderCentred('will further refine by calling AIMS (switch energy landscape)')
        IF (DEF_ENERGY(i) == 'V') CALL printHeaderCentred('will further refine by calling VASP (switch energy landscape)')
        IF (DEF_ENERGY(i) == 'N') CALL printHeaderCentred('will further refine by calling NWChem (switch energy landscape)')
        IF (DEF_ENERGY(i) == 'D') CALL printHeaderCentred('will further refine by calling DMOL3 (switch energy landscape)')
        CALL printHeaderCentred('')
        CALL printHeaderCentred('new minimum acceptable energy: '//TRIM(realToChar(R_ENERGY_MIN_THRESHOLD(i)))//' eV')
        CALL printHeaderCentred('new maximum acceptable energy: '//TRIM(realToChar(R_ENERGY_MAX_THRESHOLD(i)))//' eV')
      ENDDO
    ENDIF
    CALL printHeaderCentred('')
    CALL printHeaderLine

    l = 0
    CALL printHeaderCentred('')
    CALL printHeaderCentred('Please note, KLMC will also:')
    CALL printHeaderCentred('')
    DO i=1,N_DEF_ENERGY
      IF (DEF_ENERGY(i) == 'G' .AND. R_GNORM_TOL > ZERO) THEN
        l=l+1
        CALL printHeaderCentred(TRIM(intToChar(l))//') apply a maximum Gnorm of '//TRIM(realToChar(R_GNORM_TOL))// &
                                ' from GULP relaxations')
        CALL printHeaderCentred('')
        EXIT
      ENDIF
    ENDDO
    l=l+1
    IF (L_ONLY_2ND_ENERGY) THEN
      CALL printHeaderCentred(TRIM(intToChar(l))//') reject LM if final refinement fails')
      CALL printHeaderCentred('')
    ELSE
      CALL printHeaderCentred(TRIM(intToChar(l))//') keep LM even if final refinement fails')
      CALL printHeaderCentred('')
    ENDIF
    DO i=2,N_DEF_ENERGY
      IF (DEF_ENERGY(i) == 'G' .AND. DEF_ENERGY(i-1) == 'G' .AND. R_SHELL_CORE_OFFSET > ZERO) THEN
        l=l+1
        CALL printHeaderCentred(TRIM(intToChar(l))//') apply a '//TRIM(realToChar(R_SHELL_CORE_OFFSET))// &
                                ' eV offset if shells not added')
        CALL printHeaderCentred('')
        EXIT
      ENDIF
    ENDDO
    IF (L_SURFACE_ENERGY) THEN
      R_ENERGY_ZERO=1.0E10
      l=l+1
      CALL printHeaderCentred(TRIM(intToChar(l))//') use surface energy when comparing candidates')
      CALL printHeaderCentred('')
    ENDIF
    IF (L_ENERGY_ONLY) THEN
      l=l+1
      CALL printHeaderCentred(TRIM(intToChar(l))//') not upload refined coordinates')
      CALL printHeaderCentred('')
    ENDIF
    CALL printHeaderLine
    CALL printEmptyLine
END SUBROUTINE displayLandscapeDefn

!==========================================================================================!

SUBROUTINE displayControl(str)
    CHARACTER(LEN=*), INTENT(IN) :: str
    INTEGER :: i
    CALL printHeaderLine

    IF (DEBUG_LEVEL.gt.0) THEN
      CALL printHeaderCentred('DEBUGGING job '//TRIM(str))
    ELSE
      CALL printHeaderCentred('Running job '//TRIM(str))
      IF (L_RUN_REMOTE) THEN 
        CALL printHeaderCentred('Job run by '//TRIM(USER)//'on machine '//TRIM(HOST))
      END IF
    ENDIF

    CALL printHeaderLine
    IF (nprocs==1 .AND. NUM_FARMS==1) THEN
      CALL printHeaderCentred('There is '//TRIM(intToChar(nprocs))//' processor available'// &
                            ' and '//TRIM(intToChar(NUM_FARMS))//' processing farm')
    ELSEIF (nprocs==1) THEN
      CALL printHeaderCentred('There is '//TRIM(intToChar(nprocs))//' processor available'// &
                            ' and '//TRIM(intToChar(NUM_FARMS))//' processing farms')
    ELSEIF (NUM_FARMS==1) THEN
      CALL printHeaderCentred('There are '//TRIM(intToChar(nprocs))//' processors available'// &
                            ' and '//TRIM(intToChar(NUM_FARMS))//' processing farm')
    ELSE
      CALL printHeaderCentred('There are '//TRIM(intToChar(nprocs))//' processors available'// &
                            ' and '//TRIM(intToChar(NUM_FARMS))//' processing farms')
    ENDIF
    IF (master_alone) CALL printHeaderCentred('One CPU controlling all others')
    CALL printHeaderLine

    IF (OUTPUT_LEVEL /= 0) THEN
      CALL printBanner('INCREASED OUTPUT REQUESTED','start')
      CALL printHeaderCentred('')
      IF (OUTPUT_LEVEL.lt.6) THEN
        CALL printHeaderCentred('will output more update messages')
        CALL printHeaderCentred('')
      ELSEIF (OUTPUT_LEVEL.gt.5) THEN
        CALL printHeaderCentred('will retain all third party I/O files')
        CALL printHeaderCentred('')
      ENDIF
      CALL printHeaderLine
    ENDIF

    CALL displayLandscapeDefn ! ENERGY LANDSCAPE DEFINITION

    CALL printBanner('TOP STRUCTURES','continue')
    CALL printHeaderCentred('')
    CALL printHeaderCentred('Seeking '//TRIM(intToChar(INT_MAX_BEST))//' optimal solutions')
    IF (INT_MAX_BEST > 0) THEN
      CALL printHeaderCentred('with an energy less than '//TRIM(realToChar(R_BEST_EMAX))//' eV')
    ENDIF
    IF (LIMIT_BEST.ne.0) THEN
      CALL printHeaderCentred('and where number of files limited to '//TRIM(intToChar(LIMIT_BEST))// &
                              ' solutions')
    ENDIF
    CALL printHeaderCentred('')
    CALL printHeaderLine
    CALL printEmptyLine

    IF (JOB_TYPE /= 3) THEN
    IF (.NOT.L_ENERGY_ONLY.OR.BH_METHOD /= 'FIXED') THEN
      CALL printBanner('GEOMETRY CONSTRAINTS','continue')
      CALL printHeaderCentred('')
      CALL printHeaderCentred('Minimum 1st interatomic distance: '//TRIM(realToChar(R_COLLAPSE))//' Angs')
      IF (N_PBC == 0) THEN
        CALL printHeaderCentred('Maximum  fragmentation  distance: '//TRIM(realToChar(R_FRAGMENT))//' Angs')
        CALL printHeaderCentred('Minimum 2nd interatomic distance: '//TRIM(realToChar(R_SPECIES))//' Angs')
      ENDIF
      CALL printHeaderCentred('')
      CALL printHeaderLine
      CALL printEmptyLine
    ENDIF
    ENDIF

    CALL printBanner('FILE LOCATIONS (I/O)','continue')
    CALL printHeaderCentred('')
    IF (LEN_TRIM(WORKING_DIR)==0) THEN
      CALL printStringColumns('Working Directory','.')
    ELSE
      CALL printStringColumns('Working Directory',WORKING_DIR)
    ENDIF
    CALL printStringColumns('Data Folder',TRIM(WORKING_DIR)//DATA_FOLDER)
    CALL printStringColumns('Relaxed Folder',RELAXED_FOLDER)
    CALL printStringColumns('Final Folder',FINAL_FOLDER)
    IF (RESTART_FOLDER == '' .AND. JOB_TYPE == 2) THEN
      RESTART_FOLDER='GA-restart/'
    ENDIF

    IF (RESTART_FOLDER == '' .AND. JOB_TYPE == 0) THEN
      RESTART_FOLDER='GA-restart/POP/'
    ENDIF

    IF (RESTART_FOLDER == '') THEN
      CALL printHeaderCentred('No Restart Folder Defined!')
    ELSE
      CALL printStringColumns('Restart Folder',RESTART_FOLDER)
    ENDIF
    IF (L_GULP_RUN) THEN
      CALL printStringColumns('GULP Execute Path',EXE_GULP_PATH)
      CALL printStringColumns('Master GULP input filename',GULP_MASTER_NAME)
    ENDIF
    IF (L_AIMS_RUN) THEN
      CALL printStringColumns('AIMS Execute Path',EXE_AIMS_PATH)
      CALL printStringColumns('Master AIMS control filename',AIMS_MASTER_NAME)
    ENDIF
!   IF (L_VASP_RUN) THEN
!     CALL printStringColumns('VASP Execute Path',EXE_VASP_PATH)
!     CALL printStringColumns('Master VASP control filename',VASP_MASTER_NAME)
!   ENDIF
    CALL printHeaderCentred('')
    CALL printHeaderLine
    CALL printEmptyLine

    CALL printBanner('MISCELLANEOUS','continue')
    CALL printHeaderCentred('')
    CALL printStringColumns('Start counter from :',intToChar(N_INDIVIDUALS))
    CALL printStringColumns('Dimensions (pbc) :',intToChar(N_PBC))
    CALL printHeaderCentred('')
    IF (N_PBC == 0 .AND. JOB_TYPE == 1) THEN ! should recheck logical
      IF (L_ENFORCE_CONTAINER) THEN
        CALL printStringColumns('Enforce boundary |x| < ',realToChar(R_MAX_CLUSTER_BOUNDARY(1)))
        CALL printStringColumns('Enforce boundary |y| < ',realToChar(R_MAX_CLUSTER_BOUNDARY(2)))
        CALL printStringColumns('Enforce boundary |z| < ',realToChar(R_MAX_CLUSTER_BOUNDARY(3)))
      ELSE
        CALL printStringColumns('Initial boundary |x| < ',realToChar(R_MAX_CLUSTER_BOUNDARY(1)))
        CALL printStringColumns('Initial boundary |y| < ',realToChar(R_MAX_CLUSTER_BOUNDARY(2)))
        CALL printStringColumns('Initial boundary |z| < ',realToChar(R_MAX_CLUSTER_BOUNDARY(3)))
      ENDIF
      CALL printHeaderCentred('')
    ENDIF
    IF (L_IMAGE_ATOMS) THEN
      CALL printHeaderCentred('Will attempt to add image atoms of cluster')
      DO i = 1, N_IMAGES
         CALL printStringColumns('Replace |'//DEF_IMAGES(1,i),'| FOR |'//DEF_IMAGES(2,i)//'|')
      ENDDO
      CALL printStringColumns('Assuming a mirror plane at z =',realToChar(R_MIRROR_Z))
      CALL printHeaderCentred('')
    ENDIF
    CALL printHeaderLine
    CALL printEmptyLine

    IF(JOB_TYPE == 0) THEN
      CALL printBanner('TASK FARMING - go start a factory of calcs!','continue')
      CALL printHeaderCentred('')
      IF (N_PBC == 0) THEN
        CALL printStringColumns('Atoms in each cluster :',intToChar(N_ATOMS))
      ELSE
        CALL printStringColumns('Atoms in each unit cell :',intToChar(N_ATOMS))
      ENDIF
      CALL printStringColumns('Population',intToChar(N_POPULATION))

    ELSEIF(JOB_TYPE == 1) THEN
      CALL printBanner('MONTE CARLO - go kick-start a walker!','continue')
      CALL printHeaderCentred('')
      CALL printStringColumns('Population',intToChar(N_POPULATION))
      IF (N_PBC == 0) THEN
        CALL printStringColumns('Atoms in each cluster',intToChar(N_ATOMS))
        CALL printStringColumns('centred at',coordToChar(R_CENTRE_CLUSTER))
      ELSEIF(N_PBC == 3) THEN
        CALL printStringColumns('Atoms in each unit cell',intToChar(N_ATOMS))
      ENDIF
      CALL printHeaderCentred('')
      CALL printStringColumns('Basin Hopping Max Steps',intToChar(N_MAX_BH_STEPS))
      CALL printStringColumns('Basin Hopping Method',BH_METHOD)
      CALL printStringColumns('BH Acceptance',BH_ACCEPT)
      CALL printStringColumns('Basin Hopping Stepsize',realToChar(R_BH_STEPSIZE))
      CALL printStringColumns('BH Dynamic Threshold',intToChar(N_BH_DYNAMIC_THRESHOLD))
      IF (BH_ACCEPT /= 'RANDOMISE')CALL printStringColumns('Temperature',realToChar(R_TEMPERATURE))
      IF (BH_METHOD == 'OSCIL') THEN
        CALL printStringColumns('Number of sequential quenched steps',intToChar(N_STEPS_LOW_T))
        CALL printStringColumns('Number of sequential  high T  steps',intToChar(N_STEPS_HIGH_T))
      ELSEIF (BH_METHOD == 'FIXED' .AND. BH_ACCEPT == 'METROPOLIS' ) THEN
        CALL printStringColumns('MC steps per Temperature',intToChar(N_TEMPERATURE))
        CALL printStringColumns('Then Temperature scaled by',realToChar(SA_TEMP_SCALE))
      ENDIF
      IF (N_PBC == 0) THEN
        IF (L_ABOVE_SURFACE) THEN
        ELSEIF (L_MC_STEPS_ONLY) THEN
          CALL printHeaderCentred('')
          CALL printStringColumns('Only allow standard MC moves',' ')
        ELSE
          CALL printHeaderCentred('')
          IF (N_BH_MOVECLASS_THRESHOLD == 0) THEN
            CALL printStringColumns('Immediately apply',' ')
          ELSE
            CALL printStringColumns('Probabilities after',TRIM(intToChar(N_BH_MOVECLASS_THRESHOLD))//' bad steps')
          ENDIF
          CALL printStringColumns('Cation switch',TRIM(realToChar(PROB_SWAP_CATIONS)))
          CALL printStringColumns('Atoms switch',TRIM(realToChar(PROB_SWAP_ATOMS)))
          CALL printStringColumns('Cluster Mutation',TRIM(realToChar(PROB_MUTATE_CLUSTER)))
          CALL printStringColumns('Twisting Cluster',TRIM(realToChar(PROB_TWIST_CLUSTER)))
        ENDIF
      ELSEIF(N_PBC == 3) THEN
        CALL printStringColumns('Probability of mutating cell as well',TRIM(realToChar(PROB_MC_UNITCELL)))
        IF (L_MC_STEPS_ONLY) THEN
          CALL printStringColumns('Only allow standard MC moves',' ')
        ELSE
          IF (N_BH_MOVECLASS_THRESHOLD == 0) THEN
            CALL printStringColumns('Immediately apply',' ')
          ELSE
            CALL printStringColumns('Probabilities after',TRIM(intToChar(N_BH_MOVECLASS_THRESHOLD))//' bad steps')
          ENDIF
          CALL printStringColumns('Cation switch',TRIM(realToChar(PROB_SWAP_CATIONS)))
          CALL printStringColumns('Atoms switch',TRIM(realToChar(PROB_SWAP_ATOMS)))
        ENDIF
      ENDIF

    ELSEIF(JOB_TYPE == 2) THEN
      CALL printBanner('Genetic Algorithm - go fight to survive and procreate!','continue')
      CALL printHeaderCentred('')
      IF (N_PBC == 0) THEN
        CALL printStringColumns('Atoms in each cluster :',intToChar(N_ATOMS))
        CALL printStringColumns('centred at :',coordToChar(R_CENTRE_CLUSTER))
      ELSE
        CALL printStringColumns('Atoms in each unit cell :',intToChar(N_ATOMS))
      ENDIF
      CALL printHeaderCentred('')
      CALL printStringColumns('Population',intToChar(N_POPULATION))
      CALL printStringColumns('Elites',intToChar(N_ELITE_POP))
      CALL printStringColumns('Maximum number of generations',intToChar(N_GENERATIONS))
      CALL printStringColumns('Monte Carlo stepsize',realToChar(R_BH_STEPSIZE))
      CALL printStringColumns('Tournament size for fatherhood',intToChar(N_TOURNAMENT_SIZE))
      CALL printStringColumns('with probability',realToChar(1.0_dp))
      CALL printStringColumns('Tournament size for motherhood',intToChar(1))
      CALL printStringColumns('with probability',realToChar(1.0_dp))
      CALL printStringColumns('Will terminate early if E_MIN <',realToChar(R_GLOBAL_MIN_VALUE))
      CALL printStringColumns('Will save data every',TRIM(intToChar(N_SAVE_FREQUENCY))// ' cycles')
    
    ELSEIF(JOB_TYPE == 3) THEN
      CALL printBanner('Solid Solutions - go mix some solutions!','continue')
      CALL printHeaderCentred('')
      CALL printStringColumns('Atoms (cores) in solid',intToChar(N_ARC_ATOMS))
      CALL printStringColumns('Sites in solid solution',intToChar(N_ATOMS))
      CALL printStringColumns('Vacancies in solid solution',intToChar(N_VAC))
      CALL printStringColumns('Atoms to export from KLMC',intToChar(N_ATOMS-N_VAC))


      IF (N_REGIONS > 0) THEN
        CALL printHeaderCentred('')
        CALL printHeaderCentred('Fix '//TRIM(intToChar(N_ATOMS_R(0)))//' sites in default region')
        DO i=1,N_REGIONS
          IF (L_FIX_R(i)) THEN
            CALL printHeaderCentred('Fix '//TRIM(intToChar(N_ATOMS_R(i)))//' sites in region '//intToChar(i))
          ELSE
            CALL printHeaderCentred('Mix '//TRIM(intToChar(N_ATOMS_R(i)))//' sites in region '//intToChar(i))
          ENDIF
        ENDDO
      ENDIF
      IF (N_MIX > 0) THEN
        CALL printHeaderCentred('')
        CALL printHeaderCentred('Will use the following mixing rules')
        DO i=1,N_MIX
          CALL printHeaderCentred('Exchange '//TRIM(intToChar(T_MIX(i)%nexchg))//' sites in region '// &
                                               TRIM(intToChar(T_MIX(i)%from))//' to region '//T_MIX(i)%to)
        ENDDO
      ENDIF

      CALL printHeaderCentred('')

      CALL printStringColumns('Basin Hopping Max Steps',intToChar(N_MAX_BH_STEPS))
      CALL printStringColumns('Basin Hopping Method',BH_METHOD)
      CALL printStringColumns('BH Acceptance',BH_ACCEPT)


      IF (BH_ACCEPT /= 'RANDOMISE') THEN
        CALL printStringColumns('Max exchanges per step',intToChar(N_MC_EXCHANGE))
        IF (L_MC_RESET .AND. N_MC_EXCHANGE > 2) THEN
          CALL printHeaderCentred('with a chance of cyclic moves or switch back')
        ELSEIF (L_MC_RESET .AND. N_MC_EXCHANGE > 1) THEN
          CALL printHeaderCentred('with a change to switch back')
        ELSE
          CALL printHeaderCentred('with at least one switch guaranteed')
        ENDIF
      ENDIF
      IF (BH_ACCEPT == 'METROPOLIS')CALL printStringColumns('Temperature',realToChar(R_TEMPERATURE))
      IF (L_COMPUTE_RDF) THEN
        CALL printStringColumns('Will compute RDF from ARC files',' ')
        CALL printStringColumns('from r = 0.0 to ',realToChar(R_RDF_CUTOFF))
        CALL printStringColumns('with sigma (thermal smearing) =',realToChar(R_RDF_SIGMA))
      ENDIF

    ELSEIF (JOB_TYPE == 4) THEN
      CALL printBanner('Adaptive Polarisation Model - go refine some springs!','continue')
      CALL printHeaderCentred('')
      CALL printStringColumns('Number of atoms',intToChar(N_ATOMS))

    ELSEIF (JOB_TYPE == 5) THEN
      CALL printBanner('Clusters over a surface - go scan the box!','continue')
      CALL printHeaderCentred('')
      CALL printStringColumns('Population :',intToChar(N_POPULATION))
      CALL printStringColumns('Atoms in each cluster :',intToChar(N_ATOMS))
      CALL printStringColumns('centred at :',coordToChar(R_CENTRE_CLUSTER))
      CALL printStringColumns('Dimensions (pbc) :',intToChar(N_PBC))
      IF (BH_ACCEPT == 'RANDOMISE') THEN
        CALL printStringColumns('Random scan over box such that :',' ')
        CALL printStringColumns('|x| <',realToChar(R_MAX_CLUSTER_BOUNDARY(1)))
        CALL printStringColumns('|y| <',realToChar(R_MAX_CLUSTER_BOUNDARY(2)))
        CALL printStringColumns('|z| <',realToChar(R_MAX_CLUSTER_BOUNDARY(3)))
        CALL printStringColumns('No Monte Carlo moves towards xy-surface',' ')
      ELSE
        CALL printStringColumns('Initial boundary |x| <',realToChar(R_MAX_CLUSTER_BOUNDARY(1)))
        CALL printStringColumns('Initial boundary |y| <',realToChar(R_MAX_CLUSTER_BOUNDARY(2)))
        CALL printStringColumns('Initial boundary |z| <',realToChar(R_MAX_CLUSTER_BOUNDARY(3)))
        IF (L_ABOVE_SURFACE) THEN
          CALL printStringColumns('No Monte Carlo moves towards xy-surface',' ')
          CALL printStringColumns('Prob(MC cluster xy-move) =',realToChar(PROB_TRANS_SURFACE))
          CALL printStringColumns('Prob(MC cluster spin) =',realToChar(PROB_ROTATE_SURFACE))
        ENDIF
      ENDIF
      IF (BH_ACCEPT == 'RANDOMISE') THEN
        CALL printStringColumns('Max number of locations',intToChar(N_MAX_BH_STEPS))
      ELSE
        CALL printStringColumns('Basin Hopping Max Steps',intToChar(N_MAX_BH_STEPS))
      ENDIF
      CALL printStringColumns('Basin Hopping Method',BH_METHOD)
      CALL printStringColumns('BH Acceptance',BH_ACCEPT)
      CALL printStringColumns('Basin Hopping Stepsize',realToChar(R_BH_STEPSIZE))
      CALL printStringColumns('BH Dynamic Threshold',intToChar(N_BH_DYNAMIC_THRESHOLD))
      CALL printStringColumns('BH Moveclass Threshold',intToChar(N_BH_MOVECLASS_THRESHOLD))
      IF (BH_ACCEPT /= 'RANDOMISE')CALL printStringColumns('Temperature',realToChar(R_TEMPERATURE))
      IF (BH_METHOD == 'OSCIL') THEN
        CALL printStringColumns('Number of sequential quenched steps',intToChar(N_STEPS_LOW_T))
        CALL printStringColumns('Number of sequential high T steps',intToChar(N_STEPS_HIGH_T))
      ELSEIF (BH_METHOD == 'FIXED' .AND. BH_ACCEPT == 'METROPOLIS' ) THEN
        CALL printStringColumns('Monte Carlo steps per Temperature',intToChar(N_TEMPERATURE))
        CALL printStringColumns('Then Temperature scaled by',realToChar(SA_TEMP_SCALE))
      ENDIF

    ELSEIF (JOB_TYPE == 6) THEN
      CALL printBanner('Simulated Annealing - slow downhill walker with kids!','continue')
      CALL printHeaderCentred('')
      CALL printStringColumns('Population',intToChar(N_POPULATION))
      IF (N_PBC == 0) THEN
        CALL printStringColumns('Atoms in each cluster',intToChar(N_ATOMS))
       !CALL printStringColumns('centred at',coordToChar(R_CENTRE_CLUSTER))
      ELSEIF(N_PBC == 3) THEN
        CALL printStringColumns('Atoms in each unit cell',intToChar(N_ATOMS))
      ENDIF
      CALL printHeaderCentred('')
      CALL printStringColumns('Number of Temperatures',intToChar(N_TEMPERATURE))
      IF (N_MC_DONE/=1) THEN
        CALL printStringColumns('Will start from Temperature step',intToChar(N_MC_DONE))
      ENDIF
      CALL printStringColumns('Temperature scaling factor',TRIM(realToChar(SA_TEMP_SCALE)))
      CALL printStringColumns('MC Steps for each sample point',intToChar(N_THRESHOLD_STEPS))
      CALL printStringColumns('Freq Each Temperature Sampled',intToChar(N_SAMPLE_PTS))
      CALL printStringColumns('Number of runners',intToChar(N_RUNNERS))
      CALL printStringColumns('Quench MC Steps',intToChar(N_QUENCH_STEPS))
      CALL printStringColumns('Monte Carlo Stepsize',realToChar(R_BH_STEPSIZE))

    ELSEIF (JOB_TYPE == 7) THEN
      CALL printBanner('Energy Lid - go run thresholds and runners!','continue')
      CALL printHeaderCentred('')
      CALL printStringColumns('Population',intToChar(N_POPULATION))
      IF (N_PBC == 0) THEN
        CALL printStringColumns('Atoms in each cluster',intToChar(N_ATOMS))
       !CALL printStringColumns('centred at',coordToChar(R_CENTRE_CLUSTER))
      ELSEIF(N_PBC == 3) THEN
        CALL printStringColumns('Atoms in each unit cell',intToChar(N_ATOMS))
      ENDIF
      CALL printHeaderCentred('')
      CALL printStringColumns('Number of Energy Lids',intToChar(N_ENERGY_LIDS))
      IF (N_MC_DONE/=1) THEN
        CALL printStringColumns('Will start from Energy Lid',intToChar(N_MC_DONE))
      ENDIF
      CALL printStringColumns('Threshold steps of',TRIM(realToChar(R_ENERGY_LID))//' eV')
      CALL printStringColumns('Threshold MC Steps',intToChar(N_THRESHOLD_STEPS))
      CALL printStringColumns('Freq Each Lid Sampled',intToChar(N_SAMPLE_PTS))
      CALL printStringColumns('Number of runners',intToChar(N_RUNNERS))
      CALL printStringColumns('Quench MC Steps',intToChar(N_QUENCH_STEPS))
      CALL printStringColumns('Monte Carlo Stepsize',realToChar(R_BH_STEPSIZE))

    ELSEIF (JOB_TYPE == 8) THEN

      ! Testing
      CALL printBanner('RUNNING TESTING!','continue')

    ELSEIF (JOB_TYPE == 9) THEN

      CALL printBanner('Hybrid Genetic Production Run','continue')

    ELSE !Unknown job type
      CALL printBanner('Unknown job type, code = '//intToChar(JOB_TYPE),'continue')
      CALL printEmptyLine
      WRITE(stderr,*)'Unknown job type, code =',JOB_TYPE
      STOP
    ENDIF

    CALL printHeaderCentred('')
    CALL printHeaderLine
    CALL printEmptyLine

!   10 FORMAT(1X, A30, T35, I10)
!   20 FORMAT(1X, A30, T35, F10.3)
!   30 FORMAT(1X, A30, T35, A)
!   40 FORMAT(1X, A32, T35)
END SUBROUTINE displayControl

!==========================================================================================!

SUBROUTINE readJobs(inout_jobs, inout_total_jobs)
    CHARACTER(LEN=*), DIMENSION(:), INTENT(INOUT) :: inout_jobs
    INTEGER, INTENT(INOUT) :: inout_total_jobs
    INTEGER :: in_error=1, in_unit=26
    LOGICAL :: job_exist=.FALSE.
    CHARACTER(LEN=MAX_DIRECTORY_WIDTH) :: job_path=''
    CHARACTER(LEN=200) :: buffer=''
    inout_total_jobs = 0

    ! Allow user to redefine on comand line where input is found, defaulting to 'data'
    IF (command_argument_count() > 0) THEN
      CALL get_command_argument(1,job_path)

      !INQUIRE(FILE=TRIM(job_path), EXIST=job_exist)
      job_exist = inquireDirectory(job_path)
      IF(.NOT.job_exist) job_path = TRIM(DATA_FOLDER)//TRIM(job_path)
      job_exist = inquireDirectory(job_path)
      !INQUIRE(FILE=TRIM(job_path), EXIST=job_exist)
    ENDIF


    IF (.NOT.job_exist) job_path = TRIM(DATA_FOLDER)

    job_exist = inquireDirectory(job_path)
    !INQUIRE(FILE=TRIM(job_path), EXIST=job_exist)

    IF(job_exist) THEN
      DATA_FOLDER = TRIM(job_path)
    ELSE
      WRITE(stdout, *) 'Proc:',my_rank,'Job "'//TRIM(job_path)//'" does not exist.'
      CALL comms_finalize
      STOP
    END IF

    ! Write all job files to 'jobs'
    IF (MASTER_PROC) THEN
      CALL SYSTEM('ls -m1 "'//TRIM(DATA_FOLDER)//'" | grep ".job" > '&
      //TRIM(DATA_FOLDER)//'jobs')
    ENDIF

    CALL MPI_BARRIER(MY_COMM,in_error) !Sync processors

    ! Parse through all jobs for processing
    OPEN(UNIT=in_unit, FILE=TRIM(DATA_FOLDER)//'jobs', &
    STATUS='OLD', ACTION='READ', IOSTAT=in_error, IOMSG=ERROR_MSG)

    IF(in_error == 0) THEN
      REWIND(in_unit)
      DO
          READ(UNIT=in_unit,FMT='(A)', IOSTAT=in_error, IOMSG=ERROR_MSG) buffer
          IF(in_error == 0) THEN
              inout_total_jobs = inout_total_jobs + 1
              inout_jobs(inout_total_jobs) = buffer
          ELSE
              EXIT
          END IF
      END DO
      CLOSE(UNIT=in_unit)
    ELSE
      WRITE(stdout,*) ERROR_MSG
      WRITE(stdout,*) 'Proc:',my_rank,'Job "'//TRIM(job_path)//'" does not exist.'
      CALL comms_finalize
      STOP
    END IF

END SUBROUTINE readJobs

!==========================================================================================!

SUBROUTINE findRestartData() ! define N_SEEDS and restart_file()
    INTEGER :: in_error=1, in_unit=27
    CHARACTER(LEN=100) :: buffer=''
    INTEGER :: n,m
    LOGICAL :: l_exist
    N_SEEDS = 0

    IF (RESTART_FOLDER == '') THEN
       L_RESTART = .FALSE.
       RETURN ! LEN_TRIM(RESTART_FOLDER) > 0
    END IF
    
    CALL makeFolder(TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER), l_exist)

    IF (.NOT.l_exist) THEN
      CALL printHeaderCentred('Fresh start - created above folder for restart data')
      CALL printHeaderCentred('')
      RETURN
    ENDIF

    !INQUIRE(FILE=TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'/POP',EXIST=L_RESTART)
    L_RESTART = inquireDirectory(TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'/POP')

    ! Write all cluster file names to 'data-stack'
    CALL SYSTEM('ls -m1 "'//TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'" | grep ".xyz" > '&
    //TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'/data-stack')

    CALL SYSTEM('ls -m1 "'//TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'" | grep ".car" >> '&
    //TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'/data-stack')

    ! Write all bulk file names to 'data-stack'
    CALL SYSTEM('ls -m1 "'//TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'" | grep ".arc" >> '&
    //TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'/data-stack')

    ! Write all candidate file names to 'data-stack'
    CALL SYSTEM('ls -m1 "'//TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'" | grep ".can" >> '&
    //TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'/data-stack')

    INQUIRE(FILE=TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'/data-stack', EXIST=l_exist )
    IF (.NOT.l_exist) THEN
      WRITE(stderr,*)'No seeds (clusters) defined in '//TRIM(RESTART_FOLDER)
      IF (L_RESTART) THEN
        RETURN
      ELSE
        STOP
      ENDIF
    ENDIF

    ! Parse through all files for processing
    OPEN(UNIT=in_unit, FILE=TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'/data-stack', &
    STATUS='OLD', ACTION='READ', IOSTAT=in_error, IOMSG=ERROR_MSG)
    IF(in_error == 0) THEN
        REWIND(in_unit)
        DO
            READ(UNIT=in_unit,FMT='(A)', IOSTAT=in_error, IOMSG=ERROR_MSG) buffer
            IF (in_error == 0) THEN
                N_SEEDS = N_SEEDS + 1
                restart_file(N_SEEDS) = buffer
            ELSE
                EXIT
            END IF
        END DO
        CLOSE(UNIT=in_unit)
    ELSE
        WRITE(stderr,*) 'Read error in findRestartData'
        WRITE(stderr,*) ERROR_MSG
        STOP
    END IF

    INQUIRE(FILE=TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'/data-done', EXIST=l_exist )
    IF (l_exist) THEN ! remove any already done
      ! Parse through all restart files already completed
      OPEN(UNIT=in_unit, FILE=TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'/data-done', &
      STATUS='OLD', ACTION='READ', IOSTAT=in_error, IOMSG=ERROR_MSG)

      IF(in_error == 0) THEN
          REWIND(in_unit)
          DO
            READ(UNIT=in_unit,FMT='(A)', IOSTAT=in_error, IOMSG=ERROR_MSG) buffer
            IF (in_error == 0) THEN
              DO n = 1, N_SEEDS
                 IF (restart_file(n)==buffer) THEN
                   DO m = n+1, N_SEEDS
                     restart_file(m-1) = restart_file(m)
                   ENDDO
                   N_SEEDS = N_SEEDS - 1
                   EXIT
                 ENDIF
              ENDDO
              IF (N_SEEDS == 0) THEN
                WRITE(stdout,*)'All restart structures already processed on previous run'
                STOP
              ENDIF
            ELSE
              EXIT
            ENDIF
          END DO
          CLOSE(UNIT=in_unit)
      ELSE
          WRITE(stdout,*) 'Read error in findRestartData'
          WRITE(stdout,*) ERROR_MSG
          STOP
      ENDIF
    ENDIF

END SUBROUTINE findRestartData

!==========================================================================================!

SUBROUTINE getRestartData(inout_cluster, in_item_no)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    INTEGER, INTENT(IN) :: in_item_no
    CHARACTER(LEN=MAX_DIRECTORY_WIDTH) :: filename=''
    LOGICAL :: l_found
    INTEGER :: index_xyz, index_can, index_car
    LOGICAL :: pbc

    pbc=(index(UNIT_CELL,'cell').ne.0.OR.index(UNIT_CELL,'vector').ne.0)
    filename = TRIM(RESTART_FOLDER)//TRIM(restart_file(in_item_no))

    IF (pbc) THEN
      INQUIRE(FILE=TRIM(WORKING_DIR)//TRIM(filename), EXIST=l_found)
      IF (l_found) THEN
        WRITE(stdsee,*)'Uploading structural data from ',TRIM(filename)
        CALL retrieveData(inout_cluster, filename)
      ENDIF
    ELSE
      index_xyz = index(filename,'.xyz')
      index_can = index(filename,'.can')
      index_car = index(filename,'.car')

      IF (index_xyz /= 0) THEN

        IF (index_xyz /= 0) filename(index_xyz:index_xyz+3)='    '
        INQUIRE(FILE=TRIM(WORKING_DIR)//TRIM(filename)//'.xyz', EXIST=l_found)
        IF (l_found) THEN
          WRITE(stdsee,*)'Uploading previous cluster from ',TRIM(filename)//'.xyz as ', inout_cluster%id
          !CALL retrieveData(inout_cluster, filename)
          CALL readXYZ(inout_cluster, filename)
        ENDIF

      ELSEIF (index_can /= 0) THEN

        filename(index_can:index_can+3)='    '

        CALL inputCluster(inout_cluster, filename)

      ELSEIF (index_car /= 0) THEN

        IF (index_car /= 0) filename(index_car:index_car+3)='    '

        INQUIRE(FILE=TRIM(WORKING_DIR)//TRIM(filename)//'.car', EXIST=l_found)

        IF (l_found) THEN
          WRITE(stdsee,*)'Uploading previous cluster from ',TRIM(filename)//'.xyz as ', inout_cluster%id

          CALL readCAR(inout_cluster, filename)
        ENDIF

      ENDIF

    ENDIF

    !WRITE(stdout,*) 'Rename ' // TRIM(restart_file(in_item_no)) // ' as '//TRIM(inout_cluster%id)
END SUBROUTINE getRestartData

!==========================================================================================!

SUBROUTINE reportItemDone(in_item_no)
    INTEGER, INTENT(IN) :: in_item_no
    INTEGER :: in_unit=27 ! opened in readScan

    !INQUIRE(FILE=TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'data-done',EXIST=l_exist)
    !IF (.not.l_exist) THEN
    !  OPEN(UNIT=in_unit, &
    !  FILE=TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'data-done', &
    !  STATUS='NEW')
    !ENDIF
    !IF (in_item_no == 0) THEN
    !  CLOSE(in_unit)
    !ELSE
    !  WRITE(UNIT=in_unit,FMT='(A)') restart_file(in_item_no)
    !ENDIF
    OPEN(UNIT=in_unit, &
     FILE=TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'/data-temp', &
     STATUS='NEW')
    WRITE(UNIT=in_unit,FMT='(A)') restart_file(in_item_no)
    CALL SYSTEM('cat '//TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)// &
       'data-temp >> '//TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'/data-done')
    CALL SYSTEM('rm '//TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'/data-temp')
    CLOSE(in_unit)
END SUBROUTINE reportItemDone

!==========================================================================================!

SUBROUTINE readControl(job_filename)
    CHARACTER(LEN=*), INTENT(IN) :: job_filename
    INTEGER :: in_error, in_unit=26, position=0
    CHARACTER(LEN=200) :: buffer='', label=''
    INTEGER :: i_region, ide

    OPEN(UNIT=in_unit, FILE=TRIM(DATA_FOLDER)//job_filename, &
    STATUS='OLD', ACTION='READ', IOSTAT=in_error, IOMSG=ERROR_MSG)

    IF (in_error /= 0) THEN
       WRITE(stderr,*) 'Error opening',job_filename
       STOP
    END IF 

    REWIND(in_unit)
    DO
        READ(UNIT=in_unit,FMT='(A)', IOSTAT=in_error, IOMSG=ERROR_MSG) buffer
        IF(in_error == 0) THEN
            position = SCAN(buffer,':')
            IF(position == 0) THEN
                label = ''
                buffer = ''
            ELSE
                label = TRIM(buffer(1:position-1))
                buffer = buffer(position+1:)
            END IF
            SELECT CASE (label)
            ! Job type
            CASE('JOB_TYPE')
                READ(buffer,*) JOB_TYPE
            ! RANDOM seed  if SEED == 0 generate a seed
            CASE('SEED')
                READ(buffer,*) SEED
            ! Debug code
            CASE('DEBUG')
                READ(buffer,*) DEBUG_LEVEL
            ! Examine run
            CASE('OUTPUT')
                READ(buffer,*) OUTPUT_LEVEL

            ! General variabLes
            CASE('DIMENSIONS')
                READ(buffer,*) N_PBC
            CASE('RANDOM_START')
                READ(buffer,*) L_RANDOM_START
            CASE('COMPUTE_RDF')
                READ(buffer,*) L_COMPUTE_RDF
            CASE('R_RDF_CUTOFF')
                READ(buffer,*) R_RDF_CUTOFF
            CASE('R_RDF_SIGMA')
                READ(buffer,*) R_RDF_SIGMA
            CASE('ENERGY_ONLY')
                READ(buffer,*) L_ENERGY_ONLY
            CASE('ENFORCE_MASTER')
                READ(buffer,*) L_ENFORCE_MASTER
            CASE('COUNTER')
                READ(buffer,*) N_INDIVIDUALS
            CASE('POPULATION')
                READ(buffer,*) N_POPULATION
                
                IF (JOB_TYPE == 1 .AND. N_POPULATION > 1) THEN
                   WRITE(STDOUT,*) 'WARNING: Population greater than one not & 
                                   yet supported for JOB_TYPE = 1'
                   WRITE(STDOUT,*) 'Setting POPULATION = 1'
                   N_POPULATION = 1
                ENDIF
                
            CASE('N_ELITES')
                READ(buffer,*) N_ELITE_POP
            CASE('SAVE_FREQUENCY')
                READ(buffer,*) N_SAVE_FREQUENCY
            CASE('GLOBAL_MIN')
                READ(buffer,*) R_GLOBAL_MIN_VALUE
            CASE('R_MAX_CLUSTER_BOUNDARY')
                READ(buffer,*) R_MAX_CLUSTER_BOUNDARY(1)
                R_MAX_CLUSTER_BOUNDARY(2)=R_MAX_CLUSTER_BOUNDARY(1)
                R_MAX_CLUSTER_BOUNDARY(3)=R_MAX_CLUSTER_BOUNDARY(1)
            CASE('R_MAX_CLUSTER_X_BOUNDARY')
                READ(buffer,*) R_MAX_CLUSTER_BOUNDARY(1)
            CASE('R_MAX_CLUSTER_Y_BOUNDARY')
                READ(buffer,*) R_MAX_CLUSTER_BOUNDARY(2)
            CASE('R_MAX_CLUSTER_Z_BOUNDARY')
                READ(buffer,*) R_MAX_CLUSTER_BOUNDARY(3)
            CASE('X_CENTRE_CLUSTER')
                READ(buffer,*) R_CENTRE_CLUSTER(1)
            CASE('Y_CENTRE_CLUSTER')
                READ(buffer,*) R_CENTRE_CLUSTER(2)
            CASE('Z_CENTRE_CLUSTER')
                READ(buffer,*) R_CENTRE_CLUSTER(3)
            CASE('ENFORCE_CONTAINER')
                READ(buffer,*) L_ENFORCE_CONTAINER
            CASE('E_SURFACE')
                READ(buffer,*) L_SURFACE_ENERGY
            CASE('KEEP_ALLIGNMENT')
                READ(buffer,*) L_FIX_ALLIGNMENT
            CASE('MC_SURFACE')
                READ(buffer,*) L_ABOVE_SURFACE
            CASE('TRANS_CLUSTER')
                READ(buffer,*) PROB_TRANS_SURFACE
            CASE('ROTATE_CLUSTER')
                READ(buffer,*) PROB_ROTATE_SURFACE
            CASE('REMOVE')
                N_REMOVE=1
                READ(buffer,*) SPECIES_REMOVE(1)
                WRITE(stdout,*)SPECIES_REMOVE(1)
                STOP

            ! Constraints on cluster
            CASE('N_ATTEMPTS_RNDM_STRUCT')
                READ(buffer,*) N_ATTEMPTS_RNDM_STRUCT
            CASE('RNDM_INI_BY_ATOM')
                READ(buffer,*) RNDM_INI_BY_ATOM
            CASE('CHECK_ZERO_COORDINATION')
                READ(buffer,*) CHECK_ZERO_COORDINATION
            CASE('CHECK_FRAGMENTATION')
                READ(buffer,*) CHECK_FRAGMENTATION
            CASE('FRAGMENT')
                READ(buffer,*) R_FRAGMENT
            CASE('USE_VARIABLE_RAD_FRAG')
                READ(buffer,*) USE_VARIABLE_RAD_FRAG
            CASE('COLLAPSE')
                READ(buffer,*) R_COLLAPSE
            CASE('USE_VARIABLE_RAD_COLL')
                READ(buffer,*) USE_VARIABLE_RAD_COLL

            CASE('COLL_COEF_DIFF')
                READ(buffer,*) COLL_COEF_DIFF
            CASE('COLL_COEF_SAME')
                READ(buffer,*) COLL_COEF_SAME
            CASE('FRAG_COEF')
                READ(buffer,*) FRAG_COEF

            CASE('DSPECIES')
                READ(buffer,*) R_SPECIES
            CASE('L_RECENTRE_AFTER_EVAL')
                READ(buffer,*) L_RECENTRE_AFTER_EVAL
            CASE('L_CMPR_PMOI')
                READ(buffer,*) L_CMPR_PMOI

            ! Mutation options
            CASE('MUTATE_SWAP_PROP')
                READ(buffer,*) MUTATE_SWAP_PROP
            CASE('MUTATE_EXPAND_PROP')
                READ(buffer,*) MUTATE_EXPAND_PROP
            CASE('MUTATE_CONTRACT_PROP')
                READ(buffer,*) MUTATE_CONTRACT_PROP
            CASE('MUTATE_EXPAND_COEF')
                READ(buffer,*) MUTATE_EXPAND_COEF
            CASE('MUTATE_CONTRACT_COEF')
                READ(buffer,*) MUTATE_CONTRACT_COEF
            CASE('DIM_TOLERANCE')
                READ(buffer,*) DIM_TOLERANCE

            ! Initial Random Population Configuration
            CASE('L_APPLY_PRESSURE')
                READ(buffer,*) L_APPLY_PRESSURE
            CASE('C_ATOM_PRESSURE')
                READ(buffer,*) C_ATOM_PRESSURE
            CASE('I_TYPE_PRESSURE')
                READ(buffer,*) I_TYPE_PRESSURE

                IF ((I_TYPE_PRESSURE .LE. 1) .AND. (I_TYPE_PRESSURE .GE. 2)) THEN
                  WRITE(stdout,*) 'Unidentified distribution of pressure atoms'
                  STOP
                ENDIF
            CASE('R_RADIUS_PRESSURE')
                READ(buffer,*) R_RADIUS_PRESSURE
            CASE('ATOMSFILE_PRESSURE')
                READ(buffer,*) ATOMSFILE_PRESSURE

            ! The Grid
            CASE('GRID_ON')
                READ(buffer,*) GRID_ON
            CASE('GRID_NPOINTS_X')
                READ(buffer,*) GRID_NPOINTS_X
            CASE('GRID_NPOINTS_Y')
                READ(buffer,*) GRID_NPOINTS_Y
            CASE('GRID_NPOINTS_Z')
                READ(buffer,*) GRID_NPOINTS_Z
            CASE('GRID_RADIUS')
                READ(buffer,*) GRID_RADIUS

            ! Spawn remote job on host
            CASE ('RUN_REMOTE')
                READ(buffer,*) L_RUN_REMOTE
            CASE ('RUN_BATCH')
                READ(buffer,*) L_BATCH_SUBMISSION
            CASE ('HOST') 
                READ(buffer,*) HOST
                IF (INDEX(HOST,'archer')/=0) ARCHER=.TRUE.
            CASE ('USERNAME')
                READ(buffer,*) USER
            CASE ('BUDGET')
                READ(buffer,*) BUDGET
            CASE ('JOB_WORKING_DIR')
                READ(buffer,*) JOB_WORKING_DIR
            CASE ('HOURS' )
                 READ(buffer,*)HOURS
            CASE ('MINS')
                READ(buffer,*) MINS
            CASE ('SECS') 
                READ(buffer,*) SECS
            CASE ('PROCESSORS')
                READ(buffer,*) PROCESSORS
            CASE ('NTASKS')
                READ(buffer,*) NTASKS
            CASE ('JOB_COMPLETE')
                READ(buffer,*) JOB_COMPLETE
            CASE ('POLL_DELAY')
                READ(buffer,*) POLL_DELAY
            !NWChem variables
            CASE ('NWCHEM_BASIS_SET')
                READ(buffer,*) NWCHEM_BASIS_SET
            ! Energy Definition
            CASE('N_DEF_ENERGY')

              CALL findEnegyDefFunctions(buffer)

              IF (L_VASP_RUN) CALL ReadVASPInput

            CASE('N_RELAXATION_ATTEMPTS')
                READ(buffer,*) N_RELAXATION_ATTEMPTS
            CASE('GNORM')
                READ(buffer,*) R_GNORM_TOL
            CASE('R_MIN_CLUSTER_ENERGY')
                READ(buffer,*) R_ENERGY_MIN_THRESHOLD(1)
                DO ide=2,N_DEF_ENERGY
                  R_ENERGY_MIN_THRESHOLD(ide)=R_ENERGY_MIN_THRESHOLD(1)
                ENDDO
            CASE('R_MIN_CLUSTER_ENERGY_1')
                READ(buffer,*) R_ENERGY_MIN_THRESHOLD(1)
            CASE('R_MIN_CLUSTER_ENERGY_2')
                READ(buffer,*) R_ENERGY_MIN_THRESHOLD(2)
            CASE('R_MIN_CLUSTER_ENERGY_3')
                READ(buffer,*) R_ENERGY_MIN_THRESHOLD(3)
            CASE('R_MAX_CLUSTER_ENERGY')
                READ(buffer,*) R_ENERGY_MAX_THRESHOLD(1)
                DO ide=2,N_DEF_ENERGY
                  R_ENERGY_MAX_THRESHOLD(ide)=R_ENERGY_MAX_THRESHOLD(1)
                ENDDO
            CASE('R_MAX_CLUSTER_ENERGY_1')
                READ(buffer,*) R_ENERGY_MAX_THRESHOLD(1)
            CASE('R_MAX_CLUSTER_ENERGY_2')
                READ(buffer,*) R_ENERGY_MAX_THRESHOLD(2)
            CASE('R_MAX_CLUSTER_ENERGY_3')
                READ(buffer,*) R_ENERGY_MAX_THRESHOLD(3)
            CASE('R_REFINE_THRESHOLD')
                READ(buffer,*) R_REFINE_THRESHOLD(1)
                DO ide=2,N_DEF_ENERGY
                  R_REFINE_THRESHOLD(ide)=R_REFINE_THRESHOLD(1)
                ENDDO
            CASE('R_REFINE_THRESHOLD_1')
                READ(buffer,*) R_REFINE_THRESHOLD(1)
            CASE('R_REFINE_THRESHOLD_2')
                READ(buffer,*) R_REFINE_THRESHOLD(2)
            CASE('R_REFINE_THRESHOLD_3')
                READ(buffer,*) R_REFINE_THRESHOLD(3)
            CASE('ONLY_2ND_ENERGY')
                READ(buffer,*) L_ONLY_2ND_ENERGY
            CASE('NUMBER_BEST_CLUSTERS')
                READ(buffer,*) INT_MAX_BEST
                IF (INT_MAX_BEST > 999999999) THEN
                  WRITE(stdout,*)'Too many best clusters to find'
                  STOP
                ELSEIF (INT_MAX_BEST > 99999999) THEN
                  INT_BEST=9
                ELSEIF (INT_MAX_BEST > 9999999) THEN
                  INT_BEST=8
                ELSEIF (INT_MAX_BEST > 999999) THEN
                  INT_BEST=7
                ELSEIF (INT_MAX_BEST > 99999) THEN
                  INT_BEST=6
                ELSEIF (INT_MAX_BEST > 9999) THEN
                  INT_BEST=5
                ELSEIF (INT_MAX_BEST > 999) THEN
                  INT_BEST=4
                ELSEIF (INT_MAX_BEST > 99) THEN
                  INT_BEST=3
                ELSEIF (INT_MAX_BEST > 9) THEN
                  INT_BEST=2
                ELSEIF (INT_MAX_BEST < 0) THEN
                  WRITE(stdout,*)'Too few best clusters to find'
                  STOP
                ELSE
                  INT_BEST=1
                ENDIF
            CASE('LIMIT_BEST_FILES')
                READ(buffer,*) LIMIT_BEST
            CASE('R_BEST_CUTOFF')
                READ(buffer,*) R_BEST_EMAX
            CASE('R_SHELL_CORE_OFFSET')
                READ(buffer,*) R_SHELL_CORE_OFFSET


            ! Monte Carlo variables
            CASE('R_BH_STEPSIZE')
                READ(buffer,*) R_BH_STEPSIZE
            CASE('N_TEMPERATURE')
                READ(buffer,*) N_TEMPERATURE
            CASE('R_TEMPERATURE')
                READ(buffer,*) R_TEMPERATURE
            CASE('SA_TEMP_SCALE')
                READ(buffer,*) SA_TEMP_SCALE
            CASE('1ST_TEMPERATURE')
                READ(buffer,*) N_MC_DONE
            CASE('MC_TEMPERATURE_STEPS')
                READ(buffer,*) N_THRESHOLD_STEPS

            ! Basin Hopping variables
            CASE('N_MAX_BH_STEPS')
                READ(buffer,*) N_MAX_BH_STEPS
            CASE('N_BH_DYNAMIC_THRESHOLD')
                READ(buffer,*) N_BH_DYNAMIC_THRESHOLD
            CASE('N_BH_MOVECLASS_THRESHOLD')
                READ(buffer,*) N_BH_MOVECLASS_THRESHOLD
            CASE('BH_METHOD')
                READ(buffer,*) BH_METHOD
            CASE('BH_ACCEPT')
                READ(buffer,*) BH_ACCEPT
            CASE('N_HIGH_TEMPERATURE')
                READ(buffer,*) N_STEPS_HIGH_T
            CASE('N_LOW_TEMPERATURE')
                READ(buffer,*) N_STEPS_LOW_T
            CASE('MC_STEPS_ONLY')
                READ(buffer,*) L_MC_STEPS_ONLY
            CASE('PROB_SWITCH_ATOMS')
                READ(buffer,*) PROB_SWAP_ATOMS
            CASE('PROB_SWITCH_CATIONS')
                READ(buffer,*) PROB_SWAP_CATIONS
            CASE('PROB_MUTATE_CLUSTER')
                READ(buffer,*) PROB_MUTATE_CLUSTER
            CASE('PROB_TWIST_CLUSTER')
                READ(buffer,*) PROB_TWIST_CLUSTER
            CASE('PROB_MUTATE_CELL')
                READ(buffer,*) PROB_MC_UNITCELL

            !Genetic Algorithm variables
            CASE('N_GENERATIONS')
              READ(buffer,*) N_GENERATIONS
            CASE('N_TOURNAMENT_SIZE')
              READ(buffer,*) N_TOURNAMENT_SIZE
            CASE('R_REINSERT_ELITES_RATIO')
              READ(buffer,*) R_REINSERT_ELITES_RATIO
            CASE('R_POP_REPLACEMENT_RATIO')
              READ(buffer,*) R_POP_REPLACEMENT_RATIO

            CASE('R_ENERGY_TOLERANCE')
              READ(buffer,*) R_ENERGY_TOLERANCE
            CASE('R_PMOI_TOLERANCE')
              READ(buffer,*) R_PMOI_TOLERANCE

            CASE('GA_GEN_STATS')
              READ(buffer,*) GA_GEN_STATS
            CASE('GA_MUTATION_RATIO')
              READ(buffer,*) GA_MUTATION_RATIO
            CASE('GA_MUT_SELFCROSS_RATIO')
              READ(buffer,*) GA_MUT_SELFCROSS_RATIO
            CASE('GA_STEPSIZE')
              READ(buffer,*) GA_STEPSIZE
            CASE('GA_CROSS_CHECK')
              READ(buffer,*) GA_CROSS_CHECK
            CASE('GA_CROSS_ATTEMPTS')
              READ(buffer,*) GA_CROSS_ATTEMPTS
            CASE('GA_CROSS_1_2D_RATIO')
              READ(buffer,*) GA_CROSS_1_2D_RATIO
            CASE('GA_SAVE_POP_FREQ')
              READ(buffer,*) GA_SAVE_POP_FREQ
            CASE('GA_ENFORCE_MIN_SIZE')
              READ(buffer,*) GA_ENFORCE_MIN_SIZE
            CASE('GA_INI_POP_ATTEMPTS')
              READ(buffer,*) GA_INI_POP_ATTEMPTS

            CASE('HGA_POP')
              READ(buffer,*) HGA_POP
            CASE('HGA_CROSS_ATTEMPTS')
              READ(buffer,*) HGA_CROSS_ATTEMPTS
            CASE('HGA_FILE1')
              READ(buffer,*) HGA_FILE1
            CASE('HGA_FILE2')
              READ(buffer,*) HGA_FILE2

            ! Energy Lid and Threshold variables
            CASE('ENERGY_LIDS')
                READ(buffer,*) N_ENERGY_LIDS
            CASE('1ST_ENERGY_LID')
                READ(buffer,*) N_MC_DONE
            CASE('ENERGY_LID_STEP')
                READ(buffer,*) R_ENERGY_LID
            CASE('THRESHOLD_STEPS')
                READ(buffer,*) N_THRESHOLD_STEPS
            CASE('QUENCH_STEPS')
                READ(buffer,*) N_QUENCH_STEPS
            CASE('SAMPLE_PTS')
                READ(buffer,*) N_SAMPLE_PTS
            CASE('RUNNERS')
                READ(buffer,*) N_RUNNERS

            ! Springs Refinement variables
            CASE('K_TOL1')
                READ(buffer,*) K_TOL(1)
            CASE('K_TOL2')
                READ(buffer,*) K_TOL(2)
            CASE('K_DAMP')
                READ(buffer,*) K_DAMP

            ! Environment variables
            CASE('GULP_MASTER_NAME')
                READ(buffer,*) GULP_MASTER_NAME
            CASE('AIMS_MASTER_NAME')
                READ(buffer,*) AIMS_MASTER_NAME
            CASE('WORKING_DIR')
                READ(buffer,*) WORKING_DIR
            CASE('RELAXED_FOLDER')
                READ(buffer,*) RELAXED_FOLDER
            CASE('RESTART_FOLDER')
                READ(buffer,*) RESTART_FOLDER
            CASE('FINAL_FOLDER')
                READ(buffer,*) FINAL_FOLDER
            CASE('EXE_GULP_PATH')
                READ(buffer,*) EXE_GULP_PATH
            CASE('EXE_AIMS_PATH')
                READ(buffer,*) EXE_AIMS_PATH

            ! Dmol environment parameters
            CASE('DMOL_VERSION')
                READ(buffer,*) DMOL_VERSION

                IF ((DMOL_VERSION /= "3.2") .AND. (DMOL_VERSION /= "6.1")) THEN
                  WRITE(stdout,*) 'KLMC is not configured for the specified DMOL_VERSION'
                  STOP
                ENDIF

            CASE('DMOL_MASTER_NAME')
                READ(buffer,*) DMOL_MASTER_NAME
            CASE('DMOL_EXE_PATH')
                READ(buffer,*) DMOL_EXE_PATH
            CASE('DMOL_TAR_OUTPUT')
                READ(buffer,*) DMOL_TAR_OUTPUT
            CASE('DMOL_LICENCE_PATH')
                READ(buffer,*) DMOL_LICENCE_PATH
            CASE('DMOL_DATA_PATH')
                READ(buffer,*) DMOL_DATA_PATH
            CASE('DMOL_PATH')
                READ(buffer,*) DMOL_PATH
            CASE('DMOL_LD_PATH')
                READ(buffer,*) DMOL_LD_PATH
            CASE('DMOL_ACCELRYS_PACKDIR')
                READ(buffer,*) DMOL_ACCELRYS_PACKDIR
            CASE('DMOL_SAVE_INITIAL_STRUCT')
                READ(buffer,*) DMOL_SAVE_INITIAL_STRUCT

            ! Mode zero 0
            !   Datamining
            CASE('DM_FLAG')
                READ(buffer,*) DM_FLAG
            CASE('DM_REPLACE_ATOMS')
                READ(buffer,*) DM_REPLACE_ATOMS
            CASE('DM_NOEVAL_OUT_FORMAT')
                READ(buffer,*) DM_NOEVAL_OUT_FORMAT
            CASE('DM_RECENTRE')
                READ(buffer,*) DM_RECENTRE

            ! Solid Solution Rules
            CASE('FIX')
                READ(buffer,*)i_region
                IF (i_region>0 .AND. i_region <10) THEN
                  L_FIX_R(i_region)=.TRUE.
                ENDIF
            CASE('MIX')
                N_MIX = N_MIX + 1
                position = SCAN(buffer,'F')
                label = buffer(1:position-1)
                READ(label,*)T_MIX(N_MIX)%nexchg
                buffer = buffer(position+4:)
                position = SCAN(buffer,'T')
                label = buffer(1:position-1)
                READ(label,*)T_MIX(N_MIX)%from
                buffer = buffer(position+2:)
                READ(buffer,*)T_MIX(N_MIX)%to
            CASE('EXCHANGE')
                READ(buffer,*) N_MC_EXCHANGE
            CASE('MC_RESET')
                READ(buffer,*) L_MC_RESET

            CASE('SS_STATS')
              READ(buffer,*) SS_STATS
            CASE('SS_STATS_BACKUP')
              READ(buffer,*) SS_STATS_BACKUP
            CASE('SS_IMPORT_LIB')
              READ(buffer,*) SS_IMPORT_LIB
            CASE('SS_LIB_DIR')
              READ(buffer,*) SS_LIB_DIR
            CASE('SS_DELETE_FAILED')
              READ(buffer,*) SS_DELETE_FAILED
            CASE('SS_KEEP_ONLY_LOW')
              READ(buffer,*) SS_KEEP_ONLY_LOW
            CASE('SS_SKIP_EVALUATION')
              READ(buffer,*) SS_SKIP_EVALUATION
            CASE('SS_CHECK_GEOMETRY')
              READ(buffer,*) SS_CHECK_GEOMETRY
            CASE('SS_NO_FILES')
              READ(buffer,*) SS_NO_FILES

            ! Images Rules
            CASE('ADD_IMAGE')
                READ(buffer,*) L_IMAGE_ATOMS
            CASE('MIRROR_Z')
                WRITE(stdout,*)'Use default value of MIRROR_Z = 0.0'
                STOP
                READ(buffer,*) R_MIRROR_Z
            CASE('IMAGE')
                N_IMAGES = N_IMAGES + 1
                DEF_IMAGES(1,N_IMAGES)='  '
                DEF_IMAGES(1,N_IMAGES)='  '
                position = SCAN(buffer,'FOR')
                label = buffer(1:position-1)
                READ(label,*)DEF_IMAGES(1,N_IMAGES)
                label = buffer(position+4:)
                READ(label,*)DEF_IMAGES(2,N_IMAGES)

            ! Topological analysis variables
            CASE('L_USE_TOP_ANALYSIS')
              READ(buffer,*) L_USE_TOP_ANALYSIS

            CASE('HKG_PATH')
              READ(buffer,*) HKG_PATH

            CASE('C_HASHKEY_RADIUS')
              READ(buffer,*) C_HASHKEY_RADIUS

            CASE('HASHKEY_RADIUS_CONST')
              READ(buffer,*) HASHKEY_RADIUS_CONST

            CASE('HKG_STATS_FOLDER')
              READ(buffer,*) HKG_STATS_FOLDER

            END SELECT
        ELSE
            EXIT
        END IF
    END DO
    CLOSE(UNIT=in_unit)
END SUBROUTINE readControl


SUBROUTINE ReadVASPInput
   CHARACTER(LEN=10),PARAMETER :: vasp_input='input.vasp'
   INTEGER :: in_error=1, in_unit=77, position=0
   CHARACTER(LEN=100) :: buffer='', label=''

   OPEN(UNIT=in_unit, FILE=TRIM(DATA_FOLDER)//TRIM(vasp_input), &
   STATUS='OLD', ACTION='READ', IOSTAT=in_error, IOMSG=ERROR_MSG)

   IF (in_error /= 0) THEN
     WRITE(stderr,*) 'Error opening '//TRIM(vasp_input)
     STOP
   END IF

   REWIND(in_unit)

   DO
   READ(UNIT=in_unit,FMT='(A)',IOSTAT=in_error, IOMSG=ERROR_MSG) buffer
   IF(in_error == 0) THEN
      position = SCAN(buffer,':')
      IF(position == 0) THEN
        label=''
        buffer=''
      ELSE 
        label=TRIM(buffer(1:position-1))
        buffer=buffer(position+1:)
      END IF

      SELECT CASE (label)

      CASE ('EXECUTABLE')
        READ(buffer,*) EXEC
      CASE ('VASP_NSW')
        READ(buffer,*) VASP_NSW
      CASE ('MAXVASPSTEPS')
        READ(buffer,*) MAXVASPSTEPS
      CASE ('VASP_SYSTEM')
        READ(buffer,*) VASP_SYSTEM
      CASE ('VASP_SYSTEM_SIZE')
          READ(buffer,*) VASP_SYSTEM_SIZE
      END SELECT
   ELSE
     EXIT
   END IF
   END DO
   CLOSE(UNIT=in_unit)
   RETURN
END SUBROUTINE

!==========================================================================================!

SUBROUTINE initialiseEnvironment()
    IMPLICIT NONE

    INTEGER :: i,j,k,m,lm,hm,n
    LOGICAL :: l_exist, h_exist
    CHARACTER(LEN=MAX_DIRECTORY_WIDTH+MAX_FOLDER_WIDTH+8) :: foldername
    CHARACTER(LEN=4) :: old_idnum, new_idnum
    CHARACTER(LEN=10) :: HASH_ID_BEST
    CHARACTER(LEN=200) :: ERROR_MSG
    old_idnum(1:4)='    '
    new_idnum(1:4)='    '

    ! Create relax folder to store relaxation files
    foldername = TRIM(WORKING_DIR)//RELAXED_FOLDER
    !INQUIRE(FILE=TRIM(foldername), EXIST=l_exist )
    l_exist = inquireDirectory(TRIM(foldername))

    IF (l_exist) THEN
        CALL printStringColumns('Removing previous files in:',TRIM(foldername))
        CALL SYSTEM('rm -fR '//TRIM(foldername)//'*')
    ELSE
        CALL makeFolder(TRIM(foldername))
    END IF

    IF(L_AIMS_RUN) THEN
      foldername = TRIM(WORKING_DIR)//TRIM(DATA_FOLDER)//TRIM(AIMS_MASTER_NAME)
      INQUIRE(FILE=TRIM(foldername), EXIST=l_exist)
      IF (.NOT. l_exist) THEN
        WRITE(stderr,*)'Master Aims control file missing!'
        STOP
      ENDIF
    END IF

    IF (L_APPLY_PRESSURE) THEN
      IF (.NOT. lookForAtom(C_ATOM_PRESSURE)) THEN
        WRITE(stdout,*) 'Unidentified pressure atoms symbol (C_ATOM_PRESSURE)'
        STOP
      ENDIF
    ENDIF

    ! Create final folder to store ab initio files
    !foldername = TRIM(WORKING_DIR)//FINAL_FOLDER
    !INQUIRE(FILE=TRIM(foldername), EXIST=l_exist )
    !IF (l_exist) THEN
        !WRITE(stdout,*)'Removing previous files in ',TRIM(foldername)
        !CALL SYSTEM('rm -fR '//TRIM(foldername)//'*')
    !ELSE
        !CALL makeFolder(TRIM(foldername))
    !END IF

    IF (JOB_TYPE /= 4) THEN
    ! Create folder to store better cluster files
    foldername = TRIM(WORKING_DIR)//TRIM(BEST_FOLDER)
    CALL printHeaderCentred('')
    CALL printStringColumns('Now checking for files:',TRIM(foldername)//'statistics')
    CALL printStringColumns(' ',TRIM(foldername)//'energies')
    CALL printHeaderCentred('')

    INQUIRE(FILE=TRIM(foldername)//'statistics', EXIST=l_exist )

    h_exist = .FALSE.
    IF (L_USE_TOP_ANALYSIS) THEN
      ! check wether the hashkeys file exists
      INQUIRE(FILE=TRIM(foldername)//TRIM(HASHKEYS_FILE), EXIST=h_exist)
    ENDIF

    IF (l_exist) INQUIRE(FILE=TRIM(foldername)//'energies', EXIST=l_exist )
      IF (l_exist) THEN
         CALL printHeaderCentred('Found so now will read in current top candidates')
         CALL printHeaderCentred('')

         OPEN(UNIT=10,FILE=TRIM(foldername)//'statistics')
         OPEN(UNIT=20,FILE=TRIM(foldername)//'energies')

         IF (h_exist) THEN
            OPEN(UNIT=33, FILE=TRIM(foldername)//TRIM(HASHKEYS_FILE))
            READ(33,*) hm
         ENDIF

         READ(10,*)m !previous value of INT_MAX_BEST

         IF ( m > INT_MAX_BEST ) THEN
           CALL printHeaderCentred('It is strange that you now want to find fewer configurations!')
           WRITE(stderr,*)'It is strange that you now want to find fewer configurations!'
         ELSEIF ( m < 10 ) THEN
           lm=1
         ELSEIF ( m < 100 ) THEN
           lm=2
         ELSEIF ( m < 1000 ) THEN
           lm=3
         ELSEIF ( m < 10000 ) THEN
           lm=4
         ELSEIF ( m < 100000 ) THEN
           lm=5
         ELSEIF ( m < 1000000 ) THEN
           lm=6
         ELSEIF ( m < 10000000 ) THEN
           lm=7
         ELSEIF ( m < 100000000 ) THEN
           lm=8
         ELSEIF ( m < 1000000000 ) THEN
           lm=9
         ELSE
           lm=INT_BEST+1
         ENDIF

         IF (h_exist .AND. (hm .NE. m)) THEN

           ERROR_MSG = 'Error: number of configurations in the statistics file ' // &
             'does not match the number of configurations in the hashkeys file!'

           CALL printHeaderCentred(ERROR_MSG)
           WRITE(stderr,*) ERROR_MSG

           STOP
         ENDIF

         IF ( lm > INT_BEST ) STOP

         DO i=1,m ! old INT_MAX_BEST

           READ(10,*)j,ID_BEST(i),R_BEST_ENERGIES(i),N_BEST_ENERGIES(i)

           IF (index(ID_BEST(i),'A').eq.1) THEN
             READ(20,*)k,A_BEST_ENERGIES(i)
             IDE_BEST(i)=1

           ELSEIF (index(ID_BEST(i),'B').eq.1) THEN
             READ(20,*)k,A_BEST_ENERGIES(i),B_BEST_ENERGIES(i)
             IDE_BEST(i)=2

           ELSEIF (index(ID_BEST(i),'C').eq.1) THEN
             READ(20,*)k,A_BEST_ENERGIES(i),B_BEST_ENERGIES(i),C_BEST_ENERGIES(i)
             IDE_BEST(i)=3

           ELSEIF (index(ID_BEST(i),'D').eq.1) THEN
             READ(20,*)k,A_BEST_ENERGIES(i),B_BEST_ENERGIES(i),C_BEST_ENERGIES(i), &
               D_BEST_ENERGIES(i)
             IDE_BEST(i)=4

           ELSEIF (index(ID_BEST(i),'E').eq.1) THEN
             READ(20,*)k,A_BEST_ENERGIES(i),B_BEST_ENERGIES(i),C_BEST_ENERGIES(i), &
               D_BEST_ENERGIES(i),E_BEST_ENERGIES(i)
             IDE_BEST(i)=5

           ELSEIF (index(ID_BEST(i),'F').eq.1) THEN
             READ(20,*)k,A_BEST_ENERGIES(i),B_BEST_ENERGIES(i),C_BEST_ENERGIES(i), &
               D_BEST_ENERGIES(i),E_BEST_ENERGIES(i),F_BEST_ENERGIES(i)
             IDE_BEST(i)=6

           ELSE
             IDE_BEST(i)=0
           ENDIF

           IF (h_exist) THEN

             READ(33,*) n, HASH_ID_BEST, BEST_HASHKEYS(i)

             ! It might be excessive to check the IDs here

             IF (TRIM(ID_BEST(i)) .NE. TRIM(HASH_ID_BEST)) THEN

               ERROR_MSG = 'Error: ID number of ' // ID_BEST(i) // ' in the statistics file ' // &
                 'does not match the number of configurations in the hashkeys file!'

               CALL printHeaderCentred(ERROR_MSG)
               WRITE(stderr,*) ERROR_MSG

               STOP
             ENDIF
           ENDIF

           ! IF h_exist it should be generated??
           IF (index(ID_BEST(i),'x').eq.0) THEN
             WRITE(stdout,*)j,ID_BEST(i),R_BEST_ENERGIES(i),N_BEST_ENERGIES(i)
             IF ( lm /= INT_BEST ) THEN
               CALL num2str(j,lm,old_idnum)
               CALL num2str(j,INT_BEST,new_idnum)
               CALL SYSTEM('mv '//TRIM(foldername)//TRIM(ID_BEST(i))//'-'//TRIM(old_idnum)//'.xyz ' &
                                //TRIM(foldername)//TRIM(ID_BEST(i))//'-'//TRIM(new_idnum)//'.xyz ')
             ENDIF
           ENDIF
         ENDDO

         !IF (index(ID_BEST(INT_MAX_BEST),'x').ne.0) &
         !    WRITE(stdout,*)j,ID_BEST(INT_MAX_BEST),R_BEST_ENERGIES(INT_MAX_BEST),N_BEST_ENERGIES(i)

         R_BEST_EMAX=R_BEST_ENERGIES(INT_MAX_BEST)-R_BEST_EDIF

         DO i=m+1,INT_MAX_BEST
           ID_BEST(i)='x'
           IDE_BEST(i)=0
           R_BEST_ENERGIES(i)=R_BEST_EMAX
           N_BEST_ENERGIES(i)=0

           BEST_HASHKEYS(i) = 'x'
         ENDDO

         IF ( m < INT_MAX_BEST ) THEN
           REWIND(10)
           WRITE(10,*)INT_MAX_BEST
           DO i=1,INT_MAX_BEST
             WRITE(10,*)i,ID_BEST(i),R_BEST_ENERGIES(i),N_BEST_ENERGIES(i)
           ENDDO

           IF (h_exist) THEN
             REWIND(33)
             WRITE(33,*) INT_MAX_BEST

             DO i=1,INT_MAX_BEST
               WRITE(33,*) i, ID_BEST(i), BEST_HASHKEYS(i)
             ENDDO
           ENDIF
         ENDIF

         CLOSE(10)
         CLOSE(20)

         IF (h_exist) THEN
           CLOSE(33)
         ENDIF

      ELSE
         foldername = TRIM(WORKING_DIR)//TRIM(BEST_FOLDER)
         CALL makeFolder(TRIM(foldername))
         DO i=1,INT_MAX_BEST
           ID_BEST(i)='x'
           IDE_BEST(i)=0
           R_BEST_ENERGIES(i)=R_BEST_EMAX
           N_BEST_ENERGIES(i)=0

           IF (L_USE_TOP_ANALYSIS) THEN
              BEST_HASHKEYS(i) = 'x'
           ENDIF
         ENDDO
      END IF

    ENDIF

    IF (L_USE_TOP_ANALYSIS .AND. GA_GEN_STATS) THEN
      foldername = TRIM(WORKING_DIR)//TRIM(HKG_STATS_FOLDER)
      CALL makeFolder(TRIM(foldername))
    ENDIF

END SUBROUTINE initialiseEnvironment

!==========================================================================================!

SUBROUTINE readRestart
    LOGICAL :: restart_exist
    IF (RESTART_FOLDER/='') THEN
      INQUIRE(FILE=TRIM(RESTART_FOLDER)//'/restart-data', EXIST=restart_exist)
      !FILE=TRIM(DATA_FOLDER)//'../'//TRIM(RESTART_FOLDER)//'restart-data')
      IF (restart_exist) CALL readControl('../'//TRIM(RESTART_FOLDER)//'restart-data')
    ELSE
      WRITE(*,*) RESTART_FOLDER
    ENDIF
END SUBROUTINE readRestart

!==========================================================================================!

SUBROUTINE writeRestart
    INTEGER :: in_error, in_unit=26
    OPEN(UNIT=in_unit, FILE=TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'/restart-data', &
    STATUS='UNKNOWN', ACTION='WRITE', IOSTAT=in_error, IOMSG=ERROR_MSG)
    WRITE(in_unit,FMT='(a)')'COUNTER:'//intToChar(N_INDIVIDUALS)
    IF(L_RESTART)WRITE(in_unit,FMT='(a)')'RANDOM_START:FALSE'
    CLOSE(in_unit)
END SUBROUTINE writeRestart

  !==========================================================================================!
  ! A routine to find the energy functions to be used during the evualation
  !==========================================================================================!

  SUBROUTINE findEnegyDefFunctions(buffer)
    CHARACTER(*), INTENT(IN) :: buffer

    INTEGER :: pos_gulp = 0, pos_aims = 0, pos_vasp = 0, pos_nwchem = 0, pos_dmol = 0, pos_none = 0
    INTEGER :: postSt, posEnd, eDefCount, bufferLen
    LOGICAL :: CONTLOOP = .TRUE.
    CHARACTER(LEN=1) :: defEnCode

    eDefCount = 0
    postSt = 1
    posEnd = 0
    bufferLen = LEN(TRIM(buffer))

    DO WHILE(CONTLOOP)
      eDefCount = eDefCount + 1

      posEnd = SCAN(buffer(postSt:bufferLen),'-')

      IF(posEnd == 0) THEN
        posEnd = bufferLen
        CONTLOOP = .FALSE.
      ELSE
        posEnd = postSt+posEnd
      ENDIF

      pos_gulp   = INDEX(buffer(postSt:posEnd),'GULP')
      pos_aims   = INDEX(buffer(postSt:posEnd),'AIMS')
      pos_vasp   = INDEX(buffer(postSt:posEnd),'VASP')
      pos_nwchem = INDEX(buffer(postSt:posEnd),'NW')
      pos_dmol   = INDEX(buffer(postSt:posEnd),'DMOL')
      pos_none   = INDEX(buffer(postSt:posEnd),'NONE')

      IF(pos_gulp /= 0) THEN
        defEnCode = 'G'
        L_GULP_RUN = .TRUE.

      ELSEIF(pos_aims /= 0) THEN
        defEnCode = 'A'
        L_AIMS_RUN = .TRUE.

      ELSEIF(pos_vasp /= 0) THEN
        defEnCode ='V'
        L_VASP_RUN = .TRUE.

      ELSEIF(pos_nwchem /= 0) THEN
        defEnCode = 'N'
        L_NWCHEM_RUN = .TRUE.

      ELSEIF(pos_dmol /= 0) THEN
        defEnCode = 'D'
        L_DMOL_RUN = .TRUE.

      ELSEIF(pos_none /= 0) THEN
        defEnCode = 'X'
        L_NONE_RUN = .TRUE.

      ELSE
        WRITE(stderr,*)'Error in job file when defining ', eDefCount, ' energy'
        WRITE(stderr,*) postSt,posEnd,pos_gulp,pos_aims,pos_vasp,pos_nwchem,pos_dmol,pos_none, &
          DEF_ENERGY(eDefCount)
        STOP
      ENDIF

      DEF_ENERGY(eDefCount) = defEnCode

      postSt = posEnd
    ENDDO

    ! Checking the energy definitions setup
    IF ((L_NONE_RUN) .AND. (JOB_TYPE /= 0)) THEN
      WRITE(stderr,*) 'Error in job file when defining energy definitions!'
      WRITE(stderr,*) 'NONE definition is only allowed in JOB_TYPE:0!'
      STOP

    ELSEIF ((L_NONE_RUN) .AND. (eDefCount .GT. 1)) THEN
      WRITE(stderr,*) 'Error in job file when defining energy definitions!'
      WRITE(stderr,*) 'If NONE definition is used no other definitions are allowed!'
      WRITE(stderr,*) pos_gulp,pos_aims,pos_vasp,pos_nwchem,pos_dmol,pos_none,DEF_ENERGY(eDefCount)
      STOP

    ENDIF

    N_DEF_ENERGY = eDefCount

  END SUBROUTINE findEnegyDefFunctions

END MODULE Environment
