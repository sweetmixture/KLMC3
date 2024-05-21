MODULE Config
    IMPLICIT NONE
    SAVE

    !###############################################################################################
    ! CONSTANTS
    !###############################################################################################
    INTEGER, PARAMETER :: DP=SELECTED_REAL_KIND(15,307)
    INTEGER, PARAMETER :: DBL=SELECTED_REAL_KIND(15,307)
    INTEGER, PARAMETER :: MAX_ATOMS=10000             ! Maximum number of atoms allowed in cluster
    INTEGER, PARAMETER :: MASTER_FILE_WIDTH=200       ! Maximum character width per line of input file
    INTEGER, PARAMETER :: MAX_GULP_ROWS=10000
    INTEGER, PARAMETER :: MAX_GULP_EXTRA_ROWS=10000
    INTEGER, PARAMETER :: MAX_GULP_COLUMNS=12         ! Maximum columns in Gulp atoms input line
    INTEGER, PARAMETER :: MAX_GULP_COLUMN_WIDTH=20    ! Maximum characters per column
    INTEGER, PARAMETER :: MAX_GULP_WIDTH=200          ! Maximum characters per input line
    INTEGER, PARAMETER :: MAX_DIRECTORY_WIDTH=100     ! Maximum characters in directory path
    INTEGER, PARAMETER :: MAX_FOLDER_WIDTH=30         ! Maximum folder characters
    INTEGER, PARAMETER :: MAX_RDF_DIST=5000           ! Maximum number of RDF STEPS
    INTEGER, PARAMETER :: MAX_RDF_UDIST=50000         ! Maximum number of unique pairwise distances
    INTEGER, PARAMETER :: MAX_SPECIES=5               ! Maximum number of unique species
    INTEGER, PARAMETER :: MAX_RDF_PAIRS=36            ! Maximum number of unique pairs
    INTEGER, PARAMETER :: MAX_N_SEEDS=100000          ! Maximum number of seeded structures
    INTEGER, PARAMETER :: MAX_E_DEFN=6                ! Maximum number of energy definitions
    CHARACTER(LEN=1), PARAMETER :: INITIAL_PREFIX='X' ! Initial characters of file names
    CHARACTER(LEN=1), DIMENSION(MAX_E_DEFN) :: CLUSTER_PREFIX ! First character of file names
    REAL(KIND=dp),PARAMETER,PUBLIC    :: PI = 3.141592653589793238462643383279502884197_dp
    REAL(KIND=dp),PARAMETER,PUBLIC    :: twoPI = PI*2
    
    REAL(KIND=dp),PARAMETER,PUBLIC    :: Ha2eV = 27.2113850560606060606060606060606060606060_DP
    REAL(KIND=dp),PARAMETER,PUBLIC    :: deg2Rad = 0.01745329251994_DP
    REAL(KIND=dp),PARAMETER,PUBLIC    :: auToAngstr = 0.529177249_DP

    INTEGER, PARAMETER :: N_HASH_LEN=27               ! Length of a hashkey generated by Nauty (23?)
    REAL(KIND=dp), PARAMETER :: epsilon_x = 0.0000001
    !###############################################################################################
    ! Master File Array
    ! - used for saving master GULP file into memory
    ! - which allows for faster writing of GULP files
    !###############################################################################################
    CHARACTER(LEN=MASTER_FILE_WIDTH), DIMENSION(MAX_GULP_ROWS) :: MASTER_TOP=''
    CHARACTER(LEN=MASTER_FILE_WIDTH)                           :: MASTER_KEY=''
    CHARACTER(LEN=MAX_GULP_COLUMN_WIDTH), DIMENSION(MAX_ATOMS,MAX_GULP_COLUMNS) :: MASTER_ATOMS=''
    CHARACTER(LEN=MASTER_FILE_WIDTH), DIMENSION(MAX_GULP_EXTRA_ROWS) :: MASTER_LOCAL=''
    CHARACTER(LEN=MASTER_FILE_WIDTH), DIMENSION(MAX_GULP_ROWS) :: MASTER_BOTTOM=''
    CHARACTER(LEN=MASTER_FILE_WIDTH), DIMENSION(MAX_GULP_ROWS) :: MASTER_SECOND=''

    !###############################################################################################
    ! Output channels, errors and exception handling
    !###############################################################################################
    INTEGER,PARAMETER,PUBLIC  :: stderr = 99      ! Where all the MPI errors go
    INTEGER,PARAMETER,PUBLIC  :: stdout = 11      ! output file
    INTEGER,PARAMETER,PUBLIC  :: stdsee =  6      ! output progress
    INTEGER, PUBLIC :: ALLOCATE_STAT=1            ! status of allocated array
    INTEGER, PUBLIC :: EXCEPTION=0                ! 0 = No exceptions
    INTEGER, PUBLIC :: CLOCK_START=0              ! clock start time using SYSTEM_CLOCK
    CHARACTER(LEN=200), PUBLIC :: ERROR_MSG=''    ! general error message
    LOGICAL :: LOCKDOWN=.FALSE.                   ! protect restricted areas

    !###############################################################################################
    ! CONFIGURATION VARIABLES (global scope)
    ! Naming Convention
    ! N_ : integer
    ! R_ : real
    ! L_ : logical
    !###############################################################################################
    INTEGER, PUBLIC :: JOB_TYPE=0                   ! define job type 
                                                    ! 1=MCBH,2=GA,3=Solutions,4=Springs 
    INTEGER         :: DEBUG_LEVEL=0                ! level of debugging (0=off,1=limited,..)
    INTEGER         :: OUTPUT_LEVEL=0               ! level of output (0=off,1=limited,..)
    REAL(KIND=DBL)  :: R_ENERGY_ZERO=0.0_DP         ! the default energy value for any candidate
    REAL(KIND=DBL)  :: ZERO=0.0_DP                  ! absolute zero
    REAL(KIND=DBL)  :: dZERO=0.000000000001_DP      ! almost zero
    REAL(KIND=8)    :: KLMC_STARTTIME
    !###############################################################################################
    INTEGER         :: N_PBC=0                      ! total number of periodic boundary conditions
                                                    ! - set by user and influences what MC rountines to use
    INTEGER, PUBLIC :: N_ATOMS=0                    ! total number of atoms in cluster
                                                    ! - internally counted from master.gin
    INTEGER, PUBLIC :: N_CATIONS=0                  ! total number of cations in cluster
                                                    ! - internally counted from master.gin
    INTEGER, PUBLIC :: N_VAC=0                      ! total number of vacancies in cluster
                                                    ! - internally counted from master.gin
    INTEGER, PUBLIC :: N_SPECIES=0                  ! total number of unique species
                                                    ! - internally counted from master.gin
    INTEGER, PUBLIC :: N_EXTRA_ATOMS=0              ! total number of extra atoms
    INTEGER, PUBLIC :: N_PAIRS=0                    ! total number of unique pairs of species
    INTEGER, PUBLIC :: N_ARC_ATOMS=0                ! total number of atoms expected in arc files
    INTEGER, PUBLIC :: N_POPULATION=1               ! total number of clusters in population
    INTEGER, PUBLIC :: N_ELITE_POP=10               ! total number of clusters in elite arrays
    INTEGER, PUBLIC :: N_INDIVIDUALS=0              ! number of unique individuals generated
    INTEGER, PUBLIC :: N_RELAXATIONS=0              ! total number of geometry relaxation calls
    INTEGER, PUBLIC :: N_EVALUATIONS=0              ! total objective function evaluation calls
    INTEGER, PUBLIC :: N_SEEDS=0                    ! current number of structures to process or seeds to include
    INTEGER, PUBLIC :: SEED=0                       !IF SEED > 0 then NOT random
    LOGICAL, PUBLIC :: L_RESTART=.FALSE.            ! indicates whether or not this run is a restart
    INTEGER, PUBLIC :: N_RELAXATION_ATTEMPTS=1      ! maximum number of times GULP is called before giving up

    !GA VARIABLES
    INTEGER, PUBLIC :: N_GENERATIONS=1              ! number of genetic algorithm iterations
    INTEGER, PUBLIC :: N_SAVE_FREQUENCY=100         ! how often (#generations) to dump out data
    INTEGER, PUBLIC :: N_TOURNAMENT_SIZE=2
    INTEGER         :: N_MC_EXCHANGE=1              ! number of exchanges for mixing solution
    LOGICAL         :: L_MC_RESET=.FALSE.           ! allow an atom to be chosen more than once
    REAL(KIND=DBL),PUBLIC::R_REINSERT_ELITES_RATIO = 0.7 !between 0 and 1 ie:0.7=70% chance elite reinserted
    REAL(KIND=DBL), PUBLIC :: R_POP_REPLACEMENT_RATIO=0.5! between 0 and 1  % of old population replaced
    REAL(KIND=DBL), PUBLIC :: R_GLOBAL_MIN_VALUE    ! targeted minimum value for early exiting GA

    REAL(KIND=DBL), PUBLIC :: R_ENERGY_TOLERANCE = 0.001 ! min tolerance between energies for comparisons
    LOGICAL       , PUBLIC :: L_CMPR_PMOI = .FALSE.      ! A flag to turn on the comparison using principal moments of inertia of identical clusters
    REAL(KIND=DBL), PUBLIC :: R_PMOI_TOLERANCE   = 0.0   ! Tolerance for principal moments of inertia of identical clusters


    LOGICAL, PUBLIC        :: GA_GEN_STATS = .FALSE.  ! generates statistics of the GA run
    REAL(KIND=DBL), PUBLIC :: GA_MUTATION_RATIO = 1.0 ! the mutation ration after a crossover
    REAL(KIND=DBL), PUBLIC :: GA_MUT_SELFCROSS_RATIO = 0.0 !a ration to perform selfcrossover mutation
    REAL(KIND=DBL), PUBLIC :: GA_STEPSIZE = 1.0       ! an atom displacement stepsize during mutation
    LOGICAL, PUBLIC        :: GA_CROSS_CHECK = .FALSE.   ! check the validity of a cluster after a crossover
    INTEGER, PUBLIC        :: GA_CROSS_ATTEMPTS = 10000  ! a maximum number of crossover attempts
    REAL(KIND=DBL), PUBLIC :: GA_CROSS_1_2D_RATIO = 1.0  ! if two clusters are 1 or 2 D, use chromosomalCrossover_1D_2D
                                                         ! instead  with this ratio.
    INTEGER, PUBLIC        :: GA_ITER = 0                ! Current GA iteration number
    INTEGER, PUBLIC        :: GA_SAVE_POP_FREQ        ! Frequency to save pop during GA
    CHARACTER(LEN=3), PUBLIC :: GA_SAVE_POP_OUT_FORMAT = 'xyz' ! Population save file format
    REAL(KIND=DBL), PUBLIC :: GA_ENFORCE_MIN_SIZE     ! Minimum initial unique GA population as percentage of desired population
    INTEGER, PUBLIC        :: GA_INI_POP_ATTEMPTS=1     ! number of attempts to fully generate unique initial population

    !BH VARIABLES
    INTEGER, PUBLIC :: N_MAX_MC_STEPS=10            ! maximum Monte Carlo steps
    INTEGER, PUBLIC :: N_QUENCH_STEPS=10            ! maximum Monte Carlo Quench steps
    INTEGER, PUBLIC :: N_THRESHOLD_STEPS=10         ! number of Monte Carlo Threshold steps
    INTEGER, PUBLIC :: N_SAMPLE_PTS=5               ! number of sample points
    INTEGER, PUBLIC :: N_RUNNERS=2                  ! number of runners from a holding point
    INTEGER, PUBLIC :: N_MC_DONE=1                  ! starting energy lid or SA temperature number
    INTEGER, PUBLIC :: N_ENERGY_LIDS=10             ! number of energy lids
    REAL(KIND=DBL), PUBLIC :: R_ENERGY_LID=0.01_DP  ! current steps in lids
    REAL(KIND=DBL), PUBLIC :: R_THRESHOLD           ! current Threshold
    INTEGER, PUBLIC :: N_MAX_BH_STEPS=10            ! maximum basin hopping steps
    INTEGER, PUBLIC :: N_BH_DYNAMIC_THRESHOLD=20    ! dynamic basin hopping stepsize change threshold
    INTEGER, PUBLIC :: N_BH_MOVECLASS_THRESHOLD=50  ! dynamic basin hopping switch moveclass threshold
    INTEGER, PUBLIC :: N_TEMPERATURE=100            ! number of MC steps before TEMPERATURE lowered
    INTEGER, PUBLIC :: N_STEPS_LOW_T=1000           ! number of quenched steps before turning up T
    INTEGER, PUBLIC :: N_STEPS_HIGH_T=100           ! number of high T steps before reverting to quenches
    REAL(KIND=DBL), PUBLIC :: R_TEMPERATURE=100     ! temperature used in Metropolis criteria
    REAL(KIND=DBL), PUBLIC :: SA_TEMP_SCALE=0.99    ! temperature reduction used in Metropolis criteria
    REAL(KIND=DBL), PUBLIC :: R_BH_STEPSIZE=0.1          ! basin hopping step size
    REAL(KIND=DBL), PUBLIC :: R_MAX_CLUSTER_BOUNDARY(3)  ! the maximum size of cluster
    REAL(KIND=DBL), PUBLIC :: R_CENTRE_CLUSTER(3)        ! coords for origin of cluster
    REAL(KIND=DBL), PUBLIC :: R_SHELL_CORE_OFFSET=0.     ! adjustment for Metropolis Criterion
    REAL(KIND=DBL), PUBLIC :: R_GNORM_TOL=0.0001         ! Gnorm tol from GULP runs
    CHARACTER(LEN=10), PUBLIC :: BH_METHOD='RELAX'       ! defaults to Basin Hopping approach
    CHARACTER(LEN=10), PUBLIC :: BH_ACCEPT='METROPOLIS'  ! BH acceptance criteria
    !INTEGER, PUBLIC :: N_GULP_RUN=1                ! number of reqd GULP runs per energy calc
    INTEGER, PUBLIC :: N_GULP_L=0                   ! total number of line searches performed by GULP
    INTEGER, PUBLIC :: N_GULP_E=0                   ! total number of energy evaluations made by GULP
    INTEGER, PUBLIC :: N_GULP_dE=0                  ! total number of times energy gradients calculated by GULP
    INTEGER, PUBLIC :: N_GULP_d2E=0                 ! total number of times 2nd energy gradients calculated by GULP
    INTEGER, PUBLIC :: N_AIMS_RUNS=0
    INTEGER, PUBLIC :: N_DMOL_RUNS=0
    LOGICAL, PUBLIC :: L_ENERGY_ONLY=.FALSE.        ! update energy only (ignore new coords)
    LOGICAL, PUBLIC :: L_DOUBLE_RUN=.FALSE.         ! run GULP twice for each energy (IP)
    LOGICAL, PUBLIC :: L_ONLY_2ND_ENERGY=.FALSE.    ! only keep LM found from 2nd GULP run 
    LOGICAL, PUBLIC :: L_SHELLS_1ST=.FALSE.         ! use shell model on each first GULP run
    LOGICAL, PUBLIC :: L_SHELLS_2ND=.FALSE.         ! use shell model on each 2nd GULP run
    lOGICAL, PUBLIC :: L_SURFACE_ENERGY=.FALSE.     ! use surface rather then total energies
    LOGICAL, PUBLIC :: L_ENFORCE_CONTAINER=.TRUE.   ! enforce square container?
    lOGICAL, PUBLIC :: L_ABOVE_SURFACE=.FALSE.      ! standard MC move within a sphere or hemi-sphere?
    lOGICAL, PUBLIC :: L_IMAGE_ATOMS=.FALSE.        ! add image of cluster in metallic region
    REAL(KIND=DBL)  :: R_MIRROR_Z=0.                ! location of metallic surface
    REAL(KIND=DBL)  :: R_MIRROR_ZF=0.               ! location of metallic surface in fractional coords
    lOGICAL, PUBLIC :: L_MC_STEPS_ONLY=.FALSE.      ! only perform small MC steps (no special moves)
    lOGICAL, PUBLIC :: L_FIX_ALLIGNMENT=.FALSE.     ! rotate cluster only about z-axis (normal to surface)
    lOGICAL, PUBLIC :: L_COMPUTE_RDF=.FALSE.        ! compute rdf of arc files
    lOGICAL, PUBLIC :: L_COMPUTE_RDF_Z=.FALSE.      ! compute xy-plane rather than total rdf
    lOGICAL, PUBLIC :: L_HISTOGRAM=.TRUE.           ! compute histogram of rdf is sigma < 0.001
    lOGICAL, PUBLIC :: L_COMPUTE_OCC=.FALSE.        ! compute total site occupancy in each region
    lOGICAL, PUBLIC :: L_RANDOM_START=.FALSE.       ! start by randomising Master coordinates
    REAL(KIND=DBL), PUBLIC :: PROB_TRANS_SURFACE=0. ! probability of translating cluster along surface
    REAL(KIND=DBL), PUBLIC :: PROB_ROTATE_SURFACE=0. !probability of rotating cluster above surface
    REAL(KIND=DBL) :: PROB_SWAP_CATIONS=0.1         ! probability of swapping cations in BH
    REAL(KIND=DBL) :: PROB_SWAP_ATOMS=0.1           ! probability of swapping atoms in BH
    REAL(KIND=DBL) :: PROB_MUTATE_CLUSTER=0.2       ! probability of mutating cluster in BH
    REAL(KIND=DBL) :: PROB_TWIST_CLUSTER=0.2        ! probability of twisting cluster in BH
    REAL(KIND=DBL) :: PROB_MC_UNITCELL=0.3          ! prob of MC applied to unit cell in BH
    INTEGER, PUBLIC :: N_DEF_ENERGY=0               ! number of functions defining energy landscape
    CHARACTER(LEN=1), PUBLIC :: DEF_ENERGY(MAX_E_DEFN)  ! external software G=>GULP A=>AIMS
    LOGICAL, PUBLIC :: L_AIMS_RUN=.FALSE.           ! intention to call FHI-AIMS
    LOGICAL, PUBLIC :: L_GULP_RUN=.FALSE.           ! intention to call GULP
    LOGICAL, PUBLIC :: L_VASP_RUN=.FALSE.           ! intention to call VASP
    LOGICAL, PUBLIC :: L_NWCHEM_RUN=.FALSE.         ! intention to call NWCHEM
    LOGICAL, PUBLIC :: L_DMOL_RUN=.FALSE.           ! intention to call DMOL
    LOGICAL, PUBLIC :: L_NONE_RUN=.FALSE.           ! intention to call DMOL

    LOGICAL, PUBLIC :: FILLED_BEST_ARRAYS=.FALSE.   ! set to true once MAX_BEST clusters found
    INTEGER, PUBLIC :: NEW_THIS_RUN=0               ! number of new unique individuals in top set
    INTEGER,           PUBLIC :: INT_MAX_BEST=10    ! number of top clusters to look for
    INTEGER,           PUBLIC :: INT_BEST=2         ! number of characters required to describe ID
    INTEGER,           PUBLIC :: LIMIT_BEST=0       ! limit number of files for top and bottom clusters to keep
    CHARACTER(LEN=10), ALLOCATABLE, DIMENSION(:) :: ID_BEST         ! ID for top MAX_BEST clusters
    INTEGER,           ALLOCATABLE, DIMENSION(:) :: IDE_BEST        ! edefn for top MAX_BEST energies
    REAL(KIND=DBL),    ALLOCATABLE, DIMENSION(:) :: R_BEST_ENERGIES ! top MAX_BEST energies found on combined landscapes

    REAL(KIND=DBL),    ALLOCATABLE, DIMENSION(:) :: A_BEST_ENERGIES ! top MAX_BEST energies found on landscape A
    REAL(KIND=DBL),    ALLOCATABLE, DIMENSION(:) :: B_BEST_ENERGIES ! top MAX_BEST energies found on landscape B
    REAL(KIND=DBL),    ALLOCATABLE, DIMENSION(:) :: C_BEST_ENERGIES ! top MAX_BEST energies found on landscape C
    REAL(KIND=DBL),    ALLOCATABLE, DIMENSION(:) :: D_BEST_ENERGIES ! top MAX_BEST energies found on landscape D
    REAL(KIND=DBL),    ALLOCATABLE, DIMENSION(:) :: E_BEST_ENERGIES ! top MAX_BEST energies found on landscape E
    REAL(KIND=DBL),    ALLOCATABLE, DIMENSION(:) :: F_BEST_ENERGIES ! top MAX_BEST energies found on landscape F


    INTEGER,           ALLOCATABLE, DIMENSION(:) :: N_BEST_ENERGIES ! number of times found these top energies
    INTEGER                   :: ALLOCATE_STAT_1=1  ! used when allocating memory for ID_BEST
    INTEGER                   :: ALLOCATE_STAT_2=1  ! used when allocating memory for IDE_BEST
    INTEGER                   :: ALLOCATE_STAT_R=1  ! used when allocating memory for R_BEST_ENERGIES
    INTEGER                   :: ALLOCATE_STAT_N=1  ! used when allocating memory for N_BEST_ENERGIES

    INTEGER                   :: ALLOCATE_STAT_A=1  ! used when allocating memory for A_BEST_ENERGIES
    INTEGER                   :: ALLOCATE_STAT_B=1  ! used when allocating memory for B_BEST_ENERGIES
    INTEGER                   :: ALLOCATE_STAT_C=1  ! used when allocating memory for C_BEST_ENERGIES
    INTEGER                   :: ALLOCATE_STAT_D=1  ! used when allocating memory for D_BEST_ENERGIES
    INTEGER                   :: ALLOCATE_STAT_E=1  ! used when allocating memory for E_BEST_ENERGIES
    INTEGER                   :: ALLOCATE_STAT_F=1  ! used when allocating memory for F_BEST_ENERGIES

    REAL(KIND=DBL), DIMENSION(MAX_E_DEFN) :: R_ENERGY_MIN_THRESHOLD ! the minimum energy value for cluster
    REAL(KIND=DBL), DIMENSION(MAX_E_DEFN) :: R_ENERGY_MAX_THRESHOLD ! the minimum energy value for cluster
    REAL(KIND=DBL), DIMENSION(MAX_E_DEFN) :: R_REFINE_THRESHOLD     ! refine iff cluster energy better
    REAL(KIND=DBL), PUBLIC :: R_BEST_EMAX=1.0       ! below the energy cutoff of R_BEST_EMAX
    REAL(KIND=DBL), PUBLIC :: R_BEST_EDIF=0.0001    ! difference between clusters to be stored
    REAL(KIND=DBL), PUBLIC :: R_COLLAPSE=1.00000    ! minimum interatomic distance between all atoms
    REAL(KIND=DBL), PUBLIC :: R_SPECIES =1.50000    ! minimum interatomic distance between like species
    REAL(KIND=DBL)         :: R_FRAGMENT=3.00000    ! minimum distance between clusters

    LOGICAL                :: L_GULP_OPTI=.FALSE.   ! Master file directive (no Gnorm if single pt run)
    LOGICAL                :: L_ENFORCE_MASTER=.FALSE. ! Enforce atom types defined in Master
	
	  CHARACTER(LEN=5), PUBLIC :: DMOL_INPUT_PREFIX = 'dmol3', DMOL_INPUT_SUFFIX = 'input'
    LOGICAL, PUBLIC          :: DMOL_TAR_OUTPUT = .FALSE. ! A flag to tar and save dmol3 output


    INTEGER,        PUBLIC :: N_ATTEMPTS_RNDM_STRUCT = 100000   ! number of attempts to generate a random structure (cluster)
    LOGICAL,        PUBLIC :: RNDM_INI_BY_ATOM = .FALSE.        ! when generating a random structure atoms are added one by one and checked
    LOGICAL,        PUBLIC :: CHECK_ZERO_COORDINATION = .FALSE. ! flag to turn on the check for zero-coordinated atoms
                                                                ! during the validation of a cluster
    LOGICAL,        PUBLIC :: CHECK_FRAGMENTATION = .TRUE.      ! flag to turn on the check for fragmentation during
                                                                ! the validation of a cluster

    REAL(KIND=DBL), PUBLIC :: MUTATE_SWAP_PROP = 0.0     ! A proportion of mutations carried out by swaping atoms
    REAL(KIND=DBL), PUBLIC :: MUTATE_EXPAND_PROP = 0.0   ! A proportion of mutations carried out by expansion of the cluster
    REAL(KIND=DBL), PUBLIC :: MUTATE_CONTRACT_PROP = 0.0 ! A proportion of mutations carried out by contraction of the cluster
                                                         ! The rest is carried out by moving atoms

    REAL(KIND=DBL), PUBLIC :: MUTATE_EXPAND_COEF = 0.0   ! An expension coefficient
    REAL(KIND=DBL), PUBLIC :: MUTATE_CONTRACT_COEF = 0.0 ! A contraction coefficient
    REAL(KIND=DBL), PUBLIC :: DIM_TOLERANCE = 0.0

    LOGICAL,        PUBLIC :: L_RECENTRE_AFTER_EVAL = .FALSE. ! Flag to recentre cluster after evaluation

    LOGICAL,        PUBLIC :: USE_VARIABLE_RAD_FRAG = .FALSE. ! Flag to use variable radii for the fragmentation check
    LOGICAL,        PUBLIC :: USE_VARIABLE_RAD_COLL = .FALSE. ! Flag to use variable radii for the collapse check
    REAL(KIND=DBL), PUBLIC :: COLL_COEF_DIFF = 0.8 ! Collapse coefficient for different species
    REAL(KIND=DBL), PUBLIC :: COLL_COEF_SAME = 0.9 ! Collapse coefficient for same species
    REAL(KIND=DBL), PUBLIC :: FRAG_COEF     = 1.15 ! Fragmentation coefficient

    !###############################################################################################
    ! Datamining Configuration
    !###############################################################################################
    LOGICAL,                          PUBLIC :: DM_FLAG = .FALSE. ! Flag to swap atoms and recentre (Data mining)
    CHARACTER(LEN=100),               PUBLIC :: DM_REPLACE_ATOMS = ''
    CHARACTER(LEN=1)                         :: DM_ATOMS_SEP_SYM = ';', DM_ATOMS_CHNG_SYM = '-'
    CHARACTER(LEN=2), DIMENSION(MAX_SPECIES) :: DM_ATOMS_FROM, DM_ATOMS_TO
    INTEGER                                  :: DM_ATOMS_SWAP_CNT = 0
    REAL(KIND=DBL)                           :: DM_COORD_COEF = 0.0
    CHARACTER(LEN=3),                 PUBLIC :: DM_NOEVAL_OUT_FORMAT = ''
    LOGICAL,                          PUBLIC :: DM_RECENTRE = .FALSE.

    !###############################################################################################
    ! Hybrid Genetic Production Run Configuration
    !###############################################################################################
    INTEGER,            PUBLIC :: HGA_POP = 10000
    INTEGER,            PUBLIC :: HGA_CROSS_ATTEMPTS=1000
    CHARACTER(LEN=100), PUBLIC :: HGA_FILE1, HGA_FILE2



    !###############################################################################################
    ! Initial Random Population Configuration
    !###############################################################################################

    LOGICAL,          PUBLIC :: L_APPLY_PRESSURE      ! Flag to create
    CHARACTER(LEN=2), PUBLIC :: C_ATOM_PRESSURE       ! Pressure atom symbol
    INTEGER,          PUBLIC :: I_TYPE_PRESSURE       ! How the atoms should be placed around a cluster
    REAL(KIND=DBL),   PUBLIC :: R_RADIUS_PRESSURE     ! Radius at which pressure atoms will be distributed
    CHARACTER(LEN=MAX_FOLDER_WIDTH), PUBLIC :: ATOMSFILE_PRESSURE = 'pressureAtoms.xyz' ! If I_TYPE_PRESSURE = 2
                                                      ! the pressure atoms posstions are read from this file

    !###############################################################################################
    ! REMOTE JOB VARIABLES
    !###############################################################################################
    LOGICAL                :: L_RUN_REMOTE=.FALSE.  ! Run KLMC's DFT on remote computer
    LOGICAL                :: L_BATCH_SUBMISSION = .FALSE.
    LOGICAL                :: ARCHER=.FALSE.
    CHARACTER(LEN=81)      :: HOST='Localhost'
    CHARACTER(LEN=81)      :: USER='mfarrow'
    CHARACTER(LEN=81)      :: BUDGET='e05-qmdev-smw'
    CHARACTER(LEN=81)      :: JOB_WORKING_DIR='$(HOME)/klmc_run'
    CHARACTER(LEN=81)      :: NWCHEM_BASIS_SET='6-311g'
    CHARACTER(LEN=81)      :: VASP_SYSTEM=''
    CHARACTER(LEN=81)      :: VASP_SYSTEM_SIZE=''
    INTEGER                :: MAXVASPSTEPS=100
    INTEGER                :: VASP_NSW=100
    INTEGER                :: HOURS=0  !Time to run remote jobs
    INTEGER                :: MINS=20
    INTEGER                :: SECS=0
    INTEGER                :: PROCESSORS=32   !How many procs to run remote job
    INTEGER                :: NTASKS=24
    INTEGER                :: POLL_DELAY=600
    CHARACTER(LEN=81)      :: JOB_COMPLETE="Job has finished"
    CHARACTER(LEN=256)     :: EXEC="vasp"
    INTEGER                :: N_REMOVE=0            ! number of species to remove from inputs
    CHARACTER(LEN=2)       :: SPECIES_REMOVE(1)     ! the name of the species to be removed
    CHARACTER(LEN=2)       :: MASTER_SPECIES(MAX_SPECIES) ! the names of the unique species
    REAL(KIND=DBL)         :: R_RDF_SIGMA=0.1_DP    ! sigma used in Gaussian smearing atoms
    REAL(KIND=DBL)         :: R_RDF_ZIGMA=1.0_DP    ! sigma used in Gaussian smearing plane
    REAL(KIND=DBL)         :: R_RDF_Z=0.0_DP        ! expected value used in Gaussian plane
    REAL(KIND=DBL)         :: R_RDF_ZERO=0.0_DP     ! minimum value for r used in g(r) output
    REAL(KIND=DBL)         :: R_RDF_STEP            ! distance between r used in g(r) output
    INTEGER                :: N_RDF_STEPS=MAX_RDF_DIST ! number of data points for g(r) output
    REAL(KIND=DBL)         :: R_RDF_CUTOFF=15.0_DP  ! interatomic cutoff when calculating RDF
    REAL(KIND=DBL)         :: R_RDF_ACC=0.00001_DP  ! minimum interatomic distance calculating RDF
    INTEGER                :: N_IMAGES=0            ! number of different types of images
    CHARACTER(LEN=2), DIMENSION(2,3) :: DEF_IMAGES  ! definition of images

    !###############################################################################################
    ! Springs
    !###############################################################################################
    CHARACTER(LEN=2), DIMENSION(3), PUBLIC   :: MASTER_spring_type
    CHARACTER(LEN=1), DIMENSION(6), PUBLIC   :: CELL_FLAGS
    CHARACTER(LEN=1), DIMENSION(6), PUBLIC   :: VECTOR_FLAGS
    CHARACTER(LEN=10),              PUBLIC   :: COORD_TYPE='          '
    CHARACTER(LEN=7),               PUBLIC   :: UNIT_CELL='       '
    INTEGER, DIMENSION(3),          PUBLIC   :: MASTER_n
    REAL(KIND=DBL), DIMENSION(3),   PUBLIC :: MASTER_k
    REAL(KIND=DBL), DIMENSION(3),   PUBLIC :: MASTER_V
    REAL(KIND=DBL), DIMENSION(3),   PUBLIC :: MASTER_P
    REAL(KIND=DBL),                 PUBLIC :: ONSITE_V
    REAL(KIND=DBL),                 PUBLIC :: ONSITE_N
    REAL(KIND=DBL),                 PUBLIC :: K_DAMP=1.0_DP
    REAL(KIND=DBL), DIMENSION(2),   PUBLIC :: K_TOL
    REAL(KIND=DBL), DIMENSION(3,3), PUBLIC :: VECTOR
    REAL(KIND=DBL), DIMENSION(6),   PUBLIC :: MASTER_CELL

    !###############################################################################################
    ! Environment Variables (global scope)
    !###############################################################################################
    CHARACTER(LEN=MAX_FOLDER_WIDTH), PUBLIC :: DATA_FOLDER='data/'
    CHARACTER(LEN=MAX_FOLDER_WIDTH), PUBLIC :: RELAXED_FOLDER=''
    CHARACTER(LEN=MAX_FOLDER_WIDTH), PUBLIC :: RESTART_FOLDER=''
    CHARACTER(LEN=MAX_FOLDER_WIDTH), PUBLIC :: FINAL_FOLDER=''
    CHARACTER(LEN=MAX_FOLDER_WIDTH), PUBLIC :: BEST_FOLDER='top_structures/'
    CHARACTER(LEN=MAX_FOLDER_WIDTH), PUBLIC :: AIMS_MASTER_NAME='control.in'
    CHARACTER(LEN=MAX_FOLDER_WIDTH), PUBLIC :: GULP_MASTER_NAME=''
    CHARACTER(LEN=MAX_FOLDER_WIDTH), PUBLIC :: DMOL_MASTER_NAME=''
    CHARACTER(LEN=MAX_DIRECTORY_WIDTH), PUBLIC :: WORKING_DIR=''
    CHARACTER(LEN=MAX_DIRECTORY_WIDTH), PUBLIC :: EXE_GULP_PATH=''
    CHARACTER(LEN=MAX_DIRECTORY_WIDTH), PUBLIC :: EXE_AIMS_PATH=''

    CHARACTER(LEN=3)             , PUBLIC :: DMOL_VERSION
    CHARACTER(LEN=MAX_GULP_WIDTH), PUBLIC :: DMOL_EXE_PATH=''
    CHARACTER(LEN=MAX_GULP_WIDTH), PUBLIC :: DMOL_LICENCE_PATH=''
    CHARACTER(LEN=MAX_GULP_WIDTH), PUBLIC :: DMOL_DATA_PATH=''
    CHARACTER(LEN=MAX_GULP_WIDTH), PUBLIC :: DMOL_PATH=''
    CHARACTER(LEN=MAX_GULP_WIDTH), PUBLIC :: DMOL_LD_PATH=''
    CHARACTER(LEN=MAX_GULP_WIDTH), PUBLIC :: DMOL_ACCELRYS_PACKDIR=''
    LOGICAL,                       PUBLIC :: DMOL_SAVE_INITIAL_STRUCT=.FALSE.

    !###############################################################################################
    !       MPI VARIABLES
    !###############################################################################################
    INTEGER,                        PUBLIC :: NUM_PROCS_IN_FARM=1
    LOGICAL,                        PUBLIC :: MASTER_PROC=.FALSE.

    !###############################################################################################
    !       TOPOLOGICAL ANALYSIS. HASHKEY AND NAUTY VARIABLES
    !###############################################################################################
    LOGICAL,            PUBLIC :: L_USE_TOP_ANALYSIS      ! Flag to turn on the topoligcal analysis with Nauty
    CHARACTER(LEN=2),   PUBLIC :: C_HASHKEY_RADIUS = 'IR'   ! Radius to generate hashkeys (Options: CR - covalent, IR - ionic)
    INTEGER            :: ALLOCATE_STAT_HASH=1  ! Used when allocating memory for BEST_HASHKEYS
    REAL(KIND=DBL)     :: hashRadius
    REAL(KIND=DBL)     :: HASHKEY_RADIUS_CONST

    CHARACTER(LEN=MAX_DIRECTORY_WIDTH)           :: HASHKEYS_FILE = 'hashkeys'  ! File name for saving hashkeys
    CHARACTER(LEN=MAX_DIRECTORY_WIDTH),   PUBLIC :: HKG_PATH = ''  ! Path to Nauty wrapper
    CHARACTER(LEN=MAX_DIRECTORY_WIDTH),   PUBLIC :: HKG_STATS_FOLDER = 'hashkey_stats/'

    CHARACTER(LEN=N_HASH_LEN), ALLOCATABLE, DIMENSION(:), PUBLIC :: BEST_HASHKEYS ! Top MAX_BEST hashkeys

    INTEGER, PUBLIC :: BLACKLIST_HASHKEYS_CNT = 0
    INTEGER,          PARAMETER :: MAX_BLACKLIST_HASHKEYS_CNT=1000000
    CHARACTER(LEN=1), PARAMETER :: UNDEFINED_HASHKEY = 'X'

    CHARACTER(LEN=N_HASH_LEN), ALLOCATABLE, DIMENSION(:), PUBLIC :: BLACKLIST_HASHKEYS

    !###############################################################################################
    ! THE SOLID SOLUTIONS VARIABLES
    !###############################################################################################

    LOGICAL,                          PUBLIC :: SS_STATS = .FALSE. ! a flag to generate statistics after the run
    INTEGER,                          PUBLIC :: SS_STATS_BACKUP = 0 ! dump stats every SS_STATS_BACKUP steps
    LOGICAL,                          PUBLIC :: SS_IMPORT_LIB = .FALSE. !a flag to import previous structures
                                                                  ! L_USE_TOP_ANALYSIS must be turned on too
    CHARACTER(LEN=MASTER_FILE_WIDTH), PUBLIC :: SS_LIB_DIR='' ! a path to a directory where structures
                                                                  ! from previous runs a kept
    LOGICAL,                          PUBLIC :: SS_SKIP_EVALUATION = .FALSE. !a flag to switch off energy evaluation
    LOGICAL,                          PUBLIC :: SS_NO_FILES = .FALSE. ! a flag to switch off saving structures into files

    INTEGER :: SS_UNIQUE_CNT = 0

    INTEGER :: PREV_HASHKEYS_CNT = 0
    CHARACTER(LEN=N_HASH_LEN), ALLOCATABLE, DIMENSION(:) :: PREV_HASHKEYS

    INTEGER :: GEN_HASHKEYS_CNT = 0
    CHARACTER(LEN=N_HASH_LEN), ALLOCATABLE, DIMENSION(:) :: GEN_HASHKEYS

    CHARACTER(LEN=5), PUBLIC :: SS_FILEIN_PREFIX = ''
    CHARACTER(LEN=5), PUBLIC :: SS_FILEIN_SUFFIX = '*.xyz'
    LOGICAL,          PUBLIC :: SS_DELETE_FAILED = .FALSE. ! a flag to delete cluster files of failed attempts
    LOGICAL,          PUBLIC :: SS_KEEP_ONLY_LOW = .FALSE. ! a flag to keep only lowest energy structures
    LOGICAL,          PUBLIC :: SS_CHECK_GEOMETRY = .FALSE. ! a flag to check the geometry of clusters

    CHARACTER(LEN=50), PUBLIC :: SS_HASHKEYS_STATS_FILE = 'ssHashkeyStatistics'

    !###############################################################################################
    ! ATOMS INFO
    !###############################################################################################

    CHARACTER(LEN=MAX_FOLDER_WIDTH), PUBLIC :: INPUT_FOLDER='input/'
    CHARACTER(LEN=MAX_FOLDER_WIDTH), PUBLIC :: ATOMS_FILE_NAME='atoms.in'

    INTEGER :: ATOMS_CNT = 0
    INTEGER, ALLOCATABLE, DIMENSION(:)          :: ATOMS_ANUM
    CHARACTER(LEN=2), ALLOCATABLE, DIMENSION(:) :: ATOMS_SYM
    REAL(KIND=DBL), ALLOCATABLE, DIMENSION(:)   :: ATOMS_AMASS, ATOMS_COVR, ATOMS_IONR

    !###############################################################################################
    ! THE GRID VARIABLES
    !###############################################################################################

    LOGICAL,            PUBLIC :: GRID_ON           ! Flag to turn on the topoligcal analysis with Nauty
    INTEGER,            PUBLIC :: GRID_NPOINTS_X, & ! Number of grid points in X direction
                                  GRID_NPOINTS_Y, & ! Number of grid points in Y direction
                                  GRID_NPOINTS_Z    ! Number of grid points in Z direction
    REAL(KIND=DBL),     PUBLIC :: GRID_RADIUS       !

    REAL(KIND=DBL), ALLOCATABLE, DIMENSION(:, :) :: PAIR_FRAGMENT_RADIUS
    REAL(KIND=DBL), ALLOCATABLE, DIMENSION(:, :) :: PAIR_COLLAPSE_RADIUS



    !###############################################################################################
    ! DERIVED TYPES
    ! atom      : atomic type, cartesain coordinates
    ! clusters  : a collection of atoms
    !###############################################################################################
    TYPE, PUBLIC :: atom
        CHARACTER(LEN=2) :: symbol='--'
        CHARACTER(LEN=1) :: cstype='-'
        REAL(KIND=DBL) :: xc=0.00_dp, yc=0.00_dp, zc=0.00_dp
        REAL(KIND=DBL) :: xf=0.00_dp, yf=0.00_dp, zf=0.00_dp
        INTEGER :: site=0
        REAL(KIND=DBL) :: pot=0.00_dp
        REAL(KIND=DBL) :: k=0.00_dp
        REAL(KIND=DBL) :: charge = 1.00_dp
    END TYPE atom

    TYPE, PUBLIC :: GA_STATS
      CHARACTER(LEN=6)  :: gaOrigin = '------'       ! Origin of a cluster
      INTEGER           :: gaRandomAttempts = 0      ! Number of random atoms attemps
      INTEGER           :: gaCrossAttempts = 0       ! Number of crossover attempts
      CHARACTER(LEN=10) :: gaCrossParent1 = ""
      CHARACTER(LEN=10) :: gaCrossParent2 = "" ! Crossover parents
      INTEGER           :: gaMutateAttempts = 0      ! Number of mutation attemps
      INTEGER           :: gaMutateSwapCnt = 0       ! Number of atoms swaped during the mutation
      INTEGER           :: gaMutateDisplCnt = 0      ! Number of atoms displaced during the mutation
      INTEGER           :: gaRepopAttempts = 0       ! Number of repopulation attempts
      INTEGER           :: gaGenNo = 0               ! The GA generation number on which the cluster was created
      INTEGER           :: gaNoOfOccur = 1           ! Number of occurances of the cluster

    END TYPE GA_STATS

    TYPE, PUBLIC :: cluster
      CHARACTER(LEN=10) :: id=INITIAL_PREFIX//'0'     ! identity number of the cluster
      INTEGER :: edefn                                ! definition of energy
      REAL(KIND=DBL), DIMENSION(0:MAX_E_DEFN)::energy ! ground-state energy of the cluster
      REAL(KIND=DBL), DIMENSION(MAX_E_DEFN) :: gnorm  ! measure of how relaxed the cluster is
      REAL(KIND=DBL), DIMENSION(6) :: box             ! cell parameters a,b,c,alpha,beta,gamma
      TYPE(atom), DIMENSION(MAX_ATOMS) :: atoms       ! array of atoms

      LOGICAL :: relaxFailed = .FALSE.
      CHARACTER(LEN=N_HASH_LEN) :: hashkey1st = UNDEFINED_HASHKEY
      CHARACTER(LEN=N_HASH_LEN) :: hashkey    = UNDEFINED_HASHKEY !cannonical labelling of a cluster aka hashkey
      INTEGER :: hashkeyMatchNo = 0, hashkeyMatchIter = -1

      TYPE(GA_STATS) :: ga_stats                      ! gathering ga statistics
      REAL(KIND=DBL), DIMENSION(3) :: pmoi            ! principle moments of inertia
      REAL(KIND=DBL), DIMENSION(3) :: pmog            ! principle moments of geometry :)
      INTEGER :: status = 1                           ! cluster status, 1 - ok, 0 - cluster is duff

    END TYPE cluster

    TYPE, PUBLIC :: mixdefn
        INTEGER :: nexchg=0                          ! number of exchanges
        INTEGER :: from=0                            ! from this site 
        CHARACTER(LEN=9) :: to='0'                   ! to these sites
    END TYPE mixdefn

    TYPE(mixdefn), DIMENSION(9) :: T_MIX             ! max of 9 mixing rules
    LOGICAL :: L_FIX_R(9)                            ! prevent mixing within each of 9 regions
    INTEGER :: N_MIX                                 ! number of mixing rules
    INTEGER :: N_REGIONS                             ! number of regions
    INTEGER, DIMENSION(0:9) :: N_ATOMS_R             ! number of sites within each region

    TYPE, PUBLIC :: pair
        CHARACTER(LEN=5) :: name='  -  '
        INTEGER :: ndist=0
        REAL(KIND=DBL), DIMENSION(MAX_RDF_UDIST) :: rdist
        REAL(KIND=DBL), DIMENSION(MAX_RDF_UDIST) :: gr
    END TYPE pair

    TYPE, PUBLIC :: rdf
        REAL(KIND=DBL)                       :: energy
        INTEGER                              :: ndegen
        INTEGER                              :: npairs
        TYPE(pair), DIMENSION(MAX_RDF_PAIRS) :: pairs
    END TYPE rdf

    TYPE, PUBLIC :: Trdf
        INTEGER                               :: nstruct
        REAL(KIND=DBL)                        :: energy
        REAL(KIND=DBL)                        :: norm
        REAL(KIND=DBL)                        :: invkBT
        INTEGER                               :: npairs
        TYPE(pair), DIMENSION(MAX_RDF_PAIRS)  :: pairs
    END TYPE Trdf

    CONTAINS

SUBROUTINE setDefaults(inout_cluster)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    INTEGER :: i
    inout_cluster%edefn  = 0
    inout_cluster%energy(0) = R_ENERGY_ZERO
    DO i=1,MAX_E_DEFN
      inout_cluster%energy(i) = R_ENERGY_ZERO
      inout_cluster%gnorm(i)  = 1000.0
    ENDDO
END SUBROUTINE setDefaults

END MODULE Config
