PROGRAM SCOTT
!######################################################################
!  Stochastic Cluster Optimisation based on Thermodynamical Techniques
!######################################################################
!
!     (1) Basin Hopping optimisation
!     (2) Genetic Algorithm optimisation
!     (3) Solid Solutions
!     (4) Refine Spring Constants
!     (5) Scan Clusters over Surface
!     (6) Simulated Annealing Monte Carlo (Metropolis)
!     (7) Energy Lid
!     (8) Testing
!     (9) Hybrid Genetic Production Run
!
!  Conditions of use:
!
!  SCOTT is available free of charge to academic institutions
!  and non-commerical establishments only. Copies should be
!  obtained from the author only and should not be distributed
!  in any form by the user to a third party without the express
!  permission of the author. This notice applies to all parts
!  of the program, except any library routines which are
!  distributed with the code for completeness. All rights for
!  such routines remain with the original distributor.
!
!  No claim is made that this program is free from errors and
!  no liability will be accepted for any loss or damage that
!  may result. The user is responsible for checking the validity
!  of their results.
!
!  Copyleft UCL 2011
!
    USE Config
    USE Format
    USE Library
    USE Utilities
    USE Annealing,        ONLY : runAnnealing
    USE BasinHopping,     ONLY : runBasinHopping
    USE Comms,            ONLY : comms_create_farm, comms_initialize, comms_finalize, MPI_BARRIER, MY_COMM
    USE EnergyLid,        ONLY : runEnergyLid
    USE Environment,      ONLY : displayControl, displayMainHeader, extinctArrays, finalise, initialise, &
                                 initialiseArrays, initialiseEnvironment, readControl, readJobs, readControl, &
                                 readRestart

    USE File,             ONLY : openErrorFile, openOutputFile, peek, readGulpMaster, outputCAR, readOutMol, &
                                 makeFolder, openProgressFile
    USE GeneticAlgorithm, ONLY : runGeneticAlgorithm
    USE Master,           ONLY : computeNumbers, writeMasterCluster, computeNspecies
    !USE Population,       ONLY : ENERGY_MIN, MASTER_CLUSTER, &
    !                             initialisePopulation, makeExtinct, peekIntoPopulation
    USE Population
    USE ProductionRun,    ONLY : runProduction, runEnforcedCrossover
    USE RefineSprings,    ONLY : runRefineSprings
    USE ScanBox,          ONLY : runScanBox
    USE SolidSolutions,   ONLY : runSolidSolutions

    USE TIMER,            ONLY : WTIME
    USE Atoms,            ONLY : readAtoms, deallocateAtomsArrays

    USE Testing,          ONLY : runTesting
    USE Grid,             ONLY : initiliaseSpecieArrays

    IMPLICIT NONE
    CHARACTER(LEN=20), DIMENSION(20) :: jobs=''
    INTEGER :: job_no=0, total_jobs=0, ierror, nSpecies

    KLMC_STARTTIME = WTIME()

    CALL SYSTEM_CLOCK(COUNT=CLOCK_START)       ! start the system timer
    CALL comms_initialize                      ! start the MPI communications

    CALL openOutputFile                        ! create file for dumping output information
    CALL openErrorFile                         ! create file for dumping error information
    ! DEBUG WKJEE 2024
    write(*,*) "Flag 5"
    write(*,*) SEED
    CALL openProgressFile                      ! create file for dumping log

    ! DEBUG WKJEE 2024
    write(*,*) "Flag 6", SEED
    CALL rand_num_set_seed(SEED)               ! random number generator if SEED==0-> Random ! on CRAY ARCHER2 2024 - SEED: SegFault Err
    ! DEBUG WKJEE 2024
    write(*,*) "Flag 7", SEED


   !CALL selectChannel(stdsee) ! merge output files if debugging
    IF (MASTER_PROC) CALL displayMainHeader
    CALL readJobs(jobs, total_jobs)            ! read the jobs file

    CALL printBanner('Seed set to '//intToChar(SEED))

    DO job_no = 1, total_jobs

        IF (MASTER_PROC) WRITE(stdsee,*)'Running job('//TRIM(intToChar(job_no))//'):'//TRIM(jobs(job_no))

        CALL initialise                        ! set default values for arrays

        CALL debugBanner                       ('### CALL readControl ###')
        CALL readControl(jobs(job_no))         ! read in new control variables

        CALL debugBanner                       ('### CALL readRestart ###')
        CALL readRestart                       ! read in any changes from previous run

        CALL initialiseRandomSeed

        CALL debugBanner                       ('### CALL comms_create_farm ###')
        CALL comms_create_farm                 ! set up task farming

        CALL debugBanner                       ('### CALL readGulpMaster ###')
        CALL readGulpMaster                    ! save GULP input file into memory

        CALL debugBanner                       ('### CALL readAtoms ###')
        CALL readAtoms

        CALL debugBanner                       ('### CALL writeMasterCluster ###')
        CALL writeMasterCluster                ! populate master cluster

        CALL debugBanner                       ('### CALL computeNumbers ###')
        CALL computeNumbers                    ! compute numbers and rearrange atoms if reqd

        IF (MASTER_PROC) THEN
          CALL debugBanner                     ('### CALL displayControl ###')
          CALL displayControl(jobs(job_no))    ! display control variables for confirmation
          CALL debugBanner                     ('### CALL peek MASTER_CLUSTER ###')
          IF (DEBUG_LEVEL > 0) CALL peek(MASTER_CLUSTER) ! display contents of MASTER_CLUSTER
        ENDIF

        CALL debugBanner                       ('### CALL initialiseArrays ###')
        CALL initialiseArrays                  ! allocate memory for large arrays

        CALL debugBanner                       ('### CALL initialiseEnvironment ###')
        CALL initialiseEnvironment             ! after folder environments have been set

        CALL debugBanner('STARTING JOB_TYPE: '//TRIM(intToChar(JOB_TYPE)))

        nSpecies = computeNspecies()
        CALL initiliaseSpecieArrays()

        IF ((DM_FLAG) .AND. (JOB_TYPE .EQ. 0)) THEN
          CALL getDataMiningAtomList()
          CALL getDataMiningCoef()

          CALL printBanner('ATM datamining does not include charge change!')
        ENDIF

        IF (JOB_TYPE == 0) THEN                ! let's start a factory of calcs

            CALL runProduction                 ! generate input files for structures in restart and run

        ELSEIF (JOB_TYPE == 1) THEN            ! let's go generate some clusters

            CALL initialisePopulation          ! generate initial random population
            CALL runBasinHopping               ! global search for optimal structures
            IF (MASTER_PROC) CALL printBanner('Final energy = '//TRIM(realToChar(ENERGY_MIN)))

        ELSEIF (JOB_TYPE == 2) THEN            ! let's go fight to procreate

            CALL initialisePopulation          ! generate initial random population
            CALL peekIntoPopulation            ! if debugging, let's see the initial population
            CALL runGeneticAlgorithm           ! global search for optimal structures
            CALL peekIntoPopulation            ! if debugging, let's see the final population
            IF (MASTER_PROC) CALL printBanner('Final energy = '//TRIM(realToChar(ENERGY_MIN)))

        ELSEIF (JOB_TYPE == 3) THEN            ! let's go mix some solutions

            CALL runSolidSolutions             ! global search for optimal solution

        ELSEIF (JOB_TYPE == 4) THEN            ! let's refine some spring constants

            CALL runRefineSprings              ! refine population of springs

        ELSEIF (JOB_TYPE == 5) THEN            ! let's scan the box

            CALL runScanBox                    ! refine location of cluster 

        ELSEIF (JOB_TYPE == 6) THEN            ! let's go simulate a slow annealing process

            CALL initialisePopulation          ! generate initial population
            CALL runAnnealing                  ! let's run a single annealing walker with kids
            IF (MASTER_PROC) CALL printBanner('Final energy = '//TRIM(realToChar(ENERGY_MIN)))

        ELSEIF (JOB_TYPE == 7) THEN            ! let's go generate some clusters

            CALL initialisePopulation          ! generate initial population
            CALL runEnergyLid                  ! let's run many Thresholds plus Runners
            IF (MASTER_PROC) CALL printBanner('Final energy = '//TRIM(realToChar(ENERGY_MIN)))

        ELSEIF (JOB_TYPE == 8) THEN            ! This job type is just for testing

            CALL runTesting()

        ELSEIF (JOB_TYPE == 9) THEN            ! A hybrid ginetic algorithma and production run

            CALL runEnforcedCrossover()

        ENDIF

        CALL MPI_BARRIER(MY_COMM,ierror)       ! Sync processors
        CALL extinctArrays                     ! deallocate memory for large arrays
    END DO

    CALL makeExtinct
    CALL comms_finalize

    PRINT *,'Finished!'
    PRINT *, "Run time: ",  WTIME() - KLMC_STARTTIME, " s."

    CALL finalise
    CALL deallocateAtomsArrays

END PROGRAM SCOTT
