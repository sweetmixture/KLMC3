MODULE File

    USE Config
    USE Format
    USE Library
    USE Algebra
    USE Comms,     ONLY : comms_finalize, my_rank
    USE UnitCell,  ONLY : lattice_c2v_3D, lattice_v2c_3D, xctoxf, xftoxc, updateCell, vect_to_box, &
      lattice_c2v_2D

    USE TIMER

    IMPLICIT NONE

!==========================================================================================!
CONTAINS

! routines used to perform I/O to files
!
! subroutines: RUN_AIMS RUN_GULP RUN_VASP
!              copyClusterFiles displayGULPcounters 
!              generateAimsInput generateGulpInput generateVaspInput
!              inputAtom inputCluster 
!              logEnergy logEnergies logMessage logMetropolis logSuccess logEbest
!              makeFolder moveBestdata openErrorFile openOutputFile 
!              outputAtom outputCluster outputXYZ
!              peek printBestSet printRDF printTrdf 
!              readARC readGulpMaster readID readPotential readXYZ
!              removeArcFiles removeClusterFiles 
!              retrieveAimsData retrieveData retrieveVaspData
!
!  functions:  EXCEPTIONS, readEnergy

  !==========================================================================================!
  ! A funtion to check if a file exists
  !==========================================================================================!
!  LOGICAL FUNCTION inquireFile(filePath)
!
!    CHARACTER(LEN=*), INTENT(IN) :: filePath
!
!    LOGICAL :: exists
!
!
!
!  END FUNCTION

  !==========================================================================================!
  ! A function to check if a directory exists
  !==========================================================================================!
  LOGICAL FUNCTION inquireDirectory(dirPath)

    CHARACTER(LEN=*), INTENT(IN) :: dirPath

    LOGICAL :: exists

    ! Works with gfortran
    INQUIRE (FILE=TRIM(dirPath), EXIST=exists)

    inquireDirectory = exists

    RETURN
  END FUNCTION inquireDirectory


  !==========================================================================================!
  ! A routine to run DMOL3
  !==========================================================================================!
  SUBROUTINE RUN_DMOL(input_name, ierr, ide, onlyEnergyEval)
    CHARACTER(LEN=*),INTENT(IN) :: input_name
    INTEGER,        INTENT(OUT) :: ierr
    INTEGER,         INTENT(IN) :: ide
    LOGICAL,         INTENT(IN) :: onlyEnergyEval

    CHARACTER(LEN=1) :: quot,pipe
    CHARACTER(LEN=MAX_FOLDER_WIDTH) :: execDestination, wd
    CHARACTER(LEN=MAX_FOLDER_WIDTH*3) :: tarFile, dmolShFile, dmolInoputFile
    CHARACTER(LEN=50) :: dmolOutputFile, carOutputFile

    INTEGER :: success=0
    LOGICAL :: l_exist
    REAL(KIND=8) :: DMOL_STARTTIME
    CHARACTER(LEN=100) :: message

    CALL getcwd(wd)

    quot=char(ichar("'"))
    pipe=char(ichar("|"))

    CALL increment(N_DMOL_RUNS)

    execDestination = TRIM(WORKING_DIR) // TRIM(RELAXED_FOLDER) // TRIM(input_name) // '/'

    CALL makeFolder(TRIM(execDestination), l_exist)

    IF (l_exist) THEN
      CALL printStringColumns('Removing previous files in:', TRIM(execDestination))
      CALL SYSTEM('rm -fR ' // TRIM(execDestination) // '*')
      CALL makeFolder(TRIM(execDestination))
    ELSE
      CALL makeFolder(TRIM(execDestination))
    END IF

    ! Creating dmol3 input file
    dmolInoputFile = TRIM(execDestination) // '/' // TRIM(input_name) // '.input'

    CALL generateDmolInput(ide, dmolInoputFile, onlyEnergyEval)

    !CALL chdir(execDestination)

    ! Saving the initial configuration
    IF (DMOL_SAVE_INITIAL_STRUCT) THEN
      CALL SYSTEM('cp ' // TRIM(RELAXED_FOLDER) // TRIM(input_name) // &
       '.car ' // TRIM(RELAXED_FOLDER) // '/' // TRIM(input_name) // '_ini.car')
    ENDIF

    ! Copying input files
    !    System file
    CALL SYSTEM('cp ' // TRIM(RELAXED_FOLDER) // TRIM(input_name) // &
      '.car ' // TRIM(execDestination) // '/')

    ! Generate .sh file to setenv and to rundmol3
    dmolShFile = TRIM(execDestination) // '/' // 'runDmol3.sh'
    CALL outputDmolSh(dmolShFile, execDestination, input_name)

    DMOL_STARTTIME = WTIME()

    CALL SYSTEM('cd ' // TRIM(execDestination)//';chmod +x ' // 'runDmol3.sh; ./runDmol3.sh')

    WRITE(message, *) "DMOL Run time: ",  WTIME() - DMOL_STARTTIME, " s."
    CALL printLog("RUN_DMOL", message, 10)

    dmolOutputFile = TRIM(input_name) // ".outmol"
    carOutputFile  = TRIM(input_name) // ".car"

    ! Check for execution errors
    CALL readOutMolError(TRIM(execDestination), TRIM(input_name), ierr)

    ! Saving the car file
    CALL SYSTEM('cp -f ' // TRIM(execDestination)//'/'//TRIM(dmolOutputFile) // ' ' // TRIM(RELAXED_FOLDER) // '/')
    CALL SYSTEM('cp -f ' // TRIM(execDestination)//'/'//TRIM(carOutputFile)  // ' ' // TRIM(RELAXED_FOLDER) // '/')

    ! TAR IT AND SAVE IT?
    IF (DMOL_TAR_OUTPUT) THEN
      tarFile = TRIM(RELAXED_FOLDER) // '/' // TRIM(input_name) // '.tar'

      CALL SYSTEM('tar -cPf ' // TRIM(tarFile) // ' ' // TRIM(execDestination) // '/*')
    ENDIF

    CALL chdir(wd)

    ! Deleting working directory
    CALL SYSTEM('rm -rf ' // TRIM(execDestination))

    RETURN

  END SUBROUTINE RUN_DMOL

!==========================================================================================!

  SUBROUTINE outputDmolSh(filePath, tmpDirectory, systemName)
    CHARACTER(LEN=*), INTENT(IN) :: filePath, tmpDirectory, systemName
    INTEGER :: outErr=1, outUnit=31

    CHARACTER(LEN=200) :: SRC_DIR, FILELINE

    OPEN(UNIT=outUnit, &
         FILE=TRIM(filePath), &
         STATUS='REPLACE', &
         ACTION='WRITE', &
         IOSTAT=outErr, &
         IOMSG=ERROR_MSG)

    IF (outErr == 0) THEN

      WRITE(UNIT=outUnit,FMT=*, IOSTAT=outErr, IOMSG=ERROR_MSG) "#!/bin/bash"

      WRITE(UNIT=outUnit,FMT=*, IOSTAT=outErr, IOMSG=ERROR_MSG) 'export DMOL_TMP="' &
        // TRIM(tmpDirectory) // '"'
      WRITE(UNIT=outUnit,FMT=*, IOSTAT=outErr, IOMSG=ERROR_MSG) 'export DMOL3_DATA="' &
        // TRIM(DMOL_DATA_PATH) // '"'

      IF (DMOL_VERSION == "3.2") THEN
        WRITE(UNIT=outUnit,FMT=*, IOSTAT=outErr, IOMSG=ERROR_MSG) 'source '// TRIM(DMOL_LICENCE_PATH)

         WRITE(UNIT=outUnit,FMT=*, IOSTAT=outErr, IOMSG=ERROR_MSG) TRIM(DMOL_EXE_PATH) &
          // ' ' //  TRIM(systemName)

      ELSE IF (DMOL_VERSION == "6.1") THEN
        WRITE(UNIT=outUnit,FMT=*, IOSTAT=outErr, IOMSG=ERROR_MSG) 'export PATH="' &
          // TRIM(DMOL_PATH) // '"'

        WRITE(UNIT=outUnit,FMT=*, IOSTAT=outErr, IOMSG=ERROR_MSG) 'export LD_LIBRARY_PATH="' &
          // TRIM(DMOL_LD_PATH) // '"'

        WRITE(UNIT=outUnit,FMT=*, IOSTAT=outErr, IOMSG=ERROR_MSG) 'export ACCELRYS_LIC_PACK_DIR="' &
          // TRIM(DMOL_ACCELRYS_PACKDIR) // '"'

         WRITE(UNIT=outUnit,FMT=*, IOSTAT=outErr, IOMSG=ERROR_MSG) TRIM(DMOL_EXE_PATH) &
          // ' ' //  TRIM(systemName) // ' ' // TRIM(systemName) // '__DOS'

      ENDIF

      CLOSE(UNIT=outUnit)
    ELSE
       WRITE(*,*) ERROR_MSG
    END IF

  END SUBROUTINE outputDmolSh

!==========================================================================================!

SUBROUTINE RUN_GULP(input_name,output_name,Energy,Gnorm,ierr)
    CHARACTER(LEN=*), INTENT(IN) :: input_name,output_name
    REAL(KIND=DBL), INTENT(OUT) :: Energy,Gnorm
    INTEGER, INTENT(OUT) :: ierr
    CHARACTER(LEN=MAX_GULP_WIDTH) :: buffer
    CHARACTER(LEN=1) :: quot,pipe
    INTEGER :: i
    CHARACTER(LEN=100) :: message

    CHARACTER(LEN=200) :: inputPath, outputPath

    quot=char(ichar("'"))
    pipe=char(ichar("|"))

    inputPath = TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//TRIM(input_name)
    outputPath = TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//TRIM(output_name)

    !CALL SYSTEM('rm -rf ' // TRIM(outputPath) // '.gout')

    IF (DEBUG_LEVEL > 100) THEN
      WRITE(message, *) TRIM(EXE_GULP_PATH)//' < '// TRIM(inputPath)//'.gin > '// &
        TRIM(outputPath)//'.gout'

      CALL printLog("RUN_GULP", message, 10)

    ELSEIF (DEBUG_LEVEL > 0) THEN
      WRITE(message, *) 'Now running ', TRIM(input_name), '.gin'

      CALL printLog("RUN_GULP", message, 10)
    ENDIF

    CALL SYSTEM(EXE_GULP_PATH//' < '// TRIM(inputPath)//'.gin > '// &
       TRIM(outputPath)//'.gout')

    IF (OUTPUT_LEVEL.ge.10) THEN
      CALL SYSTEM('cat ' // TRIM(outputPath)//'.gout '// '>> '//TRIM(inputPath)//'_save.gout')
    ENDIF

    IF (L_GULP_OPTI) THEN

      CALL SYSTEM('grep '//quot//'Cycle:'//quot//' '// &
         TRIM(outputPath)//'.gout '// pipe//' tail -1 >> number-of-line-searches')

      CALL SYSTEM('grep '//quot//'Optimisation achieved'//quot//' '// &
         TRIM(outputPath) // '.gout > Extract.tmp')

      CALL SYSTEM('grep '//quot//'unless gradient norm is small'//quot//' '// &
         TRIM(outputPath) // '.gout >> Extract.tmp')

      CALL SYSTEM('grep '//quot//'Too many failed attempts to opt'//quot//' '// &
         TRIM(outputPath) // '.gout >> Extract.tmp')

      CALL SYSTEM('grep '//quot//'Maximum number of function call'//quot//' '// &
         TRIM(outputPath) // '.gout >> Extract.tmp')

      IF (DEBUG_LEVEL > 0) CALL SYSTEM('cat Extract.tmp ')

      ierr=-1
      buffer=''
      OPEN(UNIT=1,FILE='Extract.tmp')
      READ(UNIT=1,FMT='(A)', END=1, ERR=1) buffer
1     CLOSE(UNIT=1,STATUS='DELETE')

      IF ( INDEX(buffer,'achieved' ) /= 0 ) THEN
        ierr=0
      ELSEIF (INDEX(buffer,'gradient') /= 0 ) THEN
        ierr=1
      ELSEIF (INDEX(buffer,'attempts') /= 0 ) THEN
        ierr=2
      ELSEIF (INDEX(buffer,'reach') /= 0 ) THEN
        ierr=-2
      ENDIF

      IF (L_SURFACE_ENERGY) THEN
        CALL SYSTEM('grep '//quot//'Surface energy (region 1)'//quot//' '// &
        TRIM(outputPath)//'.gout '// &
        pipe//' awk {'//quot//'print $6'//quot//'} '//pipe//' tail -1 >> Extract.tmp')
      ELSE ! READ IN TOTAL ENERGY
        CALL SYSTEM('grep '//quot//'Total lattice energy'//quot//' '// &
        TRIM(outputPath)//'.gout '// &
        pipe//' awk {'//quot//'print $5'//quot//'} '//pipe//           &
        ' head -3 '//pipe//' tail -1 >> Extract.tmp')
      ENDIF

      CALL SYSTEM('grep '//quot//'Final Gnorm'//quot//' '// &
         TRIM(outputPath)//'.gout '// &
         pipe//' awk {'//quot//'print $4'//quot//'} >> Extract.tmp')

      Energy=ZERO
      Gnorm=ZERO
      OPEN(UNIT=1,FILE='Extract.tmp')
      READ(UNIT=1,FMT=*,END=2,ERR=2)Energy
      READ(UNIT=1,FMT=*,END=2,ERR=2)Gnorm
2     CLOSE(UNIT=1,STATUS='DELETE')

      IF (DEBUG_LEVEL > 0) THEN
        WRITE(message, *) 'Code=',ierr,' Energy=',Energy,' Gnorm =',Gnorm
        CALL printLog("RUN_GULP", message, 10)
      ENDIF

! Now to extract GULP call information

      CALL SYSTEM('grep '//quot//'Cycle:'//quot//' '// &
         TRIM(outputPath)//'.gout '// &
         pipe//' tail -1 '//pipe//' awk {'//quot//'print $2'//quot//'} >> Extract.tmp')

      CALL SYSTEM('grep '//quot//' times '//quot//' '// &
         TRIM(outputPath)//'.gout '// &
         pipe//' awk {'//quot//'print $7'//quot//'} >> Extract.tmp')

      CALL SYSTEM('grep '//quot//' times '//quot//' '// &
         TRIM(outputPath)//'.gout '// &
         ' >> number-of-line-searches')

      OPEN(UNIT=1,FILE='Extract.tmp')
      READ(UNIT=1,FMT=*,END=3,ERR=3)i
      N_GULP_L = N_GULP_L + i
      READ(UNIT=1,FMT=*,END=3,ERR=3)i
      N_GULP_E = N_GULP_E + i
      READ(UNIT=1,FMT=*,END=3,ERR=3)i
      N_GULP_dE = N_GULP_dE + i
      READ(UNIT=1,FMT=*,END=3,ERR=3)i
      N_GULP_d2E = N_GULP_d2E + i
3     CLOSE(UNIT=1,STATUS='DELETE')

    ELSE ! GULP SINGLE POINT RUN

      IF (L_SURFACE_ENERGY) THEN
         CALL SYSTEM('grep '//quot//'Surface energy (region 1)'//quot//' '// &
         TRIM(outputPath)//'.gout '// &
         pipe//' awk {'//quot//'print $6'//quot//'} > Extract.tmp')
      ELSE
         CALL SYSTEM('grep '//quot//'Total lattice energy'//quot//' '// &
         TRIM(outputPath)//'.gout '// &
         pipe//' awk {'//quot//'print $5'//quot//'} > Extract.tmp')
      ENDIF

      OPEN(UNIT=1,FILE='Extract.tmp')
      READ(UNIT=1,FMT=*,END=4,ERR=4)Energy
4     CLOSE(UNIT=1,STATUS='DELETE')

!      IF (DEBUG_LEVEL > 0) THEN
!        WRITE(*,*)'Energy=',Energy
!      ENDIF

      CALL SYSTEM('grep '//quot//' times '//quot//' '// &
         TRIM(outputPath)//'.gout '// pipe//' awk {'//quot//'print $7'//quot//'} >> Extract.tmp')

      CALL SYSTEM('grep '//quot//' times '//quot//' '// &
         TRIM(outputPath)//'.gout '// ' >> number-of-line-searches')

      OPEN(UNIT=1,FILE='Extract.tmp')
      READ(UNIT=1,FMT=*,END=5,ERR=5)i
      N_GULP_E = N_GULP_E + i
      READ(UNIT=1,FMT=*,END=5,ERR=5)i
      N_GULP_dE = N_GULP_dE + i
      READ(UNIT=1,FMT=*,END=5,ERR=5)i
      N_GULP_d2E = N_GULP_d2E + i

5     CLOSE(UNIT=1,STATUS='DELETE')

    ENDIF ! READ ENERGY (AND GNORM) FROM GULP OUTPUT

    CALL SYSTEM('rm ' //TRIM(outputPath)//'.gout')

END SUBROUTINE RUN_GULP

!==========================================================================================!

SUBROUTINE RUN_AIMS (input_name,Energy,ierr)
    CHARACTER(LEN=*),INTENT(IN) :: input_name 
    INTEGER, INTENT(OUT)        :: ierr
    REAL(KIND=DBL), INTENT(OUT) :: Energy
    INTEGER                     :: f_unit=77
    INTEGER                     :: ierror,success=0
    CHARACTER(LEN=1) :: quot

    CALL increment(N_AIMS_RUNS)
    quot=char(ichar("'"))

    CALL SYSTEM(TRIM(DATA_FOLDER)//'run_aims.bash '//TRIM(RELAXED_FOLDER) &
                //TRIM(input_name)//' '//TRIM(input_name))

   !NOW TO CHECK CALCULATIONS

   !***MRF Aug2015 - moved grep to script as causing system hang***! 
!    CALL SYSTEM('grep -c "SELF-CONSISTENCY CYCLE DID NOT CONVERGE"' &
!         //TRIM(RELAXED_FOLDER)//TRIM(input_name)//'/'//&
!         TRIM(input_name)//'.out > Extract.tmp')

    OPEN(UNIT=f_unit,FILE='Extract.tmp',STATUS='old',IOSTAT=ierror)
    IF (ierror/=0) THEN 
       WRITE(stderr,*) 'ERROR opening info file Extract.tmp'
       STOP
    END IF
    READ(f_unit,FMT=*,IOSTAT=ierror) success
    IF (ierror /= 0 .OR. (success > 0) ) THEN 
       WRITE(stderr,*) 'SCF not converged'
       success = -1
       ierr = 1
       Energy = 1
    END IF
    CLOSE(f_unit)

    IF (success == 0) THEN 
       !SCF CONVERGED - GET ENERGY 
       ierr = 0
       CALL SYSTEM('grep "Total energy uncorrected" '&
         //TRIM(RELAXED_FOLDER)//TRIM(input_name)//'/'  &
         //TRIM(input_name)//'.out | awk '//quot//'{print $6}'//quot//' | tail -1 > Extract.tmp')
       OPEN(UNIT=f_unit,FILE='Extract.tmp')
       READ(f_unit,*) Energy
       WRITE(stdsee,*) 'Read FHI-aims energy as:',Energy
       CLOSE(f_unit)
    END IF
    CALL SYSTEM('rm -f Extract.tmp')
    RETURN
END SUBROUTINE RUN_AIMS

!==========================================================================================!

SUBROUTINE RUN_NWCHEM(input_name,Energy,ierr)
    CHARACTER(LEN=*),INTENT(IN) :: input_name 
    INTEGER, INTENT(OUT)        :: ierr
    REAL(KIND=DBL), INTENT(OUT) :: Energy
    INTEGER                     :: file,ierror,stat
    CHARACTER(LEN=1)            :: quot
    CHARACTER(LEN=*), PARAMETER :: job_complete='Optimization converged' 
    CHARACTER(LEN=*), PARAMETER :: job_fail    ='Calculation failed to converge' 
    CHARACTER(LEN=256)          :: buffer  
    LOGICAL                     :: readfile = .FALSE.
    quot=char(ichar("'"))

    IF (L_RUN_REMOTE) THEN
   !    CALL RUN_REMOTE(input_name(:))
    ELSE
       WRITE(stderr,*) 'Sorry, NWChem can only be called using remote functionality at the moment'
       STOP
    END IF

    !NOW TO CHECK CALCULATIONS
    OPEN(unit=file,file=TRIM(RELAXED_FOLDER)//TRIM(input_name)//'/output.nw', status='old', iostat=stat)
    IF  (stat .ne. 0) THEN
       WRITE(stderr,*) "Error opening NWChem output"
       Energy = 1
       ierr   = 1
    ELSE   
       buffer = ""
       readfile = .FALSE.
       DO WHILE (.NOT.readfile)
          READ(file,"(A)",iostat = stat) buffer
          !WRITE(*,*) buffer
          IF (stat .ne. 0 ) THEN
             readfile = .TRUE.
             Energy = 1
             ierr   = 1
          ELSE
             !Check for completion
             IF (index(buffer,job_complete) .ne. 0) THEN
                readfile = .TRUE.
                CLOSE(file,iostat=stat)
                IF (stat .ne. 0) stop "Error closing output.nw file"
                ! Calculation completed OK
                CALL SYSTEM('grep "Total DFT energy" '//&
                & TRIM(RELAXED_FOLDER)//TRIM(input_name)//'/output.nw | tail -1 & 
                & | awk  '//quot//'{print $5}'//quot//' > Extract.tmp')
                OPEN(UNIT=1,FILE='Extract.tmp')
                READ(1,*) Energy
                ierr = 0
                WRITE(*,*) 'NWChem run completed with energy ',Energy
                CLOSE(1,STATUS='DELETE')
             ELSE IF (index(buffer,job_fail) .ne. 0) THEN
                !Calculation SCF failed
                WRITE(STDOUT,*) 'Calculation SCF failed'
                readfile = .TRUE.
                Energy = 1
                ierr   = 1
             END IF
          END IF
      END DO
   END IF
END SUBROUTINE RUN_NWCHEM

SUBROUTINE RUN_VASP(input_name,Energy,ierr)
    IMPLICIT NONE
    CHARACTER(LEN=*),DIMENSION(:),INTENT(IN)  :: input_name 
    INTEGER, DIMENSION(:), INTENT(OUT)        :: ierr
    REAL(KIND=DBL), DIMENSION(:), INTENT(OUT) :: Energy

    INTEGER                     :: file,ierror,stat,i
    CHARACTER(LEN=1)            :: quot
    CHARACTER(LEN=*), PARAMETER :: job_complete='reached required accuracy' 
    CHARACTER(LEN=256)          :: buffer  
    LOGICAL                     :: readfile = .FALSE.

    quot=char(ichar("'"))

    IF (L_RUN_REMOTE ) THEN
       CALL RUN_REMOTE(input_name)
    ELSE
       CALL SYSTEM(TRIM(DATA_FOLDER)//'run_vasp.bash '//TRIM(RELAXED_FOLDER) &
                //TRIM(input_name(1))//' '//TRIM(input_name(1))//'.out')
    END IF

    !NOW TO CHECK CALCULATIONS
    DO i = 1,SIZE(input_name)
      OPEN(unit=file,file=TRIM(RELAXED_FOLDER)//TRIM(input_name(i))//'/OUTCAR', status='old', iostat=stat)
      IF  (stat .ne. 0) THEN
        WRITE(stderr,*) "Error opening OUTCAR"
        Energy(i) = 1
        ierr(i)   = 1
       ELSE   
         buffer = ""
         readfile = .FALSE.
         DO WHILE (.NOT.readfile)
            READ(file,"(A)",iostat = stat) buffer
            !WRITE(*,*) buffer
            IF (stat .ne. 0 ) THEN
               readfile = .TRUE.
               Energy(i) = 1
               ierr(i)   = 1
               WRITE(STDERR,*) 'VASP calculation did not complete'
            ELSE
              !Check for completion
              IF (index(buffer,job_complete) .ne. 0) THEN
                 readfile = .TRUE.
                 CLOSE(file,iostat=stat)
                 IF (stat .ne. 0) stop "Error closing OUTCAR file"
                 ! Calculation completed OK
                 CALL SYSTEM('grep "energy(sigma->0)" '//&
                 & TRIM(RELAXED_FOLDER)//TRIM(input_name(i))//'/OUTCAR | tail -1 & 
                 & | awk  '//quot//'{print $7}'//quot//' > Extract.tmp')
                 OPEN(UNIT=1,FILE='Extract.tmp')
                 READ(1,*) Energy(i)
                 ierr(i) = 0
                 WRITE(*,*) 'VASP run completed with energy ',Energy(i)
                 CLOSE(1,STATUS='DELETE')
              END IF
            END IF
         END DO
       END IF
    END DO  
   RETURN
END SUBROUTINE RUN_VASP

SUBROUTINE RUN_REMOTE(filename)
!**************************************
! A short program to submit and monitor
! a batch job on HECToR using Fortran
! Iain Bethune, May 2013
!**************************************
! Runs on any remote machine running SUN GridEngine.
! Matt Farrow, August 2013
!************************************************
   implicit none

   LOGICAL                     :: readfile = .FALSE.

   character(len=*),DIMENSION(:),intent(in) :: filename
! access (must have passwordless SSH set up for this user)
! Details of the job to be submitted

   character(len=*), parameter :: job_script = "submit.pbs"
   character(len=*), parameter :: job_name = "klmcjob"
   integer                     :: num_cores
   integer                     :: walltime_h, walltime_m, walltime_s
   character(len=256)          :: job_cwd
   character(len=256)          :: executable

! File manipulation stuff
   character(len=*), parameter :: quiet = " > /dev/null 2>&1"
   character(len=*), parameter :: tmp_file = "tmp_file"
   character(len=*), parameter :: to_tmp = " > "//tmp_file//" 2>&1"
   character(len=81)           :: qstat_job_complete

! Misc vars 
   integer :: file,stat, i
   logical :: complete=.FALSE.
   LOGICAL :: up_flag = .TRUE.
   character(len=256) :: buffer, jobid

! Write out the job script to a file
   complete=.FALSE.
   job_cwd = TRIM(JOB_WORKING_DIR) 

   IF (L_NWCHEM_RUN) THEN
      !Faraday executable
      !executable = '/usr/local/nwchem/bin/nwchem '
      ! HECToR executable
      executable = 'nwchem '
      ! Faraday job_complete
      !qstat_job_complete = "Following jobs do not exist:"
      ! HECToR job complete
      qstat_job_complete = "Unknown Job Id"
   ELSE IF (L_VASP_RUN) THEN
      executable = TRIM(EXEC) 
!      executable =   '$HOME/bin/vasp' 
!      qstat_job_complete = "qstat: Unknown Job Id" 
!      qstat_job_complete = "Job has finished"
      qstat_job_complete = TRIM(JOB_COMPLETE)
   ELSE   
      WRITE(*,*) 'Remote functionality called but not VASP or NWChem run!'
      STOP
   END IF

!  Are we batch processing?
   IF (L_BATCH_SUBMISSION) THEN
     WRITE(*,*) 'Creating batch submission script...'   
     CALL createBatchScript(filename)
   ELSE 
   walltime_h = HOURS
   walltime_m = MINS
   walltime_s = SECS

   num_cores = PROCESSORS


!   job_cwd = TRIM(job_cwd)//'_'//TRIM(filename)

   ! IF NWCHEM, Faraday doesn't like remote qsub, so done via a script! 
!   IF (L_NWCHEM_RUN) THEN
!      open(unit=file,file=TRIM(RELAXED_FOLDER)//TRIM(filename)//'/sub.sh',STATUS='REPLACE',iostat=stat)
!      if (stat .ne. 0) stop "Error opening NWChem sub.sh file"
!      WRITE(file,"(A)") "#!/bin/bash --login"
!      WRITE(file,"(A)") ". /usr/local/sge6.2u5/default/common/settings.sh"
!      WRITE(file,"(A)") "cd "//job_cwd
!      WRITE(file,"(A)") "/usr/local/sge6.2u5/bin/lx24-amd64/qsub submit.pbs"
!      CLOSE(file,iostat=stat)
!      IF (stat .ne. 0) stop "Error closing NWChem submission script file"
!      stat = SYSTEM("chmod +x "//TRIM(RELAXED_FOLDER)//TRIM(filename)//"/sub.sh")
   
!      open(unit=file,file=TRIM(RELAXED_FOLDER)//TRIM(filename)//'/qstat.sh',STATUS='REPLACE',iostat=stat)
!      if (stat .ne. 0) stop "Error opening NWChem qstat.sh file"
!      WRITE(file,"(A)") "#!/bin/bash --login"
!      WRITE(file,"(A)") "export SGE_ROOT=/usr/local/sge6.2u5"
!      WRITE(file,"(A)") "cd "//job_cwd
!      WRITE(file,"(A)") "/usr/local/sge6.2u5/bin/lx24-amd64/qstat -j $1"
!      CLOSE(file,iostat=stat)
!      IF (stat .ne. 0) stop "Error closing NWChem submission script file"
!      stat = SYSTEM("chmod +x "//TRIM(RELAXED_FOLDER)//TRIM(filename)//"/qstat.sh")
!   END IF

   open(unit=file,file=TRIM(RELAXED_FOLDER)//'/'//job_script,iostat=stat)
   if (stat .ne. 0) stop "Error opening script file"

   write(file,"(A)") "#!/bin/bash --login"
   write(file,"(A)") "#PBS -N "//job_name
   IF (ARCHER) THEN 
     write(file,"(A,I0)") "#PBS -l select=",num_cores
   ELSE
     write(file,"(A,I0)") "#PBS -l mppwidth=",num_cores
     write(file,"(A,I0)") "#PBS -l mppnppn=",min(num_cores,32)
   END IF
   write(file,"(A,I0,A,I0,A,I0)") "#PBS -l walltime=",walltime_h,":",walltime_m,":",walltime_s
!   IF (L_NWCHEM_RUN) write(file,"(A)") "#PBS -q ib.q " 
   write(file,"(A)") "#PBS -A "//TRIM(BUDGET)
   write(file,"(A)") "#PBS -o "//TRIM(job_cwd)//"/q.error"
   write(file,"(A)") "#PBS -e "//TRIM(job_cwd)//"/q.output"
   write(file,*) ""
   !write(file,*) "mkdir "//TRIM(job_cwd)
   write(file,"(A)") "cd "//job_cwd
   write(file,*) ""
   write(file,*) "date"
   IF (L_VASP_RUN) THEN
      write(file,*) "cp POSCAR POSCAR.orig"
      write(file,*) "for count in {1..100}"
      write(file,*) "do"
      write(file,*) "echo $count >> run_position"
      write(file,"(A,I0,A,I0,A)") "aprun -n ",num_cores, " -N ", min(num_cores,32)," "// executable
      write(file,*) 'flag=`grep -c "reached required accuracy" OUTCAR`'
      write(file,*) 'if [ $flag -eq 1 ]'
      write(file,*) "  then"
      write(file,*) "  break"
      write(file,*) "fi"
      write(file,*) "if [ $count -eq 50 ]" 
      write(file,*) "then"
      write(file,*) "echo $count iterations reached, switching to conjugate gradients >> run_position"
      write(file,*) "sed 's/IBRION = 1/IBRION = 2/g' INCAR > tmp"
      write(file,*) "mv tmp INCAR"
      write(file,*) "fi"
      write(file,*) "cp CONTCAR POSCAR"
      write(file,*) "cp OUTCAR OUTCAR.lastStep"
      write(file,*) "done"
   ELSE IF (L_NWCHEM_RUN) THEN
      WRITE(file,*) 'module load nwchem'
      write(file,*) 'aprun -n ',num_cores, ' -N ',min(num_cores,32),' ',executable//' input.nw > output.nw' 
   END IF
   close(file,iostat=stat)

   if (stat .ne. 0) stop "Error closing script file"

  END IF  !end logical 

! Copy the job file to remote machine and submit
  write(STDOUT,*) "Staging jobs to "//TRIM(HOST)

   CALL CheckSystemIsUp
   WRITE(STDOUT,*)"Creating "//TRIM(job_cwd)//" on remote system"
   CALL SYSTEM("ssh "//TRIM(USER)//"@"//TRIM(HOST)//" mkdir -p "//TRIM(job_cwd)//quiet, stat)
   if (stat .ne. 0) stop "Error creating job_cwd"

   CALL SYSTEM("scp "//TRIM(RELAXED_FOLDER)//"/"//job_script//" "// &
      TRIM(USER)//"@"//TRIM(HOST)//":"//job_cwd//quiet, stat)
   if (stat .ne. 0) stop "Error copying job script"
  
  
   DO i = 1,SIZE(filename)
      CALL CheckSystemIsUp
      CALL SYSTEM("ssh "//TRIM(USER)//"@"//TRIM(HOST)//" mkdir -p "//TRIM(job_cwd)//"/simulation_"//intToChar(i)//quiet, stat)
      IF (stat .ne. 0) THEN
        WRITE(STDERR,*) "Error creating job_cwd simulation_"//intToChar(i)
        STOP
      END IF
      CALL CheckSystemIsUp

      CALL SYSTEM("scp "//TRIM(RELAXED_FOLDER)//"/"//TRIM(filename(i))//"/* "//&
                 TRIM(USER)//"@"//TRIM(HOST)//":"//TRIM(job_cwd)//"/simulation_"//intToChar(i)//quiet, stat)
      if (stat .ne. 0) stop "Error copying data"
   END DO

!  write(*,*) "Submitting job on "//TRIM(HOST)

!   IF (L_VASP_RUN) THEN
      CALL CheckSystemIsUp
      CALL SYSTEM("ssh "//TRIM(USER)//"@"//TRIM(HOST)//" qsub "//TRIM(job_cwd)//"/"//job_script//">tmp_file", stat)
!   ELSE IF (L_NWCHEM_RUN) THEN
!      stat = system("ssh "//TRIM(USER)//"@"//TRIM(HOST)//" "//TRIM(job_cwd)//"/"//"sub.sh"//to_tmp)
!   END IF
   if (stat .ne. 0) stop "Error submitting job"
   open(unit=file,file=tmp_file, status='old', iostat=stat)
   if (stat .ne. 0) stop "Error opening temporary file"
   buffer = ""
!   IF (L_VASP_RUN) 
   read(file,*) buffer
!   IF (L_NWCHEM_RUN) read(file,*) buffer,buffer,buffer,jobid
   close(file,iostat=stat)
   if (stat .ne. 0) stop "Error closing temporary file"
   jobid = trim(buffer)
   WRITE(*,*) 'job ID:',jobid
 
 ! Loop checking until the job is found to be completed 
   WRITE(*,*) "Checking job status every",POLL_DELAY,"seconds..."
   DO WHILE (.not.complete)
!     ! Check to see if machine is up...

!     IF (L_VASP_RUN) THEN
         CALL CheckSystemIsUp
         CALL SYSTEM("ssh "//TRIM(USER)//"@"//TRIM(HOST)//" qstat -x "//trim(jobid)//"| tail -1 " &
                        //" | awk '{print $5}'"//to_tmp, stat)
!    ELSE IF (L_NWCHEM_RUN) THEN
!         stat = system("ssh "//TRIM(USER)//"@"//TRIM(HOST)//" "//TRIM(job_cwd) & 
!         &              //"/"//"qstat.sh "//trim(jobid)//to_tmp)
!     END IF
  
      IF (stat .ne. 0) then
       ! if non-zero return code either there is a real error, or the job completed
          open(unit=file,file=tmp_file, status='old', iostat=stat)
          if (stat .ne. 0) stop "Error opening temporary file"
          buffer = ""
          READ(file,"(A)", IOSTAT=stat) buffer
          WRITE(*,*) buffer
          IF (stat /= 0) THEN
            WRITE(stderr,*) 'WARNING! Problem with reading qstat status'
            complete = .TRUE.
            EXIT
          ELSE
            SELECT CASE(TRIM(buffer))
            ! Job finished
            CASE ('F')
              WRITE(*,*) 'Job completed.'   
              complete = .TRUE.
            ! Job running
            CASE ('R')
               WRITE(*,*) 'Job is running...'
            ! Job queued
            CASE ('Q')
              !WRITE(*,*) 'Job is in the queue...' 
            ! Exiting or error
            CASE ('E')
               WRITE(*,*) 'Job finished and cleaning up'
            END SELECT

           !  IF (index(buffer,TRIM(qstat_job_complete)) .ne. 0) then
           !    complete = .true.
           !    EXIT
           !   ELSE
           !     WRITE(STDERR,*) "WARNING! Error checking job status"
           !     EXIT
           ! END IF
            close(file,iostat=stat,status='delete')
            IF (stat .ne. 0) stop "Error closing temporary file"
         END IF
       END IF
      IF (.not. complete) call sleep(POLL_DELAY)
     END DO !while loop

    ! Copy output file back 
      IF (L_VASP_RUN) THEN
         CALL CheckSystemIsUp
         DO i = 1, SIZE(filename)
            CALL SYSTEM("scp "//TRIM(USER)//"@"//TRIM(HOST)//":" // &
                  TRIM(job_cwd)//"/simulation_"//TRIM(intToChar(i))//"/* " // &
                  TRIM(RELAXED_FOLDER)//"/"//TRIM(filename(i))//quiet, stat)
            if (stat .ne. 0) stop "Error copying output file"
         END DO

      ELSE IF (L_NWCHEM_RUN) THEN
         CALL CheckSystemIsUp
         CALL SYSTEM("scp "//TRIM(USER)//"@"//TRIM(HOST)//":"//&
                TRIM(job_cwd)//"/output.nw "//TRIM(RELAXED_FOLDER)//TRIM(filename(1))//quiet, stat)
         if (stat .ne. 0) stop "Error copying output file"
      END IF

      !Clean up files
      CALL CheckSystemIsUp
      CALL SYSTEM("ssh "//TRIM(USER)//"@"//TRIM(HOST)//" rm -rf "//trim(job_cwd)//quiet, stat)
      if (stat .ne. 0) WRITE(stderr,*) "Error removing files on "//host

  !999 close(file,iostat=stat)
  !    if (stat .ne. 0) stop "Error closing output file"

     RETURN
  END SUBROUTINE RUN_REMOTE


SUBROUTINE createBatchScript(filename)
  IMPLICIT none

  LOGICAL                     :: readfile = .FALSE.

  character(len=*),dimension(:),intent(in) :: filename
  ! access (must have passwordless SSH set up for this user)
  ! Details of the job to be submitted

  character(len=*), parameter :: job_script = "submit.pbs"
  character(len=*), parameter :: job_name = "klmcjob"
  integer                     :: num_jobs,num_cores
  integer                     :: walltime_h, walltime_m,walltime_s
  character(len=256)          :: job_cwd
  character(len=256)          :: executable=''

  ! File manipulation stuff
  character(len=*), parameter :: quiet = " > /dev/null 2>&1"
  character(len=*), parameter :: tmp_file = "tmp_file"
  character(len=*), parameter :: to_tmp = " > "//tmp_file//" 2>&1"
  character(len=81)           :: qstat_job_complete

  ! Misc vars.
  integer :: file,stat, i
  logical :: complete=.FALSE.
  character(len=256) :: buffer,jobid

  walltime_h = HOURS
  walltime_m = MINS
  walltime_s = SECS
  num_cores  = PROCESSORS

  ! Write out the job script to a file
  complete=.FALSE.
  job_cwd = TRIM(JOB_WORKING_DIR)
  executable = TRIM(EXEC)
  
  num_jobs = SIZE(filename)
  !write(*,*) 'There are ',num_jobs,'simulations to do!'

  open(unit=file,file=TRIM(RELAXED_FOLDER)//'/'//job_script,iostat=stat)
  if (stat .ne. 0) stop "Error opening script file"

  write(file,"(A)") "#!/bin/bash --login"
  write(file,"(A)") "#PBS -N "//job_name
  IF (ARCHER) THEN
     ! Check that if replacing a member of the population, then we dont use a
     ! lot of resource 
     IF (num_jobs == 1) THEN
        num_cores = min( (num_cores / N_POPULATION),24)
     END IF
     write(file,"(A,I0)") "#PBS -l select=",num_cores
  ELSE
     write(file,"(A,I0)") "#PBS -l mppwidth=",num_cores
     write(file,"(A,I0)") "#PBS -l mppnppn=",min(num_cores,32)
  END IF
  write(file,"(A,I0,A,I0,A,I0)") "#PBS -l walltime=",walltime_h,":",walltime_m,":",walltime_s
  write(file,"(A)") "#PBS -A "//TRIM(BUDGET)
  write(file,"(A)") "#PBS -o "//TRIM(job_cwd)//"/q.error"
  write(file,"(A)") "#PBS -e "//TRIM(job_cwd)//"/q.output"
  write(file,*) ""
  write(file,"(A)") "cd "//job_cwd
  write(file,*) ""
 ! write(file,*)  "export NPROC_TOT=`qstat -f $PBS_JOBID | awk '/mppwidth/ {print $3}'`"
!  write(file,*)  "export NTASK=`qstat -f $PBS_JOBID | awk '/mppnppn/  {print $3}'`"
  write(file,*)"NTASK="//TRIM(intToChar(NTASKS))
! Start the main script

  write(file,*) "date"
  write(file,*) "numSimsStart="//TRIM(intToChar(num_jobs))
  write(file,*) "ToRun=$((numSimsStart))"
  write(file,*) "# Initialise the array that knows which simulations to process"
  write(file,*) "for i in $(eval echo {1..$ToRun});do"
  write(file,*) "  work_array[$i]=$i"
  write(file,*) '  run_array[$i]="TRUE"'
  write(file,*) "done"
 
  write(file,*) "numSteps="//TRIM(intToChar(VASP_NSW))  !Make sure this matches the INCAR file NSW!!  
  write(file,*) "maxCount="//TRIM(intToChar(MAXVASPSTEPS))
  write(file,*) "#Loop until all done"
  write(file,*) "for count in $( eval echo {1..$maxCount});do"
  write(file,*) "#Loop over all structures to process"
  write(file,*) "  numSims=$((ToRun))"
  write(file,*) "#Work out what the work is to be done"   
  write(file,*) "  NoverS=$(($numSimsStart / $numSims))"
  write(file,*) "  W=$(($numSimsStart - ($numSims * $NoverS)))"
  write(file,*) "  for i in $(eval echo {1..$ToRun});do"
  write(file,*) "#work out how many procs to use"    
  write(file,*) "    if [ $i -le $W ];then" 
  write(file,*) "      NPROC=$(( $NTASK * ($NoverS + 1) ))"
  write(file,*) "    else"
  write(file,*) "      NPROC=$(( $NTASK * $NoverS))"
  write(file,*) "    fi"
  write(file,*) "    cd simulation_${work_array[$i]}/"
  write(file,*) "    if [ $count -gt 1 ];then"
  write(file,*) "       cp POSCAR POSCAR.lastStep"
  write(file,*) "       cp CONTCAR POSCAR"
  write(file,*) "       cp OUTCAR OUTCAR.lastStep"
  write(file,*) "    else"
  write(file,*) "       cp POSCAR POSCAR.orig"
  write(file,*) "    fi"
  write(file,*) "    echo $count >> run_position"
  write(file,*) "    echo Simulation_${work_array[$i]} has $NPROC processors >> ../info.out"
 ! write(file,*) "    aprun -N $NTASK -n $NPROC ${HOME}/bin/vasp >> simulation_${work_array[$i]}.out &" 
  write(file,*) "    aprun -n $NPROC "//TRIM(executable)//" >> simulation_${work_array[$i]}.out &" 
  write(file,*) "    cd .."
  write(file,*) "  done"
  write(file,*) "#Wait for all simulations to complete"
  write(file,*) "  wait"
  write(file,*) "  for i in $(eval echo {1..$ToRun});do"
  write(file,*) "     cd simulation_${work_array[$i]}/"
  write(file,*) '     flag=`grep -c "reached required accuracy" OUTCAR`'
  write(file,*) "     if [ ${flag} -eq 1 ];then"
  write(file,*) "       echo Simulation_${work_array[$i]} completed successfully >> ../info.out"
  write(file,*) '       run_array[${work_array[$i]}]="FALSE"'      
  write(file,*) "     else"
  write(file,*) '       flag=`grep -c "EDIFF is reached" OUTCAR`'
  write(file,*) "       if [ ${flag} -eq  $numSteps ];then"
  write(file,*) "         #All fine, just not finished!"
  write(file,*) "         echo Simulation_${work_array[$i]} SCF was successful >> ../info.out"
  write(file,*) "       else"
  write(file,*) "         echo Simulation_${work_array[$i]} failed SCF!  >> ../info.out"
  write(file,*) '         run_array[${work_array[$i]}]="FALSE" '
  write(file,*) "       fi"
  write(file,*) "     fi"
  write(file,*) "     cd .."
  write(file,*) "   done"
  write(file,*) "#Re-run jobs that need it"
  write(file,*) "   val=1"
  write(file,*) "   for i in $(eval echo {1..$ToRun});do"
  write(file,*) '   if [ "${run_array[${work_array[$i]}]}" == "TRUE" ];then'
  write(file,*) "     #There is work still to do!"
  write(file,*) "      work_array[$val]=${work_array[$i]}"
  write(file,*) "      val=$(($val+1))"
  write(file,*) "   else"
  write(file,*) "#Work has been completed"
  write(file,*) "     ToRun=$(($ToRun-1))"
  write(file,*) "   fi"
  write(file,*) " done"
  write(file,*) " if [ ${ToRun} -ge 1 ];then"
  write(file,*) "   numSims=$(($ToRun))"
  write(file,*) "   On iteration $count >> info.out"
  write(file,*) "   echo There are $numSims simulations still to process >> info.out"
  write(file,*) " else"
  write(file,*) "   echo All simulations complete! >> info.out"
  write(file,*) "   break"
  write(file,*) " fi"
  write(file,*) "if [ $count -eq $maxCount ];then"
  write(file,*) "    echo WARNING:Max count reached! >> info.out" 
  write(file,*) "fi" 
  write(file,*) "done"

  close(file,iostat=stat)
  if (stat .ne. 0) stop "Error closing script file"

  RETURN 
  END SUBROUTINE createBatchScript

SUBROUTINE CheckSystemIsUp
    IMPLICIT NONE
    LOGICAL :: up_flag = .TRUE.
    CHARACTER(LEN=*), PARAMETER :: tmp_file = "tmp_file"
    CHARACTER(LEN=*), PARAMETER :: to_tmp = " > "//tmp_file//" 2>&1"
    INTEGER :: counter=0   
    INTEGER :: stat,file
    INTEGER, PARAMETER  :: MAX_COUNT=20
    CHARACTER(LEN=256)  :: buffer
       
    DO WHILE (up_flag)
       CALL SYSTEM("ssh "//TRIM(USER)//"@"//TRIM(HOST)//" ls "//to_tmp, stat)
       OPEN(unit=file,file=tmp_file, status='old', iostat=stat)
       buffer = ""
       READ(file,"(A)", IOSTAT=stat) buffer
       IF (stat/=0) THEN 
          WRITE(STDERR,*)'WARNING! Problem reading tmp_file'
       ELSE   
         IF ( (INDEX(buffer,TRIM("Could not resolve hostname")) .ne. 0 ) .OR. &
              (INDEX(buffer,TRIM("No route to host"))           .ne. 0  ) ) THEN
            !Machine is down! 
            WRITE(*,*) 'Could not contact  '//TRIM(HOST)
            WRITE(*,*) 'Will try again in ',POLL_DELAY  
            CLOSE(file,iostat=stat,status='delete')
            counter = counter + 1
            IF (counter < MAX_COUNT) THEN
               CALL SLEEP(POLL_DELAY)
            ELSE 
               WRITE(STDERR,*) 'Host'//TRIM(HOST)//  &
               & ' appears to be unreachable - check details and re-submit' 
               STOP
            END IF   
         ELSE
           !Machine is up!
           up_flag = .FALSE.
           CLOSE(file,iostat=stat,status='delete')
         END IF
      END IF
    END DO
    RETURN
END SUBROUTINE 

  !==========================================================================================!

  SUBROUTINE removeClusterFiles(in_cluster)

    TYPE(cluster), INTENT(IN) :: in_cluster

    IF (OUTPUT_LEVEL < 10) THEN
      IF (OUTPUT_LEVEL > 5) THEN
        WRITE(stdsee,*)'Will now remove structure ',TRIM(in_cluster%id)
      ENDIF

      CALL SYSTEM('rm -f '// &
         TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//TRIM(in_cluster%id)//'.*')

      CALL SYSTEM('rm -f '// &
         TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//TRIM(in_cluster%id)//'s.*')
!    ELSE
!      WRITE(stdsee,*)'Would normally remove structure ',TRIM(in_cluster%id)
    ENDIF

    IF (N_PBC == 3) CALL removeArcFiles(in_cluster)

  END SUBROUTINE removeClusterFiles

  !==========================================================================================
  ! A routine to remove cluster files
  !==========================================================================================!
  SUBROUTINE removeClusterFilesX(in_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster

    CALL SYSTEM('rm -f '// TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//TRIM(in_cluster%id)//'.*')

    CALL SYSTEM('rm -f '// TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//TRIM(in_cluster%id)//'s.*')

    IF (N_PBC == 3) CALL removeArcFiles(in_cluster)

  END SUBROUTINE removeClusterFilesX

  !==========================================================================================!

SUBROUTINE writeArcFile(in_cluster)
   TYPE(cluster), INTENT(IN) :: in_cluster
   INTEGER           :: i,ierror,ide
   INTEGER,PARAMETER :: arc_unit = 79
   CHARACTER(LEN=256) :: OUTPUT_FOLDER=' '
   ide=in_cluster%edefn 

   OUTPUT_FOLDER = TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)


   OPEN(UNIT=arc_unit,FILE=TRIM(OUTPUT_FOLDER)//& 
                           TRIM(in_cluster%id)//'_3D.arc',&
                           IOSTAT=ierror)


   WRITE(arc_unit,*) '!BIOSYM archive 3'
   WRITE(arc_unit,*) 'PBC=ON'
   WRITE(arc_unit,*) '                            ',in_cluster%energy(ide)
   WRITE(arc_unit,*) '!DATE'
   WRITE(arc_unit,1001) in_cluster%box(1),in_cluster%box(2),in_cluster%box(3), &
                        in_cluster%box(4),in_cluster%box(5),in_cluster%box(6)
   DO i =1,N_ATOMS
      WRITE(arc_unit,*) in_cluster%atoms(i)%symbol, & 
                        in_cluster%atoms(i)%xc,&
                        in_cluster%atoms(i)%yc,&
                        in_cluster%atoms(i)%zc,&
                        'CORE', i, in_cluster%atoms(i)%symbol,' ',&
                        in_cluster%atoms(i)%symbol,&
                        in_cluster%atoms(i)%charge
   END DO
   WRITE(arc_unit,*) 'end'
   WRITE(arc_unit,*) 'end'
   CLOSE(arc_unit)

1001 FORMAT('PBC',6F10.4,'(unknown)') 

   RETURN
END SUBROUTINE writeArcFile
  
  
  
  SUBROUTINE removeArcFiles(in_cluster)
      TYPE(cluster), INTENT(IN) :: in_cluster
      LOGICAL :: lold

      INQUIRE(              &
           FILE=TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//TRIM(in_cluster%id)//'_bulk.arc', &
           EXIST=lold)

      IF (OUTPUT_LEVEL < 10) THEN
        IF (OUTPUT_LEVEL > 5) THEN
          WRITE(stdsee,*)'Will now remove Arc file for cluster ',TRIM(in_cluster%id)
        ENDIF
        IF (lold) THEN
          CALL SYSTEM('rm -f '// &
           TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//TRIM(in_cluster%id)//'_bulk.*')
          CALL SYSTEM('rm -f '// &
           TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//TRIM(in_cluster%id)//'s_bulk.*')
        ELSE
          CALL SYSTEM('rm -f '// &
           TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//TRIM(in_cluster%id)//'_3D.*')
          CALL SYSTEM('rm -f '// &
           TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//TRIM(in_cluster%id)//'s_3D.*')
        ENDIF
      ELSE
        WRITE(stdsee,*)'Will normally remove Arc file for cluster ',TRIM(in_cluster%id)
      ENDIF
  END SUBROUTINE removeArcFiles

  !==========================================================================================!

  SUBROUTINE copyClusterFiles(in_new,in_cluster)
      TYPE(cluster), INTENT(IN) :: in_cluster
      INTEGER, INTENT(IN)       :: in_new
      CHARACTER(LEN=200)        :: foldername=''
      CHARACTER(LEN=200)        :: filename1=''
      CHARACTER(LEN=200)        :: filename2=''
      CHARACTER(LEN=200)        :: filename3=''
      CHARACTER(LEN=9)          :: fileMid=''
      CHARACTER(LEN=7)          :: fileEnd=''
      CHARACTER(LEN=10)         :: idb
      CHARACTER(LEN=5)          :: idnum
      INTEGER                   :: i,idn,nlast,ide
      LOGICAL                   :: lold,lpbc

  !   in_cluster is the new LM found and has a rank of in_new
  !   if in_new=0 then no moving reqd, simply write out energies
  !   BEST arrays have already been updated - here we simply want
  !   to rename, copy or remove the files associated with these LM

      foldername = TRIM(WORKING_DIR)//TRIM(BEST_FOLDER)
      filename1 = ''
      filename2 = ''
      fileMid = ''
      fileEnd = ''

      IF (in_new > 0) THEN

        idnum(1:5)='     '

        idb=ID_BEST(in_new)

        ide = in_cluster%edefn

        IF ((DM_FLAG) .AND. (JOB_TYPE .EQ. 0) .AND. (ide .EQ. -1)) THEN
          ide = 1
        ENDIF

        ! File type
        IF (DEF_ENERGY(ide) == 'D') THEN
          fileEnd(1:4)='.car'
          fileMid(1:9)='.car     '

        ELSEIF (N_PBC == 3) THEN
        !lpbc = (index(UNIT_CELL,'cell').ne.0.OR.index(UNIT_CELL,'vector').ne.0)

        !IF (lpbc) THEN

          INQUIRE(              &
              FILE=TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//TRIM(idb)//'/'//TRIM(idb)//'_bulk.arc', &
              EXIST=lold)

          fileEnd(1:4)='.arc'
          IF (lold) THEN
            fileMid(1:9)='_bulk.arc'
          ELSE
            fileMid(1:9)='_3D.arc  '
          ENDIF
        ELSE
          INQUIRE(              &
            FILE=TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//TRIM(idb)//'_clus.arc', &
            EXIST=lold)

          fileEnd(1:4)='.xyz'
          fileMid(1:9)='.xyz     '

          IF (lold) THEN
            fileMid(1:9)='_clus.arc'
    !     ELSE
    !       fileMid(1:9)='_0D.arc  '
          ENDIF
        ENDIF

        IF (FILLED_BEST_ARRAYS) THEN ! need to remove files for departing cluster
          CALL num2str(INT_MAX_BEST,INT_BEST,idnum)
          filename1 = TRIM(foldername)//TRIM(ID_BEST(INT_MAX_BEST+1))//'-'//TRIM(idnum)//'*'
          IF (DEBUG_LEVEL>0) THEN
            !CALL SYSTEM('ls '//TRIM(foldername)//' ')
            !WRITE(stdsee,*)'SYSTEM CALL:','rm -f '//filename1//' '
          ENDIF
          CALL SYSTEM('rm -f '//filename1//' ')
          ID_BEST(INT_MAX_BEST+1)=ID_BEST(INT_MAX_BEST)
        ENDIF

        IF (LIMIT_BEST.eq.0) THEN !keep all files for top LM
          DO idn=INT_MAX_BEST,in_new+1,-1
            idb=ID_BEST(idn) ! ID_BEST already updated
            IF (idb(1:1).ne.'x') THEN  ! move files
              CALL num2str(idn-1,INT_BEST,idnum)
              filename1=TRIM(foldername)//TRIM(idb)//'-'//TRIM(idnum)//TRIM(fileEnd)
              CALL num2str(idn,INT_BEST,idnum)
              filename2=TRIM(foldername)//TRIM(idb)//'-'//TRIM(idnum)//TRIM(fileEnd)
              !WRITE(6,*)'SYSTEM CALL:','mv -f '//filename1//' '//filename2
              CALL SYSTEM('mv -f '//filename1//' '//filename2)
            ENDIF
          ENDDO
        ELSE !restrict the number of files that are kept
          nlast=0 !counts the number of LM already moved
          DO idn=INT_MAX_BEST,in_new+1,-1
            idb=ID_BEST(idn) ! ID_BEST already updated
            IF (idb(1:1).ne.'x'.AND.idb(1:1).ne.'X') THEN  ! files exist
              nlast=nlast+1
              !NB LM can be part of top and bottom set but never move between
              !   as LM never move up and going down requires loosing a LM
              !   which is not allowed if LIMIT_BEST > 0
              IF (nlast.le.LIMIT_BEST.OR.idn.le.LIMIT_BEST) THEN ! move files
                CALL num2str(idn-1,INT_BEST,idnum)
                filename1=TRIM(foldername)//TRIM(idb)//'-'//TRIM(idnum)//TRIM(fileEnd)
                CALL num2str(idn,INT_BEST,idnum)
                filename2=TRIM(foldername)//TRIM(idb)//'-'//TRIM(idnum)//TRIM(fileEnd)
                !WRITE(6,*)'SYSTEM CALL:','mv -f '//filename1//' '//filename2
                CALL SYSTEM('mv -f '//filename1//' '//filename2)
              ELSEIF (idn.eq.LIMIT_BEST+1) THEN ! LM no longer part of top set so remove files
                CALL num2str(idn-1,INT_BEST,idnum)
                filename1=TRIM(foldername)//TRIM(idb)//'-'//TRIM(idnum)//TRIM(fileEnd)
                !WRITE(6,*)'SYSTEM CALL:','rm -f '//filename1
                CALL SYSTEM('rm -f '//filename1)
                ID_BEST(idn)(1:1)='X' ! mark to indicate files have been removed
              ENDIF
            ENDIF
          ENDDO
    !     completed tasks for files already stored for each LM with higher energy than new LM

          IF (nlast.lt.LIMIT_BEST) THEN ! new LM found in bottom set
            idn = in_new + nlast   ! the last LM in the bottom set
            idn = idn - LIMIT_BEST ! the LM that just climbed out of the bottom set
            IF (idn.gt.LIMIT_BEST) THEN ! it is also not within the top set
              CALL num2str(idn,INT_BEST,idnum) ! no change in rank for LM above in_new
              filename1=TRIM(foldername)//TRIM(ID_BEST(idn))//'-'//TRIM(idnum)//TRIM(fileEnd)
              !WRITE(6,*)'SYSTEM CALL:','rm -f '//filename1
              CALL SYSTEM('rm -f '//filename1)
              ID_BEST(idn)(1:1)='X' ! mark to indicate files have been removed
            ENDIF
          ENDIF
        ENDIF
    !   Completed tasks for original files

    !   Now consider saving files for the new LM
        IF (     LIMIT_BEST.eq.0          &
            .OR.     in_new.le.LIMIT_BEST &
            .OR.      nlast.lt.LIMIT_BEST ) THEN
          CALL num2str(in_new,INT_BEST,idnum)
          idb=ID_BEST(in_new)
          filename1=TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//TRIM(idb)//TRIM(fileMid)
          filename2=TRIM(foldername)//TRIM(idb)//'-'//TRIM(idnum)//TRIM(fileEnd)
          !WRITE(6,*)'SYSTEM CALL:','cp '//filename1//' '//filename2
          CALL SYSTEM('cp '//filename1//' '//filename2)
        ELSE ! new LM is between the top and bottom LIMIT_BEST LM
          ID_BEST(in_new)(1:1)='X' ! mark to indicate files do not exist in best folder
        ENDIF

      ENDIF !(in_new > 0)

      filename1 = TRIM(foldername)//'statistics'

      OPEN(UNIT=10,FILE=filename1)
      REWIND(10)
      WRITE(10,*)INT_MAX_BEST
      DO i=1,INT_MAX_BEST
         WRITE(10,*)i,ID_BEST(i),R_BEST_ENERGIES(i),N_BEST_ENERGIES(i)
      ENDDO
      CLOSE(10)

      filename2 = TRIM(foldername)//'energies'

      OPEN(UNIT=10,FILE=filename2)
      REWIND(10)
      DO i=1,INT_MAX_BEST

        IF ((DM_FLAG) .AND. (JOB_TYPE .EQ. 0)) THEN
          WRITE(10,*)i,R_BEST_ENERGIES(i)

        ELSEIF (index(ID_BEST(i),'A').eq.1) THEN
          WRITE(10,*)i,A_BEST_ENERGIES(i)

        ELSEIF (index(ID_BEST(i),'B').eq.1) THEN
          WRITE(10,*)i,A_BEST_ENERGIES(i),B_BEST_ENERGIES(i)

        ELSEIF (index(ID_BEST(i),'C').eq.1) THEN
          WRITE(10,*)i,A_BEST_ENERGIES(i),B_BEST_ENERGIES(i),C_BEST_ENERGIES(i)

        ELSEIF (index(ID_BEST(i),'D').eq.1) THEN
          WRITE(10,*)i,A_BEST_ENERGIES(i),B_BEST_ENERGIES(i),C_BEST_ENERGIES(i),D_BEST_ENERGIES(i)

        ELSEIF (index(ID_BEST(i),'E').eq.1) THEN
          WRITE(10,*)i,A_BEST_ENERGIES(i),B_BEST_ENERGIES(i),C_BEST_ENERGIES(i),D_BEST_ENERGIES(i), &
            E_BEST_ENERGIES(i)

        ELSEIF (index(ID_BEST(i),'F').eq.1) THEN
          WRITE(10,*)i,A_BEST_ENERGIES(i),B_BEST_ENERGIES(i),C_BEST_ENERGIES(i),D_BEST_ENERGIES(i), &
            E_BEST_ENERGIES(i),F_BEST_ENERGIES(i)
        ENDIF

      ENDDO
      CLOSE(10)

      IF (L_USE_TOP_ANALYSIS) THEN
        filename3 = TRIM(foldername)//TRIM(HASHKEYS_FILE)

        OPEN(UNIT=10,FILE=filename3)
        REWIND(10)
        WRITE(10,*)INT_MAX_BEST
        DO i=1,INT_MAX_BEST
           WRITE(10,*) i, ID_BEST(i), BEST_HASHKEYS(i)
        ENDDO
        CLOSE(10)
      ENDIF

      IF (.NOT.FILLED_BEST_ARRAYS) THEN
       idb=ID_BEST(INT_MAX_BEST) ! Name of the last best cluster in array
       IF (idb(1:1).ne.'x') THEN ! MAX_BEST configurations already found
        IF (LIMIT_BEST.ne.0) THEN
          print*,'Arrays not big enough to store all LM'
          STOP
        ELSE
          FILLED_BEST_ARRAYS=.TRUE.
          ID_BEST(INT_MAX_BEST+1)=ID_BEST(INT_MAX_BEST)
        ENDIF
       ENDIF
      ENDIF
  END SUBROUTINE copyClusterFiles

  !==========================================================================================!

  SUBROUTINE retrieveData(inout_cluster, in_filename)
      TYPE(cluster), INTENT(INOUT) :: inout_cluster
      CHARACTER(LEN=*), INTENT(IN) :: in_filename
      REAL(KIND=DBL),DIMENSION(6) :: LOCAL_CELL(6)
      LOGICAL :: pbc

      pbc=(index(UNIT_CELL,'cell').ne.0.OR.index(UNIT_CELL,'vector').ne.0)

      IF (pbc) THEN
        CALL readARC(inout_cluster, in_filename)
        CALL updateCell(inout_cluster,LOCAL_CELL)
        CALL lattice_c2v_3D(LOCAL_CELL,VECTOR)
      ELSE
        CALL readXYZ(inout_cluster, in_filename)
      ENDIF

  END SUBROUTINE retrieveData

  !==========================================================================================!

  SUBROUTINE retrieveAimsData (input_name,in_cluster)
      TYPE(cluster), INTENT(INOUT)  :: in_cluster
      CHARACTER(LEN=*),INTENT(IN)   :: input_name
      CHARACTER(LEN=20)             :: junk
      CHARACTER(LEN=81)             :: fname,buffer
      INTEGER                       :: i,j,ierror
      INTEGER,PARAMETER             :: in_unit=1
      LOGICAL                       :: pbc=.FALSE.
      LOGICAL                       :: readflag

      IF (N_PBC > 0) pbc = .TRUE.

      readflag = .FALSE.
      fname =  &
      TRIM(RELAXED_FOLDER)//TRIM(input_name)//'/'//TRIM(input_name)//'.out'
      OPEN(in_unit,STATUS='OLD',IOSTAT=ierror,FILE=TRIM(fname)) 
      IF (ierror /=0) THEN
          WRITE(stderr,*) 'Error opening FHI-aims output for reading'
          STOP
      END IF
      !READ THE NEW POSITIONS
       DO
          READ(UNIT=in_unit,FMT=*, IOSTAT=ierror, IOMSG=ERROR_MSG) buffer
          IF(ierror /= 0) THEN
              ! Search and copy until structural data is found
              ! Read in cell dimensions and flags if found
              !WRITE(stderr,*) ERROR_MSG
              EXIT
          END IF
         IF(TRIM(buffer) == 'Final' ) THEN
             IF ( .NOT. pbc) THEN
                READ(in_unit,*) junk
                DO i = 1,N_ATOMS
                   READ(in_unit,*)  &
                   junk,in_cluster%atoms(i)%xc,in_cluster%atoms(i)%yc,&
                   in_cluster%atoms(i)%zc,in_cluster%atoms(i)%symbol
                END DO
             ELSE
                !Read in cell parameters as well
                READ(in_unit,*) junk
                DO i = 1,3
                   READ(in_unit,*)&
                   junk,VECTOR(1,i),VECTOR(2,i),VECTOR(3,i)
                  ! WRITE(*,*) VECTOR(1,i),VECTOR(2,i),VECTOR(3,i)
                END DO
                DO i = 1,N_ATOMS
                   READ(in_unit,*)  &
                   junk,in_cluster%atoms(i)%xc,in_cluster%atoms(i)%yc,&
                   in_cluster%atoms(i)%zc,in_cluster%atoms(i)%symbol
                !   WRITE(*,*) in_cluster%atoms(i)%symbol,in_cluster%atoms(i)%xc
                END DO 
                READ(in_unit,*) junk
                READ(in_unit,*) junk
                DO i = 1,N_ATOMS
                   READ(in_unit,*)  &
                   junk,in_cluster%atoms(i)%xf,in_cluster%atoms(i)%yf,&
                   in_cluster%atoms(i)%zf,in_cluster%atoms(i)%symbol
                !   WRITE(*,*) in_cluster%atoms(i)%symbol,in_cluster%atoms(i)%xf
                 END DO
             ENDIF !PBC
            readflag = .TRUE.
          END IF !Found coords
          IF (readflag) EXIT
       END DO
       IF (.NOT. readflag) THEN
         !aims must have failed to run
         WRITE(stderr,*)'WARNING in retrieveAimsData: No data to extract'
       ELSE
         !Let's create an output file
      CALL SYSTEM(TRIM(DATA_FOLDER)//'aims2xyz.sh '//TRIM(RELAXED_FOLDER) &
                  //'/'//TRIM(input_name)//'/'//TRIM(input_name)//'.out')
       END IF
    RETURN
  END SUBROUTINE retrieveAimsData

  !==========================================================================================!

  SUBROUTINE retrieveVaspData(input_name,in_cluster)
     TYPE(cluster), INTENT(INOUT)  :: in_cluster
     CHARACTER(LEN=*),INTENT(IN)   :: input_name
     CHARACTER(LEN=20)              :: junk
     CHARACTER(LEN=81)             :: fname,buffer
     INTEGER                       :: i,j,ierror
     INTEGER,PARAMETER             :: in_unit=1
     LOGICAL                       :: pbc=.FALSE.
     LOGICAL                       :: readflag
     REAL(KIND=DBL)                 :: scaling

     IF (N_PBC > 0) pbc = .TRUE.
        fname =  &
        TRIM(RELAXED_FOLDER)//TRIM(input_name)//'/'//'CONTCAR'
        OPEN(in_unit,STATUS='OLD',IOSTAT=ierror,FILE=TRIM(fname)) 
        IF (ierror /=0) THEN
           WRITE(stderr,*) 'Error opening VASP CONTCAR for reading'
           STOP
         END IF

        IF (pbc) THEN
           READ(in_unit,*) junk
           READ(in_unit,*) scaling

           DO i = 1,3
              READ(in_unit,*) (VECTOR(j,i),j=1,3)
           END DO
           ! Sort out the scaling
           DO i = 1,3
              DO j = 1,3
                 VECTOR(j,i) = VECTOR(j,i) * scaling
             END DO
           END DO

        ELSE
           WRITE(STDERR,*) 'Error: Clusters not supported for VASP yet'
           STOP
        END IF

        READ(in_unit,*) junk
        READ(in_unit,*) junk
        READ(in_unit,*) buffer

        IF (TRIM(buffer) == 'Direct' ) THEN 

           !READ THE NEW POSITIONS AS FRACTIONALS
           readflag = .FALSE.
           DO i = 1,N_ATOMS
              READ(UNIT=in_unit,FMT=*, IOSTAT=ierror, IOMSG=ERROR_MSG) &
                                               in_cluster%atoms(i)%xf, &
                                               in_cluster%atoms(i)%yf, &
                                               in_cluster%atoms(i)%zf 
              IF(ierror /= 0) THEN
                 WRITE(stderr,*) ERROR_MSG
                 readflag = .FALSE.
                 EXIT
              END IF        
              readflag = .TRUE.
           END DO
            
           IF (.NOT. readflag) THEN
              !VASP must have failed to run
              WRITE(stderr,*)'WARNING in retrieveVaspData: No data to extract'
           ELSE
              !All ok so convert lattice vector to box
              CALL vect_to_box(in_cluster,VECTOR)
              ! Convert fractional to cartesian
              CALL xftoxc(in_cluster)
              !Let's create an output file
              CALL writeArcFile(in_cluster) 
           END IF
        END IF
     RETURN
  END SUBROUTINE retrieveVaspData

  SUBROUTINE retrieveNWChemData(input_name,in_cluster)
     TYPE(cluster), INTENT(INOUT)  :: in_cluster
     CHARACTER(LEN=*),INTENT(IN)   :: input_name
     CHARACTER(LEN=20)              :: junk,num_lines
     CHARACTER(LEN=81)             :: fname,buffer
     INTEGER                       :: i,j,ierror,val
     INTEGER,PARAMETER             :: in_unit=1
     LOGICAL                       :: pbc=.FALSE.
     LOGICAL                       :: readflag
     REAL(KIND=DBL)                :: scaling
     CHARACTER(1)                  :: quot
     
     quot=char(ichar("'"))

     IF (N_PBC > 0) pbc = .TRUE.
        
        num_lines = TRIM(intToChar(6+N_ATOMS))
        CALL SYSTEM('grep -A'//TRIM(num_lines)//' Geometry ' &
                    //TRIM(RELAXED_FOLDER)//TRIM(input_name) & 
                    //'/output.nw | tail -'//TRIM(intToChar(N_ATOMS))//'| awk ' &
                    //TRIM(quot)//'{print $2,$4,$5,$6}'//TRIM(quot)&
                    //' > '//TRIM(RELAXED_FOLDER)//TRIM(input_name)//'/geom.tmp')
        
        fname =  &
        TRIM(RELAXED_FOLDER)//TRIM(input_name)//'/geom.tmp'
        OPEN(in_unit,STATUS='OLD',IOSTAT=ierror,FILE=TRIM(fname)) 
        IF (ierror /=0) THEN
           WRITE(stderr,*) 'Error opening NWChem output  for reading'
           STOP
         END IF

        DO i=1,N_ATOMS
           READ(in_unit,*) in_cluster%atoms(i)%symbol,&
                           in_cluster%atoms(i)%xc, &
                           in_cluster%atoms(i)%yc, &
                           in_cluster%atoms(i)%zc 
        END DO
        CALL outputXYZ(in_cluster,'NWChem output',& 
            TRIM(RELAXED_FOLDER)//'/'//TRIM(input_name)//'/'//TRIM(input_name))
     RETURN
  END SUBROUTINE retrieveNWChemData

  !==========================================================================================!

  SUBROUTINE readARC(inout_cluster, in_filename)
      TYPE(cluster), INTENT(INOUT) :: inout_cluster
      CHARACTER(LEN=*), INTENT(IN) :: in_filename
      INTEGER :: in_error=1, in_unit=26, ide
      CHARACTER(LEN=MAX_GULP_COLUMN_WIDTH), DIMENSION(MAX_GULP_COLUMNS) :: buffer=''
      REAL(KIND=DBL) :: energy
      INTEGER :: n,m
      LOGICAL :: L_FOUND, lold
      !DATA  ev_to_kcal/23.0604d0/  ! as used in GULP 4.0
      !DATA  kcal_to_ev/0.043364382231d0/  
      REAL(KIND=DBL), PARAMETER  :: to_kcal    = 23.0604_dp  ! as used in GULP 4.0
      REAL(KIND=DBL), PARAMETER  :: kcal_to_ev = 0.043364382231_dp

  ! NO LONGER SET N_ATOMS = N_ARC_ATOMS AS NOW CHECK FOR VACANCY FLAG 'x'
  ! THIS ALLOWS MORE FLEXIBILITY WITH HOW FILLED SITES AND VAC ARE ARRANGED
  ! NB ASSUME N_ATOMS INCLUDE EXTRA ATOMS OUTSIDE SOLUTION IF COMPUTING RDF 

      IF (DEBUG_LEVEL > 0) THEN
        CALL printEmptyLine
        WRITE(stdsee,*)'Retrieving Data from ARC file: ',in_filename
      ENDIF

      L_FOUND=(index(in_filename,'.arc')/=0)

      IF (L_FOUND) THEN
       OPEN(UNIT=in_unit,   &
           FILE=TRIM(WORKING_DIR)//TRIM(in_filename), &
           STATUS='OLD',    &
           IOSTAT=in_error, &
           IOMSG=ERROR_MSG)
           IF (in_error == 0) THEN
             READ(UNIT=in_unit,FMT=*, IOSTAT=in_error, IOMSG=ERROR_MSG) buffer(1:3)
           ENDIF
           IF (in_error == 0) THEN
             lold=(buffer(3)(1:1) == '2') 
             IF (lold) THEN
               m=8
             ELSE
               m=9
             ENDIF
           ENDIF
           REWIND(UNIT=in_unit)
      ELSE
       INQUIRE(              &
           FILE=TRIM(WORKING_DIR)//TRIM(in_filename)//'_bulk.arc', &
           EXIST=lold)

       IF (lold) THEN
         OPEN(UNIT=in_unit,   &
           FILE=TRIM(WORKING_DIR)//TRIM(in_filename)//'_bulk.arc', &
           STATUS='OLD',    &
           IOSTAT=in_error, &
           IOMSG=ERROR_MSG)
           m=8
       ELSE
         OPEN(UNIT=in_unit,   &
           FILE=TRIM(WORKING_DIR)//TRIM(in_filename)//'_3D.arc', &
           STATUS='OLD',    &
           IOSTAT=in_error, &
           IOMSG=ERROR_MSG)
           m=9
       ENDIF
      ENDIF

      IF (in_error == 0) THEN
          READ(UNIT=in_unit,FMT=*, IOSTAT=in_error, IOMSG=ERROR_MSG) buffer(1:5)
          IF(buffer(5)(1:1) /= '*') THEN
              ide=inout_cluster%edefn
              READ(buffer(5),'(1F14.10)') energy
              inout_cluster%energy(ide)=energy*kcal_to_ev
          END IF
          READ(UNIT=in_unit,FMT=*, IOSTAT=in_error, IOMSG=ERROR_MSG) buffer(1:m)
          READ(buffer(3),'(1F14.10)') inout_cluster%box(1)
          READ(buffer(4),'(1F14.10)') inout_cluster%box(2)
          READ(buffer(5),'(1F14.10)') inout_cluster%box(3)
          READ(buffer(6),'(1F14.10)') inout_cluster%box(4)
          READ(buffer(7),'(1F14.10)') inout_cluster%box(5)
          READ(buffer(8),'(1F14.10)') inout_cluster%box(6)
          IF (lold) m=10
          readloop: DO n = 1, N_ATOMS
            IF (inout_cluster%atoms(n)%cstype /= 'x') THEN
              READ(UNIT=in_unit,FMT=*, IOSTAT=in_error, IOMSG=ERROR_MSG) buffer(1:m)
              READ(buffer(8),'(A)') inout_cluster%atoms(n)%symbol
              READ(buffer(2),'(1F14.10)') inout_cluster%atoms(n)%xc
              READ(buffer(3),'(1F14.10)') inout_cluster%atoms(n)%yc
              READ(buffer(4),'(1F14.10)') inout_cluster%atoms(n)%zc
            ENDIF
          END DO readloop
      ELSE
         WRITE(*,*) ERROR_MSG
      END IF
      CLOSE(in_unit)

      CALL xctoxf(inout_cluster)

      IF (DEBUG_LEVEL > 0) THEN
        CALL printEmptyLine
        CALL peek(inout_cluster)
        CALL printEmptyLine
        WRITE(stdsee,*)'Readin from ARC file COMPLETED'
        CALL printEmptyLine
      ENDIF

      IF (LOCKDOWN.AND.EXCEPTIONS(inout_cluster)) LOCKDOWN=.FALSE.
      IF (LOCKDOWN) STOP
  END SUBROUTINE readARC

  !==========================================================================================!

  LOGICAL FUNCTION EXCEPTIONS(in_cluster)
      TYPE(cluster), INTENT(IN) :: in_cluster
      INTEGER :: n
      EXCEPTIONS=.TRUE.

      readloop: DO n = 1, N_ATOMS
          !IF (in_cluster%atoms(n)%symbol == 'Zn') WRITE(*,*)'Zn found for atom ',n
          !IF (in_cluster%atoms(n)%symbol == 'O') WRITE(*,*)'O  found for atom ',n
          !IF (in_cluster%atoms(n)%symbol /= 'Zn' .AND. in_cluster%atoms(n)%symbol /= 'O') EXCEPTIONS=.FALSE.
          !IF (in_cluster%atoms(n)%symbol /= 'Si' .AND. in_cluster%atoms(n)%symbol /= 'Al' .AND. &
          !    in_cluster%atoms(n)%symbol /= 'Mg' .AND. in_cluster%atoms(n)%symbol /= 'O' ) EXCEPTIONS=.FALSE.
      END DO readloop

  END FUNCTION EXCEPTIONS

  !==========================================================================================!

  SUBROUTINE readXYZ(inout_cluster, in_filename)
      TYPE(cluster), INTENT(INOUT) :: inout_cluster
      CHARACTER(LEN=*), INTENT(IN) :: in_filename
      INTEGER :: in_error=1, in_unit=26
      CHARACTER(LEN=MAX_GULP_COLUMN_WIDTH), DIMENSION(MAX_GULP_COLUMNS) :: buffer=''
      INTEGER :: n, ide, num_atoms
      CHARACTER(LEN=100) :: message

      OPEN(UNIT=in_unit, FILE=TRIM(WORKING_DIR)//TRIM(in_filename)//'.xyz', &
        STATUS='OLD', IOSTAT=in_error, IOMSG=ERROR_MSG)

      IF (in_error /=0) THEN
         WRITE(STDERR,*) 'Error opening '//TRIM(WORKING_DIR)//TRIM(in_filename)//'.xyz'
         STOP
      END IF

      IF(in_error == 0) THEN

        READ(UNIT=in_unit,FMT=*,IOSTAT=in_error,IOMSG=ERROR_MSG) num_atoms
        
        IF (in_error == 0) THEN 
           IF (num_atoms /= N_ATOMS) THEN 

			  WRITE(message, *) 'WARNING! Number of atoms in XYZ file does not '
			  CALL print2All("readXYZ", message, 0)
			  WRITE(message, *) 'match  KLMC internal number of atoms for a cluster! (', N_ATOMS, ')'
			  CALL print2All("readXYZ", message, 0)
			  WRITE(message, *) 'Setting N_ATOMS = ', num_atoms 
			  CALL print2All("readXYZ", message, 0)
			  WRITE(message, *) 'Check results carefully!'
			  CALL print2All("readXYZ", message, 0)
			  
              N_ATOMS = num_atoms
            END IF  
        ELSE
            WRITE(STDERR,*) 'Error reading xyz file'
        END IF
        
        !Need to rewind unit to make sure values line-up
        REWIND(UNIT=in_unit)
        READ(UNIT=in_unit,FMT=*, IOSTAT=in_error, IOMSG=ERROR_MSG) buffer(1:4)

        IF (inout_cluster%edefn .EQ. -1) THEN
          ide = 0
        ELSE
          ide = inout_cluster%edefn
        ENDIF

        inout_cluster%energy(ide) = R_ENERGY_ZERO

        IF(buffer(4)(1:1) /= '*') THEN
            !READ(buffer(4),'(1F14.10)') inout_cluster%energy(ide)
            READ(buffer(4),'(1F24.8)', ERR=518) inout_cluster%energy(ide) ! this is what is in GULP
        ENDIF

518     CONTINUE
        readloop: DO n = 1, N_ATOMS
          IF (inout_cluster%atoms(n)%cstype /= 'x') THEN
            !READ(UNIT=in_unit,FMT=*, IOSTAT=in_error, IOMSG=ERROR_MSG) buffer(1:4)
            !READ(buffer(1),'(A)') inout_cluster%atoms(n)%symbol
            !READ(buffer(2),'(1F14.10)') inout_cluster%atoms(n)%xc ! Gulp has F20.9
            !READ(buffer(3),'(1F14.10)') inout_cluster%atoms(n)%yc ! Gulp has F20.9
            !READ(buffer(4),'(1F14.10)') inout_cluster%atoms(n)%zc ! Gulp has F20.9
            READ(UNIT=in_unit,FMT=*, IOSTAT=in_error, IOMSG=ERROR_MSG) &
                                     inout_cluster%atoms(n)%symbol,    &
                                     inout_cluster%atoms(n)%xc ,        &
                                     inout_cluster%atoms(n)%yc ,        &
                                     inout_cluster%atoms(n)%zc

          ENDIF
        END DO readloop
      ELSE
         WRITE(*,*) ERROR_MSG
      END IF
      CLOSE(in_unit)

      IF (LOCKDOWN.AND.EXCEPTIONS(inout_cluster)) LOCKDOWN=.FALSE.
      IF (LOCKDOWN) STOP
  END SUBROUTINE readXYZ

  !==========================================================================================!
  ! A routine to read in a xyz file for solid solutions
  !==========================================================================================!
  SUBROUTINE readXYZ_SS(inout_cluster, in_filename)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    CHARACTER(LEN=*), INTENT(IN) :: in_filename

    INTEGER :: in_error=1, in_unit=26
    CHARACTER(LEN=MAX_GULP_COLUMN_WIDTH), DIMENSION(MAX_GULP_COLUMNS) :: buffer=''
    INTEGER :: n, clusterIdx
    LOGICAL :: checkVacancy


    CALL printLog('readXYZ_SS', 'Reading in: ' // TRIM(in_filename), 5)

    OPEN(UNIT=in_unit, FILE=TRIM(in_filename), STATUS='OLD', IOSTAT=in_error, IOMSG=ERROR_MSG)

    IF (in_error /= 0) THEN
      OPEN(UNIT=in_unit, FILE=TRIM(in_filename) // '.xyz', STATUS='OLD', IOSTAT=in_error, IOMSG=ERROR_MSG)
    ENDIF

    IF (in_error /= 0) THEN
      CALL print2All('readXYZ_SS', ERROR_MSG, 0)
      STOP
    END IF

    IF(in_error == 0) THEN
      inout_cluster%energy(0) = R_ENERGY_ZERO

      READ(UNIT=in_unit,FMT=*, IOSTAT=in_error, IOMSG=ERROR_MSG) buffer(1:4)

      ! The last N_VAC must be skipped (vacancies are not saved in the xyz file)

      clusterIdx = 1
      DO n = 1, N_ATOMS - N_VAC

        ! Skip the positions of vacancies
        checkVacancy = .TRUE.
        DO WHILE (checkVacancy)
          IF (index(inout_cluster%atoms(clusterIdx)%cstype,'x') .NE. 0) THEN
            clusterIdx = clusterIdx + 1
          ELSE
            checkVacancy = .FALSE.
          ENDIF
        ENDDO

        READ(UNIT=in_unit,FMT=*, IOSTAT=in_error, IOMSG=ERROR_MSG) &
          inout_cluster%atoms(clusterIdx)%symbol, inout_cluster%atoms(clusterIdx)%xc, &
          inout_cluster%atoms(clusterIdx)%yc, inout_cluster%atoms(clusterIdx)%zc

        clusterIdx = clusterIdx + 1
      ENDDO
    ELSE
      CALL print2All('readXYZ_SS', ERROR_MSG, 0)
    END IF

    CLOSE(in_unit)

  END SUBROUTINE readXYZ_SS

  !==========================================================================================!

  SUBROUTINE outputCAR(in_cluster, in_message, in_filename, addPressureAtoms)
    TYPE(cluster),     INTENT(IN) :: in_cluster
    CHARACTER(LEN=*),  INTENT(IN) :: in_message, in_filename
    LOGICAL, OPTIONAL, INTENT(IN) :: addPressureAtoms

    INTEGER :: out_error=1, out_unit=31, n, ide, in_error=1, in_unit=26
    CHARACTER(LEN=8) :: symAndNum

    INTEGER ::  i, j, k, nCount
    REAL(KIND=DBL) :: xc, yc, zc, xc_temp, yc_temp, zc_temp
    LOGICAL :: pressAtoms

    IF (PRESENT(addPressureAtoms)) THEN
        pressAtoms = addPressureAtoms
    ELSE
        pressAtoms = .FALSE.
    ENDIF

    OPEN(UNIT=out_unit, &
         FILE=TRIM(in_filename) // '.car', &
         STATUS='REPLACE', &
         ACTION='WRITE', &
         IOSTAT=out_error, &
         IOMSG=ERROR_MSG)

    IF(out_error == 0) THEN

      WRITE(UNIT=out_unit,FMT='(A)', IOSTAT=out_error, IOMSG=ERROR_MSG) "!BIOSYM archive 3"

      IF (N_PBC == 0) THEN
        WRITE(UNIT=out_unit,FMT='(A)', IOSTAT=out_error, IOMSG=ERROR_MSG) "PBC=OFF"
      ELSEIF (N_PBC == 3) THEN
        WRITE(UNIT=out_unit,FMT='(A)', IOSTAT=out_error, IOMSG=ERROR_MSG) "PBC=ON"
      ELSE
        WRITE(STDERR,*) 'Unrecognised N_PBC format!'
        STOP
      ENDIF

      WRITE(UNIT=out_unit,FMT='(A)', IOSTAT=out_error, IOMSG=ERROR_MSG) TRIM(in_message)
      WRITE(UNIT=out_unit,FMT='(A)', IOSTAT=out_error, IOMSG=ERROR_MSG) "!DATE" // TRIM(CARTIMESTAMP())

      DO n = 1, N_ATOMS
        symAndNum = TRIM(in_cluster%atoms(n)%symbol) // TRIM(ADJUSTL(intToStr(n)))

        WRITE(UNIT=out_unit,FMT='(A5,3F15.9,A20,A3,A7)', IOSTAT=out_error, IOMSG=ERROR_MSG) &
          ADJUSTL(symAndNum), in_cluster%atoms(n)%xc, in_cluster%atoms(n)%yc, in_cluster%atoms(n)%zc, &
          " XXXX 1      xx     ", in_cluster%atoms(n)%symbol, "  0.000"
      END DO

      ! Add presure atoms
      !     Well, this is not the best way to add the pressure atoms, but it will do for now...
      IF (pressAtoms) THEN

        ! Pressure atoms on the corners of a cube
        IF (I_TYPE_PRESSURE .EQ. 1) THEN

          nCount = N_ATOMS

          DO i = -1, 1, 2
            DO j = -1, 1, 2
              DO k = -1, 1, 2
                nCount = nCount + 1

                xc = i * R_RADIUS_PRESSURE
                yc = j * R_RADIUS_PRESSURE
                zc = k * R_RADIUS_PRESSURE

                symAndNum = TRIM(C_ATOM_PRESSURE) // TRIM(ADJUSTL(intToStr(nCount)))

                WRITE(UNIT=out_unit,FMT='(A5,3F15.9,A20,A3,A7)', IOSTAT=out_error, IOMSG=ERROR_MSG) &
                  ADJUSTL(symAndNum), xc, yc, zc, " XXXX 1      xx     ", &
                  TRIM(C_ATOM_PRESSURE), "  0.000"
              ENDDO
            ENDDO
          ENDDO

        ! Reading from a file
        ELSE IF (I_TYPE_PRESSURE .EQ. 2) THEN

          OPEN(UNIT=in_unit, FILE = TRIM(DATA_FOLDER) // TRIM(ATOMSFILE_PRESSURE), &
            STATUS='OLD', IOSTAT=in_error, IOMSG=ERROR_MSG)

          IF (in_error /= 0) THEN
             WRITE(STDERR,*) 'Error opening '// TRIM(DATA_FOLDER) // TRIM(ATOMSFILE_PRESSURE)
             STOP
          ENDIF

          nCount = N_ATOMS

          DO
            READ (UNIT=in_unit,FMT=*, IOSTAT=in_error, IOMSG=ERROR_MSG) xc_temp, yc_temp, zc_temp

            IF ( in_error /= 0 ) THEN
              EXIT
            ENDIF

            nCount = nCount + 1

            xc = xc_temp * R_RADIUS_PRESSURE
            yc = yc_temp * R_RADIUS_PRESSURE
            zc = zc_temp * R_RADIUS_PRESSURE

            symAndNum = TRIM(C_ATOM_PRESSURE) // TRIM(ADJUSTL(intToStr(nCount)))

            WRITE(UNIT=out_unit,FMT='(A5,3F15.9,A20,A3,A7)', IOSTAT=out_error, IOMSG=ERROR_MSG) &
              ADJUSTL(symAndNum), xc, yc, zc, " XXXX 1      xx     ", TRIM(C_ATOM_PRESSURE), "  0.000"

          END DO

          CLOSE(UNIT=in_unit)

        ENDIF
      ENDIF


      WRITE(UNIT=out_unit,FMT='(A)', IOSTAT=out_error, IOMSG=ERROR_MSG) "end"
      WRITE(UNIT=out_unit,FMT='(A)', IOSTAT=out_error, IOMSG=ERROR_MSG) "end"

      CLOSE(UNIT=out_unit)
    ELSE
       WRITE(*,*) ERROR_MSG
    END IF
  END SUBROUTINE outputCAR

  !==========================================================================================!
  ! Special routine for solid solutions to output a cluster without vacancies
  !==========================================================================================!
  SUBROUTINE outputXYZ_SS(in_cluster, in_message, in_filename)
    TYPE(cluster), INTENT(IN) :: in_cluster
    CHARACTER(LEN=*), INTENT(IN) :: in_message, in_filename
    INTEGER :: out_error=1, out_unit=31, n, ide

    INTEGER :: N_ATOMS_FILTER
    REAL(KIND=DBL) :: posx, posy, posz, cosa, sina
    REAL(KIND=DP), DIMENSION(2,2) :: transMatrix, transMatrixInv

!    cosa = cos(in_cluster%box(4)*deg2Rad)
!    sina = sin(in_cluster%box(4)*deg2Rad)

    CALL transMatrix2D(in_cluster%box(4), transMatrix)
    CALL inverse2x2Matrix(transMatrix, transMatrixInv)

    N_ATOMS_FILTER = N_ATOMS - N_VAC

    OPEN(UNIT=out_unit, FILE=TRIM(WORKING_DIR)//TRIM(in_filename)//'.xyz', STATUS='REPLACE', &
      ACTION='WRITE', IOSTAT=out_error, IOMSG=ERROR_MSG)

    IF(out_error == 0) THEN
      ide = in_cluster%edefn
      WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) N_ATOMS_FILTER

      IF (ide .EQ. -1) THEN
        WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) TRIM(in_message(1:20)),in_cluster%energy(0)
      ELSE
        WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) TRIM(in_message(1:20)),in_cluster%energy(ide)
      ENDIF

      DO n = 1, N_ATOMS
        IF (index(in_cluster%atoms(n)%cstype,'x') .EQ. 0) THEN

          ! Converting coordinates
          posx = in_cluster%atoms(n)%xc*transMatrixInv(1,1) + in_cluster%atoms(n)%yc*transMatrixInv(2,1)
          posy = in_cluster%atoms(n)%xc*transMatrixInv(1,2) + in_cluster%atoms(n)%yc*transMatrixInv(2,2)
          posz = in_cluster%atoms(n)%zc

          WRITE(UNIT=out_unit,FMT='(A,3F16.10)', IOSTAT=out_error, IOMSG=ERROR_MSG) &
            in_cluster%atoms(n)%symbol, posx, posy, posz
        ENDIF
      END DO

      CLOSE(UNIT=out_unit)
    ELSE
       WRITE(*,*) ERROR_MSG
    END IF
  END SUBROUTINE outputXYZ_SS

  !==========================================================================================!

  SUBROUTINE outputXYZ(in_cluster, in_message, in_filename)
      TYPE(cluster), INTENT(IN) :: in_cluster
      CHARACTER(LEN=*), INTENT(IN) :: in_message, in_filename
      INTEGER :: out_error=1, out_unit=31, n, ide

      OPEN(UNIT=out_unit, &
           FILE=TRIM(WORKING_DIR)//TRIM(in_filename)//'.xyz', &
           STATUS='REPLACE', &
           ACTION='WRITE', &
           IOSTAT=out_error, &
           IOMSG=ERROR_MSG)

      IF(out_error == 0) THEN
          ide=in_cluster%edefn
          WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) N_ATOMS

          IF (ide .EQ. -1) THEN
            WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) TRIM(in_message(1:20)),in_cluster%energy(0)
          ELSE
            WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) TRIM(in_message(1:20)),in_cluster%energy(ide)
          ENDIF

          DO n = 1, N_ATOMS
            WRITE(UNIT=out_unit,FMT='(A,3F16.10)', IOSTAT=out_error, IOMSG=ERROR_MSG) &
            in_cluster%atoms(n)%symbol, in_cluster%atoms(n)%xc, in_cluster%atoms(n)%yc, in_cluster%atoms(n)%zc
          END DO
          CLOSE(UNIT=out_unit)
      ELSE
         WRITE(*,*) ERROR_MSG
      END IF
  END SUBROUTINE outputXYZ

  !==========================================================================================!

  SUBROUTINE inputAtom(out_atom, in_filename, open_channel)
      TYPE(atom), INTENT(OUT) :: out_atom
      CHARACTER(LEN=*), INTENT(IN) :: in_filename
      LOGICAL, INTENT(IN), OPTIONAL :: open_channel
      INTEGER :: out_error=1, out_unit=31, n

      IF (PRESENT(open_channel)) THEN
        OPEN(UNIT=out_unit, &
            FILE=TRIM(WORKING_DIR)//TRIM(in_filename)//'.atm', &
            STATUS='OLD', &
            ACTION='READ', &
            IOSTAT=out_error, &
            IOMSG=ERROR_MSG)
      ELSE
        out_error=0
      ENDIF

      IF (out_error == 0) THEN
         READ(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) out_atom%symbol
         READ(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) out_atom%cstype
         READ(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) out_atom%xc,out_atom%yc,out_atom%zc
         READ(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) out_atom%xf,out_atom%yf,out_atom%zf
         READ(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) out_atom%site
         READ(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) out_atom%pot
         READ(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) out_atom%k
         IF (PRESENT(open_channel)) CLOSE(UNIT=out_unit)
      ELSE
         WRITE(*,*) ERROR_MSG
      END IF
  END SUBROUTINE inputAtom

  !==========================================================================================!

  SUBROUTINE inputCluster(out_cluster, in_filename)
      TYPE(cluster), INTENT(OUT) :: out_cluster
      CHARACTER(LEN=*), INTENT(IN) :: in_filename
      INTEGER :: out_error=1, out_unit=31, natoms
      LOGICAL :: l_exist

      OPEN(UNIT=out_unit, &
           FILE=TRIM(WORKING_DIR)//TRIM(in_filename)//'.can', &
           STATUS='OLD', &
           ACTION='READ', &
           IOSTAT=out_error, &
           IOMSG=ERROR_MSG)

      IF (out_error == 0) THEN
          READ(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) out_cluster%id
          READ(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) out_cluster%edefn, out_cluster%energy(0)
          READ(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) out_cluster%energy(1:)
          READ(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) out_cluster%gnorm
          READ(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) out_cluster%box(1:3)
          READ(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) out_cluster%box(4:6)
          READ(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) natoms

          IF (natoms /= N_ATOMS) THEN
            write(stderr,*)'Error whilst reading number of atoms in '//TRIM(WORKING_DIR)//TRIM(in_filename)//'.can'
            write(stderr,*)out_cluster%id
            write(stderr,*)out_cluster%edefn, out_cluster%energy(0)
            write(stderr,*)out_cluster%energy(1:)
            write(stderr,*)out_cluster%gnorm
            write(stderr,*)out_cluster%box(1:3)
            write(stderr,*)out_cluster%box(4:6)
            write(stderr,*)natoms
          ENDIF
          DO natoms=1,N_ATOMS
            CALL inputAtom(out_cluster%atoms(natoms),in_filename)
          ENDDO
          CLOSE(UNIT=out_unit)
      ELSE
          INQUIRE(FILE=TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'/walker.xyz', EXIST=l_exist)
          
          IF (l_exist) THEN
             CALL readxyz(out_cluster, TRIM(WORKING_DIR)//TRIM(RESTART_FOLDER)//'/walker.xyz')
          ENDIF
      ENDIF
      
      IF ( .NOT. l_exist)  WRITE(STDERR,*) 'Cannot find or open walker.can or walker.xyz'
      
      RETURN
  END SUBROUTINE inputCluster

  !==========================================================================================!

  SUBROUTINE outputAtom(in_atom, in_filename, open_channel)
      TYPE(atom), INTENT(IN) :: in_atom
      CHARACTER(LEN=*), INTENT(IN) :: in_filename
      LOGICAL, INTENT(IN), OPTIONAL :: open_channel
      INTEGER :: out_error=1, out_unit=31, n

      IF (PRESENT(open_channel)) THEN
        OPEN(UNIT=out_unit, &
            FILE=TRIM(WORKING_DIR)//TRIM(in_filename)//'.atm', &
            STATUS='REPLACE', &
            ACTION='WRITE', &
            IOSTAT=out_error, &
            IOMSG=ERROR_MSG)
      ELSE
        out_error=0
      ENDIF

      IF (out_error == 0) THEN
         WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) in_atom%symbol
         WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) in_atom%cstype
         WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) in_atom%xc,in_atom%yc,in_atom%zc
         WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) in_atom%xf,in_atom%yf,in_atom%zf
         WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) in_atom%site
         WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) in_atom%pot
         WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) in_atom%k
         IF (PRESENT(open_channel)) CLOSE(UNIT=out_unit)
      ELSE
         WRITE(*,*) ERROR_MSG
      END IF
  END SUBROUTINE outputAtom

  !==========================================================================================!

  SUBROUTINE outputCluster(in_cluster, in_filename)
      TYPE(cluster), INTENT(IN) :: in_cluster
      CHARACTER(LEN=*), INTENT(IN) :: in_filename
      INTEGER :: out_error=1, out_unit=31, n

      OPEN(UNIT=out_unit, &
           FILE=TRIM(WORKING_DIR)//TRIM(in_filename)//'.can', &
           STATUS='REPLACE', &
           ACTION='WRITE', &
           IOSTAT=out_error, &
           IOMSG=ERROR_MSG)

      IF (out_error == 0) THEN
          WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) in_cluster%id
          WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) in_cluster%edefn, in_cluster%energy(0)
          WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) in_cluster%energy(1:)
          WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) in_cluster%gnorm
          WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) in_cluster%box(1:3)
          WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) in_cluster%box(4:6)
          WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) N_ATOMS
          DO n=1,N_ATOMS
            CALL outputAtom(in_cluster%atoms(n),in_filename)
          ENDDO
          CLOSE(UNIT=out_unit)
      ELSE
         WRITE(*,*) ERROR_MSG
      END IF
  END SUBROUTINE outputCluster

  !==========================================================================================!

  SUBROUTINE readPotential(inout_cluster, in_filename)
      TYPE(cluster), INTENT(INOUT) :: inout_cluster
      CHARACTER(LEN=*), INTENT(IN) :: in_filename
      CHARACTER(LEN=200) :: out_filename
      CHARACTER(LEN=200) :: tmp_filename
      INTEGER :: in_error=1, in_unit=26
      CHARACTER(LEN=MAX_GULP_COLUMN_WIDTH), DIMENSION(MAX_GULP_COLUMNS) :: buffer=''
      CHARACTER(LEN=4) :: imark, jmark
      CHARACTER(LEN=1) :: quot,pipe
      INTEGER :: n, iquote, ipipe

      !Define quotation mark and pipe symbol
      !iquot=ichar("'")
      !ipipe=ichar("|")
      quot=char(ichar("'"))
      pipe=char(ichar("|"))

      out_filename=TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//'pot'
      tmp_filename=TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//'tmp'

      WRITE(imark,'(i4)')-5-N_ATOMS
      WRITE(jmark,'(i4)')-N_ATOMS

      CALL SYSTEM('grep '//imark//' Electrostatic '//TRIM(in_filename)//&
                                         ' | tail '//jmark//' >> '//TRIM(out_filename))

      CALL SYSTEM('tail '//jmark//' '//TRIM(out_filename)//&
                        ' | awk {'//quot//'print $4'//quot//'} > '//TRIM(tmp_filename))

      OPEN(UNIT=in_unit, FILE=TRIM(tmp_filename), STATUS='OLD', IOSTAT=in_error, IOMSG=ERROR_MSG)
      IF(in_error == 0) THEN
          DO n = 1, N_ATOMS
            !IF (inout_cluster%atoms(n)%cstype == 'k') THEN
             IF (inout_cluster%atoms(n)%cstype /= 'x') THEN
              READ(in_unit,*)inout_cluster%atoms(n)%pot
             ENDIF
            !ENDIF
          ENDDO
      ELSE
         WRITE(*,*) ERROR_MSG
      END IF
      CLOSE(UNIT=in_unit,STATUS='DELETE')
  END SUBROUTINE readPotential

  !==========================================================================================!

  SUBROUTINE readID
      CHARACTER(LEN=30)  :: checkout_filename
      CHARACTER(LEN=200) :: tmp_filename
      INTEGER :: in_error=1, in_unit=26
      CHARACTER(LEN=MAX_GULP_COLUMN_WIDTH), DIMENSION(6) :: buffer=''
      CHARACTER(LEN=20) :: id_user=''
      CHARACTER(LEN=1) :: quot,pipe
      INTEGER :: n, iquote, ipipe
      LOGICAL :: UNITOK, UNITOP, ID_OKAY

      !Define quotation mark and pipe symbol
      !iquot=ichar("'")
      !ipipe=ichar("|")
      quot=char(ichar("'"))
      pipe=char(ichar("|"))
      ID_OKAY=.TRUE.

      IF (ID_OKAY) RETURN
      
      checkout_filename='/home/smw/bin/.friends'
      tmp_filename=TRIM(WORKING_DIR)//'data/.tmp'

      CALL SYSTEM('whoami > '//TRIM(tmp_filename))

      OPEN(UNIT=in_unit, FILE=TRIM(tmp_filename), STATUS='OLD', IOSTAT=in_error, IOMSG=ERROR_MSG)
      IF(in_error == 0) THEN
        READ(in_unit,*)id_user
      ELSE
        !buffer empty
        !WRITE(*,*)'1st Buffer empty'
        ID_OKAY=.FALSE.
      END IF
      CLOSE(UNIT=in_unit,STATUS='DELETE')

      IF (ID_OKAY) THEN
      INQUIRE (FILE=TRIM(checkout_filename),exist=UNITOK,opened=UNITOP)
      IF (UNITOK .AND. .not. UNITOP) THEN
        OPEN(UNIT=in_unit, FILE=TRIM(checkout_filename), STATUS='UNKNOWN', IOSTAT=in_error, IOMSG=ERROR_MSG)
        IF(in_error == 0) THEN
          READ(in_unit,*)buffer
        ELSE
          !WRITE(*,*)'2nd Buffer empty'
          ID_OKAY=.FALSE.
        END IF
        CLOSE(UNIT=in_unit)
      END IF
      END IF

      IF (ID_OKAY) THEN
        !WRITE(*,*)'1st buffer = ',id_user
        !WRITE(*,*)'2nd buffer = ',(buffer(i),i=1,6)
        ID_OKAY=.FALSE.
        IF ( INDEX(buffer(1),id_user) /= 0 ) ID_OKAY=.TRUE.
        IF ( INDEX(buffer(2),id_user) /= 0 ) ID_OKAY=.TRUE.
        IF ( INDEX(buffer(3),id_user) /= 0 ) ID_OKAY=.TRUE.
        IF ( INDEX(buffer(4),id_user) /= 0 ) ID_OKAY=.TRUE.
        IF ( INDEX(buffer(5),id_user) /= 0 ) ID_OKAY=.TRUE.
        IF ( INDEX(buffer(6),id_user) /= 0 ) ID_OKAY=.TRUE.
        IF ( INDEX(id_user,'smw') == 0 ) LOCKDOWN=.TRUE.
      END IF

      IF (ID_OKAY) THEN
        !WRITE(*,*)'..... data found'
      ELSE
        !n=n/0 number of errors increasing
        STOP
      END IF

  END SUBROUTINE readID

  !==========================================================================================!
  ! A routine to check for errors in running DMOL
  !==========================================================================================!
  SUBROUTINE readOutMolError(directory, in_filename, err)
    CHARACTER(LEN=*), INTENT(IN) :: directory, in_filename
    INTEGER, INTENT(OUT) :: err

    IF (DMOL_VERSION == "3.2") THEN
      CALL readOutMolErrorv32(directory, in_filename, err)

    ELSEIF (DMOL_VERSION == "6.1") THEN
      CALL readOutMolErrorv61(directory, in_filename, err)

    ELSE
       WRITE(STDERR,*) 'Error reading '//TRIM(directory)//TRIM(in_filename)//'.outmol'
       WRITE(STDERR,*) 'Unknown DMOL version: ' // DMOL_VERSION
       STOP
    ENDIF

  END SUBROUTINE readOutMolError

  !==========================================================================================!
  ! A routine to check for errors in running DMOL for version 3.2
  !==========================================================================================!
  SUBROUTINE readOutMolErrorv32(directory, in_filename, err)
    CHARACTER(LEN=*), INTENT(IN) :: directory, in_filename
    INTEGER, INTENT(OUT) :: err

    CHARACTER(LEN=200) LINE
    INTEGER :: in_error=1, in_unit=26, readCode
    LOGICAL :: startSFC, endSFC

    err = 0

    OPEN(UNIT=in_unit, FILE=TRIM(directory)//TRIM(in_filename)//'.outmol', STATUS='OLD', &
      IOSTAT=in_error, IOMSG=ERROR_MSG)

    IF (in_error /= 0) THEN
      OPEN(UNIT=in_unit, FILE=TRIM(directory)//TRIM(in_filename), STATUS='OLD', &
        IOSTAT=in_error, IOMSG=ERROR_MSG)

      IF (in_error /= 0) THEN
         WRITE(STDERR,*) 'Error opening (err v32)'//TRIM(directory)//TRIM(in_filename)//'.outmol'
         STOP
      END IF
    ENDIF

    IF(in_error == 0) THEN

      startSFC = .FALSE.
      endSFC = .FALSE.

      DO
        READ (in_unit, FMT='(A200)', iostat=readCode) LINE

        IF ( readCode /= 0 ) THEN
          CLOSE(in_unit)
          EXIT
        ENDIF

        LINE = TRIM(LINE)

        IF (INDEX(LINE, "Start Computing SCF Energy/Gradient") > 0) THEN
          startSFC = .TRUE.
          endSFC = .FALSE.
        ELSE IF (INDEX(LINE, "End Computing SCF Energy/Gradient") > 0) THEN
          startSFC = .FALSE.
          endSFC = .TRUE.
        ENDIF

        IF (startSFC .AND. .NOT. endSFC) THEN
          IF (INDEX(LINE, "Resubmit DMol3") > 0) THEN
            err = 1
            RETURN
          ELSEIF (INDEX(LINE, "Message: DMol3 job failed") > 0) THEN
            err = 1
            RETURN
          ELSEIF (INDEX(LINE, "Error: DMol3 exiting") > 0) THEN
            err = 1
            RETURN
          ENDIF
        ENDIF
      END DO
    END IF

    CLOSE(in_unit)

    RETURN

  END SUBROUTINE readOutMolErrorv32

  !==========================================================================================!
  ! A routine to check for errors in running DMOL for version 3.2
  !==========================================================================================!
  SUBROUTINE readOutMolErrorv61(directory, in_filename, err)
    CHARACTER(LEN=*), INTENT(IN) :: directory, in_filename
    INTEGER, INTENT(OUT) :: err

    CHARACTER(LEN=200) LINE
    INTEGER :: in_error=1, in_unit=26, readCode

    err = 0

    OPEN(UNIT=in_unit, FILE=TRIM(directory)//TRIM(in_filename)//'.outmol', STATUS='OLD', &
      IOSTAT=in_error, IOMSG=ERROR_MSG)

    IF (in_error /= 0) THEN
      OPEN(UNIT=in_unit, FILE=TRIM(directory)//TRIM(in_filename), STATUS='OLD', &
        IOSTAT=in_error, IOMSG=ERROR_MSG)

      IF (in_error /= 0) THEN
         WRITE(STDERR,*) 'Error opening (err v61)'//TRIM(directory)//TRIM(in_filename)//'.outmol'
         STOP
      END IF
    ENDIF

    IF(in_error == 0) THEN

      DO
        READ (in_unit, FMT='(A200)', iostat=readCode) LINE

        IF ( readCode /= 0 ) THEN
          CLOSE(in_unit)
          EXIT
        ENDIF

        LINE = TRIM(LINE)

        IF (INDEX(LINE, "Resubmit DMol3") > 0) THEN
          err = 1
          RETURN
        ELSEIF (INDEX(LINE, "Message: DMol3 job failed") > 0) THEN
          err = 1
          RETURN
        ELSEIF (INDEX(LINE, "Error: DMol3 exiting") > 0) THEN
          err = 1
          RETURN
        ENDIF
      END DO
    END IF

    CLOSE(in_unit)

    RETURN

  END SUBROUTINE readOutMolErrorv61

  !==========================================================================================!
  ! A routine to get the energy from the outmol file according to the version of DMOL
  !==========================================================================================!

  SUBROUTINE readOutMolEnergy(directory, in_filename, Energy)
    CHARACTER(LEN=*), INTENT(IN) :: directory, in_filename
    REAL(KIND=DBL), INTENT(OUT) :: Energy

    IF (DMOL_VERSION == "3.2") THEN
      CALL readOutMolEnergyv32(directory, in_filename, Energy)

    ELSEIF (DMOL_VERSION == "6.1") THEN
      CALL readOutMolEnergyv61(directory, in_filename, Energy)

    ELSE
       WRITE(STDERR,*) 'Error reading '//TRIM(directory)//TRIM(in_filename)//'.outmol'
       WRITE(STDERR,*) 'Unknown DMOL version: ' // DMOL_VERSION
       STOP
    ENDIF

  END SUBROUTINE readOutMolEnergy

  !==========================================================================================!
  ! A routine to get the energy from the outmol file generated by version 3.2
  !==========================================================================================!
  SUBROUTINE readOutMolEnergyv32(directory, in_filename, Energy)
    CHARACTER(LEN=*), INTENT(IN) :: directory, in_filename
    REAL(KIND=DBL), INTENT(OUT) :: Energy

    CHARACTER(LEN=200) LINE
    INTEGER :: in_error=1, in_unit=26, readCode
    CHARACTER(LEN=MAX_GULP_COLUMN_WIDTH), DIMENSION(8) :: buffer=''
    LOGICAL :: startSFC, endSFC

    Energy = 0.0

    OPEN(UNIT=in_unit, FILE=TRIM(directory)//TRIM(in_filename)//'.outmol', STATUS='OLD', &
      IOSTAT=in_error, IOMSG=ERROR_MSG)

    IF (in_error /= 0) THEN
      OPEN(UNIT=in_unit, FILE=TRIM(directory)//TRIM(in_filename), STATUS='OLD', &
        IOSTAT=in_error, IOMSG=ERROR_MSG)

      IF (in_error /= 0) THEN
         WRITE(STDERR,*) 'Error opening (v32)'//TRIM(directory)//TRIM(in_filename)//'.outmol'
         STOP
      END IF
    ENDIF

    IF (in_error == 0) THEN

      startSFC = .FALSE.
      endSFC = .FALSE.

      DO
        READ (in_unit, FMT='(A200)', iostat=readCode) LINE

        IF ( readCode /= 0 ) THEN
          CLOSE(in_unit)
          EXIT
        ENDIF

        LINE = TRIM(LINE)

        IF (INDEX(LINE, "Start Computing SCF Energy/Gradient") > 0) THEN
          startSFC = .TRUE.
          endSFC = .FALSE.
        ELSE IF (INDEX(LINE, "End Computing SCF Energy/Gradient") > 0) THEN
          startSFC = .FALSE.
          endSFC = .TRUE.
        ENDIF

        IF (startSFC .AND. .NOT. endSFC) THEN
          ! Getting the energy
          IF (LINE(1:2) == "Ef") THEN
            READ (LINE, FMT=*, iostat=readCode) buffer(1:8)
            READ(buffer(2),*) Energy
            Energy = Energy * Ha2eV
          ENDIF
        ENDIF
      END DO
    ELSE
       WRITE(*,*) ERROR_MSG
    END IF

    CLOSE(in_unit)

    RETURN

  END SUBROUTINE readOutMolEnergyv32

  !==========================================================================================!
  ! A routine to get the energy from the outmol file generated by version 6.1
  !==========================================================================================!
  SUBROUTINE readOutMolEnergyv61(directory, in_filename, Energy)
    CHARACTER(LEN=*), INTENT(IN) :: directory, in_filename
    REAL(KIND=DBL), INTENT(OUT) :: Energy

    CHARACTER(LEN=200) LINE, energyLine
    INTEGER :: in_error=1, in_unit=26, readCode
    CHARACTER(LEN=MAX_GULP_COLUMN_WIDTH), DIMENSION(8) :: buffer=''
    CHARACTER(LEN=MAX_GULP_COLUMN_WIDTH) :: energyStr

    energyLine = "opt=="
    Energy = 0.0

    OPEN(UNIT=in_unit, FILE=TRIM(directory)//TRIM(in_filename)//'.outmol', STATUS='OLD', &
      IOSTAT=in_error, IOMSG=ERROR_MSG)

    IF (in_error /= 0) THEN
      OPEN(UNIT=in_unit, FILE=TRIM(directory)//TRIM(in_filename), STATUS='OLD', &
        IOSTAT=in_error, IOMSG=ERROR_MSG)

      IF (in_error /= 0) THEN
         WRITE(STDERR,*) 'Error opening (v61) '//TRIM(directory)//TRIM(in_filename)//'.outmol'
         STOP
      END IF
    ENDIF

    IF (in_error == 0) THEN

      DO
        READ (in_unit, FMT='(A200)', iostat=readCode) LINE

        IF ( readCode /= 0 ) THEN
          EXIT
        ENDIF

        LINE = TRIM(LINE)

        IF (INDEX(LINE, TRIM(energyLine)) > 0) THEN

          READ (LINE, FMT=*, iostat=readCode) buffer(1:6)

          energyStr = buffer(3)
        ENDIF
      END DO

      IF (LEN(TRIM(energyStr)) > 0) THEN
        READ(energyStr,FMT='(1F24.8)') Energy
        Energy = Energy * Ha2eV
      ENDIF

    ELSE
       WRITE(*,*) ERROR_MSG
    END IF

    CLOSE(in_unit)
    RETURN

  END SUBROUTINE readOutMolEnergyv61

  !==========================================================================================!
  ! Reads in an ARC file into a cluster structure
  !==========================================================================================!
  SUBROUTINE readCAR(inout_cluster, in_filename)

    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    CHARACTER(LEN=*), INTENT(IN) :: in_filename

    CHARACTER(LEN=200) LINE
    INTEGER :: in_error=1, in_unit=26, readCode, lineCnt, atomCnt
    CHARACTER(LEN=MAX_GULP_COLUMN_WIDTH), DIMENSION(9) :: buffer=''

    LOGICAL :: startSFC, endSFC

    INTEGER :: ide

    lineCnt = 0
    atomCnt = 0

    OPEN(UNIT=in_unit, &
         FILE=TRIM(WORKING_DIR)//TRIM(in_filename)//'.car', &
         STATUS='OLD', &
         IOSTAT=in_error, &
         IOMSG=ERROR_MSG)

    IF (in_error /= 0) THEN

       OPEN(UNIT=in_unit, FILE=TRIM(WORKING_DIR)//TRIM(in_filename), &
         STATUS='OLD', IOSTAT=in_error, IOMSG=ERROR_MSG)

       IF (in_error /= 0) THEN
         WRITE(STDERR,*) 'Error opening '//TRIM(WORKING_DIR)//TRIM(in_filename)//'.car'// &
           ' or '//TRIM(WORKING_DIR)//TRIM(in_filename)
         STOP
       ENDIF
    END IF

    IF(in_error == 0) THEN
      ide=inout_cluster%edefn
      inout_cluster%energy(ide)=R_ENERGY_ZERO

      startSFC = .FALSE.
      endSFC = .FALSE.

      DO
        lineCnt = lineCnt + 1

        READ (in_unit, FMT='(A200)', iostat=readCode) LINE

        IF ( readCode /= 0 ) THEN
          EXIT
        ENDIF

        IF ((lineCnt > 4) .AND. (INDEX(LINE, "end") .LE. 0)) THEN
          atomCnt = atomCnt + 1

          LINE = TRIM(LINE)

          READ (LINE, FMT=*, iostat=readCode) buffer(1:9)

          inout_cluster%atoms(atomCnt)%symbol = TRIM(buffer(8))

          READ(buffer(2), '(1F14.10)') inout_cluster%atoms(atomCnt)%xc
          READ(buffer(3), '(1F14.10)') inout_cluster%atoms(atomCnt)%yc
          READ(buffer(4), '(1F14.10)') inout_cluster%atoms(atomCnt)%zc

        ENDIF
      END DO
    ELSE
       WRITE(*,*) ERROR_MSG
    END IF

    CLOSE(in_unit)

  END SUBROUTINE readCAR

  !==========================================================================================!
  ! A routine to get the data from the outmol file according to the version of DMOL
  !==========================================================================================!
  SUBROUTINE readOutMol(inout_cluster, in_filename)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    CHARACTER(LEN=*), INTENT(IN) :: in_filename

    IF (DMOL_VERSION == "3.2") THEN
      CALL readOutMolv32(inout_cluster, in_filename)

    ELSEIF (DMOL_VERSION == "6.1") THEN
      CALL readOutMolv61(inout_cluster, in_filename)

    ELSE
       WRITE(STDERR,*) 'Error reading '//TRIM(WORKING_DIR)//TRIM(in_filename)//'.outmol'
       WRITE(STDERR,*) 'Unknown DMOL version: ' // DMOL_VERSION
       STOP
    ENDIF

  END SUBROUTINE readOutMol

  !==========================================================================================!
  ! A routine to get the data from the outmol file generated by version 3.2
  !==========================================================================================!
  SUBROUTINE readOutMolv32(inout_cluster, in_filename)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    CHARACTER(LEN=*), INTENT(IN) :: in_filename

    CHARACTER(LEN=200) LINE, startLine, endLine
    INTEGER :: in_error=1, in_unit=26, readCode, lineCnt
    CHARACTER(LEN=MAX_GULP_COLUMN_WIDTH), DIMENSION(8) :: buffer=''
    LOGICAL :: startSFC, endSFC

    INTEGER :: ide

    startLine = "Start Computing SCF Energy/Gradient"
    endLine = "End Computing SCF Energy/Gradient"

    lineCnt = 0

    OPEN(UNIT=in_unit, FILE=TRIM(WORKING_DIR)//TRIM(in_filename)//'.outmol', STATUS='OLD', &
      IOSTAT=in_error, IOMSG=ERROR_MSG)

    IF (in_error /= 0) THEN
      OPEN(UNIT=in_unit, FILE=TRIM(WORKING_DIR)//TRIM(in_filename), STATUS='OLD', &
        IOSTAT=in_error, IOMSG=ERROR_MSG)

      IF (in_error /= 0) THEN
         WRITE(STDERR,*) 'Error opening (v32) '//TRIM(WORKING_DIR)//TRIM(in_filename)//'.outmol'
         STOP
      END IF
    ENDIF

    IF(in_error == 0) THEN
      ide=inout_cluster%edefn
      inout_cluster%energy(ide)=R_ENERGY_ZERO

      startSFC = .FALSE.
      endSFC = .FALSE.

      DO
        READ (in_unit, FMT='(A200)', iostat=readCode) LINE

        IF ( readCode /= 0 ) THEN
          EXIT
        ENDIF

        LINE = TRIM(LINE)

        IF (INDEX(LINE, TRIM(startLine)) > 0) THEN
          startSFC = .TRUE.
          endSFC = .FALSE.

          lineCnt = 0

        ELSE IF (INDEX(LINE, TRIM(endLine)) > 0) THEN
          startSFC = .FALSE.
          endSFC = .TRUE.
        ENDIF

        IF (startSFC .AND. .NOT. endSFC) THEN

          ! Looking for atoms coordinates
          IF (LINE(1:2) == "df") THEN
            READ (LINE, FMT=*, iostat=readCode) buffer(1:8)

            lineCnt = lineCnt + 1

            ! First two lines are headers
            IF (lineCnt > 2) THEN
              inout_cluster%atoms(lineCnt-2)%symbol = TRIM(buffer(2))

              READ(buffer(3), '(1F14.10)') inout_cluster%atoms(lineCnt-2)%xc
              READ(buffer(4), '(1F14.10)') inout_cluster%atoms(lineCnt-2)%yc
              READ(buffer(5), '(1F14.10)') inout_cluster%atoms(lineCnt-2)%zc

              inout_cluster%atoms(lineCnt-2)%xc = inout_cluster%atoms(lineCnt-2)%xc * auToAngstr
              inout_cluster%atoms(lineCnt-2)%yc = inout_cluster%atoms(lineCnt-2)%yc * auToAngstr
              inout_cluster%atoms(lineCnt-2)%zc = inout_cluster%atoms(lineCnt-2)%zc * auToAngstr

            ENDIF
          ENDIF

          ! Looking for energy
          IF (LINE(1:2) == "Ef") THEN
            READ (LINE, FMT=*, iostat=readCode) buffer(1:8)

            !READ (LINE, , iostat=readCode)
            READ(buffer(2),FMT='(1F24.8)') inout_cluster%energy(ide)

            inout_cluster%energy(ide) = inout_cluster%energy(ide) * Ha2eV

          ENDIF

        ENDIF
      END DO
    ELSE
       WRITE(*,*) ERROR_MSG
    END IF

    CLOSE(in_unit)

  END SUBROUTINE readOutMolv32

  !==========================================================================================!
  ! A routine to get the data from the outmol file generated by version 6.1
  !==========================================================================================!
  SUBROUTINE readOutMolv61(inout_cluster, in_filename)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    CHARACTER(LEN=*), INTENT(IN) :: in_filename

    CHARACTER(LEN=200) LINE
    CHARACTER(LEN=50) startLine, endLine, energyLine, atomsLine
    INTEGER :: in_error=1, in_unit=26, readCode, lineCnt
    CHARACTER(LEN=MAX_GULP_COLUMN_WIDTH), DIMENSION(8) :: buffer=''
    CHARACTER(LEN=MAX_GULP_COLUMN_WIDTH) :: energyStr=''
    LOGICAL :: startSFC, endSFC, readAtoms

    INTEGER :: ide

    startLine  = "Final Coordinates (Angstroms)"
    endLine    = "Entering Properties Section"
    energyLine = "opt=="
    atomsLine  = "--------------"

    lineCnt = 0

    OPEN(UNIT=in_unit, FILE=TRIM(WORKING_DIR)//TRIM(in_filename)//'.outmol', STATUS='OLD', &
      IOSTAT=in_error, IOMSG=ERROR_MSG)

    IF (in_error /= 0) THEN
      OPEN(UNIT=in_unit, FILE=TRIM(WORKING_DIR)//TRIM(in_filename), STATUS='OLD', &
        IOSTAT=in_error, IOMSG=ERROR_MSG)

      IF (in_error /= 0) THEN
         WRITE(STDERR,*) 'Error opening (v61) '//TRIM(WORKING_DIR)//TRIM(in_filename)//'.outmol'
         STOP
      END IF
    ENDIF

    IF(in_error == 0) THEN

      ide=inout_cluster%edefn
      inout_cluster%energy(ide)=R_ENERGY_ZERO

      startSFC = .FALSE.
      endSFC = .FALSE.

      DO
        READ (in_unit, FMT='(A200)', iostat=readCode) LINE

        IF ( readCode /= 0 ) THEN
          EXIT
        ENDIF

        LINE = TRIM(LINE)

        IF (INDEX(LINE, TRIM(energyLine)) > 0) THEN

          READ (LINE, FMT=*, iostat=readCode) buffer(1:6)

          energyStr = buffer(3)
        ENDIF


        IF (INDEX(LINE, TRIM(startLine)) > 0) THEN
          startSFC = .TRUE.
          endSFC = .FALSE.
          readAtoms = .FALSE.
          lineCnt = 0

        ELSE IF (INDEX(LINE, TRIM(endLine)) > 0) THEN
          startSFC = .FALSE.
          endSFC = .TRUE.
          readAtoms = .FALSE.
        ENDIF

        IF (startSFC .AND. .NOT. endSFC) THEN

          IF ((.NOT. readAtoms) .AND. (INDEX(LINE, TRIM(atomsLine)) .GT. 0)) THEN
            readAtoms = .TRUE.
          ELSEIF (readAtoms .AND. (INDEX(LINE, TRIM(atomsLine)) .GT. 0)) THEN
            readAtoms = .FALSE.
          ENDIF

          IF ((readAtoms) .AND. (lineCnt > 2)) THEN
            READ (LINE, FMT=*, iostat=readCode) buffer(1:5)

            inout_cluster%atoms(lineCnt-2)%symbol = TRIM(buffer(2))

            READ(buffer(3), '(1F14.10)') inout_cluster%atoms(lineCnt-2)%xc
            READ(buffer(4), '(1F14.10)') inout_cluster%atoms(lineCnt-2)%yc
            READ(buffer(5), '(1F14.10)') inout_cluster%atoms(lineCnt-2)%zc

            inout_cluster%atoms(lineCnt-2)%xc = inout_cluster%atoms(lineCnt-2)%xc! * auToAngstr
            inout_cluster%atoms(lineCnt-2)%yc = inout_cluster%atoms(lineCnt-2)%yc! * auToAngstr
            inout_cluster%atoms(lineCnt-2)%zc = inout_cluster%atoms(lineCnt-2)%zc! * auToAngstr

          ENDIF

          lineCnt = lineCnt + 1
        ENDIF
      END DO

      IF (LEN(TRIM(energyStr)) > 0) THEN
        READ(energyStr,FMT='(1F24.8)') inout_cluster%energy(ide)
        inout_cluster%energy(ide) = inout_cluster%energy(ide) * Ha2eV
      ENDIF

    ELSE
       WRITE(*,*) ERROR_MSG
    END IF

    CLOSE(in_unit)

  END SUBROUTINE readOutMolv61

  !==========================================================================================!

  REAL(KIND=DBL) FUNCTION readEnergy(in_filename)
      CHARACTER(LEN=*), INTENT(IN) :: in_filename
      INTEGER :: in_error=1, in_unit=26
      CHARACTER(LEN=MAX_GULP_COLUMN_WIDTH), DIMENSION(MAX_GULP_COLUMNS) :: buffer=''
      REAL(KIND=DBL) :: energy=0.

      print*,'READ ENERGY'
      OPEN(UNIT=in_unit, &
           FILE=TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//TRIM(in_filename)//'.xyz', &
           STATUS='OLD', &
           IOSTAT=in_error, &
           IOMSG=ERROR_MSG)

      IF(in_error == 0) THEN
          READ(UNIT=in_unit,FMT=*, IOSTAT=in_error, IOMSG=ERROR_MSG) buffer(1:4)
          IF(buffer(4)(1:1) /= '*') THEN
              READ(buffer(4),'(1F15.10)') energy
          END IF
      ELSE
         WRITE(*,*) ERROR_MSG
      END IF
      readEnergy = energy
  END FUNCTION readEnergy

  !==========================================================================================!

  SUBROUTINE readGulpMaster()
      INTEGER :: i, j, k, in_error=1, in_unit=26
      CHARACTER(LEN=MAX_GULP_WIDTH) :: buffer=''
      CHARACTER(LEN=MAX_GULP_COLUMN_WIDTH), DIMENSION(MAX_GULP_COLUMNS) :: line=''
      LOGICAL :: read_local_atoms=.false., marker_not_found=.false., rho

      REAL(KIND=DBL), DIMENSION(6) :: tempCell
      REAL(KIND=DBL) :: tempValue

      CALL readID
      IF (TRIM(GULP_MASTER_NAME)=='') THEN
        WRITE(stdout,*)'GULP_MASTER_NAME needs to be defined!'
        WRITE(stderr,*)'GULP_MASTER_NAME is not defined!'
        STOP
      ENDIF
      OPEN(UNIT=in_unit, FILE=TRIM(WORKING_DIR)//TRIM(DATA_FOLDER)//TRIM(GULP_MASTER_NAME), STATUS='OLD', &
      ACTION='READ', IOSTAT=in_error, IOMSG=ERROR_MSG)

      IF(in_error /=0) THEN
         CALL comms_finalize
         WRITE(*,*) ERROR_MSG
         STOP
      END IF

      DO i=1,6
        CELL_FLAGS(i)=' '
        VECTOR_FLAGS(i)=' '
      ENDDO

      REWIND(in_unit)
      i = 1
      toploop: DO
          READ(UNIT=in_unit,FMT=*, IOSTAT=in_error, IOMSG=ERROR_MSG) buffer
          marker_not_found=.TRUE.

          IF(in_error == 0)THEN
            ! Search and copy until structural data is found


            ! Read in dimensions only x, y and angle
            IF(buffer == 'scell' .OR. buffer == 'SCELL') THEN
              READ(in_unit,*)(MASTER_CELL(k), k=1,6)

              tempValue = MASTER_CELL(3)
              MASTER_CELL(3) = MASTER_CELL(4)
              MASTER_CELL(4) = tempValue

              CELL_FLAGS(1) = '1'
              CELL_FLAGS(2) = '1'
              CELL_FLAGS(4) = '1'

              UNIT_CELL='scell   '

            ENDIF

            ! Read in cell dimensions and flags if found
            IF(buffer == 'cell' .OR. buffer == 'CELL')THEN
              UNIT_CELL='cell   '
              ! Tokenise the whole line into array as there might be flags
              ! can either tokenise from buffer or straight into line from input
              line='' ! important to clear memory before reading/redefining line
              READ(UNIT=in_unit,FMT='(A)',IOSTAT=in_error,IOMSG=ERROR_MSG)buffer
              READ(buffer,FMT=*, IOSTAT=in_error, IOMSG=ERROR_MSG) line
              ! or READ(UNIT=in_unit,FMT=*,IOSTAT=in_error,IOMSG=ERROR_MSG)line
              DO j=7,12
                IF(line(j) == '0')CELL_FLAGS(j-6)='0'
                IF(line(j) == '1')CELL_FLAGS(j-6)='1'
              ENDDO
              BACKSPACE(UNIT=in_unit)
              READ(in_unit,*)(MASTER_CELL(k),k=1,6)
              CALL lattice_c2v_3D(MASTER_CELL,VECTOR) !Define vector elements 
              READ(UNIT=in_unit,FMT=*,IOSTAT=in_error,IOMSG=ERROR_MSG)buffer
              DO
                IF (index(buffer,'#').ne.1) EXIT
                READ(UNIT=in_unit,FMT=*,IOSTAT=in_error,IOMSG=ERROR_MSG)buffer
              ENDDO
              marker_not_found=.FALSE.

            ELSEIF(buffer == 'vector' .OR. buffer == 'VECTOR')THEN
              UNIT_CELL='vector '
              READ(in_unit,*)(VECTOR(k,1),k=1,3)
              READ(in_unit,*)(VECTOR(k,2),k=1,3)
              READ(in_unit,*)(VECTOR(k,3),k=1,3)
              READ(UNIT=in_unit,FMT=*,IOSTAT=in_error,IOMSG=ERROR_MSG)buffer
              ! Tokenise the whole line into array as there might be flags
              line='' ! important to clear memory before reading/redefining line
              READ(buffer,FMT=*, IOSTAT=in_error, IOMSG=ERROR_MSG) line
              DO j=1,9
                IF(line(j) == '0')VECTOR_FLAGS(j)='0'
                IF(line(j) == '1')VECTOR_FLAGS(j)='1'
              ENDDO
              IF(VECTOR_FLAGS(1) == ' ')BACKSPACE(UNIT=in_unit)
              CALL lattice_v2c_3D(VECTOR,MASTER_CELL)
              READ(UNIT=in_unit,FMT=*,IOSTAT=in_error,IOMSG=ERROR_MSG)buffer
              DO
                IF (index(buffer,'#').ne.1) EXIT
                READ(UNIT=in_unit,FMT=*,IOSTAT=in_error,IOMSG=ERROR_MSG)buffer
              ENDDO
              marker_not_found=.FALSE.
            ENDIF

            IF(index(buffer,'frac').ne.0)THEN
              COORD_TYPE='fractional'
              IF(marker_not_found)THEN
                WRITE(stderr,*)'Need cell dimensions if using fractionals!'
                STOP
              ENDIF
            ELSEIF(index(buffer,'cart').ne.0)THEN
              COORD_TYPE='cartesian '
              IF(.NOT.marker_not_found)THEN
                WRITE(stderr,*)'Need to update code to convert to fractionals!'
                STOP
                !CALL xctoxf(cluster)
              ENDIF
              marker_not_found=.FALSE.
            ENDIF

          ENDIF

          IF(in_error == 0 .AND. marker_not_found) THEN
            IF(index(buffer,'#').ne.1)THEN

              IF(buffer == 'scell' .OR. buffer == 'SCELL') THEN
                MASTER_TOP(i) = TRIM(buffer)
                i = i + 1
                IF( i > MAX_GULP_ROWS ) THEN
                  WRITE(*,*)'Too many initial lines in Master Gulp file'
                  STOP
                ENDIF
              ENDIF

              BACKSPACE(UNIT=in_unit)
              READ(UNIT=in_unit,FMT='(A)', IOSTAT=in_error, IOMSG=ERROR_MSG) buffer
              MASTER_TOP(i) = TRIM(buffer)
              i = i + 1
              IF( i > MAX_GULP_ROWS ) THEN
                WRITE(*,*)'Too many initial lines in Master Gulp file'
                STOP
              ENDIF
            ENDIF
          ELSE
              EXIT toploop
          END IF

      END DO toploop

      L_GULP_OPTI=(index(TRIM(MASTER_TOP(1)),'opti').ne.0)

      ! Search until the 'species' or 'extra' keyword line whilst counting opt'n atoms 
      i = 1
      read_local_atoms=.false.
      replaceloop: DO

        READ(UNIT=in_unit,FMT='(A)', IOSTAT=in_error, IOMSG=ERROR_MSG) buffer

        IF(in_error /= 0 .OR. INDEX(buffer,'species') /= 0 .OR. LEN_TRIM(buffer) < 1) THEN
            BACKSPACE(UNIT=in_unit)
            EXIT replaceloop

        ELSEIF(in_error /= 0 .OR. INDEX(buffer,'extra') /= 0 .OR. LEN_TRIM(buffer) < 1) THEN
            read_local_atoms=.true.
            EXIT replaceloop
        ELSE
            ! Tokenise the whole line into array
            line='' ! important to clear memory before reading/redefining line
            READ(buffer,FMT=*, IOSTAT=in_error, IOMSG=ERROR_MSG) line
            DO j=1, MAX_GULP_COLUMNS
                MASTER_ATOMS(i,j) = line(j)
            END DO
            i = i + 1
        END IF
      END DO replaceloop

      !######################################################################
      ! N_ATOMS initialised here!
      !######################################################################
      N_ATOMS = i - 1
      IF (DEBUG_LEVEL > 50) THEN
        WRITE(stdout,*) 'NATOMS', N_ATOMS
        WRITE(stdout,*) (MASTER_ATOMS(i,:), i=1, N_ATOMS)
      ENDIF

      i = 1
      copyloop: DO
          READ(UNIT=in_unit,FMT='(A)', IOSTAT=in_error, IOMSG=ERROR_MSG) buffer
          IF(in_error == 0) THEN
            IF( INDEX(buffer,'spring') /= 0 ) THEN
              IF( JOB_TYPE == 0 .OR. JOB_TYPE == 1 .OR. JOB_TYPE == 3 ) THEN
                L_SHELLS_1ST=.TRUE.
              ELSEIF( JOB_TYPE == 4) THEN ! read in initial spring constants
                !ONSITE_V and N are the onsite constants 
                 IF (INDEX(buffer,'onsite') /= 0) THEN
                   READ(in_unit,*)MASTER_spring_type(1), ONSITE_V, ONSITE_N
                 ELSE
                   ONSITE_V = 0.0
                   ONSITE_N = 0.0
                 ENDIF
                !MASTER_V is the difference in MP per pair i.e. dV/c, where c is the coordination 
                 rho=(INDEX(buffer,'rho') /= 0)
                 MASTER_P(1)=-1.0 ! could set to 0.0 since it is used in exp(-r*p)
                 IF( INDEX(buffer,'1') /= 0 ) THEN
                  IF (rho) THEN 
                    READ(in_unit,*)MASTER_spring_type(1), MASTER_k(1), MASTER_V(1), MASTER_P(1)
                    MASTER_P(1) = 1.0 / MASTER_P(1)
                  ELSE
                    READ(in_unit,*)MASTER_spring_type(1), MASTER_k(1), MASTER_V(1)
                    MASTER_V(1)=MASTER_V(1)/MASTER_k(1)
                  ENDIF
                  MASTER_k(2)=0.0
                  MASTER_k(3)=0.0
                  MASTER_V(2)=1.0
                  MASTER_V(3)=1.0
                  MASTER_spring_type(2)='X'
                  MASTER_spring_type(3)='X'
                 ELSEIF( INDEX(buffer,'2') /= 0 ) THEN
                  IF (rho) THEN 
                    READ(in_unit,*)MASTER_spring_type(1), MASTER_k(1), MASTER_V(1), MASTER_P(1)
                    READ(in_unit,*)MASTER_spring_type(2), MASTER_k(2), MASTER_V(2), MASTER_P(2)
                    MASTER_P(1) = 1.0 / MASTER_P(1)
                    MASTER_P(2) = 1.0 / MASTER_P(2)
                  ELSE
                    READ(in_unit,*)MASTER_spring_type(1), MASTER_k(1), MASTER_V(1)
                    READ(in_unit,*)MASTER_spring_type(2), MASTER_k(2), MASTER_V(2)
                    MASTER_V(1)=MASTER_V(1)/MASTER_k(1)
                    MASTER_V(2)=MASTER_V(2)/MASTER_k(2)
                  ENDIF
                  MASTER_k(3)=0.0
                  MASTER_V(3)=1.0
                  MASTER_spring_type(3)='X'
                 ELSEIF( INDEX(buffer,'3') /= 0 ) THEN
                  IF (rho) THEN 
                    READ(in_unit,*)MASTER_spring_type(1), MASTER_k(1), MASTER_V(1), MASTER_P(1)
                    READ(in_unit,*)MASTER_spring_type(2), MASTER_k(2), MASTER_V(2), MASTER_P(2)
                    READ(in_unit,*)MASTER_spring_type(3), MASTER_k(3), MASTER_V(3), MASTER_P(3)
                    MASTER_P(1) = 1.0 / MASTER_P(1)
                    MASTER_P(2) = 1.0 / MASTER_P(2)
                    MASTER_P(3) = 1.0 / MASTER_P(3)
                  ELSE
                    READ(in_unit,*)MASTER_spring_type(1), MASTER_k(1), MASTER_V(1)
                    READ(in_unit,*)MASTER_spring_type(2), MASTER_k(2), MASTER_V(2)
                    READ(in_unit,*)MASTER_spring_type(3), MASTER_k(3), MASTER_V(3)
                    MASTER_V(1)=MASTER_V(1)/MASTER_k(1)
                    MASTER_V(2)=MASTER_V(2)/MASTER_k(2)
                    MASTER_V(3)=MASTER_V(3)/MASTER_k(3)
                  ENDIF
                 ELSE
                  WRITE(6,*)'Expecting to read data for either 1, 2 or 3 reference springs'
                  STOP
                 ENDIF
                EXIT copyloop
              ENDIF
            ENDIF
            IF( INDEX(buffer,'species') /= 0 .AND. read_local_atoms) THEN
              !N_EXTRA_ATOMS=i
              MASTER_BOTTOM(1) = TRIM(buffer)
              read_local_atoms=.false.
              i = 2 !reset for BOTTOM
            ELSEIF(read_local_atoms) THEN
              MASTER_LOCAL(i) = TRIM(ADJUSTL(buffer))
              i = i + 1
              IF( i > MAX_GULP_EXTRA_ROWS ) THEN
                WRITE(*,*)'Too many extra atom lines in Master Gulp file'
                STOP
              ENDIF
            ELSEIF( INDEX(buffer,'species') /= 0 .AND. i > 1) THEN
              L_DOUBLE_RUN=.TRUE.
              !write(*,*)'double run'
              BACKSPACE(UNIT=in_unit)
              EXIT copyloop
            ELSE
              MASTER_BOTTOM(i) = TRIM(buffer)
              i = i + 1
              IF( i > MAX_GULP_ROWS ) THEN
                WRITE(*,*)'Too many potential lines in Master Gulp file'
                STOP
              ENDIF
                ENDIF
              ELSE
              EXIT copyloop
          END IF
      END DO copyloop
              !WRITE(*,*) (TRIM(MASTER_BOTTOM(k)),'  ', k=1, 100)

      IF (L_DOUBLE_RUN .OR. JOB_TYPE == 4) THEN ! Copy the rest of the file
        i = 1
        lastloop: DO
          READ(UNIT=in_unit,FMT='(A)', IOSTAT=in_error, IOMSG=ERROR_MSG) buffer
          IF(in_error == 0) THEN
              IF( INDEX(buffer,'spring') /= 0 )L_SHELLS_2ND=.TRUE.
              MASTER_SECOND(i) = TRIM(buffer)
              i = i + 1
              IF( i > MAX_GULP_ROWS ) THEN
                WRITE(*,*)'Too many 2nd potential lines in Master Gulp file'
                STOP
              ENDIF
          ELSE
              EXIT lastloop
          END IF
        END DO lastloop
              !WRITE(*,*) (TRIM(MASTER_SECOND(k)),'  ', k=1, 100)
      ENDIF

      CLOSE(UNIT=in_unit)
  END SUBROUTINE readGulpMaster

  !==========================================================================================!

  SUBROUTINE logEnergy(in_cluster, in_min_energy)
      TYPE(cluster), INTENT(IN) :: in_cluster
      REAL(KIND=DBL), INTENT(IN) :: in_min_energy
      INTEGER :: out_error=1, out_unit=28, ide
      LOGICAL :: l_exist
      INQUIRE(FILE=TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//'energy',EXIST=l_exist)
      OPEN(UNIT=out_unit, FILE=TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//'energy', &
      POSITION='APPEND', ACTION='WRITE', IOSTAT=out_error, IOMSG=ERROR_MSG)

      IF (out_error == 0) THEN
        IF (.NOT.l_exist) WRITE(UNIT=out_unit,FMT='(A)',IOSTAT=out_error,IOMSG=ERROR_MSG) &
            'step          energy(new)         Gnorm       putative GM'
        ide=in_cluster%edefn

        IF (ide == 0) THEN
          WRITE(UNIT=out_unit,FMT='(A,3(X,F15.9))',IOSTAT=out_error,IOMSG=ERROR_MSG) &
            in_cluster%id, R_ENERGY_ZERO, R_ENERGY_ZERO, R_ENERGY_ZERO
        ELSEIF (ide == -1) THEN
          WRITE(UNIT=out_unit,FMT='(A,3(X,F15.9))',IOSTAT=out_error,IOMSG=ERROR_MSG) &
            in_cluster%id, R_ENERGY_ZERO, R_ENERGY_ZERO, R_ENERGY_ZERO
        ELSE
          WRITE(UNIT=out_unit,FMT='(A,3(X,F15.9))',IOSTAT=out_error,IOMSG=ERROR_MSG) &
            in_cluster%id,in_cluster%energy(ide),in_cluster%gnorm(ide),in_min_energy
        ENDIF
        CLOSE(UNIT=out_unit)
      END IF
  END SUBROUTINE logEnergy

  !==========================================================================================!

  SUBROUTINE logEnergies(in_cluster, in_old_energy, in_min_energy)
      TYPE(cluster), INTENT(IN) :: in_cluster
      REAL(KIND=DBL), INTENT(IN) :: in_min_energy, in_old_energy
      INTEGER :: out_error=1, out_unit=28, ide
      LOGICAL :: l_exist

      INQUIRE(FILE=TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//'energy',EXIST=l_exist)
      OPEN(UNIT=out_unit, FILE=TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//'energy', &
      POSITION='APPEND', ACTION='WRITE', IOSTAT=out_error, IOMSG=ERROR_MSG)

      IF(out_error == 0) THEN
        IF ( L_AIMS_RUN .OR. L_VASP_RUN) THEN
           IF (.NOT.l_exist) WRITE(UNIT=out_unit,FMT='(A)',IOSTAT=out_error,IOMSG=ERROR_MSG) &
                 'step          energy(new)        energy(old)     putative GM'
           ide=in_cluster%edefn
           WRITE(UNIT=out_unit,FMT='(A,3(X,F15.9))',IOSTAT=out_error,IOMSG=ERROR_MSG) &
            in_cluster%id, in_cluster%energy(ide), in_old_energy, in_min_energy
        ELSE
           IF (.NOT.l_exist) WRITE(UNIT=out_unit,FMT='(A)',IOSTAT=out_error,IOMSG=ERROR_MSG) &
                 'step          energy(new)         Gnorm       energy(old)     putative GM'
           ide=in_cluster%edefn

           IF (ide .EQ. 0) THEN
              WRITE(UNIT=out_unit,FMT='(A,4(X,F15.9))',IOSTAT=out_error,IOMSG=ERROR_MSG) &
                in_cluster%id, 0.0, 0.0, 0.0, 0.0
           ELSE
             WRITE(UNIT=out_unit,FMT='(A,4(X,F15.9))',IOSTAT=out_error,IOMSG=ERROR_MSG) &
              in_cluster%id, in_cluster%energy(ide), in_cluster%gnorm(ide), in_old_energy, in_min_energy
           ENDIF
        END IF
        CLOSE(UNIT=out_unit)
      END IF
  END SUBROUTINE logEnergies

  !==========================================================================================!

  SUBROUTINE logMetropolis(in_id, in_step, in_prob, in_r, in_result)
      CHARACTER(LEN=*), INTENT(IN) :: in_id
      REAL(KIND=DBL), INTENT(IN) :: in_step, in_prob, in_r
      LOGICAL, INTENT(IN) :: in_result
      INTEGER :: out_error=1, out_unit=30
      LOGICAL :: l_exist

      INQUIRE(FILE=TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//'metropolis',EXIST=l_exist)

      OPEN(UNIT=out_unit, FILE=TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//'metropolis', &
      POSITION='APPEND', ACTION='WRITE', IOSTAT=out_error, IOMSG=ERROR_MSG)

      IF(out_error == 0) THEN
        IF (.NOT.l_exist) THEN
          WRITE(UNIT=out_unit,FMT='(A55)', IOSTAT=out_error, &
          IOMSG=ERROR_MSG) 'step       step-height  prob(accept)  Rn-number accept?'
        ENDIF
        IF ( in_r > 2.5 .OR. OUTPUT_LEVEL/=0 ) THEN
          WRITE(UNIT=out_unit,FMT='(A,F10.5,X,F15.10,15X,L1)', IOSTAT=out_error, &
          IOMSG=ERROR_MSG) in_id, -in_step, in_prob, in_result
        ELSEIF ( in_r < 1.1 ) THEN
          WRITE(UNIT=out_unit,FMT='(A,F10.5,X,F15.10,2X,F7.4,6X,L1)', IOSTAT=out_error, &
          IOMSG=ERROR_MSG) in_id, -in_step, in_prob, in_r, in_result
        ENDIF
        CLOSE(UNIT=out_unit)
      END IF
  END SUBROUTINE logMetropolis

  !==========================================================================================!

  SUBROUTINE logSuccess(in_step, in_e, in_str, in_var)
      INTEGER, INTENT(IN) :: in_step
      REAL(KIND=DBL), INTENT(IN) :: in_e
      REAL(KIND=DBL), INTENT(IN) :: in_var
      CHARACTER(LEN=*), INTENT(IN) :: in_str
      INTEGER :: out_error=1, out_unit=69
      OPEN(UNIT=out_unit, FILE=TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//'log-mc', &
      POSITION='APPEND', ACTION='WRITE', IOSTAT=out_error, IOMSG=ERROR_MSG)

      IF(out_error == 0) THEN
        IF (index(in_str,'MC').ne.0) THEN
          WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) &
                   'MC Step ',in_step,': Temp = ',in_var,' E = ',in_e
        ELSEIF (index(in_str,'Monte').ne.0) THEN
          WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) &
                   'MC Step ',in_step,': Step< ',in_var,' E = ',in_e
        ELSE
          WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) &
                   'MC Step ',in_step,': ',in_str,'            E = ',in_e
        ENDIF
        CLOSE(UNIT=out_unit)
      END IF
  END SUBROUTINE logSuccess

  !==========================================================================================!

  SUBROUTINE logMessage(in_step,in_e,in_str)
      INTEGER, INTENT(IN) :: in_step
      REAL(KIND=DBL), INTENT(IN) :: in_e
      CHARACTER(LEN=*), INTENT(IN) :: in_str
      INTEGER :: out_error=1, out_unit=29
      OPEN(UNIT=out_unit, FILE=TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//'log', &
      POSITION='APPEND', ACTION='WRITE', IOSTAT=out_error, IOMSG=ERROR_MSG)

      IF(out_error == 0) THEN
        IF (index(in_str,'Lower').ne.0) THEN
          WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) &
                   'Step ',in_step,': ',in_str,' E = ',in_e
        ELSE
          WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) &
                                               'Step ',in_step,': ',in_str
          IF (index(in_str,'stepsize').ne.0) THEN
            WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) &
                                    '     step size is now ',R_BH_STEPSIZE
          ENDIF
        ENDIF
        CLOSE(UNIT=out_unit)
      END IF
  END SUBROUTINE logMessage

  !==========================================================================================!

  SUBROUTINE logEbest(in_id, in_rank, in_energy)
      CHARACTER(LEN=*), INTENT(IN) :: in_id
      REAL(DBL), INTENT(IN) :: in_energy
      INTEGER, INTENT(IN) :: in_rank
      INTEGER :: out_error=1, out_unit=10
      OPEN(UNIT=out_unit, FILE=TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//'logBest', &
      POSITION='APPEND', ACTION='WRITE', IOSTAT=out_error, IOMSG=ERROR_MSG)

      IF(out_error == 0) THEN
          NEW_THIS_RUN=NEW_THIS_RUN+1
          WRITE(out_unit,*)'New cluster ',in_id,'is ranked',in_rank
          WRITE(out_unit,*)'and has an energy of',in_energy,'eV'
          CLOSE(UNIT=out_unit)
      END IF
  END SUBROUTINE logEbest

  !==========================================================================================!

  SUBROUTINE generateGulpInput(n_gulp_run, generate_cluster, in_filename)
      TYPE(cluster), INTENT(IN) :: generate_cluster
      CHARACTER(LEN=*), INTENT(IN) :: in_filename
      INTEGER, INTENT(IN) :: n_gulp_run

      REAL(DBL) :: xcoord(MAX_ATOMS),ycoord(MAX_ATOMS),zcoord(MAX_ATOMS) ! local copy of atomic coords
      CHARACTER(LEN=6) :: sym_num
      CHARACTER(LEN=4) :: shell_num
      CHARACTER(LEN=2) :: psym
      INTEGER   :: i, j, k, out_error=1, out_unit=27, n_shells, i_site
      INTEGER   :: nset, nsets, im_shells
      REAL(DBL) :: zloc, R_MIRROR_Zx2, R_MIRROR_Zfx2
      LOGICAL   :: l_add_shells, pbc

      l_add_shells=.FALSE.
      IF (n_gulp_run < 2 .AND. L_SHELLS_1ST) l_add_shells=.TRUE.
      IF (n_gulp_run > 1 .AND. L_SHELLS_2ND) l_add_shells=.TRUE.
      !IF (JOB_TYPE_.eq.4) l_add_shells=.TRUE. ! optimisation of springs requires shells

      pbc = ((index(UNIT_CELL,'cell') .NE. 0 .OR. index(UNIT_CELL,'vector') .NE. 0) .AND. &
            (index(UNIT_CELL,'scell') .EQ. 0))

      OPEN(UNIT=out_unit, &
           FILE=TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//TRIM(in_filename)//'.gin', &
           STATUS='REPLACE', &
           ACTION='WRITE', &
           IOSTAT=out_error, &
           IOMSG=ERROR_MSG)

      IF(out_error == 0) THEN
        ! Search and copy until MASTER_TOP is empty (pre-structural data)
        toploop: DO i = 1, SIZE(MASTER_TOP)
           IF(LEN_TRIM(MASTER_TOP(i)) < 1) EXIT toploop
           WRITE(UNIT=out_unit,FMT='(A)', IOSTAT=out_error, IOMSG=ERROR_MSG) TRIM(MASTER_TOP(i))
        END DO toploop

        IF (pbc) THEN
          WRITE(UNIT=out_unit,FMT='(A)', IOSTAT=out_error, IOMSG=ERROR_MSG) TRIM(UNIT_CELL)

          IF (L_GULP_OPTI) THEN
            IF(index(UNIT_CELL,'cell').ne.0)THEN
              WRITE(UNIT=out_unit,FMT='(3(F10.6,X),3(F8.4,X),6(A,X))') &
                   (generate_cluster%box(i),i=1,6),(CELL_FLAGS(j),j=1,6)
            ELSEIF(index(UNIT_CELL,'vector').ne.0)THEN
              WRITE(UNIT=out_unit,FMT='(3(F12.8,X))')(VECTOR(i,1),i=1,3)
              WRITE(UNIT=out_unit,FMT='(3(F12.8,X))')(VECTOR(i,2),i=1,3)
              WRITE(UNIT=out_unit,FMT='(3(F12.8,X))')(VECTOR(i,3),i=1,3)
              WRITE(UNIT=out_unit,FMT='(6(A,X))')(VECTOR_FLAGS(i),i=1,6)
            ENDIF
          ELSE
            IF(index(UNIT_CELL,'cell').ne.0)THEN
              WRITE(UNIT=out_unit,FMT='(6(F12.8,X))')(generate_cluster%box(i),i=1,6)
            ELSEIF(index(UNIT_CELL,'vector').ne.0)THEN
              WRITE(UNIT=out_unit,FMT='(3(F12.8,X))')(VECTOR(i,1),i=1,3)
              WRITE(UNIT=out_unit,FMT='(3(F12.8,X))')(VECTOR(i,2),i=1,3)
              WRITE(UNIT=out_unit,FMT='(3(F12.8,X))')(VECTOR(i,3),i=1,3)
            ENDIF
          ENDIF

        ENDIF

        WRITE(UNIT=out_unit,FMT='(A)', IOSTAT=out_error, IOMSG=ERROR_MSG) TRIM(COORD_TYPE)
        IF (index(COORD_TYPE,'fract').ne.0) THEN ! use fractionals
          DO i = 1, N_ATOMS
            xcoord(i)=generate_cluster%atoms(i)%xf
            ycoord(i)=generate_cluster%atoms(i)%yf
            zcoord(i)=generate_cluster%atoms(i)%zf
          ENDDO
        ELSE ! use Cartesians
          DO i = 1, N_ATOMS
            xcoord(i)=generate_cluster%atoms(i)%xc
            ycoord(i)=generate_cluster%atoms(i)%yc
            zcoord(i)=generate_cluster%atoms(i)%zc
          ENDDO
        ENDIF

        IF (L_IMAGE_ATOMS) THEN
          nset = 2
          R_MIRROR_Zx2 = R_MIRROR_Z + R_MIRROR_Z
          R_MIRROR_Zfx2 = R_MIRROR_Zf + R_MIRROR_Zf
        ELSE
          nset = 1
          R_MIRROR_Zx2 = 0.0
          R_MIRROR_Zfx2 = 0.0
        ENDIF

        DO nsets = 1, nset
         atomsloop: DO i = 1, N_ATOMS
          psym =  generate_cluster%atoms(i)%symbol
          IF (nsets == 1) THEN
            zloc =  zcoord(i)
          ELSE
            DO j=1, N_IMAGES
              IF (psym==DEF_IMAGES(2,j)) THEN
                psym = DEF_IMAGES(1,j)
                EXIT
              ENDIF
            ENDDO
            zloc = R_MIRROR_Zx2 - zcoord(i)
          ENDIF
          IF (index(generate_cluster%atoms(i)%cstype,'x').eq.0) THEN
           !IF (index(generate_cluster%atoms(i)%cstype,'k').eq.1) THEN
            IF (JOB_TYPE == 4) THEN
              shell_num(1:4)='    '
              i_site = generate_cluster%atoms(i)%site
              IF (i_site < 10) THEN
                WRITE(shell_num(1:1), '(i1)' ) i_site
              ELSEIF (i_site < 100) THEN
                WRITE(shell_num(1:2), '(i2)' ) i_site
              ELSEIF (i_site < 1000) THEN
                WRITE(shell_num(1:3), '(i3)' ) i_site
              ELSE
                WRITE(shell_num(1:4), '(i4)' ) i_site
              ENDIF
              WRITE(sym_num,*)TRIM(psym),TRIM(shell_num)
              WRITE(MASTER_ATOMS(i,1),'(6A)') sym_num
            ELSE
              WRITE(MASTER_ATOMS(i,1),'(2A)') psym
            ENDIF
            WRITE(MASTER_ATOMS(i,2),'(4A)') 'core'
            !WRITE(MASTER_ATOMS(i,3),'(1F15.12)') xcoord(i)
            !WRITE(MASTER_ATOMS(i,4),'(1F15.12)') ycoord(i)
            !WRITE(MASTER_ATOMS(i,5),'(1F15.12)') zloc
            WRITE(MASTER_ATOMS(i,3),'(1F20.9)') xcoord(i) ! values taken from Gulp
            WRITE(MASTER_ATOMS(i,4),'(1F20.9)') ycoord(i) ! values taken from Gulp
            WRITE(MASTER_ATOMS(i,5),'(1F20.9)') zloc      ! values taken from Gulp
            WRITE(UNIT=out_unit,FMT=*) (TRIM(ADJUSTL(MASTER_ATOMS(i,k))),'  ', k=1, 10)
          ENDIF
         ENDDO atomsloop

         IF (JOB_TYPE == 4) THEN

          n_shells=0
          shellskloop: DO i = 1, N_ATOMS
            IF ((index(generate_cluster%atoms(i)%cstype,'s').eq.1) .OR. &
                (index(generate_cluster%atoms(i)%cstype,'k').eq.1) ) THEN
              n_shells=n_shells+1
              WRITE(MASTER_ATOMS(i,2),'(4A)') 'shel'
              WRITE(UNIT=out_unit,FMT=*) (TRIM(ADJUSTL(MASTER_ATOMS(i,k))),'  ', k=1, 10)
            ENDIF
          END DO shellskloop

         ELSEIF (nsets > 1) THEN

          im_shells=0
          IF (l_add_shells) THEN ! And now for the shells
            imshellsloop: DO i = 1, N_ATOMS
              IF ((index(generate_cluster%atoms(i)%cstype,'s').eq.1).OR. &
                (n_gulp_run>1.AND.index(generate_cluster%atoms(i)%cstype,'h').eq.1)) THEN
                im_shells=im_shells+1
                WRITE(MASTER_ATOMS(i,2),'(4A)') 'shel'
                WRITE(UNIT=out_unit,FMT=*) (TRIM(ADJUSTL(MASTER_ATOMS(i,k))),'  ', k=1, 10)
              ENDIF
            END DO imshellsloop
          ENDIF

         ELSE

          IF (l_add_shells) THEN ! And now for the shells
            shellsloop: DO i = 1, N_ATOMS
              IF ((index(generate_cluster%atoms(i)%cstype,'s').eq.1).OR. &
                (n_gulp_run>1.AND.index(generate_cluster%atoms(i)%cstype,'h').eq.1)) THEN
                WRITE(MASTER_ATOMS(i,2),'(4A)') 'shel'
                WRITE(UNIT=out_unit,FMT=*) (TRIM(ADJUSTL(MASTER_ATOMS(i,k))),'  ', k=1, 10)
              ENDIF
            END DO shellsloop
          ENDIF

         ENDIF
        ENDDO ! L_IMAGE_ATOMS => REPEAT THIS LOOP

        ! copy in additional (extra) atoms that are initially known
        localloop: DO i = 1, SIZE(MASTER_LOCAL)
          IF(LEN_TRIM(MASTER_LOCAL(i)) < 1) EXIT localloop
          WRITE(UNIT=out_unit,FMT='(A)', IOSTAT=out_error, IOMSG=ERROR_MSG) MASTER_LOCAL(i)
        END DO localloop

        IF (L_IMAGE_ATOMS) THEN
         !ideally R_MIRROR_Zx2 should be R_MIRROR_Zx2*generate_cluster%box(3) but if
         !we use "VECTORS" as gulp option word and not "VECTOR" or "VECT" no cell parameters available
          IF (l_add_shells) THEN
            WRITE(UNIT=out_unit,FMT=*)'constrain ',3*(N_ATOMS+im_shells)
            DO i = 1, N_ATOMS+im_shells ! use im_shells, but should use N_SHELLS?
              WRITE(UNIT=out_unit,FMT=*)i,' x ',i+N_ATOMS+im_shells,' x  1.0 0.0'
              WRITE(UNIT=out_unit,FMT=*)i,' y ',i+N_ATOMS+im_shells,' y  1.0 0.0'
              WRITE(UNIT=out_unit,FMT=*)i,' z ',i+N_ATOMS+im_shells,' z -1.0 ',R_MIRROR_Zfx2
            ENDDO
            !NB GULP re-arranges cores and shells AND adds N_EXTRA_ATOMS to pointer used in constrian option
          ELSE
            WRITE(UNIT=out_unit,FMT=*)'constrain ',3*N_ATOMS
            DO i = 1, N_ATOMS
              WRITE(UNIT=out_unit,FMT=*)i,' x ',i+N_ATOMS,' x  1.0 0.0'
              WRITE(UNIT=out_unit,FMT=*)i,' y ',i+N_ATOMS,' y  1.0 0.0'
              WRITE(UNIT=out_unit,FMT=*)i,' z ',i+N_ATOMS,' z -1.0 ',R_MIRROR_Zfx2
            ENDDO
          ENDIF
        ENDIF

        IF (JOB_TYPE == 4) THEN

          ! Write out copy up until the 'spring' keyword line
          bottomkloop: DO i = 1, SIZE(MASTER_BOTTOM)
            IF(LEN_TRIM(MASTER_BOTTOM(i)) < 1) EXIT bottomkloop
            WRITE(UNIT=out_unit,FMT='(A)', IOSTAT=out_error, IOMSG=ERROR_MSG) TRIM(MASTER_BOTTOM(i))
          END DO bottomkloop

          ! Write out spring constants
          IF (n_shells > 0) THEN
            WRITE(UNIT=out_unit,FMT=*)'spring' ! ,n_shells
            springsloop: DO i = 1, N_ATOMS
             IF (index(generate_cluster%atoms(i)%cstype,'k').eq.1) THEN
              WRITE(UNIT=out_unit,FMT=*) TRIM(MASTER_ATOMS(i,1)),' ',generate_cluster%atoms(i)%k
             ENDIF
            END DO springsloop
          ENDIF

          ! Write out rest of copy
          secondkloop: DO i = 1, SIZE(MASTER_SECOND)
            IF(LEN_TRIM(MASTER_SECOND(i)) < 1) EXIT secondkloop
            WRITE(UNIT=out_unit,FMT='(A)', IOSTAT=out_error, IOMSG=ERROR_MSG) TRIM(MASTER_SECOND(i))
          END DO secondkloop

        ELSE

          ! Copy the rest of the file
          IF (n_gulp_run < 2) THEN ! on first run so use first IP
            bottomloop: DO i = 1, SIZE(MASTER_BOTTOM)
              IF(LEN_TRIM(MASTER_BOTTOM(i)) < 1) EXIT bottomloop
              WRITE(UNIT=out_unit,FMT='(A)', IOSTAT=out_error, IOMSG=ERROR_MSG) TRIM(MASTER_BOTTOM(i))
            END DO bottomloop
          ELSE
            secondloop: DO i = 1, SIZE(MASTER_SECOND)
              IF(LEN_TRIM(MASTER_SECOND(i)) < 1) EXIT secondloop
              WRITE(UNIT=out_unit,FMT='(A)', IOSTAT=out_error, IOMSG=ERROR_MSG) TRIM(MASTER_SECOND(i))
            END DO secondloop
          ENDIF

        ENDIF

        ! Print output format and path
        IF (pbc) THEN
          WRITE(UNIT=out_unit,FMT='(A,A)',IOSTAT=out_error,IOMSG=ERROR_MSG) &
               'output arc ',TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//TRIM(in_filename)
        ELSE
          WRITE(UNIT=out_unit,FMT='(A,A)',IOSTAT=out_error,IOMSG=ERROR_MSG) &
               'output xyz ',TRIM(WORKING_DIR)//TRIM(RELAXED_FOLDER)//TRIM(in_filename)
        ENDIF
        CLOSE(UNIT=out_unit)

      END IF
  END SUBROUTINE generateGulpInput

  !==========================================================================================!

  SUBROUTINE generateAimsInput(in_cluster, in_filename)
      TYPE(cluster), INTENT(IN)    :: in_cluster
      CHARACTER(LEN=*), INTENT(IN) :: in_filename
      CHARACTER(LEN=256)           :: aims_control_file
      INTEGER                      :: i, k, out_error=1, out_unit=27
      LOGICAL                      :: pbc =.TRUE.

      !Cluster or bulk? 
      IF (N_PBC == 0) pbc = .FALSE.

      CALL makeFolder(TRIM(RELAXED_FOLDER)//TRIM(in_filename))

      aims_control_file = TRIM(DATA_FOLDER)//TRIM(AIMS_MASTER_NAME)
      CALL SYSTEM('cp '//TRIM(aims_control_file)//' '// &
                       TRIM(RELAXED_FOLDER)//TRIM(in_filename)//'/control.in' )

      OPEN(UNIT=out_unit, FILE=TRIM(RELAXED_FOLDER)//TRIM(in_filename)// &
           '/geometry.in',STATUS='REPLACE', ACTION='WRITE', & 
           IOSTAT=out_error, IOMSG=ERROR_MSG)
      IF(out_error == 0) THEN
         IF (pbc) THEN 
            WRITE(UNIT=out_unit,FMT='(A,3(F12.8,X))') &
            'lattice_vector', (VECTOR(i,1),i=1,3)
            WRITE(UNIT=out_unit,FMT='(A,3(F12.8,X))') &
            'lattice_vector', (VECTOR(i,2),i=1,3)
            WRITE(UNIT=out_unit,FMT='(A,3(F12.8,X))') &
            'lattice_vector', (VECTOR(i,3),i=1,3)
         END IF
         ! Now for atomic positions 
         DO i = 1, N_ATOMS
            WRITE(out_unit,'(1X,A,3(2X,F16.8),2X,A)') 'atom ', &
            in_cluster%atoms(i)%xc, in_cluster%atoms(i)%yc, &
            in_cluster%atoms(i)%zc, in_cluster%atoms(i)%symbol
         END DO
         CLOSE(UNIT=out_unit)
      ELSE
          WRITE(stderr,*) 'Error opening fhi-aims geometry file on CPU',my_rank
          STOP
      END IF
    RETURN
  END SUBROUTINE generateAimsInput
  !==========================================================================================!
SUBROUTINE generateNWChemInput(in_cluster, in_filename)
      TYPE(cluster), INTENT(IN)    :: in_cluster
      CHARACTER(LEN=*), INTENT(IN) :: in_filename
      INTEGER                      :: i, k, out_error=1, out_unit=27
      LOGICAL                      :: pbc =.TRUE.

      !Cluster or bulk? 
      IF (N_PBC == 0) pbc = .FALSE.

      CALL makeFolder(TRIM(RELAXED_FOLDER)//TRIM(in_filename))

      OPEN(UNIT=out_unit, FILE=TRIM(RELAXED_FOLDER)//TRIM(in_filename)// &
           '/input.nw',STATUS='REPLACE', ACTION='WRITE', & 
           IOSTAT=out_error, IOMSG=ERROR_MSG)
      IF(out_error == 0) THEN
         WRITE(out_unit,*) 'Start NWChem job'
         WRITE(out_unit,*) 'title "KLMC generated input file"'
         WRITE(out_unit,*) 'geometry units angstrom'

        !IF (pbc) THEN 
        !END IF
        !Now for atomic positions 
         DO i = 1, N_ATOMS
            WRITE(out_unit,'(1X,A,3(2X,F16.8))') &
            in_cluster%atoms(i)%symbol, &
            in_cluster%atoms(i)%xc,     & 
            in_cluster%atoms(i)%yc,     &
            in_cluster%atoms(i)%zc
         END DO

         WRITE(out_unit,*) 'end'
         WRITE(out_unit,*) 'basis'
         WRITE(out_unit,*) 'Zn library ',TRIM(NWCHEM_BASIS_SET)
         WRITE(out_unit,*) 'O  library ',TRIM(NWCHEM_BASIS_SET)
         WRITE(out_unit,*) 'end'        
         WRITE(out_unit,*) 'dft'        
         WRITE(out_unit,*) 'xc xpbe96 cpbe96'        
         WRITE(out_unit,*) 'iterations 200'        
         WRITE(out_unit,*) 'end'       
         WRITE(out_unit,*) 'DRIVER'       
         WRITE(out_unit,*) 'NOXYZ'       
         WRITE(out_unit,*) 'MAXITER 100'       
         WRITE(out_unit,*) 'end'       
         
         WRITE(out_unit,*) 'task dft optimize'        
         CLOSE(UNIT=out_unit)
      ELSE
          WRITE(stderr,*) 'Error opening NWChem file on CPU',my_rank
          STOP
      END IF
    RETURN
  END SUBROUTINE generateNWChemInput

  !==========================================================================================!

  SUBROUTINE generateVaspInput(in_cluster, in_filename)
      TYPE(cluster), INTENT(IN)    :: in_cluster
      CHARACTER(LEN=*), INTENT(IN) :: in_filename
      INTEGER                      :: i,j,ierror,out_unit=27
      LOGICAL                      :: pbc=.TRUE.
      
      !Cluster or bulk?.
      IF (N_PBC == 0) pbc = .FALSE.

      CALL makeFolder(TRIM(RELAXED_FOLDER)//TRIM(in_filename))
      CALL SYSTEM('cp '//TRIM(DATA_FOLDER)//'POTCAR ' &
                       //TRIM(RELAXED_FOLDER)//TRIM(in_filename) )
      CALL SYSTEM('cp '//TRIM(DATA_FOLDER)//'INCAR ' &
                       //TRIM(RELAXED_FOLDER)//TRIM(in_filename) )
      CALL SYSTEM('cp '//TRIM(DATA_FOLDER)//'KPOINTS ' &
                       //TRIM(RELAXED_FOLDER)//TRIM(in_filename) )

      OPEN(UNIT=out_unit, FILE=TRIM(RELAXED_FOLDER)//TRIM(in_filename)// &
           '/POSCAR',STATUS='REPLACE',ACTION='WRITE', &
           IOSTAT=ierror, IOMSG=ERROR_MSG)

      WRITE(out_unit,*) 'KLMC generated VASP input'
      IF (pbc) THEN
         WRITE(out_unit,*) 1.00000_dp
         DO i = 1,3
            WRITE(out_unit,*)  (VECTOR(j,i),j=1,3)
         END DO
         !WRITE(out_unit,*) 'Li Be F'
         WRITE(out_unit,*) TRIM(VASP_SYSTEM)
         WRITE(out_unit,*) TRIM(VASP_SYSTEM_SIZE)
         !WRITE(out_unit,*) 'Ta O'
         
         !WRITE(out_unit,*) '4 10'
         !WRITE(out_unit,*) '8 20'
         !WRITE(out_unit,*) '6 15'
         !WRITE(out_unit,*) '2 1 4'
         !WRITE(out_unit,*) '2 5'
         WRITE(out_unit,*) 'Direct'
         DO i = 1,N_ATOMS
            WRITE(out_unit,*) in_cluster%atoms(i)%xf,&
                              in_cluster%atoms(i)%yf,&
                              in_cluster%atoms(i)%zf
        END DO
      ELSE
         WRITE(stderr,*) 'Cluster not available for KLMC using VASP yet!'
         STOP
      END IF
      CLOSE(out_unit)
     RETURN
  END SUBROUTINE generateVaspInput

  !==========================================================================================!
  ! A procedure to generate a dmol3 input file according to the stage
  !==========================================================================================!
  SUBROUTINE generateDmolInput(nDmolStage, outFilePath, onlyEnergyEval)
    INTEGER,          INTENT(IN) :: nDmolStage
    CHARACTER(LEN=*), INTENT(IN) :: outFilePath
    LOGICAL,          INTENT(IN) :: onlyEnergyEval


    INTEGER :: in_unit=26, out_unit=31, in_error=1, out_error=1, lineNumber, nLineSt=0, nLineEnd=0
    CHARACTER(LEN=100) :: masterDmolPath
    CHARACTER(LEN=200) :: line, energyLine

    CHARACTER(LEN=10) :: stageSt, stageEnd

    ! locate stage lines
    lineNumber = 0

    stageSt  = "#Stage" // TRIM(ADJUSTL(intToStr(nDmolStage)))
    stageEnd = "#Stage" // TRIM(ADJUSTL(intToStr(nDmolStage+1)))

    masterDmolPath = TRIM(WORKING_DIR) // TRIM(DATA_FOLDER) // TRIM(DMOL_MASTER_NAME)

    OPEN(UNIT=in_unit, FILE=masterDmolPath, STATUS='OLD', IOSTAT=in_error, IOMSG=ERROR_MSG)

    IF (in_error /=0) THEN
       WRITE(STDERR,*) 'generateDmolInput :: Error opening ' // TRIM(masterDmolPath)
       STOP
    END IF

    IF(in_error == 0) THEN
      DO
        lineNumber = lineNumber + 1

        READ (in_unit, FMT='(A200)', iostat=in_error, IOMSG=ERROR_MSG) line

        IF (in_error /= 0) THEN
          EXIT
        ENDIF

        IF (index(line, stageSt) > 0) THEN
          nLineSt = lineNumber + 1
        ENDIF

        IF ((nLineSt > 0) .AND. (index(line, stageEnd) > 0)) THEN
          nLineEnd = lineNumber - 1
        ENDIF

      ENDDO
    ELSE
       WRITE(*,*) ERROR_MSG
    END IF

    CLOSE(in_unit)

    IF (nLineSt  .EQ. 0) nLineSt  = 1
    IF (nLineEnd .EQ. 0) nLineEnd = lineNumber - 1

    IF (nLineSt .GE. nLineEnd) THEN
      WRITE(STDERR,*) 'Error opening in locating stage: ' // TRIM(stageSt)  // ' in ' // &
        masterDmolPath

      STOP
    ENDIF

    ! copy the stage lines
    lineNumber = 0
    OPEN(UNIT=in_unit, FILE=masterDmolPath, STATUS='OLD', IOSTAT=in_error, IOMSG=ERROR_MSG)

    IF (in_error /= 0) THEN
       WRITE(STDERR,*) 'Error opening ' // TRIM(masterDmolPath)
       STOP
    END IF

    OPEN(UNIT=out_unit, FILE=TRIM(outFilePath), STATUS='REPLACE', ACTION='WRITE', &
        IOSTAT=out_error, IOMSG=ERROR_MSG)

    IF (out_error /= 0) THEN
       WRITE(STDERR,*) 'Error opening ' // TRIM(outFilePath)
       STOP
    END IF

    DO
      lineNumber = lineNumber + 1

      READ (in_unit, FMT='(A200)', iostat=in_error, IOMSG=ERROR_MSG) line

      IF (in_error /= 0) THEN
        EXIT
      ENDIF
      IF (line(1:4) /= '#---') THEN
        IF ((lineNumber .GE. nLineSt) .AND. (lineNumber .LE. nLineEnd)) THEN

          IF ((onlyEnergyEval) .AND. (index(line(1:10), "Calculate") > 0)) THEN

!            WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) &
!              "Calculate                     energy"

          ELSE
            WRITE(UNIT=out_unit,FMT='(A200)', IOSTAT=out_error, IOMSG=ERROR_MSG) line
          ENDIF
        ENDIF
      ENDIF
    ENDDO

    CLOSE(in_unit)
    CLOSE(out_unit)

  END SUBROUTINE generateDmolInput
  !==========================================================================================!

  SUBROUTINE peek(in_cluster)
      TYPE(cluster), INTENT(IN) :: in_cluster
      INTEGER :: j, ide, n, nblank
      CHARACTER(LEN=6) :: atom_number
      CHARACTER(LEN=60) :: coords

      CALL findChannel(n)

      ide=in_cluster%edefn
      CALL printHeaderLine
      WRITE(n,*)'ID: ',TRIM(in_cluster%id),' Energy: ',in_cluster%energy(ide)
      IF (N_PBC > 0) THEN 
         CALL printHeaderLine
         CALL printLine('  a           b           c           alpha        beta       gamma')
         WRITE(n,'(3g12.5,3g12.4)')(in_cluster%box(j),j=1,6)
      END IF
      CALL printHeaderLine
      CALL printLine('       Atom         X                   Y                   Z')
      IF (N_ATOMS > 999999) STOP
      DO j = 1, N_ATOMS
         atom_number(1:6)='      '
         nblank=7-LEN_TRIM(intToChar(j))
         atom_number(nblank:6)=TRIM(intToChar(j))
         WRITE(coords,'(3(f16.10,4x))')in_cluster%atoms(j)%xc,in_cluster%atoms(j)%yc,in_cluster%atoms(j)%zc
         WRITE(n,*)'  '//  atom_number(1:6) //' ',in_cluster%atoms(j)%symbol,' '// TRIM(coords)
      END DO
      IF (N_PBC > 0) THEN 
         CALL printHeaderLine
         CALL printLine('   Fractional       x                   y                   z')
         DO j = 1, N_ATOMS
            atom_number(1:6)='      '
            nblank=7-LEN_TRIM(intToChar(j))
            atom_number(nblank:6)=TRIM(intToChar(j))
            WRITE(coords,'(3(f16.10,4x))')in_cluster%atoms(j)%xf,in_cluster%atoms(j)%yf,in_cluster%atoms(j)%zf
            WRITE(n,*)'  '//  atom_number(1:6) //' ',in_cluster%atoms(j)%symbol,' '// TRIM(coords)
         END DO
      END IF
      CALL printHeaderLine
      CALL printEmptyLine
  END SUBROUTINE peek

  !==========================================================================================!

  SUBROUTINE printRDF(in_rdf,in_filename)
      TYPE(rdf), INTENT(IN) :: in_rdf
      CHARACTER(LEN=*), INTENT(IN) :: in_filename
      INTEGER :: i,j
      CHARACTER*5  :: outname='.data'

      DO i=1,in_rdf%npairs
         OPEN(UNIT=1,FILE=TRIM(in_filename)//'-'// &
            TRIM(in_rdf%pairs(i)%name(1:2))//'-'// &
            TRIM(in_rdf%pairs(i)%name(4:5))//outname)
         WRITE(1,*)in_rdf%pairs(i)%ndist
         DO j=1,in_rdf%pairs(i)%ndist
            WRITE(1,*)in_rdf%pairs(i)%rdist(j),in_rdf%pairs(i)%gr(j)
         ENDDO
         CLOSE(UNIT=1)
      ENDDO

  END SUBROUTINE printRDF

  !==========================================================================================!

  SUBROUTINE printTrdf(in_Trdf,in_filename)
      TYPE(Trdf), INTENT(IN) :: in_Trdf
      CHARACTER(LEN=*), INTENT(IN) :: in_filename
      INTEGER, DIMENSION(:), ALLOCATABLE :: n ! next distance
      INTEGER :: i,j
      LOGICAL :: lstop
      REAL(KIND=DBL) :: zero, sum, dr_acc, r, E_min
      CHARACTER*5  :: outname='.plot'
      zero=0.0_DBL
      dr_acc=R_RDF_ACC ! accuracy between discrete unique distances
      E_min=0.00001_DP ! accuracy below which Histogram data is not printed

      OPEN(UNIT=1,FILE=TRIM(in_filename)//outname)
      WRITE(1,*)'# ',in_Trdf%nstruct,' structures'
      WRITE(1,*)'# ',R_TEMPERATURE,' K'
      WRITE(1,*)'# ',in_Trdf%energy,' eV'
      WRITE(1,*)'# ',in_Trdf%norm
      DO i=1,in_Trdf%npairs
        WRITE(1,*)'# ',TRIM(in_Trdf%pairs(i)%name(1:2))//' ' &
                //TRIM(in_Trdf%pairs(i)%name(4:5))
      ENDDO
      WRITE(1,*)
      IF (R_RDF_SIGMA < 0.001 .AND. .NOT. L_HISTOGRAM) THEN
        ALLOCATE(n(in_Trdf%npairs))
        DO i = 1, in_Trdf%npairs
          n(i)=1
        ENDDO
        DO
          lstop=.true.
          DO i=1,in_Trdf%npairs
            IF (n(i).le.in_Trdf%pairs(i)%ndist) lstop = .false.
          ENDDO
          IF (lstop) EXIT
          r = 100.0
          DO i = 1, in_Trdf%npairs
            IF (n(i).le.in_Trdf%pairs(i)%ndist) THEN
              IF (in_Trdf%pairs(i)%rdist(n(i)) < r) r = in_Trdf%pairs(i)%rdist(n(i))
            ENDIF
          ENDDO
          r = r + dr_acc
          sum = zero
          DO i=1,in_Trdf%npairs
  !         IF (n(i).le.in_Trdf%pairs(i)%ndist) THEN
            DO j=n(i),in_Trdf%pairs(i)%ndist
               IF (in_Trdf%pairs(i)%rdist(j) < r) THEN
                 sum = sum + in_Trdf%pairs(i)%gr(j)
                 n(i)=n(i)+1
               ELSE
                 EXIT
               ENDIF
            ENDDO
  !         ENDIF
          ENDDO
          WRITE(1,*)r-dr_acc,sum/in_Trdf%norm
        ENDDO
        DEALLOCATE(n)
      ELSE
        DO j=1,N_RDF_STEPS ! default = MAX_RDF_DIST
          sum = zero
          DO i=1,in_Trdf%npairs
            sum = sum + in_Trdf%pairs(i)%gr(j)
          ENDDO
          IF (R_RDF_SIGMA .ge. 0.001 .OR. sum > E_min) THEN
  !         IF (in_Trdf%norm < 0.0001) THEN
  !           WRITE(1,*)in_Trdf%pairs(1)%rdist(j),zero
  !         ELSE
  ! JULY 19
  !   WAS     WRITE(1,*)in_Trdf%pairs(1)%rdist(j),sum*exp(-in_Trdf%pairs(1)%rdist(j))/in_Trdf%norm
              WRITE(1,*)in_Trdf%pairs(1)%rdist(j),sum/in_Trdf%norm
  ! JULY 19
  !         ENDIF
          ENDIF
        ENDDO
      ENDIF
      CLOSE(UNIT=1)

      DO i=1,in_Trdf%npairs
         OPEN(UNIT=1,FILE=TRIM(in_filename)//'-'// &
           TRIM(in_Trdf%pairs(i)%name(1:2))//'-'// &
           TRIM(in_Trdf%pairs(i)%name(4:5))//outname)
         WRITE(1,*)'# ',in_Trdf%pairs(i)%ndist,' ',in_Trdf%norm
         DO j=1,in_Trdf%pairs(i)%ndist
            IF (R_RDF_SIGMA .ge. 0.001 .OR. in_Trdf%pairs(i)%gr(j) > E_min) THEN
              IF (in_Trdf%pairs(i)%gr(j) < 0.0001) THEN
                WRITE(1,*)in_Trdf%pairs(i)%rdist(j),zero
              ELSE
  ! JULY 19
                WRITE(1,*)in_Trdf%pairs(i)%rdist(j), &
                                                in_Trdf%pairs(i)%gr(j)/in_Trdf%norm
  !  WAS        WRITE(1,*)in_Trdf%pairs(i)%rdist(j), &
  !  WAS        in_Trdf%pairs(i)%gr(j)*exp(-in_Trdf%pairs(i)%rdist(j))/in_Trdf%norm
  ! JULY 19
  !                       in_Trdf%pairs(i)%gr(j) / (in_Trdf%pairs(i)%rdist(j)* &
  !               in_Trdf%pairs(i)%rdist(j)*in_Trdf%pairs(i)%rdist(j)*in_Trdf%norm)
              ENDIF
            ENDIF
         ENDDO
         CLOSE(UNIT=1)
      ENDDO

  END SUBROUTINE printTrdf

  !==========================================================================================!

  SUBROUTINE printBestSet(in_number)
      INTEGER, INTENT(IN) :: in_number
      INTEGER :: i,n

      IF (in_number.eq.0) THEN
        loop_find: DO i=INT_MAX_BEST,1,-1
          IF (index(ID_BEST(i),'x').eq.0) THEN
            IF (NEW_THIS_RUN > 0) THEN
              CALL printHeaderCentred('Found '//TRIM(intToChar(i))//' candidates with energy < ' &
              //TRIM(realToChar(R_BEST_EMAX))//' eV, '//TRIM(intToChar(NEW_THIS_RUN))//' on this run!')
            ELSE
              CALL printHeaderCentred('Did not improve current set of top candidates on this run!')
              CALL printHeaderCentred('Still have '//TRIM(intToChar(i))//' candidates with energy ' &
                                                  //'less than '//TRIM(realToChar(R_BEST_EMAX))//' eV')
            ENDIF
            EXIT loop_find
          ELSEIF (i.eq.1) THEN
            CALL printHeaderCentred('Failed to find candidates with energy less than '// &
                                                                  TRIM(realToChar(R_BEST_EMAX))//' eV')
          ENDIF
        ENDDO loop_find
      ELSE
        n=in_number
        IF (in_number>INT_MAX_BEST) n=INT_MAX_BEST
        CALL printHeaderCentred('Top candidates found with energy less than '//TRIM(realToChar(R_BEST_EMAX))//' eV ')
        DO i=1,n
          IF (index(ID_BEST(i),'x').eq.0) THEN
            CALL printHeaderCentred(' Candidate '//TRIM(intToChar(i))//' with energy '//TRIM(realToChar(R_BEST_ENERGIES(i))))
          ENDIF
        ENDDO
      ENDIF

  END SUBROUTINE printBestSet

  !==========================================================================================!

  SUBROUTINE moveBestdata(in_set_number)
      CHARACTER(LEN=*), INTENT(IN) :: in_set_number
      CHARACTER(LEN=200) :: foldername

      foldername=TRIM(WORKING_DIR)//TRIM(BEST_FOLDER)

      CALL makeFolder(TRIM(foldername)//TRIM(in_set_number))
      CALL SYSTEM('mv -f '//TRIM(foldername)//'statistics '//TRIM(foldername)//TRIM(in_set_number))
      CALL SYSTEM('mv -f '//TRIM(foldername)//'energies '//TRIM(foldername)//TRIM(in_set_number))
      CALL SYSTEM('mv -f '//TRIM(foldername)//'*xyz '//TRIM(foldername)//TRIM(in_set_number))
      IF (N_PBC == 3) THEN
        CALL SYSTEM('mv -f '//TRIM(foldername)//'*arc '//TRIM(foldername)//TRIM(in_set_number))
      ENDIF

  END SUBROUTINE moveBestdata

  !==========================================================================================!

  SUBROUTINE displayGULPcounters
      CALL printEmptyLine
      CALL printHeaderLine
      CALL printHeaderCentred('DATA EXTRACTED FROM GULP OUTPUT')
      CALL printHeaderLine
      CALL printHeaderCentred('Number of line searches attempted: '//TRIM(intToChar(N_GULP_L)))
      IF (N_GULP_E > 0) THEN
      CALL printHeaderCentred('Number of times energy computed: '//TRIM(intToChar(N_GULP_E)))
      CALL printHeaderCentred('Number of times gradients computed: '//TRIM(intToChar(N_GULP_dE)))
      CALL printHeaderCentred('Number of 2nd derivatives computed: '//TRIM(intToChar(N_GULP_d2E)))
      ENDIF
      CALL printHeaderLine
      CALL printEmptyLine
  END SUBROUTINE displayGULPcounters

  !==========================================================================================!

  SUBROUTINE openOutputFile
      INTEGER :: ierror
      OPEN(UNIT=stdout, FILE='KLMC.out', &
         STATUS='REPLACE', ACTION='WRITE', IOSTAT=ierror, IOMSG=ERROR_MSG)
      IF(ierror /= 0) THEN
         WRITE(*,*) 'Error opening output file!',ERROR_MSG
         STOP
      END IF
      RETURN
  END SUBROUTINE openOutputFile

  !==========================================================================================!
  SUBROUTINE openProgressFile
    INTEGER :: ierror

    !DEBUG WKJEE 2024
    write(*,*) "1 - File.f90", ierror
    OPEN(UNIT=stdsee, FILE='KLMC.log', STATUS='REPLACE', ACTION='WRITE', &
         IOSTAT=ierror, IOMSG=ERROR_MSG)
    !DEBUG
    write(*,*) "1 - File.f90", ierror

    IF(ierror /= 0) THEN
       WRITE(*,*) 'Error opening log file!', ERROR_MSG
       STOP
    END IF

    !DEBUG WKJEE 2024
    write(*,*) "2 - File.f90", ierror
    RETURN
  END SUBROUTINE openProgressFile

  !==========================================================================================!

  SUBROUTINE makeFolder(in_folderName, out_already_opened)
    CHARACTER(LEN=*), INTENT(IN) :: in_folderName
    LOGICAL, INTENT(OUT), OPTIONAL :: out_already_opened

    LOGICAL :: l_exist

    IF (in_folderName == '') THEN ! LEN_TRIM(in_folderName) > 0
      l_exist = .TRUE.
    ELSE

      !INQUIRE(FILE=TRIM(in_folderName),EXIST=l_exist)
      l_exist = inquireDirectory(in_folderName)

      IF (l_exist) THEN
        CALL printHeaderCentred('')
        CALL printStringColumns('Reopening folder:',TRIM(in_folderName))
        CALL printHeaderCentred('')

      ELSE
        CALL printHeaderCentred('')
        CALL printStringColumns('Creating folder:',TRIM(in_folderName))
        !DEBUG WKJEE 2024
        write(*,*) "3 - File.f90 : ", in_folderName
        CALL SYSTEM('mkdir -pv ' // TRIM(in_folderName))
        !DEBUG WKJEE 2024
        write(*,*) "4 - File.f90 : ", in_folderName

        CALL printHeaderCentred('')

        !DEBUG WKJEE 2024
        write(*,*) "5 - File.f90 ------ "
      ENDIF
    ENDIF

    IF (PRESENT(out_already_opened)) out_already_opened = l_exist

  END SUBROUTINE makeFolder

  !==========================================================================================!

  SUBROUTINE openErrorFile
      INTEGER :: ierror

      IF(MASTER_PROC) THEN
         !OPEN file for any errors/ warnings

         OPEN(UNIT=stderr, FILE=TRIM(WORKING_DIR)//'KLMC.err', &
              STATUS='REPLACE', ACTION='READWRITE', IOSTAT=ierror, IOMSG=ERROR_MSG)
         IF (ierror /= 0 ) THEN
            WRITE(*,*) 'Error opening error file!'
            STOP
         ELSE
            WRITE(stderr,*)'      -KLMC-error-file-      '
         END IF
      END IF
      RETURN
  END SUBROUTINE openErrorFile

END MODULE File
