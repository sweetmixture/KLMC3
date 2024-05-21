!==========================================================================================!
! This module is dedicated to hold testing routines (unittests)
!==========================================================================================!

MODULE Testing
  USE Config
  USE Format

  USE File
  USE ClusterRoutines
  USE Utilities
  USE Grid
  USE Population
  USE Master
  USE Moveclass
  USE SolidSolutions

  IMPLICIT NONE

CONTAINS

  !==========================================================================================!
  ! A routine which is called from the SCOTT module to perform the testing
  !==========================================================================================!
  SUBROUTINE runTesting()

!    PRINT *, "Running test: test_ClusterRoutines_recentreCluster"
!    CALL test_ClusterRoutines_recentreCluster

!    PRINT *, "Running test: test_File_outputCAR"
!    CALL test_File_outputCAR

!    PRINT *, "Running test: test_File_outputCAR"
!    CALL test_ClusterRoutines_relaxAtoms()

!    PRINT *, "Running test: initiliase the Grid"
!    CALL test_Grid_initialiseGrid()

!    CALL test_fragmentation()

!    CALL test_ClusterRoutines_dataMine()

!    CALL test_standardiseCoords()

!    CALL test_MOIS()

!    CALL test_dmol6_read_outmol()

!    CALL test_ClusterRoutines_rescaleCluster

!    CALL test_2clusterPMOI()

!    CALL test_crossover()

!    CALL test_diagonalisation()

!    CALL test_hashkeys()

!    CALL test_Duplicates()

!    CALL test_hashkey()

!    CALL test_checkZeroCoordination()

!    CALL temp_getEnergies()

!    CALL readinHashkeyStats()

    CALL testmakeGraphFile()

  END SUBROUTINE runTesting

  !==========================================================================================
  !
  !==========================================================================================
  SUBROUTINE testmakeGraphFile

    TYPE(cluster) :: tempCluster
    CHARACTER(LEN=50) :: fileName
    REAL(KIND=DP) ::hashRadius
    CHARACTER(LEN=N_HASH_LEN) :: hashKey

    fileName = "A11"
    print *, fileName
    CALL readXYZ(tempCluster, fileName)

    hashRadius = getHashkeyRadius(tempCluster, N_ATOMS)

    PRINT *, "Hashkey radius: ", hashRadius

    hashKey = getHashKey(tempCluster, hashRadius)

    PRINT *, "Hashkey: ", hashKey

    hashKey = getHashKeyDreadnaut(tempCluster, hashRadius)

    PRINT *, "Hashkey: ", hashKey

  END SUBROUTINE testmakeGraphFile

  !==========================================================================================
  !
  !==========================================================================================
  SUBROUTINE readinHashkeyStats()

    INTEGER, ALLOCATABLE, DIMENSION(:) :: ssPrevHashkeysOccur

    CALL SSInitialize(ssPrevHashkeysOccur)

    DEALLOCATE(ssPrevHashkeysOccur)

    CALL SSFinilize()

  END SUBROUTINE readinHashkeyStats

  !==========================================================================================
  !
  !==========================================================================================
  SUBROUTINE temp_getEnergies()
    CHARACTER(LEN=100), DIMENSION(100) :: files
    INTEGER :: i, filesCnt
    CHARACTER(LEN=6) :: prefix
    TYPE(cluster) :: tempCluster
    INTEGER :: ierr

    CALL chdir("runx/")

    files = ''
    filesCnt = 0
    prefix = "outmol"

!    CALL getListOfFiles(files, filesCnt, prefix)
!
!    DO i = 1, filesCnt
!
!
!      CALL readOutMolError("./", files(i), ierr)
!
!      IF (ierr .NE. 0) THEN
!        PRINT *, i, ',', TRIM(files(i)), ',', 99999.99
!      ELSE
!        CALL readOutMol(tempCluster, files(i))
!        PRINT *, i, ',', TRIM(files(i)), ',', tempCluster%energy(0)
!      ENDIF
!    ENDDO

  END SUBROUTINE temp_getEnergies

  !==========================================================================================
  !
  !==========================================================================================
  SUBROUTINE test_Duplicates()

    CHARACTER(LEN=100), DIMENSION(100) :: files
    TYPE(cluster), DIMENSION(100) :: clusters
    INTEGER :: filesCnt, i, j
    CHARACTER(LEN=3) :: prefix
    TYPE(cluster) :: tempCluster
    REAL(KIND=DP) :: hashRadius
    CHARACTER(LEN=N_HASH_LEN) :: hashKey

    files = ''
    filesCnt = 0
    prefix = "car"

!    CALL getListOfFiles(files, filesCnt, prefix)
!
!    DO i = 1, filesCnt
!      CALL readCAR(tempCluster, files(i))
!
!      clusters(i) = tempCluster
!
!      hashRadius = getHashkeyRadius(clusters(i), N_ATOMS)
!
!      hashKey = getHashKey(clusters(i), hashRadius)
!
!      CALL getClusterPMOI(clusters(i))
!
!      PRINT *, "Hashkeys ::", i, TRIM(files(i)), " ", TRIM(hashKey), clusters(i)%pmoi
!    ENDDO
!
!    DO i = 1, filesCnt
!      DO j = i+1, filesCnt
!        PRINT *, i, j, comparePMOI(clusters(i), clusters(j))
!      ENDDO
!    ENDDO

  END SUBROUTINE test_Duplicates

  !==========================================================================================
  !
  !==========================================================================================
  SUBROUTINE test_hashkeys()
    TYPE(cluster) :: tempCluster1, tempCLuster2
    CHARACTER(LEN=50) :: fileName
    REAL(KIND=DP) ::hashRadius
    CHARACTER(LEN=N_HASH_LEN) :: hashKey1, hashKey2

    fileName = "A11"
    print *, fileName
    CALL readXYZ(tempCluster1, fileName)

    hashRadius = getHashkeyRadius(tempCluster1, N_ATOMS)

    PRINT *, "Hashkey radius: ", hashRadius

    hashKey1 = getHashKey(tempCluster1, hashRadius)

    PRINT *, "Hashkey: ", hashKey1

    CALL getClusterPMOI(tempCluster1)

    fileName = "A12"
    print *, fileName
    CALL readXYZ(tempCluster2, fileName)

    hashRadius = getHashkeyRadius(tempCluster2, N_ATOMS)

    PRINT *, "Hashkey radius: ", hashRadius

    hashKey2 = getHashKey(tempCluster2, hashRadius)

    PRINT *, "Hashkey: ", hashKey2

    CALL getClusterPMOI(tempCluster2)

    PRINT *, comparePMOI(tempCluster1, tempCluster2)


  END SUBROUTINE test_hashkeys

!  !==========================================================================================
!  !
!  !==========================================================================================
!  SUBROUTINE test_diagonalisation
!    TYPE(cluster) :: tempCluster1, tempCLuster2, tempCluster
!    CHARACTER(LEN=50) :: fileName
!
!    REAL(KIND=DP), DIMENSION(3, 3) :: A_INI, A, ROTEVV
!    REAL(KIND=DP), DIMENSION(3) :: ROTEV
!    REAL(KIND=DP) :: DET
!
!
!    REAL(KIND=DP) :: tolerance
!    fileName = "A478_1"
!    print *, fileName
!    CALL readCAR(tempCluster, fileName)
!
!    CALL moveToCOM(tempCluster)
!
!    ! Find the moments of inertia
!    A_INI = momentsOfInertia(tempCluster)
!
!    A = A_INI
!
!    PRINT *, "A_INI", A_INI
!
!    PRINT *, "A(1,1)", A(1,1), "A(1,2)", A(1,2), "A(1,3)", A(1,3)
!    PRINT *, "A(2,1)", A(2,1), "A(2,2)", A(2,2), "A(2,3)", A(2,3)
!    PRINT *, "A(3,1)", A(3,1), "A(3,2)", A(3,2), "A(3,3)", A(3,3)
!
!    ! Get eigenvalues and eigenvectors
!    CALL ZHEEVJ3(A, ROTEVV, ROTEV)
!
!    DET = M33DET(A)
!
!    PRINT *, "!!!ZHEEVJ3!!"
!
!    PRINT *, "DET", DET
!    PRINT *, "ROTEVV", ROTEVV
!    PRINT *, "ROTEV", ROTEV
!
!    tolerance = 1.0e-09
!
!    A = A_INI
!    PRINT *, "A", A
!    CALL Jacobi(A, ROTEVV, tolerance)
!

!    PRINT *, "!!!JACOBI!!"
!    PRINT *, "A", A
!    PRINT *, "ROTEVV", ROTEVV

!    fileName = "A973"
!    print *, fileName
!    CALL readXYZ(tempCluster1, fileName)
!
!    CALL moveToCOM(tempCluster1)
!
!    CALL getClusterPMOI(tempCluster1)
!
!    ! Find the moments of inertia
!    A = momentsOfInertia(tempCluster1)
!
!    ! Get eigenvalues and eigenvectors
!    CALL ZHEEVJ3(A, ROTEVV, ROTEV)
!
!    DET = M33DET(A)
!
!    PRINT *, "A", A
!    PRINT *, "DET", DET
!    PRINT *, "ROTEVV", ROTEVV
!    PRINT *, "ROTEV", ROTEV
!
!    fileName = "A974"
!    print *, fileName
!    CALL readXYZ(tempCluster2, fileName)
!
!    CALL moveToCOM(tempCluster2)
!
!    CALL getClusterPMOI(tempCluster2)
!
!    ! Find the moments of inertia
!    A = momentsOfInertia(tempCluster2)
!
!    ! Get eigenvalues and eigenvectors
!    CALL ZHEEVJ3(A, ROTEVV, ROTEV)
!
!    DET = M33DET(A)
!
!    PRINT *, "A", A
!    PRINT *, "DET", DET
!    PRINT *, "ROTEVV", ROTEVV
!    PRINT *, "ROTEV", ROTEV
!
!  END SUBROUTINE

  !==========================================================================================
  !
  !==========================================================================================
  SUBROUTINE test_checkZeroCoordination
    TYPE(cluster) :: tempCluster
    CHARACTER(LEN=50) :: fileName
    LOGICAL :: zeroCoordinated

    fileName = "cluster"
    CALL readCAR(tempCluster, fileName)

    zeroCoordinated = checkZeroCoordination(tempCluster, R_FRAGMENT, USE_VARIABLE_RAD_FRAG)

    PRINT *, "Is cluster ", fileName, " zero-coordinated? ", zeroCoordinated

  END SUBROUTINE test_checkZeroCoordination

  !==========================================================================================
  !
  !==========================================================================================
  SUBROUTINE test_crossover
    TYPE(cluster) :: tempCluster1, tempCluster2, newCluster
    CHARACTER(LEN=50) :: fileName

    fileName = "A3839"
    CALL readCAR(tempCluster1, fileName)
    CALL standardiseClusterCoordinates(tempCluster1)
    CALL outputCAR(tempCluster1, "Generated by KLMC", fileName)

    fileName = "A3833"
    CALL readCAR(tempCluster2, fileName)
    CALL standardiseClusterCoordinates(tempCluster2)
    CALL outputCAR(tempCluster2, "Generated by KLMC", fileName)

    newCluster = chromosomalCrossover(tempCluster1, tempCluster2)

    fileName = "crossed"
    CALL outputCAR(newCluster, "Generated by KLMC", fileName)

  END SUBROUTINE test_crossover

  !==========================================================================================
  ! checking the pmoi values of two similar clusters
  !==========================================================================================
  SUBROUTINE test_2clusterPMOI()
    TYPE(cluster) :: tempCluster1, tempCluster2
    CHARACTER(LEN=50) :: fileName
    LOGICAL :: result

    fileName = "A796"
    CALL readCAR(tempCluster1, fileName)
    CALL getClusterPMOI(tempCluster1)
    PRINT *, "TLA DEBUG: ", fileName, " pmoi:", tempCluster1%pmoi

    fileName = "A3837"
    CALL readCAR(tempCluster2, fileName)
    CALL getClusterPMOI(tempCluster2)
    PRINT *, "TLA DEBUG: ", fileName, " pmoi:", tempCluster2%pmoi

    result = comparePMOI(tempCluster1, tempCluster2)

  END SUBROUTINE test_2clusterPMOI

  !==========================================================================================
  ! Testing ClusterRoutines:rescaleCluster
  !==========================================================================================
  SUBROUTINE test_ClusterRoutines_rescaleCluster()
    TYPE(cluster) :: tempCluster
    CHARACTER(LEN=50) :: fileName

    fileName = "cluster"
    CALL readXYZ(tempCluster, fileName)

    CALL rescaleCluster(tempCluster, MUTATE_EXPAND_COEF)

    CALL outputXYZ(tempCluster, "Expanding the cluster  ", fileName // "expended")

    fileName = "cluster"
    CALL readXYZ(tempCluster, fileName)

    CALL rescaleCluster(tempCluster, MUTATE_CONTRACT_COEF)

    CALL outputXYZ(tempCluster, "Contracting the cluster  ", fileName // "contracted")

  END SUBROUTINE test_ClusterRoutines_rescaleCluster

  !==========================================================================================!
  ! Testing reading outmol
  !==========================================================================================!
  SUBROUTINE test_dmol6_read_outmol()
    TYPE(cluster) :: tempCluster
    CHARACTER(LEN=50) :: fileName
    REAL(KIND=DBL) :: Energy
    INTEGER :: ierr

    fileName = "A1"

    print *, "reading outmol"

    CALL readOutMolError(TRIM(WORKING_DIR), TRIM(fileName), ierr)

    PRINT *, "Error?: ", ierr

    CALL readOutMol(tempCluster, fileName)

    PRINT *, "Read in energy using readOutMol: ", tempCluster%energy(tempCluster%edefn)

    PRINT *, "Atoms' positions:"

    CALL peek(tempCluster)

    CALL readOutMolEnergy(WORKING_DIR, fileName, Energy)

    PRINT *, "Read in energy using readOutMolEnergy: ", Energy

  END SUBROUTINE test_dmol6_read_outmol

  !==========================================================================================!
  ! Testing MOIS
  !==========================================================================================!
  SUBROUTINE test_MOIS()
    TYPE(cluster) :: tempCluster1, tempCluster2
    CHARACTER(LEN=50) :: fileName
    CHARACTER(LEN=1) :: fileNo
    LOGICAL :: result

    INTEGER :: i, j

    PRINT *, "Running test: testing MOIS"

    DO i = 1, 9

      WRITE(fileNo, '(I1)') i
      fileName = TRIM(fileNo)

      CALL readXYZ(tempCluster1, fileName)
      tempCluster1%id = fileNo

      !CALL standardiseClusterCoordinates(tempCluster1)
      CALL getClusterPMOI(tempCluster1)

      DO j = i+1, 9

        WRITE(fileNo, '(I1)') j
        fileName = TRIM(fileNo)

        CALL readXYZ(tempCluster2, fileName)
        tempCluster2%id = fileNo

        !CALL standardiseClusterCoordinates(tempCluster2)
        CALL getClusterPMOI(tempCluster2)

        result = comparePMOI(tempCluster1, tempCluster2)
      ENDDO
    ENDDO

  END SUBROUTINE test_MOIS

  !==========================================================================================!
  ! Testing ClusterRoutines:recentreCluster
  !==========================================================================================!
  SUBROUTINE test_ClusterRoutines_dataMine()
    TYPE(cluster) :: tempCluster
    CHARACTER(LEN=50) :: fileName

    PRINT *, "Running test: test_ClusterRoutines_dataMine"

    CALL readInDataMiningConfiguration()

    fileName = "cluster"

    CALL readXYZ(tempCluster, fileName)

    CALL dataMineCluster(tempCluster)

    CALL outputXYZ(tempCluster, "Testing datamining  ", fileName // "_mined")

  END SUBROUTINE test_ClusterRoutines_dataMine

  !==========================================================================================!
  ! Testing ClusterRoutines:recentreCluster
  !==========================================================================================!
  SUBROUTINE test_ClusterRoutines_recentreCluster()

    TYPE(cluster) :: tempCluster
    CHARACTER(LEN=50) :: fileName

    fileName = "cluster"

    CALL readXYZ(tempCluster, fileName)

    CALL recentreCluster(tempCluster)

  END SUBROUTINE test_ClusterRoutines_recentreCluster

  !==========================================================================================!
  ! Testing ClusterRoutines:relaxAtoms
  !==========================================================================================!
  SUBROUTINE test_ClusterRoutines_relaxAtoms()
    CHARACTER(LEN=50) :: fileName

    TYPE(cluster), DIMENSION(1) :: clusters

    fileName = "cluster"

    CALL readXYZ(clusters(1), fileName)

    clusters(:)%edefn=1
    clusters(:)%id(1:1)="T"

    CALL relaxAtoms(clusters)

  END SUBROUTINE test_ClusterRoutines_relaxAtoms

  !==========================================================================================!
  ! Testing file:outputCAR
  !==========================================================================================!
  SUBROUTINE test_File_outputCAR()

    TYPE(cluster) :: tempCluster
    CHARACTER(LEN=100) :: fileName, filePath

    fileName = "cluster"

    filePath = TRIM(RELAXED_FOLDER) // TRIM("IT")

    CALL readXYZ(tempCluster, fileName)

    CALL recentreCluster(tempCluster)

    CALL outputCAR(tempCluster, "Generated by KLMC", filePath, .TRUE.)

  END SUBROUTINE test_File_outputCAR

  !==========================================================================================!
  ! Testing file:fragmentation
  !==========================================================================================!

  SUBROUTINE test_fragmentation()

    TYPE(cluster) :: test_cluster
    CHARACTER(LEN=100) :: fileName
    LOGICAL :: fragMattRes, fragOldRes
    !CHARACTER(LEN=2) :: specieA, specieB

    INTEGER :: nSpecies
    REAL(KIND=DBL) :: R_FRAG = 2.50000!, radius

    PRINT *, "Running test: test_fragmentation"


    CALL initiliaseSpecieArrays()

    ! reading in a car file (cluster to be analysed)
    fileName = "A3129-01"

    CALL readCAR(test_cluster, fileName)

    fragMattRes = fragMatt(test_cluster, R_FRAG)
    fragOldRes = fragmented_old(test_cluster, R_FRAG)

    PRINT *, "Is the cluster fragmented? "
    PRINT *, "FragMatt: ", fragMattRes
    PRINT *, "FragOld: ", fragOldRes

    nSpecies = computeNspecies()

!    specieA = "Si"
!    specieB = "Si"
!    CALL getCollapsedRadius(specieA, specieB, radius)
!    PRINT *, "The collapse radius: ", specieA, specieB, radius
!
!    specieA = "O"
!    specieB = "O"
!    CALL getCollapsedRadius(specieA, specieB, radius)
!    PRINT *, "The collapse radius: ", specieA, specieB, radius
!
!    specieA = "Si"
!    specieB = "O"
!    CALL getCollapsedRadius(specieA, specieB, radius)
!    PRINT *, "The collapse radius: ", specieA, specieB, radius
!
!    specieA = "O"
!    specieB = "Si"
!    CALL getCollapsedRadius(specieA, specieB, radius)
!    PRINT *, "The collapse radius: ", specieA, specieB, radius
!
!
!    specieA = "Si"
!    specieB = "Si"
!    CALL getFragmentRadius(specieA, specieB, radius)
!    PRINT *, "The fragmented radius: ", specieA, specieB, radius
!
!    specieA = "O"
!    specieB = "O"
!    CALL getFragmentRadius(specieA, specieB, radius)
!    PRINT *, "The fragmented radius: ", specieA, specieB, radius
!
!    specieA = "Si"
!    specieB = "O"
!    CALL getFragmentRadius(specieA, specieB, radius)
!    PRINT *, "The fragmented radius: ", specieA, specieB, radius
!
!    specieA = "O"
!    specieB = "Si"
!    CALL getFragmentRadius(specieA, specieB, radius)
!    PRINT *, "The fragmented radius: ", specieA, specieB, radius

    fragMattRes = fragMatt(test_cluster, R_FRAG, .TRUE.)
    PRINT *, "FragMattExt: ", fragMattRes

    fragOldRes = fragmented_old(test_cluster, R_FRAG, .TRUE.)
    PRINT *, "FragOldExt: ", fragOldRes

    CALL deallocatePairSpecieArrays()

  END SUBROUTINE test_fragmentation

  !==========================================================================================!
  ! Testing Grid:initialiseGrid
  !==========================================================================================!

  SUBROUTINE test_Grid_initialiseGrid()

      CALL initialiseGrid()

  END SUBROUTINE test_Grid_initialiseGrid

  !==========================================================================================!
  ! Testing standartisation of coordinates
  !==========================================================================================!

  SUBROUTINE test_standardiseCoords()
    TYPE(cluster) :: oCluster
    CHARACTER(LEN=200) :: inFilePath, outFilePath
    CHARACTER(LEN=100) :: message=''

    inFilePath = "64"
    outFilePath = "64_standardised"

    CALL readXYZ(oCluster, inFilePath)

    CALL standardiseClusterCoordinates(oCluster)
    CALL outputXYZ(oCluster, message, outFilePath)

    inFilePath = "95"
    outFilePath = "95_standardised"

    CALL readXYZ(oCluster, inFilePath)

    CALL standardiseClusterCoordinates(oCluster)
    CALL outputXYZ(oCluster, message, outFilePath)

  END SUBROUTINE test_standardiseCoords

  !==========================================================================================!
  ! A unit test subroutine to check the hashkey functionality
  !==========================================================================================!
  SUBROUTINE test_hashkey()
    TYPE(cluster) :: oCluster
    CHARACTER(LEN=200) :: filePath
    CHARACTER(LEN=N_HASH_LEN) :: hashKey
    INTEGER :: match, hashArrLen

    CHARACTER(LEN=N_HASH_LEN), ALLOCATABLE, DIMENSION(:) :: testHashkeys

    hashArrLen = 1
    ALLOCATE(testHashkeys(hashArrLen))

    filePath = "A1"

    CALL readCAR(oCluster, filePath)

    hashRadius = getHashkeyRadius(oCluster, N_ATOMS)

    PRINT *, "Hashkey radius: ", hashRadius

    hashKey = getHashKey(oCluster, hashRadius)

    testHashkeys(1) = hashKey

    PRINT *, "Hashkey: ", hashKey

    match = findHashKeyMatch(testHashkeys, hashArrLen, hashKey)

    !PRINT *, "Match? ", match

    DEALLOCATE(testHashkeys)

  END SUBROUTINE test_hashkey

END MODULE Testing
