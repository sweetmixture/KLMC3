MODULE Utilities

    USE Config
    USE Format
    USE Timer

    USE File,        ONLY : outputXYZ, outputXYZ_SS, readARC, readXYZ
    USE Atoms,       ONLY : getAtomProperty, lookForAtom
    USE Algebra

    IMPLICIT NONE

CONTAINS

  !==========================================================================================!
  ! Writes out statistics for duplicates for every cluster.
  !==========================================================================================!
  SUBROUTINE addStructureToHashKeyLib(in_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster

    INTEGER :: out_error = 1, out_unit = 10
    CHARACTER(LEN=200) :: strLine
    TYPE(GA_STATS) :: cGaStat
    CHARACTER(LEN=1) :: comma = ','

    cGaStat = in_cluster%ga_stats


    OPEN(UNIT=out_unit, FILE=TRIM(WORKING_DIR)//TRIM(HKG_STATS_FOLDER)//TRIM(in_cluster%hashkey)//'.tla', &
         POSITION='APPEND', ACTION='WRITE', IOSTAT=out_error, IOMSG=ERROR_MSG)

    IF(out_error == 0) THEN

      WRITE(strLine, *) TRIM(in_cluster%id), comma, in_cluster%edefn, comma, in_cluster%energy(in_cluster%edefn), &
        comma, cGaStat%gaOrigin, comma, cGaStat%gaRandomAttempts, comma, cGaStat%gaCrossAttempts, comma, &
        cGaStat%gaCrossParent1, comma, cGaStat%gaCrossParent2, comma, cGaStat%gaMutateAttempts, comma, &
        cGaStat%gaMutateSwapCnt, comma, cGaStat%gaMutateDisplCnt, comma, cGaStat%gaRepopAttempts, comma, cGaStat%gaGenNo

      WRITE(out_unit,*) TRIM(strLine)

      CLOSE(UNIT=out_unit)
    ENDIF

  END SUBROUTINE addStructureToHashKeyLib

  !==========================================================================================!
  ! Wipes the statistics for a GA generation
  !==========================================================================================!
  SUBROUTINE wipeGAStats(inout_clusters)
    TYPE(cluster), INTENT(INOUT), DIMENSION(:) :: inout_clusters

    INTEGER :: n, i

    n = SIZE(inout_clusters)

    DO i = 1, n
      CALL wipeGAStatsSingle(inout_clusters(i))
    ENDDO

    RETURN

  END SUBROUTINE wipeGAStats

  !==========================================================================================!
  ! Wipes the statistics for a cluster
  !==========================================================================================!
  SUBROUTINE wipeGAStatsSingle(inout_cluster)
    TYPE(cluster), INTENT(INOUT):: inout_cluster

    TYPE(GA_STATS) :: cGaStat

    cGaStat = GA_STATS()
    inout_cluster%ga_stats = cGaStat

    RETURN

  END SUBROUTINE wipeGAStatsSingle

  !==========================================================================================!
  ! Gets the specie index
  !==========================================================================================!
  INTEGER FUNCTION getMasterSpecieIdx(specie)
    CHARACTER(LEN=2), INTENT(IN) :: specie

    INTEGER :: i, specieIdx
    specieIdx = 0

    DO i = 1, MAX_SPECIES
      IF (specie .EQ. MASTER_SPECIES(i)) THEN
        specieIdx = i
      ENDIF
    ENDDO

    getMasterSpecieIdx = specieIdx

  END FUNCTION getMasterSpecieIdx

  !==========================================================================================!
  ! A routine to return cluster's centre of a mass
  !==========================================================================================!
  FUNCTION getCentreOfMass(in_cluster)

    REAL(KIND=DP), DIMENSION(1:3) :: getCentreOfMass
    TYPE(cluster),   INTENT(IN) :: in_cluster

    REAL(KIND=DP) :: atomMass, sumx, sumy, sumz, sumMass
    INTEGER       :: i
    CHARACTER(LEN=2) :: prop = 'AM'

    sumx = 0
    sumy = 0
    sumz = 0
    sumMass = 0

    DO i = 1, N_ATOMS
      atomMass = getAtomProperty(in_cluster%atoms(i)%symbol, prop)

      sumMass = sumMass + atomMass

      sumx = sumx+in_cluster%atoms(i)%xc*atomMass
      sumy = sumy+in_cluster%atoms(i)%yc*atomMass
      sumz = sumz+in_cluster%atoms(i)%zc*atomMass

    END DO

    getCentreOfMass(1) = sumx/sumMass
    getCentreOfMass(2) = sumy/sumMass
    getCentreOfMass(3) = sumz/sumMass

  END FUNCTION

  !==========================================================================================!
  ! A fucntion to obtain cluster number from its ID
  !==========================================================================================!
  INTEGER FUNCTION getClusterNumber(inout_cluster)

    TYPE(cluster),   INTENT(INOUT) :: inout_cluster

    CHARACTER(LEN=9) :: tmpStr

    tmpStr = inout_cluster%id(2:10)

    getClusterNumber = strToInt(tmpStr)

  END FUNCTION getClusterNumber

  !==========================================================================================!
  ! A subroutine to estimate cluster's principal moments of inertia
  !==========================================================================================!
  SUBROUTINE getClusterPMOI(inout_cluster)
    TYPE(cluster),   INTENT(INOUT) :: inout_cluster

    CALL getClusterPMO(inout_cluster, 0)

  END SUBROUTINE getClusterPMOI

  !==========================================================================================!
  ! A subroutine to estimate cluster's principal moments of geometry
  !==========================================================================================!
  SUBROUTINE getClusterPMOG(inout_cluster)
    TYPE(cluster),   INTENT(INOUT) :: inout_cluster

    CALL getClusterPMO(inout_cluster, 1)

  END SUBROUTINE getClusterPMOG

  !==========================================================================================!
  ! A subroutine to estimate cluster's principal moments of (inertia/geometry)
  !==========================================================================================!
  SUBROUTINE getClusterPMO(inout_cluster, typeOfMoments)
    TYPE(cluster),   INTENT(INOUT) :: inout_cluster
    INTEGER, INTENT(IN) :: typeOfMoments

    REAL(KIND=DP), DIMENSION(3, 3) :: A, ROTEVV
    REAL(KIND=DP), DIMENSION(3) :: ROTEV

    ! Find the moments
    IF (typeOfMoments .EQ. 0) THEN
      A = momentsOfInertia(inout_cluster)
    ELSE
      A = momentsOfInertia(inout_cluster, .FALSE.)
    ENDIF

    ! Get eigenvalues and eigenvectors
    CALL ZHEEVJ3(A, ROTEVV, ROTEV)

    ! Sorting in an ascending order
    CALL sort3ElementArraySimple(ROTEV)

    ! update cluster's pmoi
    inout_cluster%pmog(:) = ROTEV(:)

  END SUBROUTINE getClusterPMO

  !==========================================================================================!
  ! A function to estimate the squared separation between two atoms
  !==========================================================================================!
  REAL(KIND=DP) FUNCTION getClusterAtomsSepSq(in_cluster, atomI, atomJ)
    TYPE(cluster), INTENT(IN) :: in_cluster
    INTEGER,       INTENT(IN) :: atomI, atomJ

    REAL(KIND=DP) :: dx2, dy2, dz2, dist2

    dx2 = (in_cluster%atoms(atomI)%xc - in_cluster%atoms(atomJ)%xc)**2
    dy2 = (in_cluster%atoms(atomI)%yc - in_cluster%atoms(atomJ)%yc)**2
    dz2 = (in_cluster%atoms(atomI)%zc - in_cluster%atoms(atomJ)%zc)**2
    dist2 = dx2 + dy2 + dz2

    getClusterAtomsSepSq = dist2

  END FUNCTION getClusterAtomsSepSq

  !==========================================================================================!
  ! A routine to read in data mining atom lists
  !==========================================================================================!
  SUBROUTINE getDataMiningAtomList()

    INTEGER :: tempIdx, prevIdx, tempIdx2, cntDMPairs
    LOGICAL :: contLook = .TRUE.
    CHARACTER(LEN=100) :: atomList, atomPair
    CHARACTER(LEN=2) :: atom1, atom2

    tempIdx = 0
    prevIdx = 0
    cntDMPairs = 0

    atomList = DM_REPLACE_ATOMS

    DO WHILE (contLook)
      prevIdx = prevIdx + 1

      atomList = atomList(prevIdx:)

      tempIdx = INDEX(atomList, DM_ATOMS_SEP_SYM)

      IF (tempIdx .LE. 0) THEN
        contLook = .FALSE.
        atomPair = TRIM(atomList)
      ELSE
        atomPair = TRIM(atomList(:tempIdx-1))
      ENDIF

      tempIdx2 = INDEX(atomPair, DM_ATOMS_CHNG_SYM)

      IF (tempIdx2 .LE. 0) THEN
        WRITE(stderr,*) 'Strange formatting in DM_REPLACE_ATOMS: ',TRIM(DM_REPLACE_ATOMS)
        EXIT
      ELSE
        atom1 = TRIM(atomPair(:tempIdx2-1))
        atom2 = TRIM(atomPair(tempIdx2+1:))

        IF (.NOT. lookForAtom(atom1) .OR. .NOT. lookForAtom(atom1)) THEN
          WRITE(stderr,*) 'Unidentified atom symbols in DM_REPLACE_ATOMS: ',TRIM(DM_REPLACE_ATOMS)
          EXIT
        ENDIF

        cntDMPairs = cntDMPairs + 1

        DM_ATOMS_FROM(cntDMPairs) = TRIM(atom1)
        DM_ATOMS_TO(cntDMPairs)   = TRIM(atom2)

      ENDIF

      prevIdx = tempIdx
    END DO

    DM_ATOMS_SWAP_CNT = cntDMPairs

  END SUBROUTINE getDataMiningAtomList

  !==========================================================================================!
  ! A routine to estimate datamining distance coefficient
  !==========================================================================================!
  SUBROUTINE getDataMiningCoef()

    REAL(KIND=DBL) :: fromIonicRad, toIonicRad, coef

    IF ((DM_ATOMS_SWAP_CNT .EQ. 0) .OR. (DM_ATOMS_SWAP_CNT .GT. 2)) THEN
      WRITE(stderr,*) 'Check the number of atom pairs in DM_REPLACE_ATOMS: ',TRIM(DM_REPLACE_ATOMS)
      RETURN
    ENDIF

    IF (DM_ATOMS_SWAP_CNT .EQ. 1) THEN

      fromIonicRad = getAtomProperty(DM_ATOMS_FROM(1), 'IR')
      toIonicRad   = getAtomProperty(DM_ATOMS_TO(1), 'IR')

    ELSE IF (DM_ATOMS_SWAP_CNT .EQ. 2) THEN

      fromIonicRad = getAtomProperty(DM_ATOMS_FROM(1), 'IR') + getAtomProperty(DM_ATOMS_FROM(2), 'IR')
      toIonicRad   = getAtomProperty(DM_ATOMS_TO(1), 'IR') + getAtomProperty(DM_ATOMS_TO(2), 'IR')

    ELSE
      fromIonicRad = 1.0
      toIonicRad = 1.0
    ENDIF

    coef = toIonicRad / fromIonicRad

    DM_COORD_COEF = coef

  END SUBROUTINE getDataMiningCoef

  !==========================================================================================!
  ! A function to look for a hashkey in a hashkey array
  !==========================================================================================!
  INTEGER FUNCTION findHashKeyMatch(HASHKEYS, lenHashkeys, hashkey)
    CHARACTER(LEN=N_HASH_LEN), ALLOCATABLE, DIMENSION(:), INTENT(IN) :: HASHKEYS
    INTEGER, INTENT(IN) :: lenHashkeys
    CHARACTER(LEN=N_HASH_LEN), INTENT(IN) :: hashkey

    INTEGER :: i

    findHashKeyMatch = -1

    IF (TRIM(hashkey) .EQ. TRIM(UNDEFINED_HASHKEY)) THEN
      RETURN
    ENDIF

    DO i = lenHashkeys, 1, -1
      !PRINT *, "xx", hashkey, i, HASHKEYS(i)
      IF (hashkey == HASHKEYS(i)) THEN
        findHashKeyMatch = i
        EXIT
      ENDIF
    ENDDO

  END FUNCTION findHashKeyMatch

  !==========================================================================================!
  ! A function to look for energy match
  !==========================================================================================!
  INTEGER FUNCTION findEnergyMatch(ide, energy)
    INTEGER         , INTENT(IN) :: ide
    REAL(KIND=DBL)  , INTENT(IN) :: energy

    INTEGER :: I, IDX = -1

    DO I = INT_MAX_BEST,1,-1

      IF (IDE_BEST(I) < ide) THEN
        IDX = I
      ELSEIF (IDE_BEST(I) == ide) THEN
        IF (R_BEST_ENERGIES(I) > energy) IDX = I
      ENDIF
    ENDDO

    IF (IDE_BEST(IDX-1) == ide .AND. ABS(R_BEST_ENERGIES(IDX-1)-energy)<R_BEST_EDIF) THEN
      findEnergyMatch = IDX-1
    ELSE
      findEnergyMatch = -1
    ENDIF

    RETURN
  END FUNCTION findEnergyMatch

  !==========================================================================================!
  ! Estimates the matrix of the principal moments of inertia
  !==========================================================================================!
  FUNCTION momentsOfInertia(in_cluster, noMasses)
    REAL(KIND=DP), DIMENSION(3,3) :: momentsOfInertia
    TYPE(cluster), INTENT(IN) ::in_cluster
    LOGICAL, INTENT(IN), OPTIONAL :: noMasses

    REAL(KIND=DP), DIMENSION(1:6) :: MOI
    REAL(KIND=DP), DIMENSION(3,3) :: A

    INTEGER :: i
    REAL(KIND=DP) :: atomMass
    CHARACTER(LEN=2) :: prop = 'AM'
    LOGICAL :: includeMasses


    includeMasses = .TRUE.
    MOI(:) = 0.00_dp

    IF (PRESENT(noMasses)) THEN
      IF (noMasses) THEN
        includeMasses = .FALSE.
      ENDIF
    ENDIF

    !Ixx=y^2+z^2;Iyy=x^2+z^2;Izz=x^2+y^2
    !Ixy=Iyx=xy ;Ixz=Izx=xz ;Iyz=Izy=yz

    !Ixx = MIO(1); Iyy=MOI(2);Izz=MOI(3);Ixy=-MOI(4);Ixz=-MOI(5);Iyz=-MOI(6)

    IF (includeMasses) THEN
      ! with masses
      DO i = 1, N_ATOMS
        atomMass = getAtomProperty(in_cluster%atoms(i)%symbol, prop)

        MOI(1) = MOI(1) + ( in_cluster%atoms(i)%yc**2 + in_cluster%atoms(i)%zc**2) * atomMass
        MOI(2) = MOI(2) + ( in_cluster%atoms(i)%xc**2 + in_cluster%atoms(i)%zc**2) * atomMass
        MOI(3) = MOI(3) + ( in_cluster%atoms(i)%xc**2 + in_cluster%atoms(i)%yc**2) * atomMass

        MOI(4) = MOI(4) + (-1.0*(in_cluster%atoms(i)%xc * in_cluster%atoms(i)%yc)) * atomMass
        MOI(5) = MOI(5) + (-1.0*(in_cluster%atoms(i)%xc * in_cluster%atoms(i)%zc)) * atomMass
        MOI(6) = MOI(6) + (-1.0*(in_cluster%atoms(i)%yc * in_cluster%atoms(i)%zc)) * atomMass
      END DO
    ELSE
      ! without masses
      DO i = 1, N_ATOMS
        MOI(1) = MOI(1) + ( in_cluster%atoms(i)%yc**2 + in_cluster%atoms(i)%zc**2)
        MOI(2) = MOI(2) + ( in_cluster%atoms(i)%xc**2 + in_cluster%atoms(i)%zc**2)
        MOI(3) = MOI(3) + ( in_cluster%atoms(i)%xc**2 + in_cluster%atoms(i)%yc**2)

        MOI(4) = MOI(4) - (in_cluster%atoms(i)%xc * in_cluster%atoms(i)%yc)
        MOI(5) = MOI(5) - (in_cluster%atoms(i)%xc * in_cluster%atoms(i)%zc)
        MOI(6) = MOI(6) - (in_cluster%atoms(i)%yc * in_cluster%atoms(i)%zc)
      END DO
    ENDIF

    A(1,1) = MOI(1)
    A(2,2) = MOI(2)
    A(3,3) = MOI(3)

    A(1,2) = MOI(4)
    A(1,3) = MOI(5)

    A(2,1) = MOI(4)
    A(2,3) = MOI(6)

    A(3,1) = MOI(5)
    A(3,2) = MOI(6)

    momentsOfInertia = A

    RETURN

  END FUNCTION momentsOfInertia

  !==========================================================================================!
  ! Moves a cluster to its centre of mass
  !==========================================================================================!
  SUBROUTINE moveToCOM(inout_cluster, fixedSize)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    INTEGER, INTENT(IN), OPTIONAL :: fixedSize

    REAL(KIND=DP),DIMENSION(1:3)  :: com
    INTEGER :: i, n

    IF (PRESENT(fixedSize)) THEN
      n = fixedSize
    ELSE
      n = N_ATOMS
    ENDIF

    com = getCentreOfMass(inout_cluster)

    DO i = 1, n
      inout_cluster%atoms(i)%xc = inout_cluster%atoms(i)%xc - com(1)
      inout_cluster%atoms(i)%yc = inout_cluster%atoms(i)%yc - com(2)
      inout_cluster%atoms(i)%zc = inout_cluster%atoms(i)%zc - com(3)
    END DO

  END SUBROUTINE

  !==========================================================================================!
  ! Multiply 2 3x3 matrices. matrix1 x matrix2. Matrix1 is the product
  !==========================================================================================!
  SUBROUTINE multiplyMatrices(matrix1, matrix2)

    REAL(KIND=DP), DIMENSION(3,3), INTENT(INOUT) :: matrix1
    REAL(KIND=DP), DIMENSION(3,3), INTENT(IN) :: matrix2

    REAL(KIND=DP), DIMENSION(3,3) :: result

    INTEGER :: i, j, k

    DO i = 1, 3
      DO j = 1, 3
        result(i, j) = 0.0
      ENDDO
    ENDDO

    DO i = 1, 3
      DO j = 1, 3
        DO k = 1, 3
          result(i, j) = result(i, j) + matrix1(i, k) * matrix2(k, j)
        ENDDO
      ENDDO
    ENDDO

    matrix1 = result

  END SUBROUTINE multiplyMatrices

  !==========================================================================================!
  ! A function to check whether a cluster is in black or best hashkeys lists?
  !==========================================================================================!
  SUBROUTINE lookForTopologicalMatch(inout_cluster)

    TYPE(cluster), INTENT(INOUT) :: inout_cluster

    INTEGER :: hashkeyMatch, blackHashKeyMatch!, ide
    CHARACTER(LEN=N_HASH_LEN) :: hashkey
    LOGICAL :: hashkeyMatchFound

    inout_cluster%hashkeyMatchIter = 0
    inout_cluster%hashkeyMatchNo = 0

    hashkey = inout_cluster%hashkey

    ! Checking if the hashkey matches one of the top structure's hashkey
    hashkeyMatch = findHashKeyMatch(BEST_HASHKEYS, INT_MAX_BEST, hashkey)

    ! Checking if the hashkey matches one of the blacklisted hashkeys
    IF (hashkeyMatch <= 0) THEN
      blackHashKeyMatch = findHashKeyMatch(BLACKLIST_HASHKEYS, BLACKLIST_HASHKEYS_CNT, hashkey)
    ENDIF

    IF ((hashkeyMatch > 0) .OR. (blackHashKeyMatch > 0)) THEN
      hashkeyMatchFound = .TRUE.
    ENDIF

    ! If a match of the hashkey has been found, do not do the consecutive relaxAtoms!
    IF (hashkeyMatchFound) THEN

      IF (hashkeyMatch > 0) THEN
        inout_cluster%hashkeyMatchNo = hashkeyMatch

      ELSE IF (blackHashKeyMatch > 0) THEN
        inout_cluster%hashkeyMatchNo = -1
      ENDIF

      ! Saving at which relaxation stage the hashkey match was found
      !inout_cluster%hashkeyMatchIter = ide
    ENDIF

  END SUBROUTINE lookForTopologicalMatch

  !==========================================================================================!
  ! Routine to add a hashkey to a blacklist
  !==========================================================================================!
  SUBROUTINE addHashkeyToBlacklist(hashkey)
    CHARACTER(LEN=N_HASH_LEN), INTENT(IN) :: hashkey

    IF (.NOT. checkHashkeyBlacklist(hashkey)) THEN
      IF (BLACKLIST_HASHKEYS_CNT >= MAX_BLACKLIST_HASHKEYS_CNT) THEN
         WRITE(*,*) "BLACKLIST_HASHKEYS HAS REACHED ITS MAXIMUM CAPACITY!"
      ELSE
        BLACKLIST_HASHKEYS_CNT = BLACKLIST_HASHKEYS_CNT + 1
        BLACKLIST_HASHKEYS(BLACKLIST_HASHKEYS_CNT) = hashkey
      ENDIF
    ENDIF

  END SUBROUTINE addHashkeyToBlacklist

  !==========================================================================================!
  ! Function to check whether a hashkey is blacklisted
  !==========================================================================================!
  LOGICAL FUNCTION checkHashkeyBlacklist(hashkey)
    CHARACTER(LEN=N_HASH_LEN), INTENT(IN) :: hashkey

    INTEGER :: i

    DO i = 1, BLACKLIST_HASHKEYS_CNT
      IF (BLACKLIST_HASHKEYS(i) == hashkey) THEN
        checkHashkeyBlacklist = .TRUE.
        RETURN
      ENDIF
    ENDDO

    checkHashkeyBlacklist = .FALSE.

  END FUNCTION checkHashkeyBlacklist

  !==========================================================================================!
  ! A function to check if two hashkeys are equal
  !==========================================================================================!
  LOGICAL FUNCTION compareHashKeys(in_cluster1, in_cluster2, checkInitialHashKey)
    TYPE(cluster), INTENT(IN) :: in_cluster1, in_cluster2
    LOGICAL, INTENT(IN) :: checkInitialHashKey

    CHARACTER(LEN=N_HASH_LEN) :: hashkey1, hashkey2

    IF (.NOT. L_USE_TOP_ANALYSIS) THEN
      compareHashKeys = .FALSE.
      RETURN
    ENDIF

    IF (checkInitialHashKey) THEN
      hashkey1 = in_cluster1%hashkey1st
      hashkey2 = in_cluster2%hashkey1st
    ELSE
      hashkey1 = in_cluster1%hashkey
      hashkey2 = in_cluster2%hashkey
    ENDIF

    IF ((TRIM(hashkey1) .EQ. UNDEFINED_HASHKEY) .OR. (TRIM(hashkey2) .EQ. UNDEFINED_HASHKEY)) THEN
      compareHashKeys = .FALSE.
      RETURN
    ENDIF

    IF (TRIM(hashkey1) .EQ. TRIM(hashkey2)) THEN
      compareHashKeys = .TRUE.
    ELSE
      compareHashKeys = .FALSE.
    ENDIF

    RETURN
  END FUNCTION compareHashKeys

  !==========================================================================================!
  ! A procedure to compare principal moments of inertia of two clusters
  !==========================================================================================!
  LOGICAL FUNCTION comparePMOI(in_cluster1, in_cluster2)
    TYPE(cluster), INTENT(IN) :: in_cluster1, in_cluster2

    REAL(KIND=DP), DIMENSION(3) :: ev1, ev2
    INTEGER :: i
    REAL(KIND=DP) :: ev1Sum, ev2Sum, diff
    CHARACTER(LEN=200) :: message

    ev1 = in_cluster1%pmoi
    ev2 = in_cluster2%pmoi

    comparePMOI = .FALSE.

    ev1Sum = 0.0
    ev2Sum = 0.0

    DO i = 1, 3
      ev1Sum = ev1Sum + ev1(i)
      ev2Sum = ev2Sum + ev2(i)
    ENDDO

    diff = 0.0
    DO i = 1, 3
      diff = diff + ABS((ev1(i) / ev1Sum) - (ev2(i) / ev2Sum))
    ENDDO

    IF (ABS(diff) .LE. R_PMOI_TOLERANCE) THEN
      comparePMOI = .TRUE.
    ENDIF

!    WRITE(message, *) 'PMOI difference between clusters: ', TRIM(in_cluster1%id), ' and ', &
!      TRIM(in_cluster2%id), ':', diff, '. Are they identical? ', comparePMOI
!
!    CALL printLog("comparePMOI", message, 10)

    RETURN

  END FUNCTION comparePMOI

  !==========================================================================================!
  ! Does what it says
  !==========================================================================================!
  SUBROUTINE debugBanner(str1,str2)
      CHARACTER(LEN=*), INTENT(IN) :: str1
      CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: str2

      IF (DEBUG_LEVEL > 10) THEN
        IF (PRESENT(str2)) THEN
          CALL printBanner(str1,str2)
        ELSE
          CALL printBanner(str1)
        ENDIF
      ENDIF

  END SUBROUTINE debugBanner

  !==========================================================================================!
  ! A function to calculate dimensions of a cluster
  !==========================================================================================!
  INTEGER FUNCTION getDimsOfACluster(in_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    REAL(KIND=DBL) :: x_min, x_max, y_min, y_max, z_min, z_max, dist
    INTEGER :: i, dims

    dims = 0

    x_min =  R_MAX_CLUSTER_BOUNDARY(3)
    x_max = -R_MAX_CLUSTER_BOUNDARY(3)
    y_min = x_min
    y_max = x_max
    z_min = x_min
    z_max = x_max

    DO i = 1, N_ATOMS
      IF (in_cluster%atoms(i)%xc < x_min) x_min = in_cluster%atoms(i)%xc
      IF (in_cluster%atoms(i)%xc > x_max) x_max = in_cluster%atoms(i)%xc

      IF (in_cluster%atoms(i)%yc < y_min) y_min = in_cluster%atoms(i)%yc
      IF (in_cluster%atoms(i)%yc > y_max) y_max = in_cluster%atoms(i)%yc

      IF (in_cluster%atoms(i)%zc < z_min) z_min = in_cluster%atoms(i)%zc
      IF (in_cluster%atoms(i)%zc > z_max) z_max = in_cluster%atoms(i)%zc
    ENDDO

    dist = x_max - x_min
    IF (ABS(dist) .GT. DIM_TOLERANCE) THEN
      dims = dims + 1
    ENDIF

    dist = y_max - y_min
    IF (ABS(dist) .GT. DIM_TOLERANCE) THEN
      dims = dims + 1
    ENDIF

    dist = z_max - z_min
    IF (ABS(dist) .GT. DIM_TOLERANCE) THEN
      dims = dims + 1
    ENDIF

    getDimsOfACluster = dims

  END FUNCTION getDimsOfACluster

  !==========================================================================================!
  ! A function to retrieve a hashkey for a cluster by system calling the python/Nauty script.
  !==========================================================================================!
  CHARACTER(LEN=N_HASH_LEN) FUNCTION getHashKey(in_cluster, radius)
    TYPE(cluster), INTENT(IN) :: in_cluster
    REAL(KIND=DBL),   INTENT(IN) :: radius

    getHashKey = getHashKeyDreadnaut(in_cluster, radius)

    !getHashKey = getHashKeyHKG(in_cluster, radius)

  END FUNCTION getHashKey

  !==========================================================================================!
  ! A function to retrieve a hashkey for a cluster by creating a graph and system calling the Dreadnaut.
  !==========================================================================================!
  CHARACTER(LEN=N_HASH_LEN) FUNCTION getHashKeyDreadnaut(in_cluster, radius)
    TYPE(cluster),  INTENT(IN) :: in_cluster
    REAL(KIND=DBL), INTENT(IN) :: radius


    CHARACTER(LEN=60)  :: graphFilename = "hashkeyDread.graph", hashFilename = "hashkeyDread.tmp"
    CHARACTER(LEN=200) :: systemCmd, hkgOptions, message
    CHARACTER(LEN=15)  :: lenStr

    TYPE(cluster) :: modi_ss_cluster

    INTEGER :: n
    REAL(KIND=DP), DIMENSION(2,2) :: transMatrix, transMatrixInv
    REAL(KIND=DBL) :: posx, posy, posz


    IF (JOB_TYPE==3) THEN

      modi_ss_cluster = in_cluster

      CALL transMatrix2D(modi_ss_cluster%box(4), transMatrix)
      CALL inverse2x2Matrix(transMatrix, transMatrixInv)

      DO n = 1, N_ATOMS
        IF (index(in_cluster%atoms(n)%cstype,'x') .EQ. 0) THEN

          ! Converting coordinates
          posx = modi_ss_cluster%atoms(n)%xc*transMatrixInv(1,1) + modi_ss_cluster%atoms(n)%yc*transMatrixInv(2,1)
          posy = modi_ss_cluster%atoms(n)%xc*transMatrixInv(1,2) + modi_ss_cluster%atoms(n)%yc*transMatrixInv(2,2)

          modi_ss_cluster%atoms(n)%xc = posx
          modi_ss_cluster%atoms(n)%yc = posy
        ENDIF
      END DO
    ELSE
      modi_ss_cluster = in_cluster
    ENDIF

    CALL makeGraphFile(modi_ss_cluster, radius, graphFilename)

    hkgOptions = '-e'

    systemCmd = 'python ' // TRIM(HKG_PATH) // ' ' // TRIM(hkgOptions) // ' ' // &
      TRIM(graphFilename) // ' > ' // TRIM(hashFilename)

    CALL SYSTEM(systemCmd)

    ! Retrieve the result
    getHashKeyDreadnaut = readHashKey(hashFilename)

    message = TRIM(in_cluster%id) // " hashkey: " // TRIM(getHashKeyDreadnaut)
    CALL printLog("getHashKeyDreadnaut", message, 10)

    ! Delete the temporary files
    systemCmd = 'rm -rf ' // TRIM(graphFilename) // ' ' // TRIM(hashFilename)

    CALL SYSTEM(systemCmd)

  END FUNCTION getHashKeyDreadnaut

  !==========================================================================================!
  ! A function to retrieve a hashkey for a cluster by system calling the python/Nauty script.
  !==========================================================================================!
  CHARACTER(LEN=N_HASH_LEN) FUNCTION getHashKeyHKG(in_cluster, radius)
    TYPE(cluster), INTENT(IN) :: in_cluster
    REAL(KIND=DBL),   INTENT(IN) :: radius

    CHARACTER(LEN=60)  :: xyzFilename = "hashkeyHKG", hashFilename = "hashkeyHKG.tmp"
    CHARACTER(LEN=200) :: systemCmd, hkgOptions, message2
    CHARACTER(LEN=15)  :: hashRadiusStr, lenStr
    CHARACTER(LEN=20)  :: message

    message = "This is for Nauty!"

    IF (JOB_TYPE==3) THEN
      ! Save cluster into a xyz file apply filters for coordinates
      CALL outputXYZ_SS(in_cluster, message, xyzFilename)
    ELSE
      ! Save cluster into a xyz file
      CALL outputXYZ(in_cluster, message, xyzFilename)
    ENDIF

    ! Call python to execute Nauty
    WRITE(hashRadiusStr, '(f8.5)') radius

    hkgOptions = ''

    ! Should the PBC be applied
    IF (in_cluster%box(1) > epsilon_x) THEN
      WRITE(lenStr, '(f8.5)') in_cluster%box(1)

      hkgOptions = ' -x ' // TRIM(lenStr)
    ENDIF
    IF (in_cluster%box(2) > epsilon_x) THEN
      WRITE(lenStr, '(f8.5)') in_cluster%box(2)

      hkgOptions = TRIM(hkgOptions) // ' -y ' // TRIM(lenStr)
    ENDIF
    IF (in_cluster%box(3) > epsilon_x) THEN
      WRITE(lenStr, '(f8.5)') in_cluster%box(3)

      hkgOptions = TRIM(hkgOptions) // ' -z ' // TRIM(lenStr)
    ENDIF

    systemCmd = 'python ' // TRIM(HKG_PATH) // ' ' // TRIM(hkgOptions) // ' ' // &
      TRIM(xyzFilename) // '.xyz ' // TRIM(hashRadiusStr) // ' > ' // TRIM(hashFilename)

    CALL SYSTEM(systemCmd)

    ! Retrieve the result
    getHashKeyHKG = readHashKey(hashFilename)

    message2 = TRIM(in_cluster%id) // " hashkey: " // TRIM(getHashKeyHKG)
    CALL printLog("getHashKeyHKG", message2, 10)

    ! Delete the temporary files
    systemCmd = 'rm -rf ' // TRIM(xyzFilename) // '.xyz ' // TRIM(hashFilename)

    CALL SYSTEM(systemCmd)

  END FUNCTION getHashKeyHKG

  !==========================================================================================!
  ! A function to estimate a hashkey radius for a cluster.
  !==========================================================================================!
  REAL(KIND=DBL) FUNCTION getHashkeyRadius(in_cluster, nAtomsCluster)

    TYPE(cluster), INTENT(IN) :: in_cluster
    INTEGER, INTENT(IN) :: nAtomsCluster

    CHARACTER(LEN=2), DIMENSION(MAX_ATOMS) :: uAtomsSyms
    CHARACTER(LEN=2) :: aSym
    INTEGER :: uAtomsSymsCnt, i, j
    LOGICAL :: found
    REAL(KIND=DBL) :: maxDist, ar1, ar2, dist

    uAtomsSymsCnt = 0
    maxDist = 0.0

    DO i = 1, nAtomsCluster
      found = .FALSE.
      aSym = in_cluster%atoms(i)%symbol

      DO j = 1, uAtomsSymsCnt
        IF (uAtomsSyms(uAtomsSymsCnt) .eq. aSym) THEN
          found = .TRUE.
          EXIT
        ENDIF
      ENDDO

      IF (.NOT. found) THEN
        uAtomsSymsCnt = uAtomsSymsCnt + 1
        uAtomsSyms(uAtomsSymsCnt) = aSym
      ENDIF
    ENDDO

    DO i = 1, uAtomsSymsCnt
      DO j = 1, uAtomsSymsCnt

        IF (uAtomsSyms(i) .NE. uAtomsSyms(j)) THEN

          ar1 = getAtomProperty(uAtomsSyms(i), C_HASHKEY_RADIUS)
          ar2 = getAtomProperty(uAtomsSyms(j), C_HASHKEY_RADIUS)
          dist = ar1 + ar2 + HASHKEY_RADIUS_CONST

          IF (dist > maxDist) THEN
            maxDist = dist
          ENDIF

        ELSEIF (uAtomsSymsCnt .EQ. 1) THEN
          ar1 = getAtomProperty(uAtomsSyms(i), C_HASHKEY_RADIUS)
          ar2 = getAtomProperty(uAtomsSyms(j), C_HASHKEY_RADIUS)
          maxDist = ar1 + ar2 + HASHKEY_RADIUS_CONST
        ENDIF
      END DO
    END DO

    getHashkeyRadius = maxDist

  END FUNCTION getHashkeyRadius

  !==========================================================================================!
  ! A routine to generate a graph file from a cluster
  !==========================================================================================!
  SUBROUTINE makeGraphFile(in_cluster, radius, graphFilename)

    TYPE(cluster),    INTENT(IN) :: in_cluster
    REAL(KIND=DBL),   INTENT(IN) :: radius
    CHARACTER(LEN=*), INTENT(IN) :: graphFilename

    REAL(KIND=DBL) :: radiusSq, sepSq
    INTEGER :: i, j, specieIdx, masterSpecieCnt
    CHARACTER(LEN=300) :: line
    INTEGER, DIMENSION(N_ATOMS, MAX_SPECIES) :: graphColors
    INTEGER, DIMENSION(MAX_SPECIES) :: graphColorsCount
    INTEGER, DIMENSION(N_ATOMS) :: newIdexes

    INTEGER :: out_error=1, out_unit=31, N_ATOMS_FILTER, rmCnt

    masterSpecieCnt = LEN(MASTER_SPECIES)

    OPEN(UNIT=out_unit, FILE=TRIM(graphFilename), STATUS='REPLACE', ACTION='WRITE', &
      IOSTAT=out_error, IOMSG=ERROR_MSG)

    rmCnt = 0
    N_ATOMS_FILTER = N_ATOMS - N_VAC

    IF(out_error == 0) THEN

      graphColorsCount(:) = 0

      radiusSq = radius * radius

      WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) "l=1000"
      WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) "c"
      WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) "n=", TRIM(ADJUSTL(intToStr(N_ATOMS_FILTER))), " g"

      DO i = 1, N_ATOMS
        IF (index(in_cluster%atoms(i)%cstype,'x') .NE. 0) THEN
          rmCnt = rmCnt + 1

          newIdexes(i) = -1
        ELSE
          newIdexes(i) = i - rmCnt - 1
        ENDIF
      ENDDO

      ! Generating connectivity
      DO i = 1, N_ATOMS
        IF (index(in_cluster%atoms(i)%cstype,'x') .NE. 0) THEN
          CYCLE
        ENDIF

        WRITE(line, *) TRIM(ADJUSTL(intToStr(newIdexes(i)))), " : "

        DO j = 1, N_ATOMS
          IF (index(in_cluster%atoms(j)%cstype,'x') .NE. 0) THEN
            CYCLE
          ENDIF

          IF (i .EQ. j) CYCLE

          sepSq = atomicSeparation(in_cluster, i, j)

          IF (sepSq .LE. radiusSq) THEN
            WRITE(line, *) TRIM(ADJUSTL(line)), " ", TRIM(ADJUSTL(intToStr(newIdexes(j))))
          ENDIF
        ENDDO

        IF (i .LT. N_ATOMS) THEN
          WRITE(line, *) TRIM(ADJUSTL(line)), " ;"
        ELSE
          WRITE(line, *) TRIM(ADJUSTL(line)), " ."
        ENDIF

        specieIdx = getMasterSpecieIdx(in_cluster%atoms(i)%symbol)

        graphColorsCount(specieIdx) = graphColorsCount(specieIdx) + 1
        graphColors(graphColorsCount(specieIdx), specieIdx) = newIdexes(i)

        WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) TRIM(ADJUSTL(line))
      ENDDO

      ! Reordering the graph coloring
      CALL sortNElementArray(MAX_SPECIES, masterSpecieCnt, graphColorsCount, N_ATOMS, graphColors)

      ! Generating specie list
      WRITE(line, *) "f=["

      DO i = 1, masterSpecieCnt
        DO j = 1, graphColorsCount(i)
          WRITE(line, *) TRIM(ADJUSTL(line)), TRIM(ADJUSTL(intToStr(graphColors(j, i)))), ","
        ENDDO

        IF (i .LT. masterSpecieCnt) THEN
          WRITE(line, *) TRIM(ADJUSTL(line)), "|"
        ENDIF
      ENDDO

      WRITE(line, *) TRIM(ADJUSTL(line)), "]"

      WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) TRIM(ADJUSTL(line))
      WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) "x"
      WRITE(UNIT=out_unit,FMT=*, IOSTAT=out_error, IOMSG=ERROR_MSG) "z"

      CLOSE(UNIT=out_unit)
    ELSE
      WRITE(*,*) ERROR_MSG
    END IF
  END SUBROUTINE makeGraphFile

  !==========================================================================================!
  ! Evaluates atomic separation between two atoms within a cluster
  !========================================================================================
  REAL(KIND=DBL) FUNCTION atomicSeparation(in_cluster, atomI, atomJ)
    TYPE(cluster),  INTENT(IN) :: in_cluster
    INTEGER, INTENT(IN) :: atomI, atomJ

    REAL(KIND=DBL) :: rx, ry, rz

    rx = in_cluster%atoms(atomI)%xc - in_cluster%atoms(atomJ)%xc
    ry = in_cluster%atoms(atomI)%yc - in_cluster%atoms(atomJ)%yc
    rz = in_cluster%atoms(atomI)%zc - in_cluster%atoms(atomJ)%zc

    IF (in_cluster%box(1) > epsilon_x) THEN
      rx = rx - REAL(ANINT(rx / in_cluster%box(1))) * in_cluster%box(1)
    ENDIF

    IF (in_cluster%box(2) > epsilon_x) THEN
      ry = ry - REAL(ANINT(ry / in_cluster%box(2))) * in_cluster%box(2)
    ENDIF

    IF (in_cluster%box(3) > epsilon_x) THEN
      rz = rz - REAL(ANINT(rz / in_cluster%box(3))) * in_cluster%box(3)
    ENDIF

    atomicSeparation = rx * rx + ry * ry + rz * rz

  END FUNCTION atomicSeparation

  !==========================================================================================!
  ! A routine to a count of files
  !==========================================================================================!
  SUBROUTINE getCountOfFiles(directory, prefix, suffix, filesCnt)
    CHARACTER(LEN=*), INTENT(IN) :: directory
    CHARACTER(LEN=*), INTENT(IN) :: prefix, suffix
    INTEGER, INTENT(OUT) :: filesCnt

    CHARACTER(LEN=100) :: systemCmd, buffer, fileExpr
    CHARACTER(LEN=200) :: message
    INTEGER :: in_error=1, in_unit=26

    filesCnt = 0

    fileExpr = TRIM(prefix) // "*" // TRIM(suffix)

    message = "Counting " // TRIM(fileExpr) // " files in directory: " // TRIM(directory)
    CALL printLog("getCountOfFiles", message, 10)

    systemCmd = "ls -m1 " // TRIM(directory) // " | grep " // TRIM(fileExpr) // " > files.tmp"
    CALL SYSTEM(systemCmd)

    OPEN(UNIT=in_unit,  FILE="files.tmp", STATUS='OLD', ACTION='READ', IOSTAT=in_error)

    IF(in_error == 0) THEN
      REWIND(in_unit)
      DO
        READ(UNIT=in_unit,FMT='(A)', IOSTAT=in_error) buffer
        IF(in_error == 0) THEN
          filesCnt = filesCnt + 1
        ELSE
          EXIT
        END IF
      END DO
      CLOSE(UNIT=in_unit)

      message = "Found " // TRIM(ADJUSTL(intToStr(filesCnt))) // " files."
      CALL printLog("getCountOfFiles", message, 10)
    ENDIF

    systemCmd = "rm -rf files.tmp"
    CALL SYSTEM(systemCmd)

  END SUBROUTINE getCountOfFiles

  !==========================================================================================!
  ! A routine to generate a list of files
  !==========================================================================================!
  SUBROUTINE getListOfFiles(files, directory, prefix, suffix, filesCnt)
    CHARACTER(LEN=*), DIMENSION(:), INTENT(INOUT) :: files
    CHARACTER(LEN=*), INTENT(IN) :: directory
    CHARACTER(LEN=*), INTENT(IN) :: prefix, suffix
    INTEGER, INTENT(OUT) :: filesCnt

    CHARACTER(LEN=100) :: systemCmd, buffer, fileExpr
    CHARACTER(LEN=200) :: message

    INTEGER :: in_error=1, in_unit=26

    filesCnt = 0
    fileExpr = TRIM(prefix) // "*" // TRIM(suffix)

    message = "Looking for " // TRIM(fileExpr) // " files in directory: " // TRIM(directory)
    CALL printLog("getListOfFiles", message, 10)

    systemCmd = "ls -m1 " // TRIM(directory) // " | grep " // TRIM(fileExpr) // " > files.tmp"
    CALL SYSTEM(systemCmd)

    OPEN(UNIT=in_unit,  FILE="files.tmp", STATUS='OLD', ACTION='READ', IOSTAT=in_error)

    IF(in_error == 0) THEN
      REWIND(in_unit)
      DO
        READ(UNIT=in_unit,FMT='(A)', IOSTAT=in_error) buffer
        IF(in_error == 0) THEN
            filesCnt = filesCnt + 1
            files(filesCnt) = buffer
        ELSE
            EXIT
        END IF
      END DO
      CLOSE(UNIT=in_unit)

      message = "Found " // TRIM(ADJUSTL(intToStr(filesCnt))) // " files."
      CALL printLog("getListOfFiles", message, 10)

    ENDIF

    systemCmd = "rm -rf files.tmp"
    CALL SYSTEM(systemCmd)

  END SUBROUTINE getListOfFiles

  !==========================================================================================!
  ! A function to get the actual number of valid clusters in the population
  !==========================================================================================!
  INTEGER FUNCTION getValidClustersLen(in_pop)
    TYPE(cluster), INTENT(IN), DIMENSION(:) :: in_pop

    INTEGER :: n, i, ide
    REAL(KIND=DBL) :: energy

    n = SIZE(in_pop)

    getValidClustersLen = 0

    DO i = 1, n
      ide = in_pop(i)%edefn

      IF ((ide .NE. 0) .AND. (ide .LE. MAX_E_DEFN)) THEN
        energy = in_pop(i)%energy(ide)

        IF (energy >= R_ENERGY_MIN_THRESHOLD(ide) .AND. (energy < R_ENERGY_MAX_THRESHOLD(ide))) THEN
          getValidClustersLen = getValidClustersLen + 1
        ENDIF
      ENDIF
    ENDDO

    RETURN
  END FUNCTION getValidClustersLen

  !==========================================================================================!
  ! A function to retrieve a hashkey value from a file generated by the python/Nauty script.
  !==========================================================================================!
  CHARACTER(LEN=N_HASH_LEN) FUNCTION readHashKey(in_filename)
    CHARACTER(LEN=*), INTENT(IN) :: in_filename
    INTEGER :: in_error=1, in_unit=26

    CHARACTER(LEN=N_HASH_LEN) :: tempValue

    OPEN(UNIT=in_unit, FILE=TRIM(WORKING_DIR)//TRIM(in_filename), &
        STATUS='OLD', IOSTAT=in_error, IOMSG=ERROR_MSG)

    IF(in_error == 0) THEN
        READ(UNIT=in_unit,FMT=*, IOSTAT=in_error, IOMSG=ERROR_MSG) tempValue

        IF (in_error /= 0) THEN
          WRITE(*,*) "Cannot retrieve hashkey from file (1): ", TRIM(in_filename), " ", ERROR_MSG

          readHashKey = TRIM(UNDEFINED_HASHKEY)
        ELSE
          readHashKey = TRIM(tempValue)
        ENDIF

    ELSE
        WRITE(*,*) "Cannot retrieve hashkey from file (2): ", TRIM(in_filename), " ", ERROR_MSG

        readHashKey = TRIM(UNDEFINED_HASHKEY)
    END IF

    CLOSE(in_unit)

  END FUNCTION readHashKey

  !==========================================================================================!
  ! A subroutine to read in data mining configuration
  !==========================================================================================!
  SUBROUTINE readInDataMiningConfiguration()

  END SUBROUTINE readInDataMiningConfiguration

  !==========================================================================================!
  ! A subroutine to update the MASTER_CLUSTER according to the data mining parameters
  !==========================================================================================!
  SUBROUTINE updateDataMiningMasterCluster()

    INTEGER :: I, J

    DO I = 1, LEN(MASTER_SPECIES)
      DO J = 1, DM_ATOMS_SWAP_CNT
        IF (TRIM(MASTER_SPECIES(I)) .EQ. TRIM(DM_ATOMS_FROM(J))) THEN
          MASTER_SPECIES(I) = TRIM(DM_ATOMS_TO(J))
        ENDIF
      ENDDO
    ENDDO

  END SUBROUTINE updateDataMiningMasterCluster

  !==========================================================================================!
  ! A function to count a number of occurences of a substring in a string
  !==========================================================================================!
  FUNCTION countSubString(s1, s2) result(c)

    CHARACTER(*), INTENT(IN) :: s1, s2
    INTEGER :: c, p, posn

    c = 0
    IF (LEN(s2) == 0) RETURN
    p = 1

    DO
      posn = index(s1(p:), s2)
      IF (posn == 0) RETURN
      c = c + 1
      p = p + posn + len(s2)
    ENDDO

  END FUNCTION countSubString

  !==========================================================================================!
  ! A procedure to rotate a cluster around the principal axis of moments of inertia
  !==========================================================================================!
  SUBROUTINE rotateAroundMOI(inout_cluster, A)
    TYPE(cluster),              INTENT(INOUT) :: inout_cluster
    REAL(KIND=DP), DIMENSION(3,3), INTENT(IN) :: A

    TYPE(cluster) :: tmp_cluster
    INTEGER :: i

    tmp_cluster = inout_cluster

    DO i = 1, N_ATOMS

      tmp_cluster%atoms(i)%xc = A(1, 1) * inout_cluster%atoms(i)%xc + &
                                  A(2, 1) * inout_cluster%atoms(i)%yc + &
                                  A(3, 1) * inout_cluster%atoms(i)%zc

      tmp_cluster%atoms(i)%yc = A(1, 2) * inout_cluster%atoms(i)%xc + &
                                  A(2, 2) * inout_cluster%atoms(i)%yc + &
                                  A(3, 2) * inout_cluster%atoms(i)%zc

      tmp_cluster%atoms(i)%zc = A(1, 3) * inout_cluster%atoms(i)%xc + &
                                  A(2, 3) * inout_cluster%atoms(i)%yc + &
                                  A(3, 3) * inout_cluster%atoms(i)%zc
    END DO

    inout_cluster = tmp_cluster

  END SUBROUTINE rotateAroundMOI

  !==========================================================================================!
  ! A procedure to sort a 3 element eigenvalues array and eigenvectors matrix accordingly
  !==========================================================================================!
  SUBROUTINE sort3ElementArray(EV, EVV)

    REAL(KIND=DP), DIMENSION(3), INTENT(INOUT) :: EV
    REAL(KIND=DP), DIMENSION(3,3), INTENT(INOUT) :: EVV

    REAL(KIND=DP), DIMENSION(3) :: EVTEMP
    REAL(KIND=DP), DIMENSION(3,3) :: EVVTEMP

    INTEGER :: I, J
    REAL(KIND=DP) :: tempValue

    EVTEMP = EV
    EVVTEMP = EVV

    DO I = 1, 2
      IF (EVTEMP(1) .GT. EVTEMP(2)) THEN
        tempValue = EVTEMP(2)
        EVTEMP(2) = EVTEMP(1)
        EVTEMP(1) = tempValue

        DO J = 1, 3
          tempValue = EVVTEMP(J, 2)
          EVVTEMP(J, 2) = EVVTEMP(J, 1)
          EVVTEMP(J, 1) = tempValue
        ENDDO
      ENDIF

      IF (EVTEMP(2) .GT. EVTEMP(3)) THEN
        tempValue = EVTEMP(3)
        EVTEMP(3) = EVTEMP(2)
        EVTEMP(2) = tempValue

        DO J = 1, 3
          tempValue = EVVTEMP(J, 3)
          EVVTEMP(J, 3) = EVVTEMP(J, 2)
          EVVTEMP(J, 2) = tempValue
        ENDDO
      ENDIF
    ENDDO

    EV = EVTEMP
    EVV = EVVTEMP

  END SUBROUTINE sort3ElementArray

  !==========================================================================================!
  ! A procedure to sort a N element integer array and integer matrix accordingly
  !==========================================================================================!
  SUBROUTINE sortNElementArray(arrLen, arrActualLen, arr, arrLenRow, arr2)
    INTEGER, INTENT(IN) :: arrLen, arrActualLen
    INTEGER, DIMENSION(arrLen), INTENT(INOUT) :: arr
    INTEGER, INTENT(IN) ::arrLenRow
    INTEGER, DIMENSION(arrLenRow, arrLen), INTENT(INOUT) :: arr2

    INTEGER :: I, J, K
    INTEGER :: tempValue

    INTEGER, DIMENSION(arrLen) :: tempArr
    INTEGER, DIMENSION(arrLenRow, arrLen) :: tempArr2

    tempArr = arr
    tempArr2 = arr2

    DO I = 1, arrActualLen - 1
      DO J = I + 1, arrActualLen
        IF (tempArr(J-1) .GT. tempArr(J)) THEN
          tempValue = tempArr(J)
          tempArr(J) = tempArr(J-1)
          tempArr(J-1) = tempValue

          DO K = 1, arrLenRow
            tempValue = tempArr2(K, J)
            tempArr2(K, J) = tempArr2(K, J-1)
            tempArr2(K, J-1) = tempValue
          ENDDO
        ENDIF
      ENDDO
   ENDDO

   arr = tempArr
   arr2 = tempArr2

  END SUBROUTINE sortNElementArray

  !==========================================================================================!
  ! A procedure to sort a 3 element eigenvalues array
  !==========================================================================================!
  SUBROUTINE sort3ElementArraySimple(EV)

    REAL(KIND=DP), DIMENSION(3), INTENT(INOUT) :: EV

    REAL(KIND=DP), DIMENSION(3) :: EVTEMP

    INTEGER :: I, J
    REAL(KIND=DP) :: tempValue

    EVTEMP = EV

    DO I = 1, 2
      IF (EVTEMP(1) .GT. EVTEMP(2)) THEN
        tempValue = EVTEMP(2)
        EVTEMP(2) = EVTEMP(1)
        EVTEMP(1) = tempValue
      ENDIF

      IF (EVTEMP(2) .GT. EVTEMP(3)) THEN
        tempValue = EVTEMP(3)
        EVTEMP(3) = EVTEMP(2)
        EVTEMP(2) = tempValue
      ENDIF
    ENDDO

    EV = EVTEMP

  END SUBROUTINE sort3ElementArraySimple

  !==========================================================================================!
  ! A procedure to standartise cluster coordinates (rotate and position a cluster according
  !     to MOI and COM)
  !==========================================================================================!
  SUBROUTINE standardiseClusterCoordinates(inout_cluster)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster

    REAL(KIND=DP), DIMENSION(3, 3) :: A, ROTEVV
    REAL(KIND=DP), DIMENSION(3) :: ROTEV
    REAL(KIND=DP) :: DET

    CHARACTER(LEN=100) :: message

    WRITE(message, FMT='(A,A5)') "Standardising coordinates for a cluster: ", inout_cluster%id
    CALL printLog("standardiseClusterCoordinates", message, 10)

    ! First we move the cluster to the centre of mass
    CALL moveToCOM(inout_cluster)

    ! Find the moments of inertia
    A = momentsOfInertia(inout_cluster)

    ! Finding the determinant
    DET = M33DET(A)
    IF (DET .EQ. 0) THEN

      WRITE(message, FMT='(A,A5)') "Determinant of the matrix of moments of inertia is equal to 0. STOP."

      CALL print2All("standardiseClusterCoordinates", message, 0)
    ENDIF

    ! Get eigenvalues and eigenvectors
    CALL ZHEEVJ3(A, ROTEVV, ROTEV)

    ! Sorting in an ascending order
    CALL sort3ElementArray(ROTEV, ROTEVV)

    ! Rotate along the axes to inertia
    CALL rotateAroundMOI(inout_cluster, ROTEVV)

  END SUBROUTINE standardiseClusterCoordinates

END MODULE Utilities
