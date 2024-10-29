MODULE Moveclass

USE Config
USE Library
USE UnitCell,         ONLY : copyBoxSize, defineClusterBox, lattice_c2v_3D, xctoxf, xftoxc
USE ClusterRoutines,  ONLY : collapsed, dspecies, enforceContainer, inclusionZone, recentreCluster, &
                             fragMatt, fragmented_old, notValidClusterNoEChk, rescaleCluster, &
                             clusterGeometricalCheck, clusterGeometricalCheckSingle
USE Utilities,        ONLY : getDimsOfACluster, moveToCOM, standardiseClusterCoordinates
USE Timer

IMPLICIT NONE

!==========================================================================================!
CONTAINS

! originally from GA version within Moveclass.f90
!
! subroutines: swapAtomsGA swapCationsGA twistClusterGA mutateClusterGA 
!              displaceAtom deformUnitCell
!
!  functions:  crossover mutate chromosomalCrossover unitcellCrossover
!              crystalMutate agitateAtoms getRandom

! originally from BH version within MonteCarlo.f90
!
! subroutines: getRandomAtoms randomiseAtoms randomiseCell randomiseLocation randomiseCluster
!              moveAtoms swapAtoms swapCations twistCluster mutateCluster
!              oldtranslateCluster translateCluster rotateCluster randomiseSolution mixSolution
!

!==========================================================================================!

FUNCTION crossover(cluster1, cluster2, chckValidity, totAttemptCnt)
    TYPE(cluster) :: crossover
    TYPE(cluster), INTENT(IN) :: cluster1, cluster2
    LOGICAL, INTENT(IN) :: chckValidity
    INTEGER, INTENT(IN) :: totAttemptCnt

    LOGICAL :: crossSuccess
    INTEGER :: attemptCnt

    attemptCnt = 0
    crossSuccess = .FALSE.

    DO WHILE ((.NOT. crossSuccess) .AND. (attemptCnt <= totAttemptCnt))

      IF (N_PBC == 0) THEN
        crossover = chromosomalCrossover(cluster1, cluster2)
      ELSEIF (N_PBC == 3) THEN
        crossover = unitcellCrossover(cluster1, cluster2)
      ELSE
        ! should write subroutine printError('')
        WRITE(stderr,*)'crossover not available for selected periodicity'
        STOP
      ENDIF
      ! Geometrical checking
      IF (chckValidity .AND. notValidClusterNoEChk(crossover)) THEN

        crossSuccess = .FALSE.
      ELSE
        crossSuccess = .TRUE.
      ENDIF

      attemptCnt = attemptCnt + 1

    ENDDO

    !crossover%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))

    IF (.NOT. crossSuccess) THEN
      crossover%status = 0
    ELSE
      crossover%status = 1
    ENDIF

    IF (((JOB_TYPE == 2) .OR. (JOB_TYPE == 9)) .AND. GA_GEN_STATS) THEN
      crossover%ga_stats%gaCrossAttempts = attemptCnt

      crossover%ga_stats%gaCrossParent1 = cluster1%id
      crossover%ga_stats%gaCrossParent2 = cluster2%id
    ENDIF

END FUNCTION crossover

!==========================================================================================!

FUNCTION mutate(in_cluster,in_mutations)
    TYPE(cluster) :: mutate
    TYPE(cluster), INTENT(IN) :: in_cluster
    INTEGER, INTENT(IN), OPTIONAL :: in_mutations

    INTEGER :: n_mutations

    IF (PRESENT(in_mutations)) THEN
      n_mutations = in_mutations
    ELSE
      n_mutations = N_ATOMS ! + 3 x N_PBC unless n_mutations=0
    ENDIF

    IF (N_PBC == 0) THEN
      mutate = agitateAtoms(in_cluster,n_mutations)
    ELSEIF (N_PBC == 3) THEN
      mutate = crystalMutate(in_cluster,n_mutations)
    ELSE
      ! should write subroutine printError('')
      WRITE(stderr,*)'mutate not available for selected periodicity'
      STOP
    ENDIF
END FUNCTION mutate


  !==========================================================================================!
  ! Chooses which cromosomal procedure to follow
  !==========================================================================================!
  FUNCTION chromosomalCrossover(cluster1, cluster2)
    TYPE(cluster) :: chromosomalCrossover
    TYPE(cluster), INTENT(IN) :: cluster1, cluster2

    INTEGER :: dim1, dim2
    REAL(KIND=DBL) :: r
    TYPE(cluster) :: out_cluster
    CHARACTER(LEN=100) :: message

    dim1 = getDimsOfACluster(cluster1)
    dim2 = getDimsOfACluster(cluster2)

    CALL RANDOM_NUMBER(r)

    IF ((MAX(dim1, dim2) .EQ. 3) .AND. (r .GT. GA_CROSS_1_2D_RATIO))THEN
      out_cluster = chromosomalCrossover_3D(cluster1, cluster2)
      WRITE(message, *) "performing chromosomalCrossover_3D. Cluster1: ", TRIM(cluster1%id), &
        " , cluster2: ", TRIM(cluster2%id)
    ELSE

      out_cluster = chromosomalCrossover_1D_2D(cluster1, cluster2, dim1, dim2)
      WRITE(message, *) "performing chromosomalCrossover_1D_2D. Cluster1: ", TRIM(cluster1%id), &
        " , cluster2: ", TRIM(cluster2%id)
    ENDIF

    CALL printLog("chromosomalCrossover", message, 10)

    chromosomalCrossover = out_cluster
  END FUNCTION chromosomalCrossover

  !==========================================================================================!
  FUNCTION chromosomalCrossover_3D(cluster1, cluster2)
    TYPE(cluster) :: chromosomalCrossover_3D
    TYPE(cluster), INTENT(IN) :: cluster1, cluster2

    TYPE(cluster) :: out_cluster
    INTEGER :: i, j, k

    REAL(KIND=DBL),DIMENSION(:), ALLOCATABLE :: x_new, y_new, z_new
    LOGICAL, DIMENSION(:), ALLOCATABLE :: l_old

    REAL(KIND=DBL), DIMENSION(5) :: r   !Random number 3-D array
    CHARACTER(LEN=2) :: element='--'
    REAL(KIND=DBL) :: x_old, y_old, z_old, xy_old, z_min, z_max
    REAL(KIND=DBL) :: cos_phi, cos_eta, sin_phi, sin_eta

    ALLOCATE(x_new(N_ATOMS))
    ALLOCATE(y_new(N_ATOMS))
    ALLOCATE(z_new(N_ATOMS))
    ALLOCATE(l_old(N_ATOMS))

    !CALL RANDOM_NUMBER(r) ! define angles of rotation and cutting point
    DO i = 1,5
       !r(i) = rand_num_uniform()
       CALL RANDOM_NUMBER(r(i))
    END DO

    DO i=1, 4
      r(i) = twoPI * r(i)
    ENDDO

    cos_phi=COS(r(1))
    sin_phi=SIN(r(1))
    cos_eta=COS(r(2))
    sin_eta=SIN(r(2))

    DO i=1, N_ATOMS
        x_old  = cluster1%atoms(i)%xc
        y_old  = cluster1%atoms(i)%yc
        z_old  = cluster1%atoms(i)%zc
        xy_old = sin_eta*x_old  + cos_eta*y_old
        out_cluster%atoms(i)%xc = cos_eta*x_old  - sin_eta*y_old
        out_cluster%atoms(i)%yc = cos_phi*xy_old - sin_phi*z_old
        out_cluster%atoms(i)%zc = sin_phi*xy_old + cos_phi*z_old
    END DO

    cos_phi=COS(r(3))
    sin_phi=SIN(r(3))
    cos_eta=COS(r(4))
    sin_eta=SIN(r(4))

    z_min =  R_MAX_CLUSTER_BOUNDARY(3)
    z_max = -R_MAX_CLUSTER_BOUNDARY(3)

    DO i=1, N_ATOMS
        x_old  = cluster2%atoms(i)%xc
        y_old  = cluster2%atoms(i)%yc
        z_old  = cluster2%atoms(i)%zc
        xy_old  =  sin_eta*x_old  + cos_eta*y_old
        x_new(i) = cos_eta*x_old  - sin_eta*y_old
        y_new(i) = cos_phi*xy_old - sin_phi*z_old
        z_new(i) = sin_phi*xy_old + cos_phi*z_old
        IF ( z_new(i) < z_min ) z_min = z_new(i)
        IF ( z_new(i) > z_max ) z_max = z_new(i)
        l_old(i) = .TRUE.
    END DO

    xy_old = z_min + ((0.1 + 0.8*r(5))*(z_max-z_min))

    DO i=1, N_ATOMS
      IF ( out_cluster%atoms(i)%zc < xy_old ) THEN ! replace atom with nearest second copy
        k=0
        z_min=z_max+1.0
        element = cluster1%atoms(i)%symbol
        DO j=1, N_ATOMS
          IF (l_old(j)) THEN ! can still use this atom as a replacement
            IF (index(cluster2%atoms(j)%symbol,element).NE.0) THEN ! same species
              IF ( z_new(j) < z_min ) THEN
                k=j
                z_min=z_new(j)
              ENDIF
            ENDIF
          ENDIF
        END DO
        IF ( k == 0 ) THEN
          WRITE(*,*)'Error during the process of mutatin-g cluster PLACE1'
          STOP
        ELSE
          l_old(k) = .FALSE.
          out_cluster%atoms(i)%xc = x_new(k)
          out_cluster%atoms(i)%yc = y_new(k)
          out_cluster%atoms(i)%zc = z_new(k)
        ENDIF
      ENDIF

      out_cluster%atoms(i)%symbol = cluster1%atoms(i)%symbol
      out_cluster%atoms(i)%cstype = cluster1%atoms(i)%cstype
    END DO

    !out_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))
    out_cluster%edefn = 0

    DEALLOCATE(l_old)
    DEALLOCATE(z_new)
    DEALLOCATE(y_new)
    DEALLOCATE(x_new)

    chromosomalCrossover_3D = out_cluster
  END FUNCTION chromosomalCrossover_3D

  !==========================================================================================!
  ! Chromosomal crossover algorythm for 1D and 2D clusters
  !==========================================================================================!
  FUNCTION chromosomalCrossover_1D_2D(cluster1, cluster2, cluster1Dim, cluster2Dim)
    TYPE(cluster) :: chromosomalCrossover_1D_2D
    TYPE(cluster), INTENT(IN) :: cluster1, cluster2
    INTEGER,       INTENT(IN) :: cluster1Dim, cluster2Dim

    INTEGER :: coord, temp1Cnt, temp2Cnt, crossCnt, i, j, k
    REAL(KIND=DBL) :: r, cord_min, cord_max, cut, cord_value, cord2_value, addHeight
    TYPE(cluster) :: temp1Cluster, temp2Cluster, crossProduct, cluster1rot, cluster2rot

    CHARACTER(LEN=2) :: element='--'
    LOGICAL, DIMENSION(:), ALLOCATABLE :: l_old
    ALLOCATE(l_old(N_ATOMS))

    cluster1rot = cluster1
    cluster2rot = cluster2

    ! Rotating the clusters
    CALL rotateClusterZAxis(cluster1rot)
    CALL rotateClusterZAxis(cluster2rot)

    temp1Cnt = 0
    temp2Cnt = 0

    ! Choose axis
    IF (MAX(cluster1Dim, cluster2Dim) .EQ. 1) THEN
      coord = 1
    ELSE
      CALL RANDOM_NUMBER(r)

      IF (r < 0.5) THEN
        coord = 1
      ELSE
        coord = 2
      ENDIF
    ENDIF

    cord_min =  R_MAX_CLUSTER_BOUNDARY(3)
    cord_max = -R_MAX_CLUSTER_BOUNDARY(3)

    DO i = 1, N_ATOMS
      IF (coord == 1) THEN
        IF (cluster2rot%atoms(i)%xc < cord_min) cord_min = cluster2rot%atoms(i)%xc
        IF (cluster2rot%atoms(i)%xc > cord_max) cord_max = cluster2rot%atoms(i)%xc
      ELSE
        IF (cluster2rot%atoms(i)%yc < cord_min) cord_min = cluster2rot%atoms(i)%yc
        IF (cluster2rot%atoms(i)%yc > cord_max) cord_max = cluster2rot%atoms(i)%yc
      ENDIF

      l_old(i) = .TRUE.
    ENDDO

    CALL RANDOM_NUMBER(r)
    cut = cord_min + ((0.1 + 0.8*r)*(cord_max - cord_min))

    DO i = 1, N_ATOMS
      IF (coord == 1) THEN
        cord_value = cluster1rot%atoms(i)%xc
      ELSE
        cord_value = cluster1rot%atoms(i)%yc
      ENDIF

      IF (cord_value < cut) THEN
        k = 0
        cord_min = cord_max + 1.0
        element = cluster1rot%atoms(i)%symbol

        DO j = 1, N_ATOMS
          IF (index(cluster2rot%atoms(j)%symbol, element) .NE. 0) THEN
            IF (l_old(j)) THEN
              IF (coord == 1) THEN
                cord2_value = cluster2rot%atoms(j)%xc
              ELSE
                cord2_value = cluster2rot%atoms(j)%yc
              ENDIF

              IF (cord2_value <= cord_min) THEN
                k = j
                cord_min = cord2_value
              ENDIF
            ENDIF
          ENDIF
        ENDDO

        IF ( k == 0 ) THEN

          WRITE(*,*) "TLADEBUG :: ", cord2_value, cord_min
          WRITE(*,*) 'Error during the process of mutatin-g cluster PLACE2'
          STOP
        ELSE
          l_old(k) = .FALSE.
          temp2Cnt = temp2Cnt + 1

          temp2Cluster%atoms(temp2Cnt)%xc = cluster2rot%atoms(k)%xc
          temp2Cluster%atoms(temp2Cnt)%yc = cluster2rot%atoms(k)%yc
          temp2Cluster%atoms(temp2Cnt)%zc = cluster2rot%atoms(k)%zc

          temp2Cluster%atoms(temp2Cnt)%symbol = cluster1rot%atoms(i)%symbol
          temp2Cluster%atoms(temp2Cnt)%cstype = cluster1rot%atoms(i)%cstype
        ENDIF

      ELSE
        temp1Cnt = temp1Cnt + 1

        temp1Cluster%atoms(temp1Cnt)%xc = cluster1rot%atoms(i)%xc
        temp1Cluster%atoms(temp1Cnt)%yc = cluster1rot%atoms(i)%yc
        temp1Cluster%atoms(temp1Cnt)%zc = cluster1rot%atoms(i)%zc

        temp1Cluster%atoms(temp1Cnt)%symbol = cluster1rot%atoms(i)%symbol
        temp1Cluster%atoms(temp1Cnt)%cstype = cluster1rot%atoms(i)%cstype
      ENDIF
    ENDDO

    CALL moveToCOM(temp1Cluster, temp1Cnt)
    CALL moveToCOM(temp2Cluster, temp2Cnt)

    ! Adding cluster no 1
    crossCnt = 0

    DO i = 1, temp1Cnt
      crossCnt = crossCnt + 1

      crossProduct%atoms(crossCnt)%xc     = temp1Cluster%atoms(i)%xc
      crossProduct%atoms(crossCnt)%yc     = temp1Cluster%atoms(i)%yc
      crossProduct%atoms(crossCnt)%zc     = temp1Cluster%atoms(i)%zc
      crossProduct%atoms(crossCnt)%symbol = temp1Cluster%atoms(i)%symbol
      crossProduct%atoms(crossCnt)%cstype = temp1Cluster%atoms(i)%cstype
    ENDDO

    ! Adding cluster no 2
    addHeight = (FRAG_COEF + MAX(COLL_COEF_DIFF, COLL_COEF_SAME)) * 0.5

    DO i = 1, temp2Cnt
      crossCnt = crossCnt + 1

      crossProduct%atoms(crossCnt)%xc     = temp2Cluster%atoms(i)%xc
      crossProduct%atoms(crossCnt)%yc     = temp2Cluster%atoms(i)%yc
      crossProduct%atoms(crossCnt)%zc     = temp2Cluster%atoms(i)%zc + addHeight
      crossProduct%atoms(crossCnt)%symbol = temp2Cluster%atoms(i)%symbol
      crossProduct%atoms(crossCnt)%cstype = temp2Cluster%atoms(i)%cstype
    ENDDO

    !crossProduct%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))
    crossProduct%edefn = 0

    CALL standardiseClusterCoordinates(crossProduct)

    DEALLOCATE(l_old)

    chromosomalCrossover_1D_2D = crossProduct
  END FUNCTION chromosomalCrossover_1D_2D

!==========================================================================================!

FUNCTION unitcellCrossover(cluster1, cluster2)
    TYPE(cluster) :: unitcellCrossover
    TYPE(cluster), INTENT(IN) :: cluster1, cluster2

    TYPE(cluster) :: out_cluster

    REAL(KIND=DBL) :: r_cut           !Random cut 
    INTEGER :: n_axis                 !Random axis

    REAL(KIND=DBL), DIMENSION(:,:), ALLOCATABLE :: coords_tmp
    LOGICAL, DIMENSION(:), ALLOCATABLE :: original

    REAL(KIND=DBL) :: a(2), b(2), c(2), tmp(3), r_min
    INTEGER, DIMENSION(2) :: imin, imid, imax
    CHARACTER(LEN=2) :: element='--'
    INTEGER :: i, j, k
    LOGICAL :: L_MATCH_abc=.TRUE.

    ALLOCATE(coords_tmp(N_ATOMS,3))
    ALLOCATE(original(N_ATOMS))

    out_cluster = cluster1

    IF (L_MATCH_abc) THEN

      imin(1)=1
      imin(2)=1
      imid(1)=2
      imid(2)=2
      imax(1)=3
      imax(2)=3

    ELSE ! match largest, middle, and smallest cell lengths

      a(1)=cluster1%box(1)
      b(1)=cluster1%box(2)
      c(1)=cluster1%box(3)
      a(2)=cluster2%box(1)
      b(2)=cluster2%box(2)
      c(2)=cluster2%box(3)

      DO i=1,2
        IF (a(i) > b(i)) THEN
          IF (a(i) > c(i)) THEN
            imax(i) = 1
            IF (b(i) > c(i)) THEN
              imid(i) = 2
              imin(i) = 3
            ELSE
              imid(i) = 3
              imin(i) = 2
            ENDIF
          ELSE
            imax(i) = 3
            imid(i) = 1
            imin(i) = 2
          ENDIF
        ELSE
          IF (a(i) < c(i)) THEN
            imin(i) = 1
            IF (b(i) < c(i)) THEN
              imid(i) = 2
              imax(i) = 3
            ELSE
              imid(i) = 3
              imax(i) = 2
            ENDIF
          ELSE
            imin(i) = 3
            imid(i) = 1
            imax(i) = 2
          ENDIF
        ENDIF
      ENDDO
    ENDIF

    out_cluster%box(imin(1)) = 0.5*(cluster1%box(imin(1))+cluster2%box(imin(2)))
    out_cluster%box(imid(1)) = 0.5*(cluster1%box(imid(1))+cluster2%box(imid(2)))
    out_cluster%box(imax(1)) = 0.5*(cluster1%box(imax(1))+cluster2%box(imax(2)))
    out_cluster%box(imin(1)+3) = 0.5*(cluster1%box(imin(1)+3)+cluster2%box(imin(2)+3))
    out_cluster%box(imid(1)+3) = 0.5*(cluster1%box(imid(1)+3)+cluster2%box(imid(2)+3))
    out_cluster%box(imax(1)+3) = 0.5*(cluster1%box(imax(1)+3)+cluster2%box(imax(2)+3))

    DO i=1, N_ATOMS
      tmp(1) = cluster2%atoms(i)%xf
      tmp(2) = cluster2%atoms(i)%yf
      tmp(3) = cluster2%atoms(i)%zf
      coords_tmp(i,imin(1)) = tmp(imin(2))
      coords_tmp(i,imid(1)) = tmp(imid(2))
      coords_tmp(i,imax(1)) = tmp(imax(2))
    ENDDO

    ! define direction and cutting point
    n_axis=randomInteger(1,3)
    r_cut=randomReal(0.1,0.9)

    DO i=1, N_ATOMS
      original=.TRUE.
    ENDDO

    DO i=1, N_ATOMS
      IF ( coords_tmp(i,n_axis) < r_cut ) THEN ! replace atom i with ...
        k = 0
        r_min = 2.0_dp
        element = cluster2%atoms(i)%symbol
        DO j=1, N_ATOMS
          IF (original(j)) THEN ! can still use this atom as a replacement
            IF (index(cluster1%atoms(j)%symbol,element).NE.0) THEN ! same species
              IF (n_axis.eq.1 .AND. cluster1%atoms(j)%xf < r_min ) THEN
                k = j
                r_min = cluster1%atoms(j)%xf
              ELSEIF (n_axis.eq.2 .AND. cluster1%atoms(j)%yf < r_min ) THEN
                k = j
                r_min = cluster1%atoms(j)%yf
              ELSEIF (n_axis.eq.3 .AND. cluster1%atoms(j)%zf < r_min ) THEN
                k = j
                r_min = cluster1%atoms(j)%zf
              ENDIF
            ENDIF
          ENDIF
        END DO
        IF ( k == 0 ) THEN
          WRITE(stderr,*)'Error during the process of mutating cluster PLACE3'
          STOP
        ELSE ! ... atom k
          original(k) = .FALSE.
          out_cluster%atoms(k)%xf = coords_tmp(i,1)
          out_cluster%atoms(k)%yf = coords_tmp(i,2)
          out_cluster%atoms(k)%zf = coords_tmp(i,3)
        ENDIF
      ENDIF
    END DO

    out_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))
    out_cluster%energy(0) = 0.00_dp
    CALL xftoxc(out_cluster)

    DEALLOCATE(original)
    DEALLOCATE(coords_tmp)

    unitcellCrossover = out_cluster
END FUNCTION unitcellCrossover

!==========================================================================================!

FUNCTION crystalMutate(in_cluster, in_mutations, in_stepsize)
    TYPE(cluster) :: crystalMutate
    TYPE(cluster), INTENT(IN) :: in_cluster
    INTEGER, INTENT(IN), OPTIONAL :: in_mutations
    REAL(KIND=DBL), INTENT(IN), OPTIONAL :: in_stepsize

    TYPE(cluster) :: out_cluster
    REAL(KIND=DBL) :: stepsize
    INTEGER :: i, j, n_mutations
    LOGICAL :: mutate_all, mutate_cell

    out_cluster = in_cluster
    n_mutations = N_ATOMS
    stepsize = R_BH_STEPSIZE
    mutate_all=.TRUE.
    mutate_cell=.TRUE.

    IF (PRESENT(in_mutations)) THEN
      IF (in_mutations > 0 .AND. in_mutations < N_ATOMS) THEN
        n_mutations = in_mutations
        mutate_all=.FALSE.
      ELSEIF (in_mutations == 0) THEN
        mutate_cell=.FALSE.
      ENDIF
    ENDIF
    IF (PRESENT(in_stepsize)) stepsize = ABS(in_stepsize)

    DO i=1, n_mutations
      IF (mutate_all) THEN
        j = i
      ELSE
        j = getRandom(1,N_ATOMS)
      ENDIF
      CALL displaceAtom(out_cluster%atoms(j),stepsize)
    END DO

    CALL xctoxf(out_cluster)

    IF (mutate_cell) THEN
      stepsize = 0.05_dp ! max 5% change
      CALL deformUnitCell(out_cluster%box,stepsize)
      CALL xftoxc(out_cluster)
    ENDIF

    out_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))
    out_cluster%edefn = 0

    crystalMutate = out_cluster
END FUNCTION crystalMutate

!==========================================================================================!

FUNCTION agitateAtoms(in_cluster, in_mutations, in_stepsize)
  TYPE(cluster) :: agitateAtoms
  TYPE(cluster), INTENT(IN) :: in_cluster
  INTEGER, INTENT(IN), OPTIONAL :: in_mutations
  REAL(KIND=DBL), INTENT(IN), OPTIONAL :: in_stepsize

  TYPE(cluster) :: out_cluster
  REAL(KIND=DBL) :: stepsize, mutationType
  INTEGER :: i, j, k, n_mutations, attemptCnt
  LOGICAL :: mutate_all
  LOGICAL :: look4AtomToSwap = .TRUE., mutationSuccess

  REAL(KIND=DBL) :: mut1, mut2, mut3

  mut1 = MUTATE_SWAP_PROP
  mut2 = mut1 + MUTATE_EXPAND_PROP
  mut3 = mut2 + MUTATE_CONTRACT_PROP

  n_mutations = N_ATOMS

  IF (JOB_TYPE .EQ. 2) THEN
    stepsize = GA_STEPSIZE
  ELSE
    stepsize = R_BH_STEPSIZE
  ENDIF

  mutate_all=.TRUE.
  attemptCnt = 0

  IF (PRESENT(in_mutations)) THEN
    IF (in_mutations > 0 .AND. in_mutations < N_ATOMS) THEN
      n_mutations = in_mutations
      mutate_all=.FALSE.
    ENDIF
  ENDIF
  IF (PRESENT(in_stepsize)) stepsize = ABS(in_stepsize)

  ! Enforcing successful mutation
  mutationSuccess = .FALSE.
  DO WHILE ((.NOT. mutationSuccess) .AND. (attemptCnt < 100000))

    attemptCnt = attemptCnt + 1
    out_cluster = in_cluster

    out_cluster%ga_stats%gaMutateSwapCnt = 0
    out_cluster%ga_stats%gaMutateDisplCnt = 0

    !Decide how to mutate
    CALL RANDOM_NUMBER(mutationType)

    ! Expension of a cluster
    IF ((mut1 <= mutationType) .AND. (mutationType < mut2)) THEN
      CALL rescaleCluster(out_cluster, MUTATE_EXPAND_COEF)

    ! Contraction of a cluster
    ELSEIF ((mut2 <= mutationType) .AND. (mutationType < mut3)) THEN
      CALL rescaleCluster(out_cluster, MUTATE_CONTRACT_COEF)

    ELSE

      DO i=1, n_mutations

        IF (mutate_all) THEN
          j = i
        ELSE
          j = getRandom(1,N_ATOMS)
        ENDIF

        ! Swap atoms
        IF (mutationType < mut1) THEN
          IF (N_ATOMS > 1) THEN

            look4AtomToSwap = .TRUE.
            DO WHILE (look4AtomToSwap)
              k = getRandom(1,N_ATOMS)

              IF (k .NE. j) THEN
                look4AtomToSwap = .FALSE.
              ENDIF
            END DO

            CALL swap2Atoms(out_cluster, j, k)

            IF (((JOB_TYPE == 2) .OR. (JOB_TYPE == 9)) .AND. GA_GEN_STATS) THEN
              out_cluster%ga_stats%gaMutateSwapCnt = out_cluster%ga_stats%gaMutateSwapCnt + 1
            ENDIF
          ENDIF

        ! Displacing atoms
        ELSE
          CALL displaceAtom(out_cluster%atoms(j),stepsize)

          IF (((JOB_TYPE == 2) .OR. (JOB_TYPE == 9)) .AND. GA_GEN_STATS) THEN
            out_cluster%ga_stats%gaMutateDisplCnt = out_cluster%ga_stats%gaMutateDisplCnt + 1
          ENDIF
        ENDIF

        CALL enforceContainer(out_cluster, j)
      END DO
    ENDIF

    ! Geometrical checking
    IF (notValidClusterNoEChk(out_cluster)) THEN
      mutationSuccess = .FALSE.
    ELSE
      mutationSuccess = .TRUE.
    ENDIF

    IF (((JOB_TYPE == 2) .OR. (JOB_TYPE == 9)) .AND. GA_GEN_STATS) THEN
      out_cluster%ga_stats%gaMutateAttempts = attemptCnt
    ENDIF
  ENDDO

  IF (.NOT. mutationSuccess) THEN
    out_cluster%status = 0
  ELSE
    out_cluster%status = 1
  ENDIF

  agitateAtoms = out_cluster

END FUNCTION agitateAtoms

!==========================================================================================!

INTEGER FUNCTION getRandom(lower,upper)
   INTEGER, INTENT(IN)  :: lower,upper
   REAL(KIND=DBL) :: r

   CALL RANDOM_NUMBER(r)

   getRandom = int(r*(upper+1-lower))+lower

END FUNCTION getRandom

!==========================================================================================!

SUBROUTINE displaceAtom(inout_atom, in_stepsize)
    TYPE(atom), INTENT(INOUT) :: inout_atom
    REAL(KIND=DBL), INTENT(IN) :: in_stepsize
    REAL(KIND=DBL) :: r(3)

    CALL RANDOM_NUMBER(r)
    inout_atom%xc = inout_atom%xc + ((r(1)*2)-1) * in_stepsize
    inout_atom%yc = inout_atom%yc + ((r(2)*2)-1) * in_stepsize
    inout_atom%zc = inout_atom%zc + ((r(3)*2)-1) * in_stepsize

    RETURN
END SUBROUTINE displaceAtom

!==========================================================================================!

SUBROUTINE deformUnitCell(inout_box, in_stepsize)
    REAL(KIND=DBL), INTENT(INOUT) :: inout_box(6)
    REAL(KIND=DBL), INTENT(IN) :: in_stepsize
    REAL(KIND=DBL) :: r(3)

    CALL RANDOM_NUMBER(r)
    r(1)=((r(1)*2)-1) * in_stepsize
    r(2)=((r(2)*2)-1) * in_stepsize
    r(3)=((r(3)*2)-1) * in_stepsize
    inout_box(1) = inout_box(1) * (r(1)+1)
    inout_box(2) = inout_box(2) * (r(2)+1)
    inout_box(3) = inout_box(3) * (r(3)+1)

    RETURN
END SUBROUTINE deformUnitCell

  !==========================================================================================!
  ! A procedure to swap two atoms of a cluster
  !==========================================================================================!
  SUBROUTINE swap2Atoms(inout_cluster, i, j)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    INTEGER, INTENT(IN)          :: i, j

    REAL(KIND=DBL) :: xc, yc, zc, xf, yf, zf

    xc = inout_cluster%atoms(j)%xc
    yc = inout_cluster%atoms(j)%yc
    zc = inout_cluster%atoms(j)%zc
    xf = inout_cluster%atoms(j)%xf
    yf = inout_cluster%atoms(j)%yf
    zf = inout_cluster%atoms(j)%zf

    inout_cluster%atoms(j)%xc = inout_cluster%atoms(i)%xc
    inout_cluster%atoms(j)%yc = inout_cluster%atoms(i)%yc
    inout_cluster%atoms(j)%zc = inout_cluster%atoms(i)%zc
    inout_cluster%atoms(j)%xf = inout_cluster%atoms(i)%xf
    inout_cluster%atoms(j)%yf = inout_cluster%atoms(i)%yf
    inout_cluster%atoms(j)%zf = inout_cluster%atoms(i)%zf

    inout_cluster%atoms(i)%xc = xc
    inout_cluster%atoms(i)%yc = yc
    inout_cluster%atoms(i)%zc = zc
    inout_cluster%atoms(i)%xf = xf
    inout_cluster%atoms(i)%yf = yf
    inout_cluster%atoms(i)%zf = zf

    RETURN
  END SUBROUTINE swap2Atoms

  !==========================================================================================!

SUBROUTINE swapAtomsGA(in_cluster, out_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    TYPE(cluster), INTENT(INOUT) :: out_cluster

    INTEGER, DIMENSION(:), ALLOCATABLE :: atomOrder

    INTEGER :: i,j,k
    REAL :: r_random=0. , s_random=0.

    ALLOCATE(atomOrder(N_ATOMS))
    DO i=1, N_ATOMS
        atomOrder(i) = i
    ENDDO

    DO i=1, N_ATOMS
      CALL RANDOM_NUMBER(r_random)
      IF (r_random > 0.8) THEN
        CALL RANDOM_NUMBER(s_random)
        j = 1 + INT(s_random*(N_ATOMS))
        IF (j > N_ATOMS) j=N_ATOMS
        k = atomOrder(i)
        atomOrder(i) = atomOrder(j)
        atomOrder(j) = k
      ENDIF
    ENDDO

    DO i=1, N_ATOMS
        j = atomOrder(i)
        out_cluster%atoms(i)%symbol = in_cluster%atoms(i)%symbol
        out_cluster%atoms(i)%cstype = in_cluster%atoms(i)%cstype
        out_cluster%atoms(i)%xc = in_cluster%atoms(j)%xc
        out_cluster%atoms(i)%yc = in_cluster%atoms(j)%yc
        out_cluster%atoms(i)%zc = in_cluster%atoms(j)%zc
    END DO

    out_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))
    out_cluster%edefn = 0

    DEALLOCATE(atomOrder)
END SUBROUTINE swapAtomsGA

!==========================================================================================!

SUBROUTINE swapCationsGA(in_cluster, out_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    TYPE(cluster), INTENT(INOUT) :: out_cluster

    INTEGER, DIMENSION(:), ALLOCATABLE :: atomOrder

    INTEGER :: i,j,k
    REAL :: r_random=0. , s_random=0.

    ALLOCATE(atomOrder(N_CATIONS))
    DO i=1, N_CATIONS
        atomOrder(i) = i
    ENDDO

    DO i=1, N_CATIONS
      CALL RANDOM_NUMBER(r_random)
      IF (r_random > 0.8) THEN
        CALL RANDOM_NUMBER(s_random)
        j = 1 + INT(s_random*(N_CATIONS))
        IF (j > N_CATIONS) write(*,*)'UCL ',j
        IF (j > N_CATIONS) j=N_CATIONS
        k = atomOrder(i)
        atomOrder(i) = atomOrder(j)
        atomOrder(j) = k
      ENDIF
    ENDDO

    DO i=1, N_CATIONS
        j = atomOrder(i)
        out_cluster%atoms(i)%symbol = in_cluster%atoms(i)%symbol
        out_cluster%atoms(i)%cstype = in_cluster%atoms(i)%cstype
        out_cluster%atoms(i)%xc = in_cluster%atoms(j)%xc
        out_cluster%atoms(i)%yc = in_cluster%atoms(j)%yc
        out_cluster%atoms(i)%zc = in_cluster%atoms(j)%zc
    END DO
    DO i=N_CATIONS+1,N_ATOMS
        j = i
        out_cluster%atoms(i)%cstype = in_cluster%atoms(i)%cstype
        out_cluster%atoms(i)%symbol = in_cluster%atoms(i)%symbol
        out_cluster%atoms(i)%xc = in_cluster%atoms(j)%xc
        out_cluster%atoms(i)%yc = in_cluster%atoms(j)%yc
        out_cluster%atoms(i)%zc = in_cluster%atoms(j)%zc
    END DO

    out_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))
    out_cluster%edefn = 0

    DEALLOCATE(atomOrder)
END SUBROUTINE swapCationsGA

!==========================================================================================!

SUBROUTINE twistClusterGA(in_cluster, out_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    TYPE(cluster), INTENT(INOUT) :: out_cluster

    REAL, DIMENSION(:), ALLOCATABLE :: x_new, y_new, z_new

    REAL(KIND=DBL), DIMENSION(3) :: r   !Random number 3-D array
    REAL :: r_random=0.
    REAL :: x_old, y_old, z_old, xy_new, z_min, z_max
    REAL :: cos_phi, cos_eta, cos_psi
    REAL :: sin_phi, sin_eta, sin_psi
    INTEGER :: i, j

    ALLOCATE(x_new(N_ATOMS))
    ALLOCATE(y_new(N_ATOMS))
    ALLOCATE(z_new(N_ATOMS))

    CALL RANDOM_NUMBER(r) ! define two angles of rotation and one for twist
    !DO j= 1,3
    !   r(j) = rand_num_uniform()
   ! END DO

    cos_phi=COS(twoPI*r(1))
    sin_phi=SIN(twoPI*r(1))
    cos_eta=COS(twoPI*r(2))
    sin_eta=SIN(twoPI*r(2))
    cos_psi=COS(twoPI*r(3))
    sin_psi=SIN(twoPI*r(3))

    DO i=1, N_ATOMS
        x_old  = in_cluster%atoms(i)%xc
        y_old  = in_cluster%atoms(i)%yc
        z_old  = in_cluster%atoms(i)%zc
        xy_new  =  sin_eta*x_old  + cos_eta*y_old
        x_new(i) = cos_eta*x_old  - sin_eta*y_old
        y_new(i) = cos_phi*xy_new - sin_phi*z_old
        z_new(i) = sin_phi*xy_new + cos_phi*z_old
    END DO

    CALL RANDOM_NUMBER(r_random) !  define fraction of cluster to twist
    z_min =  R_MAX_CLUSTER_BOUNDARY(3)
    z_max = -R_MAX_CLUSTER_BOUNDARY(3)
    DO i=1, N_ATOMS
      IF ( z_new(i) < z_min ) z_min = z_new(i)
      IF ( z_new(i) > z_max ) z_max = z_new(i)
    END DO
    xy_new = z_min + ((0.1 + r_random*0.8)*(z_max-z_min))
    DO i=1, N_ATOMS
      out_cluster%atoms(i)%symbol = in_cluster%atoms(i)%symbol
      out_cluster%atoms(i)%cstype = in_cluster%atoms(i)%cstype
      IF ( z_new(i) < xy_new ) THEN ! twist atoms about z-axis by psi
        out_cluster%atoms(i)%xc = cos_psi*x_new(i) - sin_psi*y_new(i)
        out_cluster%atoms(i)%yc = sin_psi*x_new(i) + cos_psi*y_new(i)
      ELSE
        out_cluster%atoms(i)%xc = x_new(i)
        out_cluster%atoms(i)%yc = y_new(i)
      ENDIF
      out_cluster%atoms(i)%zc = z_new(i)
    END DO
    out_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))
    out_cluster%edefn = 0

    DEALLOCATE(x_new)
    DEALLOCATE(y_new)
    DEALLOCATE(z_new)
END SUBROUTINE twistClusterGA

!==========================================================================================!

SUBROUTINE mutateClusterGA(in_cluster, out_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    TYPE(cluster), INTENT(INOUT) :: out_cluster

    REAL, DIMENSION(:), ALLOCATABLE :: x_new, y_new, z_new
    LOGICAL, DIMENSION(:), ALLOCATABLE :: l_old

    REAL(KIND=DBL), DIMENSION(5) :: r   !Random number 3-D array
    CHARACTER(LEN=2) :: element='--'
    REAL :: x_old, y_old, z_old, xy_old, z_min, z_max
    REAL :: cos_phi, cos_eta, sin_phi, sin_eta
    INTEGER :: i,j,k

    ALLOCATE(l_old(N_ATOMS))
    ALLOCATE(x_new(N_ATOMS))
    ALLOCATE(y_new(N_ATOMS))
    ALLOCATE(z_new(N_ATOMS))

    CALL RANDOM_NUMBER(r) ! define angles of rotation and cutting point
    do i=1,4
      r(i)=twoPI*r(i)
    enddo

    cos_phi=COS(r(1))
    sin_phi=SIN(r(1))
    cos_eta=COS(r(2))
    sin_eta=SIN(r(2))

    DO i=1, N_ATOMS
        x_old  = in_cluster%atoms(i)%xc
        y_old  = in_cluster%atoms(i)%yc
        z_old  = in_cluster%atoms(i)%zc
        xy_old =                 sin_eta*x_old  + cos_eta*y_old
        out_cluster%atoms(i)%xc = cos_eta*x_old  - sin_eta*y_old
        out_cluster%atoms(i)%yc = cos_phi*xy_old - sin_phi*z_old
        out_cluster%atoms(i)%zc = sin_phi*xy_old + cos_phi*z_old
    END DO

    cos_phi=COS(r(3))
    sin_phi=SIN(r(3))
    cos_eta=COS(r(4))
    sin_eta=SIN(r(4))

    DO i=1, N_ATOMS
        x_old  = in_cluster%atoms(i)%xc
        y_old  = in_cluster%atoms(i)%yc
        z_old  = in_cluster%atoms(i)%zc
        xy_old  =  sin_eta*x_old  + cos_eta*y_old
        x_new(i) = cos_eta*x_old  - sin_eta*y_old
        y_new(i) = cos_phi*xy_old - sin_phi*z_old
        z_new(i) = sin_phi*xy_old + cos_phi*z_old
    END DO

    z_min =  R_MAX_CLUSTER_BOUNDARY(3)
    z_max = -R_MAX_CLUSTER_BOUNDARY(3)
    DO i=1, N_ATOMS
      IF ( z_new(i) < z_min ) z_min = z_new(i)
      IF ( z_new(i) > z_max ) z_max = z_new(i)
      l_old(i) = .TRUE.
    END DO
    xy_old = z_min + ((0.1 + 0.8*r(5))*(z_max-z_min))

    DO i=1, N_ATOMS
      IF ( out_cluster%atoms(i)%zc < xy_old ) THEN ! replace atom with nearest second copy
        k=0
        z_min=z_max+1.0
        element = in_cluster%atoms(i)%symbol
        DO j=1, N_ATOMS
          IF (l_old(j)) THEN ! can still use this atom as a replacement
            IF (index(in_cluster%atoms(j)%symbol,element).ne.0) THEN ! same species
              IF ( z_new(j) < z_min ) THEN
                k=j
                z_min=z_new(j)
              ENDIF
            ENDIF
          ENDIF
        END DO
        IF ( k == 0 ) THEN
          WRITE(*,*)'Error during the process of mutating cluster PLACE4'
          STOP
        ELSE
          l_old(k) = .FALSE.
          out_cluster%atoms(i)%xc = x_new(k)
          out_cluster%atoms(i)%yc = y_new(k)
          out_cluster%atoms(i)%zc = z_new(k)
        ENDIF
      ENDIF
      out_cluster%atoms(i)%symbol = in_cluster%atoms(i)%symbol
      out_cluster%atoms(i)%cstype = in_cluster%atoms(i)%cstype
    END DO

    out_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))
    out_cluster%edefn = 0

    DEALLOCATE(x_new)
    DEALLOCATE(y_new)
    DEALLOCATE(z_new)
    DEALLOCATE(l_old)
END SUBROUTINE mutateClusterGA

SUBROUTINE getRandomAtoms(inout_cluster,already_named)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    LOGICAL,INTENT(IN), OPTIONAL :: already_named

    INTEGER :: k

    IF (PRESENT(already_named).AND.already_named) THEN
    ELSE
      inout_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))
    ENDIF

    DO k = 1, N_ATTEMPTS_RNDM_STRUCT

      ! Randomly distributing atoms
      IF (RNDM_INI_BY_ATOM) THEN
        CALL randomiseAtoms1by1(inout_cluster)
      ELSE
        CALL randomiseAtoms(inout_cluster)
      ENDIF

      ! Geometrical check
      IF (.NOT. clusterGeometricalCheck(inout_cluster)) THEN

        CALL recentreCluster(inout_cluster)

        ! Why this is called?
        CALL enforceContainer(inout_cluster)

        IF (((JOB_TYPE == 2) .OR. (JOB_TYPE == 9)) .AND. GA_GEN_STATS) THEN
          inout_cluster%ga_stats%gaRandomAttempts = k
        ENDIF

        RETURN
      ENDIF

    END DO

    IF (((JOB_TYPE == 2) .OR. (JOB_TYPE == 9)) .AND. GA_GEN_STATS) THEN
      inout_cluster%ga_stats%gaRandomAttempts = k
      inout_cluster%ga_stats%gaOrigin = "FAILED"
    ENDIF

    WRITE(stderr,*)'Very unlucky with randomised initial structures or using a too small box!'
    WRITE(stderr,*) N_ATTEMPTS_RNDM_STRUCT, ' tries attempted!'

    DO k = 1,N_ATOMS
       WRITE(stderr,*)inout_cluster%atoms(k)%symbol,inout_cluster%atoms(k)%xc,inout_cluster%atoms(k)%yc,inout_cluster%atoms(k)%zc 
    END DO    
    
    STOP
END SUBROUTINE getRandomAtoms

SUBROUTINE randomiseAtoms(inout_cluster)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster

    REAL(KIND=DBL), DIMENSION(3) :: r   !Random number 3-D array
    INTEGER :: i,j

    CALL setDefaults(inout_cluster)
    !WRITE(stdout,*)'R_CENTRE_CLUSTER:',R_CENTRE_CLUSTER(:)
    IF (N_PBC == 0) THEN
      DO j = 1, N_ATOMS
        !CALL RANDOM_NUMBER(r)
         DO i = 1,3
           r(i)  = rand_num_uniform()
         END DO
        inout_cluster%atoms(j)%xc = (((r(1)*2)-1)*R_MAX_CLUSTER_BOUNDARY(1)) +R_CENTRE_CLUSTER(1)
        inout_cluster%atoms(j)%yc = (((r(2)*2)-1)*R_MAX_CLUSTER_BOUNDARY(2)) +R_CENTRE_CLUSTER(2)
        inout_cluster%atoms(j)%zc = (((r(3)*2)-1)*R_MAX_CLUSTER_BOUNDARY(3)) +R_CENTRE_CLUSTER(3)
      END DO
    ELSEIF (N_PBC == 3) THEN
      DO j = 1, N_ATOMS
        !CALL RANDOM_NUMBER(r)
         DO i = 1,3
           r(i)  = rand_num_uniform()
         END DO
        inout_cluster%atoms(j)%xf = r(1)
        inout_cluster%atoms(j)%yf = r(2)
        inout_cluster%atoms(j)%zf = r(3)
      END DO
      CALL xftoxc(inout_cluster) !convert fractionals to cartesian
    ELSE ! Not yet defined
      WRITE(stderr,*)'Need to update randomiseAtoms'
      STOP
    ENDIF
END SUBROUTINE randomiseAtoms

  !==========================================================================================!
  ! A routine to randomise cluster atoms by putting atom by atom
  !==========================================================================================!
  SUBROUTINE randomiseAtoms1by1(inout_cluster)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster

    REAL(KIND=DBL), DIMENSION(3) :: r   !Random number 3-D array
    INTEGER :: i, j, atomsPlaced, attempts
    LOGICAL :: randomiseAtom

    CALL setDefaults(inout_cluster)

    atomsPlaced = 0

    IF (N_PBC == 0) THEN
      DO i = 1, N_ATOMS

        atomsPlaced = atomsPlaced + 1
        attempts = 0
        randomiseAtom = .TRUE.

        DO WHILE ((randomiseAtom) .AND. (attempts < N_ATTEMPTS_RNDM_STRUCT))

          attempts = attempts + 1

          ! Randomising coordinates
          CALL RANDOM_NUMBER(r)

          DO j = 1, 3
           r(j) = rand_num_uniform()
          END DO

          inout_cluster%atoms(i)%xc = (((r(1)*2)-1)*R_MAX_CLUSTER_BOUNDARY(1)) +R_CENTRE_CLUSTER(1)
          inout_cluster%atoms(i)%yc = (((r(2)*2)-1)*R_MAX_CLUSTER_BOUNDARY(2)) +R_CENTRE_CLUSTER(2)
          inout_cluster%atoms(i)%zc = (((r(3)*2)-1)*R_MAX_CLUSTER_BOUNDARY(3)) +R_CENTRE_CLUSTER(3)

          ! Comparing against other atoms to see if the new atom does make the cluster fragmented etc.
          IF (i .EQ. 1) THEN
            randomiseAtom = .FALSE.
          ELSE
            randomiseAtom = clusterGeometricalCheckSingle(inout_cluster, i, &
              USE_VARIABLE_RAD_COLL, USE_VARIABLE_RAD_FRAG)
          ENDIF
        ENDDO
      ENDDO
    ELSE
      CALL print2All("randomiseAtoms1by1", "Works only with non-periodic structures.", 0)
      STOP
    ENDIF

  END SUBROUTINE randomiseAtoms1by1

  !==========================================================================================!
  !==========================================================================================!

SUBROUTINE randomiseCell(inout_cluster)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster

    REAL(KIND=DBL), DIMENSION(3) :: r   !Random number 3-D array
    REAL(KIND=DBL) :: R_ABC_RANGE = 0.2 !Max percentage change for a b c
    REAL(KIND=DBL) :: R_ANG_RANGE = 0.05!Max percentage change for angles
    REAL(KIND=DBL) :: a0,da
    INTEGER :: i

    a0 = 1.0 - R_ABC_RANGE
    da = R_ABC_RANGE + R_ABC_RANGE
    CALL RANDOM_NUMBER(r)
    DO i=1,3
      IF (CELL_FLAGS(i) == '1') THEN
        inout_cluster%box(i) = (a0 + r(i)*da) * inout_cluster%box(i)
      ENDIF
    ENDDO

    a0 = 1.0 - R_ANG_RANGE
    da = R_ANG_RANGE + R_ANG_RANGE
    CALL RANDOM_NUMBER(r)
    DO i=4,6
      IF (CELL_FLAGS(i) == '1') THEN
        inout_cluster%box(i) = (a0 + r(i-3)*da) * inout_cluster%box(i)
!       inout_cluster%box(i) = (60.0 + r(i-3)*60.0) would give a 60-120 deg range
      ENDIF
    ENDDO
    CALL lattice_c2v_3D(inout_cluster%box,VECTOR)

END SUBROUTINE randomiseCell

SUBROUTINE randomiseLocation(in_cluster, out_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    TYPE(cluster), INTENT(INOUT) :: out_cluster

    REAL(KIND=DBL), DIMENSION(3) :: r   !Random number 3-D array
    REAL(KIND=DBL) :: ri,xmin,xmax,ymin,ymax,zmin,zmax,xref,yref,zref
    INTEGER :: i

! locate extremes of cluster
    xmin = in_cluster%atoms(1)%xc
    xmax = xmin
    ymin = in_cluster%atoms(1)%yc
    ymax = ymin
    zmin = in_cluster%atoms(1)%zc
    zmax = zmin

    DO i=1, N_ATOMS
      ri = in_cluster%atoms(i)%xc 
      IF (ri < xmin) xmin = ri
      IF (ri > xmax) xmax = ri
      ri = in_cluster%atoms(i)%yc 
      IF (ri < ymin) ymin = ri
      IF (ri > ymax) ymax = ri
      ri = in_cluster%atoms(i)%zc 
      IF (ri < zmin) zmin = ri
      IF (ri > zmax) zmax = ri
    END DO

! calculate change in the frame of reference (move cluster into box)
    xref = R_CENTRE_CLUSTER(1) - 0.5*(xmax + xmin)
    yref = R_CENTRE_CLUSTER(2) - 0.5*(ymax + ymin)
    IF (L_ABOVE_SURFACE) THEN
      zref = R_CENTRE_CLUSTER(3) - R_MAX_CLUSTER_BOUNDARY(3) - zmin
    ELSE
      zref = R_CENTRE_CLUSTER(3) - 0.5*(zmax + zmin)
    ENDIF

! calculate max dimensions of cluster
    xmax = xmax - xmin
    ymax = ymax - ymin
    zmax = zmax - zmin

! calculate max displacement of cluster
    xmax = 2.0*R_MAX_CLUSTER_BOUNDARY(1) - xmax
    ymax = 2.0*R_MAX_CLUSTER_BOUNDARY(2) - ymax
    zmax = 2.0*R_MAX_CLUSTER_BOUNDARY(3) - zmax

! calculate fractional position of cluster in box
    CALL RANDOM_NUMBER(r) 

! calculate displacement of cluster
    IF (xmax > 0.0) THEN
      r(1) = xmax * (r(1)-0.5)
    ELSE
      IF (DEBUG_LEVEL /= 0) WRITE(stdsee,*)'cluster longer than box'
      r(1) = 0.0
    ENDIF
    IF (ymax > 0.0) THEN
      r(2) = ymax * (r(2)-0.5)
    ELSE
      IF (DEBUG_LEVEL /= 0) WRITE(stdsee,*)'cluster wider than box'
      r(2) = 0.0
    ENDIF
    IF (zmax > 0.0) THEN
      IF (L_ABOVE_SURFACE) THEN
        r(3) = zmax * r(3)
      ELSE
        r(3) = ymax * (r(3)-0.5)
      ENDIF
    ELSE
      IF (DEBUG_LEVEL /= 0) WRITE(stdsee,*)'cluster taller than box'
      r(3) = 0.0
    ENDIF

! adjust for zero size of cluster
    r(1) = r(1) + xref
    r(2) = r(2) + yref
    r(3) = r(3) + zref

    CALL setDefaults(out_cluster)
    CALL copyBoxSize(in_cluster,out_cluster)

    DO i=1, N_ATOMS
      out_cluster%atoms(i)%symbol = in_cluster%atoms(i)%symbol
      out_cluster%atoms(i)%cstype = in_cluster%atoms(i)%cstype
      out_cluster%atoms(i)%xc = in_cluster%atoms(i)%xc + r(1)
      out_cluster%atoms(i)%yc = in_cluster%atoms(i)%yc + r(2)
      out_cluster%atoms(i)%zc = in_cluster%atoms(i)%zc + r(3)
    END DO

    CALL xctoxf(out_cluster)

    out_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))

END SUBROUTINE randomiseLocation

SUBROUTINE randomiseCluster(in_cluster, out_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    TYPE(cluster), INTENT(INOUT) :: out_cluster

    INTEGER :: i

    DO i=1, N_ATOMS
      out_cluster%atoms(i)%symbol = in_cluster%atoms(i)%symbol
      out_cluster%atoms(i)%cstype = in_cluster%atoms(i)%cstype
    END DO

    CALL defineClusterBox(out_cluster)
    CALL randomiseCell(out_cluster)

    CALL randomiseAtoms(out_cluster)

    out_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))

    !IF (L_ENFORCE_CONTAINER) CALL inclusionZone(out_cluster)
END SUBROUTINE randomiseCluster

SUBROUTINE moveAtoms(in_cluster, out_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    TYPE(cluster), INTENT(INOUT) :: out_cluster

    REAL(KIND=DBL), DIMENSION(3) :: r   !Random number 3-D array
    INTEGER :: i

    CALL setDefaults(out_cluster)
    CALL copyBoxSize(in_cluster, out_cluster)

    IF (L_ABOVE_SURFACE) THEN ! MC move constrained within hemisphere
      DO i=1, N_ATOMS
        CALL RANDOM_NUMBER(r)
        out_cluster%atoms(i)%symbol = in_cluster%atoms(i)%symbol
        out_cluster%atoms(i)%cstype = in_cluster%atoms(i)%cstype
        out_cluster%atoms(i)%xc = in_cluster%atoms(i)%xc + ((r(1)*2)-1) * R_BH_STEPSIZE
        out_cluster%atoms(i)%yc = in_cluster%atoms(i)%yc + ((r(2)*2)-1) * R_BH_STEPSIZE
        out_cluster%atoms(i)%zc = in_cluster%atoms(i)%zc +   r(3)       * R_BH_STEPSIZE
      END DO
    ELSE ! standard MC move within a spherical radius of R_BH_STEPSIZE
      DO i=1, N_ATOMS
        CALL RANDOM_NUMBER(r)
        out_cluster%atoms(i)%symbol = in_cluster%atoms(i)%symbol
        out_cluster%atoms(i)%cstype = in_cluster%atoms(i)%cstype
        out_cluster%atoms(i)%xc = in_cluster%atoms(i)%xc + ((r(1)*2)-1) * R_BH_STEPSIZE
        out_cluster%atoms(i)%yc = in_cluster%atoms(i)%yc + ((r(2)*2)-1) * R_BH_STEPSIZE
        out_cluster%atoms(i)%zc = in_cluster%atoms(i)%zc + ((r(3)*2)-1) * R_BH_STEPSIZE
      END DO
    ENDIF

    IF (L_ENFORCE_CONTAINER) CALL inclusionZone(out_cluster)
    CALL xctoxf(out_cluster)

    out_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))

END SUBROUTINE moveAtoms

SUBROUTINE swapAtoms(in_cluster, out_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    TYPE(cluster), INTENT(INOUT) :: out_cluster

    INTEGER, DIMENSION(:), ALLOCATABLE :: atomOrder

    REAL(KIND=DBL) :: r_random=0. 
    INTEGER :: i, j, k

    ALLOCATE(atomOrder(N_ATOMS))
    DO i=1, N_ATOMS
        atomOrder(i) = i
    ENDDO
     
    DO i=1, N_ATOMS
      CALL RANDOM_NUMBER(r_random)
      IF (r_random > 0.8) THEN
        j = randomInteger(1,N_ATOMS)
        k = atomOrder(i) 
        atomOrder(i) = atomOrder(j)
        atomOrder(j) = k
      ENDIF
    ENDDO

    CALL setDefaults(out_cluster)
    CALL copyBoxSize(in_cluster, out_cluster)

    DO i=1, N_ATOMS
        j = atomOrder(i)
        out_cluster%atoms(i)%symbol = in_cluster%atoms(i)%symbol
        out_cluster%atoms(i)%cstype = in_cluster%atoms(i)%cstype
        out_cluster%atoms(i)%xc = in_cluster%atoms(j)%xc
        out_cluster%atoms(i)%yc = in_cluster%atoms(j)%yc
        out_cluster%atoms(i)%zc = in_cluster%atoms(j)%zc
        out_cluster%atoms(i)%xf= in_cluster%atoms(j)%xf
        out_cluster%atoms(i)%yf= in_cluster%atoms(j)%yf
        out_cluster%atoms(i)%zf= in_cluster%atoms(j)%zf
    END DO

    out_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))

    DEALLOCATE(atomOrder)
END SUBROUTINE swapAtoms

SUBROUTINE swapCations(in_cluster, out_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    TYPE(cluster), INTENT(INOUT) :: out_cluster

    INTEGER, DIMENSION(:), ALLOCATABLE :: atomOrder

    REAL(KIND=DBL) :: r_random=0. 
    INTEGER :: i, j, k

    ALLOCATE(atomOrder(N_CATIONS))
    DO i=1, N_CATIONS
        atomOrder(i) = i
    ENDDO

    DO i=1, N_CATIONS
      CALL RANDOM_NUMBER(r_random)
      IF (r_random > 0.8) THEN
        j = randomInteger(1,N_CATIONS)
        k = atomOrder(i)
        atomOrder(i) = atomOrder(j)
        atomOrder(j) = k
      ENDIF
    ENDDO

    CALL setDefaults(out_cluster)
    CALL copyBoxSize(in_cluster, out_cluster)

    DO i=1, N_CATIONS
        j = atomOrder(i)
        out_cluster%atoms(i)%symbol = in_cluster%atoms(i)%symbol
        out_cluster%atoms(i)%cstype = in_cluster%atoms(i)%cstype
        out_cluster%atoms(i)%xc = in_cluster%atoms(j)%xc
        out_cluster%atoms(i)%yc = in_cluster%atoms(j)%yc
        out_cluster%atoms(i)%zc = in_cluster%atoms(j)%zc
        out_cluster%atoms(i)%xf = in_cluster%atoms(j)%xf
        out_cluster%atoms(i)%yf = in_cluster%atoms(j)%yf
        out_cluster%atoms(i)%zf = in_cluster%atoms(j)%zf
    END DO
    DO i=N_CATIONS+1,N_ATOMS
        j = i
        out_cluster%atoms(i)%cstype = in_cluster%atoms(i)%cstype
        out_cluster%atoms(i)%symbol = in_cluster%atoms(i)%symbol
        out_cluster%atoms(i)%xc = in_cluster%atoms(j)%xc
        out_cluster%atoms(i)%yc = in_cluster%atoms(j)%yc
        out_cluster%atoms(i)%zc = in_cluster%atoms(j)%zc
        out_cluster%atoms(i)%xf = in_cluster%atoms(j)%xf
        out_cluster%atoms(i)%yf = in_cluster%atoms(j)%yf
        out_cluster%atoms(i)%zf = in_cluster%atoms(j)%zf
    END DO

    out_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))

    DEALLOCATE(atomOrder)
END SUBROUTINE swapCations

SUBROUTINE twistCluster(in_cluster, out_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    TYPE(cluster), INTENT(INOUT) :: out_cluster

    REAL(KIND=DBL), DIMENSION(:), ALLOCATABLE :: x_new, y_new, z_new

    REAL(KIND=DBL), DIMENSION(3) :: r   !Random number 3-D array
    REAL(KIND=DBL) :: r_random=0.
    REAL(KIND=DBL) :: x_old, y_old, z_old, xy_new, z_min, z_max
    REAL(KIND=DBL) :: cos_phi, cos_eta, cos_psi
    REAL(KIND=DBL) :: sin_phi, sin_eta, sin_psi
    INTEGER :: i

    CALL setDefaults(out_cluster)
    CALL copyBoxSize(in_cluster, out_cluster)

    ALLOCATE(x_new(N_ATOMS))
    ALLOCATE(y_new(N_ATOMS))
    ALLOCATE(z_new(N_ATOMS))

    CALL RANDOM_NUMBER(r) ! define two angles of rotation and one for twist
    cos_phi=COS(twoPI*r(1))
    sin_phi=SIN(twoPI*r(1))
    cos_eta=COS(twoPI*r(2))
    sin_eta=SIN(twoPI*r(2))
    cos_psi=COS(twoPI*r(3))
    sin_psi=SIN(twoPI*r(3))

    DO i=1, N_ATOMS
        x_old  = in_cluster%atoms(i)%xc
        y_old  = in_cluster%atoms(i)%yc
        z_old  = in_cluster%atoms(i)%zc
        xy_new  =  sin_eta*x_old  + cos_eta*y_old
        x_new(i) = cos_eta*x_old  - sin_eta*y_old
        y_new(i) = cos_phi*xy_new - sin_phi*z_old
        z_new(i) = sin_phi*xy_new + cos_phi*z_old
    END DO

    CALL RANDOM_NUMBER(r_random) !  define fraction of cluster to twist
    z_min =  R_MAX_CLUSTER_BOUNDARY(3)
    z_max = -R_MAX_CLUSTER_BOUNDARY(3)
    DO i=1, N_ATOMS
      IF ( z_new(i) < z_min ) z_min = z_new(i)
      IF ( z_new(i) > z_max ) z_max = z_new(i)
    END DO
    xy_new = z_min + ((0.1 + r_random*0.8)*(z_max-z_min))
    DO i=1, N_ATOMS
      out_cluster%atoms(i)%symbol = in_cluster%atoms(i)%symbol
      out_cluster%atoms(i)%cstype = in_cluster%atoms(i)%cstype
      IF ( z_new(i) < xy_new ) THEN ! twist atoms about z-axis by psi 
        out_cluster%atoms(i)%xc = cos_psi*x_new(i) - sin_psi*y_new(i)
        out_cluster%atoms(i)%yc = sin_psi*x_new(i) + cos_psi*y_new(i)
      ELSE
        out_cluster%atoms(i)%xc = x_new(i)
        out_cluster%atoms(i)%yc = y_new(i)
      ENDIF
      out_cluster%atoms(i)%zc = z_new(i)
    END DO

    CALL xctoxf(out_cluster)

    out_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))

    DEALLOCATE(x_new)
    DEALLOCATE(y_new)
    DEALLOCATE(z_new)
END SUBROUTINE twistCluster

SUBROUTINE mutateCluster(in_cluster, out_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    TYPE(cluster), INTENT(INOUT) :: out_cluster

    REAL(KIND=DBL), DIMENSION(:), ALLOCATABLE :: x_new, y_new, z_new
    LOGICAL, DIMENSION(:), ALLOCATABLE :: l_old

    REAL(KIND=DBL), DIMENSION(5) :: r   !Random number 3-D array
    CHARACTER(LEN=2) :: element='--'
    REAL(KIND=DBL) :: x_old, y_old, z_old, xy_old, z_min, z_max
    REAL(KIND=DBL) :: cos_phi, cos_eta, sin_phi, sin_eta
    INTEGER :: i, j, k

    CALL setDefaults(out_cluster)
    CALL copyBoxSize(in_cluster, out_cluster)

    ALLOCATE(l_old(N_ATOMS))
    ALLOCATE(x_new(N_ATOMS))
    ALLOCATE(y_new(N_ATOMS))
    ALLOCATE(z_new(N_ATOMS))

    CALL RANDOM_NUMBER(r) ! define angles of rotation and cutting point
    do i=1,4
      r(i)=twoPI*r(i)
    enddo

    cos_phi=COS(r(1))
    sin_phi=SIN(r(1))
    cos_eta=COS(r(2))
    sin_eta=SIN(r(2))

    DO i=1, N_ATOMS
        x_old  = in_cluster%atoms(i)%xc
        y_old  = in_cluster%atoms(i)%yc
        z_old  = in_cluster%atoms(i)%zc
        xy_old =                  sin_eta*x_old  + cos_eta*y_old
        out_cluster%atoms(i)%xc = cos_eta*x_old  - sin_eta*y_old
        out_cluster%atoms(i)%yc = cos_phi*xy_old - sin_phi*z_old
        out_cluster%atoms(i)%zc = sin_phi*xy_old + cos_phi*z_old
    END DO

    cos_phi=COS(r(3))
    sin_phi=SIN(r(3))
    cos_eta=COS(r(4))
    sin_eta=SIN(r(4))

    DO i=1, N_ATOMS
        x_old  = in_cluster%atoms(i)%xc
        y_old  = in_cluster%atoms(i)%yc
        z_old  = in_cluster%atoms(i)%zc
        xy_old  =  sin_eta*x_old  + cos_eta*y_old
        x_new(i) = cos_eta*x_old  - sin_eta*y_old
        y_new(i) = cos_phi*xy_old - sin_phi*z_old
        z_new(i) = sin_phi*xy_old + cos_phi*z_old
    END DO

    z_min =  R_MAX_CLUSTER_BOUNDARY(3)
    z_max = -R_MAX_CLUSTER_BOUNDARY(3)
    DO i=1, N_ATOMS
      IF ( z_new(i) < z_min ) z_min = z_new(i)
      IF ( z_new(i) > z_max ) z_max = z_new(i)
      l_old(i) = .TRUE.
    END DO
    xy_old = z_min + ((0.1 + 0.8*r(5))*(z_max-z_min))

    DO i=1, N_ATOMS
      IF ( out_cluster%atoms(i)%zc < xy_old ) THEN ! replace atom with nearest second copy
        k=0
        z_min=z_max+1.0
        element = in_cluster%atoms(i)%symbol
        DO j=1, N_ATOMS
          IF (l_old(j)) THEN ! can still use this atom as a replacement
            IF (index(in_cluster%atoms(j)%symbol,element).ne.0) THEN ! same species
              IF ( z_new(j) < z_min ) THEN
                k=j
                z_min=z_new(j)
              ENDIF
            ENDIF
          ENDIF
        END DO
        IF ( k == 0 ) THEN
          WRITE(*,*)'Error during the process of mutating cluster PLACE5'
          STOP
        ELSE
          l_old(k) = .FALSE.
          out_cluster%atoms(i)%xc = x_new(k)
          out_cluster%atoms(i)%yc = y_new(k)
          out_cluster%atoms(i)%zc = z_new(k)
        ENDIF
      ENDIF
      out_cluster%atoms(i)%symbol = in_cluster%atoms(i)%symbol
      out_cluster%atoms(i)%cstype = in_cluster%atoms(i)%cstype
    END DO

    CALL xctoxf(out_cluster)

    out_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))

    DEALLOCATE(z_new)
    DEALLOCATE(y_new)
    DEALLOCATE(x_new)
    DEALLOCATE(l_old)
END SUBROUTINE mutateCluster

SUBROUTINE oldtranslateCluster(in_cluster, out_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    TYPE(cluster), INTENT(INOUT) :: out_cluster

    REAL(KIND=DBL), DIMENSION(3) :: r   !Random number 3-D array
    INTEGER :: i

    CALL setDefaults(out_cluster)
    CALL copyBoxSize(in_cluster, out_cluster)

    IF (PROB_TRANS_SURFACE == 0.) RETURN

    CALL RANDOM_NUMBER(r) ! define three displacements

    IF (L_ABOVE_SURFACE) THEN ! MC move constrained within plane parallel to surface
      DO i=1, N_ATOMS
        out_cluster%atoms(i)%symbol = in_cluster%atoms(i)%symbol
        out_cluster%atoms(i)%cstype = in_cluster%atoms(i)%cstype
        out_cluster%atoms(i)%xc = in_cluster%atoms(i)%xc + ((r(1)*2)-1) * R_BH_STEPSIZE
        out_cluster%atoms(i)%yc = in_cluster%atoms(i)%yc + ((r(2)*2)-1) * R_BH_STEPSIZE
        out_cluster%atoms(i)%zc = in_cluster%atoms(i)%zc 
      END DO
    ELSE ! standard MC move within a spherical radius of R_BH_STEPSIZE
      DO i=1, N_ATOMS
        out_cluster%atoms(i)%symbol = in_cluster%atoms(i)%symbol
        out_cluster%atoms(i)%cstype = in_cluster%atoms(i)%cstype
        out_cluster%atoms(i)%xc = in_cluster%atoms(i)%xc + ((r(1)*2)-1) * R_BH_STEPSIZE
        out_cluster%atoms(i)%yc = in_cluster%atoms(i)%yc + ((r(2)*2)-1) * R_BH_STEPSIZE
        out_cluster%atoms(i)%zc = in_cluster%atoms(i)%zc + ((r(3)*2)-1) * R_BH_STEPSIZE
      END DO
    ENDIF

    CALL xctoxf(out_cluster)

    out_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))

END SUBROUTINE oldtranslateCluster

SUBROUTINE translateCluster(in_cluster, out_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    TYPE(cluster), INTENT(INOUT) :: out_cluster

    REAL(KIND=DBL), DIMENSION(3) :: r   !Random number 3-D array
    REAL(KIND=DBL) :: zmin ! lowest part of cluster
    REAL(KIND=DBL) :: z_edge ! edge of surface
    INTEGER :: i

    CALL setDefaults(out_cluster)
    CALL copyBoxSize(in_cluster, out_cluster)

    IF (PROB_TRANS_SURFACE == 0.) RETURN

    IF (L_ABOVE_SURFACE) THEN ! locate lowest part of cluster
      zmin = in_cluster%atoms(1)%zc
      DO i=2, N_ATOMS
        IF (in_cluster%atoms(i)%zc < zmin) zmin = in_cluster%atoms(i)%zc
      END DO
    ENDIF

    CALL RANDOM_NUMBER(r) ! define three displacements

    ! Ideally should have a larger step size, for example
    ! r(1)=R_MAX_CLUSTER_BOUNDARY(1)*R_BH_STEPSIZE*(r(1)-0.5)
    ! r(2)=R_MAX_CLUSTER_BOUNDARY(2)*R_BH_STEPSIZE*(r(2)-0.5)
    ! r(3)=R_MAX_CLUSTER_BOUNDARY(3)*R_BH_STEPSIZE*(r(3)-0.5)
    ! and perhaps rescaled by input parameter R_SCALE_STEP

    ! Could also reduce step in x or y if cluster tries to leave box

    r(1)=2.0*R_BH_STEPSIZE*(r(1)-0.5)
    r(2)=2.0*R_BH_STEPSIZE*(r(2)-0.5)
    r(3)=2.0*R_BH_STEPSIZE*(r(3)-0.5)

    IF (L_ABOVE_SURFACE) THEN
      z_edge = R_CENTRE_CLUSTER(3)-R_MAX_CLUSTER_BOUNDARY(3)
      IF (zmin+r(3) < z_edge)THEN
        r(3) = z_edge - zmin
      ENDIF
    ENDIF
    
    DO i=1, N_ATOMS
      out_cluster%atoms(i)%symbol = in_cluster%atoms(i)%symbol
      out_cluster%atoms(i)%cstype = in_cluster%atoms(i)%cstype
      out_cluster%atoms(i)%xc = in_cluster%atoms(i)%xc + r(1)
      out_cluster%atoms(i)%yc = in_cluster%atoms(i)%yc + r(2)
      out_cluster%atoms(i)%zc = in_cluster%atoms(i)%zc + r(3)
    ENDDO

    CALL xctoxf(out_cluster)

    out_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))

END SUBROUTINE translateCluster

  !==========================================================================================
  ! Rotates a cluster around Z axis
  !==========================================================================================
  SUBROUTINE rotateClusterZAxis(inout_cluster)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster

    REAL(KIND=DBL) :: r, r_2pi, cos_phi, sin_phi, x_old, y_old
    INTEGER :: i

    CALL RANDOM_NUMBER(r)
    r_2pi = r * twoPI

    cos_phi = COS(r_2pi)
    sin_phi = SIN(r_2pi)

    DO i = 1, N_ATOMS
      x_old = inout_cluster%atoms(i)%xc
      y_old = inout_cluster%atoms(i)%yc

      inout_cluster%atoms(i)%xc = cos_phi*x_old - sin_phi*y_old
      inout_cluster%atoms(i)%yc = sin_phi*x_old + cos_phi*y_old
    ENDDO

  END SUBROUTINE rotateClusterZAxis

  !==========================================================================================

SUBROUTINE rotateCluster(in_cluster, out_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    TYPE(cluster), INTENT(INOUT) :: out_cluster

    REAL(KIND=DBL), DIMENSION(3) :: r   !Random number 3-D array
    REAL(KIND=DBL) :: r_random=0.
    REAL(KIND=DBL) :: x_old, y_old, z_old, x_min, y_min, z_min, x_max, y_max, z_max
    REAL(KIND=DBL) :: cos_phi, cos_eta, sin_phi, sin_eta
    REAL(KIND=DBL) :: compn, x_medium, y_medium, z_medium, xy_new
    INTEGER :: i

    CALL setDefaults(out_cluster)
    CALL copyBoxSize(in_cluster, out_cluster)

    IF (L_FIX_ALLIGNMENT) THEN ! only rotate about z-axis

     CALL RANDOM_NUMBER(r) ! require one angle of rotation
     cos_eta=COS(twoPI*r(1))
     sin_eta=SIN(twoPI*r(1))

     x_min =  in_cluster%atoms(1)%xc
     y_min =  in_cluster%atoms(1)%yc
     x_max =  x_min
     y_max =  y_min
     DO i=2, N_ATOMS
        compn=in_cluster%atoms(i)%xc
        IF ( compn < x_min ) x_min = compn
        IF ( compn > x_max ) x_max = compn
        compn=in_cluster%atoms(i)%yc
        IF ( compn < y_min ) y_min = compn
        IF ( compn > y_max ) y_max = compn
     END DO

     x_medium = 0.5*(x_min + x_max)
     y_medium = 0.5*(y_min + y_max)

!    Recentre to origin before rotating and then move back
     DO i=1, N_ATOMS
        x_old  = in_cluster%atoms(i)%xc - x_medium
        y_old  = in_cluster%atoms(i)%yc - y_medium
        xy_new  =  sin_eta*x_old  + cos_eta*y_old
        out_cluster%atoms(i)%xc = cos_eta*x_old  - sin_eta*y_old + x_medium
        out_cluster%atoms(i)%yc = sin_eta*x_old  + cos_eta*y_old + y_medium
        out_cluster%atoms(i)%zc = in_cluster%atoms(1)%zc
     END DO

    ELSE 

     CALL RANDOM_NUMBER(r) ! require two angles of rotation
     cos_phi=COS(twoPI*r(1))
     sin_phi=SIN(twoPI*r(1))
     cos_eta=COS(twoPI*r(2))
     sin_eta=SIN(twoPI*r(2))

     x_min =  in_cluster%atoms(1)%xc
     y_min =  in_cluster%atoms(1)%yc
     z_min =  in_cluster%atoms(1)%zc
     x_max =  x_min
     y_max =  y_min
     z_max =  z_min
     DO i=2, N_ATOMS
        compn=in_cluster%atoms(i)%xc
        IF ( compn < x_min ) x_min = compn
        IF ( compn > x_max ) x_max = compn
        compn=in_cluster%atoms(i)%yc
        IF ( compn < y_min ) y_min = compn
        IF ( compn > y_max ) y_max = compn
        compn=in_cluster%atoms(i)%zc
        IF ( compn < z_min ) z_min = compn
        IF ( compn > z_max ) z_max = compn
     END DO

     x_medium = 0.5*(x_min + x_max)
     y_medium = 0.5*(y_min + y_max)
     z_medium = 0.5*(z_min + z_max)

!    Recentre to origin before rotating and then move back
     DO i=1, N_ATOMS
        x_old  = in_cluster%atoms(i)%xc - x_medium
        y_old  = in_cluster%atoms(i)%yc - y_medium
        z_old  = in_cluster%atoms(i)%zc - z_medium
        xy_new  =  sin_eta*x_old  + cos_eta*y_old
        out_cluster%atoms(i)%xc = cos_eta*x_old  - sin_eta*y_old + x_medium
        out_cluster%atoms(i)%yc = cos_phi*xy_new - sin_phi*z_old + y_medium
        out_cluster%atoms(i)%zc = sin_phi*xy_new + cos_phi*z_old + z_medium
     END DO
 
     x_min =  out_cluster%atoms(1)%zc
     x_max =  x_min
     DO i=2, N_ATOMS
        compn=in_cluster%atoms(i)%zc
        IF ( compn < x_min ) x_min = compn
        IF ( compn > x_max ) x_max = compn
     END DO

     x_medium = z_min - x_min ! move cluster so that min distance of any atom unchanged

     DO i=1, N_ATOMS
        out_cluster%atoms(i)%symbol = in_cluster%atoms(i)%symbol
        out_cluster%atoms(i)%cstype = in_cluster%atoms(i)%cstype
        out_cluster%atoms(i)%zc = out_cluster%atoms(i)%zc + x_medium
     END DO

    ENDIF

    CALL xctoxf(out_cluster)

    out_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))

END SUBROUTINE rotateCluster

SUBROUTINE randomiseSolution(in_cluster, out_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    TYPE(cluster), INTENT(INOUT) :: out_cluster

    INTEGER, DIMENSION(:), ALLOCATABLE :: atomOrder,p_1,m_1
    LOGICAL, DIMENSION(:), ALLOCATABLE :: notMoved

    REAL(KIND=DBL) :: r_random=0. 
    INTEGER :: i,j,k,M_ATOMS,p1,i0,nm,nr
    CHARACTER(LEN=1) :: sr=' '

    ALLOCATE(atomOrder(N_ATOMS))
    ALLOCATE(p_1(N_ATOMS))

    DO i=1,6
      out_cluster%box(i) = in_cluster%box(i)
    ENDDO

    IF (N_REGIONS == 0) THEN

      DO i=1, N_ATOMS
        p_1(i) = i ! points to all available atoms
      ENDDO

      M_ATOMS = N_ATOMS
      DO i=1, N_ATOMS-1
        j = randomInteger(1,M_ATOMS)
        atomOrder(i) = p_1(j)
        M_ATOMS=M_ATOMS-1
        DO k=j,M_ATOMS
          p_1(k)=p_1(k+1)
        ENDDO
      ENDDO
      atomOrder(N_ATOMS) = p_1(1)

    ELSE ! MIX DIFFERENT REGIONS SEPARATELY

      i0 = 0
      DO nr = 0, N_REGIONS

        IF (nr==0) THEN
          DO i=i0+1, i0+N_ATOMS_R(nr)
            atomOrder(i) = i ! no change in order within this region
          ENDDO

        ELSEIF (L_FIX_R(nr)) THEN
           DO i=i0+1, i0+N_ATOMS_R(nr)
            atomOrder(i) = i ! no change in order within this region
          ENDDO

        ELSE

          DO i=1, N_ATOMS_R(nr)
            p_1(i) = i0 + i ! points to all available atoms in Region nr
          ENDDO

          M_ATOMS = N_ATOMS_R(nr)
          DO i=1, N_ATOMS_R(nr)-1
            j = randomInteger(1,M_ATOMS)
            atomOrder(i0+i) = p_1(j)
            M_ATOMS=M_ATOMS-1
            DO k=j,M_ATOMS
              p_1(k)=p_1(k+1)
            ENDDO
          ENDDO
          atomOrder(i0+N_ATOMS_R(nr)) = p_1(1)

        ENDIF
        i0 = i0 + N_ATOMS_R(nr)
      ENDDO

    ENDIF

    IF (N_MIX /=0 ) THEN ! MIXING BETWEEN REGIONS REQUIRED

      ALLOCATE(m_1(N_ATOMS))
      ALLOCATE(notMoved(N_ATOMS))

      DO i=1,N_ATOMS
        notMoved(i)=.TRUE.
      ENDDO

      DO nm=1, N_MIX ! loop over mixing rules

        nr = T_MIX(nm)%from
        i0 = 0
        DO i=0,nr-1
          i0 = i0 + N_ATOMS_R(i)
        ENDDO

        M_ATOMS=0 ! increase to include all possible sites that can move "from" region
        DO i=1, N_ATOMS_R(nr)
          ! j = atomOrder(i0+i) and references to i0+i below could be replaced by j
          IF (notMoved(i0+i)) THEN
            M_ATOMS = M_ATOMS + 1
            p_1(M_ATOMS) = i0 + i ! points to all available "from" sites in Region nr
          ENDIF
        ENDDO

        DO i=1, T_MIX(nm)%nexchg
          j = randomInteger(1,M_ATOMS)
          m_1(i) = p_1(j) ! points to chosen "from" sites that will later be moved
          M_ATOMS=M_ATOMS-1
          DO k=j,M_ATOMS
            p_1(k)=p_1(k+1)
          ENDDO
        ENDDO

        i0 = 0
        M_ATOMS=0 ! increase to include all possible destination sites within "to" regions
        DO nr=0,N_REGIONS
          CALL num2str(nr,1,sr)
          IF (index(T_MIX(nm)%to,sr).ne.0) THEN
            DO i=1, N_ATOMS_R(nr)
              IF (notMoved(i0+i)) THEN
                M_ATOMS = M_ATOMS + 1
                p_1(M_ATOMS) = i0 + i ! points to available destination sites
              ENDIF
            ENDDO
          ENDIF
          i0 = i0 + N_ATOMS_R(i)
        ENDDO

        DO i=1, T_MIX(nm)%nexchg
          j = randomInteger(1,M_ATOMS)

          k = atomOrder(p_1(j))
          atomOrder(p_1(j)) = atomOrder(m_1(i))
          atomOrder(m_1(i)) = k
          IF (in_cluster%atoms(atomOrder(m_1(i)))%site /= in_cluster%atoms(k)%site) THEN
            notMoved(p_1(j))=.FALSE.
            notMoved(m_1(i))=.FALSE.
          ENDIF

          M_ATOMS=M_ATOMS-1
          DO k=j,M_ATOMS
            p_1(k)=p_1(k+1)
          ENDDO
        ENDDO
      ENDDO

      DEALLOCATE(m_1)
      DEALLOCATE(notMoved)

    ENDIF

    CALL setDefaults(out_cluster)
    CALL copyBoxSize(in_cluster, out_cluster)
     
    DO i=1, N_ATOMS
      j = atomOrder(i)
      out_cluster%atoms(i)%symbol = in_cluster%atoms(i)%symbol
      out_cluster%atoms(i)%cstype = in_cluster%atoms(i)%cstype
      out_cluster%atoms(i)%site = in_cluster%atoms(j)%site
      out_cluster%atoms(i)%xc = in_cluster%atoms(j)%xc
      out_cluster%atoms(i)%yc = in_cluster%atoms(j)%yc
      out_cluster%atoms(i)%zc = in_cluster%atoms(j)%zc
      out_cluster%atoms(i)%xf = in_cluster%atoms(j)%xf
      out_cluster%atoms(i)%yf = in_cluster%atoms(j)%yf
      out_cluster%atoms(i)%zf = in_cluster%atoms(j)%zf
    END DO

    out_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))

    DEALLOCATE(p_1)
    DEALLOCATE(atomOrder)
END SUBROUTINE randomiseSolution

SUBROUTINE mixSolution(in_exchg, in_cluster, out_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    TYPE(cluster), INTENT(INOUT) :: out_cluster
    INTEGER, INTENT(IN) :: in_exchg

    INTEGER, DIMENSION(:), ALLOCATABLE :: atomOrder,p_1,p_2,r_1
    INTEGER, DIMENSION(:), ALLOCATABLE :: M_ATOMS_R

    INTEGER :: i,j,k,M_ATOMS,p1,p2,i0,n_exchg,nr

    out_cluster = in_cluster
    CALL setDefaults(out_cluster)

    ALLOCATE(p_1(N_ATOMS))
    ALLOCATE(p_2(N_ATOMS))
    ALLOCATE(atomOrder(N_ATOMS))
    DO i=1, N_ATOMS
      atomOrder(i) = i
    ENDDO

    IF (N_REGIONS == 0) THEN

      DO i=1, N_ATOMS
        p_1(i) = i ! points to all available atoms
      ENDDO

      M_ATOMS = N_ATOMS
      DO n_exchg = 1, in_exchg

      ! This check could be moved into separate sub called once from runSolidSolution
        IF (M_ATOMS.lt.2) THEN
          WRITE(stdsee,*)'Error - impossible to mix solution'
          WRITE(stderr,*)'Data: ',n_exchg,in_exchg,M_ATOMS
          STOP
        ENDIF

        j = randomInteger(1,M_ATOMS)

        p1 = p_1(j) ! first atom defined
        M_ATOMS=M_ATOMS-1
        DO k=j,M_ATOMS
          p_1(k)=p_1(k+1)
        ENDDO

        k=0
        DO j=1,M_ATOMS
          IF (in_cluster%atoms(p1)%symbol.ne.in_cluster%atoms(p_1(j))%symbol) THEN
              k=k+1
              p_2(k)=p_1(j)
          ELSEIF(in_cluster%atoms(p1)%cstype.ne.in_cluster%atoms(p_1(j))%cstype)THEN
              k=k+1
              p_2(k)=p_1(j)
          ENDIF
        ENDDO
      ! WRITE(6,*)'Found ',k,' possible replacements sites'
        IF (k.eq.0) THEN
          WRITE(stdsee,*)'Error - impossible to mix solution'
          WRITE(stderr,*)'Data: ',i,p1,in_cluster%atoms(p1)%symbol
          STOP
        ENDIF

        j = randomInteger(1,k)
        p2 = p_2(j) ! second atom defined

        k = atomOrder(p1) 
        atomOrder(p1) = atomOrder(p2)
        atomOrder(p2) = k

        IF (n_exchg /= in_exchg) THEN
          IF (L_MC_RESET) THEN
            M_ATOMS = N_ATOMS
            DO i=1, N_ATOMS
              p_1(i) = i ! reset all atoms available for next round
            ENDDO
          ELSE
            DO k=j,M_ATOMS
              IF (p_1(k) > p2) p_1(k-1)=p_1(k)
            ENDDO
            M_ATOMS=M_ATOMS-1
          ENDIF
        ENDIF
      ENDDO

    ELSE ! MIX DIFFERENT REGIONS SEPARATELY

      ALLOCATE(r_1(N_ATOMS))
      ALLOCATE(M_ATOMS_R(N_REGIONS))
      DO n_exchg = 1, in_exchg

        IF (n_exchg==1 .OR. L_MC_RESET) THEN
          M_ATOMS = 0
          i0 = N_ATOMS_R(0)
          DO nr=1, N_REGIONS
            IF (L_FIX_R(nr) .OR. N_ATOMS_R(nr)<2) THEN
              M_ATOMS_R(nr) = 0
            ELSE
              M_ATOMS_R(nr) = N_ATOMS_R(nr)
              DO i=1, M_ATOMS_R(nr)
                p_1(i+M_ATOMS) = i0 + i ! points to all available atoms in Region nr
                r_1(i+M_ATOMS) = nr     ! points to the region available atom is from
              ENDDO
              M_ATOMS = M_ATOMS + M_ATOMS_R(nr)
            ENDIF
            i0 = i0 + N_ATOMS_R(nr)
          ENDDO
        ELSE
          IF (M_ATOMS_R(nr) == 1) THEN
            M_ATOMS = M_ATOMS - 2
            DO k=i0+1,M_ATOMS
              p_1(k)=p_1(k+2)
              r_1(k)=r_1(k+2)
            ENDDO
            M_ATOMS_R(nr) = 0
          ELSE
            DO k=i0+j,M_ATOMS
              IF (p_1(k) > p2) THEN
                p_1(k-1)=p_1(k)
                r_1(k-1)=r_1(k)
              ENDIF
            ENDDO
            M_ATOMS = M_ATOMS - 1
            M_ATOMS_R(nr) = M_ATOMS_R(nr) - 1
          ENDIF
          IF (M_ATOMS < 2) THEN
            WRITE(stderr,*)'Error - impossible to mix solution'
            WRITE(stdsee,*)'Error - impossible to mix solution'
            STOP
          ENDIF
        ENDIF

      ! WRITE(*,*)M_ATOMS
      ! DO k=1,M_ATOMS
      !   WRITE(*,*)p_1(k),r_1(k)
      ! ENDDO

        j = randomInteger(1,M_ATOMS)
        p1 = p_1(j) ! first atom defined
        nr = r_1(j) ! find second atom from this region

        M_ATOMS = M_ATOMS - 1
        DO k=j,M_ATOMS
          p_1(k)=p_1(k+1)
          r_1(k)=r_1(k+1)
        ENDDO
        M_ATOMS_R(nr) = M_ATOMS_R(nr) - 1

        i0 = 0
        DO i=1,nr-1
          i0 = i0 + M_ATOMS_R(i)
        ENDDO
      ! j = i0 + randomInteger(1,M_ATOMS_R(nr)) if happy to switch like species

        k=0
        DO j=1+i0,M_ATOMS_R(nr)+i0
          IF (in_cluster%atoms(p1)%symbol.ne.in_cluster%atoms(p_1(j))%symbol) THEN
            k=k+1
            p_2(k)=p_1(j)
          ELSEIF(in_cluster%atoms(p1)%cstype.ne.in_cluster%atoms(p_1(j))%cstype)THEN
            k=k+1
            p_2(k)=p_1(j)
          ENDIF
        ENDDO
        IF (k.eq.0) THEN
          WRITE(stdsee,*)'Error - impossible to mix solution'
          WRITE(stderr,*)'Data: ',i0,nr,M_ATOMS_R(nr),p1,in_cluster%atoms(p1)%symbol
          STOP
        ENDIF

        j = randomInteger(1,k)
        p2 = p_2(j) ! second atom defined

        IF (DEBUG_LEVEL /= 0) THEN
          WRITE(stdsee,*)'Found '//TRIM(intToChar(k))//' possible replacements sites'
          WRITE(stdsee,*)'Exchange combo:'//TRIM(intToChar(p1))//' - '// &
                         TRIM(intToChar(p2))//' in region '//TRIM(intToChar(nr))
        ENDIF

        k = atomOrder(p1) 
        atomOrder(p1) = atomOrder(p2)
        atomOrder(p2) = k

        IF (N_MIX /=0 ) THEN ! MIXING BETWEEN REGIONS REQUIRED
          WRITE(stderr,*)'MIXING BETWEEN REGIONS within mixSolution NOT supported!'
          WRITE(stdout,*)'MIXING BETWEEN REGIONS within mixSolution NOT supported!'
          STOP
        ENDIF
      ENDDO ! n_exchg

      DEALLOCATE(M_ATOMS_R)
      DEALLOCATE(r_1)
    ENDIF

    DO i=1, N_ATOMS
      j = atomOrder(i)
      out_cluster%atoms(i)%xc = in_cluster%atoms(j)%xc
      out_cluster%atoms(i)%yc = in_cluster%atoms(j)%yc
      out_cluster%atoms(i)%zc = in_cluster%atoms(j)%zc
      out_cluster%atoms(i)%xf = in_cluster%atoms(j)%xf
      out_cluster%atoms(i)%yf = in_cluster%atoms(j)%yf
      out_cluster%atoms(i)%zf = in_cluster%atoms(j)%zf
    END DO
    DEALLOCATE(atomOrder)

    out_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(N_INDIVIDUALS)))

    DEALLOCATE(p_1)
    DEALLOCATE(p_2)
END SUBROUTINE mixSolution

END MODULE Moveclass
