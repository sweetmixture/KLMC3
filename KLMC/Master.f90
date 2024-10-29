MODULE Master

    USE Config
    USE Library
    USE ClusterRoutines, ONLY : getEnergy
    USE File,            ONLY : copyClusterFiles, logEbest, moveBestdata, outputXYZ
    USE Population,      ONLY : MASTER_CLUSTER
    USE UnitCell,        ONLY : lattice_c2v_3D, xctoxf, xftoxc
    USE Utilities,       ONLY : addHashkeyToBlacklist, lookForTopologicalMatch

    IMPLICIT NONE

!==========================================================================================!
CONTAINS

! routines used on Master File and Top Clusters
!
! subroutines: correctCluster computeNcations computeNumbers 
!              define_N_ATOMS_R
!              removeKeyword insertKeyword
!              reNumberRegions
!              reorderNcations reorderRegions reorderVacancies resetCluster
!              shiftAtomOrder
!              updateBestSet updateTopClusters moveBestSet
!              writeMasterCluster
!
!  functions:  correctNatoms computeNregions computeNspecies computeNvac 
!              foundKeyword



!==========================================================================================!
!
!==========================================================================================!
FUNCTION foundKeyword(in_word)
    INTEGER :: foundKeyword
    CHARACTER(LEN=*), INTENT(IN) :: in_word
    INTEGER :: ikey

    foundKeyword=0
    DO ikey = 1, SIZE(MASTER_TOP)
      MASTER_TOP(ikey)=TRIM(MASTER_TOP(ikey))
      IF (LEN_TRIM((MASTER_TOP(ikey))) < 1) THEN
        RETURN
      ELSEIF (index(MASTER_TOP(ikey),'#').ne.0) THEN
        ! located early comment line in Master File
      ELSE
        foundKeyword=ikey
        EXIT 
      ENDIF
    ENDDO 

    IF (LEN_TRIM(in_word) > 2) THEN
      IF (index(MASTER_TOP(ikey),TRIM(in_word))==0) foundKeyword=0
    ENDIF
    RETURN
END FUNCTION foundKeyword

!==========================================================================================!

SUBROUTINE removeKeyword(in_word,in_key)
    CHARACTER(LEN=*), INTENT(IN) :: in_word
    INTEGER, INTENT(IN), OPTIONAL :: in_key
    INTEGER :: i,istart,ikey

    IF (PRESENT(in_key)) THEN
      ikey = in_key
    ELSE
      ikey = foundKeyword('')
    ENDIF

    istart=index(MASTER_TOP(ikey),in_word)
    IF (istart.ne.0) THEN
      DO i=istart,istart+20
        IF (i > MASTER_FILE_WIDTH) THEN
          RETURN
        ELSEIF (index(MASTER_TOP(ikey)(i:i),' ').ne.0) THEN
          RETURN
        ELSE
          MASTER_TOP(ikey)(i:i)=' '
        ENDIF
      ENDDO
    ENDIF
END SUBROUTINE removeKeyword

!==========================================================================================!

SUBROUTINE insertKeyword(in_word,in_key)
    CHARACTER(LEN=*), INTENT(IN) :: in_word
    INTEGER, INTENT(IN), OPTIONAL :: in_key
    INTEGER :: i,istart,igap,ikey,inlength,kwlength
    inlength = LEN_TRIM(in_word) + 1

    IF (PRESENT(in_key)) THEN
      ikey = in_key
    ELSE
      ikey = foundKeyword('')
    ENDIF

    kwlength = LEN_TRIM((MASTER_TOP(ikey)))

    IF (kwlength < 1) THEN
      MASTER_TOP(ikey)=in_word
      RETURN
    ENDIF

    IF (index(MASTER_TOP(ikey),in_word).ne.0) RETURN

    igap = 0
    DO i = 1,kwlength
       IF (index(MASTER_TOP(1)(i:i),' ').ne.0) THEN
         IF (igap == 0) istart = i
         igap = igap + 1
         IF (igap > inlength) THEN
           IF (istart == 1) THEN
             EXIT
           ELSEIF (igap > inlength+1) THEN
             istart=istart+1
             EXIT
           ENDIF
         ENDIF
       ELSE
         igap = 0
         istart = i + 1
       ENDIF
    ENDDO

    IF (istart+inlength-1 < MASTER_FILE_WIDTH) THEN
      MASTER_TOP(ikey)(istart:istart+inlength-1)=in_word(1:inlength)
    ELSE
      WRITE(stderr,*)'Failed to insert keyword',TRIM(in_word)
      WRITE(stderr,*)'Failed to insert ',TRIM(in_word),' into ',MASTER_TOP(ikey)
      STOP
    ENDIF

END SUBROUTINE insertKeyword

!==========================================================================================!

SUBROUTINE writeMasterCluster()
    INTEGER :: i,isite
    IF (N_ATOMS > 0) THEN ! make sure N_ATOMS initialised
        CALL setDefaults(MASTER_CLUSTER)
        DO i = 1, 6
            MASTER_CLUSTER%box(i)=MASTER_CELL(i)
        ENDDO
        isite  = 0
        DO i = 1, N_ATOMS
            READ(MASTER_ATOMS(i,1), '(A)') MASTER_CLUSTER%atoms(i)%symbol

            IF (index((MASTER_ATOMS(i,2)),'s').eq.1) THEN
              MASTER_CLUSTER%atoms(i)%cstype='s'
            ELSEIF (index((MASTER_ATOMS(i,2)),'S').eq.1) THEN
              MASTER_CLUSTER%atoms(i)%cstype='s'
            ELSEIF (index((MASTER_ATOMS(i,2)),'h').eq.1) THEN
              MASTER_CLUSTER%atoms(i)%cstype='h'
            ELSEIF (index((MASTER_ATOMS(i,2)),'H').eq.1) THEN
              MASTER_CLUSTER%atoms(i)%cstype='h'
            ELSEIF (index((MASTER_ATOMS(i,2)),'i').eq.1) THEN
              MASTER_CLUSTER%atoms(i)%cstype='x'
            ELSEIF (index((MASTER_ATOMS(i,2)),'I').eq.1) THEN
              MASTER_CLUSTER%atoms(i)%cstype='x'
            ELSEIF (index((MASTER_ATOMS(i,2)),'x').eq.1) THEN
              MASTER_CLUSTER%atoms(i)%cstype='x'
            ELSEIF (index((MASTER_ATOMS(i,2)),'X').eq.1) THEN
              MASTER_CLUSTER%atoms(i)%cstype='x'
            ELSEIF (index((MASTER_ATOMS(i,2)),'k').eq.1) THEN
              MASTER_CLUSTER%atoms(i)%cstype='k'
            ELSEIF (index((MASTER_ATOMS(i,2)),'K').eq.1) THEN
              MASTER_CLUSTER%atoms(i)%cstype='k'
            ELSE ! we have a rigid ion or a core
              MASTER_CLUSTER%atoms(i)%cstype='c'
            ENDIF

            IF ( JOB_TYPE == 4 ) THEN
              isite = isite + 1
            ELSE
              IF (index((MASTER_ATOMS(i,2)),'1').ne.0) THEN
                isite = 1
              ELSEIF (index((MASTER_ATOMS(i,2)),'2').ne.0) THEN
                isite = 2
              ELSEIF (index((MASTER_ATOMS(i,2)),'3').ne.0) THEN
                isite = 3
              ELSEIF (index((MASTER_ATOMS(i,2)),'4').ne.0) THEN
                isite = 4
              ELSEIF (index((MASTER_ATOMS(i,2)),'5').ne.0) THEN
                isite = 5
              ELSEIF (index((MASTER_ATOMS(i,2)),'6').ne.0) THEN
                isite = 6
              ELSEIF (index((MASTER_ATOMS(i,2)),'7').ne.0) THEN
                isite = 7
              ELSEIF (index((MASTER_ATOMS(i,2)),'8').ne.0) THEN
                isite = 8
              ELSEIF (index((MASTER_ATOMS(i,2)),'9').ne.0) THEN
                isite = 9
              ELSE
                isite = 0
              ENDIF
            ENDIF
            MASTER_CLUSTER%atoms(i)%site=isite

            IF (index(COORD_TYPE,'fract').ne.0) THEN ! use fractionals
              ! READ(MASTER_ATOMS(i,3),'(1F15.12)') MASTER_CLUSTER%atoms(i)%xf
              ! READ(MASTER_ATOMS(i,4),'(1F15.12)') MASTER_CLUSTER%atoms(i)%yf
              ! READ(MASTER_ATOMS(i,5),'(1F15.12)') MASTER_CLUSTER%atoms(i)%zf
              READ(MASTER_ATOMS(i,3),*) MASTER_CLUSTER%atoms(i)%xf
              READ(MASTER_ATOMS(i,4),*) MASTER_CLUSTER%atoms(i)%yf
              READ(MASTER_ATOMS(i,5),*) MASTER_CLUSTER%atoms(i)%zf
            ELSE
              ! READ(MASTER_ATOMS(i,3),'(1F15.12)') MASTER_CLUSTER%atoms(i)%xc
              ! READ(MASTER_ATOMS(i,4),'(1F15.12)') MASTER_CLUSTER%atoms(i)%yc
              ! READ(MASTER_ATOMS(i,5),'(1F15.12)') MASTER_CLUSTER%atoms(i)%zc
              READ(MASTER_ATOMS(i,3),*) MASTER_CLUSTER%atoms(i)%xc
              READ(MASTER_ATOMS(i,4),*) MASTER_CLUSTER%atoms(i)%yc
              READ(MASTER_ATOMS(i,5),*) MASTER_CLUSTER%atoms(i)%zc
            ENDIF

            !IF ( JOB_TYPE == 4 .AND. MASTER_CLUSTER%atoms(i)%cstype=='s' ) THEN
            !  IF( INDEX(MASTER_CLUSTER%atoms(i)%symbol,MASTER_spring_type) /= 0 ) THEN
            !    MASTER_CLUSTER%atoms(i)%k=MASTER_k
            !  ELSE
            !    WRITE(*,*)'Spring type is not initially defined'
            !    STOP
            !  ENDIF
            !ENDIF
        END DO

! WARNING: IDEALLY SHOULD ALSO CATER FOR 1D and 2D PBC
        IF (index(COORD_TYPE,'fract').ne.0) THEN ! need Cartesians
          CALL xftoxc(MASTER_CLUSTER)
        ELSEIF(N_PBC==3) THEN ! may need fractional coordinates
          CALL xctoxf(MASTER_CLUSTER)
        ENDIF
    END IF
END SUBROUTINE writeMasterCluster

!==========================================================================================!

SUBROUTINE resetCluster(inout_cluster)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    INTEGER :: i

    DO i=1,6
      inout_cluster%box(i)=MASTER_CLUSTER%box(i)
    ENDDO
    DO i=1,N_ATOMS
      inout_cluster%atoms(i)%xc=MASTER_CLUSTER%atoms(i)%xc
      inout_cluster%atoms(i)%yc=MASTER_CLUSTER%atoms(i)%yc
      inout_cluster%atoms(i)%zc=MASTER_CLUSTER%atoms(i)%zc
    ENDDO
    CALL lattice_c2v_3D(MASTER_CELL,VECTOR)
    CALL xctoxf(inout_cluster)
END SUBROUTINE resetCluster

!==========================================================================================!

SUBROUTINE correctCluster(in_cluster,inout_cluster)
    TYPE(cluster), INTENT(IN)    :: in_cluster
    TYPE(cluster), INTENT(INOUT) :: inout_cluster

    !convert cartesians into fractionals for sites with atoms

END SUBROUTINE correctCluster

!==========================================================================================!

SUBROUTINE computeNumbers()
    N_VAC = 0
    N_ARC_ATOMS = N_ATOMS
    N_EXTRA_ATOMS = correctNatoms() ! required IF(L_IMAGE_ATOMS) for GULP constrain option
    IF (JOB_TYPE==1) THEN
      CALL computeNcations
      CALL reorderNcations
    ELSEIF (JOB_TYPE == 3 .OR. JOB_TYPE == 8) THEN
      N_VAC = computeNvac() ! number of vacancies
      N_ARC_ATOMS = N_ATOMS - N_VAC + N_EXTRA_ATOMS
      N_REGIONS = computeNregions() ! number of regions
      IF (N_REGIONS > 0) THEN
        CALL reorderRegions
        CALL define_N_ATOMS_R
      ELSE
        CALL reorderVacancies
      ENDIF
      IF (L_COMPUTE_RDF) N_SPECIES = computeNspecies()
    ENDIF
END SUBROUTINE computeNumbers

!==========================================================================================!

SUBROUTINE computeNcations()
    INTEGER :: i
    IF (N_ATOMS > 0) THEN 
      DO i = 1, N_ATOMS
        IF (isCation(MASTER_CLUSTER%atoms(i))) CALL increment(N_CATIONS)
      ENDDO
    ENDIF
END SUBROUTINE computeNcations

!==========================================================================================!

SUBROUTINE reorderNcations() 
    INTEGER :: i,j,k
    IF (N_CATIONS > 0) THEN 
      DO i = 1, N_ATOMS
        IF (isAnion(MASTER_CLUSTER%atoms(i))) THEN ! anion found
          IF ( i < N_CATIONS + 1 ) THEN ! a cation exists below index i
            k = 0
            DO j = N_CATIONS + 1, N_ATOMS
              IF (isCation(MASTER_CLUSTER%atoms(j))) THEN ! cation found
                k = j
              ENDIF
            ENDDO
            IF (k == 0) THEN
             WRITE(stderr,*)'Bug - k=0 found within reorderNcations'
            ELSE
             CALL switchAtomIndex(MASTER_CLUSTER%atoms(i),MASTER_CLUSTER%atoms(k))
            ENDIF
          ENDIF
        ENDIF
      ENDDO
    ENDIF
END SUBROUTINE reorderNcations

!==========================================================================================!

SUBROUTINE reorderVacancies()
    INTEGER        :: i,idum,j,k,M_ATOMS
    M_ATOMS=N_ATOMS-N_VAC ! there are N_ATOMS sites, including vacancies
    IF (N_VAC > 0) THEN 
      DO i = 1, M_ATOMS !loop over actual number of atoms in search
        IF (isVacancy(MASTER_CLUSTER%atoms(i))) THEN ! need to find misplaced atom
          k = 0
          DO j = N_ATOMS, M_ATOMS+1, -1
            IF (.NOT.isVacancy(MASTER_CLUSTER%atoms(j))) THEN
              k = j
              EXIT
            ENDIF
          ENDDO
          ! now shift rather than switch to avoid mixing anions and cations
          IF (k == 0) THEN
            WRITE(stderr,*)'Bug - k=0 found within reorderVacancies'
          ELSE
            idum = i
            CALL shiftAtomOrder(MASTER_CLUSTER,idum,k)
          ENDIF
        ENDIF
      ENDDO
    ENDIF
END SUBROUTINE reorderVacancies

!==========================================================================================!

SUBROUTINE reNumberRegions(in_nr,in_n)
    INTEGER, INTENT(IN)    :: in_nr, in_n
    INTEGER :: n,m,i
    CHARACTER(LEN=1) :: sr
    DO n=in_nr,in_n-1
      DO i = 1, N_ATOMS
        IF (MASTER_CLUSTER%atoms(i)%site == n+1) MASTER_CLUSTER%atoms(i)%site = n
      ENDDO
      L_FIX_R(n)=L_FIX_R(n+1)
      DO i=1,N_MIX
        IF (T_MIX(i)%from == n+1) T_MIX(i)%from = n
        CALL num2str(n+1,1,sr)
        m = index(T_MIX(i)%to,sr)
        CALL num2str(n,1,sr)
        IF (m /= 0) T_MIX(i)%to(m:m) = sr
      ENDDO
    ENDDO
END SUBROUTINE reNumberRegions

!==========================================================================================!

SUBROUTINE reorderRegions()
    INTEGER :: n,nr,i,idum
    n=1
    DO nr=0, N_REGIONS
      DO i = n, N_ATOMS
        IF (MASTER_CLUSTER%atoms(i)%site == nr) THEN
          IF (.NOT.isVacancy(MASTER_CLUSTER%atoms(i))) THEN
            IF (i == n) THEN
              n=n+1
            ELSE
              idum = i
              CALL shiftAtomOrder(MASTER_CLUSTER,idum,n)
              n=n+1
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      DO i = n, N_ATOMS
        IF (MASTER_CLUSTER%atoms(i)%site == nr) THEN
          IF (isVacancy(MASTER_CLUSTER%atoms(i))) THEN
            IF (i == n) THEN
              n=n+1
            ELSE
              idum = i
              CALL shiftAtomOrder(MASTER_CLUSTER,idum,n)
              n=n+1
            ENDIF
          ENDIF
        ENDIF
      ENDDO
    ENDDO
END SUBROUTINE reorderRegions

!==========================================================================================!

SUBROUTINE shiftAtomOrder(inout_cluster,in_from,in_to)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    INTEGER, INTENT(INOUT) :: in_from, in_to
    TYPE(atom) :: tmp_atom
    INTEGER :: i, ishift

    IF (in_from > in_to) THEN
      ishift =  1
    ELSEIF (in_from < in_to) THEN
      ishift = -1
    ELSE
      RETURN ! ishift = 0
    ENDIF

    CALL copyAtom(tmp_atom,MASTER_CLUSTER%atoms(in_from))

    DO i=in_from-ishift,in_to,-ishift
      CALL copyAtom(MASTER_CLUSTER%atoms(i+ishift),MASTER_CLUSTER%atoms(i))
    ENDDO

    CALL copyAtom(MASTER_CLUSTER%atoms(in_to),tmp_atom)

END SUBROUTINE shiftAtomOrder

!==========================================================================================!

SUBROUTINE define_N_ATOMS_R()
    INTEGER :: n,nr,i
    DO nr=0,N_REGIONS
      n=0
      DO i=1,N_ATOMS
        IF (MASTER_CLUSTER%atoms(i)%site == nr) n=n+1
      ENDDO
      N_ATOMS_R(nr) = n
      IF (DEBUG_LEVEL > 0) WRITE(6,*)'N_ATOMS_R(',nr,') =',N_ATOMS_R(nr)
    ENDDO
END SUBROUTINE define_N_ATOMS_R

!==========================================================================================!

INTEGER FUNCTION computeNvac()
    INTEGER :: i,n
    n=0
    DO i = 1, N_ATOMS
      IF (isVacancy(MASTER_CLUSTER%atoms(i))) n=n+1
    ENDDO
    computeNvac=n
END FUNCTION computeNvac

!==========================================================================================!

INTEGER FUNCTION computeNregions()
    INTEGER :: i,m,n,nr
    LOGICAL :: lfound
    n=0
    DO i = 1, N_ATOMS
      IF (MASTER_CLUSTER%atoms(i)%site > n) n=MASTER_CLUSTER%atoms(i)%site
    ENDDO
    IF (n > 0) THEN
      m = n - 1
      DO nr = m, 1, -1
        lfound=.FALSE.
        DO i=1, N_ATOMS
          IF (MASTER_CLUSTER%atoms(i)%site == nr) lfound = .TRUE.
          IF (lfound) EXIT
        ENDDO
        IF (.not.lfound) THEN
          CALL reNumberRegions(nr,n)
          n = n -1
        ENDIF
      ENDDO
    ENDIF
    computeNregions=n
END FUNCTION computeNregions

!==========================================================================================!

INTEGER FUNCTION computeNspecies()
    INTEGER :: i,j,n
    CHARACTER(LEN=2) :: XY
    LOGICAL :: lnew
    n=1
    MASTER_SPECIES(1)=MASTER_CLUSTER%atoms(1)%symbol
    DO i=2,N_ATOMS
      XY=MASTER_CLUSTER%atoms(i)%symbol
      lnew=.TRUE.
      DO j=1,n
        IF (XY == MASTER_SPECIES(j)) lnew=.FALSE.
      ENDDO
      IF (lnew) THEN
        n=n+1
        IF (n > MAX_SPECIES) THEN
          WRITE(stderr,*)'Happy with ',(MASTER_SPECIES(j),j=1,MAX_SPECIES)
          WRITE(stderr,*)'Now found another new species: ',XY
          WRITE(stderr,*)'Recompile with larger value for MAX_SPECIES'
          STOP
        ENDIF
        MASTER_SPECIES(n)=MASTER_CLUSTER%atoms(i)%symbol
      ENDIF
    ENDDO
    DO i = 1, SIZE(MASTER_LOCAL)
      IF (LEN_TRIM(MASTER_LOCAL(i)) < 1) EXIT
      ! MASTER_LOCAL(i)=TRIM(ADJUSTL(MASTER_LOCAL(i))) upon reading in master file
      IF (MASTER_LOCAL(i)(2:2)==' '.OR.MASTER_LOCAL(i)(3:3)==' ') THEN
        ! name of element found on the next new line
        XY=MASTER_LOCAL(i)(1:2)
        lnew=.TRUE.
        DO j=1,n
          IF (XY == MASTER_SPECIES(j)) lnew=.FALSE.
        ENDDO
        IF (lnew) THEN
          n=n+1
          IF (n > MAX_SPECIES) THEN
            WRITE(stderr,*)'Happy with ',(MASTER_SPECIES(j),j=1,MAX_SPECIES)
            WRITE(stderr,*)'Now found another new species: ',XY
            WRITE(stderr,*)'Recompile with larger value for MAX_SPECIES',MAX_SPECIES
            STOP
          ENDIF
          MASTER_SPECIES(n)=XY
        ENDIF
      ENDIF
    ENDDO
    computeNspecies=n
END FUNCTION computeNspecies

!==========================================================================================!

INTEGER FUNCTION correctNatoms()
    INTEGER :: i,n
    n=0
    localloop: DO i = 1, SIZE(MASTER_LOCAL)
      IF (LEN_TRIM(MASTER_LOCAL(i)) < 1) EXIT localloop
      IF (index(TRIM(MASTER_LOCAL(i)),' c').ne.0) n=n+1
      IF (index(TRIM(MASTER_LOCAL(i)),' C').ne.0) n=n+1
    END DO localloop
    correctNatoms=n
END FUNCTION correctNatoms

!==========================================================================================!

SUBROUTINE updateTopClusters(in_clusters)
    TYPE(cluster), INTENT(INOUT) :: in_clusters(:)
    INTEGER :: i
    DO i = 1, SIZE(in_clusters)
       IF (in_clusters(i)%edefn /= 0) THEN
         CALL updateBestSet(in_clusters(i))
       ENDIF
    END DO
END SUBROUTINE updateTopClusters

!==========================================================================================!
LOGICAL FUNCTION checkHashkey(in_cluster)

  TYPE(cluster), INTENT(IN) :: in_cluster
  INTEGER :: ii
  CHARACTER(LEN=N_HASH_LEN) :: hashkey

  checkHashkey = .FALSE.
  hashkey = in_cluster%hashkey

  DO ii = INT_MAX_BEST, 1, -1
    IF (hashkey == BEST_HASHKEYS(ii)) THEN

      !PRINT *, "HASHKEY DEBUG > FOUND A MATCH: ", ID_BEST(ii)

      IF (JOB_TYPE == 3 .AND. N_INDIVIDUALS == 0) THEN
        ! do not update freq of finding LM
      ELSE
          N_BEST_ENERGIES(ii) = N_BEST_ENERGIES(ii)+1
          checkHashkey = .TRUE.
      ENDIF

      CALL copyClusterFiles(0, in_cluster) ! print out new freq

      EXIT
    ENDIF

  ENDDO

END FUNCTION checkHashkey

!==========================================================================================!

SUBROUTINE updateBestSet(inout_cluster)

    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    REAL(KIND=DBL) :: e_new
    INTEGER :: ii,imax,ide, iHashkey
    LOGICAL :: hashkeyMatch, hashkeyInTopList

    imax=0
    ide = inout_cluster%edefn
    e_new = getEnergy(inout_cluster) + R_BEST_EDIF
    hashkeyMatch = .FALSE.
    hashkeyInTopList = .FALSE.

    IF (L_USE_TOP_ANALYSIS) THEN
      ! Look for a topological match

      CALL lookForTopologicalMatch(inout_cluster)

      iHashkey = inout_cluster%hashkeyMatchNo

      ! Check for a topological match with a current step
      IF (iHashkey > 0) THEN
        N_BEST_ENERGIES(iHashkey) = N_BEST_ENERGIES(iHashkey) + 1
        CALL copyClusterFiles(0, inout_cluster)

        hashkeyMatch = .TRUE.

      ELSE IF (iHashkey < 0) THEN
        hashkeyMatch = .TRUE.
      ENDIF
    ENDIF

    ! If a topological match was not found continue as usual.
    IF (.NOT. hashkeyMatch) THEN

      DO ii=INT_MAX_BEST,1,-1

        IF (IDE_BEST(ii) < ide) THEN
          imax=ii

        ELSEIF (IDE_BEST(ii) == ide) THEN
          IF (R_BEST_ENERGIES(ii) > e_new) imax=ii

        ELSEIF ((DM_FLAG) .AND. (JOB_TYPE .EQ. 0) .AND. (ide .eq. -1)) THEN
          IF (R_BEST_ENERGIES(ii) > e_new) imax=ii
        ENDIF
      ENDDO

      !imax>0 => inout_cluster has a lower energy than the current (imax)th top LM
      IF (imax > 1) THEN ! check whether we already have this cluster
        IF (IDE_BEST(imax-1) == ide .AND. ABS(R_BEST_ENERGIES(imax-1)-getEnergy(inout_cluster))<R_BEST_EDIF) THEN
          IF (JOB_TYPE == 3 .AND. N_INDIVIDUALS == 0) THEN
            ! do not update freq of finding LM
          ELSE
            N_BEST_ENERGIES(imax-1)=N_BEST_ENERGIES(imax-1)+1
          ENDIF
          imax=0

          IF (L_USE_TOP_ANALYSIS) THEN
            hashkeyInTopList = .TRUE.
          ENDIF

          CALL copyClusterFiles(imax,inout_cluster) ! print out new freq
        ENDIF
      ENDIF

      IF (imax > 0) THEN ! found new low energy cluster so update data
        DO ii=INT_MAX_BEST,imax+1,-1
          ID_BEST(ii) = ID_BEST(ii-1)
          IDE_BEST(ii) = IDE_BEST(ii-1)
          R_BEST_ENERGIES(ii) = R_BEST_ENERGIES(ii-1)
          N_BEST_ENERGIES(ii) = N_BEST_ENERGIES(ii-1)
          IF (IDE_BEST(ii) > 0) A_BEST_ENERGIES(ii) = A_BEST_ENERGIES(ii-1)
          IF (IDE_BEST(ii) > 1) B_BEST_ENERGIES(ii) = B_BEST_ENERGIES(ii-1)
          IF (IDE_BEST(ii) > 2) C_BEST_ENERGIES(ii) = C_BEST_ENERGIES(ii-1)
          IF (IDE_BEST(ii) > 3) D_BEST_ENERGIES(ii) = D_BEST_ENERGIES(ii-1)
          IF (IDE_BEST(ii) > 4) E_BEST_ENERGIES(ii) = E_BEST_ENERGIES(ii-1)
          IF (IDE_BEST(ii) > 5) F_BEST_ENERGIES(ii) = F_BEST_ENERGIES(ii-1)

          ! Saving hashkeys
          IF (L_USE_TOP_ANALYSIS) THEN
            BEST_HASHKEYS(ii) = BEST_HASHKEYS(ii-1)
          ENDIF
        ENDDO

        ID_BEST(imax) = inout_cluster%id
        IDE_BEST(imax) = inout_cluster%edefn
        R_BEST_ENERGIES(imax) = getEnergy(inout_cluster)

        ! Saving hashkeys
        IF (L_USE_TOP_ANALYSIS) THEN
          BEST_HASHKEYS(imax) = inout_cluster%hashkey
        ENDIF

        IF (ide == 1) THEN
          A_BEST_ENERGIES(imax) = getEnergy(inout_cluster)
         !A_BEST_ENERGIES(imax) = R_BEST_ENERGIES(imax)

        ELSEIF (ide == 2) THEN
          A_BEST_ENERGIES(imax) = getEnergy(inout_cluster,1)
          B_BEST_ENERGIES(imax) = getEnergy(inout_cluster)
         !B_BEST_ENERGIES(imax) = R_BEST_ENERGIES(imax)

        ELSEIF (ide == 3) THEN
          A_BEST_ENERGIES(imax) = getEnergy(inout_cluster,1)
          B_BEST_ENERGIES(imax) = getEnergy(inout_cluster,2)
          C_BEST_ENERGIES(imax) = getEnergy(inout_cluster)
         !C_BEST_ENERGIES(imax) = R_BEST_ENERGIES(imax)

        ELSEIF (ide == 4) THEN
          A_BEST_ENERGIES(imax) = getEnergy(inout_cluster,1)
          B_BEST_ENERGIES(imax) = getEnergy(inout_cluster,2)
          C_BEST_ENERGIES(imax) = getEnergy(inout_cluster,3)
          D_BEST_ENERGIES(imax) = getEnergy(inout_cluster)
         !D_BEST_ENERGIES(imax) = R_BEST_ENERGIES(imax)

        ELSEIF (ide == 5) THEN
          A_BEST_ENERGIES(imax) = getEnergy(inout_cluster,1)
          B_BEST_ENERGIES(imax) = getEnergy(inout_cluster,2)
          C_BEST_ENERGIES(imax) = getEnergy(inout_cluster,3)
          D_BEST_ENERGIES(imax) = getEnergy(inout_cluster,4)
          E_BEST_ENERGIES(imax) = getEnergy(inout_cluster)
         !E_BEST_ENERGIES(imax) = R_BEST_ENERGIES(imax)

        ELSEIF (ide == 6) THEN
          A_BEST_ENERGIES(imax) = getEnergy(inout_cluster,1)
          B_BEST_ENERGIES(imax) = getEnergy(inout_cluster,2)
          C_BEST_ENERGIES(imax) = getEnergy(inout_cluster,3)
          D_BEST_ENERGIES(imax) = getEnergy(inout_cluster,4)
          E_BEST_ENERGIES(imax) = getEnergy(inout_cluster,5)
          F_BEST_ENERGIES(imax) = getEnergy(inout_cluster)
         !F_BEST_ENERGIES(imax) = R_BEST_ENERGIES(imax)

        ENDIF

        IF (JOB_TYPE == 3 .AND. N_INDIVIDUALS == 0) THEN
            N_BEST_ENERGIES(imax) = 0
        ELSE
            N_BEST_ENERGIES(imax) = 1
        ENDIF

        CALL logEbest(inout_cluster%id,imax,getEnergy(inout_cluster))
        CALL copyClusterFiles(imax,inout_cluster)

        R_BEST_EMAX = R_BEST_ENERGIES(INT_MAX_BEST) - R_BEST_EDIF

!      ELSE
!        ! If a hashkey was not located in the toplist, it will be added to the blacklist
!        IF ((L_USE_TOP_ANALYSIS) .AND. (.NOT. hashkeyInTopList)) THEN
!          CALL addHashkeyToBlacklist(inout_cluster%hashkey)
!        ENDIF
      ENDIF
    ENDIF

END SUBROUTINE updateBestSet

!==========================================================================================!

SUBROUTINE moveBestSet(in_set_number)
    INTEGER, INTENT(IN) :: in_set_number
    INTEGER :: i

    CALL moveBestdata(intToChar(in_set_number))

    NEW_THIS_RUN = 0
    DO i=1,INT_MAX_BEST
       ID_BEST(i)='x'
       IDE_BEST(i)=0
       R_BEST_ENERGIES(i)=R_BEST_EMAX
       N_BEST_ENERGIES(i)=0

       BEST_HASHKEYS(i) = 'x'
    ENDDO

END SUBROUTINE moveBestSet

!==========================================================================================!



END MODULE Master

