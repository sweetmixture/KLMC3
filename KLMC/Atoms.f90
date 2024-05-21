MODULE Atoms

    USE Config
    USE Format

    IMPLICIT NONE

  !==========================================================================================!
CONTAINS

  !==========================================================================================!

  SUBROUTINE deallocateAtomsArrays()

    IF (ALLOCATED(ATOMS_ANUM)) THEN
      DEALLOCATE(ATOMS_ANUM)
    ENDIF

    IF (ALLOCATED(ATOMS_SYM)) THEN
      DEALLOCATE(ATOMS_SYM)
    ENDIF

    IF (ALLOCATED(ATOMS_AMASS)) THEN
      DEALLOCATE(ATOMS_AMASS)
    ENDIF

    IF (ALLOCATED(ATOMS_COVR)) THEN
      DEALLOCATE(ATOMS_COVR)
    ENDIF

    IF (ALLOCATED(ATOMS_IONR)) THEN
      DEALLOCATE(ATOMS_IONR)
    ENDIF

  END SUBROUTINE deallocateAtomsArrays

  !==========================================================================================!

  SUBROUTINE readAtoms()

    INTEGER :: inError=1, inUnit=26, nAtoms, I, aNum
    CHARACTER(LEN=2) :: aSym
    REAL(KIND=DBL) :: aMass, aCovr, aIonr
    LOGICAL :: lfound

    INQUIRE(FILE=TRIM(WORKING_DIR)//TRIM(DATA_FOLDER)//TRIM(ATOMS_FILE_NAME), EXIST=lfound)

    IF (lfound) THEN

      OPEN(UNIT=inUnit, FILE=TRIM(WORKING_DIR)//TRIM(DATA_FOLDER)//TRIM(ATOMS_FILE_NAME), &
        STATUS='OLD', IOSTAT=inError, IOMSG=ERROR_MSG)

      IF(inError == 0) THEN

        READ(UNIT=inUnit,FMT=*) nAtoms

        ATOMS_CNT = nAtoms

        IF (.NOT. ALLOCATED(ATOMS_ANUM)) ALLOCATE(ATOMS_ANUM(nAtoms))
        IF (.NOT. ALLOCATED(ATOMS_SYM)) ALLOCATE(ATOMS_SYM(nAtoms))
        IF (.NOT. ALLOCATED(ATOMS_AMASS)) ALLOCATE(ATOMS_AMASS(nAtoms))
        IF (.NOT. ALLOCATED(ATOMS_COVR)) ALLOCATE(ATOMS_COVR(nAtoms))
        IF (.NOT. ALLOCATED(ATOMS_IONR)) ALLOCATE(ATOMS_IONR(nAtoms))

        DO I = 1, nAtoms
          READ(UNIT=inUnit,FMT=*) aNum, aSym, aMass, aCovr, aIonr

          ATOMS_ANUM(I)  = aNum
          ATOMS_SYM(I)   = aSym
          ATOMS_AMASS(I) = aMass
          ATOMS_COVR(I)  = aCovr
          ATOMS_IONR(I)  = aIonr
        END DO

        CLOSE(UNIT=inUnit)

      ELSE
        WRITE(*,*) ERROR_MSG
      END IF

    ENDIF

  END SUBROUTINE readAtoms

  !==========================================================================================!

  REAL(KIND=DBL) FUNCTION getAtomProperty(atomSym, atomPropCode)

    CHARACTER(LEN=2), INTENT(IN) ::atomSym
    CHARACTER(LEN=2), INTENT(IN) ::atomPropCode

    INTEGER :: I

    DO I = 1, ATOMS_CNT

      IF (ATOMS_SYM(I) == atomSym) THEN
        IF (atomPropCode == 'AM') THEN
          getAtomProperty = ATOMS_AMASS(I)
        ELSE IF (atomPropCode == 'CR') THEN
          getAtomProperty = ATOMS_COVR(I)
        ELSE IF (atomPropCode == 'IR') THEN
          getAtomProperty = ATOMS_IONR(I)
        ENDIF
        EXIT
      ENDIF
    ENDDO
  END FUNCTION

  !==========================================================================================!

  LOGICAL FUNCTION lookForAtom(atomSym)
    CHARACTER(LEN=2), INTENT(IN) ::atomSym

    INTEGER :: I

    lookForAtom = .FALSE.

    DO I = 1, ATOMS_CNT
      IF (ATOMS_SYM(I) == TRIM(atomSym)) THEN
        lookForAtom = .TRUE.
        EXIT
      ENDIF
    ENDDO

  END FUNCTION lookForAtom

  !==========================================================================================!

END MODULE Atoms
