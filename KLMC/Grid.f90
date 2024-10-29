MODULE Grid

  USE Config
  USE Format
  USE Atoms


  IMPLICIT NONE

CONTAINS

  !==========================================================================================!
  ! Allocates specie pair related arrays and assigns specie numbers
  !==========================================================================================!
  SUBROUTINE initiliaseSpecieArrays()
    INTEGER :: I, J, nSpecies
    CHARACTER(LEN=2) :: specieI, specieJ
    REAL(KIND=DBL) :: value1, value2, coefCollapse, coefFragment

    coefFragment = FRAG_COEF

    ! Finding the maximum number of species
    nSpecies = LEN(MASTER_SPECIES)
    CALL deallocatePairSpecieArrays()

    ALLOCATE(PAIR_COLLAPSE_RADIUS(nSpecies, nSpecies))
    ALLOCATE(PAIR_FRAGMENT_RADIUS(nSpecies, nSpecies))

    DO I = 1, nSpecies
      specieI = TRIM(MASTER_SPECIES(I))

      DO J = 1, nSpecies
        specieJ = TRIM(MASTER_SPECIES(J))

        IF (specieI .EQ. specieJ) THEN
          value1 = getAtomProperty(specieI, 'CR')
          value2 = getAtomProperty(specieJ, 'CR')

          coefCollapse = COLL_COEF_SAME

        ELSE
          value1 = getAtomProperty(specieI, 'IR')
          value2 = getAtomProperty(specieJ, 'IR')

          coefCollapse = COLL_COEF_DIFF
        ENDIF

        ! collapse radius
        PAIR_COLLAPSE_RADIUS(I, J) = (value1 + value2) * coefCollapse

        ! fragmentation radius
        PAIR_FRAGMENT_RADIUS(I, J) = (value1 + value2) * coefFragment

      ENDDO
    ENDDO

  END SUBROUTINE initiliaseSpecieArrays

  !==========================================================================================!
  ! Deallocates specie pair related arrays
  !==========================================================================================!
  SUBROUTINE deallocatePairSpecieArrays()

    IF (ALLOCATED(PAIR_FRAGMENT_RADIUS)) THEN
      DEALLOCATE(PAIR_FRAGMENT_RADIUS)
    ENDIF

    IF (ALLOCATED(PAIR_COLLAPSE_RADIUS)) THEN
      DEALLOCATE(PAIR_COLLAPSE_RADIUS)
    ENDIF

  END SUBROUTINE deallocatePairSpecieArrays

  !==========================================================================================!
  ! Returns a collapsed radius
  !==========================================================================================!
  SUBROUTINE getCollapsedRadius(specieA, specieB, radius)

    CHARACTER(LEN=2), INTENT(IN) :: specieA, specieB
    REAL(KIND=DBL), INTENT(OUT) :: radius
    INTEGER :: specieAIdx, specieBIdx

    specieAIdx = getSpecieIndex(specieA)
    specieBIdx = getSpecieIndex(specieB)

    IF ((specieAIdx > 0) .AND. (specieBIdx > 0)) THEN
      radius = PAIR_COLLAPSE_RADIUS(specieAIdx, specieBIdx)
    ELSE
      radius = 0.0
    ENDIF

  END SUBROUTINE getCollapsedRadius

  !==========================================================================================!
  ! Returns a fragmentation radius
  !==========================================================================================!
  SUBROUTINE getFragmentRadius(specieA, specieB, radius)

    CHARACTER(LEN=2), INTENT(IN) :: specieA, specieB
    REAL(KIND=DBL), INTENT(OUT) :: radius
    INTEGER :: specieAIdx, specieBIdx

    specieAIdx = getSpecieIndex(specieA)
    specieBIdx = getSpecieIndex(specieB)

    IF ((specieAIdx > 0) .AND. (specieBIdx > 0)) THEN
      radius = PAIR_FRAGMENT_RADIUS(specieAIdx, specieBIdx)
    ELSE
      radius = 0.0
    ENDIF

  END SUBROUTINE getFragmentRadius

  !==========================================================================================!
  ! Returns a specie index from the master species array corresponding with the specie
  !==========================================================================================!
  INTEGER FUNCTION getSpecieIndex(specie)

    CHARACTER(LEN=2), INTENT(IN) :: specie
    INTEGER :: I

    getSpecieIndex = -1

    DO I = 1, LEN(MASTER_SPECIES)

      IF (TRIM(MASTER_SPECIES(I)) .EQ. TRIM(specie)) THEN
        getSpecieIndex = I
        EXIT
      ENDIF
    ENDDO

  END FUNCTION  getSpecieIndex

  !==========================================================================================!
  !
  !==========================================================================================!
  SUBROUTINE initialiseGrid()

!    INTEGER :: I, J, K

!    IF (GRID_ON) THEN
!      PRINT *, "IS GRID ON? ",GRID_ON
!    ENDIF

    IF ((GRID_NPOINTS_X < 1) .OR. (GRID_NPOINTS_Y < 1) .OR. (GRID_NPOINTS_Z < 1)) THEN

    ENDIF

    IF (GRID_RADIUS < 0.0) THEN
    ENDIF



  END SUBROUTINE initialiseGrid

END MODULE Grid
