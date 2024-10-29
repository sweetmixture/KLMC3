MODULE Library
    USE Config
    IMPLICIT NONE
    INTEGER,PARAMETER  :: bit32 = SELECTED_INT_KIND(9)
    INTEGER, PARAMETER :: maxele = 107
    INTEGER(kind=bit32),SAVE :: ix,iy
    CHARACTER(LEN=2), SAVE :: atsym(maxele)
    REAL(KIND=DBL), SAVE :: atmass(maxele)

 ! from GULP
DATA atsym/'H ','He','Li','Be','B ','C ','N ','O ','F ', &
       'Ne','Na','Mg','Al','Si','P ','S ','Cl','Ar', &
       'K ','Ca','Sc','Ti','V ','Cr','Mn','Fe','Co', &
       'Ni','Cu','Zn','Ga','Ge','As','Se','Br','Kr', &
       'Rb','Sr','Y ','Zr','Nb','Mo','Tc','Ru','Rh', &
       'Pd','Ag','Cd','In','Sn','Sb','Te','I ','Xe', &
       'Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu', &
       'Gd','Tb','Dy','Ho','Er','Tm','Yb','Lu','Hf', &
       'Ta','W ','Re','Os','Ir','Pt','Au','Hg','Tl', &
       'Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th', &
       'Pa','U ','Np','Pu','Am','Cm','Bk','Cf','Es', &
       'Fm','Md','No','Lr','Rf','Ha','D ','X '/
  
DATA atmass/1.01,4.00,6.94,9.01,10.81,12.01,14.01,16.00, &
       19.00,20.18,22.99,24.31,26.98,28.09,30.97, &
       32.07,35.45,39.95,39.10,40.08,44.96,47.88, &
       50.94,52.00,54.94,55.85,58.93,58.69,63.55, &
       65.39,69.72,72.61,74.92,78.96,79.90,83.80, &
       85.47,87.62,88.91,91.22,92.91,95.94,98.00, &
       101.07,102.91,106.42,107.87,112.41,114.82, &
       118.71,121.75,127.60,126.91,131.29,132.91, &
       137.33,138.91,140.12,140.91,144.24,145.00, &
       150.36,151.97,157.25,158.93,162.50,164.93, &
       167.26,168.93,173.04,174.97,178.49,180.95, &
       183.85,186.21,190.20,192.22,195.08,196.97, &
       200.59,204.38,207.20,208.98,209.00,210.00, &
       222.00,223.00,226.03,227.03,232.04,231.04, &
       238.03,237.05,244.00,243.00,247.00,247.00, &
       251.00,252.00,257.00,258.00,259.00,260.00, &
       261.00,260.00,2.01,0.00/

CONTAINS

SUBROUTINE initialiseRandomSeed()
    INTEGER :: i, n, clock
    INTEGER, DIMENSION(:), ALLOCATABLE :: myseed
    
    CALL RANDOM_SEED(SIZE = n)
    ALLOCATE(myseed(n))
    CALL SYSTEM_CLOCK(COUNT=clock)
    IF (SEED == 0 ) THEN
       myseed = clock + 37 * (/ (i - 1, i = 1, n) /)
       IF (DEBUG_LEVEL > 0) WRITE(*,*) 'The Random Number seed is:',myseed
       CALL RANDOM_SEED(PUT = myseed)
       DEALLOCATE(myseed)
    ELSE
       myseed = SEED
       CALL RANDOM_SEED(PUT = myseed)
       CALL RANDOM_SEED(GET = myseed)
       WRITE(*,*) 'The Random Number seed is:',myseed
    END IF   
END SUBROUTINE

INTEGER FUNCTION randomInteger(in_min,in_max)
    INTEGER, INTENT(IN) :: in_min, in_max
    INTEGER :: i
    REAL(KIND=DBL) :: r_random=0.
    IF (in_min == in_max) THEN
      i = in_min
    ELSE
      CALL RANDOM_NUMBER(r_random)
      i = in_min + INT(r_random*(in_max-in_min+1))
    ! IF (i > in_max) i=in_max
    ENDIF
    randomInteger = i
END FUNCTION randomInteger

FUNCTION randomReal(in_min,in_max)
    REAL(KIND=DBL) :: randomReal, r_random=0.
    REAL, INTENT(IN) :: in_min, in_max
    CALL RANDOM_NUMBER(r_random)
    randomReal = 1.0_dp*in_min + r_random*(in_max - in_min)
END FUNCTION randomReal

CHARACTER(LEN=10) FUNCTION intToChar(i)
    INTEGER, INTENT(IN)  :: i
    CHARACTER(LEN=10) :: str1
    WRITE( str1, '(i10)' ) i
    intToChar = TRIM(ADJUSTL(str1))
END FUNCTION intToChar

CHARACTER(LEN=20) FUNCTION realToChar(r)
    REAL(KIND=DBL), INTENT(IN)  :: r
    CHARACTER(LEN=20) :: str1
    WRITE( str1, '(g20.10)' ) r
    realToChar = TRIM(ADJUSTL(str1))
END FUNCTION realToChar

CHARACTER(LEN=34) FUNCTION coordToChar(r)
    REAL(KIND=DBL), INTENT(IN)  :: r(3)
    CHARACTER(LEN=20) :: str1,str2,str3
    WRITE( str1, '(g10.5)' ) r(1)
    WRITE( str2, '(g10.5)' ) r(2)
    WRITE( str3, '(g10.5)' ) r(3)
    coordToChar = '('//TRIM(str1)//','//TRIM(str2)//','//TRIM(str3)//')'
END FUNCTION coordToChar

INTEGER FUNCTION charToInt(str1)
    CHARACTER(LEN=*), INTENT(IN)  :: str1
    INTEGER :: i
    WRITE(i, '(i10)' ) str1
    charToInt = i
END FUNCTION charToInt

SUBROUTINE num2str(i,length,str1)
    INTEGER, INTENT(IN) :: i,length
    CHARACTER(LEN=*), INTENT(OUT) :: str1
    INTEGER :: i100000000,i10000000,i1000000
    INTEGER :: i100000,i10000,i1000,i100,i10,i1,i0
    str1=''
    IF (i > 999999999 .OR. i < 0) THEN
      WRITE(6,*)'Update num2str - outside current range'
      STOP
    ELSEIF (length > 9 .OR. length < 1) THEN
      WRITE(6,*)'Update num2str - length exceeded'
      STOP
    ENDIF
    IF ((i > 99999999 .AND. length <  9) .OR.     &
        (i > 9999999  .AND. length <  8) .OR.     &
        (i > 999999   .AND. length <  7) .OR.     &
        (i > 99999    .AND. length <  6) .OR.     &
        (i > 9999     .AND. length <  5) .OR.     &
        (i > 999      .AND. length <  4) .OR.     &
        (i > 99       .AND. length <  3) .OR.     &
        (i > 9        .AND. length <  2)) THEN
      WRITE(6,*)'Update num2str - length of number'
      STOP
    ENDIF
    i0=i
    IF ( length == 9 ) THEN
      i100000000=int(i0/100000000)
      i0 = i0 - 100000000 * i100000000
      i10000000=int(i0/10000000)
      i0 = i0 - 10000000 * i10000000
      i1000000=int(i0/1000000)
      i0 = i0 - 1000000 * i1000000
      i100000=int(i0/100000)
      i0 = i0 - 100000 * i100000
      i10000=int(i0/10000)
      i0 = i0 - 10000 * i10000
      i1000=int(i0/1000)
      i0 = i0 - 1000 * i1000
      i100=int(i0/100)
      i0 = i0 - 100 * i100
      i10=int(i0/10)
      i0 = i0 - 10 * i10
      i1=i0
      str1(1:1)=intToChar(i100000000)
      str1(2:2)=intToChar(i10000000)
      str1(3:3)=intToChar(i1000000)
      str1(4:4)=intToChar(i100000)
      str1(5:5)=intToChar(i10000)
      str1(6:6)=intToChar(i1000)
      str1(7:7)=intToChar(i100)
      str1(8:8)=intToChar(i10)
      str1(9:9)=intToChar(i1)
    ENDIF
    IF ( length == 8 ) THEN
      i10000000=int(i0/10000000)
      i0 = i0 - 10000000 * i10000000
      i1000000=int(i0/1000000)
      i0 = i0 - 1000000 * i1000000
      i100000=int(i0/100000)
      i0 = i0 - 100000 * i100000
      i10000=int(i0/10000)
      i0 = i0 - 10000 * i10000
      i1000=int(i0/1000)
      i0 = i0 - 1000 * i1000
      i100=int(i0/100)
      i0 = i0 - 100 * i100
      i10=int(i0/10)
      i0 = i0 - 10 * i10
      i1=i0
      str1(1:1)=intToChar(i10000000)
      str1(2:2)=intToChar(i1000000)
      str1(3:3)=intToChar(i100000)
      str1(4:4)=intToChar(i10000)
      str1(5:5)=intToChar(i1000)
      str1(6:6)=intToChar(i100)
      str1(7:7)=intToChar(i10)
      str1(8:8)=intToChar(i1)
    ENDIF
    IF ( length == 7 ) THEN
      i1000000=int(i0/1000000)
      i0 = i0 - 1000000 * i1000000
      i100000=int(i0/100000)
      i0 = i0 - 100000 * i100000
      i10000=int(i0/10000)
      i0 = i0 - 10000 * i10000
      i1000=int(i0/1000)
      i0 = i0 - 1000 * i1000
      i100=int(i0/100)
      i0 = i0 - 100 * i100
      i10=int(i0/10)
      i0 = i0 - 10 * i10
      i1=i0
      str1(1:1)=intToChar(i1000000)
      str1(2:2)=intToChar(i100000)
      str1(3:3)=intToChar(i10000)
      str1(4:4)=intToChar(i1000)
      str1(5:5)=intToChar(i100)
      str1(6:6)=intToChar(i10)
      str1(7:7)=intToChar(i1)
    ENDIF
    IF ( length == 6 ) THEN
      i100000=int(i0/100000)
      i0 = i0 - 100000 * i100000
      i10000=int(i0/10000)
      i0 = i0 - 10000 * i10000
      i1000=int(i0/1000)
      i0 = i0 - 1000 * i1000
      i100=int(i0/100)
      i0 = i0 - 100 * i100
      i10=int(i0/10)
      i0 = i0 - 10 * i10
      i1=i0
      str1(1:1)=intToChar(i100000)
      str1(2:2)=intToChar(i10000)
      str1(3:3)=intToChar(i1000)
      str1(4:4)=intToChar(i100)
      str1(5:5)=intToChar(i10)
      str1(6:6)=intToChar(i1)
    ENDIF
    IF ( length == 5 ) THEN
      i10000=int(i0/10000)
      i0 = i0 - 10000 * i10000
      i1000=int(i0/1000)
      i0 = i0 - 1000 * i1000
      i100=int(i0/100)
      i0 = i0 - 100 * i100
      i10=int(i0/10)
      i0 = i0 - 10 * i10
      i1=i0
      str1(1:1)=intToChar(i10000)
      str1(2:2)=intToChar(i1000)
      str1(3:3)=intToChar(i100)
      str1(4:4)=intToChar(i10)
      str1(5:5)=intToChar(i1)
    ENDIF
    IF ( length == 4 ) THEN
      i1000=int(i0/1000)
      i0 = i0 - 1000 * i1000
      i100=int(i0/100)
      i0 = i0 - 100 * i100
      i10=int(i0/10)
      i0 = i0 - 10 * i10
      i1=i0
      str1(1:1)=intToChar(i1000)
      str1(2:2)=intToChar(i100)
      str1(3:3)=intToChar(i10)
      str1(4:4)=intToChar(i1)
    ENDIF
    IF ( length == 3 ) THEN
      i100=int(i0/100)
      i0 = i0 - 100 * i100
      i10=int(i0/10)
      i0 = i0 - 10 * i10
      i1=i0
      str1(1:1)=intToChar(i100)
      str1(2:2)=intToChar(i10)
      str1(3:3)=intToChar(i1)
    ENDIF
    IF ( length == 2 ) THEN
      i10=int(i0/10)
      i0 = i0 - 10 * i10
      i1=i0
      str1(1:1)=intToChar(i10)
      str1(2:2)=intToChar(i1)
    ENDIF
    IF ( length == 1 ) THEN
      i1=i0
      str1(1:1)=intToChar(i1)
    ENDIF
END SUBROUTINE num2str

FUNCTION getRootname (filename) RESULT (rootname)
    CHARACTER(LEN=*), INTENT(IN)             :: filename
    CHARACTER(LEN=len(filename)) :: rootname
    INTEGER                      :: kdot
    kdot   = scan( filename, '.', .TRUE. )
    rootname = filename
    IF ( kdot .NE. 0 ) THEN
       rootname = filename(1:kdot-1)
    ENDIF
END FUNCTION getRootname

SUBROUTINE debug(input)
    CHARACTER(LEN=*), INTENT(IN) :: input
    IF(LEN(input) < 1) THEN
        WRITE(*,*) '############################################################'
    ELSE
        WRITE(*,*) input
    END IF
END SUBROUTINE debug

LOGICAL FUNCTION isAnion(in_atom)
    TYPE(atom), INTENT(IN) :: in_atom
    CHARACTER(LEN=2) :: symbol
    symbol = in_atom%symbol
    IF     (symbol=='O' .OR. symbol=='o') THEN  !oxygen anion
        isAnion = .TRUE.
    ELSE IF(symbol=='S' .OR. symbol=='s') THEN  !sulphur anion
        isAnion = .TRUE.
    ELSE IF(symbol=='F' .OR. symbol=='f') THEN  !fluorine anion
        isAnion = .TRUE.
    ELSE IF(symbol=='I' .OR. symbol=='i') THEN  !iodine anion
        isAnion = .TRUE.
    ELSE IF(symbol=='N' .OR. symbol=='n') THEN  !nitrogen anion
        isAnion = .TRUE.
    ELSE IF(symbol=='P' .OR. symbol=='p') THEN  !phosporous anion
        isAnion = .TRUE.
    ELSE IF(symbol=='C' .OR. symbol=='c') THEN  !carbon anion
        isAnion = .TRUE.
    ELSE IF(symbol=='Cl' .OR. symbol=='cl') THEN  !chlorine anion
        isAnion = .TRUE.
    ELSE IF(symbol=='Br' .OR. symbol=='br') THEN  !bromine anion
        isAnion = .TRUE.
    ELSE IF(symbol=='Se' .OR. symbol=='se') THEN  !selenium anion
        isAnion = .TRUE.
    ELSE IF(symbol=='Te' .OR. symbol=='te') THEN  !telurium anion
        isAnion = .TRUE.
    ELSE !assume cation 
        isAnion = .FALSE.
    END IF
END FUNCTION isAnion

LOGICAL FUNCTION isCation(in_atom)
    TYPE(atom), INTENT(IN) :: in_atom
    isCation = .NOT.isAnion(in_atom)
END FUNCTION isCation

LOGICAL FUNCTION isVacancy(in_atom)
    TYPE(atom), INTENT(IN) :: in_atom
    IF (index(in_atom%cstype,'x').ne.0) THEN
      isVacancy=.TRUE.
    ELSE
      isVacancy=.FALSE.
    ENDIF
END FUNCTION isVacancy

SUBROUTINE increment(i)
    INTEGER, INTENT(INOUT) :: i
    i = i + 1
END SUBROUTINE increment

SUBROUTINE decrement(i)
    INTEGER, INTENT(INOUT) :: i
    i = i - 1
END SUBROUTINE decrement

SUBROUTINE copyAtom(inout_atom1,inout_atom2)
    TYPE(atom), INTENT(OUT) :: inout_atom1
    TYPE(atom), INTENT(IN)  :: inout_atom2

    inout_atom1%symbol = inout_atom2%symbol
    inout_atom1%cstype = inout_atom2%cstype
    inout_atom1%site = inout_atom2%site

    inout_atom1%xc = inout_atom2%xc
    inout_atom1%yc = inout_atom2%yc
    inout_atom1%zc = inout_atom2%zc

    inout_atom1%xf = inout_atom2%xf
    inout_atom1%yf = inout_atom2%yf
    inout_atom1%zf = inout_atom2%zf

END SUBROUTINE copyAtom

SUBROUTINE switchAtomIndex(inout_atom1,inout_atom2)
    TYPE(atom), INTENT(INOUT) :: inout_atom1, inout_atom2
    TYPE(atom) :: tmp_atom

    CALL copyAtom(tmp_atom,inout_atom1)
    CALL copyAtom(inout_atom1,inout_atom2)
    CALL copyAtom(inout_atom2,tmp_atom)

END SUBROUTINE switchAtomIndex

!=============================================================================!
! Creates a random number seed, if seed = 0 -> unique random number, if.
! seed <> 0 -> Deterministic seed is set
  !=============================================================================!
SUBROUTINE rand_num_set_seed(seed)
  USE Config,only : dp
  IMPLICIT none
  INTEGER, INTENT(INOUT)  :: seed
  INTEGER(kind=bit32)     :: iseed

  !F90 intrinsic time call
  CHARACTER(len=10) :: system_time
  !length is crucial ...
  REAL(kind=dp)     :: rtime
  IF (seed == 0 ) THEN
    call date_and_time(time=system_time)
    !character string
    !hhmmss.xxx
    read (system_time,*) rtime
    !convert to real
    rtime = rtime * 1000.0_dp
    !0<rtime<235959999.0
  ELSE
    rtime = real(abs(seed+100),kind=dp)
  !convert seed to real (and be different for each processor)
  END IF
  !and then convert to bit_32 size !integer
  iseed = int(rtime,kind=bit32)
  ix=IEOR(777755555_bit32,iseed)
  !Marsaglia generator
  iy=IOR(IEOR(888889999_bit32,iseed),1_bit32)
  !Parks-Miller generator
  seed = int(rtime)
  !return the seed that was used in default integer
  END SUBROUTINE rand_num_set_seed

  !=========================================================================!
  ! Return a single random deviate ~ uniform [0,1].                         !
  ! Based on Park-Miller "minimal standard" generator with Schrage's method
  ! !
  !  to do 32-bit multiplication without requiring higher precision, plus
  !  !
  !  additional Marsaglia shift to suppress any weaknesses &
  !  correlations.  !
  ! Using two independent methods greatly increases the period of the
  ! !
  !  generator, s.t. resulting period ~2*10^18
  !  !
  ! NB Routine is only set to work with 32 bit integers!.
  ! Based on the Box-Muller method, using the uniform_random
  ! generator.     !
  ! Actually generates a pair of uniform deviates, checks that
  ! they lie     !
  !      within the unit circle, s.t. can map onto cos/sin
  !      without being    !
  !      explicit, and then transforms into a Gaussian
  !      deviate.             !
  !-------------------------------------------------------------------------!
  ! References:
  ! !
  !   S.K. Park and K.W. Miller, Commun. ACM, 31,
  !   p1192-1201 (1988)         !
  !   L. Schrage, ACM Trans. Math. Soft., 5,
  !   p132-138 (1979)                !
  !   G. Marsaglia, Linear Algebra and its
  !   Applications, 67, p147-156 (1985)!
  !   CASTEP (algor.F90), M.I.J.Probert (2000)
  !   !............................................
  !=========================================================================!

FUNCTION rand_num_uniform()
  USE Config,only : dp
  IMPLICIT none

  REAL(kind=dp)                  :: rand_num_uniform
  INTEGER(kind=bit32)            :: iy_tmp !working value to force integer division
  INTEGER(kind=bit32),PARAMETER  :: iy_max=2147483647 !2^31-1
  REAL(kind=dp),PARAMETER        :: inv_iy_max=1.0_dp/2147483647.0_dp

  !do Marsaglia shift sequence, period 2^32-1, to get new ix
  ix=ieor(ix,ishft(ix,-17_bit32))
  ix=ieor(ix,ishft(ix,  5_bit32))

  !Park-Miller sequence, period iy_max-1, to get new iy
  iy_tmp=iy/127773_bit32 !NB integer division
  iy=16807_bit32*(iy-iy_tmp*127773_bit32)-iy_tmp*2836_bit32 !next value of iy
  if (iy < 0_bit32) iy=iy+iy_max !integer modulo iy_max

  !Combine ix and iy to get new random number, rescale onto range [0,1]
  rand_num_uniform=inv_iy_max*ior(iand(iy_max,ieor(ix,iy)),1_bit32) !with masking to ensure non-zero
  RETURN
  END FUNCTION rand_num_uniform
END MODULE  Library
