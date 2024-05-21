MODULE UnitCell
    USE Config
    USE Library
    IMPLICIT NONE

CONTAINS

SUBROUTINE defineClusterBox(inout_cluster)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    INTEGER :: i

    IF (index(UNIT_CELL,'cell').ne.0.OR.index(UNIT_CELL,'vector').ne.0) THEN
      DO i=1,6
        inout_cluster%box(i) = MASTER_CELL(i)
      ENDDO
    ENDIF

END SUBROUTINE defineClusterBox

SUBROUTINE updateCell(in_cluster, out_cell)
    TYPE(cluster), INTENT(IN) :: in_cluster
    REAL(KIND=DBL),DIMENSION(6),INTENT(OUT) :: out_cell(6)
    INTEGER :: i

    DO i=1,6
      out_cell(i)=in_cluster%box(i)
    ENDDO

END SUBROUTINE updateCell

SUBROUTINE copyBoxSize(in_cluster, inout_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    INTEGER :: i

    DO i=1,6
      inout_cluster%box(i)=in_cluster%box(i)
    ENDDO

END SUBROUTINE copyBoxSize

SUBROUTINE vect_to_box(inout_cluster,VECTOR)
   IMPLICIT none

   TYPE(cluster),INTENT(INOUT)              :: inout_cluster
   REAL(KIND=DBL),INTENT(IN),DIMENSION(3,3) :: VECTOR
   REAL(KIND=DBL)  :: temp

   !box(1) = a
   !box(2) = b
   !box(3) = c
   !box(4) = alpha
   !box(5) = beta
   !box(6) = gamma

   !a
   inout_cluster%box(1) = SQRT( VECTOR(1,1)**2 + &
                                VECTOR(1,2)**2 + &
                                VECTOR(1,3)**2 )
   !b
   inout_cluster%box(2) = SQRT( VECTOR(2,1)**2 + &
                                VECTOR(2,2)**2 + &
                                VECTOR(2,3)**2 )
   !c
   inout_cluster%box(3) = SQRT( VECTOR(3,1)**2 + &
                                VECTOR(3,2)**2 + &
                                VECTOR(3,3)**2 )

   !alpha
   temp = ( VECTOR(2,1) * VECTOR(3,1) + & 
            VECTOR(2,2) * VECTOR(3,2) + &
            VECTOR(2,3) * VECTOR(3,3))/ & 
            (inout_cluster%box(2) * inout_cluster%box(3))

   inout_cluster%box(4) = 180.0_dp * ACOS(temp)/PI

   !beta
   temp = ( VECTOR(1,1) * VECTOR(3,1) + & 
            VECTOR(1,2) * VECTOR(3,2) + &
            VECTOR(1,3) * VECTOR(3,3))/ & 
            (inout_cluster%box(1) * inout_cluster%box(3))

   inout_cluster%box(5) = 180.0_dp * ACOS(temp)/PI

   !gamma
   temp = ( VECTOR(1,1) * VECTOR(2,1) + & 
            VECTOR(1,2) * VECTOR(2,2) + &
            VECTOR(1,3) * VECTOR(2,3))/ & 
            (inout_cluster%box(1) * inout_cluster%box(2))

   inout_cluster%box(6) = 180.0_dp * ACOS(temp)/PI

   RETURN
END SUBROUTINE vect_to_box

!==========================================================================================!

SUBROUTINE checkCell(in_cluster,rejectflag)
  USE Config,only : stderr
  TYPE(cluster), INTENT(IN) :: in_cluster
  LOGICAL, INTENT(INOUT)    :: rejectflag
  INTEGER                   :: ide

  ide        = in_cluster%edefn
  rejectflag = .false.

  IF (in_cluster%box(4) + in_cluster%box(5) + in_cluster%box(6) > 360.0_dp) THEN
     !Regect cluster as approaching 2D system
     rejectflag = .true.
     WRITE(stderr,*)'Unphysical lattice! A+B+C> 360.0',in_cluster%box(:)
  !alpha - beta > gamma?
  ELSEIF (ABS(in_cluster%box(4) - in_cluster%box(5)) > in_cluster%box(6)) THEN
     rejectflag = .true.
     WRITE(stderr,*) 'Unphysical lattice! A-B > C',in_cluster%box(:)
  ! gamma - alpha > beta ?.
  ELSEIF (ABS(in_cluster%box(6) - in_cluster%box(4)) > in_cluster%box(5)) THEN
     rejectflag = .true.
     WRITE(stderr,*)'Unphysical lattice! C-A>B',in_cluster%box(:)
  ! beta - gamma > alpha?.
  ELSEIF (ABS(in_cluster%box(5) - in_cluster%box(6)) > in_cluster%box(4)) THEN
     rejectflag = .true.
     WRITE(stderr,*)'Unphysical lattice! B-C > A',in_cluster%box(:)
  END IF
  RETURN
  END SUBROUTINE checkCell
!==========================================================================================!

REAL(KIND=DBL) FUNCTION Volume()
     Volume=ABS(VECTOR(1,1)*(VECTOR(2,2)*VECTOR(3,3)-VECTOR(2,3)*VECTOR(3,2))- &
                VECTOR(2,1)*(VECTOR(1,2)*VECTOR(3,3)-VECTOR(1,3)*VECTOR(3,2))+ &
                VECTOR(3,1)*(VECTOR(1,2)*VECTOR(2,3)-VECTOR(1,3)*VECTOR(2,2)) )
END FUNCTION Volume

SUBROUTINE xctoxf(inout_cluster)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    REAL(KIND=DBL) :: a,b,c,cos_alpha,cos_beta,cos_gamma
    REAL(KIND=DBL) :: sin_gamma,r11,r12,r13,r22,r23,r33,v
    REAL(KIND=DBL) :: degree_to_radian,tiny
    INTEGER        :: i
    DATA  degree_to_radian/0.0174532925199433d0/
    DATA  tiny/0.1e-16/

    a = inout_cluster%box(1)
    b = inout_cluster%box(2)
    c = inout_cluster%box(3)
    cos_alpha = cos(inout_cluster%box(4)*degree_to_radian)
    cos_beta  = cos(inout_cluster%box(5)*degree_to_radian)
    cos_gamma = cos(inout_cluster%box(6)*degree_to_radian)
    sin_gamma = sin(inout_cluster%box(6)*degree_to_radian)

    !print*,'a',a,'b',b,'c',c
    !print*,'cos(a)',cos_alpha,'cos(b)',cos_beta
    !print*,'cos(g)',cos_gamma,'sin(g)',sin_gamma

    v = sqrt(1.0d0 - cos_alpha*cos_alpha - cos_beta*cos_beta &
              - cos_gamma*cos_gamma + 2.0*cos_alpha*cos_beta*cos_gamma )

    !print*,'v',v

    r11 = 1.0d0/a
    r12 = -cos_gamma/(a*sin_gamma)
    r13 = (cos_alpha*cos_gamma-cos_beta)/(a*v*sin_gamma)
   !r21 = 0.0d0
    r22 = 1.d0/(b*sin_gamma)
    r23 = (cos_beta*cos_gamma-cos_alpha)/(b*v*sin_gamma)
   !r31 = 0.0d0
   !r32 = 0.0d0
    r33 = sin_gamma/(c*v)

    IF (abs(r11).lt.tiny) r11=0.0d0
    IF (abs(r12).lt.tiny) r12=0.0d0
    IF (abs(r13).lt.tiny) r13=0.0d0
    IF (abs(r22).lt.tiny) r22=0.0d0
    IF (abs(r23).lt.tiny) r23=0.0d0
    IF (abs(r33).lt.tiny) r33=0.0d0

    DO i=1,N_ATOMS
      inout_cluster%atoms(i)%xf = mod (r11*inout_cluster%atoms(i)%xc &
                                     + r12*inout_cluster%atoms(i)%yc &
                                     + r13*inout_cluster%atoms(i)%zc,1.0d0)
      inout_cluster%atoms(i)%yf = mod (r22*inout_cluster%atoms(i)%yc &
                                     + r23*inout_cluster%atoms(i)%zc,1.0d0)
      inout_cluster%atoms(i)%zf = mod (r33*inout_cluster%atoms(i)%zc,1.0d0)
    ENDDO

END SUBROUTINE xctoxf

SUBROUTINE xftoxc(inout_cluster)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    INTEGER        :: i

    DO i=1,N_ATOMS
      inout_cluster%atoms(i)%xc = inout_cluster%atoms(i)%xf * VECTOR(1,1) &
                                + inout_cluster%atoms(i)%yf * VECTOR(1,2) &
                                + inout_cluster%atoms(i)%zf * VECTOR(1,3)
      inout_cluster%atoms(i)%yc = inout_cluster%atoms(i)%xf * VECTOR(2,1) &
                                + inout_cluster%atoms(i)%yf * VECTOR(2,2) &
                                + inout_cluster%atoms(i)%zf * VECTOR(2,3)
      inout_cluster%atoms(i)%zc = inout_cluster%atoms(i)%xf * VECTOR(3,1) &
                                + inout_cluster%atoms(i)%yf * VECTOR(3,2) &
                                + inout_cluster%atoms(i)%zf * VECTOR(3,3)
    ENDDO

END SUBROUTINE xftoxc

!________________________________________________________________________
!
!   Subroutines below adapted from Alexey Sokol 2011 of GULP 3
!________________________________________________________________________


    SUBROUTINE  lattice_c2v_3D(unit_cell,vect)

! Supplies matrix of Cartesian coordinates of lattice vectors
! from linear and angular lattice parameters, defined in  CELL(1:6)

! Vector a looks along axis X
! Vector b is in XY plane under angle gamma to vector a
! Vector c is over XY plane under angles alpha and beta to vectors b and a, resp
! The orientation of vectors corresponds to the right trio

    REAL(KIND=DBL), INTENT(IN) :: unit_cell(6)
    REAL(KIND=DBL), INTENT(INOUT) :: vect(3,3)
    REAL(KIND=DBL) :: a, b, c, alpha, beta, gamma
    REAL(KIND=DBL) :: tiny, degree_to_radian, f, g
    REAL(KIND=DBL) :: cos_alpha, cos_beta, cos_gamma, sin_gamma

    DATA  degree_to_radian/0.0174532925199433d0/
    DATA  tiny/0.1e-20/

    a=unit_cell(1)
    b=unit_cell(2)
    c=unit_cell(3)
    alpha=unit_cell(4)
    beta= unit_cell(5)
    gamma=unit_cell(6)

    if (alpha.eq.90.0) then
      cos_alpha = 0.0_dp
    else
      cos_alpha = cos(alpha*degree_to_radian)
    endif
    if (beta.eq.90.0) then
      cos_beta = 0.0_dp
    else
      cos_beta = cos(beta*degree_to_radian)
    endif
    if (gamma.eq.90.0) then
      sin_gamma = 1.0_dp
      cos_gamma = 0.0_dp
    else
      sin_gamma = sin(gamma*degree_to_radian)
      cos_gamma = cos(gamma*degree_to_radian)
    endif

    f = (cos_alpha - cos_beta*cos_gamma)/sin_gamma
    g = (1.0_dp - cos_beta*cos_beta - f*f)

    !GULP INDEX ORDERING
    vect(1,1) = a
    vect(2,1) = 0.0_dp
    vect(3,1) = 0.0_dp
    vect(1,2) = b*cos_gamma
    vect(2,2) = b*sin_gamma
    vect(3,2) = 0.0_dp
    vect(1,3) = c*cos_beta
    vect(2,3) = c*f
    IF (g.lt.tiny) THEN
      vect(3,3) = 0.0_dp
    ELSE
      vect(3,3) = c*sqrt(g)
    ENDIF


    !ALEXEY INDEX ORDERING
    !vect(1,1) = a
    !vect(1,2) = 0.0_dp
    !vect(1,3) = 0.0_dp
    !vect(2,1) = b*cos_gamma
    !vect(2,2) = b*sin_gamma
    !vect(2,3) = 0.0_dp
    !vect(3,1) = c*cos_beta
    !vect(3,2) = c*f
    !IF (g.lt.tiny) THEN
    !  vect(3,3) = 0.0d0
    !ELSE
    !  vect(3,3) = c*sqrt(g)
    !ENDIF

END SUBROUTINE lattice_c2v_3D

    SUBROUTINE  lattice_c2v_2D(unit_cell,vect)

! Supplies matrix of Cartesian coordinates of lattice vectors
! from linear and angular lattice parameters, defined in  CELL(1:6)

! Vector a looks along axis X
! Vector b is in XY plane under angle gamma to vector a
! Vector c is not changed as already in Cartesian and not periodic

    REAL(KIND=DBL), INTENT(IN) :: unit_cell(6)
    REAL(KIND=DBL), INTENT(INOUT) :: vect(3,3)
    REAL(KIND=DBL) :: a, b, alpha
    REAL(KIND=DBL) :: degree_to_radian
    REAL(KIND=DBL) :: cos_alpha, sin_alpha

    DATA  degree_to_radian/0.0174532925199433d0/

    a=unit_cell(1)
    b=unit_cell(2)
    alpha=unit_cell(4)

    if (alpha.eq.90.0) then
      cos_alpha = 0.0_dp
      sin_alpha = 1.0_dp
    else
      cos_alpha = cos(alpha*degree_to_radian)
      sin_alpha = sin(alpha*degree_to_radian)
    endif

    !GULP ORIENTATION
    vect(1,1) = a
    vect(2,1) = 0.0_dp
    vect(3,1) = 0.0_dp
    vect(1,2) = b*cos_alpha
    vect(2,2) = b*sin_alpha
    vect(3,2) = 0.0_dp
    vect(1,3) = 0.0_dp
    vect(2,3) = 0.0_dp
    vect(3,3) = 0.0_dp

END SUBROUTINE lattice_c2v_2D

    SUBROUTINE  lattice_c2v_1D(unit_cell,vect)

! Supplies matrix of Cartesian coordinates of lattice vectors
! from linear and angular lattice parameters, defined in  CELL(1:6)

! Vector a looks along axis X
! Vector b is not changed as already in Cartesian and not periodic
! Vector c is not changed as already in Cartesian and not periodic

    REAL(KIND=DBL), INTENT(IN) :: unit_cell(6)
    REAL(KIND=DBL), INTENT(INOUT) :: vect(3,3)
    REAL(KIND=DBL) :: a

    a=unit_cell(1)

    vect(1,1) = a
    vect(2,1) = 0.0_dp
    vect(3,1) = 0.0_dp
    vect(1,2) = 0.0_dp
    vect(2,2) = 0.0_dp
    vect(3,2) = 0.0_dp
    vect(1,3) = 0.0_dp
    vect(2,3) = 0.0_dp
    vect(3,3) = 0.0_dp

END SUBROUTINE lattice_c2v_1D

    SUBROUTINE  lattice_v2c_3D(vect,unit_cell)

! Supplies linear and angular lattice parameters, defined in CELL(1:6)
! from the matrix of Cartesian coordinates of lattice vectors

    REAL(KIND=DBL), INTENT(IN)  :: vect(3,3)
    REAL(KIND=DBL), INTENT(OUT) :: unit_cell(6)
    REAL(KIND=DBL) :: a, b, c, alpha, beta, gamma
    REAL(KIND=DBL) :: round_off_error, radian_to_degree
    REAL(KIND=DBL) :: f(3), g(3)
    INTEGER        :: i

    DATA  radian_to_degree/57.295777918682049d0/
    DATA  round_off_error/0.00001d0/

    DO i=1,3
      f(i)=0.0_dp
      g(i)=0.0_dp
    ENDDO
    DO i=1,3
      !ALEXEY ORIENTATION?
      !f(1)=f(1)+vect(1,i)*vect(1,i) etc
      !GULP ORIENTATION
      f(1)=f(1)+vect(i,1)*vect(i,1)
      f(2)=f(2)+vect(i,2)*vect(i,2)
      f(3)=f(3)+vect(i,3)*vect(i,3)
      g(1)=g(1)+vect(i,2)*vect(i,3)
      g(2)=g(2)+vect(i,3)*vect(i,1)
      g(3)=g(3)+vect(i,1)*vect(i,2)
    ENDDO
    a = sqrt(ABS(f(1)))
    b = sqrt(ABS(f(2)))
    c = sqrt(ABS(f(3)))
    alpha = radian_to_degree*acos(g(1)/(b*c))
    beta  = radian_to_degree*acos(g(2)/(c*a))
    gamma = radian_to_degree*acos(g(3)/(a*b))

    IF(ABS(alpha- 90.0).lt.round_off_error) alpha =  90.0_dp
    IF(ABS(alpha-120.0).lt.round_off_error) alpha = 120.0_dp
    IF(ABS(beta - 90.0).lt.round_off_error) beta  =  90.0_dp
    IF(ABS(beta -120.0).lt.round_off_error) beta  = 120.0_dp
    IF(ABS(gamma- 90.0).lt.round_off_error) gamma =  90.0_dp
    IF(ABS(gamma-120.0).lt.round_off_error) gamma = 120.0_dp

    unit_cell(1)=a
    unit_cell(2)=b
    unit_cell(3)=c
    unit_cell(4)=alpha
    unit_cell(5)=beta
    unit_cell(6)=gamma

END SUBROUTINE lattice_v2c_3D

    SUBROUTINE  lattice_v2c_2D(vect,unit_cell)

! Supplies linear and angular lattice parameters, defined in CELL(1:6)
! from the matrix of Cartesian coordinates of lattice vectors

    REAL(KIND=DBL), INTENT(IN) :: vect(3,3)
    REAL(KIND=DBL), INTENT(OUT) :: unit_cell(6)
    REAL(KIND=DBL) :: a, b, alpha
    REAL(KIND=DBL) :: round_off_error, radian_to_degree
    REAL(KIND=DBL) :: f(3)
    INTEGER        :: i

    DATA  radian_to_degree/57.295777918682049d0/
    DATA  round_off_error/0.00001d0/

    DO i=1,2
      f(i)=0.0_dp
    ENDDO
    DO i=1,2
      !GULP ORIENTATION
      f(1)=f(1)+vect(i,1)*vect(i,1)
      f(2)=f(2)+vect(i,2)*vect(i,2)
      f(3)=f(3)+vect(i,1)*vect(i,2)
    ENDDO
    a = sqrt(ABS(f(1)))
    b = sqrt(ABS(f(2)))
    alpha = radian_to_degree*acos(f(3)/(a*b))

    IF(ABS(alpha- 90.0).lt.round_off_error) alpha =  90.0_dp
    IF(ABS(alpha-120.0).lt.round_off_error) alpha = 120.0_dp

    unit_cell(1)=a
    unit_cell(2)=b
    unit_cell(4)=alpha

END SUBROUTINE lattice_v2c_2D

    SUBROUTINE  lattice_v2c_1D(vect,unit_cell)

! Supplies linear and angular lattice parameters, defined in CELL(1:6)
! from the matrix of Cartesian coordinates of lattice vectors

    REAL(KIND=DBL), INTENT(IN) :: vect(3,3)
    REAL(KIND=DBL), INTENT(OUT) :: unit_cell(6)
    REAL(KIND=DBL) :: a

    a = ABS(vect(1,1))

    unit_cell(1)=a

END SUBROUTINE lattice_v2c_1D

    SUBROUTINE transform_3(device,ri,rf)

! Transforms coordinates of a vector from fractional into Cartesian
! device, transformation matrix of first rank, calculated beforehand
! as Cartesian coordinates of lattice vectors and reciprocal lattice
! vectors respectively.

    REAL(KIND=DBL), INTENT(IN) :: device(3,3), ri(3)
    REAL(KIND=DBL), INTENT(OUT) :: rf(3)
    INTEGER :: i

    DO i=1,3
       rf(i)=ri(1)*device(i,1)+ri(2)*device(i,2)+ri(3)*device(i,3)
    ENDDO

END SUBROUTINE transform_3

!________________________________________________________________________
!
!   Subroutines below adapted from Alexey Sokol 2011
!________________________________________________________________________


    SUBROUTINE FIND_NN(m_check,cutoff,neighbours,dist,in_cluster,in_r0)


! Originally finds neighbours of atom m_check in UC within the cutoff distance 
! Now extended to include neighbouring UC within cutoff distance

! Declarations

    TYPE(cluster), INTENT(IN) :: in_cluster
    INTEGER, INTENT(IN) :: m_check
    INTEGER, INTENT(OUT):: neighbours(0:*)
    REAL(KIND=DBL), INTENT(IN)    :: cutoff
    REAL(KIND=DBL), INTENT(OUT):: dist(1:*)
    REAL(KIND=DBL), INTENT(IN), OPTIONAL :: in_r0(3)

!   Local declarations
    REAL(KIND=DBL) :: cell(6),f_centre(3),f(3),r,fx,fy,fz
    REAL(KIND=DBL) :: r_v,degree_to_radian
    INTEGER :: i, j, k, n_neighbours, i_max, j_max, k_max, ni
    LOGICAL :: pbc, debug=.false.
    DATA  degree_to_radian/0.0174532925199433_DP/

    IF (m_check.eq.0.AND.PRESENT(in_r0)) THEN
      f_centre(1)=in_r0(1)
      f_centre(2)=in_r0(2)
      f_centre(3)=in_r0(3)
    ELSE
      f_centre(1)=in_cluster%atoms(m_check)%xf
      f_centre(2)=in_cluster%atoms(m_check)%yf
      f_centre(3)=in_cluster%atoms(m_check)%zf
    ENDIF

    pbc=(index(UNIT_CELL,'cell').ne.0.OR.index(UNIT_CELL,'vector').ne.0)

    IF (pbc) THEN ! compute volume of unit cell etc

     CALL updateCell(in_cluster,cell)

!    Calculate extensions of UC which includes images withhin sphere of radius 
     r_v=cutoff/volume()
     i_max=r_v*cell(2)*cell(3)*ABS(SIN(degree_to_radian*cell(4)))+1
     j_max=r_v*cell(1)*cell(3)*ABS(SIN(degree_to_radian*cell(5)))+1
     k_max=r_v*cell(1)*cell(2)*ABS(SIN(degree_to_radian*cell(6)))+1

     if (debug) then
       print*,'CELL PARMETERS'
       print*,(cell(ni),ni=1,3)
       print*,(cell(ni),ni=4,6)
       print*,''
       print*,'central atom at',(f_centre(i),i=1,3)
     endif

!    Find neighbours
     n_neighbours = 0
     DO ni = 1, N_ATOMS
       f(1)=in_cluster%atoms(ni)%xf
       f(2)=in_cluster%atoms(ni)%yf
       f(3)=in_cluster%atoms(ni)%zf
       if (debug) then
         print*,'   next atom at',(f(i),i=1,3)
       endif
       CALL translate_3(VECTOR,f_centre,f)
       if (debug) then
         print*,'   translate to',(f(i),i=1,3)
       endif
       fx=f(1)
       fy=f(2)
       fz=f(3)
       DO i = -i_max,i_max
       DO j = -j_max,j_max
       DO k = -k_max,k_max
          IF (ni.eq.m_check.AND.i.eq.0.AND.j.eq.0.AND.k.eq.0) THEN
          ELSE
           f(1)=fx+i
           f(2)=fy+j
           f(3)=fz+k
           r=distance_3(VECTOR,f,f_centre)
           IF (r.le.cutoff) THEN
             n_neighbours = n_neighbours + 1
             neighbours(n_neighbours)=ni
             dist(n_neighbours)=r
             if (debug) then
               print*,'n image r',ni,i,j,k,r
               print*,'n image r',f(1),f(2),f(3)
             endif
           ENDIF
          ENDIF
       ENDDO
       ENDDO
       ENDDO
     ENDDO
     neighbours(0) = n_neighbours

    ELSE ! we have a cluster

!    Find neighbours
     n_neighbours = 0
     DO ni = 1, N_ATOMS
       f(1)=f_centre(1)-in_cluster%atoms(ni)%xc
       f(2)=f_centre(2)-in_cluster%atoms(ni)%yc
       f(3)=f_centre(3)-in_cluster%atoms(ni)%zc
       r=sqrt(f(1)*f(1)+f(2)*f(2)+f(3)*f(3))
       IF (r.le.cutoff) THEN
         n_neighbours = n_neighbours + 1
         neighbours(n_neighbours)=ni
         dist(n_neighbours)=r
       ENDIF
     ENDDO
     neighbours(0) = n_neighbours - 1

    ENDIF ! might have to expand to 1D and 2D

    if (debug) print*,'coordn of atom ',m_check,' is ',neighbours(0)

END SUBROUTINE FIND_NN

      
REAL(KIND=DBL) FUNCTION distance_3(device,f1,f2)

! Calculates absolute distance between two points f1 and f2 defined by their
! fractional coordinates.
! device, transformation matrix of first rank, calculated beforehand as
! Cartesian coordinates of lattice vectors.

    REAL(KIND=DBL), INTENT(IN) :: device(3,3), f1(3), f2(3)
    REAL(KIND=DBL)             :: d(3), r(3)
    INTEGER          :: i

    DO i=1,3
       d(i)=f2(i)-f1(i)
    ENDDO
    CALL transform_3(device,d,r)
    distance_3=sqrt( r(1)*r(1)+r(2)*r(2)+r(3)*r(3) )

END FUNCTION distance_3


    SUBROUTINE translate_3(device,f1,f2)

! Translates point f2 to the closest position to f1 
! f1 and f2 defined by their fractional coordinates. 
! device is the transformation matrix of first rank,
!      i.e. Cartesian coordinates of lattice vectors.

    REAL(KIND=DBL), INTENT(IN) :: device(3,3), f1(3)
    REAL(KIND=DBL), INTENT(INOUT) :: f2(3)

    REAL(KIND=DBL)    :: d(3),p(3),r(3),dist2n,dist2o
    INTEGER :: i,j,k,l

    DO k = 1,3
       d(k)=mod(f2(k)-f1(k),1.0d0)
       !if(d(k).lt.0.d0) d(k)=d(k)+1.d0
    ENDDO
    dist2n=0.d0
    dist2o=1.d32
    DO i = -2,2
       p(1)=d(1)+float(i)
       DO j=-2,2
          p(2)=d(2)+float(j)
          DO k=-2,2
             p(3)=d(3)+float(k)
             call transform_3(device,p,r)
             dist2n=r(1)**2+r(2)**2+r(3)**2
             IF(dist2n.lt.dist2o) then
                dist2o=dist2n
                DO l=1,3
                   f2(l)=f1(l)+p(l)
                ENDDO
             ENDIF
          ENDDO
       ENDDO
    ENDDO

END SUBROUTINE translate_3

!________________________________________________________________________
!
!   Subroutines below adapted from G42 as written by Arnulf (2013)
!________________________________________________________________________

    SUBROUTINE rectifyCell(inout_cluster)

! Subroutine rectifies a cell to have shortest lengths and shifts the atom positions

    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    REAL(KIND=DBL) :: old_uc_diag2, new_uc_diag2, acc, frac, shift_to_positive
    INTEGER :: h, k, m, n, hv1,hv2
    LOGICAL :: uc_modified

    IF (N_PBC /= 3) RETURN

    acc = 1.0d-6
    shift_to_positive = 1000.0_dp

    CALL lattice_c2v_3D(inout_cluster%box,VECTOR)

    DO ! until no more reduction is possible
     uc_modified = .FALSE.

     DO k = 1,3
      old_uc_diag2 = 0.0d0
      DO m = 1,3
        old_uc_diag2 = old_uc_diag2 + VECTOR(m,k)*VECTOR(m,k)
        !WRITE(stdsee,'(A,I3,A,I3,A,E25.18)') 'm = ',m,' k = ',k,' VECTOR(m,k) = ',VECTOR(m,k)
      ENDDO
      !WRITE(stdsee,'(A,E25.18)') 'old unit cell diagonal2 = ',old_uc_diag2

      DO h = 1,3 ! loop over cell vectors

        IF (h /= k) THEN ! add/subtract cell vector h to modify two remaining cell vectors

          new_uc_diag2 = 0.0d0
          DO m = 1,3
            new_uc_diag2 = new_uc_diag2 + (VECTOR(m,k) - VECTOR(m,h))**2
          ENDDO
          !WRITE(stdsee,'(A,E25.18)') '1st new unit cell diagonal2 = ',new_uc_diag2

          IF (new_uc_diag2 < (old_uc_diag2-acc)) THEN ! successful, so apply transformation
            DO m = 1,3
              VECTOR(m,k) = VECTOR(m,k) - VECTOR(m,h)
            ENDDO
            ! and now add h to each atom (before displacing out and back into central uc)
            DO n = 1, N_ATOMS
              IF (k.eq.1) THEN
                frac = inout_cluster%atoms(n)%xf + shift_to_positive
              ELSEIF (k.eq.2) THEN
                frac = inout_cluster%atoms(n)%yf + shift_to_positive
              ELSE ! (k.eq.3)
                frac = inout_cluster%atoms(n)%zf + shift_to_positive
              ENDIF
              IF (h.eq.1) THEN
                inout_cluster%atoms(n)%xf = mod(inout_cluster%atoms(n)%xf + frac, 1.0d0)
              ELSEIF (h.eq.2) THEN
                inout_cluster%atoms(n)%yf = mod(inout_cluster%atoms(n)%yf + frac, 1.0d0)
              ELSE ! (h.eq.3)
                inout_cluster%atoms(n)%zf = mod(inout_cluster%atoms(n)%zf + frac, 1.0d0)
              ENDIF
            ENDDO
            old_uc_diag2 = new_uc_diag2
            uc_modified = .TRUE.
          ENDIF

          new_uc_diag2 = 0.0d0
          DO m = 1,3
            new_uc_diag2 = new_uc_diag2 + (VECTOR(m,k) + VECTOR(m,h))**2
          ENDDO
          !WRITE(stdsee,'(A,E25.18)') '2nd new unit cell diagonal2 = ',new_uc_diag2

          IF (new_uc_diag2 < (old_uc_diag2-acc)) THEN ! successful, so apply transformation
            DO m = 1,3
              VECTOR(m,k) = VECTOR(m,k) + VECTOR(m,h)
            ENDDO
            ! and now add -h to each atom (before displacing out and back into central uc)
            DO n = 1, N_ATOMS
              IF (k.eq.1) THEN
                frac = inout_cluster%atoms(n)%xf - shift_to_positive
              ELSEIF (k.eq.2) THEN
                frac = inout_cluster%atoms(n)%yf - shift_to_positive
              ELSE ! (k.eq.3)
                frac = inout_cluster%atoms(n)%zf - shift_to_positive
              ENDIF
              IF (h.eq.1) THEN
                inout_cluster%atoms(n)%xf = mod(inout_cluster%atoms(n)%xf - frac, 1.0d0)
              ELSEIF (h.eq.2) THEN
                inout_cluster%atoms(n)%yf = mod(inout_cluster%atoms(n)%yf - frac, 1.0d0)
              ELSE ! (h.eq.3)
                inout_cluster%atoms(n)%zf = mod(inout_cluster%atoms(n)%zf - frac, 1.0d0)
              ENDIF
            ENDDO
            old_uc_diag2 = new_uc_diag2
            uc_modified = .TRUE.
          ENDIF

        ELSE ! (h == k) so now try using two cell vectors in order to modify cell vector k
       !ELSEIF (h == 10) THEN !DEBUG above only

          hv1 = 1+mod(k,3)
          hv2 = 1+mod((k+1),3)

          new_uc_diag2 = 0.0d0
          DO m = 1,3
            new_uc_diag2 = new_uc_diag2 + (VECTOR(m,k) + VECTOR(m,hv1) + VECTOR(m,hv2))**2
          ENDDO
          !WRITE(stdsee,'(A,E25.18)') '3rd new unit cell diagonal2 = ',new_uc_diag2

          IF (new_uc_diag2 < (old_uc_diag2-acc)) THEN ! successful, so apply transformation
            do m = 1,3
              VECTOR(m,k) = VECTOR(m,k) + VECTOR(m,hv1) + VECTOR(m,hv2)
            enddo
            ! and now move each atom back (before displacing out and back into central uc)
            DO n = 1, N_ATOMS
              IF (k.eq.1) THEN
                frac = inout_cluster%atoms(n)%xf - shift_to_positive
              ELSEIF (k.eq.2) THEN
                frac = inout_cluster%atoms(n)%yf - shift_to_positive
              ELSE ! (k.eq.3)
                frac = inout_cluster%atoms(n)%zf - shift_to_positive
              ENDIF
              IF (hv1.eq.1) THEN
                inout_cluster%atoms(n)%xf = mod(inout_cluster%atoms(n)%xf - frac, 1.0d0)
              ELSEIF (hv1.eq.2) THEN
                inout_cluster%atoms(n)%yf = mod(inout_cluster%atoms(n)%yf - frac, 1.0d0)
              ELSE ! (hv1.eq.3)
                inout_cluster%atoms(n)%zf = mod(inout_cluster%atoms(n)%zf - frac, 1.0d0)
              ENDIF
              IF (hv2.eq.1) THEN
                inout_cluster%atoms(n)%xf = mod(inout_cluster%atoms(n)%xf - frac, 1.0d0)
              ELSEIF (hv2.eq.2) THEN
                inout_cluster%atoms(n)%yf = mod(inout_cluster%atoms(n)%yf - frac, 1.0d0)
              ELSE ! (hv2.eq.3)
                inout_cluster%atoms(n)%zf = mod(inout_cluster%atoms(n)%zf - frac, 1.0d0)
              ENDIF
            ENDDO
            old_uc_diag2 = new_uc_diag2
            uc_modified = .TRUE.
          ENDIF

          new_uc_diag2 = 0.0d0
          DO m = 1,3
            new_uc_diag2 = new_uc_diag2 + (VECTOR(m,k) + VECTOR(m,hv1) - VECTOR(m,hv2))**2
          ENDDO
          !WRITE(stdsee,'(A,E25.18)') '4th new unit cell diagonal2 = ',new_uc_diag2

          IF (new_uc_diag2 < (old_uc_diag2-acc)) THEN ! successful, so apply transformation
            do m = 1,3
              VECTOR(m,k) = VECTOR(m,k) + VECTOR(m,hv1) - VECTOR(m,hv2)
            enddo
            ! and now move each atom back (before displacing out and back into central uc)
            DO n = 1, N_ATOMS
              IF (k.eq.1) THEN
                frac = inout_cluster%atoms(n)%xf - shift_to_positive
              ELSEIF (k.eq.2) THEN
                frac = inout_cluster%atoms(n)%yf - shift_to_positive
              ELSE ! (k.eq.3)
                frac = inout_cluster%atoms(n)%zf - shift_to_positive
              ENDIF
              IF (hv1.eq.1) THEN
                inout_cluster%atoms(n)%xf = mod(inout_cluster%atoms(n)%xf - frac, 1.0d0)
              ELSEIF (hv1.eq.2) THEN
                inout_cluster%atoms(n)%yf = mod(inout_cluster%atoms(n)%yf - frac, 1.0d0)
              ELSE ! (hv1.eq.3)
                inout_cluster%atoms(n)%zf = mod(inout_cluster%atoms(n)%zf - frac, 1.0d0)
              ENDIF
              frac = frac + 20.0d0
              IF (hv2.eq.1) THEN
                inout_cluster%atoms(n)%xf = mod(inout_cluster%atoms(n)%xf + frac, 1.0d0)
              ELSEIF (hv2.eq.2) THEN
                inout_cluster%atoms(n)%yf = mod(inout_cluster%atoms(n)%yf + frac, 1.0d0)
              ELSE ! (hv2.eq.3)
                inout_cluster%atoms(n)%zf = mod(inout_cluster%atoms(n)%zf + frac, 1.0d0)
              ENDIF
            ENDDO
            old_uc_diag2 = new_uc_diag2
            uc_modified = .TRUE.
          ENDIF

          new_uc_diag2 = 0.0d0
          DO m = 1,3
            new_uc_diag2 = new_uc_diag2 + (VECTOR(m,k) - VECTOR(m,hv1) + VECTOR(m,hv2))**2
          ENDDO
          !WRITE(stdsee,'(A,E25.18)') '5th new unit cell diagonal2 = ',new_uc_diag2

          IF (new_uc_diag2 < (old_uc_diag2-acc)) THEN ! successful, so apply transformation
            do m = 1,3
              VECTOR(m,k) = VECTOR(m,k) - VECTOR(m,hv1) + VECTOR(m,hv2)
            enddo
            ! and now move each atom back (before displacing out and back into central uc)
            DO n = 1, N_ATOMS
              IF (k.eq.1) THEN
                frac = inout_cluster%atoms(n)%xf + shift_to_positive
              ELSEIF (k.eq.2) THEN
                frac = inout_cluster%atoms(n)%yf + shift_to_positive
              ELSE ! (k.eq.3)
                frac = inout_cluster%atoms(n)%zf + shift_to_positive
              ENDIF
              IF (hv1.eq.1) THEN
                inout_cluster%atoms(n)%xf = mod(inout_cluster%atoms(n)%xf + frac, 1.0d0)
              ELSEIF (hv1.eq.2) THEN
                inout_cluster%atoms(n)%yf = mod(inout_cluster%atoms(n)%yf + frac, 1.0d0)
              ELSE ! (hv1.eq.3)
                inout_cluster%atoms(n)%zf = mod(inout_cluster%atoms(n)%zf + frac, 1.0d0)
              ENDIF
              frac = frac - shift_to_positive - shift_to_positive
              IF (hv2.eq.1) THEN
                inout_cluster%atoms(n)%xf = mod(inout_cluster%atoms(n)%xf - frac, 1.0d0)
              ELSEIF (hv2.eq.2) THEN
                inout_cluster%atoms(n)%yf = mod(inout_cluster%atoms(n)%yf - frac, 1.0d0)
              ELSE ! (hv2.eq.3)
                inout_cluster%atoms(n)%zf = mod(inout_cluster%atoms(n)%zf - frac, 1.0d0)
              ENDIF
            ENDDO
            old_uc_diag2 = new_uc_diag2
            uc_modified = .TRUE.
          ENDIF

          new_uc_diag2 = 0.0d0
          DO m = 1,3
            new_uc_diag2 = new_uc_diag2 + (VECTOR(m,k) - VECTOR(m,hv1) - VECTOR(m,hv2))**2
          ENDDO
          !WRITE(stdsee,'(A,E25.18)') '6th new unit cell diagonal2 = ',new_uc_diag2

          IF (new_uc_diag2 < (old_uc_diag2-acc)) THEN ! successful, so apply transformation
            do m = 1,3
              VECTOR(m,k) = VECTOR(m,k) - VECTOR(m,hv1) - VECTOR(m,hv2)
            enddo
            ! and now move each atom back (before displacing out and back into central uc)
            DO n = 1, N_ATOMS
              IF (k.eq.1) THEN
                frac = inout_cluster%atoms(n)%xf + shift_to_positive
              ELSEIF (k.eq.2) THEN
                frac = inout_cluster%atoms(n)%yf + shift_to_positive
              ELSE ! (k.eq.3)
                frac = inout_cluster%atoms(n)%zf + shift_to_positive
              ENDIF
              IF (hv1.eq.1) THEN
                inout_cluster%atoms(n)%xf = mod(inout_cluster%atoms(n)%xf + frac, 1.0d0)
              ELSEIF (hv1.eq.2) THEN
                inout_cluster%atoms(n)%yf = mod(inout_cluster%atoms(n)%yf + frac, 1.0d0)
              ELSE ! (hv1.eq.3)
                inout_cluster%atoms(n)%zf = mod(inout_cluster%atoms(n)%zf + frac, 1.0d0)
              ENDIF
              IF (hv2.eq.1) THEN
                inout_cluster%atoms(n)%xf = mod(inout_cluster%atoms(n)%xf + frac, 1.0d0)
              ELSEIF (hv2.eq.2) THEN
                inout_cluster%atoms(n)%yf = mod(inout_cluster%atoms(n)%yf + frac, 1.0d0)
              ELSE ! (hv2.eq.3)
                inout_cluster%atoms(n)%zf = mod(inout_cluster%atoms(n)%zf + frac, 1.0d0)
              ENDIF
            ENDDO
            old_uc_diag2 = new_uc_diag2
            uc_modified = .TRUE.
          ENDIF

        ENDIF ! all four (x2) linear combinations considered for changing one unit cell vector

      ENDDO ! loop over cell vectors h

     ENDDO ! loop over three components k

     IF (.NOT.uc_modified) EXIT
    ENDDO

    !WRITE(stdsee,'(A,6G10.4)') 'Before: ',(inout_cluster%box(k),k=1,6)
    CALL lattice_v2c_3D(VECTOR,inout_cluster%box)
    !WRITE(stdsee,'(A,6G10.4)') 'After:  ',(inout_cluster%box(k),k=1,6)
    CALL xftoxc(inout_cluster)

    RETURN
END SUBROUTINE rectifyCell

END MODULE UnitCell
