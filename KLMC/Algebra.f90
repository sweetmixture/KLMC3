MODULE Algebra

    USE Config
    USE Format
    USE Timer

    IMPLICIT NONE

CONTAINS

  !==========================================================================================!
  ! A subroutine to inverse 2x2 matrix
  !==========================================================================================!
  SUBROUTINE inverse2x2Matrix(matrixIn, matrixOut)

    REAL(KIND=DP), DIMENSION(2,2), INTENT(IN) :: matrixIn
    REAL(KIND=DP), DIMENSION(2,2), INTENT(OUT) :: matrixOut

    REAL(KIND=DP) :: det

    det = matrixIn(1, 1)*matrixIn(2, 2) - matrixIn(1, 2)*matrixIn(2, 1)

    matrixOut(1, 1) =  matrixIn(2, 2) / det
    matrixOut(1, 2) = -matrixIn(1, 2) / det
    matrixOut(2, 1) = -matrixIn(2, 1) / det
    matrixOut(2, 2) =  matrixIn(1, 1) / det

  END SUBROUTINE inverse2x2Matrix

  !==========================================================================================!
  ! A subroutine to create transformation matrix
  !==========================================================================================!
  SUBROUTINE transMatrix2D(alpha, matrix)

    REAL(KIND=DP), INTENT(IN)  :: alpha
    REAL(KIND=DP), DIMENSION(2,2), INTENT(OUT) :: matrix

    INTEGER :: i, j
    REAL(KIND=DP) :: angleRad

    DO i = 1, 2
      DO j = 1, 2
        matrix(i, j) = 0.0D0
      ENDDO
    ENDDO

    angleRad = alpha * deg2Rad

    matrix(1, 1) = 1
    matrix(1, 2) = 0.0
    matrix(2, 1) = cos(angleRad)
    matrix(2, 2) = sin(angleRad)

  END SUBROUTINE transMatrix2D


  !==========================================================================================!
  ! Compute the determinant of a 3x3 matrix.
  !==========================================================================================!
  FUNCTION M33DET (A) RESULT (DET)

    REAL(KIND=DP), DIMENSION(3,3), INTENT(IN)  :: A
    REAL(KIND=DP) :: DET

    DET =   A(1,1)*A(2,2)*A(3,3)  &
          - A(1,1)*A(2,3)*A(3,2)  &
          - A(1,2)*A(2,1)*A(3,3)  &
          + A(1,2)*A(2,3)*A(3,1)  &
          + A(1,3)*A(2,1)*A(3,2)  &
          - A(1,3)*A(2,2)*A(3,1)

    RETURN

  END FUNCTION M33DET

  !==========================================================================================!
  ! Calculates the eigenvalues and normalized eigenvectors of a hermitian 3x3
  ! matrix A using the Jacobi algorithm.
  ! The upper triangular part of A is destroyed during the calculation,
  ! the diagonal elements are read but not destroyed, and the lower
  ! triangular elements are not referenced at all.
  ! ----------------------------------------------------------------------------
  ! Parameters:
  !   A: The hermitian input matrix
  !   Q: Storage buffer for eigenvectors
  !   W: Storage buffer for eigenvalues
  ! Written by: (C) 2006  Joachim Kopp
  !==========================================================================================!
  SUBROUTINE ZHEEVJ3(A, Q, W)

    REAL(KIND=DP), DIMENSION(3,3), INTENT(INOUT)  :: A
    REAL(KIND=DP), DIMENSION(3,3), INTENT(OUT)    :: Q
    REAL(KIND=DP), DIMENSION(3)  , INTENT(OUT)    :: W

    INTEGER          :: N = 3

    DOUBLE PRECISION :: SD, SO
    DOUBLE PRECISION :: S, T
    DOUBLE PRECISION :: C
    DOUBLE PRECISION :: G, H, Z
    DOUBLE PRECISION :: THRESH
    INTEGER          :: I, X, Y, R

!   Initialize Q to the identitity matrix
!   --- This loop can be omitted if only the eigenvalues are desired ---
    DO X = 1, N
      Q(X,X) = 1.0D0
      DO Y = 1, X-1
        Q(X, Y) = 0.0D0
        Q(Y, X) = 0.0D0
      ENDDO
    ENDDO

!   Initialize W to diag(A)
    DO X = 1, N
      W(X) = A(X, X)
    ENDDO

!   Calculate SQR(tr(A))
    SD = 0.0D0
    DO X = 1, N
      SD = SD + ABS(W(X))
    ENDDO
    SD = SD**2

!   Main iteration loop
    DO I = 1, 50

!     Test for convergence
      SO = 0.0D0
      DO X = 1, N
        DO Y = X+1, N
          SO = SO + ABS(A(X, Y))
        ENDDO
      ENDDO

      IF (SO .EQ. 0.0D0) THEN
        RETURN
      END IF

      IF (I .LT. 5) THEN
        THRESH = 0.2D0 * SO / N**2
      ELSE
        THRESH = 0.0D0
      END IF

!     Do sweep
      DO X = 1, N
        DO Y = X+1, N
          G = 100.0D0 * ( ABS(A(X, Y)) )

          IF ((I .GT. 5) .AND. (ABS(W(X)) + G .EQ. ABS(W(X))) .AND. (ABS(W(Y)) + G .EQ. ABS(W(Y)))) THEN
            A(X, Y) = 0.0D0

          ELSE IF (ABS(A(X, Y)) .GT. THRESH) THEN

!           Calculate Jacobi transformation
            H = W(Y) - W(X)
            IF ( ABS(H) + G .EQ. ABS(H) ) THEN
              T = A(X, Y) / H
            ELSE
              IF (H .LE. 0.0D0) THEN
                T = -2.0D0 * A(X, Y) / (SQRT(H**2 + 4.0D0 * A(X, Y)**2) - H)
              ELSE IF (H .EQ. 0.0D0) THEN
                T = A(X, Y) * (1.0D0 / ABS(A(X, Y)))
              ELSE
                T = 2.0D0 * A(X, Y) / (SQRT(H**2 + 4.0D0 * A(X, Y)**2) + H)
              END IF
            END IF

            C = 1.0D0 / SQRT( 1.0D0 + T**2 )
            S = T * C
            Z = T * A(X, Y)

!           Apply Jacobi transformation
            A(X, Y) = 0.0D0
            W(X)    = W(X) - Z
            W(Y)    = W(Y) + Z

            DO R = 1, X-1
              T       = A(R, X)
              A(R, X) = C * T - S * A(R, Y)
              A(R, Y) = S * T + C * A(R, Y)
            ENDDO

            DO R = X+1, Y-1
              T       = A(X, R)
              A(X, R) = C * T - S * A(R, Y)
              A(R, Y) = S * T + C * A(R, Y)
            ENDDO

            DO R = Y+1, N
              T       = A(X, R)
              A(X, R) = C * T - S * A(Y, R)
              A(Y, R) = S * T + C * A(Y, R)
            ENDDO

!           Update eigenvectors
!           --- This loop can be omitted if only the eigenvalues are desired ---
            DO R = 1, N
              T       = Q(R, X)
              Q(R, X) = C * T - S * Q(R, Y)
              Q(R, Y) = S * T + C * Q(R, Y)
            ENDDO
          END IF
        ENDDO
      ENDDO
    ENDDO

    PRINT *, "ZHEEVJ3: No convergence."

  END SUBROUTINE ZHEEVJ3

  subroutine Jacobi(a,x,abserr)
  !===========================================================
  ! Evaluate eigenvalues and eigenvectors
  ! of a real symmetric matrix a(n,n): a*x = lambda*x
  ! method: Jacoby method for symmetric matrices
  ! Alex G. (December 2009)
  !-----------------------------------------------------------
  ! input ...
  ! a(n,n) - array of coefficients for matrix A
  ! n      - number of equations
  ! abserr - abs tolerance [sum of (off-diagonal elements)^2]
  ! output ...
  ! a(i,i) - eigenvalues
  ! x(i,j) - eigenvectors
  ! comments ...
  !===========================================================

  REAL(KIND=DP), DIMENSION(3,3), INTENT(INOUT) :: a
  REAL(KIND=DP), DIMENSION(3,3), INTENT(OUT)   :: x
  REAL(KIND=DP), INTENT(IN) :: abserr

  INTEGER :: i, j, k, n = 3
  REAL(KIND=DP) :: b2, bar
  REAL(KIND=DP) :: beta, coeff, c, s, cs, sc

  ! initialize x(i,j)=0, x(i,i)=1
  ! *** the array operation x=0.0 is specific for Fortran 90/95
  x = 0.0
  do i=1,n
    x(i,i) = 1.0
  end do

  ! find the sum of all off-diagonal elements (squared)
  b2 = 0.0
  do i=1,n
    do j=1,n
      if (i.ne.j) b2 = b2 + a(i,j)**2
    end do
  end do

  if (b2 <= abserr) return

  ! average for off-diagonal elements /2
  bar = 0.5*b2/float(n*n)

  do while (b2.gt.abserr)
    do i=1,n-1
      do j=i+1,n
        if (a(j,i)**2 <= bar) cycle  ! do not touch small elements
        b2 = b2 - 2.0*a(j,i)**2
        bar = 0.5*b2/float(n*n)
  ! calculate coefficient c and s for Givens matrix
        beta = (a(j,j)-a(i,i))/(2.0*a(j,i))
        coeff = 0.5*beta/sqrt(1.0+beta**2)
        s = sqrt(max(0.5+coeff,0.0))
        c = sqrt(max(0.5-coeff,0.0))
  ! recalculate rows i and j
        do k=1,n
          cs =  c*a(i,k)+s*a(j,k)
          sc = -s*a(i,k)+c*a(j,k)
          a(i,k) = cs
          a(j,k) = sc
        end do
  ! new matrix a_{k+1} from a_{k}, and eigenvectors
        do k=1,n
          cs =  c*a(k,i)+s*a(k,j)
          sc = -s*a(k,i)+c*a(k,j)
          a(k,i) = cs
          a(k,j) = sc
          cs =  c*x(k,i)+s*x(k,j)
          sc = -s*x(k,i)+c*x(k,j)
          x(k,i) = cs
          x(k,j) = sc
        end do
      end do
    end do
  end do
  return
  end subroutine Jacobi



END MODULE Algebra
