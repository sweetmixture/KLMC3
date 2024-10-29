MODULE RefineSprings

    USE Config
    USE Library
    USE ClusterRoutines, ONLY : getMadelungPot, relaxSprings
    USE Master,          ONLY : resetCluster
    USE Population,      ONLY : MASTER_CLUSTER, initialiseCluster
    USE UnitCell,        ONLY : FIND_NN

    IMPLICIT NONE

!==========================================================================================!
CONTAINS

! routines for Refining Spring Constants 
!
! subroutines: initialiseSprings reportSprings runRefineSprings updateSprings
!
!  functions:  

!==========================================================================================!

SUBROUTINE runRefineSprings
    REAL(KIND=DBL) :: kB = 1.380658E-23 / 1.60217733E-19
    TYPE(cluster):: new_cluster
    REAL(KIND=DBL) :: kT
    INTEGER :: refining, n, scheme=2
          
    kT = R_TEMPERATURE * kB
    IF (MASTER_P(1) > 0.0) scheme=3
    IF (ONSITE_V    > 0.0) scheme=4
          
    CALL initialiseSprings(MASTER_CLUSTER)
    MASTER_CLUSTER%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(0)))

    CALL initialiseCluster(new_cluster)
    new_cluster = MASTER_CLUSTER

    refining=0
    CALL getMadelungPot(new_cluster)
    CALL updateSprings(new_cluster,scheme,refining)

    DO n=1,N_MAX_BH_STEPS
      new_cluster%id = INITIAL_PREFIX//TRIM(ADJUSTL(intToChar(n)))
      CALL relaxSprings(new_cluster)
      IF (refining.lt.2) THEN
        CALL resetCluster(new_cluster) !use original structural parameters
      ENDIF
      CALL updateSprings(new_cluster,scheme,refining)
      IF (refining.eq.3) THEN
        WRITE(6,*)'Tolerance for refinement of springs achieved'
        EXIT
      ENDIF
    ENDDO

    CALL reportSprings(new_cluster)
      
END SUBROUTINE runRefineSprings

!--------------------------------------------------------------------------------
    
SUBROUTINE initialiseSprings(inout_cluster)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
!   UPDATES MASTER_n( ),inout_cluster%atoms( )%k
    INTEGER :: n, nA, nB, nC
    REAL(KIND=DBL) :: k_mean

! compute number of each species and initial value of springs as a weighted average

    nA = 0
    nB = 0
    nC = 0
    DO n = 1, N_ATOMS
      IF (index(inout_cluster%atoms(n)%symbol,MASTER_spring_type(1)).ne.0) nA=nA+1
      IF (index(inout_cluster%atoms(n)%symbol,MASTER_spring_type(2)).ne.0) nB=nB+1
      IF (index(inout_cluster%atoms(n)%symbol,MASTER_spring_type(3)).ne.0) nC=nC+1
    ENDDO
    k_mean = (nA*MASTER_k(1) + nB*MASTER_k(2) + nC*MASTER_k(3))/(1.0*(nA+nB+nC))

    MASTER_n(1) = nA 
    MASTER_n(2) = nB 
    MASTER_n(3) = nC 

    DO n = 1, N_ATOMS
      IF (inout_cluster%atoms(n)%cstype == 'k') THEN
        inout_cluster%atoms(n)%k = k_mean
      ENDIF
    ENDDO

END SUBROUTINE initialiseSprings

!==========================================================================================!

SUBROUTINE updateSprings(inout_cluster,scheme,refining)
    TYPE(cluster), INTENT(INOUT) :: inout_cluster
    INTEGER, INTENT(IN) :: scheme
    INTEGER, INTENT(INOUT) :: refining
    REAL(KIND=DBL) :: diff_k_sum, k_sum, k_old, knVn, R_NN
    REAL(KIND=DBL) :: dV, V_sum, O_sum, rnn(30)
    INTEGER :: n, nn(0:30), m
    LOGICAL :: firstcall,debug=.true.
    R_NN = R_MAX_CLUSTER_BOUNDARY(1)

    firstcall=(refining.eq.0)
    IF (firstcall) THEN
      refining=1
      WRITE(6,*)'Update scheme for modifying springs          : ',scheme
      WRITE(6,*)'Damping (0=>100% old, 1=>100%new value)      : ',K_DAMP
      WRITE(6,*)'Applied twobody cutoff for nearest neighbours: ',R_NN
      WRITE(6,*)
      WRITE(6,*)'Springs -    Total change                  Average         ', &
                '              <Mad Pot>                  <Mad Pot O>'
    ENDIF
    diff_k_sum = 0.0
    IF (scheme.eq.1) THEN
      knVn = ((MASTER_k(1)*MASTER_n(1)/MASTER_V(1)) &
            + (MASTER_k(2)*MASTER_n(2)/MASTER_V(2)) &
            + (MASTER_k(3)*MASTER_n(3)/MASTER_V(3)) ) &
            /(1.0*(MASTER_n(1)+MASTER_n(2)+MASTER_n(3)))
      DO n = 1, N_ATOMS
        IF (inout_cluster%atoms(n)%cstype == 'k') THEN
          k_old = inout_cluster%atoms(n)%k
          inout_cluster%atoms(n)%k = knVn * inout_cluster%atoms(n)%pot
          diff_k_sum = diff_k_sum + abs(inout_cluster%atoms(n)%k - k_old)
        ENDIF
      ENDDO
    ELSEIF (scheme.eq.2) THEN
      ! assume that MASTER_V() = alpha * (Va - Vc) / ncoord * Y * Y = (Va - Vc) / ncoord * k
      DO n = 1, N_ATOMS
        IF (inout_cluster%atoms(n)%cstype == 'k') THEN
          k_old = inout_cluster%atoms(n)%k
          if (debug) then
            print*,'Atom ',n,' = ',inout_cluster%atoms(n)%symbol
            print*,' at  ',inout_cluster%atoms(n)%xc,inout_cluster%atoms(n)%yc,inout_cluster%atoms(n)%zc
            print*,' or  ',inout_cluster%atoms(n)%xf,inout_cluster%atoms(n)%yf,inout_cluster%atoms(n)%zf
          endif
          CALL FIND_NN(n,R_NN,nn,rnn,inout_cluster)
          k_sum = 0.0
          DO m = 1, nn(0)
            if (debug) then 
               print*,'Connected to ',inout_cluster%atoms(nn(m))%symbol
               print*,' at ',inout_cluster%atoms(nn(m))%xc,inout_cluster%atoms(nn(m))%yc, &
                                                           inout_cluster%atoms(nn(m))%zc
               print*,' or ',inout_cluster%atoms(nn(m))%xf,inout_cluster%atoms(nn(m))%yf, &
                                                           inout_cluster%atoms(nn(m))%zf
               print*,' which is a distance ',rnn(m), 'away'
            endif
            dV = inout_cluster%atoms(n)%pot - inout_cluster%atoms(nn(m))%pot
            IF (inout_cluster%atoms(nn(m))%symbol == MASTER_spring_type(1)) THEN
              k_sum = k_sum + MASTER_V(1) / dV
              if (debug) print*,'  adding ... ',MASTER_V(1),' / ',dV
            ELSEIF (inout_cluster%atoms(nn(m))%symbol == MASTER_spring_type(2)) THEN
              k_sum = k_sum + MASTER_V(2) / dV
              if (debug) print*,'  adding ... ',MASTER_V(2),' / ',dV
            ELSEIF (inout_cluster%atoms(nn(m))%symbol == MASTER_spring_type(3)) THEN
              k_sum = k_sum + MASTER_V(3) / dV
              if (debug) print*,'  adding ... ',MASTER_V(3),' / ',dV
            ENDIF
          ENDDO
          if (debug) print*,'  sum of which = ',k_sum
          IF (k_sum > 0.0000001) THEN
            inout_cluster%atoms(n)%k = (1.0_DP - K_DAMP)*inout_cluster%atoms(n)%k + K_DAMP / k_sum
            if (debug) print*,'k(',n,') = ',1.0d0/k_sum
          ELSE
            WRITE(6,*)'Problem found in scheme 2'
            inout_cluster%atoms(n)%k =  10000000.0
          ENDIF
          diff_k_sum = diff_k_sum + abs(inout_cluster%atoms(n)%k - k_old)
        ENDIF
      ENDDO
    ELSEIF (scheme.eq.3) THEN
      ! assume that MASTER_V() = alpha * (Va - Vc) / ncoord * Y * Y = (Va - Vc) / ncoord * k
      DO n = 1, N_ATOMS
        IF (inout_cluster%atoms(n)%cstype == 'k') THEN
          k_old = inout_cluster%atoms(n)%k
          if (debug) then
            print*,'Atom ',n,' = ',inout_cluster%atoms(n)%symbol
            print*,' at  ',inout_cluster%atoms(n)%xc,inout_cluster%atoms(n)%yc,inout_cluster%atoms(n)%zc
            print*,' or  ',inout_cluster%atoms(n)%xf,inout_cluster%atoms(n)%yf,inout_cluster%atoms(n)%zf
          endif
          CALL FIND_NN(n,R_NN,nn,rnn,inout_cluster)
          k_sum = 0.0
          DO m = 1, nn(0)
            if (debug) then 
               print*,'Connected to ',inout_cluster%atoms(nn(m))%symbol
               print*,' at ',inout_cluster%atoms(nn(m))%xc,inout_cluster%atoms(nn(m))%yc, &
                                                           inout_cluster%atoms(nn(m))%zc
               print*,' or ',inout_cluster%atoms(nn(m))%xf,inout_cluster%atoms(nn(m))%yf, &
                                                           inout_cluster%atoms(nn(m))%zf
               print*,' which is a distance ',rnn(m), 'away'
            endif
            dV = inout_cluster%atoms(n)%pot - inout_cluster%atoms(nn(m))%pot
            IF (inout_cluster%atoms(nn(m))%symbol == MASTER_spring_type(1)) THEN
              k_sum = k_sum + MASTER_V(1) * EXP(-rnn(m)*MASTER_P(1)) / dV
              if (debug) print*,'  adding ... ',MASTER_V(1),'*',EXP(-rnn(m)*MASTER_P(1)),' / ',dV
            ELSEIF (inout_cluster%atoms(nn(m))%symbol == MASTER_spring_type(2)) THEN
              k_sum = k_sum + MASTER_V(2) * EXP(-rnn(m)*MASTER_P(2)) / dV
              if (debug) print*,'  adding ... ',MASTER_V(2),'*',EXP(-rnn(m)*MASTER_P(2)),' / ',dV
            ELSEIF (inout_cluster%atoms(nn(m))%symbol == MASTER_spring_type(3)) THEN
              k_sum = k_sum + MASTER_V(3) * EXP(-rnn(m)*MASTER_P(3)) / dV
              if (debug) print*,'  adding ... ',MASTER_V(3),'*',EXP(-rnn(m)*MASTER_P(3)),' / ',dV
            ENDIF
          ENDDO
          if (debug) print*,'  sum of which = ',k_sum
          IF (k_sum > 0.0000001) THEN
            inout_cluster%atoms(n)%k = (1.0_DP - K_DAMP)*inout_cluster%atoms(n)%k + K_DAMP / k_sum
            if (debug) print*,'k(',n,') = ',1.0d0/k_sum
          ELSE
            WRITE(6,*)'Problem found in scheme 3'
            inout_cluster%atoms(n)%k =  10000000.0
          ENDIF
          diff_k_sum = diff_k_sum + abs(inout_cluster%atoms(n)%k - k_old)
        ENDIF
      ENDDO
    ELSEIF (scheme.eq.4) THEN
      ! assume that MASTER_V() = alpha * (Va - Vc) / ncoord * Y * Y = (Va - Vc) / ncoord * k
      DO n = 1, N_ATOMS
        IF (inout_cluster%atoms(n)%cstype == 'k') THEN
          k_old = inout_cluster%atoms(n)%k
          CALL FIND_NN(n,R_NN,nn,rnn,inout_cluster)
          k_sum = ONSITE_V / (inout_cluster%atoms(n)%pot + ONSITE_N)
          if (debug) then
            print*,'Atom ',n,' = ',inout_cluster%atoms(n)%symbol
            print*,' at  ',inout_cluster%atoms(n)%xc,inout_cluster%atoms(n)%yc,inout_cluster%atoms(n)%zc
            print*,' or  ',inout_cluster%atoms(n)%xf,inout_cluster%atoms(n)%yf,inout_cluster%atoms(n)%zf
            print*,' has on-site term of ',k_sum
          endif
          DO m = 1, nn(0)
            if (debug) then 
               print*,'Connected to ',inout_cluster%atoms(nn(m))%symbol
               print*,' at ',inout_cluster%atoms(nn(m))%xc,inout_cluster%atoms(nn(m))%yc, &
                                                           inout_cluster%atoms(nn(m))%zc
               print*,' or ',inout_cluster%atoms(nn(m))%xf,inout_cluster%atoms(nn(m))%yf, &
                                                           inout_cluster%atoms(nn(m))%zf
               print*,' which is a distance ',rnn(m), 'away'
            endif
            dV = inout_cluster%atoms(n)%pot - inout_cluster%atoms(nn(m))%pot
            IF (inout_cluster%atoms(nn(m))%symbol == MASTER_spring_type(1)) THEN
              k_sum = k_sum + MASTER_V(1) * EXP(-rnn(m)*MASTER_P(1)) / dV
              if (debug) print*,'  adding ... ',MASTER_V(1),'*',EXP(-rnn(m)*MASTER_P(1)),' / ',dV
            ELSEIF (inout_cluster%atoms(nn(m))%symbol == MASTER_spring_type(2)) THEN
              k_sum = k_sum + MASTER_V(2) * EXP(-rnn(m)*MASTER_P(2)) / dV
              if (debug) print*,'  adding ... ',MASTER_V(2),'*',EXP(-rnn(m)*MASTER_P(2)),' / ',dV
            ELSEIF (inout_cluster%atoms(nn(m))%symbol == MASTER_spring_type(3)) THEN
              k_sum = k_sum + MASTER_V(3) * EXP(-rnn(m)*MASTER_P(3)) / dV
              if (debug) print*,'  adding ... ',MASTER_V(3),'*',EXP(-rnn(m)*MASTER_P(3)),' / ',dV
            ENDIF
          ENDDO
          if (debug) print*,'  sum of which = ',k_sum
          IF (k_sum > 0.0000001) THEN
            inout_cluster%atoms(n)%k = (1.0_DP - K_DAMP)*inout_cluster%atoms(n)%k + K_DAMP / k_sum
            if (debug) print*,'k(',n,') = ',1.0d0/k_sum
          ELSE
            WRITE(6,*)'Problem found in scheme 3'
            inout_cluster%atoms(n)%k =  10000000.0
          ENDIF
          diff_k_sum = diff_k_sum + abs(inout_cluster%atoms(n)%k - k_old)
        ENDIF
      ENDDO
    ENDIF
    m=0
    k_sum=0.0
    V_sum=0.0
    O_sum=0.0
    DO n = 1, N_ATOMS
      V_sum=V_sum+inout_cluster%atoms(n)%pot
      IF (inout_cluster%atoms(n)%cstype == 'k') THEN
        m=m+1
        k_sum=k_sum+inout_cluster%atoms(n)%k
        O_sum=O_sum+inout_cluster%atoms(n)%pot
        ! if (debug) print*,'m ',m,' k_sum ',k_sum
      ENDIF
    ENDDO
    k_sum=k_sum/m
    O_sum=O_sum/m
    V_sum=V_sum/N_ATOMS
    WRITE(6,*)'         ',diff_k_sum,' ',k_sum,' ',V_sum,' ',O_sum

    !Has the refinement reached a point where the cluster does not need to be reset
    IF (refining.eq.1.and.diff_k_sum.lt.K_TOL(1)) THEN
      WRITE(6,*)'Will no longer reset structural parameters!'
      refining=2
    ELSEIf (refining.eq.2.and.diff_k_sum.lt.K_TOL(2)) THEN
      !Refinement has not made any difference, so flag refining is over
      refining=3
    ENDIF

END SUBROUTINE updateSprings

!==========================================================================================!

SUBROUTINE reportSprings(in_cluster)
    TYPE(cluster), INTENT(IN) :: in_cluster
    REAL(KIND=DBL) :: r, R_NN, rnn(500)
    INTEGER :: i, j, n, nn(0:500)

    ! Report values of k as a function of distance away from atom 1

    n = 1
    R_NN = 3.0*R_MAX_CLUSTER_BOUNDARY(1)
    CALL FIND_NN(n,R_NN,nn,rnn,in_cluster)

    DO i=1,nn(0)-1
      n=nn(i)
      r=rnn(i)
      DO j=i+1, nn(0)
        IF(rnn(j) < r)THEN
          rnn(i)=rnn(j)
          rnn(j)=r
          r=rnn(i)
          nn(i)=nn(j)
          nn(j)=n
          n=nn(i)
        ENDIF
      ENDDO
    ENDDO

   WRITE(6,*)
   WRITE(6,*)'          n     Species        Springs       ', &
             '              Mad Pot                    Dist from atom 1'
    DO n = 1, nn(0)
      IF (in_cluster%atoms(nn(n))%cstype == 'k') THEN
        WRITE(6,*)n,'    ',(MASTER_ATOMS(nn(n),1)(1:9)),' ',in_cluster%atoms(nn(n))%k, &
                          ' ',in_cluster%atoms(nn(n))%pot,' ',rnn(n)
      ENDIF
    ENDDO

END SUBROUTINE reportSprings

!==========================================================================================!

END MODULE RefineSprings
