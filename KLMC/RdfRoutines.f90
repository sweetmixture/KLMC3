MODULE RdfRoutines

    USE Config
    USE Library,  ONLY : num2str
    USE File,     ONLY : printRDF, retrieveData, peek
    USE UnitCell, ONLY : FIND_NN, Volume

    IMPLICIT NONE

    REAL(KIND=DBL),PARAMETER    :: fourPi=12.566370614359172_dp

!==========================================================================================!
CONTAINS

! routines for computing the Radial Distribution Function
!
! subroutines: computeRDF fillRDF computeOcc
!              initialiseRDF initialiseTrdf
!              resetRDF restartTrdf
!              smearFi updateTrdf
!
!  functions:  GaussianFn

!==========================================================================================!

SUBROUTINE computeRDF(inout_rdf,in_filename)
    CHARACTER(LEN=*), INTENT(IN) :: in_filename
    TYPE(rdf), INTENT(INOUT)     :: inout_rdf
    TYPE(cluster)  :: cluster_rdf
    INTEGER :: IN_N_ATOMS

!   Need to change N_ATOMS to include all atoms
    IN_N_ATOMS = N_ATOMS
    N_ATOMS = N_ARC_ATOMS
    CALL setDefaults(cluster_rdf) ! with no vacancies defined

    CALL retrieveData(cluster_rdf,in_filename)
    !write(*,*)'BACK from retrieveData'
    !CALL peek(cluster_rdf)

    CALL fillRDF(cluster_rdf,inout_rdf)
    !write(*,*)'BACK from fillRDF'

    IF (DEBUG_LEVEL == 3) CALL printRDF(inout_rdf,'RDF')

    ! Return to original number of atoms (sites)
    N_ATOMS = IN_N_ATOMS

END SUBROUTINE computeRDF

SUBROUTINE restartTrdf(inout_rdf,inout_Trdf)
    TYPE(rdf), INTENT(INOUT)  :: inout_rdf
    TYPE(Trdf),  INTENT(INOUT)  :: inout_Trdf
    CHARACTER(LEN=60) :: filename
    CHARACTER(LEN=10) :: idnum
    INTEGER :: i

    inout_rdf%energy = R_BEST_ENERGIES(1)
    inout_rdf%ndegen = N_BEST_ENERGIES(1)
!   inout_rdf%ndegen = INT(R_BEST_GNORM(1))
    CALL initialiseTrdf(inout_rdf,inout_Trdf)
    DO i=1,INT_MAX_BEST
      filename=''
      CALL resetRDF(inout_rdf)
      IF (index(ID_BEST(i),'x').eq.1) EXIT
      IF (index(ID_BEST(i),'X').ne.1) THEN
        CALL num2str(i,INT_BEST,idnum)
        filename=TRIM(BEST_FOLDER)//TRIM(ID_BEST(i))//'-'//TRIM(idnum)//'.arc'
        CALL computeRDF(inout_rdf,TRIM(filename))
        inout_rdf%ndegen = N_BEST_ENERGIES(i)
!       inout_rdf%ndegen = INT(R_BEST_GNORM(i))
        CALL updateTrdf(inout_rdf,inout_Trdf)
      ENDIF
    ENDDO

END SUBROUTINE restartTrdf

SUBROUTINE fillRDF(in_cluster,inout_rdf)
    TYPE(cluster), INTENT(IN)  :: in_cluster
    TYPE(rdf),  INTENT(INOUT)  :: inout_rdf
    REAL(KIND=DBL) :: dr_acc, R_NN, G, fourPir2p
    REAL(KIND=DBL) :: z0, zi, Gz, zsum, inv_zig
    CHARACTER(LEN=2) :: XX, YY
    INTEGER :: i, j, k, n, m, ide
    REAL(KIND=DBL), DIMENSION(:), ALLOCATABLE :: rnn
    INTEGER, DIMENSION(:), ALLOCATABLE :: nn
    ALLOCATE(nn(0:10000))
    ALLOCATE(rnn(10000))

    z0=R_RDF_Z        ! user defined plane of interest
    dr_acc=R_RDF_ACC  ! accuracy between unique distances
    R_NN=R_RDF_CUTOFF ! cutoff for RDF
    inv_zig=1.0_DP/R_RDF_ZIGMA

    ide = in_cluster%edefn
    IF (ide==0) THEN
      inout_rdf%energy = R_ENERGY_ZERO
    ELSE
      inout_rdf%energy = in_cluster%energy(ide)
    ENDIF
    inout_rdf%ndegen = 1
    IF (L_COMPUTE_RDF_Z) THEN
      zsum=ZERO
    ELSE
      zsum=1.0_DP
    ENDIF
    DO i=1,N_ATOMS
      IF (L_COMPUTE_RDF_Z) THEN
        zi=in_cluster%atoms(i)%zc
        Gz=GaussianFn(z0,zi,inv_zig)
        zsum=zsum+Gz
      ELSE
        Gz=1.0_DP
      ENDIF

      CALL FIND_NN(i,R_NN,nn,rnn,in_cluster)

      DO k=1,N_PAIRS
        XX(1:2)=inout_rdf%pairs(k)%name(1:2)
        YY(1:2)=inout_rdf%pairs(k)%name(4:5)
        IF (in_cluster%atoms(i)%symbol == XX) THEN
          DO j=1,nn(0)
            IF (in_cluster%atoms(nn(j))%symbol == YY) THEN
              IF (inout_rdf%pairs(k)%ndist == 0) THEN
                inout_rdf%pairs(k)%ndist=1
                inout_rdf%pairs(k)%rdist(1)=rnn(j)
                inout_rdf%pairs(k)%gr(1)=Gz
              ELSE
                nloop: DO n=1,inout_rdf%pairs(k)%ndist
                IF (rnn(j) < inout_rdf%pairs(k)%rdist(n)-dr_acc) THEN
                  IF (inout_rdf%pairs(k)%ndist == MAX_RDF_UDIST ) THEN
                    WRITE(6,*)'Too many unique distances for RDF ',MAX_RDF_UDIST
                    STOP
                  ENDIF
                  DO m=inout_rdf%pairs(k)%ndist,n,-1
                    inout_rdf%pairs(k)%rdist(m+1)=inout_rdf%pairs(k)%rdist(m)
                    inout_rdf%pairs(k)%gr(m+1)=inout_rdf%pairs(k)%gr(m)
                  ENDDO
                  inout_rdf%pairs(k)%ndist=inout_rdf%pairs(k)%ndist+1
                  inout_rdf%pairs(k)%rdist(n)=rnn(j)
                  inout_rdf%pairs(k)%gr(n)=Gz
                  EXIT nloop
                ELSEIF (rnn(j) < inout_rdf%pairs(k)%rdist(n)+dr_acc) THEN
                  inout_rdf%pairs(k)%gr(n)=inout_rdf%pairs(k)%gr(n)+Gz
                  EXIT nloop
                ENDIF
                ENDDO nloop
              ENDIF
            ENDIF 
          ENDDO
        ENDIF
      ENDDO
    ENDDO

    ! finally loop over to normalise st g(r)->1 as r->oo
    G=fourPi*N_ATOMS/(zsum*Volume())
    DO k=1,N_PAIRS ! loop over to normalise g(r)->1 as r->oo
      DO j=1,inout_rdf%pairs(k)%ndist
         fourPir2p=G*inout_rdf%pairs(k)%rdist(j)*inout_rdf%pairs(k)%rdist(j)
         inout_rdf%pairs(k)%gr(j)=inout_rdf%pairs(k)%gr(j)/fourPir2p
      ENDDO
    ENDDO

    DEALLOCATE(rnn)
    DEALLOCATE(nn)

END SUBROUTINE fillRDF

SUBROUTINE computeOcc(in_cluster,in_atoms,out_r_occ)
    TYPE(cluster), INTENT(IN)  :: in_cluster
    TYPE(atom),  INTENT(IN)  :: in_atoms(*)
    REAL(KIND=DBL), INTENT(OUT) :: out_r_occ(*)
    INTEGER :: i, k, nr
    REAL(KIND=DBL), DIMENSION(:), ALLOCATABLE :: rnn
    INTEGER, DIMENSION(:), ALLOCATABLE :: nn
    REAL(KIND=DBL) :: norm = 0.564189583548_DP ! = 1/rootPi
    REAL(KIND=DBL) :: A, inv_sig, r0(3), R_NN

    ALLOCATE(nn(0:10000))
    ALLOCATE(rnn(10000))

    inv_sig=1.0_DP/R_RDF_SIGMA
    A = norm * inv_sig

    R_NN=R_RDF_CUTOFF ! cutoff for RDF

    DO nr=0,N_REGIONS
      out_r_occ(nr)=ZERO
      DO i=1,N_ATOMS
        IF (nr==in_atoms(i)%site) THEN
          r0(1)=in_atoms(i)%xf
          r0(2)=in_atoms(i)%yf
          r0(3)=in_atoms(i)%zf
          CALL FIND_NN(0,R_NN,nn,rnn,in_cluster,r0)
          IF (nn(0).gt.0) THEN
            DO k=1,nn(0)
              out_r_occ(nr)=out_r_occ(nr)+nn(k)*GaussianFn(rnn(k),ZERO,inv_sig)
            ENDDO
          ENDIF
        ENDIF
      ENDDO
    ENDDO

    DO nr=0,N_REGIONS
      out_r_occ(nr)=A*out_r_occ(nr)
    ENDDO

    DEALLOCATE(rnn)
    DEALLOCATE(nn)

END SUBROUTINE computeOcc

SUBROUTINE initialiseRDF(out_rdf)
    TYPE(rdf), INTENT(OUT)  :: out_rdf
    INTEGER :: i, j, k, n
    n=0
    DO i=1,N_SPECIES
      DO j=1,i
        n=n+1
        IF (n > MAX_RDF_PAIRS) THEN
          WRITE(6,*)'Number of unique species found: ',N_SPECIES
          DO k=1,N_SPECIES
            WRITE(6,*)MASTER_SPECIES(k)
          ENDDO
          WRITE(6,*)'Recompile with larger value for MAX_RDF_PAIRS',MAX_RDF_PAIRS
          STOP
        ENDIF
        out_rdf%pairs(n)%name(1:2)=MASTER_SPECIES(j)
        out_rdf%pairs(n)%name(4:5)=MASTER_SPECIES(i)
        out_rdf%pairs(n)%ndist=0
      ENDDO
    ENDDO
    N_PAIRS = n
    out_rdf%npairs = N_PAIRS
    out_rdf%ndegen = 0
END SUBROUTINE initialiseRDF

SUBROUTINE resetRDF(inout_rdf)
    TYPE(rdf), INTENT(INOUT)  :: inout_rdf
    INTEGER :: n
    inout_rdf%ndegen = 0
    DO n=1,inout_rdf%npairs
      inout_rdf%pairs(n)%ndist=0
    ENDDO
END SUBROUTINE resetRDF

SUBROUTINE updateTrdf(in_rdf,inout_Trdf)
    TYPE(rdf), INTENT(IN)  :: in_rdf
    TYPE(Trdf),  INTENT(INOUT)  :: inout_Trdf
    REAL(KIND=DBL), DIMENSION(:), ALLOCATABLE :: smeared
    REAL(KIND=DBL) :: dr_acc, boltz, rn, xmin, xmax, xpos, xstep, xhstep
    INTEGER :: i, k, n, m, nd

    dr_acc=0.00001_DP ! accuracy between discrete unique distances
    xmin=R_RDF_ZERO   ! starting point for smeared data
    xmax=R_RDF_CUTOFF ! finishing point for smeared data
    xstep=R_RDF_STEP  ! distance between points in sampling g(r)
    xhstep=0.5_DP*xstep

    boltz = exp((inout_Trdf%energy-in_rdf%energy)*inout_Trdf%invkBT)

    inout_Trdf%nstruct = inout_Trdf%nstruct + in_rdf%ndegen
    boltz = in_rdf%ndegen * boltz
    inout_Trdf%norm = inout_Trdf%norm + boltz

    IF (DEBUG_LEVEL > 20) THEN
      WRITE(6,*)inout_Trdf%energy,in_rdf%energy,inout_Trdf%invkBT,'Norm=',inout_Trdf%norm
    ELSEIF (DEBUG_LEVEL > 10) THEN
      WRITE(6,*)in_rdf%ndegen,boltz,'Norm=',inout_Trdf%norm
    ENDIF

    IF (R_RDF_SIGMA < 0.001) THEN ! no point smearing delta functions

     IF (L_HISTOGRAM) THEN

      DO k=1,inout_Trdf%npairs
       IF (inout_Trdf%pairs(k)%ndist == 0) THEN
         xpos=xmin
         DO n=1,N_RDF_STEPS ! default = MAX_RDF_DIST
           inout_Trdf%pairs(k)%rdist(n)=xpos
           inout_Trdf%pairs(k)%gr(n)=ZERO
           xpos=xpos+xstep
         ENDDO
         inout_Trdf%pairs(k)%ndist = N_RDF_STEPS ! default = MAX_RDF_DIST
       ENDIF
      ENDDO

! Could renormalise rdf%gr before adding to Trdf%gr st r^2 relates to centre of bin
! rather than interatomic distance by scaling rdf%gr by (rdf%dist/xpos)^2 = 1.0 for
! R_RDF_STEP and R_RDF_ACC -> 0.0
      DO k=1,inout_Trdf%npairs
       IF (in_rdf%pairs(k)%ndist == 0) THEN
!        DO n=1,N_RDF_STEPS ! default = MAX_RDF_DIST
!          inout_Trdf%pairs(k)%gr(n) = inout_Trdf%pairs(k)%gr(n) + 0.0*boltz
!        ENDDO
       ELSE
         DO n=1,N_RDF_STEPS ! default = MAX_RDF_DIST
           xpos = inout_Trdf%pairs(k)%rdist(n)
           xmin = xpos - xhstep
           xmax = xpos + xhstep
           DO m=1,in_rdf%pairs(k)%ndist
             IF (xmin.lt.in_rdf%pairs(k)%rdist(m).AND.in_rdf%pairs(k)%rdist(m).le.xmax) THEN
               inout_Trdf%pairs(k)%gr(n) = inout_Trdf%pairs(k)%gr(n)   &
                                           + in_rdf%pairs(k)%gr(m)*boltz
             ENDIF
           ENDDO
         ENDDO
       ENDIF
      ENDDO

     ELSE ! ALL UNIQUE DISTANCES TO BE KEPT - MEMORY EXPENSIVE

      DO k=1,inout_Trdf%npairs
       nd = inout_Trdf%pairs(k)%ndist
       IF (nd == 0) THEN
         inout_Trdf%pairs(k)%ndist = in_rdf%pairs(k)%ndist
         DO m=1,in_rdf%pairs(k)%ndist
           inout_Trdf%pairs(k)%rdist(m)=in_rdf%pairs(k)%rdist(m)
           inout_Trdf%pairs(k)%gr(m)=in_rdf%pairs(k)%gr(m)*boltz
         ENDDO
       ELSE
         DO m=1,in_rdf%pairs(k)%ndist
           rn = in_rdf%pairs(k)%rdist(m)
           DO n=1,inout_Trdf%pairs(k)%ndist
             IF (rn < inout_Trdf%pairs(k)%rdist(n)-dr_acc) THEN
               IF (inout_Trdf%pairs(k)%ndist == MAX_RDF_DIST ) THEN
                 WRITE(6,*)'Too many unique distances for TRDF ',MAX_RDF_DIST
                 STOP
               ENDIF
               DO i=inout_Trdf%pairs(k)%ndist,n,-1
                 inout_Trdf%pairs(k)%rdist(i+1)=inout_Trdf%pairs(k)%rdist(i)
                 inout_Trdf%pairs(k)%gr(i+1)=inout_Trdf%pairs(k)%gr(i)
               ENDDO
               inout_Trdf%pairs(k)%rdist(n)=rn != in_rdf%pairs(k)%rdist(m)
               inout_Trdf%pairs(k)%gr(n)=in_rdf%pairs(k)%gr(m)*boltz
               inout_Trdf%pairs(k)%ndist=inout_Trdf%pairs(k)%ndist+1
               EXIT
             ELSEIF (rn < inout_Trdf%pairs(k)%rdist(n)+dr_acc) THEN
               inout_Trdf%pairs(k)%gr(n) = inout_Trdf%pairs(k)%gr(n) &
                                           + in_rdf%pairs(k)%gr(m)*boltz
               EXIT
             ENDIF
           ENDDO
         ENDDO
       ENDIF
      ENDDO

     ENDIF ! HISTOGRAPH

    ELSE ! apply thermal smearing (discrete -> continuous)

! Could renormalise rdf%gr before adding to Trdf%gr st r^2 relates to centre of bin
! rather than interatomic distance by scaling rdf%gr by (rdf%dist/xpos)^2 = 1.0 for
! R_RDF_STEP and R_RDF_ACC -> 0.0

     ALLOCATE(smeared(MAX_RDF_DIST))
     DO k=1,inout_Trdf%npairs

       IF (inout_Trdf%pairs(k)%ndist == 0) THEN
         xpos=xmin
         DO n=1,MAX_RDF_DIST
           inout_Trdf%pairs(k)%rdist(n)=xpos
           inout_Trdf%pairs(k)%gr(n)=ZERO
           xpos=xpos+xstep
         ENDDO
         inout_Trdf%pairs(k)%ndist = MAX_RDF_DIST
       ENDIF

       IF (in_rdf%pairs(k)%ndist == 0) THEN

!        DO n=1,N_RDF_STEPS ! default = MAX_RDF_DIST
!          inout_Trdf%pairs(k)%gr(n) = inout_Trdf%pairs(k)%gr(n) + 0.0*boltz
!        ENDDO

       ELSE

         CALL smear_F(in_rdf%pairs(k)%gr,in_rdf%pairs(k)%ndist,in_rdf%pairs(k)%rdist,&
                                            smeared,xmin,xmax,MAX_RDF_DIST,R_RDF_SIGMA)

         DO n=1,N_RDF_STEPS ! default = MAX_RDF_DIST
           inout_Trdf%pairs(k)%gr(n) = inout_Trdf%pairs(k)%gr(n) &
                                       + smeared(n)*boltz
         ENDDO

       ENDIF

     ENDDO
     DEALLOCATE(smeared)

    ENDIF

END SUBROUTINE updateTrdf

SUBROUTINE initialiseTrdf(in_rdf,out_Trdf)
    TYPE(rdf),  INTENT(IN)   :: in_rdf
    TYPE(Trdf), INTENT(OUT)  :: out_Trdf
    REAL(KIND=DBL) :: invkB = 1.60217733E-19 / 1.380658E-23
    INTEGER :: i, n
!   kB = 0.000086174eV/K  => 1/kB = 11604.4K/eV
!   T = 500K => 1/kBT = 23.21 /eV
!   dE = 1eV => boltz = 8*E-11

    n = in_rdf%npairs
    out_Trdf%npairs=n
    out_Trdf%nstruct=0
    out_Trdf%energy=in_rdf%energy
    out_Trdf%norm=ZERO
    out_Trdf%invkBT=(invkB/R_TEMPERATURE)

    DO i=1,n
      out_Trdf%pairs(i)%name=in_rdf%pairs(i)%name
      out_Trdf%pairs(i)%ndist=0
    ENDDO

END SUBROUTINE initialiseTrdf

SUBROUTINE smear_F(in_F,in_n,in_ri,out_F,in_min,in_max,in_steps,in_sig)
    INTEGER,  INTENT(IN)        :: in_n, in_steps
    REAL(KIND=DBL), INTENT(IN)  :: in_F(*), in_ri(*), in_min, in_max, in_sig
    REAL(KIND=DBL), INTENT(OUT) :: out_F(*)
    REAL(KIND=DBL) :: norm = 0.564189583548_DP ! = 1/rootPi
    REAL(KIND=DBL) :: step, r, A, inv_sig
    INTEGER :: i, n

    inv_sig =1.0_DP/in_sig
    A = norm * inv_sig

    step = (in_max - in_min) / (1.0_DP*in_steps - 1.0_DP)

    r = in_min
    DO i=1,in_steps
      out_F(i)=0.0_DP
      DO n=1,in_n
        out_F(i) = out_F(i) + in_F(n)*GaussianFn(r,in_ri(n),inv_sig)
      ENDDO
      out_F(i) = A * out_F(i)
      r = r + step
    ENDDO
END SUBROUTINE smear_F

REAL(KIND=DBL) FUNCTION GaussianFn(r,ri,inv_sig)
    REAL(KIND=DBL) :: r, ri, inv_sig, x
!   G = A*exp{-{(r-r0)/sig}^2}
!   to normalise st G integrated from -inf to +inf = 1
!   set A = 1 / (sig * sqrt(Pi)), A=A*A for 2D, A=A^3 for 3D integration
!   expected value = r0
!   variance (sigma^2) = 0.5*sig^2 as we do not have 2*sig^2 in definition of G
    x = (r - ri) * inv_sig
    GaussianFn = exp(-x*x)
END FUNCTION GaussianFn

END MODULE RdfRoutines
