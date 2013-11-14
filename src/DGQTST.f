************************************************************************
*
*     Subroutine DGQTST                               Called by: DHUMIT
*
*     compute Goldfeld-Quandt-Trotter step by More-Hebden technique
*
C     (NL2SOL VERSION 2.2), MODIFIED A LA MORE AND SORENSEN
C
C        GIVEN THE (COMPACTLY STORED) LOWER TRIANGLE OF A SCALED
C     HESSIAN (APPROXIMATION) AND A NONZERO SCALED GRADIENT VECTOR,
C     THIS SUBROUTINE COMPUTES A GOLDFELD-QUANDT-TROTTER STEP OF
C     APPROXIMATE LENGTH V(8) BY THE MORE-HEBDEN TECHNIQUE.  IN
C     OTHER WORDS, STEP IS COMPUTED TO (APPROXIMATELY) MINIMIZE
C     PSI(STEP) = (G**T)*STEP + 0.5*(STEP**T)*H*STEP  SUCH THAT THE
C     2-NORM OF D*STEP IS AT MOST (APPROXIMATELY) V(8), WHERE
C     G  IS THE GRADIENT,  H  IS THE HESSIAN, AND  D  IS A DIAGONAL
C     SCALE MATRIX WHOSE DIAGONAL IS STORED IN THE PARAMETER D.
C     (DGQTST ASSUMES  DIG = D**-1 * G  AND  DIHDI = D**-1 * H * D**-1.)
C
C     D (IN)  = THE SCALE VECTOR, I.E. THE DIAGONAL OF THE SCALE
C              MATRIX  D  MENTIONED ABOVE UNDER PURPOSE.
C   DIG (IN)  = THE SCALED GRADIENT VECTOR, D**-1 * G.  IF G = 0, THEN
C              STEP = 0  AND  V(5) = 0  ARE RETURNED.
C DIHDI (IN)  = LOWER TRIANGLE OF THE SCALED HESSIAN (APPROXIMATION),
C              I.E., D**-1 * H * D**-1, STORED COMPACTLY BY ROWS., I.E.,
C              IN THE ORDER (1,1), (2,1), (2,2), (3,1), (3,2), ETC.
C    KA (I/O) = THE NUMBER OF HEBDEN ITERATIONS (SO FAR) TAKEN TO DETER-
C              MINE STEP.  KA .LT. 0 ON INPUT MEANS THIS IS THE FIRST
C              ATTEMPT TO DETERMINE STEP (FOR THE PRESENT DIG AND DIHDI)
C              -- KA IS INITIALIZED TO 0 IN THIS CASE.  OUTPUT WITH
C              KA = 0  (OR V(5) = 0)  MEANS  STEP = -(H**-1)*G.
C     L (I/O) = WORKSPACE OF LENGTH P*(P+1)/2 FOR CHOLESKY FACTORS.
C     P (IN)  = NUMBER OF PARAMETERS -- THE HESSIAN IS A  P X P  MATRIX.
C  STEP (I/O) = THE STEP COMPUTED.
C     V (I/O) CONTAINS VARIOUS CONSTANTS AND VARIABLES DESCRIBED BELOW.
C     W (I/O) = WORKSPACE OF LENGTH 4*P + 6.
C
C  ***  ENTRIES IN V  ***
C
C V(1) (I/O) = 2-NORM OF (D**-1)*G.
C V(2) (OUTPUT) = 2-NORM OF D*STEP.
C V(3)   (I/O) = 2-NORM OF D*(H**-1)*G (FOR POS. DEF. H ONLY), OR
C             OVERESTIMATE OF SMALLEST EIGENVALUE OF (D**-1)*H*(D**-1).
C V(19) (IN)  = MAX. REL. ERROR ALLOWED FOR PSI(STEP).  FOR THE
C             STEP RETURNED, PSI(STEP) WILL EXCEED ITS OPTIMAL VALUE
C             BY LESS THAN -V(19)*PSI(STEP).  SUGGESTED VALUE = 0.1.
C V(4) (OUT) = INNER PRODUCT BETWEEN G AND STEP.
C V(6) (OUT) = PSI(-(H**-1)*G) = PSI(NEWTON STEP)  (FOR POS. DEF.
C             H ONLY -- V(6) IS SET TO ZERO OTHERWISE).
C V(20) (IN)  = TOL. (TOGETHER WITH V(21)) FOR ACCEPTING STEP
C             (MORE*S SIGMA).  THE ERROR V(2) - V(8) MUST LIE
C             BETWEEN V(20)*V(8) AND V(21)*V(8).
C V(21) (IN)  (SEE V(20).)
C             SUGGESTED VALUES -- V(20) = -0.25, V(21) = 0.5.
C V(7) (OUT) = PSI(STEP) = PREDICTED OBJ. FUNC. REDUCTION FOR STEP.
C V(8) (IN)  = RADIUS OF CURRENT (SCALED) TRUST REGION.
C V(9)   (I/O) = VALUE OF V(8) FROM PREVIOUS CALL.
C V(5) (I/O) IS NORMALLY THE MARQUARDT PARAMETER, I.E. THE ALPHA
C             DESCRIBED BELOW UNDER ALGORITHM NOTES.  IF H + ALPHA*D**2
C             (SEE ALGORITHM NOTES) IS (NEARLY) SINGULAR, HOWEVER,
C             THEN V(5) = -ALPHA.
C
C     IF IT IS DESIRED TO RECOMPUTE STEP USING A DIFFERENT VALUE OF
C     V(8), THEN THIS ROUTINE MAY BE RESTARTED BY CALLING IT
C     WITH ALL PARAMETERS UNCHANGED EXCEPT V(8).  (THIS EXPLAINS
C     WHY STEP AND W ARE LISTED AS I/O).  ON AN INITIAL CALL (ONE WITH
C     KA .LT. 0), STEP AND W NEED NOT BE INITIALIZED AND ONLY COMPO-
C     NENTS V(19), V(5), V(20), V(21), V(8), AND
C     V(9) OF V MUST BE INITIALIZED.
C
C        THE DESIRED G-Q-T STEP (REF. 2, 3, 4, 6) SATISFIES
C     (H + ALPHA*D**2)*STEP = -G  FOR SOME NONNEGATIVE ALPHA SUCH THAT
C     H + ALPHA*D**2 IS POSITIVE SEMIDEFINITE.  ALPHA AND STEP ARE
C     COMPUTED BY A SCHEME ANALOGOUS TO THE ONE DESCRIBED IN REF. 5.
C     ESTIMATES OF THE SMALLEST AND LARGEST EIGENVALUES OF THE HESSIAN
C     ARE OBTAINED FROM THE GERSCHGORIN CIRCLE THEOREM ENHANCED BY A
C     SIMPLE FORM OF THE SCALING DESCRIBED IN REF. 7.  CASES IN WHICH
C     H + ALPHA*D**2 IS NEARLY (OR EXACTLY) SINGULAR ARE HANDLED BY
C     THE TECHNIQUE DISCUSSED IN REF. 2.  IN THESE CASES, A STEP OF
C     (EXACT) LENGTH V(8) IS RETURNED FOR WHICH PSI(STEP) EXCEEDS
C     ITS OPTIMAL VALUE BY LESS THAN -V(19)*PSI(STEP).  THE TEST
C     SUGGESTED IN REF. 6 FOR DETECTING THE SPECIAL CASE IS PERFORMED
C     ONCE TWO MATRIX FACTORIZATIONS HAVE BEEN DONE -- DOING SO SOONER
C     SEEMS TO DEGRADE THE PERFORMANCE OF OPTIMIZATION ROUTINES THAT
C     CALL THIS ROUTINE.
C
C DLITVM - APPLIES INVERSE-TRANSPOSE OF COMPACT LOWER TRIANG. MATRIX.
C DLIVMU - APPLIES INVERSE OF COMPACT LOWER TRIANG. MATRIX.
C DLSQRT  - FINDS CHOLESKY FACTOR (OF COMPACTLY STORED LOWER TRIANG.).
C DLSVMN - RETURNS APPROX. TO MIN. SING. VALUE OF LOWER TRIANG. MATRIX.
C
C 1.  DENNIS, J.E., GAY, D.M., AND WELSCH, R.E. (1981), AN ADAPTIVE
C             NONLINEAR LEAST-SQUARES ALGORITHM, ACM TRANS. MATH.
C             SOFTWARE, VOL. 7, NO. 3.
C 2.  GAY, D.M. (1981), COMPUTING OPTIMAL LOCALLY CONSTRAINED STEPS,
C             SIAM J. SCI. STATIST. COMPUTING, VOL. 2, NO. 2, PP.
C             186-197.
C 3.  GOLDFELD, S.M., QUANDT, R.E., AND TROTTER, H.F. (1966),
C             MAXIMIZATION BY QUADRATIC HILL-CLIMBING, ECONOMETRICA 34,
C             PP. 541-551.
C 4.  HEBDEN, M.D. (1973), AN ALGORITHM FOR MINIMIZATION USING EXACT
C             SECOND DERIVATIVES, REPORT T.P. 515, THEORETICAL PHYSICS
C             DIV., A.E.R.E. HARWELL, OXON., ENGLAND.
C 5.  MORE, J.J. (1978), THE LEVENBERG-MARQUARDT ALGORITHM, IMPLEMEN-
C             TATION AND THEORY, PP.105-116 OF SPRINGER LECTURE NOTES
C             IN MATHEMATICS NO. 630, EDITED BY G.A. WATSON, SPRINGER-
C             VERLAG, BERLIN AND NEW YORK.
C 6.  MORE, J.J., AND SORENSEN, D.C. (1981), COMPUTING A TRUST REGION
C             STEP, TECHNICAL REPORT ANL-81-83, ARGONNE NATIONAL LAB.
C 7.  VARGA, R.S. (1965), MINIMAL GERSCHGORIN SETS, PACIFIC J. MATH. 15,
C             PP. 719-729.
C
C     CODED BY DAVID M. GAY.
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH
C     SUPPORTED BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS
C     MCS-7600324, DCR75-10143, 76-14311DSS, MCS76-11989, AND
C     MCS-7906671.
*
************************************************************************
      SUBROUTINE DGQTST(D,DIG,DIHDI,KA,L,NP,STEP,V,W)
*
*     subroutine arguments
*
      INTEGER*4 KA,NP
      DOUBLE PRECISION D(*),DIG(*),DIHDI(*),L(*),V(*),STEP(*),W(*)
*
*     local vars
*
      LOGICAL RESTRT
      INTEGER*4 I,INC,IRC,J,K,KALIM,KAMIN,K1
      DOUBLE PRECISION ALPHAK,AKI,AKK,DELTA,DST,EPS,GTSTA,LK,OLDPHI,
     &                 PHI,PHIMAX,PHIMIN,PSIFAC,RAD,RADSQ,ROOT,SI,SK,
     &                 SW,T,TWOPSI,T1,T2,UK,WI
      DOUBLE PRECISION BIG,DGXFAC
*
*     function declarations
*
      DOUBLE PRECISION DDOT,DLSVMN,D1MACH,DNRM2
      DATA BIG/0.D0/,DGXFAC/0.D0/
*
*     store largest abs. entry in (D**-1)*H*(D**-1) at W(NP+2)
*
*     store Gerschgorin over- and underestimates of the largest and
*     smallest eigenvalues of (D**-1)*H*(D**-1) at W(NP+3) and W(NP+4)
*     respectively
*
*     for use in recomputing step, the final values of LK, UK, DST,AND
*     the inverse derivative of More*s PHI at 0 (for pos. def. H) are
*     stored in W(NP+5), W(NP+7), W(NP+8), and W(NP+6) respectively.
*
*     store diag of (D**-1)*H*(D**-1) in W(NP+9),...,W(NP+NP+9)
*
*     store -D*STEP in W(Q),...,W(NP+NP+NP+10)
*
*     allocate storage for scratch vector X
*
      RAD = V(8)
      RADSQ = RAD**2
*
*     PHITOL = max. error allowed in DST = V(2) = 2-norm of D*STEP
*
      PHIMAX = V(21) * RAD
      PHIMIN = V(20) * RAD
      PSIFAC = 2.D0 * V(19)/(3.D0 * (4.D0 * (V(20) + 1.D0) * 3.D0
     &                       +  4.D0) * RAD**2)
*
*     OLDPHI is used to detect limits of numerical accuracy;  if we
*     recompute step and it does not change, then we accept it
*
      OLDPHI = 0.D0
      EPS = V(19)
      IRC = 0
      RESTRT = .FALSE.
      KALIM = KA + 50
*
*     start or restart, depending on KA
*
      IF (KA .GE. 0) GOTO 290
*
*     fresh start
*
      K = 0
      UK = -1.D0
      KA = 0
      KALIM = 50
      V(1) = DNRM2(NP+1,DIG)
      V(6) = 0.D0
      V(3) = 0.D0
      KAMIN = 3
      IF (V(1) .EQ. 0.D0) KAMIN = 0
*
*     store DIAG(DIHDI) in W(NP+9),...,W(NP+NP+9)
*
      J = 0
      DO 10 I=1,NP+1
         J = J + I
         K1 = NP + 8 + I
         W(K1) = DIHDI(J)
 10   CONTINUE
*
*     determine W(NP+2), the largest element of DIHDI
*
      T1 = 0.D0
      J = (NP+1) * (NP+2) / 2
      DO 20 I = 1, J
         T = DABS(DIHDI(I))
         IF (T1 .LT. T) T1 = T
 20   CONTINUE
      W(NP+2) = T1
*
*     try ALPHA = 0
*
 30   CALL DLSQRT(NP+1,L,DIHDI,IRC)
      IF (IRC .EQ. 0) GOTO 50
*
*     indef. H -- underestimate smallest eigenvalue, use this estimate
*     to initialize lower bound LK on ALPHA.
*
      J = IRC*(IRC+1)/2
      T = L(J)
      L(J) = 1.D0
      DO 40 I = 1, IRC
         W(I) = 0.D0
 40   CONTINUE
      W(IRC) = 1.D0
      CALL DLITVM(IRC,W,L,W)
      T1 = DNRM2(IRC,W)
      LK = -T / T1 / T1
      V(3) = -LK
      IF (RESTRT) GOTO 210
      GOTO 70
*
*     positive definite H -- compute unmodified newton step
*
 50   LK = 0.D0
      T = DLSVMN(NP,L,W(NP+NP+10),W(NP+NP+10))
      IF (T .GE. 1.D0) GOTO 60
      IF (BIG .LE. 0.D0) BIG = D1MACH(2)
      IF (V(1) .GE. T*T*BIG) GOTO 70
 60   CALL DLIVMU(NP+1,W(NP+NP+10),L,DIG)
      GTSTA = DDOT(NP+1,W(NP+NP+10),W(NP+NP+10))
      V(6) = 0.5D0 * GTSTA
      CALL DLITVM(NP+1,W(NP+NP+10),L,W(NP+NP+10))
      DST = DNRM2(NP+1,W(NP+NP+10))
      V(3) = DST
      PHI = DST - RAD
      IF (PHI .LE. PHIMAX) GOTO 260
      IF (RESTRT) GOTO 210
*
*     prepare to compute Gerschgorin estimates of largest (and
*     smallest) eigenvalues.
*
 70   K = 0
      DO 100 I = 1, NP+1
         WI = 0.D0
         IF (I .EQ. 1) GOTO 90
         DO 80 J = 1, I-1
            K = K + 1
            T = DABS(DIHDI(K))
            WI = WI + T
            W(J) = W(J) + T
 80      CONTINUE
 90      W(I) = WI
         K = K + 1
 100  CONTINUE
*
*     (under-)estimate smallest eigenvalue of (D**-1)*H*(D**-1)
*
      K = 1
      T1 = W(NP+9) - W(1)
      IF (NP+1 .LE. 1) GOTO 120
      DO 110 I = 2, NP+1
         J = NP + 8 + I
         T = W(J) - W(I)
         IF (T .GE. T1) GOTO 110
         T1 = T
         K = I
 110  CONTINUE

 120  SK = W(K)
      J = NP + 8 + K
      AKK = W(J)
      K1 = K*(K-1)/2 + 1
      INC = 1
      T = 0.D0
      DO 150 I = 1, NP+1
         IF (I .EQ. K) GOTO 130
         AKI = DABS(DIHDI(K1))
         SI = W(I)
         J = NP + 8 + I
         T1 = 0.5D0 * (AKK - W(J) + SI - AKI)
         T1 = T1 + DSQRT(T1*T1 + SK*AKI)
         IF (T .LT. T1) T = T1
         IF (I .LT. K) GOTO 140
 130     INC = I
 140     K1 = K1 + INC
 150  CONTINUE

      W(NP+4) = AKK - T
      UK = V(1)/RAD - W(NP+4)
      IF (V(1) .EQ. 0.D0) UK = UK + 1.D-3 + 1.D-3*UK
*
*     compute Gerschgorin (over-)estimate of largest eigenvalue
*
      K = 1
      T1 = W(NP+9) + W(1)
      IF (NP+1 .LE. 1) GOTO 170
      DO 160 I = 2, NP+1
         J = NP + 8 + I
         T = W(J) + W(I)
         IF (T .LE. T1) GOTO 160
         T1 = T
         K = I
 160  CONTINUE

 170  SK = W(K)
      J = NP + 8 + K
      AKK = W(J)
      K1 = K*(K-1)/2 + 1
      INC = 1
      T = 0.D0
      DO 200 I = 1, NP+1
         IF (I .EQ. K) GOTO 180
         AKI = DABS(DIHDI(K1))
         SI = W(I)
         J = NP + 8 + I
         T1 = 0.5D0 * (W(J) + SI - AKI - AKK)
         T1 = T1 + DSQRT(T1*T1 + SK*AKI)
         IF (T .LT. T1) T = T1
         IF (I .LT. K) GOTO 190
 180     INC = I
 190     K1 = K1 + INC
 200  CONTINUE

      W(NP+3) = AKK + T
      LK = DMAX1(LK, V(1)/RAD - W(NP+3))
*
*     ALPHAK = current value of ALPHA (see alg. notes above); we use
*     More*s scheme for initializing it
*
      ALPHAK = DABS(V(5)) * V(9)/RAD

      IF (IRC .NE. 0) GOTO 210
*
*     compute L0 for positive definite H
*
      CALL DLIVMU(NP+1,W,L,W(NP+NP+10))
      T = DNRM2(NP+1,W)
      W(NP+6) = DST / T / T
      LK = DMAX1(LK, PHI*W(NP+6))
*
*     safeguard ALPHAK and add ALPHAK*I to (D**-1)*H*(D**-1)
*
 210  KA = KA + 1
      IF (-V(3) .GE. ALPHAK .OR. ALPHAK .LT. LK .OR. ALPHAK .GE. UK)
     &   ALPHAK = UK * DMAX1(1.D-3, DSQRT(LK/UK))
      IF(ALPHAK .LE. 0.D0) ALPHAK = 0.5D0 * UK
      K = 0
      DO 220 I = 1, NP+1
         K = K + I
         J = NP + 8 + I
         DIHDI(K) = W(J) + ALPHAK
 220  CONTINUE
*
*     try Cholesky decomposition
*
      CALL DLSQRT(NP+1,L,DIHDI,IRC)
      IF (IRC .EQ. 0) GOTO 240
*
*     (D**-1)*H*(D**-1) + ALPHAK*I is indefinite -- overestimate
*     smallest eigenvalue for use in updating LK
*
      J = (IRC*(IRC+1))/2
      T = L(J)
      L(J) = 1.D0
      DO 230 I = 1,IRC
         W(I) = 0.D0
 230  CONTINUE
      W(IRC) = 1.D0
      CALL DLITVM(IRC,W,L,W)
      T1 = DNRM2(IRC,W)
      LK = ALPHAK - T/T1/T1
      V(3) = -LK
      GOTO 210
*
*     ALPHAK makes (D**-1)*H*(D**-1) positive definite; compute
*     Q = -D*STEP, check for convergence
*
 240  CALL DLIVMU(NP+1,W(NP+NP+10),L,DIG)
      GTSTA = DDOT(NP+1,W(NP+NP+10),W(NP+NP+10))
      CALL DLITVM(NP+1,W(NP+NP+10),L,W(NP+NP+10))
      DST = DNRM2(NP+1,W(NP+NP+10))
      PHI = DST - RAD
      IF (PHI .LE. PHIMAX .AND. PHI .GE. PHIMIN) GOTO 270
      IF (PHI .EQ. OLDPHI) GOTO 270
      OLDPHI = PHI
      IF (PHI .LT. 0.D0) GOTO 330
*
*     unacceptable ALPHAK -- update LK, UK, ALPHAK
*
 250  IF (KA .GE. KALIM) GOTO 270
*
*     the following DMIN1 is necessary because of restarts
*
      IF (PHI .LT. 0.D0) UK = DMIN1(UK, ALPHAK)
*
*     KAMIN = 0 only iff the gradient vanishes
*
      IF (KAMIN .EQ. 0) GOTO 210
      CALL DLIVMU(NP+1,W,L,W(NP+NP+10))
      T1 = DNRM2(NP+1,W)
      ALPHAK = ALPHAK  +  (PHI/T1) * (DST/T1) * (DST/RAD)
      LK = DMAX1(LK, ALPHAK)
      GOTO 210
*
*     acceptable step on first try
*
 260  ALPHAK = 0.D0
*
*     successful step in general; compute STEP = -(D**-1)*Q
*
 270  DO 280 I = 1, NP+1
         J = NP + NP + 9 + I
         STEP(I) = -W(J)/D(I)
 280  CONTINUE
      V(4) = -GTSTA
      V(7) = 0.5D0 * (DABS(ALPHAK)*DST*DST + GTSTA)
      GOTO 410
*
*     restart with new radius
*
 290  IF (V(3) .LE. 0.D0 .OR. V(3) - RAD .GT. PHIMAX) GOTO 310
*
*     prepare to return Newton step
*
      RESTRT = .TRUE.
      KA = KA + 1
      K = 0
      DO 300 I = 1, NP+1
         K = K + I
         J = NP + 8 + I
         DIHDI(K) = W(J)
 300  CONTINUE
      UK = -1.D0
      GOTO 30

 310  KAMIN = KA + 3
      IF (V(1) .EQ. 0.D0) KAMIN = 0
      IF (KA .EQ. 0) GOTO 50

      DST = W(NP+8)
      ALPHAK = DABS(V(5))
      PHI = DST - RAD
      T = V(1)/RAD
      UK = T - W(NP+4)
      IF (V(1) .EQ. 0.D0) UK = UK + 1.D-3 + 1.D-3*UK
      IF (RAD .GT. V(9)) GOTO 320
*
*     SMALLER RADIUS
*
      LK = 0.D0
      IF (ALPHAK .GT. 0.D0) LK = W(NP+5)
      LK = DMAX1(LK, T - W(NP+3))
      IF (V(3) .GT. 0.D0) LK = DMAX1(LK, (V(3)-RAD)*W(NP+6))
      GOTO 250
*
*     bigger radius
*
 320  IF (ALPHAK .GT. 0.D0) UK = DMIN1(UK, W(NP+7))
      LK = DMAX1(0.D0, -V(3), T - W(NP+3))
      IF (V(3) .GT. 0.D0) LK = DMAX1(LK, (V(3)-RAD)*W(NP+6))
      GOTO 250
*
*     decide whether to check for special case... in practice (from
*     the standpoint of the calling optimization code) it seems best
*     not to check until a few iterations have failed -- hence the
*     test on KAMIN below.
*
 330  DELTA = ALPHAK + DMIN1(0.D0, V(3))
      TWOPSI = ALPHAK*DST*DST + GTSTA
      IF (KA .GE. KAMIN) GOTO 340
*
*     if the test in ref. 2 is satisfied, fall through to handle the
*     special case (as soon as the More-Sorensen test detects it).
*
      IF (DELTA .GE. PSIFAC*TWOPSI) GOTO 370
*
*     check for the special case of H + ALPHA*D**2 (nearly) singular.
*     Use one step of inverse power method with start from DLSVMN to
*     obtain approximate eigenvector corresponding to smallest
*     eigenvalue of (D**-1)*H*(D**-1).  DLSVMN returns X and W with
*     L*W = X.
*
 340  T = DLSVMN(NP,L,W(3*NP+11),W)
*
*     normalize W
*
      DO 350 I = 1, NP+1
         W(I) = T*W(I)
 350  CONTINUE
*
*     complete current inv. power iter. -- replace W by (L**-T)*W.
*
      CALL DLITVM(NP+1,W,L,W)
      T2 = 1.D0/DNRM2(NP+1,W)
      DO 360 I = 1, NP+1
         W(I) = T2*W(I)
 360  CONTINUE
      T = T2 * T
*
*     now W is the desired approximate (unit) eigenvector and
*     T*X = ((D**-1)*H*(D**-1) + ALPHAK*I)*W.
*
      SW = DDOT(NP+1,W(NP+NP+10),W)
      T1 = (RAD + DST) * (RAD - DST)
      ROOT = DSQRT(SW*SW + T1)
      IF (SW .LT. 0.D0) ROOT = -ROOT
      SI = T1 / (SW + ROOT)
*
*     the actual test for the special case...
*
      IF ((T2*SI)**2 .LE. EPS*(DST**2 + ALPHAK*RADSQ)) GOTO 380
*
*     update upper bound on smallest eigenvalue (when not positive)
*     (as recommended by More and Sorensen) and continue...
*
      IF (V(3) .LE. 0.D0) V(3) = DMIN1(V(3), T2**2 - ALPHAK)
      LK = DMAX1(LK, -V(3))
*
*     check whether we can hope to detect the special case in the
*     available arithmetic.  Accept STEP as it is if not.
*     If not yet available, obtain machine dependent value DGXFAC.
*
 370  IF (DGXFAC .EQ. 0.D0) DGXFAC = 50.D0 * D1MACH(4)
      IF (DELTA .GT. DGXFAC*W(NP+2)) GOTO 250
      GOTO 270
*
*     special case detected... negate ALPHAK to indicate special case
*
 380  ALPHAK = -ALPHAK
      V(7) = 0.5D0 * TWOPSI
*
*     accept current step if adding SI*W would lead to a further
*     relative reduction in PSI of less than V(19)/3.
*
      T1 = 0.D0
      T = SI*(ALPHAK*SW-0.5D0*SI*(ALPHAK + T*DDOT(NP+1,W(3*NP+11),W)))
      IF (T .LT. EPS*TWOPSI/6.D0) GOTO 390
      V(7) = V(7) + T
      DST = RAD
      T1 = -SI
 390  DO 400 I = 1, NP+1
         J = NP + NP + 9 + I
         W(J) = T1*W(I) - W(J)
         STEP(I) = W(J) / D(I)
 400  CONTINUE
      V(4) = DDOT(NP+1,DIG,W(NP+NP+10))
*
*     save values for use in a possible restart
*
 410  V(2) = DST
      V(5) = ALPHAK
      W(NP+5) = LK
      W(NP+7) = UK
      V(9) = RAD
      W(NP+8) = DST
*
*     restore diagonal of DIHDI
*
      J = 0
      DO 420 I=1,NP+1
         J = J + I
         K = NP + 8 + I
         DIHDI(J) = W(K)
 420  CONTINUE

      RETURN
      END
