************************************************************************
*
*     Subroutine DASSST                               Called by: DHUMIT
*
*     Assess the next candidate step for unconstrained minimization.
*     Recommend one of several courses of action, such as accepting the
*     step, recomputing it using the same or a new quadratic model, or
*     halting due to convergence or false convergence.  See the RETURN
*     code listing below.
*
C     IV (I/O) INTEGER PARAMETER AND SCRATCH VECTOR -- SEE DESCRIPTION
C             BELOW OF IV VALUES REFERENCED.
C      D (IN)  SCALE VECTOR USED IN COMPUTING V(17) -- SEE BELOW.
C      P (IN)  NUMBER OF PARAMETERS BEING OPTIMIZED.
C   STEP (I/O) ON INPUT, STEP IS THE STEP TO BE ASSESSED.  IT IS UN-
C             CHANGED ON OUTPUT UNLESS A PREVIOUS STEP ACHIEVED A
C             BETTER OBJECTIVE FUNCTION REDUCTION, IN WHICH CASE STLSTG
C             WILL HAVE BEEN COPIED TO STEP.
C STLSTG (I/O) WHEN ASSESS RECOMMENDS RECOMPUTING STEP EVEN THOUGH THE
C             CURRENT (OR A PREVIOUS) STEP YIELDS AN OBJECTIVE FUNC-
C             TION DECREASE, IT SAVES IN STLSTG THE STEP THAT GAVE THE
C             BEST FUNCTION REDUCTION SEEN SO FAR (IN THE CURRENT ITERA-
C             TION).  IF THE RECOMPUTED STEP YIELDS A LARGER FUNCTION
C             VALUE, THEN STEP IS RESTORED FROM STLSTG AND
C             X = X0 + STEP IS RECOMPUTED.
C      V (I/O) REAL PARAMETER AND SCRATCH VECTOR -- SEE DESCRIPTION
C             BELOW OF V VALUES REFERENCED.
C      X (I/O) ON INPUT, X = X0 + STEP IS THE POINT AT WHICH THE OBJEC-
C             TIVE FUNCTION HAS JUST BEEN EVALUATED.  IF AN EARLIER
C             STEP YIELDED A BIGGER FUNCTION DECREASE, THEN X IS
C             RESTORED TO THE CORRESPONDING EARLIER VALUE.  OTHERWISE,
C             IF THE CURRENT STEP DOES NOT GIVE ANY FUNCTION DECREASE,
C             THEN X IS RESTORED TO X0.
C     X0 (IN)  INITIAL OBJECTIVE FUNCTION PARAMETER VECTOR (AT THE
C             START OF THE CURRENT ITERATION).
C
C  ***  IV VALUES REFERENCED  ***
C
C    IV(29) (I/O) ON INPUT FOR THE FIRST STEP TRIED IN A NEW ITERATION,
C             IV(29) SHOULD BE SET TO 3 OR 4 (THE VALUE TO WHICH IT IS
C             SET WHEN STEP IS DEFINITELY TO BE ACCEPTED).  ON INPUT
C             AFTER STEP HAS BEEN RECOMPUTED, IV(29) SHOULD BE
C             UNCHANGED SINCE THE PREVIOUS RETURN OF ASSESS.
C                ON OUTPUT, IV(29) IS A RETURN CODE HAVING ONE OF THE
C             FOLLOWING VALUES...
C                  1 = SWITCH MODELS OR TRY SMALLER STEP.
C                  2 = SWITCH MODELS OR ACCEPT STEP.
C                  3 = ACCEPT STEP AND DETERMINE V(16) BY GRADIENT
C                       TESTS.
C                  4 = ACCEPT STEP, V(16) HAS BEEN DETERMINED.
C                  5 = RECOMPUTE STEP (USING THE SAME MODEL).
C                  6 = RECOMPUTE STEP WITH RADIUS = V(36) BUT DO NOT
C                       EVAULATE THE OBJECTIVE FUNCTION.
C                  7 = X-CONVERGENCE (SEE V(33)).
C                  8 = RELATIVE FUNCTION CONVERGENCE (SEE V(32)).
C                  9 = BOTH X- AND RELATIVE FUNCTION CONVERGENCE.
C                 10 = ABSOLUTE FUNCTION CONVERGENCE (SEE V(AFCTOL)).
C                 11 = SINGULAR CONVERGENCE (SEE V(36)).
C                 12 = FALSE CONVERGENCE (SEE V(34)).
C                 13 = IV(29) WAS OUT OF RANGE ON INPUT.
C             RETURN CODE I HAS PRECDENCE OVER I+1 FOR I = 9, 10, 11.
C IV(32) (I/O) SAVED VALUE OF IV(5).
C  IV(5) (I/O) ON INPUT, IV(5) SHOULD BE AN INTEGER IDENTIFYING
C             THE CURRENT QUADRATIC MODEL OF THE OBJECTIVE FUNCTION.
C             IF A PREVIOUS STEP YIELDED A BETTER FUNCTION REDUCTION,
C             THEN IV(5) WILL BE SET TO IV(32) ON OUTPUT.
C IV(6) (IN)  INVOCATION COUNT FOR THE OBJECTIVE FUNCTION.
C IV(7) (I/O) VALUE OF IV(6) AT STEP THAT GAVE THE BIGGEST
C             FUNCTION REDUCTION THIS ITERATION.  IV(7) REMAINS
C             UNCHANGED UNTIL A FUNCTION REDUCTION IS OBTAINED.
C IV(8) (I/O) THE NUMBER OF RADIUS INCREASES (OR MINUS THE NUMBER
C             OF DECREASES) SO FAR THIS ITERATION.
C IV(9) (OUT) SET TO 0 UNLESS X AND V(10) HAVE BEEN RESTORED, IN
C             WHICH CASE ASSESS SETS IV(9) = 1.
C  IV(10) (I/O) COUNT OF THE NUMBER OF MODELS TRIED SO FAR IN THE
C             CURRENT ITERATION.
C IV(11) (IN)  MAXIMUM NUMBER OF MODELS TO CONSIDER.
C IV(SWITCH) (OUT) SET TO 0 UNLESS A NEW MODEL IS BEING TRIED AND IT
C             GIVES A SMALLER FUNCTION VALUE THAN THE PREVIOUS MODEL,
C             IN WHICH CASE ASSESS SETS IV(SWITCH) = 1.
C IV(2) (IN)  IS NONZERO IF STEP WAS TOO BIG (E.G. IF IT CAUSED
C             OVERFLOW).
C   IV(13) (I/O) VALUE THAT IV(29) WOULD HAVE IN THE ABSENCE OF
C             CONVERGENCE, FALSE CONVERGENCE, AND OVERSIZED STEPS.
C
C  ***  V VALUES REFERENCED  ***
C
C V(AFCTOL) (IN)  ABSOLUTE FUNCTION CONVERGENCE TOLERANCE.  IF THE
C             ABSOLUTE VALUE OF THE CURRENT FUNCTION VALUE V(10) IS LESS
C             THAN V(AFCTOL), THEN ASSESS RETURNS WITH IV(29) = 10.
C V(22) (IN)  FACTOR BY WHICH TO DECREASE RADIUS WHEN IV(2) IS
C             NONZERO.
C V(2) (IN)  THE 2-NORM OF D*STEP.
C V(18) (I/O) VALUE OF V(2) ON SAVED STEP.
C   V(3) (IN)  THE 2-NORM OF D TIMES THE NEWTON STEP (WHEN DEFINED,
C             I.E., FOR V(6) .GE. 0).
C      V(10) (I/O) ON BOTH INPUT AND OUTPUT, V(10) IS THE OBJECTIVE FUNC-
C             TION VALUE AT X.  IF X IS RESTORED TO A PREVIOUS VALUE,
C             THEN V(10) IS RESTORED TO THE CORRESPONDING VALUE.
C   V(11) (OUT) THE FUNCTION REDUCTION V(13) - V(10) (FOR THE OUTPUT
C             VALUE OF V(10) IF AN EARLIER STEP GAVE A BIGGER FUNCTION
C             DECREASE, AND FOR THE INPUT VALUE OF V(10) OTHERWISE).
C V(12) (I/O) SAVED VALUE OF V(10).
C     V(13) (IN)  OBJECTIVE FUNCTION VALUE AT START OF ITERATION.
C V(14) (I/O) VALUE OF V(4) ON SAVED STEP.
C V(4) (IN)  INNER PRODUCT BETWEEN STEP AND GRADIENT.
C V(23) (IN)  MINIMUM FACTOR BY WHICH TO INCREASE RADIUS.
C  V(36) (IN)  MAXIMUM REASONABLE STEP SIZE (AND INITIAL STEP BOUND).
C             IF THE ACTUAL FUNCTION DECREASE IS NO MORE THAN TWICE
C             WHAT WAS PREDICTED, IF A RETURN WITH IV(29) = 7, 8, 9,
C             OR 10 DOES NOT OCCUR, IF V(2) .GT. V(36), AND IF
C             V(7) .LE. V(37) * ABS(V(13)), THEN ASSESS RE-
C             TURNS WITH IV(29) = 11.  IF SO DOING APPEARS WORTHWHILE,
C             THEN ASSESS REPEATS THIS TEST WITH V(7) COMPUTED FOR
C             A STEP OF LENGTH V(36) (BY A RETURN WITH IV(29) = 6).
C V(6) (I/O)  FUNCTION REDUCTION PREDICTED BY QUADRATIC MODEL FOR
C             NEWTON STEP.  IF ASSESS IS CALLED WITH IV(29) = 6, I.E.,
C             IF V(7) HAS BEEN COMPUTED WITH RADIUS = V(36) FOR
C             USE IN THE SINGULAR CONVERVENCE TEST, THEN V(6) IS
C             SET TO -V(7) BEFORE THE LATTER IS RESTORED.
C V(15) (I/O) VALUE OF V(7) ON SAVED STEP.
C V(7) (I/O) FUNCTION REDUCTION PREDICTED BY QUADRATIC MODEL FOR
C             CURRENT STEP.
C V(16) (OUT) FACTOR TO BE USED IN DETERMINING THE NEW RADIUS,
C             WHICH SHOULD BE V(16)*DST, WHERE  DST  IS EITHER THE
C             OUTPUT VALUE OF V(2) OR THE 2-NORM OF
C             DIAG(NEWD)*STEP  FOR THE OUTPUT VALUE OF STEP AND THE
C             UPDATED VERSION, NEWD, OF THE SCALE VECTOR D.  FOR
C             IV(29) = 3, V(16) = 1.0 IS RETURNED.
C V(24) (IN)  MINIMUM VALUE FOR V(16) IN TERMS OF THE INPUT
C             VALUE OF V(2) -- SUGGESTED VALUE = 0.1.
C V(25) (IN)  MAXIMUM VALUE FOR V(16) -- SUGGESTED VALUE = 4.0.
C  V(17) (OUT) SCALED RELATIVE CHANGE IN X CAUSED BY STEP, COMPUTED
C             BY FUNCTION  DRELST  AS
C                 MAX (D(I)*ABS(X(I)-X0(I)), 1 .LE. I .LE. P) /
C                    MAX (D(I)*(ABS(X(I))+ABS(X0(I))), 1 .LE. I .LE. P).
C             IF AN ACCEPTABLE STEP IS RETURNED, THEN V(17) IS COM-
C             PUTED USING THE OUTPUT (POSSIBLY RESTORED) VALUES OF X
C             AND STEP.  OTHERWISE IT IS COMPUTED USING THE INPUT
C             VALUES.
C V(32) (IN)  RELATIVE FUNCTION CONVERGENCE TOLERANCE.  IF THE
C             ACTUAL FUNCTION REDUCTION IS AT MOST TWICE WHAT WAS PRE-
C             DICTED AND  V(6) .LE. V(32)*ABS(V(13)),  THEN
C             ASSESS RETURNS WITH IV(29) = 8 OR 9.
C V(5) (IN)  MARQUARDT PARAMETER -- 0 MEANS FULL NEWTON STEP.
C V(26) (IN)  TUNING CONSTANT USED TO DECIDE IF THE FUNCTION
C             REDUCTION WAS MUCH LESS THAN EXPECTED.  SUGGESTED
C             VALUE = 0.1.
C V(27) (IN)  TUNING CONSTANT USED TO DECIDE IF THE FUNCTION
C             REDUCTION WAS LARGE ENOUGH TO ACCEPT STEP.  SUGGESTED
C             VALUE = 10**-4.
C V(28) (IN)  TUNING CONSTANT USED TO DECIDE IF THE RADIUS
C             SHOULD BE INCREASED.  SUGGESTED VALUE = 0.75.
C  V(33) (IN)  X-CONVERGENCE CRITERION.  IF STEP IS A NEWTON STEP
C             (V(5) = 0) HAVING V(17) .LE. V(33) AND GIVING
C             AT MOST TWICE THE PREDICTED FUNCTION DECREASE, THEN
C             ASSESS RETURNS IV(29) = 7 OR 9.
C  V(34) (IN)  FALSE CONVERGENCE TOLERANCE.  IF STEP GAVE NO OR ONLY
C             A SMALL FUNCTION DECREASE AND V(17) .LE. V(34),
C             THEN ASSESS RETURNS WITH IV(29) = 12.
C
C        THIS ROUTINE IS CALLED AS PART OF THE NL2SOL (NONLINEAR
C     LEAST-SQUARES) PACKAGE.  IT MAY BE USED IN ANY UNCONSTRAINED
C     MINIMIZATION SOLVER THAT USES DOGLEG, GOLDFELD-QUANDT-TROTTER,
C     OR LEVENBERG-MARQUARDT STEPS.
C
C        SEE (1) FOR FURTHER DISCUSSION OF THE ASSESSING AND MODEL
C     SWITCHING STRATEGIES.  WHILE NL2SOL CONSIDERS ONLY TWO MODELS,
C     ASSESS IS DESIGNED TO HANDLE ANY NUMBER OF MODELS.
C
C        ON THE FIRST CALL OF AN ITERATION, ONLY THE I/O VARIABLES
C     STEP, X, IV(29), IV(5), V(10), V(2), V(4), AND
C     V(7) NEED HAVE BEEN INITIALIZED.  BETWEEN CALLS, NO I/O
C     VALUES EXECPT STEP, X, IV(5), V(10) AND THE STOPPING TOLER-
C     ANCES SHOULD BE CHANGED.
C        AFTER A RETURN FOR CONVERGENCE OR FALSE CONVERGENCE, ONE CAN
C     CHANGE THE STOPPING TOLERANCES AND CALL ASSESS AGAIN, IN WHICH
C     CASE THE STOPPING TESTS WILL BE REPEATED.
C
C
C     (1) DENNIS, J.E., JR., GAY, D.M., AND WELSCH, R.E. (1981),
C        AN ADAPTIVE NONLINEAR LEAST-SQUARES ALGORITHM,
C        ACM TRANS. MATH. SOFTWARE, VOL. 7, NO. 3.
C
C     (2) POWELL, M.J.D. (1970)  A FORTRAN SUBROUTINE FOR SOLVING
C        SYSTEMS OF NONLINEAR ALGEBRAIC EQUATIONS, IN NUMERICAL
C        METHODS FOR NONLINEAR ALGEBRAIC EQUATIONS, EDITED BY
C        P. RABINOWITZ, GORDON AND BREACH, LONDON.
C
C        JOHN DENNIS DESIGNED MUCH OF THIS ROUTINE, STARTING WITH
C     IDEAS IN (2). ROY WELSCH SUGGESTED THE MODEL SWITCHING STRATEGY.
C        DAVID GAY AND STEPHEN PETERS CAST THIS SUBROUTINE INTO A MORE
C     PORTABLE FORM (WINTER 1977), AND DAVID GAY CAST IT INTO ITS
C     PRESENT FORM (FALL 1978).
C
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH
C     SUPPORTED BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS
C     MCS-7600324, DCR75-10143, 76-14311DSS, MCS76-11989, AND
C     MCS-7906671.
*
**********************************************************************
      SUBROUTINE DASSST(D,IV,NP,STEP,STLSTG,V,X,X0)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 NP,IV(*)
      DOUBLE PRECISION D(*),STEP(*),STLSTG(*),V(*),X(*),X0(*)
*
*     local vars
*
      LOGICAL GOODX
      INTEGER*4 I,NFC
      DOUBLE PRECISION EMAX,EMAXS,GTS,DRELST,RFAC1,XMAX,T
*
*     
*
      NFC = IV(6)
      IV(12) = 0
      IV(9) = 0
      RFAC1 = 1.D0
      GOODX = .TRUE.
      I = IV(29)

      GOTO (20,30,10,10,40,280,220,220,220,220,220,170), I
      IV(29) = 13
      RETURN
*
*     initialize for new iteration
*
 10   IV(10) = 1
      IV(8) = 0
      V(12) = V(13)
      IF (IV(2) .EQ. 0) GOTO 90
      IV(10) = -1
      IV(13) = I
      GOTO 60
*
*     step was recomputed with new model or smaller radius, first
*     decide which
*
 20   IF (IV(5) .EQ. IV(32)) THEN
*
*        old model retained, smaller radius tried
*        do not consider any more new models this iteration
*
         IV(10) = IV(11)
         IV(8) = -1
         GOTO 90
      ENDIF
*
*     a new model is being tried; decide whether to keep it
*
 30   IV(10) = IV(10) + 1
*
*     now we add the possibiltiy that step was recomputed with the
*     same model, perhaps because of an oversized step
*
 40   IF (IV(10) .LE. 0) THEN
*
*        step was recomputed because it was too big
*
         IF (IV(2) .NE. 0) GOTO 60
*
*        restore IV(10) and pick up where we left off
*
         IV(10) = -IV(10)
         I = IV(13)
         GOTO (20,30,90,90,70), I
      ENDIF

 50   IF (IV(2) .EQ. 0) GOTO 70
*
*     handle oversize step
*
      IF (IV(8) .GT. 0) GOTO 80
      IV(10) = -IV(10)
      IV(13) = IV(29)

 60   V(16) = V(22)
      IV(8) = IV(8) - 1
      IV(29) = 5
      RETURN

 70   IF (V(10) .LT. V(12)) GOTO 90
*
*     the new step is a loser; restore old model
*
      IF (IV(5) .NE. IV(32)) THEN
         IV(5) = IV(32)
         IV(12) = 1
      ENDIF
*
*     restore step, etc. only if a previous step decreased V(10)
*
 80   IF (V(12) .LT. V(13)) THEN
         IV(9) = 1
         V(10) = V(12)
         V(7) = V(15)
         V(4) = V(14)
         IF (IV(12) .EQ. 0) RFAC1 = V(2) / V(18)
         V(2) = V(18)
         NFC = IV(7)
         GOODX = .FALSE.
      ENDIF
*
*     COMPUTE RELATIVE CHANGE IN X BY CURRENT STEP  ***
C DRELST... COMPUTES V(17) = RELATIVE STEP SIZE.
C  ***  COMPUTE AND RELATIVE DIFFERENCE BETWEEN X AND X0  ***
C  ***  NL2SOL VERSION 2.2  ***
C
 90   EMAX = 0.D0
      XMAX = 0.D0
      DO 95 I=1,NP+1
         T = DABS(D(I) * (X(I) - X0(I)))
         IF (EMAX .LT. T) EMAX = T
         T = D(I) * (DABS(X(I)) + DABS(X0(I)))
         IF (XMAX .LT. T) XMAX = T
 95   CONTINUE
      DRELST = 0.D0
      IF (XMAX .GT. 0.D0) DRELST = EMAX / XMAX
*
*     restore X and step if necessary
*
      IF (.NOT. GOODX) THEN
         DO 100 I = 1, NP+1
            STEP(I) = STLSTG(I)
            X(I) = X0(I) + STLSTG(I)
 100     CONTINUE
      ENDIF
      V(11) = V(13) - V(10)

      IF (V(11) .LE. V(27) * V(7)) THEN
*
*        no (or only a trivial) function decrease -- try new model or
*        smaller radius
*
         V(17) = DRELST
         IF (V(10) .GE. V(13)) THEN
            IV(32) = IV(5)
            V(12) = V(10)
            V(10) = V(13)
            CALL DCOPY(NP+1,X0,X)
            IV(9) = 1
            GOTO 130
         ENDIF
         IV(7) = NFC
 130     IV(29) = 1
         IF (IV(10) .LT. IV(11)) GOTO 160
         IV(29) = 5
         IV(8) = IV(8) - 1
         GOTO 160
      ENDIF
*
*     nontrivial function decrease achieved
*
      IV(7) = NFC
      RFAC1 = 1.D0
      IF (GOODX) V(17) = DRELST
      V(18) = V(2)
      IF (V(11) .GT. V(7)*V(26)) GOTO 190
*
*     decrease was much less than predicted -- either change models or
*     accept step with decreased radius
*
      IF (IV(10) .LT. IV(11)) THEN
*
*        consider switching models
*
         IV(29) = 2
         GOTO 160
      ENDIF
*
*     accept step with decreased radius
*
      IV(29) = 4
*
*     set V(16) to Fletcher*S DECREASE FACTOR  ***
*
 160  IV(13) = IV(29)
      EMAX = V(4) + V(11)
      V(16) = 0.5D0 * RFAC1
      IF (EMAX .LT. V(4))
     &     V(16) = RFAC1 * DMAX1(V(24),0.5D0 * V(4)/EMAX)
*
*     do false convergence test
*
 170  IF (V(17) .GT. V(34)) THEN
         IV(29) = IV(13)
         IF (V(10) .LT. V(13)) GOTO 200
         GOTO 230
      ENDIF
      IV(29) = 12
      GOTO 240
*
*     handle good function decrease
*
 190  IF (V(11) .LT. (-V(28) * V(4))) GOTO 210
*
*     increasing radius looks worthwhile; see if we just recomputed
*     step with a decreased radius or restored step after recomputing
*     it with a larger radius
*
      IF (IV(8) .LT. 0) GOTO 210
      IF (IV(9) .EQ. 1) GOTO 210
*
*     we did not; try a longer step unless this was a Newton step
*
      V(16) = V(25)
      GTS = V(4)
      IF (V(11) .LT. (0.5D0/V(16) - 1.D0) * GTS)
     &     V(16) = DMAX1(V(23), 0.5D0*GTS/(GTS + V(11)))
      IV(29) = 4
      IF (V(5) .EQ. 0.D0) GOTO 230
*
*     step was not a Newton step; recompute step with a larger radius
*
      IV(29) = 5
      IV(8) = IV(8) + 1
*
*     save values corresponding to good step
*
 200  V(12) = V(10)
      IV(32) = IV(5)
      CALL DCOPY(NP+1,STEP,STLSTG)
      V(18) = V(2)
      IV(7) = NFC
      V(15) = V(7)
      V(14) = V(4)
      GOTO 230
*
*     accept step with radius unchanged
*
 210  V(16) = 1.D0
      IV(29) = 3
      GOTO 230
*
*     come here for a restart after convergence
*
 220  IV(29) = IV(13)
      IF (V(18) .GE. 0.D0) GOTO 240
      IV(29) = 12
      GOTO 240
*
*     perform convergence tests
*
 230  IV(13) = IV(29)
 240  IF (DABS(V(10)) .LT. V(31)) IV(29) = 10
      IF (0.5D0 * V(11) .GT. V(7)) RETURN
      EMAX = V(32) * DABS(V(13))
      EMAXS = V(37) * DABS(V(13))
      IF (V(2) .GT. V(36) .AND. V(7) .LE. EMAXS) IV(29) = 11
      IF (V(3) .GE. 0.D0) THEN
         I = 0
         IF ((V(6).GT.0.D0.AND.V(6).LE. EMAX) .OR.
     &        (V(6).EQ.0.D0.AND.V(7).EQ.0.D0))  I = 2
         IF (V(5) .EQ. 0.D0 .AND. V(17) .LE. V(33) .AND. GOODX)
     &        I = I + 1
         IF (I .GT. 0) IV(29) = I + 6
      ENDIF
*
*     consider recomputing step of length V(36) for singular
*     convergence test
*     
      IF (IV(29) .GT. 5 .AND. IV(29) .NE. 12) RETURN
      IF (V(2) .LE. V(36)) THEN
         IF (V(7) .GE. EMAXS) RETURN
         IF (V(3) .LE. 0.D0) GOTO 270
         IF (0.5D0 * V(3) .LE. V(36)) RETURN
         GOTO 270
      ENDIF
      IF (0.5D0 * V(2) .LE. V(36)) RETURN
      XMAX = V(36) / V(2)
      IF (XMAX * (2.D0 - XMAX) * V(7) .GE. EMAXS) RETURN
 270  IF (V(6) .LT. 0.D0) THEN
         IF (-V(6) .LE. V(32) * DABS(V(13))) IV(29) = 11
         RETURN
      ENDIF
*
*     recompute V(7) for use in singular convergence test
*
      V(14) = V(4)
      V(18) = V(2)
      IF (IV(29) .EQ. 12) V(18) = -V(18)
      V(15) = V(7)
      IV(29) = 6
      CALL DCOPY(NP+1,STEP,STLSTG)
      RETURN
*
*     perform singular convergence test with recomputed V(7)
*
 280  V(4) = V(14)
      V(2) = DABS(V(18))
      CALL DCOPY(NP+1,STLSTG,STEP)
      IV(29) = IV(13)
      IF (V(18) .LE. 0.D0) IV(29) = 12
      V(6) = -V(7)
      V(7) = V(15)
      IF (-V(6) .LE. V(32) * DABS(V(13))) IV(29) = 11

      RETURN
      END
