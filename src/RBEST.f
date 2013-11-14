      SUBROUTINE RBEST(M,S,BIAS,SCOV,R,XMOM)
C==============================================================================
C
C     PROGRAM TO COMPUTE THE RAO-BLACKWELL ESTIMATORS FOR THE
C     MEAN AND VARIANCE OF CENSORED LN2 SAMPLES
C
C    THIS METHOD EMPLOYS THE FINNEY [1941] FUNCTION, ADAPTED BY TIM COHN
C    TO THE PROBLEM OF CENSORED DATA.  IN PARTICULAR, WE ARE TRYING TO
C    FIND AN ESTIMATOR FOR
C
C        EXP[R(1)*MU + R(2)*SIGMA**2]
C
C==============================================================================
C
C     AUTHOR.................TIM COHN
C     DATE...................SEPTEMBER 23, 1986
C
C        M              R*4       MLE FOR MU (INPUT)
C        S              R*4       MLE FOR SIGMA (INPUT)
C        BIAS(2,3)      R*4       ESTIMATED STANDARDIZED BIAS (INPUT)
C        SCOV(2,2,3)    R*4       ESTIMATED (STANDARDIZED) PARM COV (INPUT)
C                                      N.B.  THE FIRST MATRIX SCOV(I,J,1),
C                                      CORRESPONDS TO THE COV WRT SIGMA;
C                                      SCOV(I,J,2) IS WRT SIGMA SQUARED;
C                                      SCOV(I,J,3) IS WRT SIGMA^4
C                                      THE SAME IS TRUE FOR BIAS(I,1),ETC.
C        R(2)           R*4       NON-CENTRAL MOMENT TO BE ESTIMATED (INPUT)
C        XMOM           R*4       VALUE OF EXPRESSION (OUTPUT)
C
C==============================================================================

      IMPLICIT REAL*8 (A-H,K-Z)
      DIMENSION BIAS(2,3),SCOV(2,2,3),R(2),COV(2,2)

      S2           =  S**2
      COV(1,1)     =  SCOV(1,1,2)*S2
      COV(1,2)     =  SCOV(1,2,2)*S**3
      COV(2,2)     =  SCOV(2,2,2)*S**4

C==============================================================================
C
C        DEVELOP UNCORRELATED PARAMETERS (W AND S^2)
C        N.B. ALSO REMOVE BIAS FROM ESTIMATED MU; IGNORE IMPACT ON COVARIANCE
C             (N^-2 EFFECTS ARE NEGLIGIBLE HERE)
C
      ALPHA1  =  -COV(1,2)/COV(2,2)
      W       =  M - S*BIAS(1,2) + ALPHA1*S2

      FACT1   =  EXP(R(1)*W)
      B       =  R(1)*ALPHA1+R(1)**2*(COV(1,1)+ALPHA1*COV(1,2))/(2.0*S2)

      T       =  S2*(R(2) - B)

C==============================================================================
C
C    COMPUTE THE GAMMA DISTRIBUTION'S PARAMETERS FROM THE ASYMPTOTIC MOMENTS
C        ALPHA     =  E[S^2]^2/VAR[S^2]
C        BETA      =  E[S^2]/VAR[S^2]
C
         VARS2     =  1.+BIAS(2,3)-(1.+BIAS(2,2))**2

      ALPHA   =  (1.0+BIAS(2,2))**2/VARS2
      BETA    =  ALPHA/(1.+BIAS(2,2))

      FACT2   =  GM(ALPHA,BETA,T)

      XMOM    =  FACT1*FACT2

      RETURN
      END
