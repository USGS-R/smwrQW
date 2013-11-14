      SUBROUTINE RBMOMNT(NCT,X,XD,PMU,PSIGMA,XMEAN,XSE)

C==============================================================================
C
C    PROGRAM TO TEST UNBIASED ESTIMATOR FOR MEAN AND STANDARD ERROR OF
C    LN2 CENSORED DATA
C
C    AUTHOR..............TIM COHN
C    DATE................SEPTEMBER 19,1986
C    REVISED...........SEPTEMBER 22, 1986
C    REVISED AGAIN.....MAY 23, 1988    by Dennis Helsel
C    REVISED AGAIN.....July 12, 1994   by Tim Cohn
C
C    NCT      I*4       THE NUMBER OF OBSERVATIONS (CENSORED OR NOT)(INPUT)
C    X(N)     R*4       THE DATA (0.0 INDICATES CENSORED VALUE)
C    XD(N)    R*4       THE CENSORING THRESHOLD (IN REAL SPACE UNITS) (INPUT)
C    PMU      R*4       THE FITTED MLE FOR MU (INPUT)
C    PSIGMA   R*4       THE FITTED MLE FOR SIGMA (INPUT)
C    XMEAN    R*4       THE ESTIMATED POPULATION MEAN
C    XSE      R*4       THE ESTIMATED POPULATION STANDARD ERROR
C
C==============================================================================

      REAL*8 X(*),XD(*),PMU,PSIGMA,XMEAN,XSE
      INCLUDE 'fmodules.inc'
      REAL*8 THR,XSI,BIAS,SCOV,R,XSIMIN,XSIMAX,VARMIN,XMOM21,XMOM22
      DIMENSION NX(MAXOBSC),THR(MAXOBSC),XSI(MAXOBSC),BIAS(2,3),
     $     SCOV(2,2,3),R(2)
      DATA XSIMIN/-1.5D0/,XSIMAX/1.5D0/,VARMIN/0.000001D0/

      NT  =  0
      DO  2 I=1,NCT
         IF (XD(I).EQ.0.)  XD(I)=1D-10
         DO 3 K=NT,1,-1
            IF(XD(I) .EQ. THR(K)) THEN
               NX(K) =  NX(K)+1
               GOTO 2
            ENDIF
    3    CONTINUE
         NT        =  NT + 1
         NX(NT)     =  1
         THR(NT)   =  XD(I)
    2 CONTINUE
      
      DO 10 I=1,NT
         XSI(I) =  MIN(XSIMAX,MAX(XSIMIN,(LOG(THR(I))-PMU)/PSIGMA))
 10   CONTINUE
      CALL LN2ASYMN(NT,NX,XSI,BIAS,SCOV)
      
      R(1)      =  1.0
      R(2)      =  0.5
      CALL RBEST(PMU,PSIGMA,BIAS,SCOV,R,XMEAN)

      R(1)      =  2.0
      R(2)      =  1.0
      CALL RBEST(PMU,PSIGMA,BIAS,SCOV,R,XMOM21)
      
      R(1)      =  2.0
      R(2)      =  2.0
      CALL RBEST(PMU,PSIGMA,BIAS,SCOV,R,XMOM22)
      
      
      XSE  =  DSQRT(MAX(VARMIN,(XMOM22-XMOM21)))
      
      RETURN
      END
