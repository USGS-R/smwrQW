************************************************************************
*
*     Subroutine TACIT_L                     Called by: DHUMSL, TACIT_R 
*
*     evaluate a particular parameter set with respect to linear
*     regression and Tobit models
*
C
C      NP       NUMBER OF PARAMETERS IN MODEL
C      Y        VECTOR OF DEPENDENT OBSERVATIONS
C      YD       VECTOR OF CENSORING THRESHOLDS; Y IS OBSERVED IFF Y>YD
C      X        MATRIX OF X.  NOTE THAT X(@,1) IS A VECTOR OF 1'S
C      B_MLE     VECTOR OF PARAMETER ESTIMATES
C      LK        VALUE OF THE L.F. GIVEN B_MLE,X,Y AND YD
*
************************************************************************
      SUBROUTINE TACIT_L(B_MLE,LK,NOBSC,NP,X,Y,YD,CENSFLAG)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 NOBSC,NP
      LOGICAL CENSFLAG(*)
      DOUBLE PRECISION LK,XSI,B_MLE(*),Y(*),YD(*),X(MAXOBSC,*)
*
*     local vars
*
      INTEGER*4 I
      DOUBLE PRECISION LT,S,XLL
*
*     function declaration
*
      DOUBLE PRECISION ERFC,PRED
*
*
*
      LK = 0.D0
      S = EXP(B_MLE(NP+1))
C     This computes the 'correct' log-lik
      XLL = -.9189385332046727D0 - B_MLE(NP+1)
      DO 10 I=1,NOBSC
         IF (CENSFLAG(I)) THEN
            XSI = (YD(I)-PRED(NP,MAXOBSC,I,X,B_MLE))/S
            IF (XSI .LT. -6.D0) THEN
               LT = LOG(-0.3989422D0/XSI) - 0.5*XSI**2
            ELSEIF (XSI .GT. 6.D0) THEN
               LT = 0.D0
            ELSE
               LT = LOG(1.D0 - ERFC(XSI*0.7071068)/2.D0)
            ENDIF
         ELSE
            LT = XLL - ((Y(I)-PRED(NP,MAXOBSC,I,X,B_MLE))/S)**2/2.D0
         ENDIF
*
*        construct negatives (LK) for IMSL routine DUMIAH (a minimizer)
*
         LK = LK - LT
 10   CONTINUE

      RETURN
      END
