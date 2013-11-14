************************************************************************
*
*     Subroutine TACIT_H                              Called by: DHUMSL
*
*     evaluate a particular parameter set with respect to linear
*     regression and Tobit models
*
C
C      NP     NUMBER OF PARAMETERS IN MODEL
C      Y      VECTOR OF DEPENDENT OBSERVATIONS
C      YD     VECTOR OF CENSORING THRESHOLDS; Y IS OBSERVED IFF Y>YD
C      X      MATRIX OF X.  NOTE THAT X(@,1) IS A VECTOR OF 1'S
C      B_MLE   VECTOR OF PARAMETER ESTIMATES
C      HF      VALUE OF THE L.F. GIVEN B_MLE,X,Y AND YD. (OUTPUT)
*
************************************************************************
      SUBROUTINE TACIT_H(B_MLE,HF,NOBSC,NP,X,Y,YD,CENSFLAG)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 NOBSC,NP
      LOGICAL CENSFLAG(*)
      DOUBLE PRECISION B_MLE(*),Y(*),YD(*),X(MAXOBSC,*),
     &                 HF(MAXPARMS+1,*)
*
*     local vars
*
      INTEGER*4 I,K,K2
      DOUBLE PRECISION S,S_2,XSI,A,B,GT2,Z
      DOUBLE PRECISION HT(MAXPARMS,MAXPARMS)
*
*     function declaration
*
      DOUBLE PRECISION PRED
*
*     
*
      S = EXP(B_MLE(NP+1))
      S_2 = (1.D0/(S*S))

      DO 20 K=1,NP+1
         DO 10 K2=1,NP+1
            HF(K,K2) = 0.D0
 10      CONTINUE
 20   CONTINUE

      DO 50 I=1,NOBSC
         IF (CENSFLAG(I)) THEN
            XSI = (YD(I)-PRED(NP,MAXOBSC,I,X,B_MLE))/S
            CALL TACIT_CALC(XSI,A)
            B = -A*(XSI+A)
            GT2 = -XSI*A/S
            HT(1,1) = S_2*B
            HT(1,2) = S_2*(A+XSI*B)
            HT(2,2) = XSI*S_2*(2.D0*A + XSI*B)
         ELSE
            Z = (Y(I)-PRED(NP,MAXOBSC,I,X,B_MLE))/S
            GT2 = (Z**2-1.D0)/S
            HT(1,1) = -S_2
            HT(1,2) = -S_2*(2.D0*Z)
            HT(2,2) = -S_2*(3.D0*Z**2-1.D0)
         ENDIF
*
*        construct negatives for IMSL routine DUMIAH (a minimizer)
*
         DO 40 K2=1,NP
            HF(NP+1,K2) = HF(NP+1,K2) - S*HT(1,2)*X(I,K2)
            HF(K2,NP+1) = HF(NP+1,K2)
            DO 30 K=1,NP
               HF(K,K2) = HF(K,K2) - HT(1,1)*X(I,K)*X(I,K2)
 30         CONTINUE
 40      CONTINUE
         HF(NP+1,NP+1) =  HF(NP+1,NP+1) - S*(GT2 + S*HT(2,2))
 50   CONTINUE

      RETURN
      END
