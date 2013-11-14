************************************************************************
*
*     Subroutine TACIT_G                              Called by: DHUMSL 
*
*     evaluate a particular parameter set with respect to linear
*     regression and Tobit models.
*
C      NP      NUMBER OF PARAMETERS IN MODEL
C      Y       VECTOR OF DEPENDENT OBSERVATIONS
C      YD      VECTOR OF CENSORING THRESHOLDS; Y IS OBSERVED IFF Y>YD
C      X       MATRIX OF X.  NOTE THAT X(@,1) IS A VECTOR OF 1'S
C      B_MLE    VECTOR OF PARAMETER ESTIMATES
C      G       VALUE OF THE L.F. GIVEN B_MLE,X,Y AND YD. (OUTPUT)
*
************************************************************************
      SUBROUTINE TACIT_G(B_MLE,G,NOBSC,NP,X,Y,YD,CENSFLAG)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 NOBSC,NP
      LOGICAL CENSFLAG(*)
      DOUBLE PRECISION B_MLE(*),G(*),Y(*),YD(*),X(MAXOBSC,*)
*
*     local vars
*
      INTEGER*4 I,K
      DOUBLE PRECISION S,XSI,A,Z,GT(2)
*
*     function declaration
*
      DOUBLE PRECISION PRED
*
*     
*
      S = EXP(B_MLE(NP+1))
      
      DO 10 I=1,NP+1
         G(I) = 0.D0
 10   CONTINUE
      
      DO 30 I=1,NOBSC
         IF(CENSFLAG(I)) THEN
            XSI = (YD(I)-PRED(NP,MAXOBSC,I,X,B_MLE))/S
            CALL TACIT_CALC(XSI,A)
            GT(1) = -A/S
            GT(2) = -XSI*A/S
         ELSE
            Z = (Y(I)-PRED(NP,MAXOBSC,I,X,B_MLE))/S
            GT(1) = Z/S
            GT(2) = (Z**2-1.D0)/S
         ENDIF
*
*        construct negatives for IMSL routine DUMIAH (a minimizer);
*        since we are optimizing with EXP(S), must adjust derivs.
*        
         DO 20 K=1,NP
            G(K) = G(K) - GT(1)*X(I,K)
 20      CONTINUE
         G(NP+1) = G(NP+1) - S*GT(2)
 30   CONTINUE

      RETURN
      END
