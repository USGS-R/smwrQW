************************************************************************
*
*     Subroutine DHUMIT                               Called by: DHUMSL
*
*     carry out unconstrained minimization iterations, using Hessian
*     matrix provided by DHUMSL
*
*
C
C D.... SCALE VECTOR.
C FX... FUNCTION VALUE.
C G.... GRADIENT VECTOR.
C H.... LOWER TRIANGLE OF THE HESSIAN, STORED ROWWISE.
C IV... INTEGER VALUE ARRAY.
C LIV.. LENGTH OF IV (AT LEAST 60).
C LV... LENGTH OF V (AT LEAST 78 + N*(N+21)/2).
C N.... NUMBER OF VARIABLES (COMPONENTS IN B_MLE AND G).
C V.... FLOATING-POINT VALUE ARRAY.
C B_MLE.... PARAMETER VECTOR.
C
C
C        PARAMETERS IV, N, V, AND B_MLE ARE THE SAME AS THE CORRESPONDING
C     ONES TO DHUMSL (WHICH SEE), EXCEPT THAT V CAN BE SHORTER (SINCE
C     THE PART OF V THAT DHUMSL USES FOR STORING G AND H IS NOT NEEDED).
C     MOREOVER, COMPARED WITH DHUMSL, IV(1) MAY HAVE THE TWO ADDITIONAL
C     OUTPUT VALUES 1 AND 2, WHICH ARE EXPLAINED BELOW, AS IS THE USE
C     OF IV(2) AND IV(7).  THE VALUE IV(G), WHICH IS AN
C     OUTPUT VALUE FROM DHUMSL, IS NOT REFERENCED BY DHUMIT OR THE
C     SUBROUTINES IT CALLS.
C
C IV(1) = 1 MEANS THE CALLER SHOULD SET FX TO F(B_MLE), THE FUNCTION VALUE
C             AT B_MLE, AND CALL DHUMIT AGAIN, HAVING CHANGED NONE OF THE
C             OTHER PARAMETERS.  AN EXCEPTION OCCURS IF F(B_MLE) CANNOT BE
C             COMPUTED (E.G. IF OVERFLOW WOULD OCCUR), WHICH MAY HAPPEN
C             BECAUSE OF AN OVERSIZED STEP.  IN THIS CASE THE CALLER
C             SHOULD SET IV(2) = IV(2) TO 1, WHICH WILL CAUSE
C             DHUMIT TO IGNORE FX AND TRY A SMALLER STEP.  THE PARA-
C             METER NF THAT DHUMSL PASSES TO CALCF (FOR POSSIBLE USE BY
C             CALCGH) IS A COPY OF IV(6) = IV(6).
C IV(1) = 2 MEANS THE CALLER SHOULD SET G TO G(B_MLE), THE GRADIENT OF F AT
C             B_MLE, AND H TO THE LOWER TRIANGLE OF H(B_MLE), THE HESSIAN OF F
C             AT B_MLE, AND CALL DHUMIT AGAIN, HAVING CHANGED NONE OF THE
C             OTHER PARAMETERS EXCEPT PERHAPS THE SCALE VECTOR D.
C                  THE PARAMETER NF THAT DHUMSL PASSES TO CALCG IS
C             IV(7) = IV(7).  IF G(B_MLE) AND H(B_MLE) CANNOT BE EVALUATED,
C             THEN THE CALLER MAY SET IV(7) TO 0, IN WHICH CASE
C             DHUMIT WILL RETURN WITH IV(1) = 65.
C                  NOTE -- DHUMIT OVERWRITES H WITH THE LOWER TRIANGLE
C             OF  DIAG(D)**-1 * H(B_MLE) * DIAG(D)**-1.
C
C     CODED BY DAVID M. GAY (WINTER 1980).  REVISED SEPT. 1982.
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH SUPPORTED
C     IN PART BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS
C     MCS-7600324 AND MCS-7906671.
C
C        (SEE DSUMSL AND DHUMSL FOR REFERENCES.)
*
************************************************************************
      SUBROUTINE DHUMIT(D,FX,G,H,IV,LV,NP,V,B_MLE)
*
*     subroutine arguments
*
      INTEGER*4 LV,NP,IV(*)
      DOUBLE PRECISION FX,D(*),G(*),H(*),V(*),B_MLE(*)
*
*     local vars
*
      INTEGER*4 I,J,K,LSTGST,STEP1,TEMP1,X01
      DOUBLE PRECISION T
*
*     function declarations
*
      DOUBLE PRECISION DDOT,DNRM2
*
*     check validity of IV and V input values
*
      IF (IV(1) .EQ. 1) THEN
         V(10) = FX
         IF (IV(35) .GE. 0) GOTO 200
         IV(1) = 2
         IF (IV(2) .EQ. 0) RETURN
         IV(1) = 63
         RETURN
      ENDIF
      IF (IV(1) .EQ. 2) GOTO 40
      IV(4) = IV(4) + (NP+1)*(NP+22)/2 + 7
      CALL DPARCK(D,IV,LV,NP,V)
*
*     transfer
*
      IF (IV(1)-2 .GT. 12) RETURN
      GOTO (200,200,200,200,200,200,160,120,160,10,10,20), IV(1)-2 
      IV(1) = 66
      RETURN
*
*     storage allocation
*
 10   IV(59) = IV(42) + ((NP+1) * (NP+2)/2)
      IV(43) = IV(59) + 2*(NP+1)
      IV(40) = IV(43) + NP + 1
      IV(41) = IV(40) + NP + 1
      IV(37) = IV(41) + NP + 1
      IV(34) = IV(37) + NP + 1
      IV(47) = IV(34) + 4*(NP+1) + 7
      IF (IV(1) .EQ. 13) THEN
         IV(1) = 14
         RETURN
      ENDIF
*
*     initializatize
*     
 20   IV(2) = 0
      IV(5) = 1
      IV(6) = 1
      IV(7) = 1
      IV(8) = 0
      IV(11) = 1
      IV(30) = 1
      IV(31) = 0
      IV(35) = -1
      IV(55) = 0
      V(9) = 0.D0
      V(5) = 0.D0
      IF (V(38) .GE. 0.D0) CALL DVSCPY(NP,D,V(38))
      K = IV(59)
      IF (V(39) .GT. 0.D0) CALL DVSCPY(NP,V(K),V(39))
      K = K + NP + 1
      IF (V(40) .GT. 0.D0) CALL DVSCPY(NP,V(K),V(40))
      IV(1) = 1
      RETURN
*
*     make sure gradient could be computed
*
 40   IF (IV(7) .EQ. 0) THEN
         IV(1) = 65
         RETURN
      ENDIF
*
*     update the scale vector D (DDUPDU)
*
 50   IF (IV(16) .GT. 0) THEN
         K = IV(37)
         J = 0
         DO 60 I=1,NP+1
            J = J + I
            V(K) = H(J)
            K = K + 1
 60      CONTINUE
         CALL DDUPDU(D,V(IV(37)),IV,NP,V)
      ENDIF
*
*     compute scaled gradient and its norm
*
 70   K = IV(37)
      DO 80 I=1,NP+1
         V(K) = G(I) / D(I)
         K = K + 1
 80   CONTINUE
      V(1) = DNRM2(NP+1,V(IV(37)))
*
*     compute scaled hessian
*
      K = 1
      DO 100 I=1,NP+1
         T = 1.D0 / D(I)
         DO 90 J=1,I
            H(K) = T * H(K) / D(J)
            K = K + 1
 90      CONTINUE
 100  CONTINUE

      IF (IV(55) .NE. 0) THEN
         IV(1) = IV(55)
         IV(55) = 0
         RETURN
      ENDIF

      IF (IV(35) .EQ. 0) GOTO 280
*
*     allow first step to have scaled 2-norm at most V(35)
*
      V(8) = V(35)
      IV(35) = 0
*
*     main loop; check iteration limit
*
 120  K = IV(31)
      IF (K .GE. IV(18)) THEN
         IV(1) = 10
         RETURN
      ENDIF
*
*     initialize for start of next iteration
*
 130  IV(31) = K + 1
      X01 = IV(43)
      V(13) = V(10)
      IV(29) = 4
      IV(33) = -1
*
*     copy B_MLE to X0
*
      CALL DCOPY(NP+1,B_MLE,V(X01))
*
*     update radius
*
      IF (K .EQ. 0) GOTO 170
      STEP1 = IV(40)
      K = STEP1
      DO 140 I=1,NP+1
         V(K) = D(I) * V(K)
         K = K + 1
 140  CONTINUE
      V(8) = V(16) * DNRM2(NP+1,V(STEP1))
*
*     come here when restarting after func. eval. limit
*
 160  IF (V(10) .LT. V(13)) THEN
         V(16) = 1.D0
         K = IV(31)
         GOTO 130
      ENDIF

 170  IF (IV(6) .GE. IV(17)) THEN
         IV(1) = 9
         IF (V(10) .GE. V(13)) RETURN
*
*        in case of function evaluation limit with improved V(10),
*        evaluate the gradient at B_MLE
*
         IV(55) = IV(1)
         IV(30) = IV(30) + 1
         IV(1) = 2
         RETURN
      ENDIF
*
*     compute candidate step (DGQTST computes optimally locally
*     constrained step.)
*
 190  STEP1 = IV(40)
      CALL DGQTST(D,V(IV(37)),H,IV(33),V(IV(42)),NP,V(STEP1),V,
     &            V(IV(34)))
*
*     compute F(X0 + STEP)
*
      IF (IV(29) .NE. 6) THEN
         X01 = IV(43)
         STEP1 = IV(40)
         DO 195 I = 1,NP+1
            B_MLE(I) = V(STEP1-1+I) + V(X01-1+I)
 195     CONTINUE
         IV(6) = IV(6) + 1
         IV(1) = 1
         IV(2) = 0
         RETURN
      ENDIF
*
*     assess candidate step (DASSST)
*
 200  STEP1 = IV(40)
      LSTGST = IV(41)
      X01 = IV(43)
      CALL DASSST(D,IV,NP,V(STEP1),V(LSTGST),V,B_MLE,V(X01))

      K = IV(29)
      GOTO (210,240,240,240,210,220,230,230,230,230,230,230,320,280),
     &       K
*
*     recompute step with new radius
*
 210  V(8) = V(16) * V(2)
      GOTO 170
*
*     compute step of length V(35) for singular convergence test
*
 220  V(8) = V(35)
      GOTO 190
*
*     convergence or false convergence
*
 230  IV(55) = K - 4
      IF (V(10) .GE. V(13) .OR. IV(13) .EQ. 14) THEN
         IV(1) = IV(55)
         IV(55) = 0
         RETURN
      ENDIF
      IV(13) = 14
*
*     process acceptable step
*
 240  IF (IV(29) .EQ. 3) THEN
         TEMP1 = LSTGST
*
*        prepare for gradient tests
*        
*        set TEMP1 = HESSIAN * STEP + G(43)
*        = DIAG(D) * (H * STEP + G(43))
*
*        use X0 vector as temporary.
*
         K = X01
         DO 250 I=1,NP+1
            V(K) = D(I) * V(STEP1)
            K = K + 1
            STEP1 = STEP1 + 1
 250     CONTINUE
         CALL DSLVMU(NP,V(TEMP1),H,V(X01))
         DO 260 I=1,NP+1
            V(TEMP1) = D(I) * V(TEMP1) + G(I)
            TEMP1 = TEMP1 + 1
 260     CONTINUE
      ENDIF
*
*     compute gradient and Hessian
*
      IV(30) = IV(30) + 1
      IV(1) = 2
      RETURN

 280  IV(1) = 2
      IF (IV(29) .NE. 3) GOTO 120
*
*     set V(16) by gradient tests
*
      TEMP1 = IV(41)
      STEP1 = IV(40)
*
*     set TEMP1 = DIAG(D)**-1 * (HESSIAN*STEP + (G(43)-G(B_MLE)))
*
      K = TEMP1
      DO 290 I=1,NP+1
         V(K) = (V(K) - G(I)) / D(I)
         K = K + 1
 290  CONTINUE
*
*     do gradient tests
*
      IF (DNRM2(NP+1,V(TEMP1)).GT.V(1)*V(29)) THEN
         IF (DDOT(NP+1,G,V(STEP1)) .GE. V(4)*V(30))  GOTO 120
      ENDIF
 300  V(16) = V(23)
      GOTO 120
*
*     misc. details, bad parameters to assess
*
*
 320  IV(1) = IV(55)
      IV(55) = 0

      RETURN
      END

