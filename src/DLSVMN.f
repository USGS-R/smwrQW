************************************************************************
*
*     Function DLSVMN                                 Called by: DGQTST
*
*     returns a good over-estimate of the smallest singular value of
*     the packed lower triangular matrix L.
*
C  NP (IN)  = THE ORDER OF L -1.  L IS A  NP+1 X NP+1  LOWER TRIANGULAR MATRIX.
C  L (IN)  = ARRAY HOLDING THE ELEMENTS OF  L  IN ROW ORDER, I.E.
C             L(1,1), L(2,1), L(2,2), L(3,1), L(3,2), L(3,3), ETC.
C  X (OUT) IF DLSVMN RETURNS A POSITIVE VALUE, THEN X IS A NORMALIZED
C             APPROXIMATE LEFT SINGULAR VECTOR CORRESPONDING TO THE
C             SMALLEST SINGULAR VALUE.  THIS APPROXIMATION MAY BE VERY
C             CRUDE.  IF DLSVMN RETURNS ZERO, THEN SOME COMPONENTS OF X
C             ARE ZERO AND THE REST RETAIN THEIR INPUT VALUES.
C  Y (OUT) IF DLSVMN RETURNS A POSITIVE VALUE, THEN Y = (L**-1)*X IS AN
C             UNNORMALIZED APPROXIMATE RIGHT SINGULAR VECTOR CORRESPOND-
C             ING TO THE SMALLEST SINGULAR VALUE.  THIS APPROXIMATION
C             MAY BE CRUDE.  IF DLSVMN RETURNS ZERO, THEN Y RETAINS ITS
C             INPUT VALUE.  THE CALLER MAY PASS THE SAME VECTOR FOR X
C             AND Y (NONSTANDARD FORTRAN USAGE), IN WHICH CASE Y OVER-
C             WRITES X (FOR NONZERO DLSVMN RETURNS).
C
C     THE ALGORITHM IS BASED ON (1), WITH THE ADDITIONAL PROVISION THAT
C     DLSVMN = 0 IS RETURNED IF THE SMALLEST DIAGONAL ELEMENT OF L
C     (IN MAGNITUDE) IS NOT MORE THAN THE UNIT ROUNDOFF TIMES THE
C     LARGEST.  THE ALGORITHM USES A RANDOM NUMBER GENERATOR PROPOSED
C     IN (4), WHICH PASSES THE SPECTRAL TEST WITH FLYING COLORS -- SEE
C     (2) AND (3).
C
C
C     (1) CLINE, A., MOLER, C., STEWART, G., AND WILKINSON, J.H.(1977),
C         AN ESTIMATE FOR THE CONDITION NUMBER OF A MATRIX, REPORT
C         TM-310, APPLIED MATH. DIV., ARGONNE NATIONAL LABORATORY.
C
C     (2) HOAGLIN, D.C. (1976), THEORETICAL PROPERTIES OF CONGRUENTIAL
C         RANDOM-NUMBER GENERATORS --  AN EMPIRICAL VIEW,
C         MEMORANDUM NS-340, DEPT. OF STATISTICS, HARVARD UNIV.
C
C     (3) KNUTH, D.E. (1969), THE ART OF COMPUTER PROGRAMMING, VOL. 2
C         (SEMINUMERICAL ALGORITHMS), ADDISON-WESLEY, READING, MASS.
C
C     (4) SMITH, C.S. (1971), MULTIPLICATIVE PSEUDO-RANDOM NUMBER
C         GENERATORS WITH PRIME MODULUS, J. ASSOC. COMPUT. MACH. 18,
C         PP. 586-593.
C
C
C     DESIGNED AND CODED BY DAVID M. GAY (WINTER 1977/SUMMER 1978).
C
C     THIS SUBROUTINE WAS WRITTEN IN CONNECTION WITH RESEARCH
C     SUPPORTED BY THE NATIONAL SCIENCE FOUNDATION UNDER GRANTS
C     MCS-7600324, DCR75-10143, 76-14311DSS, AND MCS76-11989.
*
************************************************************************
      DOUBLE PRECISION FUNCTION DLSVMN(NP,L,X,Y)
*
*     subroutine arguments
*
      INTEGER*4 NP
      DOUBLE PRECISION L(*),X(*),Y(*)
*
*     local variables
*
      INTEGER*4 I,II,IX,J,JJ,JJJ,JM1,J0
      DOUBLE PRECISION B,SMINUS,SPLUS,T,XMINUS,XPLUS
*
*     function declarations
*
      DOUBLE PRECISION DDOT,DNRM2
*
*     check whether to return DLSVMN = 0 and initialize X
*
      IX = 2
      II = 0
      J0 = (NP+1)*NP/2
      JJ = J0 + NP + 1
      IF (L(JJ) .EQ. 0.D0) THEN
         DLSVMN = 0.D0
         RETURN
      ENDIF
      IX = MOD(3432*IX, 9973)
      B = 0.5D0*(1.D0 + FLOAT(IX)/9973.D0)
      XPLUS = B / L(JJ)
      X(NP+1) = XPLUS

      DO 10 I=1,NP
         II = II + I
         IF (L(II) .EQ. 0.D0) THEN
            DLSVMN = 0.D0
            RETURN
         ENDIF
         X(I) = XPLUS * L(J0+I)
 10   CONTINUE
*
*     solve (L**T)*X = B, where the components of B have randomly
*     chosen magnitudes in (.5,1) with signs chosen to make X large
*
      DO 50 JJJ=1,NP
         J = NP + 1 - JJJ
*
*        determine X(J) in this iteration; for I = 1,2,...,J, X(I)
*        holds the current partial sum for row I
*
         IX = MOD(3432*IX, 9973)
         B = 0.5D0*(1.D0 + FLOAT(IX)/9973.D0)
         XPLUS = (B - X(J))
         XMINUS = (-B - X(J))
         SPLUS = DABS(XPLUS)
         SMINUS = DABS(XMINUS)
         JM1 = J - 1
         J0 = J*JM1/2
         JJ = J0 + J
         XPLUS = XPLUS/L(JJ)
         XMINUS = XMINUS/L(JJ)

         DO 20 I = 1,JM1
            SPLUS = SPLUS + DABS(X(I) + L(J0+I)*XPLUS)
            SMINUS = SMINUS + DABS(X(I) + L(J0+I)*XMINUS)
 20      CONTINUE

         IF (SMINUS .GT. SPLUS) XPLUS = XMINUS
         X(J) = XPLUS
*
*        update partial sums
*
         DO 30 I=1,JM1
            X(I) = XPLUS*L(J0+I) + X(I)
 30      CONTINUE

 50   CONTINUE
*
*     normalize X
*
      T = 1.D0/DNRM2(NP+1,X)
      DO 70 I = 1,NP+1
         X(I) = T*X(I)
 70   CONTINUE
*
*     solve L*Y = X and return DLSVMN = 1/TWONORM(Y)
*
      DO 100 J = 1,NP+1
         JM1 = J - 1
         J0 = J*JM1/2
         JJ = J0 + J
         T=0.D0
         IF(JM1.GT.0)T = DDOT(JM1,L(J0+1),Y)
         Y(J)=(X(J)-T)/L(JJ)
 100  CONTINUE

      DLSVMN = 1.D0/DNRM2(NP+1,Y)

      RETURN
      END
