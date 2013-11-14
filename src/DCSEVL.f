************************************************************************
*
*     Function DCSEVL                                   Called by: ERFC
*
*     Evaluate the N-term Chebyshev series CS at X.  Adapted from a
*     method presented in the paper by Broucke referenced below.
*
*     Routine is a modified version of the DCSEVL code retrieved from
*     NETLIB (www.netlib.org). 
*
*     Original author: W. Fullerton of Los Alamos National Lab
*
*
C       Input Arguments --
C  X    value at which the series is to be evaluated.
C  CS   array of N terms of a Chebyshev series.  In evaluating
C       CS, only half the first coefficient is summed.
C  N    number of terms in array CS.
C
C***REFERENCES  R. Broucke, Ten subroutines for the manipulation of
C                 Chebyshev series, Algorithm 446, Communications of
C                 the A.C.M. 16, (1973) pp. 254-256.
C               L. Fox and I. B. Parker, Chebyshev Polynomials in
C                 Numerical Analysis, Oxford University Press, 1968,
C                 page 56.
*
************************************************************************
      DOUBLE PRECISION FUNCTION DCSEVL(X,CS,N)
*
*     function arguments
*
      INTEGER*4 N
      DOUBLE PRECISION X,CS(*)
*
*     local vars
*
      INTEGER*4 I,NI
      DOUBLE PRECISION B0,B1,B2,TWOX
*
*     function declaration
*
      DOUBLE PRECISION D1MACH
*
*     error tests from LOADEST, N should be within range
*
C     LOADEST IF (N .LT. 1) CALL ERROR7(1)
C     LOADEST IF (N .GT. 1000) CALL ERROR7(2)
C     LOADEST IF (ABS(X) .GT. 1.D0 + D1MACH(4)) CALL ERROR7(3)

      B1 = 0.D0
      B0 = 0.D0
      TWOX = 2.D0*X
      DO 10 I = 1,N
         B2 = B1
         B1 = B0
         NI = N + 1 - I
         B0 = TWOX*B1 - B2 + CS(NI)
   10 CONTINUE

      DCSEVL = 0.5D0*(B0-B2)

      RETURN
      END
