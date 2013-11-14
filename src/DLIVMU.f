************************************************************************
*
*     Subroutine DLIVMU                               Called by: DGQTST
*
*     solve L*X = Y, where L is an NxN lower triangular matrix stored
*     compactly by rows.  X and Y may occupy the same storage.
*
************************************************************************
      SUBROUTINE DLIVMU(N,X,L,Y)
*
*     subroutine arguments
*
      INTEGER*4 N
      DOUBLE PRECISION X(*),L(*),Y(*)
*
*     local vars
*
      INTEGER*4 I,J,K
      DOUBLE PRECISION T
*
*     function declaration
*
      DOUBLE PRECISION DDOT
*
*     
*
      DO 10 K=1,N
         IF (Y(K) .NE. 0.D0) GOTO 20
         X(K) = 0.D0
 10   CONTINUE
      RETURN
 20   J = K*(K+1)/2
      X(K) = Y(K) / L(J)
      IF (K .GE. N) RETURN
      K = K + 1
      DO 30 I = K,N
         T = DDOT(I-1,L(J+1),X)
         J = J + I
         X(I) = (Y(I) - T)/L(J)
 30   CONTINUE

      RETURN
      END
