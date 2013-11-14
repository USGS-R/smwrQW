**********************************************************************
*
*     Subroutine DLITVM                             Called by: DGQTST
*
*     solve (L**T)*X = Y, where L is an NxN lower triangular matrix
*     stored compactly by rows.  X and Y may occupy the same storage.
*
**********************************************************************
      SUBROUTINE DLITVM(N,X,L,Y)
*
*     subroutine arguments
*
      INTEGER*4 N
      DOUBLE PRECISION X(*),L(*),Y(*)
*
*     local vars
*
      INTEGER*4 I,II,I0,J
      DOUBLE PRECISION XI
*
*     
*
      DO 10 I = 1,N
         X(I) = Y(I)
 10   CONTINUE
      I0 = N*(N+1)/2
      DO 30 II = 1,N
         I = N + 1 - II
         XI = X(I)/L(I0)
         X(I) = XI
         IF (I .LE. 1) RETURN
         I0 = I0 - I
         DO 20 J=1,I-1
            X(J) = X(J) - XI*L(I0+J)
 20      CONTINUE
 30   CONTINUE

      RETURN
      END
