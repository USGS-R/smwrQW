************************************************************************
*
*     Subroutine DLSQRT                               Called by: DGQTST
*
*     compute rows 1 through N of the Cholesky factor L of
*     A = L*(L**T), where L and the lower triangle of A are both
*     stored compactly by rows (and may occupy the same storage).
*
*     IRC = 0 means all went well.  IRC = J means the leading
*     principal JxJ submatrix of A is not positive definite -- and
*     L(J*(J+1)/2) contains the (nonpos.) reduced Jth diagonal.
*
************************************************************************
      SUBROUTINE DLSQRT(N,L,A,IRC)
*
*     subroutine arguments
*
      INTEGER*4 N,IRC
      DOUBLE PRECISION L(*),A(*)
*
*     local vars
*
      INTEGER*4 I,J,K,I0,J0
      DOUBLE PRECISION T,TD
*
*     
*
      I0 = 0
      DO 30 I=1,N
         TD = 0.D0
         J0 = 0
         DO 20 J=1,I-1
            T = 0.D0
            DO 10 K=1,J-1
               T = T + L(I0+K)*L(J0+K)
 10         CONTINUE
            J0 = J0 + J
            T = (A(I0+J) - T) / L(J0)
            L(I0+J) = T
            TD = TD + T*T
 20      CONTINUE
         I0 = I0 + I
         T = A(I0) - TD
         IF (T .LE. 0.D0) THEN
            L(I0) = T
            IRC = I
            RETURN
         ENDIF
         L(I0) = DSQRT(T)
 30   CONTINUE
      
      IRC = 0

      RETURN
      END
