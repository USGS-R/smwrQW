************************************************************************
*
*     Subroutine DSLVMU                               Called by: DHUMIT
*
*     multiplies symmetric matrix times vector, given the lower
*     triangle of the matrix
*
C  ***  SET  Y = S * X,  S = N X N SYMMETRIC MATRIX.  
C  ***  LOWER TRIANGLE OF  S  STORED ROWWISE.         
*
************************************************************************
      SUBROUTINE DSLVMU(NP,Y,S,X)
*
*     subroutine arguments
*
      INTEGER*4 NP
      DOUBLE PRECISION S(*),X(*),Y(*)
*
*     local vars
*
      INTEGER*4 I,J,K
*
*     function declaration
*
      DOUBLE PRECISION DDOT
*
*     
*
      J = 1
      DO 10 I = 1,NP+1
         Y(I) = DDOT(I,S(J),X)
         J = J + I
 10   CONTINUE

      J = 1
      DO 40 I = 2,NP+1
         J = J + 1
         DO 30 K = 1,I-1
            Y(K) = Y(K) + S(J)*X(I)
            J = J + 1
 30      CONTINUE
 40   CONTINUE

      RETURN
      END
