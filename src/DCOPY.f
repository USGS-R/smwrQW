************************************************************************
*
*     Subroutine DCOPY                 Called by: DASSST, DHUMIT
*
*     copy a vector, x, to a vector, y, using unrolled loops.  Based on
*     a LINPACK routine written by Jack Dongarra.
*
*     modified for the special case of INCY=INCX=1
*
************************************************************************
      SUBROUTINE DCOPY(N,DX,DY)
*
*     subroutine arguments
*
      INTEGER*4 N
      DOUBLE PRECISION DX(*),DY(*)
*
*     local vars
*
      INTEGER*4 I,M
*
*     code for both increments equal to 1; clean-up loop so remaining
*     vector length is a multiple of 7
*
      IF (N.LE.0) RETURN

      M = MOD(N,7)

      IF (M .NE. 0) THEN
         DO 10 I = 1,M
            DY(I) = DX(I)
 10      CONTINUE
         IF (N .LT. 7) RETURN
      ENDIF
      
      DO 20 I = M+1,N,7
        DY(I) = DX(I)
        DY(I+1) = DX(I+1)
        DY(I+2) = DX(I+2)
        DY(I+3) = DX(I+3)
        DY(I+4) = DX(I+4)
        DY(I+5) = DX(I+5)
        DY(I+6) = DX(I+6)
   20 CONTINUE

      RETURN
      END
