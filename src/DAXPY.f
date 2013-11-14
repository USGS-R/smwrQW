************************************************************************
*
*     Subroutine DAXPY      Called by: DGEFA, FACINV
*
*     constant times a vector plus a vector.  Based on a LINPACK routine
*     written by Jack Dongarra.  Uses unrolled loops. 
*
*     for the special case of INCX=INCY=1
*
************************************************************************
      SUBROUTINE DAXPY(N,DA,DX,DY)
*
*     subroutine arguments
*  
      INTEGER*4 N
      DOUBLE PRECISION DA,DX(*),DY(*)
*
*     local vars
*
      INTEGER*4 I,M
*
*     
*
      IF (N.LE.0) RETURN
      IF (DA .EQ. 0.D0) RETURN

 20   M = MOD(N,4)
      IF (M .EQ. 0) GOTO 40
      DO 30 I = 1,M
         DY(I) = DY(I) + DA*DX(I)
 30   CONTINUE
      IF (N .LT. 4) RETURN
 40   DO 50 I = M+1,N,4
         DY(I) = DY(I) + DA*DX(I)
         DY(I + 1) = DY(I + 1) + DA*DX(I + 1)
         DY(I + 2) = DY(I + 2) + DA*DX(I + 2)
         DY(I + 3) = DY(I + 3) + DA*DX(I + 3)
 50   CONTINUE

      RETURN
      END
