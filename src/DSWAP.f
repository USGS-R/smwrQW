************************************************************************
*
*     Subroutine DSWAP                                 Called by: FACINV
*
*     Interchange two vectors using unrolled loops.  Based on a LINPACK
*     routine by J. Dongarra.
*
*     for the special case of INCX=INCY=1
*
************************************************************************
      SUBROUTINE DSWAP(N,DX,DY)
*
*     subroutine arguments
*
      INTEGER*4 N
      DOUBLE PRECISION DX(*),DY(*)
*
*     local vars
*
      DOUBLE PRECISION DTEMP
      INTEGER*4 I,M
*
*     clean-up loop
*
      M = MOD(N,3)
      IF (M .EQ. 0) GOTO 40

      DO 30 I=1,M
         DTEMP = DX(I)
         DX(I) = DY(I)
         DY(I) = DTEMP
 30   CONTINUE

      IF (N .LT. 3) RETURN

 40   DO 50 I=M+1,N,3
         DTEMP = DX(I)
         DX(I) = DY(I)
         DY(I) = DTEMP
         DTEMP = DX(I+1)
         DX(I+1) = DY(I+1)
         DY(I+1) = DTEMP
         DTEMP = DX(I+2)
         DX(I+2) = DY(I+2)
         DY(I+2) = DTEMP
 50   CONTINUE
      
      RETURN
      END

