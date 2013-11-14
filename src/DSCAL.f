************************************************************************
*
*     Subroutine DSCAL            Called by: DGEFA, FACINV
* 
*     scales a vector by a constant, using unrolled loops. Based on a
*     LINPACK routine by J. Dongarra.
*
*     for the special case of INCX=1
*
************************************************************************
      SUBROUTINE DSCAL(N,DA,DX) 
*
*     subroutine arguments
*
      INTEGER*4 N
      DOUBLE PRECISION DA,DX(*)
*
*     local vars
*
      INTEGER*4 I,M
*
*     
*
      IF (N.LE.0) RETURN

      M = MOD(N,5)

      IF (M .EQ. 0) GOTO 40

      DO 30 I=1,M
        DX(I) = DA*DX(I)
   30 CONTINUE

      IF (N .LT. 5) RETURN

 40   DO 50 I = M+1,N,5
        DX(I) = DA*DX(I)
        DX(I + 1) = DA*DX(I + 1)
        DX(I + 2) = DA*DX(I + 2)
        DX(I + 3) = DA*DX(I + 3)
        DX(I + 4) = DA*DX(I + 4)
   50 CONTINUE

      RETURN
      END
