************************************************************************
*
*     Function DASUM                                   Called by: FACINV
*
*     takes the sum of the absolute values.  Based on a LINPACK routine
*     written by Jack Dongarra.  Modified for the case of INCX=1.
*
************************************************************************
      DOUBLE PRECISION FUNCTION DASUM(NPAR,DX)
*
*     function arguments
*
      INTEGER*4 NPAR
      DOUBLE PRECISION DX(*)
*
*     local vars
*
      INTEGER*4 I,M
*
*     initialize
*
      DASUM = 0.D0
*
*     clean-up loop
*
      M = MOD(NPAR,6)

      DO 10 I=1,M
         DASUM = DASUM + DABS(DX(I))
 10   CONTINUE

      IF (M .NE. 0 .AND. NPAR .LT. 6) RETURN

      DO 20 I=M+1,NPAR,6
         DASUM = DASUM + DABS(DX(I)) + DABS(DX(I+1)) + DABS(DX(I+2))
     &                 + DABS(DX(I+3)) + DABS(DX(I+4)) + DABS(DX(I+5))
   20 CONTINUE

      RETURN
      END
