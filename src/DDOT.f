************************************************************************
*
*     Function DDOT           Called by: DGQTST, DHUMIT, DLIVMU, DLSVMN,
*                                        DSLVMU, FACINV
*
*     form the dot product of DX and DY
*
*     based on linpack routine (j dongarra,3/11/78) that uses unrolled
*     loops for increments equal to one. Modified for the special case
*     of contiguous DX and DY (INCX=INCY=1).
*
************************************************************************
      DOUBLE PRECISION FUNCTION DDOT(N,DX,DY)      
*
*     function arguments
*
      INTEGER*4 N
      DOUBLE PRECISION DX(*),DY(*)
*
*     local vars
*
      INTEGER*4 I,M
*
*     code for both increments equal to 1; clean-up loop so remaining
*     vector length is a multiple of 5.
*
      DDOT = 0.D0
      IF (N.LE.0) RETURN
      M = MOD(N,5)
      IF (M.NE.0) THEN
         DO 10 I = 1,M
            DDOT = DDOT + DX(I)*DY(I)
 10      CONTINUE
         IF (N .LT. 5) RETURN
      ENDIF

      DO 20 I = M+1,N,5
         DDOT = DDOT + DX(I)*DY(I) + DX(I+1)*DY(I+1)
     &        + DX(I+2)*DY(I+2) + DX(I+3)*DY(I+3) + DX(I+4)*DY(I+4)
   20 CONTINUE

      RETURN
      END
