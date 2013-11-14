************************************************************************
*
*     Function IDAMAX                                  Called by: DGEFA
*
*     find the index of element having maximum absolute value.  Based
*     on a LINPACK routine written by Jack Dongarra.
*
*     for the special case of INCX=1
*
************************************************************************
      INTEGER*4 FUNCTION IDAMAX(N,DX)
*
*     function arguments
*
      INTEGER*4 N
      DOUBLE PRECISION DX(*)
*
*     local vars
*
      DOUBLE PRECISION DMAX
      INTEGER*4 I,IX

      IDAMAX = 0
      DMAX = 0.D0
      DO 10 I=1,N
         IF (DABS(DX(I)).GT.DMAX) THEN
            IDAMAX = I
            DMAX = DABS(DX(I))
         ENDIF
   10 CONTINUE

      RETURN
      END
