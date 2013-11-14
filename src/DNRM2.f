************************************************************************
*
*     Function DNRM2              Called by: DGQTST,DHUMIT,DLSVMN
*
*     for special case of INCX=1
*
*     return the euclidean norm of a vector, so that
*
*     DNRM2 := sqrt( x'*x )
*
*
*  -- This version written on 25-October-1982.
*     Modified on 14-October-1993 to inline the call to DLASSQ.
*     Sven Hammarling, Nag Ltd.
*
************************************************************************
      DOUBLE PRECISION FUNCTION DNRM2(N,X)
*
*     function arguments
*
      INTEGER*4 N
      DOUBLE PRECISION X(*)
*
*     local vars
*
      INTEGER*4 I
      DOUBLE PRECISION ABSXI,SCALE,SSQ
*
*     
*
      IF (N.LT.1) THEN
         DNRM2 = 0.D0
      ELSEIF (N.EQ.1) THEN
         DNRM2 = ABS(X(1))
      ELSE
         SCALE = 0.D0
         SSQ = 1.D0
*
*        The following loop is equivalent to this call in the LAPACK
*        auxiliary routine: CALL DLASSQ(N,X,INCX,SCALE,SSQ)
*
         DO 10 I=1,1+(N-1),1
            IF (X(I).NE.0.D0) THEN
               ABSXI = ABS(X(I))
               IF (SCALE.LT.ABSXI) THEN
                  SSQ = 1.D0 + SSQ*(SCALE/ABSXI)**2
                  SCALE = ABSXI
               ELSE
                  SSQ = SSQ + (ABSXI/SCALE)**2
               ENDIF
            ENDIF
   10    CONTINUE
         DNRM2  = SCALE * SQRT(SSQ)
      ENDIF

      RETURN
      END
