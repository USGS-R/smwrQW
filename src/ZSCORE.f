************************************************************************
*
*     Function ZSCORE                       Called by: NORTEST, OUTRES
*
*     calculate ZSCORE, the Z-Score for a given area (AREA) under the
*     standard normal curve.  Uses approximation given in Section
*     26.2.23 of Abramowitz and Stegun (1964, p. 933).  The error in
*     the approximation is less than 4.5e-4. 
*
*     This routine assumes:
*
*          0 < AREA < 1
*
*     which is always the case when ZSCORE is called form NORTEST and
*     OUTRES.
*
*     local vars
*     ----------
*     AREA2    used to preserve the value of AREA when AREA > 0.5
*     C0       constant used in approximation
*     C1       constant used in approximation
*     C2       constant used in approximation
*     D1       constant used in approximation
*     D2       constant used in approximation
*     D3       constant used in approximation
*     T        calculated value used in approximation
*
************************************************************************
      DOUBLE PRECISION FUNCTION ZSCORE(AREA)
*
*     subroutine args
*
      DOUBLE PRECISION AREA
*
*     local vars
*
      DOUBLE PRECISION AREA2,C0,C1,C2,D1,D2,D3,T
      PARAMETER (C0=2.515517D0,C1=0.802853D0,C2=0.010328D0)
      PARAMETER (D1=1.432788D0,D2=0.189269D0,D3=0.001308D0)

      IF (AREA .LE. 0.5D0) THEN
         T = DSQRT(DLOG(1.D0/(AREA**2)))
         ZSCORE = -(T-((C0+T*(C1+T*C2))/(1.D0+(T*(D1+T*(D2+T*D3))))))
      ELSE
         AREA2 = 1.D0 - AREA
         T = DSQRT(DLOG(1.D0/(AREA2*AREA2)))
         ZSCORE = T-((C0+T*(C1+T*C2))/(1.D0+(T*(D1+T*(D2+T*D3)))))
      ENDIF

      RETURN
      END
