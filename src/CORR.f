************************************************************************
*
*     Function CORR                  Called by: AMLSTAT, OUTEQN, OUTRES
*
*     compute correlation between two variables
*
************************************************************************
      DOUBLE PRECISION FUNCTION CORR(N,X,Y)
*
*     function args
*
      INTEGER*4 N
      DOUBLE PRECISION X(*),Y(*)
*
*     local vars
*
      INTEGER*4 I
      DOUBLE PRECISION C1,MX,MY,SX,SY
*
*     
*
      MX = 0.D0
      MY = 0.D0

      DO 10 I=1,N
        MX = MX+X(I)
        MY = MY+Y(I)
   10 CONTINUE

      MX = MX/DBLE(N)
      MY = MY/DBLE(N)
      C1 = 0.D0
      SX = 0.D0
      SY = 0.D0

      DO 20 I=1,N
        C1 = C1+(X(I)-MX)*(Y(I)-MY)
        SX = SX+(X(I)-MX)**2
        SY = SY+(Y(I)-MY)**2
   20 CONTINUE
  
      CORR = C1/SQRT(MAX(1.D-34,SX*SY))
  
      RETURN
      END
