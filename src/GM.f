      REAL*8 FUNCTION GM(ALPHA,BETA,ARG)
C==============================================================================
C
C     FUNCTION TO COMPUTE FINNEY'S [1941] 'GM'
C
C     PURPOSE: TO HELP IN COMPUTING NEARLY-UNBIASED ESTIMATORS FOR CENSORED
C              LOGNORMAL POPULATIONS, FROM SAMPLE STATISTICS
C
C              IF T IS DISTRIBUTED AS A GAMMA VARIATE WITH PARAMETERS
C              (ALPHA,BETA), THEN
C
C                   E[ GM(ARG) ]   =  EXP( E[ARG] )  =  EXP( ALPHA/BETA )
C
C     NOTE:    IN THE CASE OF NO CENSORING, AND A NORMAL POPULATION, S^2 IS
C              DISTRIBUTED AS A GAMMA VARIATE WITH PARAMETERS
C
C                   (ALPHA,BETA)   =  ( (N-1)/2,2/(N-1) )
C
C     AUTHOR.........TIM COHN
C     DATE...........SEPTEMBER 23, 1986
C
C
C     ALPHA    R*4       ALPHA OF A GAMMA POPULATION (INPUT)
C     BETA     R*4       BETA OF A GAMMA POPULATION (INPUT)
C     ARG      R*4       ARGUMENT
C
C     GM       R*4       VALUE OF FUNCTION
C
C==============================================================================

      DOUBLE PRECISION ALPHA,BETA,ARG
      DATA TOL/1.D-10/
      REAL*8 A,B,T,TERM,GMSUM,P
      INTEGER IP

      A        =  ALPHA
      B        =  BETA
      T        =  ARG

      BT       =  B*T

      GMSUM     =  1.D0
      TERM      =  1.D0
      DO 10 IP=1,50
         P         = DBLE(IP)
         TERM      =  TERM * BT/( (A+P-1.0D0) * P )
         IF(IP .GT. 1 .AND. TERM .LT. TOL)  GOTO 20
         GMSUM     =  GMSUM+TERM
 10   CONTINUE

 20   CONTINUE

      GM = GMSUM

      RETURN
      END
