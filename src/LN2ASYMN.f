      SUBROUTINE LN2ASYMN(NT,NX,XSI,BI,V)

C===============================================================================
C
C        SUBROUTINE LN2ASYMN
C
C===============================================================================
C
C        AUTHOR:   TIM COHN
C        DATE:     SEPTEMBER 19, 1986
C             REVISED:  SEPTEMBER 22, 1986
C
C===============================================================================
C
C        PROGRAM TO COMPUTE THE FIRST-ORDER BIAS AND VARIANCE OF MLE PARAMETER
C        ESTIMATORS FOR THE 2-PARAMETER LOGNORMAL DISTRIBUTION (LN2) GIVEN DATA
C        CENSORED AT MULTIPLE THRESHOLDS.
C
C        THIS MIGHT BE USEFUL FOR DEALING WITH 'NO-DETECTS' IN WATER QUALITY
C        SAMPLING EXPERIMENTS.
C
C        THE METHOD EMPLOYED HERE APPEARS IN "SHENTON & BOWMAN, 1977: MAXIMUM
C        LIKELIHOOD IN SMALL SAMPLES." (EQ. 3.12B, P. 68)
C
C        NOTE:  THIS METHOD IS AN EXTENTION OF LN2ASYM, WHICH DEALS ONLY WITH
C               THE COVARIANCES AND BIASES FOR MU AND SIGMA.  THIS ROUTINE
C               PROVIDES RESULTS FOR SIGMA2, AND SIGMA4
C
C
C===============================================================================
C
C        VARIABLE  TYPE      DEFINITION
C        --------  ----      ----------
C        NT        I*4       NUMBER OF THRESHOLDS (INPUT)
C        NX(NT)    I*4       VECTOR CONTAINING THE NUMBER OF OBSERVATIONS THAT
C                              WERE CENSORED AT EACH THRESHOLD (INPUT)
C        XSI(NT)   R*4       VECTOR CONTAINING THE NON-DIMENSIONAL DETECTION
C                              LIMIT FOR EACH THRESHOLD (INPUT)
C        BI(2,3)   R*4       MATRIX CONTAINING THE NON-DIMENSIONAL FIRST-ORDER
C                              BIAS IN THE PARAMETERS:
C                                      B(1,1) =BIAS(MU)/SIGMA
C                                      B(2,1) =BIAS(SIGMA)/SIGMA
C        V(2,2,3)  R*4       MATRIX CONTAINING THE NON-DIMENSIONAL ASYMPTOTIC
C                              COVARIANCE OF THE PARAMETERS:
C                                      V(1,1,1)=VAR(MU)/SIGMA^2
C                                      V(2,2,1)=VAR(SIGMA^2)/SIGMA^2
C                                      V(1,2,1)=V(2,1,1)=COV(MU,SIGMA)/SIGMA^2
C
C                                      V(2,2,2)=VAR(SIGMA^2)/SIGMA^4
C                                      V(2,2,3)=VAR(SIGMA^4)/SIGMA^8 ETC...
C
C===============================================================================
C
      REAL*8 XSI, BI, V
      DIMENSION BI(2,3),V(2,2,3),XSI(*)
      
      REAL*8 K2(2,2),  K3(2,2,2),  K3S(2,2,2),
     1      S2(2,2),  S3(2,2,2),  S3S(2,2,2),
     2      A2(2,2,3),A3(2,2,2,3),A3S(2,2,2,3), CONSTANT
      REAL*8 CUMPHI,XS,P,F,A,B,E,H,OP,DET
      EQUIVALENCE (A2,S2),(A3,S3),(A3S,S3S)
      
      INTEGER NX(*)
      
      DATA CONSTANT/0.398942280401433D0/
         DO 10 I=1,2
              DO 10 J=1,2
                       S2(I,J)        =  0.0
                       K2(I,J)        =  0.0
                   DO 10 K=1,2
                       S3 (I,J,K)     =  0.0
                       S3S(I,J,K)     =  0.0
                       K3 (I,J,K)     =  0.0
                       K3S(I,J,K)     =  0.0
   10    CONTINUE

       DO 1 IT = 1,NT
         XS   =  XSI(IT)
         N    =  NX(IT)

         p = CUMPHI(XS)

       OP  =  1.-P

       F  =  CONSTANT*EXP(-XS**2/2.)

       A  =  F/P
       B  =  -A*(XS+A)
       E  =  -(B*(XS+A)+(1.+B)*A)
       H  =  1.+(F*XS)/OP

       K2(1,1)   =   N*(-OP+P*B)
       K2(1,2)   =   N*(-2.*F+P*(XS*B+A))
       K2(2,2)   =   N*(OP*(1.-3.*H)+P*(XS**2*B+2.*XS*A))

       K3(1,1,1) =  N*(-P*E)
       K3(1,1,2) =  N*(OP*2.-P*(XS*E+2.*B))
       K3(1,2,2) =  N*(6.*F-P*(XS**2*E+4.*XS*B+2.*A))
       K3(2,2,2) =  N*(OP*(-2.+12.*H)-P*(XS**3*E+6.*XS**2*B+6.*XS*A))

       K3S(1,1,1)   =  -N*(F+P*(A*B+E))
       K3S(1,1,2)   =   XS*K3S(1,1,1)-2.*K2(1,1)
       K3S(1,2,1)   =  -N*(2.*XS*F+P*(A*(XS*B+A)+2.*B+XS*E))
       K3S(1,2,2)   =   XS*K3S(1,2,1)-2.*K2(1,2)
       K3S(2,2,1)   =  -N*(-F+3.*XS**2*F+P*(2.*XS*B+
     1                       (A*B+E)*XS**2+2.*(A+XS*(A*A+B))))
       K3S(2,2,2)   =   XS*K3S(2,2,1)-2.*K2(2,2)

      DO 15 J=1,2
         DO 15 K=1,2
              S2(J,K)        =  S2(J,K)+K2(J,K)
           DO 15 L=1,2
              S3 (J,K,L)     =  S3(J,K,L)+K3(J,K,L)
              S3S(J,K,L)     =  S3S(J,K,L)+K3S(J,K,L)
   15    CONTINUE

    1  CONTINUE

       S2(2,1)   =  S2(1,2)

       S3(1,2,1) =  S3(1,1,2)
       S3(2,1,1) =  S3(1,1,2)
       S3(2,1,2) =  S3(1,2,2)
       S3(2,2,1) =  S3(1,2,2)

       S3S(2,1,1)   =  S3S(1,2,1)
       S3S(2,1,2)   =  S3S(1,2,2)

C==============================================================================
C
C    COMPUTE THE RESULTS FOR HIGHER POWERS OF SIGMA
C
C    NOTE:    S2   CONTAINS THE EXPECTED VALUES OF THE SECOND DERIVATIVES
C             S3   CONTAINS THE EXPECTED VALUES OF THE THIRD DERIVATIVES
C             S3S  CONTAINS THE FIRST DERIVATIVES OF THE EXPECTED VALUES
C                  OF THE SECOND DERIVATIVES.
C
C==============================================================================

      DO 20 N=2,3
         M    =  N-1

      A2(1,1,N)      =  A2(1,1,M)
      A2(1,2,N)      =  A2(1,2,M)/2.
      A2(2,1,N)      =  A2(1,2,N)
      A2(2,2,N)      =  A2(2,2,M)/4.

      A3(1,1,1,N)    =  A3(1,1,1,M)
      A3(1,1,2,N)    =  A3(1,1,2,M)/2.
      A3(1,2,1,N)    =  A3(1,1,2,N)
      A3(1,2,2,N)    =  (A3(1,2,2,M)-A2(1,2,M))/4.
      A3(2,1,1,N)    =  A3(1,1,2,N)
      A3(2,1,2,N)    =  A3(1,2,2,N)
      A3(2,2,1,N)    =  A3(1,2,2,N)
      A3(2,2,2,N)    =  (A3(2,2,2,M)-3.*A2(2,2,M))/8.0

      A3S(1,1,1,N)   =  A3S(1,1,1,M)
      A3S(1,1,2,N)   =  A3S(1,1,2,M)/2.
      A3S(2,1,1,N)   =  A3S(2,1,1,M)/2.
      A3S(2,1,2,N)   =  (A3S(2,1,2,M)-A2(1,2,M))/4.0
      A3S(1,2,1,N)   =  A3S(2,1,1,N)
      A3S(1,2,2,N)   =  A3S(2,1,2,N)
      A3S(2,2,1,N)   =  A3S(2,2,1,M)/4.
      A3S(2,2,2,N)   =  (A3S(2,2,2,M)-2.*A2(2,2,M))/8.0

   20    CONTINUE


      DO 30 N=1,3
            DET     =  A2(1,1,N)*A2(2,2,N)-A2(1,2,N)**2
            V(1,1,N)  = -A2(2,2,N)/DET
            V(1,2,N)  =  A2(1,2,N)/DET
            V(2,1,N)  =  V(1,2,N)
            V(2,2,N)  = -A2(1,1,N)/DET

       DO 30 I=1,2
              BI(I,N)   =  0.0D0
          DO 30 J=1,2
            DO 30 K=1,2
              DO 30 L=1,2

      BI(I,N) = BI(I,N)+V(I,J,N)*V(K,L,N)*(A3S(J,K,L,N)-A3(J,K,L,N)/2.)

   30  CONTINUE

       RETURN
       END
