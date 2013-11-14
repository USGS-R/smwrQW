************************************************************************
*
*     Function GAMMAD                                 Called by: NORTEST
*
*     compute the incomplete gamma integral.  Algorithm AS239 Appl.
*     statist. (1988) vol. 37, no. 3
*
************************************************************************
      DOUBLE PRECISION FUNCTION GAMMAD(X,P)
*
*     function arguments
*
      DOUBLE PRECISION P,X
*
*     local vars
*
      DOUBLE PRECISION PN1,PN2,PN3,PN4,PN5,PN6,ARG,C,RN,A,B
*
*     function declaration
*
      DOUBLE PRECISION DLOGAM
*
*     initialize & check for valid values of X and P
*
      GAMMAD = 0.D0
      IF (X.EQ.0.D0) RETURN
*
*     Use normal approximation if P > 1000.D0.  First set the upper
*     limit of the integral (PN1), then calculate the integral of the
*     cumulative probability density function for the normal
*     distribution from minus infinity to PN1, using the approximation
*     given in Section 26.2.19 of Abramowitz and stegun (1964, p.932).
*     The error in the approximation is less than 1.5E-7.
*
      IF (P .GT. 1000.D0) THEN
        PN1 = 3.D0*DSQRT(P)*((X/P)**(1.D0/3.D0)+1.D0/(9.D0*P)-1.D0)
        GAMMAD = 1.D0-0.5D0*((((((5.383D-6*PN1+4.88906D-5)
     &           *PN1+3.80036D-5)*PN1+3.2776263D-3)*PN1+2.11410061D-2)
     &           *PN1+4.98673470D-2)*PN1+1.D0)**(-16)
        RETURN
      ENDIF
*
*     if X is extremely large compared to P then set GAMMAD to 1.0
*
      IF (X.GT.1.D6) THEN
        GAMMAD = 1.D0
        RETURN
      ENDIF
*
*     The first part of the IF stmt uses Pearson's series expansion
*     (note that P is not large enough to force overflow in DLOGAM).
*     No need to test IFAULT on exit since P > 0.0.  The second part
*     of the IF uses a continued fraction expansion
*
      IF (X.LE.1.D0.OR.X.LT.P) THEN
         ARG = P*DLOG(X)-X-DLOGAM(P+1.D0)
         C = 1.D0
         GAMMAD = 1.D0
         A = P
 40      A = A + 1.D0
         C = C*X/A
         GAMMAD = GAMMAD+C
         IF (C.GT.1.D-7) GOTO 40
         ARG = ARG+DLOG(GAMMAD)
         GAMMAD = 0.D0
         IF (ARG.GE.-88.D0) GAMMAD = DEXP(ARG)
      ELSE
         ARG = P*DLOG(X)-X-DLOGAM(P)
         A = 1.D0-P
         B = A+X+1.D0
         C = 0.D0
         PN1 = 1.D0
         PN2 = X
         PN3 = X+1.D0
         PN4 = X*B
         GAMMAD = PN3/PN4
 60      A = A+1.D0
         B = B+2.D0
         C = C+1.D0
         PN5 = B*PN3-(A*C)*PN1
         PN6 = B*PN4-(A*C)*PN2
         IF (DABS(PN6).GT.0.D0) THEN
            RN = PN5/PN6
            IF (DABS(GAMMAD-RN).LE.DMIN1(1.D-7,1.D-7*RN)) GOTO 80
            GAMMAD = RN
         ENDIF

         PN1 = PN3
         PN2 = PN4
         PN3 = PN5
         PN4 = PN6
*
*        re-scale terms in continued fraction if terms are large
*
         IF (DABS(PN5).GE. 1.D30) THEN
            PN1 = PN1/1.D30
            PN2 = PN2/1.D30
            PN3 = PN3/1.D30
            PN4 = PN4/1.D30
         ENDIF
         GOTO 60
 80      ARG = ARG + DLOG(GAMMAD)
         GAMMAD = 1.D0
         IF (ARG.GE.-88.D0) GAMMAD = 1.D0-DEXP(ARG)
      ENDIF

      END
