************************************************************************
*
*     Subroutine NORTEST                    Called by: AMLSTAT, MLESTAT
*
*     Turnbull-Weiss normality test for grouped censored data
*     (Turnbull and Weiss, 1978).  Test assumes right-censored data.
*
*     This procedure is similar to a Chi-Square test for normality.
*     It requires grouping the data into a discrete number of groups.
*     As implemented, this procedure incorporates the recommendation
*     that the groups have equal probability under the null
*     hypothesis (Kendall and Stuart, 1979, p. 455-458.).  It also
*     maintains a minimum of 5 expected observations per group except
*     for very small sample sizes (as recommended for Chi-Square tests
*     for normality, [Sachs, 1984, p. 321])
*
*     The test statistic is a likelihood ratio statistic.  Twice the
*     log of the likelihood ratio statistic is approximately
*     distributed as a Chi-Square distribution with NGROUP-3 degrees
*     of freedom (Turnbull and Weiss, 1978).  The empirical group
*     probabilities are obtained by the Kaplan-Meier estimator.
*
*     The null hypothesis that the data are normally distributed is
*     rejected if the value of the test statistic exceeds that of a
*     Chi-Square distribution with NGROUP-3 degrees of freedom at the
*     desired alpha level.
*
*     local vars
*     ----------
*     AREA     area under normal curve for which ZSCORE is desired
*     DELTA    number of censored obs. in the current or lower group
*     DIF1     intermediate quantity
*     DIF2     intermediate quantity
*     F        cumulative empirical probability at the endpoints of
*              the groups
*     GENDPT   group endpoints (groups are delineated by GENDPT(I) and
*              GENDPT(I+1))
*     GPINT    group probability interval; probability that an obs.
*              will fall into any given group under the null hypoth.
*              hypothesis
*     LAMBDA   number of uncensored obs. in the current or lower group
*     NGROUP   number of groups into which the data are divided
*     PROD1    intermediate quantity
*     PROD2    intermediate quantity
*     SHAT     empirical probability that an observation will fall
*              into a specific group
*     SUM      intermediate quantity
*     SWAVY    expected probability under the null hypothesis that
*              an observation will fall into a specific group
*     ZSCORE   Z-Score for given area under the standard normal curve
*
************************************************************************
      SUBROUTINE NORTEST(CENSFLAG,DF,LLR,NOBSC,SRESID,PLEV,IERR)
*
*     subroutine arguments
*
      LOGICAL CENSFLAG(*)
      INTEGER*4 DF,NOBSC,IERR
      DOUBLE PRECISION LLR,PLEV,SRESID(*)
*
*     local vars
*
      INTEGER*4 I,J,NGROUP,DELTA(200),LAMBDA(200)
      DOUBLE PRECISION AREA,DIF1,DIF2,GPINT,PROD1,PROD2,SUM
      DOUBLE PRECISION SWAVY(200),SHAT(200),F(0:200),GENDPT(201)
*
*     function declaration
*
      DOUBLE PRECISION ZSCORE,GAMMAD
*
*     calculate the number of groups; except at very small sample
*     sizes, each group has an expected frequency of at least 5 under
*     the normal distribution
*
      IERR = 0
      IF (NOBSC.LT.20) THEN
         NGROUP = 4
      ELSEIF (NOBSC.GE.20.AND.NOBSC.LT.1000) THEN
         NGROUP = INT(DBLE(NOBSC)/5.D0)
      ELSE
         NGROUP = 200
      ENDIF
*
*     subdivide the data into groups of equal probability.  GPINT is
*     equal to 1/NGROUP since this routine uses the equal probability
*     method of assigning group endpoints
*
      AREA = 0.D0
      GPINT = 1.D0/DBLE(NGROUP)
      GENDPT(1) = -1.D99
      DO 30 I=2,NGROUP
         AREA = AREA + GPINT
         GENDPT(I) = ZSCORE(AREA)
 30   CONTINUE
      GENDPT(NGROUP+1) = 1.D99

      DO 50 I=1,NGROUP
         SWAVY(I) = GPINT
         DELTA(I) = 0
         LAMBDA(I) = 0
         DO 40 J=1,NOBSC
            IF (SRESID(J).GE.GENDPT(I).AND.SRESID(J).LT.GENDPT(I+1))
     &      THEN
               IF (CENSFLAG(J)) THEN
                  LAMBDA(I) = LAMBDA(I) + 1
               ELSE
                  DELTA(I) = DELTA(I) + 1 
               ENDIF
            ENDIF
 40      CONTINUE
 50   CONTINUE
*
*
*
      DF = NGROUP - 3
      SHAT(NGROUP) = 1.D0
      F(0) = 1.D0
      DO 70 I=1,NGROUP-1
         SUM = 0.D0
         DO 60 J=I,NGROUP
            SUM = SUM + DBLE(DELTA(J)+LAMBDA(J))
 60      CONTINUE
         IF (SUM.GT.0.D0) THEN
            F(I) = F(I-1)*(1.D0-(DBLE(DELTA(I))/SUM))
         ELSE
            F(I) = F(I-1)
         ENDIF
         SHAT(I) = F(I-1) - F(I)
         SHAT(NGROUP) = SHAT(NGROUP) - SHAT(I)
 70   CONTINUE
*
*     tests to make sure the empirical (SHAT) and expected (SWAVY) group
*     probabilities sum to 1
*
      SUM = 0.D0
      DO 80 I=1,NGROUP
         SUM = SUM + SHAT(I)
 80   CONTINUE
      IF (DABS(1.D0-SUM) .GT. 1.D-6) IERR = -7
      SUM = 0.D0
      DO 90 I=1,NGROUP
         SUM = SUM + SWAVY(I)
 90   CONTINUE
      IF (DABS(1.D0-SUM).GT.(1.D-6)) IERR = -8
*
*
*
      PROD1 = 1.D0
      DO 100 I=1,NGROUP
         IF (SHAT(I).GT.(0.D0)) THEN
            PROD1 = PROD1*((SWAVY(I)/SHAT(I))**DELTA(I))
         ENDIF
 100  CONTINUE
      PROD2 = 1.D0

      DO 120 I=1,NGROUP-1
         DIF1 = 1.D0
         DIF2 = 1.D0
         DO 110 J=1,I
            DIF1 = DIF1 - SWAVY(J)
            DIF2 = DIF2 - SHAT(J)
 110     CONTINUE
         IF (DIF2.GT.0.D0) THEN
            PROD2 = PROD2*((DIF1/DIF2)**LAMBDA(I))
         ENDIF
 120  CONTINUE
*
*     calculate Turnbull-Weiss log likelihood ratio statistic (LLR) and
*     make sure it is greater than zero.  Then compute the Turnbull-
*     Weiss probability level (PLEV), which is equal to 1 minus the 
*     probability level for the Chi-Square distribution corresponding
*     to LLR w/ DF degress of freedom (use relation between Chi-Square
*     distribution and incomplete Gamma integral to obtain result).
*
      LLR = -2.D0*DLOG(PROD1*PROD2)
      IF (LLR .LT. 0.D0) IERR = -9
      PLEV = 1.D0 - GAMMAD(LLR/2.D0,DBLE(DF)/2.D0)

      RETURN
      END
