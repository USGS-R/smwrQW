************************************************************************
*
*     Subroutine PPCCTEST                   Called by: R
*
*     Probability plot correlation coefficient normality test for left-
*     censored data (Royston, 1993). Extended to multiply censord data
*     by Millard in the Environmental Stats for S+ library
*
*     This procedure is similar to the ppcc test for uncensored data.
*     It computes the correlation coefficient between the uncensored
*     qunatiles of the data and the corresponding normal quantiles.
*     The test statistic is the correlation. The attained p-values are
*     empirical and only approximate for multiply-censored data.
*
*     The null hypothesis that the data are normally distributed is
*     rejected if the value of the test statistic is less than the
*     critical value for the number of observations and censoring.
*
*     Note that IERR is 0 for the singly censored test and -1 for
*     the multiply censored test. The attained p-value for the 
*     multiply censored test is only approximate. IERR > 0 indicates an 
*     error and the results are not valid.
*
************************************************************************
      SUBROUTINE ppcctest(censin,NOBSC,RESID,R,PLEV,NUNCEN,IERR)
*
*     subroutine arguments
*
      integer censin(*)
      INTEGER*4 NOBSC, NUNCEN, IERR
      DOUBLE PRECISION R,PLEV,RESID(*)
*
*     local vars
*
      INTEGER*4 I, NDL,NUY
      DOUBLE PRECISION CDL,W,ADJM,ADJS,Z,CNUN,U,D
      DOUBLE PRECISION DELTA,RSMOOTH(3),ZSMOOTH(3),MU,SIGMA
      INTEGER, DIMENSION(NOBSC) :: IX
      DOUBLE PRECISION, DIMENSION(NOBSC) :: X,Y,PP,QQ
      LOGICAL, DIMENSION(NOBSC) :: XC, CENSFLAG
      INTEGER, DIMENSION(:), ALLOCATABLE :: AJ, BJ, CJ
*
*     FUNCTION DECLARATION
*
      DOUBLE PRECISION QNORM01, CORR, ERFC
*
*     Convert to logicals
*
      do i = 1, NOBSC
        if(censin(i) .eq. 1) then
          CENSFLAG(i) = .TRUE.
        else
          CENSFLAG(i) = .FALSE.
        endif
      enddo
*
*     SORT THE DATA AND COUNT UNIQUE DLS
*
      IERR = 0
      IF(NOBSC .GT. 5000) THEN
         IERR = 4
         RETURN
      ENDIF
      CALL PPORDER(CENSFLAG, RESID, NOBSC, IX)
      CDL = -1D99
      NDL = 0
      DO I=1,NOBSC
         X(I) = RESID(IX(I))
         XC(I) = CENSFLAG(IX(I))
         IF(XC(I) .AND. X(I) .GT. CDL) THEN
            NDL = NDL + 1
            CDL = X(I)
         ENDIF
      ENDDO
*
*     Compute the plotting positions and convert to quantiles of the std. norm.
*
      ALLOCATE(AJ(NDL+2))
      ALLOCATE(BJ(NDL+2))
      ALLOCATE(CJ(NDL+2))
      CALL PPARRANGE(X,XC,NOBSC,AJ,BJ,CJ,NDL,Y,NUNCEN)
      IF(NUNCEN .LT. 3) THEN
         IERR=1
         RETURN
      ENDIF
      CALL PPLOT(AJ,BJ,NDL,PP,IERR)
*
*     ERROR CHECK ON QNORM01 AND UNIQUE VALUES OF Y
*
      NUY = 0
      DO I=1,NUNCEN
         QQ(I) = QNORM01(PP(I))
         IF(QQ(I) .EQ. 0.D0 .AND. PP(I) .NE. 0.5D0) IERR=3
         IF(I .GT. 1 .AND. Y(I) .GT. Y(I-1)) NUY = NUY + 1
      ENDDO
      IF(NUY .LT. 2) THEN
         IERR=2
         RETURN
      ENDIF
*
*     COMPUTE THE CORRELATION AND TEST STAT BETWEEN THE UNCENSORED AND QNORMALS
*
      R = CORR(NUNCEN, QQ, Y)
      CNUN = DBLE(NOBSC)
      W = LOG(1.d0 - R**2)
      ADJM = log(log(CNUN)) - log(CNUN)
      ADJS = log(log(CNUN)) + 2.D0/log(CNUN)
      Z = (W - (-1.2725D0 + 1.0521D0 * ADJM))/
     $     (1.0308D0 - 0.26758D0 * ADJS)
      IF(NDL .EQ. 0) THEN
*     
*     Compute the uncensored ppcc.test
*     
         PLEV = ERFC(Z/1.4142135623731D0)/2.D0
      ELSE
*
*     Compute the censored ppcc.test
*
         U = log(dble(NOBSC))
         D = 0.76676D0 * U + 0.015814D0 * U**2
         DELTA = -LOG(dble(NOBSC - NUNCEN)/dble(NOBSC))
         RSMOOTH(1) = 0.164D0 + 0.533D0 * 0.556D0**U
         RSMOOTH(2) = 0.1736D0 + 0.315D0 * 0.622D0**U
         RSMOOTH(3) = 0.256D0 - 0.00635D0 * U
         ZSMOOTH(1) = 1.2815515655446D0 + D * RSMOOTH(1)**DELTA
         ZSMOOTH(2) = 1.64485362695147D0 + D * RSMOOTH(2)**DELTA
         ZSMOOTH(3) = 2.32634787404084D0 + D * RSMOOTH(3)**DELTA
         CALL PPFIT(ZSMOOTH, MU, SIGMA)
         PLEV = ERFC((Z - MU)/SIGMA/1.4142135623731D0)/2.D0
      ENDIF
      RETURN
      END
