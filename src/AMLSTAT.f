************************************************************************
*
*     Subroutine AMLSTAT                         Called by: evalaml, CALIBR
*
*     compute AMLE statistics and residuals for the best model
*
*     local vars
*     ----------
*     RESSD    residual standard deviation
*     SRESID   standardized residuals
*     SUM      sum of YHAT
*     SUMSQ    sum of squares, YHAT
*     SUMSR    sum of squared residuals
*
************************************************************************
      SUBROUTINE AMLSTAT(CENSFLAG,DF,LLRAML,NOBSC,NPAR,
     &     PARAML,PLEVAML,PVAL,RESID,RSQ,SCORR,
     &     XLCAL,XLIKE,YD,YHAT,YLCAL,YLCAL2,IERR)
*     
*     dimensional parameters and logical devices
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      LOGICAL CENSFLAG(*)
      INTEGER*4 DF,IERR,NOBSC,NPAR
      DOUBLE PRECISION LLRAML,PLEVAML,RSQ,SCORR,XLIKE
      DOUBLE PRECISION PARAML(*),PVAL(*),YD(*),
     &                 YLCAL(*),YLCAL2(*)
      DOUBLE PRECISION RESID(MAXOBSC),XLCAL(MAXOBSC,*),YHAT(MAXOBSC)
*
*     local vars
*
      INTEGER*4 I,K
      DOUBLE PRECISION A,RESSD,SUM,SUMSR,SUMSQ
      DOUBLE PRECISION SRESID(MAXOBSC)
*
*     function declarations
*
      DOUBLE PRECISION CORR,PRED
*
*     calculate P-values (PVAL) for the AMLE regression coefficients
*
      CALL AMLPVAL(CENSFLAG,NOBSC,NPAR,PVAL,XLCAL,XLIKE,YD,YLCAL,IERR)
*
*     calculate residuals (observed load - predicted) and standardized
*     residuals (SRESID; residual/residual std. deviation), SUM, and
*     SUMSQ.  For censored observations, use the expected value of
*     observations rather than the observations (5/7/92 Tim Cohn).
*
*     (Preserve the original YLCAL for use by LADREG and JKNIFE by
*     introducing YLCAL2).  
*
      RESSD = SQRT(PARAML(NPAR+1))
      SUM = 0.D0
      SUMSQ = 0.D0
      DO 10 I=1,NOBSC
         YLCAL2(I) = YLCAL(I)
         YHAT(I) = PRED(NPAR,MAXOBSC,I,XLCAL,PARAML)
         IF (CENSFLAG(I)) THEN
            CALL TACIT_CALC((YD(I)-YHAT(I))/RESSD,A)
            YLCAL2(I) = YHAT(I) - RESSD*A
         ENDIF
         RESID(I) = YLCAL2(I) - YHAT(I)
         SRESID(I) = RESID(I)/RESSD
         SUM = SUM + YHAT(I)
         SUMSQ = SUMSQ + YHAT(I)**2
 10   CONTINUE
*
*     calculate the Turnbull-Weiss likelihood ratio normality test
*     (S/R NORTEST) and the associated probability level
*
      CALL NORTEST(CENSFLAG,DF,LLRAML,NOBSC,SRESID,PLEVAML)
*
*     compute R^2 of the load regression and the serial correlation of
*     the residuals
*
      SUMSR = SUMSQ-SUM**2/NOBSC
      RSQ = 100.D0 * SUMSR/(((NOBSC-NPAR)*RESSD**2+SUMSR))
      SCORR = CORR(NOBSC-1,RESID(1),RESID(2))
*
      RETURN
      END



