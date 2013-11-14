************************************************************************
*
*     Subroutine AMLPVAL                   Called by: AMLSTAT, AMLSTAT2
*
*     calculate P-values corresponding to AMLE parameter estimates
*
*     based on Tim Cohn's code, obtained from www.timcohn.com
*     (Software; Tobit) Nov. 27, 2001.
*
*     local vars
*     ----------
*     ITEST    flag indicating which AMLE param. estimate PVAL is for
*     XLIKEP   log-likelihood function for AMLE parameter estimate
*
************************************************************************
      SUBROUTINE AMLPVAL(CENSFLAG,NOBSC,NPAR,PVAL,XLCAL,XLIKE,YD,YLCAL,
     &                   IERR)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      LOGICAL CENSFLAG(*)
      INTEGER*4 IERR,NOBSC,NPAR
      DOUBLE PRECISION XLIKE,PVAL(*),YD(*),YLCAL(*),XLCAL(MAXOBSC,*)
*
*     local vars
*
      INTEGER*4 I,K,ITEST(MAXPARMS)
      DOUBLE PRECISION XLIKEP(MAXPARMS)
*
*     function declaration
*
      DOUBLE PRECISION PVALUE
*
*     get likel. ratio test P-values corresponding to parameter
*     estimates
*
      DO 20 K=1,NPAR
         DO 10 I=1,NPAR
            ITEST(I) = 0
 10      CONTINUE
         ITEST(K) = 1
         CALL TACIT_TEST(CENSFLAG,ITEST,NOBSC,NPAR,XLCAL,XLIKEP(K),YD,
     &                   YLCAL)
         PVAL(K) = PVALUE(XLIKEP(K),XLIKE, IERR)
 20   CONTINUE

      RETURN
      END
