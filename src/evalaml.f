*     Subroutine EVALAML                        Called by: R
*
*     Evaluate a censored regression model by adjusted MLE,
*     used in censReg and LOADEST
*
*     Subroutine arguments
*     --------------------
*     NOBSC    Number of observations
*     NPAR     Number of parameters, except intercept
*     XLCAL    The explanatory variables
*     YLCAL    The response variable value
*     YD       The detection limit
*     CENSFLAG Censoring flag .TRUE. (1) means censored
*     PARAML   Coefficients of the AMLE model
*     STDDEV   Standard deviation of the coefficients from the AMLE regression
*     PVAL     P-values for the AMLE regression coefficients
*     COV      Adj. Max. Likelihood Est. of the covariance matrix
*     RESID    The residuals
*     RSQ      R-squared of the AMLE regression
*     LLR      The log-likelihood of the regression model
*     SCORR    serial correlation of the AMLE residuals
*     LLRAML   TWNT log likelihood ratio statistic for AMLE regression
*     PLEVAML  TWNT probability level for the AMLE regression
*     DF       degrees of freedom for Turnbull-Weiss normality test
*     IERR     Error code
*
*     dimensions required of the arguments
*     ------------------------------------
*     XLCAL    NOBSC * NPAR
*     YLCAL    NOBSC
*     YD       NOBSC
*     CENSFLAG NOBSC
*     NOBSC    scalar
*     NPAR     scalar
*     PARAML   NPAR + 1
*     STDDEV   NPAR + 1
*     PVAL     NPAR + 1
*     COV      NPAR + 1 * NPAR + 1
*     RESID    NOBSC
*     RSQ      scalar
*     LLR      scalar
*     SCORR    scalar
*     LLRAML   scalar
*     PLEVAML  scalar
*     DF       scalar
*     IERR     scalar
*
*     local vars
*     ----------
*     BIAS     bias of max likelihood estimates w.r.t. S**2
*     CV       covar matrix of std normals censored at dimensionless
*              threshold w.r.t. S**2
*     CV_M     max. likelihood estimates of the covariance matrix
*              (note: CV_M(NPAR+1) refers to covariance w.r.t. S**2)
*     CV_A     Adj. Max. Likelihood Est. of the covariance matrix
*              w.r.t. S**2 (not in R version)
*     PARMLE   storage for coefficient of the MLE regression
*     SBIAS    bias of max likelihood estimates w.r.t. S
*     SCV      covar matrix of std normals censored at dimensionless
*              threshold w.r.t. S
*     TPAR     temporarily store paraml
*     YHAT     load from the AMLE regression based on calibr data
*     IERR     error code
*
**********************************************************************
      SUBROUTINE EVALAML(NOBSC,NPAR,XLCAL,YLCAL,YD,censin,
     &     PARMLE,PARAML,BIAS,CVX,SBIAS,SCVX,
     &     STDDEV,PVAL,COV,RESID,RSQ,LLR,SCORR,LLRAML,
     $     PLEVAML,DF,login,YPRED,AIC,SPPC,IERR)
*
*     dimensional parameters
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER censin(*), login
      INTEGER*4 NOBSC,NPAR,IERR,DF
      DOUBLE PRECISION YD(*),YLCAL(*),RESID(*),PVAL(*)
      DOUBLE PRECISION PARAML(*),PARMLE(*),BIAS(*),SBIAS(*),
     &     XLCAL(NOBSC,*),CVX(NPAR+1,NPAR+1),SCVX(NPAR+1,NPAR+1),
     $     STDDEV(*),COV(NPAR+1,NPAR+1),YPRED(*)
      DOUBLE PRECISION PLEVAML,RSQ,SCORR,LLRAML,LLR,AIC,SPPC

*
*     local vars
*
      LOGICAL CENSFLAG(MAXOBSC),LOGNORM
      DOUBLE PRECISION TPAR(MAXPARMS+1),XLIKE,XLIKEP
      DOUBLE PRECISION CV(MAXPARMS+1,MAXPARMS+1),YLCAL2(MAXOBSC),
     &     CV_M(MAXPARMS+1,MAXPARMS+1),
     $     CV_A(MAXPARMS+1,MAXPARMS+1),
     &     SCV(MAXPARMS+1,MAXPARMS+1),
     &     YHAT(MAXOBSC),X(MAXOBSC,MAXPARMS+1),LOGLK
      DOUBLE PRECISION uncenmin, uncenmax, D1MACH
      real test
      INTEGER*4 K1,J
*
*     Calculate regression coefficients using Adjusted Maximum
*     Likelihood Estimation (AMLE).
*
*
*     Check for parameter limits
*
      PRINT*,5.41232
      IERR = 0
      IF(NPAR .gt. MAXPARMS) IERR=1
      IF(NOBSC .gt. MAXOBSC) IERR=2
      IF(IERR .ne. 0) RETURN
*
*     Convert to logicals
*
      if(login .eq. 1) then
        LOGNORM = .TRUE.
      else
        LOGNORM = .FALSE.
      endif
      do i = 1, NOBSC
        if(censin(i) .eq. 1) then
          CENSFLAG(i) = .TRUE.
        else
          CENSFLAG(i) = .FALSE.
        endif
      enddo
*
*     Copy XLCAL input to X dimensions for all routines
*
      Ncens = 0
      uncenmin = D1MACH(2)
      uncenmax = -D1MACH(2)
      DO 20 K1=1,NOBSC
         IF(CENSFLAG(K1)) THEN
            Ncens = Ncens + 1
         ELSE
            IF(YLCAL(K1) .LT. uncenmin) uncenmin = YLCAL(K1)
            IF(YLCAL(K1) .GT. uncenmax) uncenmax = YLCAL(K1)
         ENDIF
         DO 10 K2 =1,NPAR
            X(K1,K2) = XLCAL(K1,K2)
 10      CONTINUE
 20   CONTINUE
      test = float(Ncens) / float(NOBSC)
      IF(test .GT. 0.80) THEN
         IERR = -201
         IF(test .GT. 0.90) THEN
            IERR = 201
            RETURN
         ENDIF
      ENDIF
      IF(NOBSC - Ncens .LT. 3 * NPAR) THEN
         IERR = -202
         IF(NOBSC - Ncens .LT. 3 * NPAR / 2) THEN
            IERR = 202
            RETURN
         ENDIF
      ENDIF
      IF(uncenmin .EQ. uncenmax) THEN
         IERR = 203
         RETURN
      ENDIF
*
      CALL AMLREG(NOBSC,NPAR,X,YLCAL,YD,CENSFLAG,XLIKE,
     &     STDDEV,PARMLE,PARAML,BIAS,CV,
     &     SBIAS,SCV,CV_M,IERR)
      IF(IERR .gt. 0) RETURN
*
*     Move covariances to output
*
      DO K1=1,NPAR+1
         DO K2=1,NPAR+1
            COV(K1,K2) = CV_M(K1,K2)
            CVX(K1,K2) = CV(K1,K2)
            SCVX(K1,K2) = SCV(K1,K2)
         ENDDO
      ENDDO
*
*     save paraml
*
      DO 30 K1=1,NPAR+1
         TPAR(K1)=PARAML(K1)
 30   CONTINUE
      CALL AMLSTAT(CENSFLAG,DF,LLRAML,NOBSC,NPAR,
     &     PARAML,PLEVAML,PVAL,RESID,RSQ,SCORR,
     &     X,XLIKE,YD,YHAT,YLCAL,YLCAL2,IERR)
      IF(IERR .gt. 0) RETURN
      LLR = XLIKE
*
*     AIC modified from the original (not divide by nobsc!)
*     SPPC (BIC) also modifed
*     Further modified to use NPAR + 1 instead of NPAR to
*     correspond to what is returned by R
*
      AIC = -2.D0 * LLR + 2.D0 * DBLE(NPAR + 1)
      SPPC = -2.D0 * LLR + (DBLE(NPAR + 1)*DLOG(DBLE(NOBSC)))
*
*     resore paraml
*
      DO 40 K1=1,NPAR+1
         PARAML(K1) = TPAR(K1)
 40   CONTINUE
*
*     compute the estimates
*     for some bizarre reason, PRED does not work!
*
      if(LOGNORM) then
         call TAC_PRED(NPAR,PARMLE,BIAS,CV,SBIAS,SCV,NOBSC,X,YPRED)
      else
         do 50 K1=1,NOBSC
            YPRED(K1) = 0.d0
            do J=1,NPAR
               YPRED(K1) = YPRED(K1) + PARAML(J) * X(K1,J)
            enddo
 50      CONTINUE
      endif
      RETURN
      END
