**********************************************************************
*
*     Subroutine PREDAML                       Called by: R
*
*     Subroutine arguments
*     --------------------
*     NPAR     Number of parameters, except intercept
*     PARMLE   The MLE parameter estiamtes
*     PARAML   The adjusted MLE parameter estimates
*     BIAS     The bias (BIAS) computed by AMLE
*     CVX      The COV matrix (CV) in AMLREG
*     SBIAS    The bias (SBIAS) computed by AMLE
*     SCVX     The COV matrix (SCV) in AMLREG
*     XTXINV   The inverse of X * t(X)
*     LOGNORM  Assume that the residuals are lognormal and back-transform
*     NPRED    The number of observation in the estimation data
*     XLPRED   The prediction data
*     ESTIM    The estimates from the regression equation
*     ESTSEP   The SEP of tthe estimates
*     ESTSEE   The SEE of tthe estimates
*     BACKEST  The back-transformed and bais corrected estimate &
*     BACKVAR  The variance of BACKEST &
*     IERR     Error code
*     &        Computed only if LOGNORM is TRUE
*
**********************************************************************
      SUBROUTINE PREDAML(NPAR,PARMLE,PARAML,BIAS,CVX,SBIAS,SCVX,
     &     XTXINV, LOGNORM,
     &     NPRED,XLPRED,ESTIM,ESTSEP,ESTSEE,BACKEST,BACKVAR,IERR)
*     
*     dimensional parameters and logical devices
*
      INCLUDE 'fmodules.inc'
*
*     subroutine arguments
*
      INTEGER*4 NPRED
      INTEGER*4 NPAR,IERR
      DOUBLE PRECISION PARMLE(*),PARAML(*),BIAS(*),SBIAS(*),
     &     CVX(NPAR+1,NPAR+1),SCVX(NPAR+1,NPAR+1),XLPRED(NPRED,*),
     &     XTXINV(NPAR,NPAR)
      DOUBLE PRECISION ESTIM(*),ESTSEP(*),ESTSEE(*),BACKEST(*),
     $     BACKVAR(*)
      LOGICAL LOGNORM
*
*     local vars
*
      INTEGER K1,K2,I
      DOUBLE PRECISION XLEST(MAXOBSE,MAXPARMS),PARMS(MAXPARMS+1),
     $     XLDAT(MAXOBSC,MAXPARMS+1),MTEVAR, XTEMP
      DOUBLE PRECISION CV(MAXPARMS+1,MAXPARMS+1),
     $     SCV(MAXPARMS+1,MAXPARMS+1),XTX_INV(MAXPARMS,MAXPARMS)
*
*     local function
*
      DOUBLE PRECISION PRED
*
*     set initial values and check limits
*
      IERR=0
      IF(NPAR .gt. MAXPARMS) IERR=1
      IF(NPRED .gt. MAXOBSE) IERR=4
      IF(IERR .ne. 0) RETURN
*
*     Set up for correct dimensions in the called subroutines
*
      DO K1=1,NPRED
         DO K2 =1,NPAR
            XLEST(K1,K2) = XLPRED(K1,K2)
         ENDDO
      ENDDO
      DO K1=1,NPAR+1
         DO K2=1,NPAR+1
            CV(K1,K2) = CVX(K1,K2)
            SCV(K1,K2) = SCVX(K1,K2)
         ENDDO
      ENDDO
      DO K1=1,NPAR
         DO K2=1,NPAR
            XTX_INV(K1,K2) = XTXINV(K1,K2)
         ENDDO
      ENDDO

*     
*

      DO I=1,NPRED
         CALL MATMLT(MTEVAR,XLEST,XTX_INV,NPAR,I,I)
         ESTIM(I)=PRED(NPAR,MAXOBSE,I,XLEST,PARAML)
         ESTSEP(I)=DSQRT(PARAML(NPAR+1) * (1D0 + MTEVAR))
         ESTSEE(I)=DSQRT(PARAML(NPAR+1) * MTEVAR)
         IF(LOGNORM)
     $        CALL TAC_LOAD(NPAR,PARMLE,BIAS,CV,SBIAS,SCV,1,XLEST(I, 1),
     &        BACKEST(I),XTEMP,BACKVAR(I))
      ENDDO

      RETURN
      END
