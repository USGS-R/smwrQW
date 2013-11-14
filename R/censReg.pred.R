#'Support Functions for Censored Regression
#'
#'This is a support function for censored regression. It predicts values from
#'an AMLE/MLE regression on lcens response data.
#'
#'
#'@param fit the output from \code{censReg_AMLE.fit}.
#'@param XPRED a matrix of the prediction variables.
#'@return A list containing the output from the FORTRAN code.
#'@seealso \code{\link{censReg}},
#'@useDynLib USGSwsQW predaml
#'@export
censReg_AMLE.pred <- function(fit, XPRED) {
  ## Coding history:
  ##    2012Jul23 DLLorenz Original Coding
  ##    2012Jul31 DLLorenz Modifications for prediction and load estimation
  ##    2012Sep25 DLLorenz Begin mods for various options
  ##    2012Dec31 DLLorenz Roxygenized
  ##    2012Dec31          This version
  ##
  xtxinv <- solve(crossprod(fit$XLCAL*sqrt(fit$weights)))
  NPRED <- nrow(XPRED)
  pred <- .Fortran("predaml",
                   NPAR=fit$NPAR,
                   PARMLE=fit$PARMLE,
                   PARAML=fit$PARAML,
                   BIAS=fit$BIAS,
                   CVX=fit$CV,
                   SBIAS=fit$SBIAS,
                   SCVX=fit$SCV,
                   XTXINV=xtxinv,
                   LogNorm=fit$LogNorm,
                   NPRED=as.integer(NPRED),
                   XLPRED=XPRED,
                   ESTIM=double(NPRED),
                   ESTSEP=double(NPRED),
                   ESTSEE=double(NPRED),
                   BACKEST=double(NPRED),
                   BACKVAR=double(NPRED),
                   IERR=integer(1L))
  return(pred)
}
