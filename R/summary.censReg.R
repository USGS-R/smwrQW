#'Summarizing Linear Model Fits
#'
#'Summaryizes the output from an AMLE/MLE regression
#'
#'@param object an object of class "censReg"---output from \code{censReg}.
#'@param correlation include the correlation matrix of the estimated
#'parameters?
#'@param \dots further arguments passed to or from other methods.
#'@return An object of class "summary.censReg" containing the object, variance
#'inflation factors, a table of diagnostic statistics, critical values for
#'selected diagnostic statistics, an indication of which observations
#'exceed any of the selected diagnostic statistics, and optionally the
#'parameter correlation matrix.
#'@seealso \code{\link{censReg}}
#'@keywords regression
#'@S3method summary censReg
#'@method summary censReg
summary.censReg <- function(object, correlation=FALSE, ...) {
  ## Coding history:
  ##    2012Dec31 DLLorenz Initial Coding
  ##
  ## Construct the diagnostics table
  diagstats <- data.frame(Y=object$YLCAL,
                          ycen=object$CENSFLAG,
                          yhat=fitted(object, suppress.na.action=TRUE),
                          resids=residuals(object, suppress.na.action=TRUE),
                          leverage=residuals(object, suppress.na.action=TRUE,
                            type="leverage"),
                          cooksD=residuals(object, suppress.na.action=TRUE,
                            type="influence"))
  names(diagstats)[1L] <- make.names(deparse(object$terms[[2L]]))
  ## Compute critical values for diagnostic statistics and pick out offending
  ##  observations
  p <- object$NPAR - 1
  n <- object$NOBS
  cvlev <- 3*p/n
  cvcook <- qf(.5,p+1,n-p)
  cvs <- c(leverage=cvlev, cooksD=cvcook)
  pck <- c(diagstats$leveerage > cvlev | diagstats$cooksD > cvcook)
  ## Pack it up
  retval <- list(object=object, vif=vif(object),
                 diagstats=diagstats, crit.val=cvs, flagobs=pck)
  if(correlation) {
    corr <- cov2cor(object$COV)
    Nm <- c(colnames(object$XLCAL), "Scale")
    dimnames(corr) <- list(Nm, Nm)
    retval$correlation <- corr
  }
  class(retval) <- "summary.censReg"
  return(retval)
}
