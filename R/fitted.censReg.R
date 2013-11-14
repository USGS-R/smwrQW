#'Extract Model Fitted Values
#'
#'Extract the fitted values of an AMLE/MLE regression.
#'
#'@param object an object of class "censReg"---output from \code{censReg}
#'@param suppress.na.action logical, suppress the effects of the
#'\code{na.action} in the call to \code{censReg} and return only the fitted
#'values corresponding to the fitted data.
#'@param \dots further arguments passed to or from other methods.
#'@return The fitted values from the regression.
#'@seealso \code{\link{censReg}}
#'@keywords regression
#'@S3method fitted censReg
#'@method fitted censReg
fitted.censReg <- function(object, suppress.na.action=FALSE, ...) {
  ## Coding history:
  ##    2012Sep25 DLLorenz Initial Coding
  ##    2012Dec31 DLLorenz Roxygenized
  ##    2012Dec31          This version
  ##
  fits <- as.vector(object$XLCAL %*% object$PARAML[1L:object$NPAR])
  if(is.null(object$na.action) || !suppress.na.action)
    return(fits)
  else return(naresid(object$na.action, fits))
}
