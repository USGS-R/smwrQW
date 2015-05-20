#'Extract AIC from a Fitted Model
#'
#'Computes the Akaike An Information Criterion for an AMLE/MLE regression.
#'
#' @param fit an object of class "censReg"---output from \code{censReg}.
#' @param scale set scale, ignored in current version.
#' @param k the weight of the equivalent degrees of freedom.
#' @param correct logical if \code{TRUE} and \code{k} is set to 2, then return
#'corrected AIC, if \code{FALSE} or \code{k} is not set to 2,then return the 
#'uncorrected value, either AIC or BIC.
#' @param \dots further arguments passed to or from other methods.
#' @return A vector of length 2, giving the equivalent degreees of freedom and
#'the AIC for \code{fit}.
#' @seealso \code{\link{censReg}}, \code{\link{extractAIC}}
#' @keywords regression
#' @export
#' @method extractAIC censReg
extractAIC.censReg <- function(fit, scale=0, k=2, correct=FALSE,...) {
  ## Coding history:
  ##    2012Dec26 DLLorenz Initial Coding
  ##    2012Dec31 DLLorenz Roxygenized
  ##    2012Dec31          This version
  ##
  edf <- fit$NPAR+1
  aic <- AIC(fit, k=k)
  if(k == 2 && correct) {
  	# Compute AICc
  	aic <- aic + 2 * edf * (edf + 1)/(fit$NOBSC - edf - 1)
  }
  return(c(edf, aic))
}
