#'Variance Inflation Factors
#'
#'Computes the variance inflation factor (Helsel and Hirsch, 2002) for each
#'variable in an AMLE/MLE regression.
#'
#' @param model an object of class "censReg"---output from \code{censReg}.
#' @param \dots further arguments passed to or from other methods.
#' @return  further arguments passed to or from other methods.
#' @seealso \code{\link{censReg}}
#' @keywords regression
#' @importFrom USGSwsStats vif
#' @S3method vif censReg
#' @method vif censReg
vif.censReg <- function(model, ...) {
  ## Coding history:
  ##    2012Dec31 DLLorenz Initial Coding
  ##
  lm.x <- model$XLCAL[, -1L, drop=FALSE] # drop the intercept
  if(ncol(lm.x) == 1L)
    VIFs <- 1
  else {
    if(is.null(model$weights))
      VIFs <- diag(solve(cor(lm.x)))
    else
      VIFs <- diag(solve(cov.wt(lm.x, wt=model$weights, cor=TRUE)$cor))
  }
  names(VIFs) <- colnames(lm.x)
  return(VIFs)
}
