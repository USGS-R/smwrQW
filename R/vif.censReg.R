#' @title Variance Inflation Factors
#'
#' @description Computes the variance inflation factor (Helsel and Hirsch, 2002) for each
#'variable in an AMLE/MLE regression.
#'
#' @param model an object of class "censReg"---output from \code{censReg}.
#' @param \dots not used, further arguments passed to or from other methods.
#' @return  A named numeric vector containing the variance inflation factors 
#'for each variable.
#' @seealso \code{\link{censReg}}
#' @references Helsel, D.R. and Hirsch, R.M., 2002, Statistical methods in 
#' water resources: U.S. Geological Survey Techniques of Water-Resources 
#' Investigations, book 4, chap. A3, 522 p.
#' @keywords regression
#' @importFrom USGSwsStats vif
#' @export
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
