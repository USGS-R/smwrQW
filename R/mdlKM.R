#' @title Estimate Statistics
#'
#' @description Support function for computing statistics for left-censored data.
#'
#' @importFrom survival survfit Surv
#' @param x an object of "lcens" to compute 
#' @param group the group variable
#' @return An object of class "survfit."
#' @keywords misc
#' @export
mdlKM <- function(x, group) {
  ## Coding history:
  ##    2005Mar09 DLLorenz Initial Coding.
  ##    2005Mar23 DLLorenz Initial working version
  ##    2005Jul12 DLLorenz Bug fix
  ##    2005Nov16 DLLorenz Handling of all NAs
  ##    2006Feb21 DLLorenz Switch logx off if zero values found
  ##    2006Mar20 DLLorenz Suppress printing of censoring in summary if none
  ##    2007Jan29 DLLorenz Modified to fail on infoRich coding
  ##    2007Feb09 DLLorenz Added more options to print range of quantiles
  ##    2007Oct12 DLLorenz Bug fix in plot and added setMethod
  ##    2008Sep30 DLLorenz Modified summary to accept probabilities,
  ##                       and print.summary to print just a little more info
  ##    2008Oct01 DLLorenz bug fix
  ##    2008Oct09 DLLorenz remove the call to setMethod for plot.
  ##                       and modifed summary to merge with mdlpar()
  ##    2008Oct20 DLLorenz removed unused ... argument to print
  ##    2012Mar08 DLLorenz Conversion to R with change in purpose to support func.
  ##    2013Jan02 DLLorenz Roxygenized
  ##    2013Jan02          This version
  ##
  pvalues <- x@.Data[, 1]
  rvalues <- x@censor.codes
  ## remove NAs from data
  Good.data <- !is.na(pvalues)
  if(sum(Good.data) > 0) { # At least one value
    pvalues <- -pvalues[Good.data] # reverse data
    rvalues <- !rvalues[Good.data] # reverse sense for survfit
    if(missing(group))
      retval <- survfit(Surv(pvalues, rvalues) ~ 1)
    else {
      group <- group[Good.data]
      retval <- survfit(Surv(pvalues, rvalues) ~ group)
    }
  }
  else # no data
    retval <- list(NoData=TRUE)
  return(retval)
}
