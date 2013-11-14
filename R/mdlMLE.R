#'Estimate Statistics
#'
#'Support function for computing statistics for left-censored data.
#'
#'@param x the data to estimate, Missing values permitted and ignored.
#'Must be an object of class "lcens," a numeric vector, or the output from censpp.
#'@param method the method to use, either "MLE" or "log MLE."
#'@param alpha the offset for plotting postion (not used).
#'@return A list containing the mean and standard deviation, filled in
#'values for the censored values, and the maximum censor level.
#'@keywords misc
#'@export
mdlMLE <- function(x, method="MLE", alpha=0.4) {
  ## Coding history:
  ##    2012Mar09 DLLorenz original coding
  ##    2013Jan05 DLLorenz Roxygenized
  ##    2013Jan05          This version
  ##
  method <- match.arg(method, c("MLE", "log MLE"))
  if(class(x) != "list")
    x <- censpp(x, a=alpha)
  step1 <- Surv(c(x$x, x$xcen), c(rep(1, length(x$x)), rep(0, length(x$xcen))),
                type="left")
  if(method == "MLE") {
    step2 <- survreg(step1 ~ 1, dist="gaussian")
    coefs <- as.vector(c(step2$coefficients, step2$scale))
    step3 <- qnorm(x$ppcen) *  coefs[2L] + coefs[1L]
    step4 <- as.vector(c(step3, x$x))
    retval <- list(mean=coefs[1L], sd=coefs[2L], fitted=step4)
  }
  else {
    step2 <- survreg(step1 ~ 1, dist="lognormal")
    coefs <- as.vector(c(step2$coefficients, step2$scale))
    step3 <- qnorm(x$ppcen) *  coefs[2L] + coefs[1L]
    step4 <- as.vector(c(exp(step3), x$x))
    retval <- list(meanlog=coefs[1L], sdlog=coefs[2L], fitted=step4)
  }
  if(length(x$xcen) > 0L)
    retval$censorlevels <- x$xcen
  else
    retval$censorlevels  <- -Inf
  return(retval)
}
