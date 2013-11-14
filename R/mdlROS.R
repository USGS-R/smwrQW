#'Estimate Statistics
#'
#'Support function for computing statistics for left-censored data.
#'
#'@param x the data to estimate, Missing values permitted and ignored.
#'Must be an object of class "lcens," a numeric vector, or the output from censpp.
#'@param method the method to use, either "ROS" or "log ROS."
#'@param alpha the offset for plotting postion.
#'@return A list containing the mean and standard deviation, filled in
#'values for the censored values, and the maximum censor level.
#'@keywords misc
#'@export
mdlROS <- function(x, method="ROS", alpha=0.4) {
  ## Coding history:
  ##    2012Mar09 DLLorenz original coding
  ##    2013Jan05 DLLorenz Roxygenized
  ##    2013Jan05          This version
  ##
  method <- match.arg(method, c("ROS", "log ROS"))
  if(class(x) == "list")
    step1 <- x
  else
    step1 <- censpp(x, a=alpha)
  if(method == "ROS") {
    step2 <- with(step1, lm(x ~ qnorm(pp)))
    step3 <- predict(step2, newdata=data.frame(pp=step1$ppcen))
    step4 <- as.vector(c(step3, step1$x))
    coefs <- as.vector(coef(step2))
    retval <- list(mean=coefs[1], sd=coefs[2], fitted=step4)
  }
  else {
    step2 <- with(step1, lm(log(x) ~ qnorm(pp)))
    step3 <- predict(step2, newdata=data.frame(pp=step1$ppcen))
    step4 <- as.vector(c(exp(step3), step1$x))
    coefs <- as.vector(coef(step2))
    retval <- list(meanlog=coefs[1], sdlog=coefs[2], fitted=step4)
  }
  if(length(step1$xcen) > 0)
    retval$censorlevels <- step1$xcen
  else
    retval$censorlevels  <- -Inf
  return(retval)
}
