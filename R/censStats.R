#'Mean and Standard Deviation
#'
#'Compute the mean and standard deviation of censored data.
#'
#'The methods available in the current version are "log MLE," "MLE," "log ROS,"
#'"ROS," "log AMLE," "AMLE," and "flipped K-M." The method "flipped K-M"
#'produces statistics using the Kaplan-Meier method on flipped data described
#'by Helsel (2012). The methods "log ROS," "log MLE," and "log AMLE" are
#'described by Helsel (2012) and Helsel and Cohn (1988).  The methods "ROS,"
#'"MLE," and "AMLE" are similar to the previous except that no log- and
#'back-transforms are made on the data.
#'
#' @aliases censStats censStats.default censStats.lcens censStats.qw
#'censStats.mcens
#' @param x an object of a censored-data class whose sample mean and standard
#'deviation are needed.
#' @param method the method to use for computing the statistics. See
#'\bold{Details}.
#' @param na.rm logical; if \code{TRUE}, any NA and NaN's are removed from
#'\code{x} before the quantiles are computed.
#' @param alpha the offset fraction to be used, depending on \code{method};
#'typically in [0,1].
#' @return A list with one component for mean and sd. If "log" methods are
#'specified, then the meanlog amd meansd components are also included. The
#'values returned for \code{method}="flipped K-M" can be of class "mcens."
#' @seealso \code{\link{help}}
#' @references Helsel, D.R. 2012, Blah blah: some place, Wiley, some pages.\cr
#'Helsel, D.R. and Cohn, T.A., 1988, Estimation of descriptive statistics for
#'multiply censored water quality data,\: Water Resources Research v. 24, n.
#'12, pp.1997-2004
#' @keywords univariate
#' @examples
#'
#'set.seed(936)
#'Y <- rlnorm(32)
#'# Uncensored statistics
#'censStats(Y, method="log MLE")
#'# Censored statistics, at 1 (37.5 percent)
#'censStats(as.lcens(Y, 1), method="log MLE")
#'
#' @export
censStats <- function(x, method="MLE", na.rm=FALSE, alpha=0.4) {
  ## Coding history:
  ##    2012Mar27 DLLorenz Original Coding
  ##    2012Apr05 DLLorenz Completed methods, AMLE (last)
  ##    2012Oct21 DLLorenz Added class, necessary for printing
  ##    2013Jan01 DLLorenz Roxygenized, modifed default and added qw method
  ##    2013Jan01          This version
  ##
  UseMethod("censStats")
}

#' @rdname censStats
#' @S3method censStats default
#' @method censStats default
censStats.default <- function(x, method="MLE", na.rm=FALSE, alpha=0.4) {
  if(all(is.na(x)) || (!na.rm && any(is.na(x)))) {
    if(substring(method, 1, 3) == "log")
      retval <- list(mean=as.numeric(NA), sd=as.numeric(NA),
                     meanlog=as.numeric(NA),sdlog=as.numeric(NA))
    else
      retval <- list(mean=as.numeric(NA), sd=as.numeric(NA))
    class(retval) <- "censStats"
    return(retval)
  } else 
    x <- x[!is.na(x)]
  ## Treat x as numeric
  method <- match.arg(method, c("log MLE", "MLE", "log ROS", "ROS",
                                "log AMLE", "AMLE", "flipped K-M"))
  N=length(x) # Correction for MLE
  retval <- switch(method,
                   "log MLE"={meanlog=mean(log(x))
                              sdlog=sd(log(x))*sqrt((N-1)/N)
                              mn <- exp(meanlog + 0.5*sdlog^2)
                              vr <- (exp(sdlog^2) - 1)*
                                exp(2*meanlog + sdlog^2)
                              list(mean=mn, sd=sqrt(vr), meanlog=meanlog,
                                   sdlog=sdlog)},
                   "MLE"=list(mean=mean(x), sd=sd(x)*sqrt((N-1)/N)),
                   "log ROS"={step1 <- mdlROS(x, method=method, alpha=alpha)
                          list(mean=mean(step1$fitted), sd=sd(step1$fitted),
                            meanlog=step1$meanlog, sdlog=step1$sdlog)},
                   "ROS"={step1 <- mdlROS(x, method=method, alpha=alpha)
                          list(mean=step1$mean, sd=step1$sd)},
                   "log AMLE"=mdlAMLE(x, method=method, alpha=alpha)[1:4],
                   "AMLE"=mdlAMLE(x, method=method, alpha=alpha)[1:2],
                   "flipped K-M"=list(mean=mean(x), sd=sd(x)))
  class(retval) <- "censStats"
  return(retval)
}

#' @rdname censStats
#' @S3method censStats lcens
#' @method censStats lcens
censStats.lcens <- function(x, method="MLE", na.rm=FALSE, alpha=0.4) {
  method <- match.arg(method, c("log MLE", "MLE", "log ROS", "ROS",
                                "log AMLE", "AMLE", "flipped K-M"))
  if(all(is.na(x)) || (!na.rm && any(is.na(x)))) {
    if(substring(method, 1, 3) == "log")
      retval <- list(mean=as.numeric(NA), sd=as.numeric(NA),
                     meanlog=as.numeric(NA),sdlog=as.numeric(NA))
    else
      retval <- list(mean=as.numeric(NA), sd=as.numeric(NA))
    class(retval) <- "censStats"
    return(retval)
  }
  ## These methods automatically remove NAs
  retval <- switch(method,
                   "log MLE"={step1 <- mdlMLE(x, method=method)
                              mn <- exp(step1$meanlog + 0.5*step1$sdlog^2)
                              vr <- (exp(step1$sdlog^2) - 1)*
                                     exp(2*step1$meanlog + step1$sdlog^2)
                              list(mean=mn, sd=sqrt(vr), meanlog=step1$meanlog,
                                sdlog=step1$sdlog)},
                   "MLE"={step1 <- mdlMLE(x, method=method)
                          list(mean=step1$mean, sd=step1$sd)},
                   "log ROS"={step1 <- mdlROS(x, method=method, alpha=alpha)
                          list(mean=mean(step1$fitted), sd=sd(step1$fitted),
                            meanlog=step1$meanlog, sdlog=step1$sdlog)},
                   "ROS"={step1 <- mdlROS(x, method=method, alpha=alpha)
                          list(mean=step1$mean, sd=step1$sd)},
                   "log AMLE"=mdlAMLE(x, method=method, alpha=alpha)[1:4],
                   "AMLE"=mdlAMLE(x, method=method, alpha=alpha)[1:2],
                   "flipped K-M"={step1 <- mdlKMstats(mdlKM(x))[[1]]
                                  ## Compute the upper limit of the variance by
                                  ##   the simple method
                                  ## Compute total SS for min censor level
                                  TSS <- step1$stdev^2*(step1$obs[1] - 1)
                                  ## Adjust for substitute 0 for min censor level
                                  TSS <- TSS - step1$obs[1]*step1$censored*step1$mean[3]^2
                                  ## Account for difference in means
                                  TSS <- TSS + step1$obs[1]*(2*step1$mean[1] -
                                                             step1$mean[2])*step1$mean[2]
                                  sdmax <- sqrt(TSS/(step1$obs[1] - 1))
                                  list(mean=as.mcens(step1$mean[1] -  step1$mean[2], step1$mean[1]),
                                       sd=as.mcens(step1$stdev, sdmax))}
                   )
  class(retval) <- "censStats"
  return(retval)
}

#' @rdname censStats
#' @S3method censStats mcens
#' @method censStats mcens
censStats.mcens <- function(x, method="MLE", na.rm=FALSE, alpha=0.4) {
  if(all(is.na(x)) || (!na.rm && any(is.na(x)))) {
    if(substring(method, 1, 3) == "log")
      retval <- list(mean=as.numeric(NA), sd=as.numeric(NA),
                     meanlog=as.numeric(NA),sdlog=as.numeric(NA))
    else
      retval <- list(mean=as.numeric(NA), sd=as.numeric(NA))
    class(retval) <- "censStats"
    return(retval)
  }
  ## Only MLE and log MLE are valid
  method <- match.arg(method, c("log MLE", "MLE"))
  ## Convert to Surv
  time <-  miss2na(x@.Data[, 1L], -Inf)
  time2 <- miss2na(x@.Data[, 2L], Inf)
  Resp <- Surv(time, time2, type="interval2")
  retval <- switch(method,
                   "log MLE"={step1 <- survreg(Resp ~ 1, dist="lognormal")
                              meanlog <- as.vector(coefficients(step1))
                              sdlog <- step1$scale
                              mn <- exp(meanlog + 0.5*sdlog^2)
                              vr <- (exp(sdlog^2) - 1)*
                                exp(2*meanlog + sdlog^2)
                              list(mean=mn, sd=sqrt(vr), meanlog=meanlog,
                                   sdlog=sdlog)},
                   "MLE"={step1 <- survreg(Resp ~ 1, dist="gaussian")
                          list(mean=as.vector(coefficients(step1)),
                               sd=step1$scale)})
  class(retval) <- "censStats"
  return(retval)
}

#' @rdname censStats
#' @S3method censStats qw
#' @method censStats qw
censStats.qw <- function(x, method="log MLE", na.rm=FALSE, alpha=0.4) {
  ## Coding history:
  ##    2012Mar27 DLLorenz Original Coding
  ##
  method <- match.arg(method, c("log MLE", "MLE", "log ROS", "ROS",
                                "log AMLE", "AMLE", "flipped K-M"))
  ## Deal with missing values
  if(all(is.na(x)) || (!na.rm && any(is.na(x)))) {
    if(substring(method, 1, 3) == "log")
      retval <- list(mean=as.numeric(NA), sd=as.numeric(NA),
                     meanlog=as.numeric(NA),sdlog=as.numeric(NA))
    else
      retval <- list(mean=as.numeric(NA), sd=as.numeric(NA))
    class(retval) <- "censStats"
    return(retval)
  }
  ## Convert to appropriate type and use that method
  Cens <- censoring(x)
  if(Cens == "none")
    return(censStats.default(as.numeric(x), method=method, na.rm=na.rm))
  if(Cens == "left")
    return(censStats.lcens(as.lcens(x), method=method, na.rm=na.rm,
                           alpha=alpha))
  if(method %in% c("log ROS", "log AMLE")) {
    warning("method: ", method, " not supported for multiply censored data,",
            ' using "log MLE"')
    method <- "log MLE"
  }
  else if(method %in% c("ROS", "AMLE", "flipped K-M")) {
    warning("method: ", method, " not supported for multiply censored data,",
            ' using "MLE"')
    method <- "MLE"
  }
  return(censStats.mcens(as.mcens(x), method=method, na.rm=na.rm))
}
