#' @title Estimate Censored Values
#'
#' @description Estimates values of censored data.
#'
#' @details The methods of Regression on Order Statistics (ROS) and MLE is
#'explained in Helsel (2012). The "log ROS" first log-transform the data and
#'back-transforms the estimated values. The triangular method distributes the
#'censored value assuming a triangular distribution between 0 and the single
#'detection limit.
#'
#' @param x an object of class "lcens." Missing values are ignored.
#' @param method the method to use for estimating censored values:
#'"ROS," "log ROS," "MLE," "log MLE," or "triangular" are valid for left-
#'censored data; "ROS," "log ROS," "MLE," or "log MLE,"are valid for multiply 
#'censored data; ignored for uncensored data.
#' @param alpha the offset fraction to be used; typically in [0,0.5].
#' @return A vector of sorted estimates and actual values with an attribute of the 
#'censoring levels.
#' @seealso \code{\link{as.double.qw}}
#' @references Helsel, D.R. 2012, Blah blah: some place, Wiley, some pages.\cr
#' @keywords manip
#' @examples
#'set.seed(5420)
#'XR <- sort(rnorm(10))
#'XF <- fillIn(as.lcens(XR, -1)) # censors lowest 3 values
#'#How'd we do?
#'rbind(XR, XF)
#'#Note that this is unusual because all 10 random values were less than the mean!
#' @export
fillIn <- function(x, method, alpha) {
  ## Coding history:
  ##    2012Mar09 DLLorenz original coding
  ##    2012Oct18 DLLorenz Added more methods
  ##    2013Jan01 DLLorenz Roxygenized
  ##    2014Sep04 DLLorenz conversion to methods
  ##
	UseMethod("fillIn")
}

#' @rdname fillIn
#' @export
#' @method fillIn numeric
fillIn.numeric <- function(x, method, alpha) {
	## The arguments method and alpha are ignored
	retval <- sort(x)
	attr(retval, "censorlevels") <- -Inf
}

#' @rdname fillIn
#' @export
#' @method fillIn lcens
fillIn.lcens <- function(x, method="ROS", alpha=.4) {
	method <- match.arg(method, c("ROS", "log ROS", "MLE", "log MLE",
																"triangular"))
	temp <- switch(method,
								 "ROS" = mdlROS(x, method=method, alpha=alpha),
								 "log ROS" = mdlROS(x, method=method, alpha=alpha),
								 "MLE" = mdlMLE(x, method=method, alpha=alpha),
								 "log MLE" = mdlMLE(x, method=method, alpha=alpha),
								 "triangular" = {CL <- censorLevels(x)
								 								if(length(CL) > 2L)
								 									stop("triangular method only for single detction limit")
								 								if(CL == -Inf)
								 									return(list(fitted=sort(x@.Data[, 1L]),
								 															censorlevels=double(0)))
								 								## Create fill-in values assuming a triangular distribution
								 								x <- x[!is.na(x)]
								 								xu <- sort(x@.Data[!x@censor.codes, 1L])
								 								xc <- sqrt(ppoints(sum(x@censor.codes), alpha)) * CL
								 								list(fitted=c(xc, xu),
								 										 censorlevels=rep(CL, length(xc)))
								 })
	retval <- temp$fitted
	attr(retval, "censorlevels") <- temp$censorlevels
	return(retval)
}

#' @rdname fillIn
#' @export
#' @method fillIn mcens
fillIn.mcens <- function(x, method="ROS", alpha=.4) {
	method <- match.arg(method, c("ROS", "log ROS", "MLE", "log MLE"))
	temp <- switch(method,
								 "ROS" = mcenROS(x, method=method, alpha=alpha),
								 "log ROS" = mcenROS(x, method=method, alpha=alpha),
								 "MLE" = mcenMLE(x, method=method, alpha=alpha),
								 "log MLE" = mcenMLE(x, method=method, alpha=alpha))
	retval <- temp$fitted
	attr(retval, "censorlevels") <- temp$censorlevels
	return(retval)
}
	
	
	
