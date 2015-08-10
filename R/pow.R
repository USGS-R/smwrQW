#' @title Power Transforms
#'
#' @description The \code{pow} function provides a method for raising censored data to a positive power.
#'
#' @include lcens-class.R mcens-class.R qw-class.R
#' @param x any object that can be converted to class "mcens." Must be non negative (zero data are
#'permitted). Missing values are permitted and missing vlues in the returnned vector.
#' @param lambda the power to raise \code{x}. Must be strictly positive.
#' @return An object of class "mcens" that is the result of the requested operations. Each of the range of the 
#'lower and upper values are raised to the power of \code{lambda} and the result divided by \code{lambda}
#'to approximately maintain scale. But the reciprocal value of \code{lambda} cannot be used to restore the 
#'original values.
#' @examples
#' pow(as.mcens(c(0,2), c(1,2)), 2)
#' pow(as.mcens(c(0,2), c(1,2)), .5)
#' @export pow
pow <- function(x, lambda) {
	if(inherits(x, "qw")) { # use qw2mcens
		retval <- qw2mcens(x)
	} else if(inherits(x, "mcens")) {
		retval <- x
	} else { # convert it
		retval <- as.mcens(x)
	}
	# Checks
	if(lambda <= 0) {
		stop("lambda must be greater than 0")
	}
	if(any(retval@censor.codes < 0, na.rm=TRUE)) {
		stop("data must not be left-censored")
	}
	if(any(retval@.Data[, 1L] < 0, na.rm=TRUE)) {
		stop("data must be non negative")
	}
	# OK, do it
	retval@.Data <- retval@.Data^lambda/lambda
  return(retval)
}
