#' Empirical Cumulative Percent
#' 
#' Computes the empirical cumulative percent or percent exceedance of observed data
#'for specific values: methods for "lcens" and "qw" data.
#' 
#' 
#' @aliases percentile.lcens percentile.qw
#' @param x a vector of class "lcens" or "qw" representing the observed data.
#' @param q a vector of numeric quantiles for which the cumulative percent or percent
#' exceedence is desired.
#' @param test a character string indicating the test. The default value, '>=,'
#' is the percent equalling or exceeding the quantile and '<' would return the
#' cumulative percent below the quantile.
#' @param na.rm a logical value indication whether missing values (NAs) should
#' be removed or not. If na.rm is \code{FALSE} and there are missing values in
#' \code{x}, then the result will be NA.  The default value is \code{TRUE}.
#' @param percent a logical value indicating whether the result should be
#' expressed as a percent or proportion. The default value, \code{TRUE}, will
#' express the result as a percent.
#' @param exact logical, if \code{TRUE}, then force strict comparisons. Any 
#' other value is ignored in the current version.
#' @param \dots not used, required for method function
#' @return A named vector as long as \code{q} corresponding to the requested
#' value.
#' @note The stats package contains the \code{ecdf} function that performs a
#' similar function when \code{test} is "<=."
#' @seealso \code{\link{ecdf}}
#' @keywords univar math manip
#' @examples
#' 
#' set.seed(2342)
#' Xr <- rlnorm(24)
#' # The percentage of the observarions greater than or equal to 2
#' percentile(as.lcens(Xr, 2), 1:5)
#' # Compare to the raw data:
#' percentile(Xr, 1:5)
#' 
#' @importFrom smwrStats percentile
#' @export 
#' @method percentile lcens
percentile.lcens <- function(x, q, test='>=', na.rm=TRUE,
															 percent=TRUE, exact=TRUE, ...) {
	## The option for exact is included, but not implemented. If exact is set to
	## FALSE, then a weighting scheme like Kaplan-Meier can be employed to 
	## distribute the weights of larger less-thans over smaller values.
	## For example (2, <2, 1.5, <1) would distribute <2 equally to 1.5 and <1,
	## giving each one the equivalent of 1.5 observations.
	##
	retval <- double(length(q))
	check <- test
	test <- get(test)
	if(na.rm)
		x <- x[!is.na(x)]
	N <- length(x)
	for(i in seq(along=q)) {
		Ntest <- sum(test(x, q[i]))
		retval[i] <- Ntest/N
	}
	if(percent) {
		retval <- retval *100
		names(retval) <- paste("Percent", check, q, sep=' ')
	}
	else
		names(retval) <- paste("Proportion", check, q, sep=' ')
	return(retval)
}

#' @rdname percentile.lcens
#' @export 
#' @method percentile qw
percentile.qw <- function(x, q, test='>=', na.rm=TRUE,
														 percent=TRUE, exact=TRUE, ...) {
	##
	if(censoring(x) == "none") {
		percentile(as.numeric(x))
	} else if(censoring(x) == "left") {
		percentile(as.lcens(x))
	} else 
		stop("percentile not supported for right- or multiply-censored data")
}
