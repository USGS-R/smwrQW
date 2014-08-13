#' Arithmetic Mean
#'
#' These functions are designed to stop the user from accidentally using the
#'function \code{mean} on censored data.
#'
#'
#' @aliases mean.lcens mean.mcens
#' @param x the censored data object.
#' @param \dots further arguments passed to or from other methods.
#' @return An error message.
#' @note The mean and standard deviation of censored data should be computed
#'using \code{censStats}.
#' @seealso \code{\link{censStats}}
#' @keywords censored univariate
#' @rdname meanStats
#' @export
#' @method mean lcens
mean.lcens <- function(x, ...)
  stop("mean not supported for objects of class 'lcens' use censStats")

#' @rdname meanStats
#' @export
#' @method mean mcens
mean.mcens <- function(x, ...)
  stop("mean not supported for objects of class 'mcens' use censStats")
