#'Numeric Representations
#'
#'Converts an object of class "qw" to a numeric values.
#'
#' @param x the object to be converted.
#' @param \dots further arguments passed to or from other methods.
#' @return A numeric representation of \code{x}, using a method commonly refered
#'to as simple substitution.
#' @note The process of simple substitution proceeds in 4 steps for each kind of
#'censoring: none, interval, left and right.  First uncensored data are
#'retained as is. Interval-censored data are converted by computing the mid
#'value. Left-censored data are converted sequentially from the lowest
#'censoring level, which is set to 1/2 the reporting level, any subsequent
#'levels are replaced by the mean of all values less than the current reporting
#'level. Right-censored data are converted sequentially from the largest
#'censoring level. Each level is replaced by the mean of all values greater
#'than the current level, beginning with a value 10 percent larger than the
#'largest censoring level if there are no larger uncensored values.\cr
#'
#'The function \code{as.double} is executed for \code{as.numeric}.
#' @seealso \code{\link{fillIn}}
#' @references Is one needed? Is there one that really describes the process?
#' @keywords manip
#' @examples
#'
#'## Create a simple qw object 
#'Test.qw <- as.qw(c(1,2,3,2,4,4), c("<", "<", "<", " ", " ", " "), "", NA_real_,
#' "Miss", "None", "Mine", "TEST", "")
#'as.double(Test.qw)
#'as.numeric(Test.qw)
#'
#' @rdname as.double
#' @S3method as.double qw
#' @method as.double qw
as.double.qw <- function(x, ...) {
  Cen <- censoring(x)
  if(Cen == "none") # Just return the raw data
    return(x@.Data[,1L])
  if(Cen == "left") {
    ## Convert to lcens and process those data
    x <- as.lcens(x)
    Cl <- censorLevels(x)
    retval <- x@.Data[, 1L]
    if(length(Cl) == 1L && Cl == -Inf)
      return(retval) # no censoring
    for(i in Cl) {
      picks <- na2miss(retval < i, FALSE)
      if(any(picks))
        xltCl <- mean(retval[picks])
      else
        xltCl <- i/2
      picks <- na2miss(retval == i & x@censor.codes, FALSE)
      retval[picks] <- xltCl
    }
    return(retval)
  }
  ## Cen must be "multiple"
  stop("multiply censored not yet implemented")
}
