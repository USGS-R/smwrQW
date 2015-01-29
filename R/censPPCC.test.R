#'Test for Normality
#'
#'Generates the sequence of probability points for uncensored and censored
#'values
#'
#'
#' @param x a vector of left-censored observations, missing values are
#'removed before computation.
#' @return An object of class "htest" having these components:
#'
#'\item{statistic}{ the value of the test statistic.}
#'\item{p.value}{ the attained p-value for the test.}
#'\item{data.name}{ a character string describing the name of the data 
#'used in the test.}
#'\item{method}{ a description of the method.}
#'\item{alternative}{ a description of the null and alternate
#'hypotheses.}
#' @note The PPCC test is attractive because it has a simple, graphical
#'interpretation: it is a measure of the correlation in a Q-normal plot
#'of the uncensored data and is related to the Shapiro-Francia test for 
#'normality described by Royston (1993). The extension to multiple detection 
#'limit data is described in the help files in Millard (2001). The attained
#'p-value for multiple detection limit data is only approximate.
#' @seealso \code{\link{ppcc.test}}, \code{\link{censpp}}
#' @references Millard, S.P., 2001, EnvironmentalStats for Spotfire S+ Help,
#'Version 2.0. Probability, Statistics & Information, Seattle, WA.\cr
#'Royston, P., 1993, A toolkit for testing for non-normality in complete and
#'censored samples: The Statistician, v. 42, p. 37--43.
#' @keywords htest
#' @useDynLib smwrQW ppcctest
#' @export
censPPCC.test <- function(x) {
  ## Coding history:
  ##    2013Feb11 DLLorenz Original Coding
  ##    2013Apr10 DLLorenz Tweaks
  data.name <- deparse(substitute(x))
  x <- as.lcens(x)
  x <- x[!is.na(x)]
  N <- as.integer(length(x))
  CenFlag <- x@censor.codes
  Y <- x@.Data[,1L]
  ret <- .Fortran("ppcctest",
                  CenFlag, N, Y,
                  R=double(1L),
                  PLEV=double(1L),
                  NUNCEN=integer(1L),
                  IERR=integer(1L))
  if(ret$IERR > 0) {
    if(ret$IERR == 1)
      stop("Fewer than 3 uncensored values")
    if(ret$IERR == 2)
      stop("Fewer than 2 unique uncensored values")
    if(ret$IERR == 3)
      stop("Internal error")
    if(ret$IERR == 4)
      stop("More than 5000 values")
  }
  else if(ret$IERR == 0)
    method <- "PPCC Normality Test for single detection limit"
  else
    method <- "PPCC Normality Test for multiple detection limit"
  Stat <- ret$R
  names(Stat) <- paste("r(", round(100*(1 - ret$NUNCEN/N), 0), " percent censored)", sep="")
  retval <- list(statistic=Stat, p.value=ret$PLEV, data.name=data.name,
                  alternative="Data are not from a normal distribution\nnull hypothesis: Data are from a normal distribution", 
                  method=method)
  class(retval) <- "htest"
  return(retval)
}
