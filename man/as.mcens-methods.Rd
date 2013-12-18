\docType{methods}
\name{as.mcens-methods}
\alias{as.mcens-methods}
\alias{as.mcens,lcens,missing,missing-method}
\alias{as.mcens,mcens,missing,missing-method}
\alias{as.mcens,numeric,missing,character-method}
\alias{as.mcens,numeric,missing,missing-method}
\alias{as.mcens,numeric,missing,numeric-method}
\alias{as.mcens,numeric,numeric,missing-method}
\alias{as.mcens,qw,missing,missing-method}
\title{Methods for Function \code{as.mcens}}
\description{
  Valid conversion methods for function \code{as.mcens}.
}
\section{Methods}{
  \describe{

  \item{list(signature(lower.val = "lcens", upper.val =
  "missing", censor.codes = "missing"))}{ Objects of class
  "lcens" are converted to "mcens." }

  \item{list(signature(lower.val = "mcens", upper.val =
  "missing", censor.codes = "missing"))}{ No change to
  objects of class "mcens." }

  \item{list(signature(lower.val = "numeric", upper.val =
  "missing", censor.codes = "character"))}{ The values in
  \code{lower.val} are interpreted by \code{censor.codes}:
  "<", the actual value is less than \code{lower.val}; ">"
  the actual value is greater than \code{lower.val}; any
  other value for \code{censor.codes} the value is
  uncensored. }

  \item{list(signature(lower.val = "numeric", upper.val =
  "missing", censor.codes = "missing"))}{ All values in
  \code{lower.val} are treated as uncensored. }

  \item{list(signature(lower.val = "numeric", upper.val =
  "missing", censor.codes = "numeric"))}{ The values in
  \code{lower.val} are interpreted by \code{censor.codes}:
  -1, the actual value is less than \code{lower.val}; 1 the
  actual value is greater than \code{lower.val}; and 0 the
  value is uncensored. }

  \item{list(signature(lower.val = "numeric", upper.val =
  "numeric", censor.codes = "missing"))}{ The values in
  \code{lower.val} are the lowest value and the values in
  \code{upper.val} are the upper value representing each
  vlaue. }

  \item{list(signature(lower.val = "qw", upper.val =
  "missing", censor.codes = "missing"))}{ Objects of class
  "qw" are converted to "mcens." Values coded as
  "less-than" are converted to left- censored values. For
  conversion to interval-censored, use \code{qw2mcens}.} }
}
\examples{
## The first value is left-censored at 2, second is interval,
##  third is observed, fourth is right-censored, last is missing
as.mcens(c(-Inf,2,3,3, NA), c(2,3,3,NA, NA))
}
\seealso{
  \code{\link{qw2mcens}}
}
\keyword{manip}
\keyword{methods}

