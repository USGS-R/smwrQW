\name{censReg_AMLE.fit}
\alias{censReg_AMLE.fit}
\alias{censReg_MLE.fit}
\title{Support Functions for Censored Regression}
\usage{
  censReg_AMLE.fit(Y, X, dist = "normal")

  censReg_MLE.fit(Y, X, Wt, dist = "normal")
}
\arguments{
  \item{Y}{the response data.}

  \item{X}{a matrix of the explanatory variables, including
  the intercept term.}

  \item{dist}{the distribuiton, either "normal" or
  "lognormal."}

  \item{Wt}{observations weights.}
}
\value{
  a list containing the output from the FORTRAN code,or
  converted from survreg.
}
\description{
  These are support functions for fitting censored
  regression. They perform either AMLE or MLE regression.
}
\seealso{
  \code{\link{censReg}}, \code{\link{survreg}}
}
\keyword{censored}
\keyword{utilities}

