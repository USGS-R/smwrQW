\name{summary.censReg}
\alias{summary.censReg}
\title{Summarizing Linear Model Fits}
\usage{
  \method{summary}{censReg} (object, correlation = FALSE,
    ...)
}
\arguments{
  \item{object}{an object of class "censReg"---output from
  \code{censReg}.}

  \item{correlation}{include the correlation matrix of the
  estimated parameters?}

  \item{\dots}{further arguments passed to or from other
  methods.}
}
\value{
  An object of class "summary.censReg" containing the
  object, variance inflation factors, a table of diagnostic
  statistics, critical values for selected diagnostic
  statistics, an indication of which observations exceed
  any of the selected diagnostic statistics, and optionally
  the parameter correlation matrix.
}
\description{
  Summaryizes the output from an AMLE/MLE regression
}
\seealso{
  \code{\link{censReg}}
}
\keyword{regression}

