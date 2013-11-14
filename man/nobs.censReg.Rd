\name{nobs.censReg}
\alias{nobs.censReg}
\title{Extract the Number of Observations from a Fit}
\usage{
  \method{nobs}{censReg} (object, ...)
}
\arguments{
  \item{object}{an object of class "censReg"---output from
  \code{censReg}.}

  \item{\dots}{further arguments passed to or from other
  methods.}
}
\value{
  The number of observations used to construct the model.
}
\description{
  Extract the number of observations from an AMLE/MLE
  regression.
}
\seealso{
  \code{\link{censReg}}
}
\keyword{regression}

