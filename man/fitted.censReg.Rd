\name{fitted.censReg}
\alias{fitted.censReg}
\title{Extract Model Fitted Values}
\usage{
  \method{fitted}{censReg} (object,
    suppress.na.action = FALSE, ...)
}
\arguments{
  \item{object}{an object of class "censReg"---output from
  \code{censReg}}

  \item{suppress.na.action}{logical, suppress the effects
  of the \code{na.action} in the call to \code{censReg} and
  return only the fitted values corresponding to the fitted
  data.}

  \item{\dots}{further arguments passed to or from other
  methods.}
}
\value{
  The fitted values from the regression.
}
\description{
  Extract the fitted values of an AMLE/MLE regression.
}
\seealso{
  \code{\link{censReg}}
}
\keyword{regression}

