\name{coef.censReg}
\alias{coef.censReg}
\title{Extract Model Coefficients}
\usage{
  \method{coef}{censReg} (object, summary = FALSE, ...)
}
\arguments{
  \item{object}{the output from \code{censReg}.}

  \item{summary}{include standard errors and other
  information?}

  \item{\dots}{further arguments passed to or from other
  methods.}
}
\value{
  Either a names vector of the coefficients, if
  \code{summary} is \code{FALSE} or a matrix of the
  coefficients, their standard errors, z-scores, and
  attained p-values, if \code{summary} is \code{TRUE}.
}
\description{
  Extract the model coefficients from an AMLE/MLE
  regression.
}
\note{
  The attained p-values are computed from the
  log-likelihood test for AMLE regression and from a Wald
  chi-square test for MLE regression.
}
\seealso{
  \code{\link{censReg}},
}

