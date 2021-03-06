% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logLik.censReg.R
\name{logLik.censReg}
\alias{logLik.censReg}
\title{Extract Log-Likelihood}
\usage{
\method{logLik}{censReg}(object, ...)
}
\arguments{
\item{object}{the output from \code{censReg}.}

\item{\dots}{further arguments passed to or from other methods.}
}
\value{
An object of class "logLik" containing the log-likelihood and
the attributes "df" (degrees of freedom) and "nobs" (number of observations).
}
\description{
Extracts the log-likelihood statistic from a censored regression object: 
method for "censReg" object.
}
\seealso{
\code{\link{censReg}},
}
