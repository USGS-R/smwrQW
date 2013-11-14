\name{as.lcens}
\alias{as.lcens}
\title{Left-censored Data Conversion}
\arguments{
  \item{values}{numeric values representing "raw" values
  from a sensor.}

  \item{detlim}{the corresponding detection limit of the
  sensor.}

  \item{censor.codes}{optional codes indicating a
  left-censored value. If class "logical," then \code{TRUE}
  indicates left-censored. If class "character," then "<"
  indicates left-censored and anything other than "" or " "
  generates a warning.}
}
\value{
  An S4 object of class "lcens" having these slots:
  \item{.Data}{ a matrix of the the censored values and the
  detection limits.} \item{censor.codes}{ logical value
  indicating a left-censored value.}
}
\description{
  Convert from various formats to a left-censored data
  (\code{lcens}) object.
}
\note{
  All methods force \code{values} to be no greater than the
  corresponding \code{detlim}. This ensures unbiased
  analytical results.
}
\keyword{manip}

