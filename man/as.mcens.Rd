\name{as.mcens}
\alias{as.mcens}
\title{Multiply-censored Data Conversion}
\arguments{
  \item{lower.val}{The lower limit of the actual value, the
  special value of \code{-Inf} or \code{NA} can be used to
  indicate left-censoring.}

  \item{upper.val}{The upper limit of the actual value, the
  special value of \code{Inf} or \code{NA} can be used to
  indicate right-censoring.}

  \item{censor.codes}{optional codes if \code{upper.val} is
  missing. Any numeric value less than 0 indicates
  left-censored, any value greater than 0 indicates
  right-censored, and 0 indicates an observed value. The
  character value "<" indicates left-censored, ">"
  indicates right-censored, any anything else indicates an
  observed value.}
}
\value{
  An S4 object of class "lcens" having these slots:
  \item{.Data}{ a matrix of two columns, the lower value of
  the range of the data, -Inf indicates left-censored, and
  the upper value of the range of the data, Inf indicates
  right-censored.} \item{censor.codes}{ integer value, -1
  is left-censored, 0 is observed or interval-censored, and
  1 is right-censored.} \item{interval}{ logical,
  \code{TRUE} indicates interval censored data.}
}
\description{
  Convert from various formats to a multiply-censored data
  (\code{mcens}) object.
}
\details{
  In keeping with the logic of \code{Surv}, \code{NA} is
  permitted to indicate left- or right-censored data. If
  both are \code{NA}, then the observation is treated as
  missing.
}
\note{
  Note that \code{censor.codes} and \code{interval} are not
  necessary, but included to make some manipulations
  easier.
}
\seealso{
  \code{\link{Surv}}
}
\keyword{manip}

