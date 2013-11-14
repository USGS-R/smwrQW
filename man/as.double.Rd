\name{as.double.qw}
\alias{as.double.qw}
\title{Numeric Representations}
\usage{
  \method{as.double}{qw} (x, ...)
}
\arguments{
  \item{x}{the object to be converted.}

  \item{\dots}{further arguments passed to or from other
  methods.}
}
\value{
  A numeric representation of \code{x}, using a method
  commonly refered to as simple substitution.
}
\description{
  Converts an object of class "qw" to a numeric values.
}
\note{
  The process of simple substitution proceeds in 4 steps
  for each kind of censoring: none, interval, left and
  right.  First uncensored data are retained as is.
  Interval-censored data are converted by computing the mid
  value. Left-censored data are converted sequentially from
  the lowest censoring level, which is set to 1/2 the
  reporting level, any subsequent levels are replaced by
  the mean of all values less than the current reporting
  level. Right-censored data are converted sequentially
  from the largest censoring level. Each level is replaced
  by the mean of all values greater than the current level,
  beginning with a value 10 percent larger than the largest
  censoring level if there are no larger uncensored
  values.\cr

  The function \code{as.double} is executed for
  \code{as.numeric}.
}
\examples{
## Create a simple qw object
Test.qw <- as.qw(c(1,2,3,2,4,4), c("<", "<", "<", " ", " ", " "), "", NA_real_,
"Miss", "None", "Mine", "TEST", "")
as.double(Test.qw)
as.numeric(Test.qw)
}
\references{
  Is one needed? Is there one that really describes the
  process?
}
\seealso{
  \code{\link{fillIn}}
}
\keyword{manip}

