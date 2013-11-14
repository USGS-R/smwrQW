\name{format.lcens}
\alias{format.lcens}
\alias{format.mcens}
\alias{format.qw}
\title{Encode in a Common Format}
\usage{
  \method{format}{lcens} (x, digits = NULL, ...)

  \method{format}{mcens} (x, digits = NULL, ...)

  \method{format}{qw} (x, round = TRUE, units = FALSE, ...)
}
\arguments{
  \item{x}{the censored data object to format.}

  \item{digits}{how many significant digits are to be used
  for numbers?}

  \item{\dots}{further arguments passed to or from other
  methods.}

  \item{units}{include the units of the data?}

  \item{round}{round the data before formating? Can also be
  a numeric vector of length 2 containing the maximum
  number of significant digits and the maximum number of
  decimal digits.}
}
\value{
  A character representations of the elements of \code{x}.
}
\description{
  Format a censored or water-quality data object for pretty
  printing.
}
\examples{
format(as.lcens(1:3, 1))
format(as.mcens(1:3, 1:3))
}
\keyword{manip}

