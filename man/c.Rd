\name{c.lcens}
\alias{c.lcens}
\alias{c.mcens}
\alias{c.qw}
\title{Combine Values into a Vector}
\usage{
  \method{c}{lcens} (..., recursive = FALSE)

  \method{c}{mcens} (..., recursive = FALSE)

  \method{c}{qw} (..., recursive = FALSE)
}
\arguments{
  \item{\dots}{any objects that can be coerced to the class
  of the first one.}

  \item{recursive}{not used.}
}
\value{
  A vector of an appropriate class.
}
\description{
  These method combine arguments to form a vector. All
  arguments are coerced to a common type which is the type
  of the first argument, except for the qw method,
}
\examples{
c(as.lcens(c(1,3), 2), 2)
}

