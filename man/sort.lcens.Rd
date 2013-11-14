\name{sort.lcens}
\alias{sort.lcens}
\alias{sort.mcens}
\title{Sorting Vectors}
\usage{
  \method{sort}{lcens}(x, decreasing = FALSE, na.last = NA,
    ...)

  \method{sort}{mcens}(x, decreasing = FALSE, na.last =
    TRUE, ...)
}
\arguments{
  \item{x}{the data to be sorted.}

  \item{decreasing}{sort the data in descending order?}

  \item{na.last}{for controlling the treatment of
  \code{NA}s. If \code{TRUE}, then missing values in the
  data are put last; if \code{FALSE}, then they are put
  first; if \code{NA}, then they are removed.}

  \item{\dots}{arguments to be passed to or from methods.}
}
\value{
  An object like \code{x}, but with the data sorted.
  Left-censored values are placed between smaller numeric
  values and the corresponding uncensored values.
}
\description{
  Sort a censored-data object.
}
\examples{
set.seed(5422)
XR <- rnorm(10)
sort(as.lcens(XR, rep(c(0, -1), each=5))) # censors at 0 and -1
}
\keyword{manip}

