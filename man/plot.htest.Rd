\name{plot.htest}
\alias{plot.htest}
\alias{plot.ppw}
\title{Plot}
\usage{
  \method{plot}{htest} (x, which = "All", set.up = TRUE,
    ...)

  \method{plot}{ppw} (x, which = "All", set.up = TRUE,
    labels, ...)
}
\arguments{
  \item{x}{an object having classes "htest" and some other
  class for which a \code{plot} method exists.}

  \item{which}{either "All" or an number indicating which
  plot, see \bold{Details}.}

  \item{set.up}{set up the graphics page?}

  \item{labels}{use supplied values for y-axis labels
  instead of sequence number.}

  \item{\dots}{further arguments passed to or from other
  methods.}
}
\value{
  The object \code{x} is returned invisibly.
}
\description{
  Create diagnostic plots for selected hypothesis tests.
}
\details{
  The \code{ppw} method for \code{plot} creates 2 graphs on
  a single page. The first graph is the scaled differences
  from the PPT test with the mean difference shown by a
  blue line. The second graph is the actual differences in
  the paird values, assuming that the true minimum for
  left-censored data is 0. The median difference is shown
  by a blue line.
}
\keyword{hplot}

