\name{mdlKMstats}
\alias{mdlKMstats}
\title{Estimate Statistics}
\usage{
  mdlKMstats(x, probs = c(0.25, 0.5, 0.75))
}
\arguments{
  \item{x}{out from mdlKM or mcenKM}

  \item{probs}{numeric vector of desrired probabilities}
}
\value{
  A list containing summary statistics.
}
\description{
  Support function for computing statistics for left- and
  multiply censored data.
}
\keyword{misc}

