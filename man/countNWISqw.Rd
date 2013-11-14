\name{countNWISqw}
\alias{countNWISqw}
\title{Count Water-Quality Records}
\usage{
  countNWISqw(sites, by = "summary")
}
\arguments{
  \item{sites}{the USGS station identifiers.}

  \item{by}{character string indicating how to summarize
  the counts. Must be one of "summary," "total," "group,"
  or "pcode" indicating summary output, the total number of
  observations, the number of observations by parameter
  group, or the number of observations for each parameter
  code.}
}
\value{
  If \code{by} is "summary," then a data frame of the
  station identifiers, the begin and end dates of the
  samples, and the total number of samples. Otherwise, a
  list, organized by each site in \code{sites}, of the
  begin and end dates of the sampled data and a table of
  the counts. Verify if counts of samples or results for
  other than summary.
}
\description{
  Returns the number of observations
}
\note{
  The summary retrieval is a fast, and easy way to see how
  many samples have been taken at at site.  The other
  options can require several seconds to retrieve and
  process the data and provide a count for each parameter,
  not sample.
}
\examples{
\dontrun{
countNWISqw("05330000")
}
}
\keyword{misc}

