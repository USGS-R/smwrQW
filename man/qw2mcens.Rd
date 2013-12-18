\name{qw2mcens}
\alias{qw2mcens}
\title{Conversion to class "mcens"}
\usage{
  qw2mcens(x)
}
\arguments{
  \item{x}{an object of class "qw" to convert.}
}
\value{
  A vector of the data converted to class "mcens" retaining
  interval censoring for "less-than" data.
}
\description{
  Convert objects of class "qw" to "mcens" retaining
  interval censoring for "less-than" data.
}
\seealso{
  \code{\link{as.mcens}}
}

