\name{makeColNames}
\alias{makeColNames}
\title{Column Names}
\usage{
  makeColNames(params, name, short)
}
\arguments{
  \item{params}{the parameter codes.}

  \item{name}{the long-name of the parameter code.}

  \item{short}{the short name of teh parameter code.}
}
\value{
  A data frame containing the columns \code{parm_cd} and
  \code{col_name}.
}
\description{
  Create valid, unique column names for water-quality
  constituents
}
\details{
  The arguments \code{name} and \code{short} are expected
  to be columns retreived by those arguments in the
  function \code{pcodeNWISqw}.
}
\note{
  This function is a support function designed to be called
  within other functions.
}
\seealso{
  \code{\link{pcodeNWISqw}}, \code{\link{pcodeColNames}}
}
\keyword{utilities}

