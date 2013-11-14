\name{as.qw}
\alias{as.qw}
\title{Water-quality Data Conversion}
\arguments{
  \item{values}{the uncensored values and optionally the
  censored values.}

  \item{remark.codes}{the remarks codes: " " or ""
  indicated uncensored, "<" indicates left-censoring, ">"
  indicates right censoring. Other values have a special
  meaning that can be modified by \code{value.codes}. The
  value "I" indicates interval censoring and requires
  values for \code{value2}.}

  \item{value.codes}{special value qualifier codes. See
  Lorenz and others, 2014 for a description of these codes
  and the special \code{remark.codes}.}

  \item{reporting.level}{the reporting level associated
  with each value.}

  \item{reporting.method}{the method used to compute the
  reporting level. These can be arbitrary codes, known only
  to the user.}

  \item{reporting.units}{the reporting units. A few defined
  units such as "mg/l" are recognized and used by the
  software.}

  \item{analyte.method}{the method used to compute the
  concentration. These can be arbitrary codes, known only
  to the user.}

  \item{analyte.name}{the name of the analyte. A few names
  are recongnized such as "chloride" and used by the
  software.}

  \item{unique.code}{an arbitrary code identifying unique
  instances of data. For data retreived from the US
  Geological Survey, this should be the 5-digit parameter
  code.}

  \item{value2}{for interval censored data, the upper
  value.}
}
\value{
  An object of class "qw" that contains the numeric data in
  a two-column matrix and the meta data as additional slot
  information.
}
\description{
  Convert the data and meta data to a water-quality data
  (\code{mcens}) object.
}
\note{
  The class "qw" is intended to be a flexible and
  user-friendly way to store and manage water-quality data.
  The meta data that are stored with the numeric and remark
  data are essential for unbiased statistical analysis and
  for reducing other errors in interpretation.
}
\references{
  Lorenz, D.L., in preparation
}
\seealso{
  \code{\link{importNWISqw}}
}
\keyword{manip}

