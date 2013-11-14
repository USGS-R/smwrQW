\name{pcodeNWISqw}
\alias{pcodeNWISqw}
\title{Parameter Code Information}
\usage{
  pcodeNWISqw(params = "All", group = TRUE, name = TRUE,
    CASRN = FALSE, short = TRUE, units = TRUE,
    col.name = FALSE)
}
\arguments{
  \item{params}{A character string contains the name of a
  group of parameter codes, or a vector of pamater codes.
  See \bold{Details}.}

  \item{group}{include the parameter group in the output?}

  \item{name}{include the parameter name in the output?}

  \item{CASRN}{include the parameter Chemical Abstracts
  Service Registry Number (CASRN) in the output?}

  \item{short}{include the parameter short name in the
  output?}

  \item{units}{include the parameter units in the output?}

  \item{col.name}{include a column name for the parameter
  in the output?}
}
\value{
  A data frame with the parameter codes and the selected
  columns.
}
\description{
  Get information about USGS parameter codes.
}
\details{
  Valid groups for \code{param} are "All," "information,"
  "physical," "cations," "anions," "nutrients,"
  "microbiological," "biological," "metals," "nonmetals,"
  "toxicity," "pesticides," "pcbs," "other organics,"
  "radio chemicals,", "stable isotopes," "sediment," and
  "population/community."
}
\examples{
\dontrun{
pcodeNWISqw("00925")
}
}
\references{
  Lorenz, D.L., 2014, USGSqw OFR.\cr See information about
  discrete samples at
  \url{http://nwis.waterdata.usgs.gov/usa/nwis/qw}.
}
\keyword{datasets}
\keyword{IO}

