\docType{package}
\name{USGSwsQW-package}
\alias{USGSwsQW-package}
\title{Tools for censored water-quality data analysis}
\description{
  A collection of tools for the analysis of left- and
  multiply-censored data that focuses on the analysis of
  water-quality data.
}
\details{
  \tabular{ll}{ Package: \tab USGSwsQW\cr Type: \tab
  Package\cr Version: \tab 0.5\cr Date: \tab 2013-08-01\cr
  License: \tab file LICENSE\cr Depends: \tab USGSwsBase
  USGSwsGraphs USGSwsStats methods survival XML mvtnorm\cr
  } Modern water-quality information requires much more
  that a value and remark code to characterize the data.
  The tools in this library focus on the "qw" class, which
  store the value and the remark code, but also information
  about the reporting level, the units, the methods of
  analysis analysis, and the analyte itself.\cr Some
  specialized analytical tools for left-only censored data,
  requiring objects of class "lcens" have been developed
  and are included in this library. Other analytical tools
  for multiply- or any censored data are alos included.
  The tools include automatic conversion of "qw" data into
  the 'best' (left-only or multiply censored) format.\cr
  Need a general description of the tools by group--a brief
  synopsis of the OFR.
}
\author{
  Dave Lorenz <lorenz@usgs.gov>

  Maintainer: Dave Lorenz <lorenz@usgs.gov>
}
\references{
  Lorenz, D.L., in preparation.
}
\keyword{package}

