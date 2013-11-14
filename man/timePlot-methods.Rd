\docType{methods}
\name{timePlot-methods}
\alias{timePlot}
\alias{timePlot-methods}
\alias{timePlot,Date,lcens-method}
\alias{timePlot,Date,qw-method}
\alias{timePlot.lcens}
\alias{timePlot.qw}
\title{Censored Data Methods for Function \code{timePlot}}
\description{
  Produce a plot of censored time-series data.
}
\section{Methods}{
  \describe{

  \item{list('signature(x = "Date", y = "lcens")')}{ This
  is the method for left-censored data, \code{x} must be
  class "Date." See \code{\link{timePlot}} for details
  about the arguments. There is an optional argument
  \code{Censored} that controls how the censored data are
  plotted. It functions like the \code{Plot} argument, but
  has an additional argument \code{bar}, which draws a drop
  line if set to \code{TRUE}. }

  \item{list('signature(x = "Date", y = "qw")')}{ This is
  the method for water-quality data, \code{x} must be class
  "Date." It converts \code{y} to class "lcens" and
  executes that method. If \code{y} cannot be converted,
  then the clall fails. } }
}
\keyword{hplot}
\keyword{methods}

