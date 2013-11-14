\docType{methods}
\name{dotPlot-methods}
\alias{dotPlot}
\alias{dotPlot-methods}
\alias{dotPlot,mcens-method}
\alias{dotPlot,qw-method}
\alias{dotPlot.mcens}
\alias{dotPlot.qw}
\title{Censored Data Methods for Function \code{dotPlot}}
\description{
  Produce a dor plot of censored data.
}
\section{Methods}{
  \describe{

  \item{list('signature(x = "mcens", y = "ANY")')}{ This is
  the method for multiply-censored data, \code{x} must be
  class "mcens." See \code{\link{dotPlot}} for details
  about the arguments. There is an optional argument
  \code{Censored} that controls how the censored data are
  plotted. It functions like the \code{Plot} argument, but
  has an additional argument \code{bar}, which draws a
  connecting line if set to \code{TRUE}. }

  \item{list('signature(x = "qw", y = "ANY")')}{ This is
  the method for water-quality data, \code{x} must be class
  "qw." It simply converts \code{x} to class "mcens" and
  executes that method. } }
}
\keyword{hplot}
\keyword{methods}

