\name{sdlFill}
\alias{sdlFill}
\title{Estimate Statistics}
\usage{
  sdlFill(x, method = "fill", alpha = 0.44)
}
\arguments{
  \item{x}{the data to estimate, Missing values permitted
  and ignored. Must be an object of class "lcens," a
  numeric vector, or the output from censpp.}

  \item{method}{the method to use, either "MLE" or "log
  MLE."}

  \item{alpha}{the offset for plotting postion (not used).}
}
\value{
  A list containing the mean and standard deviation, filled
  in values for the censored values, and the maximum censor
  level.
}
\description{
  Support function for computing statistics for
  left-censored data.
}
\references{
  Gleit, A., 1985, Estiamtion for smll normal data sets
  with detection limits: Environmental Science and
  Technology, v. 19, no. 12, p. 1201--1206.
}
\keyword{misc}

