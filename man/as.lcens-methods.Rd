\docType{methods}
\name{as.lcens-methods}
\alias{as.lcens-methods}
\alias{as.lcens,lcens,missing,missing-method}
\alias{as.lcens,numeric,missing,character-method}
\alias{as.lcens,numeric,missing,logical-method}
\alias{as.lcens,numeric,missing,missing-method}
\alias{as.lcens,numeric,numeric,character-method}
\alias{as.lcens,numeric,numeric,logical-method}
\alias{as.lcens,numeric,numeric,missing-method}
\alias{as.lcens,qw,missing,missing-method}
\alias{as.lcens,qw,numeric,missing-method}
\title{Methods for Function \code{as.lcens}}
\description{
  Valid conversion methods for function \code{as.lcens}.
}
\section{Methods}{
  \describe{

  \item{list("signature(values = \"lcens\", detlim =
  \"missing\", censor.codes = \"missing\")")}{ No change to
  the "lcens" object. }

  \item{list("signature(values = \"numeric\", detlim =
  \"missing\", censor.codes = \"character\")")}{ Valid
  \code{censor.codes} are "<" for left-censored values, and
  " " for uncensored values. All others are ignored. The
  detection limits are infered from the values for censored
  values. }

  \item{list("signature(values = \"numeric\", detlim =
  \"missing\", censor.codes = \"logical\")")}{ A
  \code{censor.codes} of \code{TRUE} indicates a
  left-censored value, \code{FALSE} indicates uncensored.
  The detection limits are infered from the values for
  censored values. }

  \item{list("signature(values = \"numeric\", detlim =
  \"missing\", censor.codes = \"missing\")")}{ No censoring
  is possible. }

  \item{list("signature(values = \"numeric\", detlim =
  \"numeric\", censor.codes = \"character\")")}{ Any
  \code{values} less than \code{detlim} is left-censored. }

  \item{list("signature(values = \"numeric\", detlim =
  \"numeric\", censor.codes = \"logical\")")}{ Any
  \code{values} less than \code{detlim} is left-censored,
  ties resolved by \code{censor.codes}. }

  \item{list("signature(values = \"numeric\", detlim =
  \"numeric\", censor.codes = \"missing\")")}{ Any
  \code{values} less than \code{detlim} is left-censored,
  ties resolved by \code{censor.codes}. }

  \item{list("signature(values = \"qw\", detlim =
  \"missing\", censor.codes = \"missing\")")}{ The object
  of class "qw" is convereted if it contains only
  uncensored or left-censored data. }

  \item{list("signature(values = \"qw\", detlim =
  \"numeric\", censor.codes = \"missing\")")}{ The object
  of class "qw" is convereted if it contains only
  uncensored or left-censored data and force a minimum
  detection limit. } }
}
\examples{
## The first value is censored at 2, the second is not
as.lcens(c(1,3), 2)
}
\keyword{manip}
\keyword{methods}

