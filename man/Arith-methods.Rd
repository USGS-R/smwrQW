\docType{methods}
\name{Arith-methods}
\alias{Arith-methods}
\alias{Arith,lcens,lcens-method}
\alias{Arith,lcens,numeric-method}
\alias{Arith,mcens,mcens-method}
\alias{Arith,mcens,numeric-method}
\alias{Arith,numeric,lcens-method}
\alias{Arith,numeric,mcens-method}
\title{Arithmetic Methods for \code{lcens}, \code{mcens}, and \code{qw} objects}
\description{
  Some limited arithemtic methods are possible and
  well-defined for lcensored and water-quality data.
}
\section{Methods}{
  \describe{

  \item{list(signature(e1 = "lcens", e2 = "numeric"))}{
  Multiplication and division (positive values only) and
  addition and subtraction are defined. }

  \item{list(signature(e1 = "numeric", e2 = "lcens"))}{
  Multiplication (positive values only) and addition are
  defined for this combination. }

  \item{list(signature(e1 = "lcens", e2 = "lcens"))}{
  Addition only is defined for this combination. }

  \item{list(signature(e1 = "mcens", e2 = "numeric"))}{
  Multiplication, division, addition, and subtraction are
  defined. }

  \item{list(signature(e1 = "numeric", e2 = "mcens"))}{
  Multiplication, division, addition, and subtraction are
  defined. }

  \item{list(signature(e1 = "mcens", e2 = "mcens"))}{
  Multiplication, division, addition, and subtraction are
  defined. }

  \item{list(signature(e1 = "qw", e2 = "numeric"))}{ No
  mathematical operation defined. See \code{add}.}

  \item{list(signature(e1 = "numeric", e2 = "qw"))}{ No
  mathematical operation defined. }

  \item{list(signature(e1 = "qw", e2 = "qw"))}{ No
  mathematical operation defined. See \code{add}. } }
}
\seealso{
  \code{\link{add}}
}
\keyword{manip}
\keyword{methods}

