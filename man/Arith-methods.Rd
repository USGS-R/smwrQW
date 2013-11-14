\docType{methods}
\name{Arith-methods}
\alias{Arith-methods}
\alias{Arith,lcens,lcens-method}
\alias{Arith,lcens,numeric-method}
\alias{Arith,numeric,lcens-method}
\title{Arithmetic Methods for \code{lcens} objects}
\description{
  Some limited arithemtic methods are possible and
  well-defined for left-censored data.
}
\section{Methods}{
  \describe{

  \item{list("signature(e1 = \"lcens\", e2 =
  \"numeric\")")}{ Multiplication and division (positive
  values only) and addition and subtraction are defined. }

  \item{list("signature(e1 = \"numeric\", e2 =
  \"lcens\")")}{ Multiplication (positive values only) and
  addition are defined for this combination. }

  \item{list("signature(e1 = \"lcens\", e2 = \"lcens\")")}{
  Addition only is defined for this combination. } }
}
\keyword{manip}
\keyword{methods}

