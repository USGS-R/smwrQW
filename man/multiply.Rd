% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multiply.R
\name{multiply}
\alias{multiply}
\title{Multiply Water-Quality Data by a Number}
\usage{
multiply(x, factor, units, analyte, analyte.method = "ALGOR", pcode = "")
}
\arguments{
\item{x}{an object of water-quality (class "qw"). Missing value are permitted and result
in corresponding missing values in the output.}

\item{factor}{a single numeric value}

\item{units}{character reporting units of the resulting operation, if not supplied, 
retained from \code{x}.}

\item{analyte}{character the name of the resulting analyte.}

\item{analyte.method}{character the name of the analytical method for the result,
by default "ALGOR."}

\item{pcode}{the parameter code of the result, if not supplied, retained from \code{x}.}
}
\value{
An object of class "qw" that is the result of the requested operation.
}
\description{
Multiplies water-quality data by a number while maintaining the 
integrity of the data.
}
\details{
Multiplication is not defined for objects of class "qw" because it
can change some of the metadata, in particular the units, the analytical method,
or the unique identifier (pcode).
}
