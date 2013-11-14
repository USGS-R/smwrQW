\name{censReg}
\alias{censReg}
\title{Censored Data Regression}
\usage{
  censReg(formula, data, subset, weights, na.action,
    dist = "normal")
}
\arguments{
  \item{formula}{a formula describing the regression model.
  See \bold{Details}.}

  \item{data}{the data to search for the variables in
  \code{formula}.}

  \item{subset}{an expression to select a subset of the
  data.}

  \item{weights}{a variable to use for weights.}

  \item{na.action}{what to do with missing values.}

  \item{dist}{the distribution of the data, either
  "normal," "lognormal," or "commonlog." See
  \bold{Details}.}
}
\value{
  An object of class "censReg."
}
\description{
  Build a regression model for censored response data.
}
\details{
  The left-hand side of the formula may be any numeric
  variable, just as with \code{lm} or a variable of class
  "lcens," "mcens," or "qw."\cr For un- or left-censored
  data, AMLE is used unless weights are specified in the
  model, then MLE is used, through a call to survreg. For
  any other censored data, MLE is used.\cr If \code{dist}
  is "normal," then the regression analysis assumes that
  the residuals are normally distributed. If \code{dist} is
  "lognormal," then the regression analysis assumes that
  the residuals are lognormally distributed. In this case,
  predicted values are back transformed and optionally bias
  corrected to represent the expected mean. If \code{dist}
  is "commonlog," then the response data are transformed
  using \code{log10} and those residuals are assumed to be
  normally distributed. No back transformation is made for
  predicted values.
}
\examples{
set.seed(345)
X <- runif(24, 1, 5)
Y <- X/2 + rnorm(24)
lm(Y ~ X) # the uncensored regression
censReg(as.lcens(Y, 1) ~ X) # censored at 1
}
\references{
  will need some.
}
\seealso{
  \code{\link{lm}}
}
\keyword{censored}
\keyword{regression}

