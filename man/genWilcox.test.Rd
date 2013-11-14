\name{genWilcox.test}
\alias{genWilcox.test}
\title{Test for difference in left-censored samples}
\usage{
  genWilcox.test(x, y, alternative = "two.sided",
    data.names)
}
\arguments{
  \item{x}{the samples from each group. Forced to class
  "lcens." Missing values are removed before the analysis.}

  \item{y}{either another set of samples or a group
  identifier with exactly two groups. Missing values are
  removed before the analysis. See \bold{Details}.}

  \item{alternative}{character string describing the
  alternative hypothesis. Must be one of "two.sided,
  "greater," or "less."}

  \item{data.names}{character string to be used to explain
  the data. Default names are derived from the data
  arguments.}
}
\value{
  An object of class "htest."
}
\description{
  This function tests for differences in left-censored
  samples from 2 groups.
}
\details{
  If \code{y} is either type character or factor, then it
  is assumed to be a group identifier. Anything else is
  treated as another set of sample and forced to class
  "lcens."
}
\note{
  The \code{genWilcox.test} function uses the
  \code{survfit} function. Helsel (2012) describes flipping
  the left-censored data so that small values become large
  and left-censored values become right-censored values and
  adapt nonparametric techniques from survival analysis.\cr
}
\section{Null Hypothesis}{
  The null hypothesis is that the distributions are not
  different from one another.
}
\examples{
# Compare uncensored results
# First for grouped data
set.seed(69)
Xu <- rlnorm(22, 0, 1)
Yu <- rlnorm(22, .6, 1)
genWilcox.test(Xu, Yu)
wilcox.test(Xu, Yu)
# Compare effect of censoring
genWilcox.test(as.lcens(Xu, 1), Yu)
genWilcox.test(as.lcens(Xu, 1), as.lcens(Yu, 1))
}
\references{
  Gehan, E.A., 1965, A generalized Wilcoxon test for
  comparing arbitraritly singly censored samples:
  Biometrika, v. 52, p. 203-223.\cr

  Harrington, D.P., and Fleming, T.R., 1982, A class of
  rank test procedures for censored survival data:
  Biometrika, v. 69, p. 553-566.\cr

  Helsel, D.R. 2012, Statistics for Censored Environmental
  Data Using Minitab and R: New York, Wiley, 324 p.\cr

  Peto, R., and Peto, J., 1972, Asymptotically efficient
  rank invariant test procedures (with discussion): Journal
  of the Royal Statistical Society, Series A v. 135, p.
  185-206.\cr

  Prentice, R.L. 1978, Linear rank tests with
  right-censored data: Biometika, v 65, p 167-179.\cr

  Prentice, R.L., and Marke, P., 1979, A qualitative
  discrepancy between censored data rank tests: Biometrika,
  v. 35, p. 861-867.\cr
}
\seealso{
  \code{\link{survdiff}}, \code{\link{survfit}},
  \code{\link{lcens-class}}
}
\keyword{censored}
\keyword{htest}

