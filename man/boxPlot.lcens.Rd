\name{boxPlot.lcens}
\alias{boxPlot.lcens}
\alias{boxPlot.qw}
\title{Box Plot}
\usage{
  \method{boxPlot}{lcens} (..., group = NULL,
    Box = list(type = "truncated", show.counts = TRUE, censorbox = NA, censorstyle = "censored", nobox = 5, width = "Auto", fill = "none", truncated = c(10, 90)),
    yaxis.log = TRUE, yaxis.range = c(NA, NA),
    ylabels = "Auto", xlabels = "Auto",
    xlabels.rotate = FALSE, xtitle = "", ytitle = "",
    caption = "", margin = c(NA, NA, NA, NA))

  \method{boxPlot}{qw} (..., group = NULL,
    Box = list(type = "truncated", show.counts = TRUE, censorbox = NA, censorstyle = "censored", nobox = 5, width = "Auto", fill = "none", truncated = c(10, 90)),
    yaxis.log = TRUE, yaxis.range = c(NA, NA),
    ylabels = "Auto", xlabels = "Auto",
    xlabels.rotate = FALSE, xtitle = "", ytitle = "",
    caption = "", margin = c(NA, NA, NA, NA))
}
\arguments{
  \item{\dots}{the data to plot.}

  \item{group}{create groups of a single left-censored data
  vector. Invalid for anything else}

  \item{Box}{control parameters for the box. See
  \code{\link{boxPlot}}}

  \item{yaxis.log}{logical: log transform y axis?}

  \item{yaxis.range}{set y-axis range.}

  \item{ylabels}{set up y-axis labels. See
  \code{\link{linearPretty}} for details.}

  \item{xlabels}{set up x-axis labels.}

  \item{xlabels.rotate}{rotate x-axis labels 90 degrees?}

  \item{xtitle}{the x-axis title.}

  \item{ytitle}{the y-axis title.}

  \item{caption}{the figure caption.}

  \item{margin}{set up the plot area margins.}
}
\value{
  Information about the graph.
}
\description{
  Produces an estimated or censored style box plot for
  truncated, simple, or extended type box plot. The Tukey
  type box plot requires an estimated style box plot.
}
\details{
  If group is numeric, then the boxes will be plotted along
  a continuous numeric axis. Otherwise the x-axis will be
  discrete groups.
}
\note{
  The censored style box plot truncates the boxplot at the
  largest value of the detection limit. The estimated style
  box plot estimates values for left-censored data and uses
  those estimates to construct the full boxplot; the range
  of estimated values is shown in gray.

  The \code{fillIn} function is used to estimate values
  that are censored. If \code{yaxis.log} is \code{TRUE},
  then the "log ROS" method is used to estimate those
  values, otherwise, the "ROS" method is used.
  Occasionally, estimated values by \code{fillIn} will
  estimate values for censored data that are greater than
  the censoring level. In those cases, a red bar is drawn
  on the box plot at the largest estimated value.
}
\examples{
\dontrun{
set.seed(1932)
Xu <- rlnorm(32)
setPage("sq") # required page set up
boxPlot(as.lcens(Xu, 1.0))
}
}
\seealso{
  \code{\link{fillIn}}
}
\keyword{hplot}

