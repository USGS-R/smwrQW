#'Plot 
#'
#'Create diagnostic plots for selected hypothesis tests.
#'
#'The \code{ppw} method for \code{plot} creates 2 graphs on a single page.
#'The first graph is the scaled differences from the PPW test with the mean
#'difference shown by a blue line. The second
#'graph is the actual differences in the paired values, assuming that the 
#'true minimum for left-censored data is 0. The median difference is shown
#'by a blue line.
#'
#'The \code{genWilcox} method creates a single graph that shows the emprical
#'cumulative distributions for each group.
#'
#' @aliases plot.ppw plot.genWilcox
#' @param x an object having classes "htest" and some other class for which a
#'\code{plot} method exists.
#' @param which either "All" or an number indicating which plot,
#'see \bold{Details}.
#' @param set.up set up the graphics page?
#' @param labels use supplied values for y-axis labels instead of sequence
#'number. 
#' @param xaxis.log draw the x-axis on a logarithimic scale?
#' @param \dots not used, arguments passed to or from other methods.
#' @return The object \code{x} is returned invisibly.
#' @keywords hplot
#' @rdname plot.htest
#' @S3method plot ppw
#' @method plot ppw
plot.ppw <- function(x, which="All", set.up = TRUE, labels, ...) {
  ## Identify which plots to do:
  ## 1: 2 graphs actual diffs and score diffs
  ## 
  ## Set up graphics page
  if(set.up) 
    setGD("PPW")
  ## Set up to do the plots, ignore which
  if(missing(labels))
    labels <- as.character(seq(nrow(x$PPWmat)))
  ## Guess at y-axis margin needed
  yleft <- (max(nchar(labels)) + 1) * .5 + 1.5
  AA.lo <- setLayout(num.cols=2, shared.y=1, yleft=yleft)
  Ord <- order(x$PPWmat[, 3L])
  AA.gr <- setGraph(1, AA.lo)
  dotPlot(x$PPWmat[Ord, 3L], labels[Ord], xtitle="PPW Scaled Difference",
          xlabels=5, margin=AA.gr)
  refLine(vertical=0)
  refLine(vertical=mean(x$PPWmat[, 3L]), Plot=list(color="blue"))
  AA.gr <- setGraph(2, AA.lo)
  mind <- x$PPWmat[Ord, 4L]
  maxd <- x$PPWmat[Ord, 5L]
  dotPlot(as.mcens(mind, maxd), labels[Ord], xtitle="Computed Difference",
          xlabels=5, margin=AA.gr)
  refLine(vertical=0)
  refLine(vertical=median(as.mcens(mind, maxd)), Plot=list(color="blue"))
  invisible(x)
}

#' @rdname plot.htest
#' @S3method plot genWilcox
#' @method plot genWilcox
plot.genWilcox <- function(x, which="All", set.up = TRUE, xaxis.log=TRUE, ...) {
	## Identify which plots to do:
	## 1: The ecdf graph of the data
	## 
	## Set up graphics page
	if(set.up) 
		setGD("genWilcox")
	## Set up to do the plots, ignore which
	est <- x$estimate
	AA.pl <- xyPlot(est$time, est$surv, Plot=list(what="none"), yaxis.range=c(0,1),
									xaxis.log=xaxis.log, ytitle="Cumulative Probability",
									xtitle='Value')
	Grps <- cumsum(est$strata)
	Cols <- c("red", "green3")
	ndx <- 1L
	for(i in seq(2L)) { # Must be exactly 2
		xtopl <- est$time[c(Grps[i], seq(Grps[i], ndx))]
		ytopl <- c(est$surv[seq(Grps[i], ndx)], 1)
		ndx <- Grps[i] + 1L
		AA.pl <- addXY(xtopl, ytopl, Plot=list(name=names(Grps[i]),
																					 what="stairstep",
																					 color=Cols[i], 
																					 width="color"),
									 current=AA.pl)
	}
	addExplanation(AA.pl, where="lr", title="")
	invisible(x)
}
