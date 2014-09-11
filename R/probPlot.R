#'Probability Plot
#'
#'Produce a probability plot of censored data.
#'
#'The control parameters for left-censored data, \code{Censored}, are the same
#'as those for uncensored data, \code{Plot}. See \code{\link{setPlot}} for
#'details.
#'
#' @aliases probPlot.lcens probPlot.qw
#' @param x the data to plot. Missing values are removed before plotting.
#' @param truncate not used for left-censored data.
#' @param FLIP if TRUE, the plot the cumumlative distribution. Otherwise, plot
#'as flipped data (largest values on left).  This is not related to computing
#'statistics by the flipped Kaplan-Meier method.
#' @param distribution the name of the desired function converting from
#'probabilities to coordinates.
#' @param alpha the alpha value of the function for computing plotting
#'positions.
#' @param Plot control parameters of the uncensored data plot.
#' @param Censored control parameters of the left-censored data plot.
#' @param yaxis.log log-transform the y axis?
#' @param yrange set the range of the y axis.
#' @param ylabels set the y-axis labels. See \code{\link{linearPretty}} for
#'details.
#' @param xlabels set the x-axis labels. See \code{\link{probPretty}} for
#'details.
#' @param CDF if TRUE, then label with increasing probabilities. Otherwise label
#'with decreasing probabilities.
#' @param xtitle the x-axis title.
#' @param RI not used for left-censored data.
#' @param RItitle not used for left-censored data.
#' @param ytitle the y-axis title.
#' @param caption the figure caption.
#' @param margin the parameters of the margin of the plot area.
#' @param \dots parameters for the distribution function or other method
#'functions.
#' @return Information about the graph.
#' @seealso \code{\link{qqPlot.lcens}}
#' @keywords hplot
#' @examples
#'\dontrun{
#'set.seed(1932)
#'Xu <- rlnorm(32)
#'setPage("sq") # required page set up
#'probPlot(as.lcens(Xu, 1.0))
#'}
#'
#' @import USGSwsGraphs 
#' @rdname probPlot.lcens
#' @export
#' @method probPlot lcens
probPlot.lcens <- function(x, truncate, 
                           FLIP=FALSE, distribution="normal",
                           alpha=0.4, # data specification
                           Plot=list(name="Uncensored", what='points', type='solid',
                             width='standard', symbol='circle', filled=TRUE,
                             size=0.09, color='black'),
                           yaxis.log=TRUE, yrange=c(NA, NA), # y-axis controls
                           ylabels=11,  xlabels=11, CDF=TRUE, # labels
                           xtitle='Cumulative Probability', RI, RItitle,
                           ytitle=deparse(substitute(x)), # axis titles
                           caption="", # caption
                           margin=c(NA, NA, NA, NA), # margin control
                           Censored=list(name="Left censored", what='points', symbol='circle',
                             filled=FALSE, size=0.09, color='black'), # plot controls
                           ...) { # distribution parameters and method args
  ## Coding History:
  ##    2012Mar15 DLLorenz Original coding from default method
  ##    2013Jan13 DLLorenz Roxygenized
  ##    2013Jan13          This version.
  ##
  ytitle <- ytitle # needed to 'set' names
  qnpp <- censpp(x, alpha)
  x <- qnpp$x
  pp <- qnpp$pp
  xcen <- qnpp$xcen
  ppcen <- qnpp$ppcen
  ppall <- c(ppcen, pp)
  ## set up the axes and transform data
  if(dev.cur() == 1)
    setGD("ProbabilityPlot")
  yax <- setAxis(x, yrange, yaxis.log, FALSE, ylabels)
  x <- yax$data
  if(yaxis.log)
    xcen <- log10(xcen)
  yax <- yax$dax
  pax <- probPretty(ppall, labels=xlabels, exceedence = !CDF, distribution=distribution, ...)
  ## set up the plot
  if(FLIP) {
    pp <- 1 - pp
    ppcen <- 1 - ppcen
  }
  ## get the dist function
  qdist <- getDist.fcn(distribution, 'q')
  xcoord <- qdist(pp, ...)
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  
  par(mar=margin)
  plot(xcoord, x, type='n', xlim=pax$range, xaxs='i', axes=FALSE,
       ylim=yax$range, yaxs='i', ylab="", xlab="")
  Plot <- setPlot(Plot, name="Uncensored", what='points', type='solid',
                  width='standard', symbol='circle', filled=TRUE,
                  size=0.09, color='black') # force defaults if not set
  explan <- setExplan(Plot) # add info to set up explanation
  plotPars <- explan$current
  points(xcoord, x, type=plotPars$type, lwd=plotPars$lwd, lty=plotPars$lty,
        pch=plotPars$pch, cex=plotPars$cex, col=plotPars$col, bg=plotPars$col)
  box(lwd=frameWt())
  ## trick current
  retval <- (list(x=pp, y=x, xcen=ppcen, ycen=xcen, yaxis.log=FALSE, yaxis.rev=FALSE,
                  xaxis.log=FALSE, explanation=explan, margin=margin))
  ## Add censored values
  Censored <- setDefaults(Censored, name="Left censored", what='points', symbol='circle',
                          filled=FALSE, size=0.09, color='black')
  xcoord <- qdist(ppcen, ...)
  retval <- addXY(xcoord, xcen, Plot=Censored, current=retval)
  ## Label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(pax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  ## Fit line, if possible
  if(length(list(...)) > 0) { # if parameters specified try to plot line
    Plot$what <- "lines"
    if(yaxis.log)
      coefs <- c(0, .4342945)
    else
      coefs <- c(0, 1)
    retval <- refLine(coefficients=coefs, Plot=Plot, current=retval)
  }
  retval$yaxis.log=yaxis.log
  invisible(c(retval, xtrans=qdist, xtargs=list(...), yax=yax, xax=pax))
}

#' @rdname probPlot.lcens
#' @export
#' @method probPlot qw
probPlot.qw <- function(x, truncate, 
												FLIP=FALSE, distribution="normal",
												alpha=0.4, # data specification
												Plot=list(name="Uncensored", what='points', type='solid',
																	width='standard', symbol='circle', filled=TRUE,
																	size=0.09, color='black'),
												yaxis.log=TRUE, yrange=c(NA, NA), # y-axis controls
												ylabels=11,  xlabels=11, CDF=TRUE, # labels
												xtitle='Cumulative Probability', RI, RItitle,
												ytitle=deparse(substitute(x)), # axis titles
												caption="", # caption
												margin=c(NA, NA, NA, NA), # margin control
												Censored=list(name="Left censored", what='points', symbol='circle',
																			filled=FALSE, size=0.09, color='black'), # plot controls
												...) { # distribution parameters and method args
	## Coding History:
	##    2014May22 DLLorenz Original coding
	##
	ytitle <- ytitle # needed to 'set' names
	retval <- probPlot.lcens(as.lcens(x),
													 truncate=truncate, 
													 FLIP=FLIP, distribution=distribution,
													 alpha=alpha,
													 Plot=Plot,
													 yaxis.log=yaxis.log, yrange=yrange,
													 ylabels=ylabels,  xlabels=xlabels, CDF=CDF,
													 xtitle=xtitle, RI, RItitle,
													 ytitle=ytitle,
													 caption=caption,
													 margin=margin,
													 Censored=Censored, ...)
	invisible(retval)
}
	