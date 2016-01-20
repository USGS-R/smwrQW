#' Box Plot
#'
#' Produces an estimated or censored style box plot for truncated, Tukey, simple, or 
#'extended type box plot: methods for "lcens" and "qw" data. The Tukey type box 
#'plot requires an estimated style box plot.
#'
#' If group is numeric, then the boxes will be plotted along a continuous
#'numeric axis. Otherwise the x-axis will be discrete groups.
#'
#' @aliases boxPlot.lcens boxPlot.qw boxPlot.data.frame
#' @param \dots the data to plot.
#' @param group create groups of a single left-censored data vector. Invalid for
#'anything else
#' @param Box control parameters for the box. See \code{\link{boxPlot}}
#' @param yaxis.log logical: log transform y axis?
#' @param yaxis.range set y-axis range.
#' @param ylabels set up y-axis labels. See \code{\link{linearPretty}} for
#'details.
#' @param xlabels set up x-axis labels.
#' @param xlabels.rotate rotate x-axis labels 90 degrees?
#' @param xtitle the x-axis title.
#' @param ytitle the y-axis title.
#' @param caption the figure caption.
#' @param margin set up the plot area margins.
#' @return Information about the graph.
#' @note The censored style box plot truncates the boxplot at the largest value
#'of the reporting level. The estimated style box plot estimates values for
#'left-censored data and uses those estimates to construct the full boxplot;
#'the range of estimated values is shown in gray. The maximum reporting level, 
#'except for some simple heuristics for identifying elevated reporting levels, 
#'is used for the censoring level when there are multiple reporting levels.
#'
#'The \code{fillIn} function is used to estimate values that are censored.
#'If \code{yaxis.log} is \code{TRUE}, then the "log ROS" method is used to
#'estimate those values, otherwise, the "ROS" method is used. Occasionally,
#'estimated values by \code{fillIn} will estimate values for censored data
#'that are greater than the censoring level. In those cases, a red bar is
#'drawn on the box plot at the largest estimated value.
#'
#'The default method for data frames in smwrGraphs is replaced with a method 
#'that supports data frames that contain columns of class "qw" or "lcens." 
#' @keywords hplot
#' @seealso \code{\link{fillIn}}
#' @examples
#'\dontrun{
#'set.seed(1932)
#'Xu <- rlnorm(32)
#'setPage("sq") # required page set up
#'boxPlot(as.lcens(Xu, 1.0))
#'}
#'
#' @import smwrGraphs
#' @rdname boxPlot.lcens
#' @export
#' @method boxPlot lcens
boxPlot.lcens <- function(..., group=NULL, # data
                            Box=list(type="truncated", show.counts=TRUE,
                              censorbox=NA, censorstyle="censored", nobox=5,
                              width="Auto", fill="none", truncated=c(10,90)), # b&w controls
                            yaxis.log=TRUE, yaxis.range=c(NA,NA), # y-axis controls
                            ylabels="Auto", xlabels="Auto",
                            xlabels.rotate=FALSE, # labels
                            xtitle="", ytitle="",  caption="", # axis titles and caption
                            margin=c(NA,NA,NA,NA)) { # margin control
  ## Coding history:
  ##    2012Mar07 DLLorenz Original Code for censored box plots
  ##    2012Apr20 DLLorenz Created function to compute quartiles, not hinges
  ##                        (copied from uncensored version)
  ##    2012Sep18 DLLorenz Added fill option
  ##    2013Jan11 DLLorenz Roxygenized
  ##    2013May17 DLLorenz Bug fix on Box defaults
  ##
  ## Process data to plot, all should be lcens. If only 1, then can be grouped
  dots <- list(...)
  if(!is.null(group)) {
    if(length(dots) == 1) { # split the data, names derived from group
      if(!is.numeric(group))
        group <- recode(group, "", " ")
      dots <- split(dots[[1]], group)
      if(is.numeric(group)) { # set xrange by data
        xrange <- range(group)
        meanspacing <- diff(xrange)/(length(dots)-1)
        if(!is.numeric(xlabels))
          xlabels <- 7
        xrange <- xrange + c(-meanspacing, meanspacing)/2
        xtoplot <- as.double(names(dots))
      }
      else {
        xrange <- c(0, length(dots) + 1)
        xtoplot <- seq(length(dots))
      } # end of xrange logic
    }
    else
      stop("Multiple lcens objects cannot be grouped by boxPlot")
  }
  else {
    xrange <- c(0, length(dots) + 1)
    xtoplot <- seq(length(dots))
  }
  # Open device if necessary
  if(dev.cur() == 1L)
  	setGD("BoxPlot")
  if(is.null(names(dots))) { # try to get names
    call <- as.list(match.call())[-1] # drop boxPlot
    call <- as.character(call)
    names(dots) <- call[seq(length(dots))]
  }
  ## Fix defaults for Box
  Box <- setDefaults(Box, type="truncated", show.counts=TRUE, censorbox=NA,
                     censorstyle="censored", nobox=5, width="Auto", 
                     fill="none", truncated=c(10,90))
  Box$type <- match.arg(Box$type, c("truncated", "simple",
  																	"tukey", "extended"))
  Box$censorstyle <- match.arg(Box$censorstyle,
  														 c("censored", "estimated"))
  if(Box$type == "tukey" && Box$censorstyle == "censored") {
    warning("censored tukey boxplot not possible; creating estimated tukey boxplot")
    Box$censorstyle <- "estimated"
  }
  ## Compute the stats and produce the boxplot
  statsret <- boxPlotCensStats(dots, Box, yaxis.log)
  ##
  retval <- renderBoxPlot(xtoplot, statsret$boxes, Box, statsret$explan, statsret$z,
                          yaxis.log, yaxis.range, xrange, ylabels, xlabels, xlabels.rotate,
                          xtitle, ytitle, caption, margin)
  invisible(retval)
}

#' @rdname boxPlot.lcens
#' @export
#' @method boxPlot qw
boxPlot.qw <- function(..., group=NULL, # data
                            Box=list(type="truncated", show.counts=TRUE,
                              censorbox=NA, censorstyle="censored", nobox=5,
                              width="Auto", fill="none", truncated=c(10,90)), # b&w controls
                            yaxis.log=TRUE, yaxis.range=c(NA,NA), # y-axis controls
                            ylabels="Auto", xlabels="Auto",
                            xlabels.rotate=FALSE, # labels
                            xtitle="", ytitle="",  caption="", # axis titles and caption
                            margin=c(NA,NA,NA,NA)) { # margin control
  ## Coding history:
  ##    2012Mar07 DLLorenz Original Code for censored box plots
  ##    2012Apr20 DLLorenz Created function to compute quartiles, not hinges
  ##                        (copied from uncensored version)
  ##    2012Sep18 DLLorenz Added fill option
  ##    2013Jan11 DLLorenz Roxygenized
  ##    2015Jan16 DLLorenz Accept type == "tukey" if all censoring is none
  ##
  ## Process data to plot, all should be lcens. If only 1, then can be grouped
  dots <- list(...)
  if(!is.null(group)) {
    if(length(dots) == 1) { # split the data, names derived from group
      dots <- split(dots[[1]], group)
      if(is.numeric(group)) { # set xrange by data
        xrange <- range(group)
        meanspacing <- diff(xrange)/(length(dots)-1)
        if(!is.numeric(xlabels))
          xlabels <- 7
        xrange <- xrange + c(-meanspacing, meanspacing)/2
        xtoplot <- as.double(names(dots))
      }
      else {
        xrange <- c(0, length(dots) + 1)
        xtoplot <- seq(length(dots))
      } # end of xrange logic
    }
    else
      stop("Multiple qw objects cannot be grouped by boxPlot")
  }
  else {
    xrange <- c(0, length(dots) + 1)
    xtoplot <- seq(length(dots))
  }
  # Open device if necessary
  if(dev.cur() == 1L)
  	setGD("BoxPlot")
  if(is.null(names(dots))) { # try to get names
    call <- as.list(match.call())[-1] # drop boxPlot
    call <- as.character(call)
    names(dots) <- call[seq(length(dots))]
  }
  ## Convert to lcens, stop if other than left or no censoring
  dots.cens <- sapply(dots, censoring)
  if(any(dots.cens == "multiple"))
  	stop("Right- or interval-censored data cannot be plotted")
  dots.lcens <- any(dots.cens == "left")
  if(dots.lcens) {
  	dots <- lapply(dots, as.lcens)
  	## Fix defaults for Box
  	Box <- setDefaults(Box, type="truncated", show.counts=TRUE, censorbox=NA,
  										 censorstyle="censored", nobox=5, width="Auto", 
  										 fill="none", truncated=c(10,90))
  	Box$type <- match.arg(Box$type, c("truncated", "simple",
  																		"tukey", "extended"))
  	Box$censorstyle <- match.arg(Box$censorstyle,
  															 c("censored", "estimated"))
  	if(Box$type == "tukey" && Box$censorstyle == "censored") {
  		warning("censored tukey boxplot not possible; creating estimated tukey boxplot")
  		Box$censorstyle <- "estimated"
  	}
  	statsret <- boxPlotCensStats(dots, Box, yaxis.log)
  } else {
  	dots <- lapply(dots, as.numeric)
  	## Fix defaults for Box
  	Box <- setDefaults(Box, type="truncated", show.counts=TRUE, 
  										 nobox=5, width="Auto", 
  										 fill="none", truncated=c(10,90))
  	Box$type <- match.arg(Box$type, c("truncated", "simple",
  																		"tukey", "extended"))
  	statsret <- boxPlotStats(dots, Box, yaxis.log)
  }
  ## Produce the boxplot
  retval <- renderBoxPlot(xtoplot, statsret$boxes, Box, statsret$explan, statsret$z,
                          yaxis.log, yaxis.range, xrange, ylabels, xlabels, xlabels.rotate,
                          xtitle, ytitle, caption, margin)
  invisible(retval)
}

#' @rdname boxPlot.lcens
#' @export
#' @method boxPlot data.frame
boxPlot.data.frame <- function(..., group=NULL, # data
															 Box=list(type="truncated", show.counts=TRUE,
															 				 censorbox = NA, censorstyle = "censored",
															 				 nobox=5, width="Auto", fill="none",
															 				 truncated=c(10,90)), # b&w controls
															 yaxis.log=FALSE, yaxis.range=c(NA,NA), # y-axis controls
															 ylabels="Auto", xlabels="Auto",
															 xlabels.rotate=FALSE, # labels
															 xtitle="", ytitle="",  caption="", # axis titles and caption
															 margin=c(NA,NA,NA,NA)) { # margin control
	## Process data to plot, all must be class "data.frame" and numeric, lcens, or qw
	##  data will be processed--all other classes of columns skipped.
	if(!is.null(group))
		stop("Data frames cannot be grouped by boxPlot")
	dots <- c(...) # creates a list--drops data.frame attributes
	dots <- dots[sapply(dots, is.numeric)]
	anyCens <- any(sapply(dots, censoring) != "none")
	# Convert all to lcens if necessary
	if(anyCens) {
		dots <- lapply(dots, as.lcens)
	} else { # force to numeric
		dots <- lapply(dots, as.numeric)
	}
	xrange <- c(0, length(dots) + 1)
	xtoplot <- seq(length(dots))
	## Fix defaults for Box
	Box <- setDefaults(Box, type="truncated", show.counts=TRUE, censorbox=NA,
										 censorstyle="censored", nobox=5, width="Auto",  fill="none",
										 truncated=c(10,90))
	Box$type <- match.arg(Box$type, c("truncated", "simple",
																		"tukey", "extended"))
	## Compute the stats and produce the boxplot
	if(dev.cur() == 1L)
		setGD("BoxPlot")
	if(anyCens) {
		statsret <- boxPlotCensStats(dots, Box, yaxis.log)
	} else {
		statsret <- boxPlotStats(dots, Box, yaxis.log)
	}
	## What gets passed from statsret?
	retval <- renderBoxPlot(xtoplot, statsret$boxes, Box, statsret$explan, statsret$z,
													yaxis.log, yaxis.range, xrange, ylabels, xlabels, xlabels.rotate,
													xtitle, ytitle, caption, margin)
	invisible(retval)
}
