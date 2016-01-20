#' Empirical Distribution Function
#'
#' Produces a graph of the empirical distribution function: methods 
#'for "lcens" and "qw" data.
#'
#'
#' @aliases ecdfPlot.lcens ecdfPlot.qw
#' @rdname ecdfPlot.lcens
#' @param x the data to plot. Missing values are removed before plotting.
#' @param group create groups of the left-censored data vector.
#' @param Plot control parameters of the uncensored data plot.
#' @param xaxis.log log-transform the x axis?
#' @param xaxis.range set the range of the x axis.
#' @param xlabels set the x-axis labels. See \code{\link{linearPretty}} for
#'details.
#' @param ylabels set the y-axis labels. See \code{\link{linearPretty}} for
#'details.
#' @param ytitle the y-axis title.
#' @param xtitle the x-axis title.
#' @param caption the figure caption.
#' @param margin the parameters of the margin of the plot area.
#' @param \dots any additional arguments needed by specific methods.
#' @return Information about the graph.
#' @seealso \code{\link{probPlot.lcens}}
#' @keywords hplot
#' @examples
#'\dontrun{
#'set.seed(1932)
#'Xu <- rlnorm(32)
#'setPage("sq") # required page set up
#'ecdfPlot(as.lcens(Xu, 1.0))
#'}
#'
#' @import smwrGraphs 
#' @export
#' @method ecdfPlot lcens
ecdfPlot.lcens <- function(x, group=NULL, # data specification
                           Plot=list(name="Auto", what="stairstep", type="solid",
                             width="standard", symbol="circle", filled=TRUE,
                             size=0.09, color="Auto"), # plot controls
                           xaxis.log=TRUE, xaxis.range=c(NA, NA), # x-axis controls
                           xlabels=11,  ylabels=5, # labels
                           ytitle="Cumulative Probability",
                           xtitle=deparse(substitute(x)), # axis titles
                           caption="", # caption
                           margin=c(NA, NA, NA, NA), ...) { # margin control
  ## Coding History:
  ##    2013Feb22 DLLorenz Original coding for lcens object
  ##
  xtitle <- xtitle # needed to 'set' names
  ## Process data
  if(is.null(group))
    xtoplot <- mdlKM(x)
  else
    xtoplot <- mdlKM(x, group)
  xval <- -xtoplot$time
  ## Set up the axes and transform data
  if(dev.cur() == 1)
    setGD("ECDFPlot")
  xax <- setAxis(xval, xaxis.range, xaxis.log, FALSE, xlabels)
  xax <- xax$dax
  yax <- linearPretty(c(0,1), hard=TRUE, labels=ylabels, extend.range=FALSE)
  ## Set up the plot
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  plot(range(xval), c(0,1), type="n", xlim=xax$range, xaxs="i", axes=FALSE,
       ylim=yax$range, yaxs="i", ylab="", xlab="")
  ## No group
  if(is.null(group)) {
    Plot <- setPlot(Plot, name="", what="stairstep", type="solid",
                    width="standard", symbol="circle", filled=TRUE,
                    size=0.09, color="black") # force defaults if not set
    explan <- setExplan(Plot) # add info to set up explanation
    plotPars <- explan$current
    xval <- rev(xval)
    xval <- c(xval[1], xval) # Duplicate the first observation to be a riser
    if(xaxis.log)
      xval <- log10(xval)
    y <- c(rev(xtoplot$surv), 1)
    points(xval, y, type=plotPars$type, lwd=plotPars$lwd, lty=plotPars$lty,
           pch=plotPars$pch, cex=plotPars$cex, col=plotPars$col,
           bg=plotPars$col)
  }
  else {
    Ngrp <- length(xtoplot$strata)
    Plot <- setGroupPlot(Plot, Grps=Ngrp,
                         name=substring(names(xtoplot$strata), 7),
                         what="stairstep", type="solid",
                         width="standard", symbol="circle", filled=TRUE,
                         size=0.09, color="Auto")
    explan <- Plot$Explan
    plotPars <- Plot$current
    strt <- 1L
    for(i in seq(Ngrp)) {
      end <- strt - 1L + xtoplot$strata[i]
      xval <- -rev(xtoplot$time[seq(strt, end)])
      xval <- c(xval[1], xval)
      if(xaxis.log)
        xval <- log10(xval)
      y <- c(rev(xtoplot$surv[seq(strt, end)]), 1)
      strt <- end + 1L
      points(xval, y, type=plotPars$what[i],
             lwd=plotPars$lwd[i], lty=plotPars$lty[i],
             pch=plotPars$pch[i], cex=plotPars$cex[i], col=plotPars$col[i],
             bg=plotPars$col[i])
    }
  }
  box(lwd=frameWt())
  ## label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(xax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  invisible(list(x=x, group=group, yaxis.log=FALSE, yaxis.rev=FALSE,
                 xaxis.log=xaxis.log, explanation=explan, margin=margin,
                 yax=yax, xax=xax))
}

#' @export
#' @method ecdfPlot qw
#' @rdname ecdfPlot.lcens
ecdfPlot.qw <- function(x, group=NULL, # data specification
                           Plot=list(name="Auto", what="stairstep", type="solid",
                             width="standard", symbol="circle", filled=TRUE,
                             size=0.09, color="Auto"), # plot controls
                           xaxis.log=TRUE, xaxis.range=c(NA, NA), # x-axis controls
                           xlabels=11,  ylabels=5, # labels
                           ytitle="Cumulative Probability",
                           xtitle=deparse(substitute(x)), # axis titles
                           caption="", # caption
                           margin=c(NA, NA, NA, NA), ...) { # margin control
  ## Coding History:
  ##    2013Feb22 DLLorenz Original coding for lcens object
  ##
  xtitle <- xtitle # needed to 'set' names
  ## Process data and pass to next function
  x <- as.lcens(x)
  ret <- ecdfPlot(x=x, group=group, Plot=Plot, xaxis.log=xaxis.log,
                  xaxis.range=xaxis.range, xlabels=xlabels, ylabels=ylabels,
                  ytitle=ytitle, caption=caption, margin=margin, ...)
  invisible(ret)
}
