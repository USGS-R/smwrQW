#'Q-Q Plot
#'
#'Produce a quantile-quantile (q-q) or a q-normal plot for left-censored data.
#'
#'The argument what for either \code{LineRef} or \code{Line1.1} may be set to
#'"none" to suppress drawing of either line.
#'
#' @param x the x-axis data, or data to plot if y is missing. Must be of class
#'"lcens."
#' @param y the y-axis data. If missing, then produce a quantile-normal quantile
#'plot from the data in x.
#' @param alpha the alpha value of the function for computing plotting
#'positions.
#' @param Plot control parameters of the uncensored data plot.
#' @param LineRef control parameters of the reference line (best fit between x
#'and y. See \bold{Details}.
#' @param Line1.1 control parameters for the 1:1 line. Drawn only for q-q plot.
#'See \bold{Details}.
#' @param yaxis.log log-transform the y axis?
#' @param yaxis.range set the range of the y axis.
#' @param xaxis.log log-transform the x axis?
#' @param xaxis.range set the range of the x axis.
#' @param ylabels set the y-axis labels. See \code{\link{linearPretty}} for
#'details.
#' @param xlabels set the x-axis labels. See \code{\link{linearPretty}} for
#'details.
#' @param xtitle the x-axis title.
#' @param ytitle the y-axis title.
#' @param caption the figure caption.
#' @param margin the parameters of the margin of the plot area.
#' @param Censored control parameters of the left-censored data q-normal plot.
#' @param Projected control parameters of the projected (filled in)
#'left-censored data q-normal plot.
#' @param \dots any additional arguments required for specific methods.
#' @return Information about the graph.
#' @seealso \code{\link{probPlot.lcens}}
#' @keywords hplot
#' @examples
#'\dontrun{
#'set.seed(1932)
#'Xu <- rlnorm(32)
#'setPage("sq") # required page set up
#'qqPlot(as.lcens(Xu, 1.0), yaxis.log=TRUE)
#'}
#'
#' @importFrom USGSwsGraphs qqPlot
#' @S3method qqPlot lcens
#' @method qqPlot lcens
qqPlot.lcens <- function(x, y, # data
                         alpha=0.4,
                         Plot=list(name="", what='points', type='solid',
                           width='standard', symbol='circle', filled=TRUE,
                           size=0.09, color='black'),
                         LineRef=list(name='', what='lines', color='black'),
                         Line1.1=list(name='Line of equality', what='lines',
                           color='gray'), # plot controls
                         yaxis.log=FALSE, yaxis.range=c(NA,NA), # y-axis controls
                         xaxis.log=FALSE, xaxis.range=c(NA,NA), # x-axis controls
                         ylabels=7, xlabels=7, # labels
                         xtitle,
                         ytitle, # axis titles, missing out of necessity
                         caption='', # caption 
                         margin=c(NA, NA, NA, NA),
                         Censored=list(name="", what='points', symbol='circle',
                           filled=FALSE, size=0.09, color='black'),
                         Projected=list(name="", what='points', symbol='plus',
                           filled=FALSE, size=0.09, color='black'), ...) { # margin controls
  ## Coding History:
  ##    2012Mar14 DLLorenz Method for lcens
  ##    2013Jan13 DLLorenz Roxygenized
  ##    2013Jan13          This version.
  ##
  ## create the plotting positions
  ## set up the data and axes
  if(!missing(y)) { # Q-Q plot, make synthetic values and plot as normal data
    if(missing(xtitle))
      xtitle <- deparse(substitute(x))
    if(missing(ytitle))
      ytitle <- deparse(substitute(y))
    x <- fillIn(x, method=ifelse(xaxis.log, "log ROS", "ROS"), alpha=alpha)
    y <- fillIn(y, method=ifelse(yaxis.log, "log ROS", "ROS"), alpha=alpha)
    return(qqPlot(x, y, alpha=alpha, Plot=Plot, LineRef=LineRef,
                  Line1.1=Line1.1, yaxis.log=yaxis.log, yaxis.range=yaxis.range,
                  xaxis.log=xaxis.log, xaxis.range=xaxis.range,
                  xtitle=xtitle, ytitle=ytitle, caption=caption, margin=margin))
  }
  ## Continue with Q-normal plot
  if(missing(xtitle))
    xtitle <- "Normal Quantiles"
  if(missing(ytitle))
    ytitle <- deparse(substitute(x))
  if(dev.cur() == 1)
    setGD("QQPlot")
  mdl <- mdlROS(x, method=ifelse(yaxis.log, "log ROS", "ROS"), alpha=alpha)
  qnpp <- censpp(x, alpha)
  x <- qnorm(qnpp$pp)
  y <- qnpp$x
  ycen <- qnpp$xcen
  xcen <- qnorm(qnpp$ppcen)
  yfit <- mdl$fitted[seq(along=ycen)]
  yset <- mdl$fitted
  xset <- qnorm(ppoints(yset, alpha)) # close enough for range
  ## Set reverse option for y-axis, needed as default
  yaxis.rev <- FALSE
  if(is.list(ylabels))
    yax <- c(list(data=yset, axis.range=yaxis.range, axis.log=yaxis.log,
                  axis.rev=yaxis.rev), ylabels)
  else
    yax <- list(data=yset, axis.range=yaxis.range, axis.log=yaxis.log,
                axis.rev=yaxis.rev, axis.labels=ylabels)
  yax <- do.call("setAxis", yax)
  if(yaxis.log) { # simple conversion
    y <- log10(y)
    ycen <- log10(ycen)
    yfit <- log10(yfit)
  }
  yax <- yax$dax
  if(is.list(xlabels))
    xax <- c(list(data=xset, axis.range=xaxis.range, axis.log=xaxis.log,
                  axis.rev=FALSE), xlabels)
  else
    xax <- list(data=xset, axis.range=xaxis.range, axis.log=xaxis.log,
                axis.rev=FALSE, axis.labels=xlabels)
  
  xax <- do.call("setAxis", xax)
  xax <- xax$dax
  ## set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  ## Set up the defaults for the points and line:
  LineRef <- setDefaults(LineRef, name="", what="lines", color="black")
  Censored <- setDefaults(Censored, name="", what='points', symbol='circle',
                          filled=FALSE, size=0.09, color='black')
  Projected <- setDefaults(Projected, name="", what='points', symbol='plus',
                           filled=FALSE, size=0.09, color='black')
  ##
  plot(x, y, type='n', xlim=xax$range, xaxs='i', axes=FALSE,
       ylim=yax$range, yaxs='i', ylab="", xlab="")
  Plot <- setPlot(Plot, name="", what='points', type='solid',
                  width='standard', symbol='circle', filled=TRUE,
                  size=0.09, color='black') # force defaults if not set
  explan <- setExplan(Plot) # add info to set up explanation
  plotPars <- explan$current
  points(x, y, type=plotPars$type, lwd=plotPars$lwd, lty=plotPars$lty,
         pch=plotPars$pch, cex=plotPars$cex, col=plotPars$col, bg=plotPars$col)
  box(lwd=frameWt())
  ## label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(xax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  ## Add lines if requested
  retval <- (list(x=x, y=y, yaxis.log=yaxis.log, yaxis.rev=yaxis.rev,
                  xaxis.log=xaxis.log, explanation=explan, margin=margin))
  ## Suppress any log transforms for next section
  retval$yaxis.log <- retval$xaxis.log <- FALSE
  ## Add the reference line and censored/projected points if requested
  if(LineRef$what != "none") {
    ## Adjust mdl if yaxis.log=TRUE
    if(yaxis.log) {
      mdl[[1]] <- mdl[[1]]/2.302585
      mdl[[2]] <- mdl[[2]]/2.302585
    }
    if(LineRef$name == "") # do not add to explanation
      refLine(coefficients=c(mdl[[1]], mdl[[2]]),
              Plot=LineRef, current=retval)
    else # do add to explanation
      retval <- refLine(coefficients=c(mdl[[1]], mdl[[2]]),
                        Plot=LineRef, current=retval)
  }
  if(Censored$what != "none") {
    if(Censored$name == "")
      addXY(xcen, ycen, Plot=Censored, current=retval)
    else
      retval <- addXY(xcen, ycen, Plot=Censored, current=retval)
  }
  if(Projected$what != "none") {
    if(Projected$name == "")
      addXY(xcen, yfit, Plot=Projected, current=retval)
    else
      retval <- addXY(xcen, yfit, Plot=Projected, current=retval)
  }
  ## recover the log-transforms if necessary
  retval$yaxis.log <- yaxis.log
  retval$xaxis.log <- xaxis.log
  retval$yax <- yax
  retval$xax <- xax
  invisible(retval)
}
