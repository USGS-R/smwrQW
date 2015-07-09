#' @title Censored Data Methods for Function \code{timePlot}
#'
#' @description Produce a plot of censored time-series data.
#'
#' @details The value for \code{xlabels} must be one of "full," the full month names;
#'"abbrev," abbreviations; or "letter," the first letter of the
#'month. The default is "Auto," which selects one of the rpevious options based on
#'the number of labels.
#' 
#' @include mcens-class.R lcens-class.R qw-class.R
#' @name timePlot-censored
#' @param x the time/date data.
#' @param y the y-axis data.
#' @param Plot control parameters of the plot for uncensored data.
#' @param yaxis.log log-transform the y axis?
#' @param yaxis.rev reverse the y axis?
#' @param yaxis.range set the range of the y-axis.
#' @param xaxis.range set the range of the x-axis.
#' @param ylabels set up y-axis labels. See \code{\link{linearPretty}} for
#' details.
#' @param xlabels set up x-axis labels. See \bold{Details}.
#' @param xtitle the x-axis title.
#' @param ytitle the y-axis title.
#' @param caption the figure caption.
#' @param margin set up the plot area margins.
#' @param Censored control parameters of the plot for censored data.
#' @param ... arguments for specific methods.
#' @return Information about the graph.

#' @keywords methods hplot
#' @importMethodsFrom smwrGraphs timePlot
#' @exportMethod timePlot

#' @rdname timePlot-censored
#' @aliases timePlot,Date,lcens-method
setMethod("timePlot", signature("Date", "lcens"), # Need to allow Date only
function(x, y, # data
         Plot=list(name="", what="points", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.log=FALSE, yaxis.rev=FALSE,
         yaxis.range=c(NA, NA), # y-axis controls
         xaxis.range=range(x, na.rm=TRUE), # x-axis control
         ylabels=7, xlabels="Auto", # labels
         xtitle="",
         ytitle=deparse(substitute(y)), # axis titles
         caption="", # caption 
         margin=c(NA, NA, NA, NA), # margin controls
         Censored=list(name="", what="points", symbol="circle",
           filled=FALSE, size=0.09, color="black",
           bar=FALSE), # additional plot controls
         ...) {
  ## Coding History:
  ##    2012Mar21 DLLorenz Original coding from timePlot
  ##    2012Mar23 DLLorenz Modifed timePlot to be able to be forced generic
  ##    2012Sep27 DLLorenz Modified to use timePlot as generic in USGSgraphs
  ##                        and force Date only as x
  ##    2013Oct22 DLLorenz Bugfix for lcens data.
  ##
  ## set up the axes
  xtitle <- xtitle
  ytitle <- ytitle
  if(dev.cur() == 1)
    setGD("DateTimePlot")
  ## Buffer by .95, 1.05 to avoid ending at exactly rounded values
  yrange <- range(y@.Data[,1L], na.rm=TRUE) * c(.95, 1.05)
  if(is.list(ylabels))
    yax <- c(list(data=yrange, axis.range=yaxis.range, axis.log=yaxis.log,
                  axis.rev=yaxis.rev), ylabels)
  else
    yax <- list(data=yrange, axis.range=yaxis.range, axis.log=yaxis.log,
                axis.rev=yaxis.rev, axis.labels=ylabels)
  yax <- do.call("setAxis", yax)
  yax <- yax$dax
  dax <- datePretty(xaxis.range, major=xlabels)
  ## Prep the data, as of current version, only lcens works
  ##  may need to set up for mcens method, requires S4 object
  Good <- !is.na(y) & !is.na(x)
  y <- y[Good]
  if(yaxis.log)
    y <- log10(y)
  x <- x[Good]
  Cen <- y@censor.codes
  ycen <- y@.Data[,1L][Cen]
  y <- y@.Data[,1L][!Cen] # y as lcens is gone now
  x <- numericData(x)
  xcen <- x[Cen]
  x <- x[!Cen]
  ## set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  ## set graph
  plot(x, y, type='n', xlim=dax$range, xaxs='i', axes=FALSE,
       ylim=yax$range, yaxs='i', ylab="", xlab="")
  ## Single plot only option
  Plot <- setPlot(Plot, name="", what='points', type='solid',
                  width='standard', symbol='circle', filled=TRUE,
                  size=0.09, color='black') # force defaults if not set
  explan <- setExplan(Plot) # add info to set up explanation
  plotPars <- explan$current
  points(x, y, type=plotPars$type, lwd=plotPars$lwd, lty=plotPars$lty,
         pch=plotPars$pch, cex=plotPars$cex, col=plotPars$col, bg=plotPars$col)
  retval <- list(x=x, y=y, yaxis.log=yaxis.log, yaxis.rev=yaxis.rev,
                 xaxis.log=FALSE, explanation=explan, margin=margin,
                 yax=yax, xax=dax)
  ## Add censored values
  if(any(Cen)) {
    Censored <- setDefaults(Censored, name="Left censored", what='points', symbol='circle',
                            filled=FALSE, size=0.09, color='black', bar=FALSE)
    addXY(xcen, ycen, Plot=Censored, current=list(yaxis.log = yaxis.log, 
       yaxis.rev = yaxis.rev, xaxis.log = FALSE))
    if(Censored$bar)
      lines(xcen, ycen, type="h", col=Censored$color)
  }
  box(lwd=frameWt())
  ## label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(dax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  invisible(retval)
}
)

#' @rdname timePlot-censored
#' @aliases timePlot,Date,qw-method
setMethod("timePlot", signature("Date", "qw"), # Need to allow Date only
function(x, y, # data
         Plot=list(name="", what="points", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.log=FALSE, yaxis.rev=FALSE,
         yaxis.range=c(NA, NA), # y-axis controls
         xaxis.range=range(x, na.rm=TRUE), # x-axis control
         ylabels=7, xlabels="Auto", # labels
         xtitle="",
         ytitle=deparse(substitute(y)), # axis titles
         caption="", # caption 
         margin=c(NA, NA, NA, NA), # margin controls
         Censored=list(name="", what="points", symbol="circle",
           filled=FALSE, size=0.09, color="black",
           bar=FALSE), # additional plot controls
         ...) {
  ## Coding History:
  ##    2013Oct22 DLLorenz Original coding
  ##
  y <- as.lcens(y)
  timePlot(x, y, Plot=Plot, yaxis.log=yaxis.log, yaxis.rev=yaxis.rev,
           yaxis.range=yaxis.range, xaxis.range=xaxis.range,
           ylabels=ylabels, xlabels=xlabels, xtitle=xtitle,
           ytitle=ytitle, caption=caption, margin=margin,
           Censored=Censored, ...)
})
