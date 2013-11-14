#'Censored Data Methods for Function \code{timePlot}
#'
#'Produce a plot of censored time-series data.
#'
#'
#' @name timePlot-methods
#' @aliases timePlot-methods timePlot,Date,lcens-method timePlot.lcens
#'timePlot,Date,qw-method timePlot.qw
#' @docType methods
#' @section Methods: \describe{
#'
#'\item{list('signature(x = "Date", y = "lcens")')}{ This is the method for
#'left-censored data, \code{x} must be class "Date." See \code{\link{timePlot}}
#'for details about the arguments. There is an optional argument
#'\code{Censored} that controls how the censored data are plotted. It functions
#'like the \code{Plot} argument, but has an additional argument \code{bar},
#'which draws a drop line if set to \code{TRUE}. } 
#'
#'\item{list('signature(x = "Date", y = "qw")')}{ This is the method for
#'water-quality data, \code{x} must be class "Date." It converts \code{y}
#'to class "lcens" and executes that method. If \code{y} cannot be converted,
#'then the clall fails. } }
#' @keywords methods hplot
#' @importMethodsFrom USGSwsGraphs timePlot
#' @exportMethod timePlot
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
      lines(retval$x, retval$y, type="h", col=Censored$color)
  }
  box(lwd=frameWt())
  ## label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(dax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  invisible(retval)
}
)

#' @rdname timePlot-methods
#' @exportMethod timePlot
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
