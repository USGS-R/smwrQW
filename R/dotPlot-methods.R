#'Censored Data Methods for Function \code{dotPlot}
#'
#'Produce a dor plot of censored data.
#'
#'
#' @name dotPlot-methods
#' @aliases dotPlot-methods dotPlot,mcens-method dotPlot.mcens
#' dotPlot,qw-method dotPlot.qw
#' @docType methods
#' @section Methods: \describe{
#'
#'\item{list('signature(x = "mcens", y = "ANY")')}{ This is the method for
#'multiply-censored data, \code{x} must be class "mcens." 
#'See \code{\link{dotPlot}} for details about the arguments. There is an 
#'optional argument \code{Censored} that controls how the censored data are 
#'plotted. It functions like the \code{Plot} argument, but has an additional 
#'argument \code{bar}, which draws a connecting line if set to \code{TRUE}. }
#'
#'\item{list('signature(x = "qw", y = "ANY")')}{ This is the method for
#'water-quality data, \code{x} must be class "qw." It simply converts \code{x}
#'to class "mcens" and executes that method. } }
#' @keywords methods hplot
#' @importMethodsFrom USGSwsGraphs dotPlot
#' @exportMethod dotPlot
setMethod("dotPlot", signature("mcens"), # "ANY" ingnored in last position
function(x, y, # data
         Plot=list(name="", what="points", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.orient="table", yaxis.order="none",
         yaxis.grid=TRUE, # y-axis controls
         xaxis.log=FALSE, xaxis.range=c(NA,NA), # x-axis controls
         ylabels="full",
         xlabels=7, # labels
         xtitle=deparse(substitute(x)),
         ytitle="", # axis titles
         caption="", # caption 
         margin=c(NA, NA, NA, NA), jitter.y=TRUE,
         Censored=list(name="", what="points", symbol="circle",
           filled=FALSE, size=0.09, color="black",
           bar=TRUE), # additional plot controls
         ...) { # margin controls
  ## Coding History:
  ##    2013Jan21 DLLorenz Original coding from dotPlot
  ##
  ## create the plotting positions
  ## set up the axes
  xtitle=xtitle # needed to 'set' names
  ytitle=ytitle
  yaxis.orient <- match.arg(yaxis.orient, c("table", "grid"))
  if(length(yaxis.order) == 1)
    yaxis.order <- match.arg(yaxis.order, c("none", "ascending", "descending"))
  ylabels <- match.arg(ylabels, c("full", "abbreviate"))
  if(dev.cur() == 1)
    setGD("DotPlot")
  ## Quick fix for numeric Y
  if(is.numeric(y))
    y <- as.character(y)
  yax <- namePretty(y, orientation=yaxis.orient, order=yaxis.order,
                    label.abbr=ylabels == "abbreviate")
  ylev <- yax$labels
  y <- numericData(y, ylev)
  ## Establish the range
  xrng <- range(x@.Data[is.finite(x@.Data)])
  if(is.list(xlabels))
    xax <- c(list(data=xrng, axis.range=xaxis.range, axis.log=xaxis.log,
                  axis.rev=FALSE), xlabels)
  else
    xax <- list(data=xrng, axis.range=xaxis.range, axis.log=xaxis.log,
                axis.rev=FALSE, axis.labels=xlabels)
  
  xax <- do.call("setAxis", xax)
  xax <- xax$dax
  ## Set margins and controls
  margin.control <- setMargin(margin, yax)
  margin <- margin.control$margin
  right <- margin.control$right
  top <- margin.control$top
  left <- margin.control$left
  bot <- margin.control$bot
  par(mar=margin)
  ## Set up plot
  plot(xrng, range(y), type="n", xlim=xax$range, xaxs="i", axes=FALSE,
       ylim=yax$range, yaxs="i", ylab="", xlab="")
  if(yaxis.grid) # draw light, gray lines
    segments(x0=xax$range[1], y0=y, x1=xax$range[2], y1=y,
             col="gray", lty=1, lwd=frameWt())
  ## Process plot control
  what=Plot$what[1]
  parms <- setMultiPlot(Plot, length(x), name="", what="points", type="solid",
                        width="standard", symbol="circle", filled=TRUE,
                        size=0.09, color="black") # part of returned data
  cens <- setMultiPlot(Censored, length(x), name="", what="points", type="solid",
                       width="standard", symbol="circle", filled=FALSE,
                       size=0.09, color="black") # Ignore bar for now
  plot.info <- as.data.frame(parms$current, stringsAsFactors=FALSE)
  ## Process censored plots
  ## Need to work this into the explanation
  cens.info <- as.data.frame(cens$current, stringsAsFactors=FALSE)
  Cens <- x@censor.codes | x@interval
  if(any(Cens)) {
    cens.info$bar <- NULL # Must be removed
    plot.info[Cens, ] <- cens.info[Cens, ]
  }
  ## Draw each point
  if(jitter.y) {
    Grps <- unique(plot.info$name)
    if(length(Grps) == 1L)
      jitter.y <- 0
    else {
      Rng <- .4 - exp(-length(Grps)) # more-or-less works to expand range
      jitter.y <- seq(-Rng, Rng, length.out=length(Grps))
      names(jitter.y) <- Grps
      jitter.y <- jitter.y[plot.info$name] # one for each!
    }
  }
  xloplot <- x@.Data[, 1L]
  points(xloplot, y + jitter.y, type="p", pch=plot.info$pch, cex=plot.info$cex,
         col=plot.info$col, bg=ifelse(plot.info$filled, plot.info$col, 0))
  if(any(Cens)) {
    xhiplot <- x@.Data[, 2L]
    points(xhiplot[Cens], (y + jitter.y)[Cens], type="p", pch=plot.info$pch[Cens],
           cex=plot.info$cex[Cens], col=plot.info$col[Cens],
           bg=ifelse(plot.info$filled, plot.info$col, 0)[Cens])
    if(Censored$bar) {
      xloplot <- pmax(xloplot, xax$range[1L])
      xhiplot <- pmin(xhiplot, xax$range[2L])
      segments(xloplot[Cens], (y + jitter.y)[Cens], xhiplot[Cens],
               col=plot.info$col[Cens])
    }
  }
  box(lwd=frameWt())
  ## label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(xax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  invisible((list(x=x, y=y, yaxis.log=FALSE, yaxis.rev=FALSE,
                  yaxis.lev=ylev,
                  xaxis.log=xaxis.log, explanation=parms$Explan, margin=margin,
                  yax=yax, xax=xax)))
}
)

#' @rdname dotPlot-methods
#' @exportMethod dotPlot
setMethod("dotPlot", signature("qw"), # "ANY" ingnored in last position
function(x, y, # data
         Plot=list(name="", what="points", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.orient="table", yaxis.order="none",
         yaxis.grid=TRUE, # y-axis controls
         xaxis.log=FALSE, xaxis.range=c(NA,NA), # x-axis controls
         ylabels="full",
         xlabels=7, # labels
         xtitle=deparse(substitute(x)),
         ytitle="", # axis titles
         caption="", # caption 
         margin=c(NA, NA, NA, NA), jitter.y=TRUE,
         Censored=list(name="", what="points", symbol="circle",
           filled=FALSE, size=0.09, color="black",
           bar=TRUE), # additional plot controls
         ...) { # margin controls
  ## Coding History:
  ##    2013Oct22 DLLorenz Original coding
  ##
  x <- as.mcens(x)
  dotPlot(x=x, y=y, Plot=Plot, yaxis.origin=yaxis.origin,
  yaxis.order=yaxis.order, yaxis.grid=yaxis.grid, xaxis.log=xaxis.log,
  xaxis.range=xaxis.range, ylabels=ylabels, xlabels=xlabels,
  xtitle=xtitle, ytitle=ytitle, caption=caption, margin=margin,
  jitter.y=jitter.y, Censored=Censored, ...)
})
