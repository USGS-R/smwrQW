#' @title Censored Data Methods for Function \code{xyPlot}
#'
#' @description Produce a scatter plot of censored data.
#'
#' Setting \code{xlabels} to 0 or negative values will 
#'suppress ticks and labels. If negative, then try to create that absolute value
#'number of labels. That can be useful for relative axes or specialized labeling,
#'but is only valid for numeric \code{x}.
#' 
#' @include mcens-class.R lcens-class.R qw-class.R
#' @name xyPlot-censored
#' @param x the x-axis data.
#' @param y the y-axis data.
#' @param Plot control parameters of the plot for uncensored data.
#' @param yaxis.log log-transform the y axis?
#' @param yaxis.rev reverse the y axis?
#' @param yaxis.range set the range of the y-axis.
#' @param xaxis.log logical, if \code{TRUE}, then log-transform the x axis.
#' @param xaxis.range set the range of the x-axis.
#' @param ylabels set up y-axis labels. See \code{\link{linearPretty}} for
#' details.
#' @param xlabels set up x-axis labels. See \bold{Details}.
#' @param xtitle the x-axis title.
#' @param ytitle the y-axis title.
#' @param caption the figure caption.
#' @param margin set up the plot area margins.
#' @param Censored control parameters of the plot for censored data. Same arguments as 
#'for \code{Plot}, but adds \code{bar} which if set to \code{TRUE} will draw lines from
#'left-censored values to the minimum and draw lines from right-censored values to the
#'maximum. No bars are drawn for interval censored values.
#' @param ... additional arguments for specific methods.
#' @return Information about the graph.
#' @note A call should be made to \code{setPage} to set up the graphics
#' environment before calling \code{xyPlot}.
#' @keywords methods hplot
#' @importMethodsFrom smwrGraphs xyPlot
#' @exportMethod xyPlot

#' @rdname xyPlot-censored
#' @aliases xyPlot,numeric,lcens-method
setMethod("xyPlot", signature("numeric", "lcens"), 
function(x, y, # data
         Plot=list(name="", what="points", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.log=FALSE, yaxis.rev=FALSE,
         yaxis.range=c(NA, NA), # y-axis controls
				 xaxis.log=FALSE, xaxis.range=c(NA,NA), # x-axis controls
         ylabels=7, xlabels=7, # labels
         xtitle=deparse(substitute(x)),
         ytitle=deparse(substitute(y)), # axis titles
         caption="", # caption 
         margin=c(NA, NA, NA, NA), # margin controls
         Censored=list(name="", what="points", symbol="circle",
           filled=FALSE, size=0.09, color="black", width="hairline",
           bar=FALSE), # additional plot controls
         ...) {
  ##
  ## set up the axes
  xtitle <- xtitle
  ytitle <- ytitle
  if(dev.cur() == 1)
    setGD("XYPlot")
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
  xlabel0 <- FALSE
  if(is.list(xlabels))
  	xax <- c(list(data=x, axis.range=xaxis.range, axis.log=xaxis.log,
  								axis.rev=FALSE), xlabels)
  else {
  	if(is.numeric(xlabels) && length(xlabels) == 1 && xlabels <= 0) {
  		xlabel0 <- TRUE
  		xax <- list(data=x, axis.range=xaxis.range, axis.log=xaxis.log,
  								axis.rev=FALSE, axis.labels=max(2, -xlabels))
  	}
  	else
  		xax <- list(data=x, axis.range=xaxis.range, axis.log=xaxis.log,
  								axis.rev=FALSE, axis.labels=xlabels)
  }
  if(!is.null(Plot$what) && Plot$what == "lines")
  	xax$extend.range <- FALSE
  xax <- do.call("setAxis", xax)
  x <- xax$data
  xax <- xax$dax
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
  ## Suppress ticks and labels if requested
  if(xlabel0)
  	bot$ticks <- bot$labels <- top$ticks <- top$labels <- FALSE
  par(mar=margin)
  ## set graph
  plot(x, y, type='n', xlim=xax$range, xaxs='i', axes=FALSE,
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
                 xaxis.log=xaxis.log, explanation=explan, margin=margin,
                 yax=yax, xax=xax)
  ## Add censored values
  if(any(Cen)) {
    Censored <- setDefaults(Censored, name="Left censored", what='points', symbol='circle',
                            filled=FALSE, size=0.09, color='black',  width="hairline", bar=FALSE)
    addXY(xcen, ycen, Plot=Censored, current=list(yaxis.log = FALSE, 
       yaxis.rev = yaxis.rev, xaxis.log = xaxis.log))
    if(Censored$bar) {
    	ymin <- rep(min(yax$range), length(ycen))
      segments(xcen, ymin, xcen, ycen, col=Censored$color, lwd=lineWt(Censored$width))
    }
  }
  box(lwd=frameWt())
  ## label the axes
  renderY(yax, lefttitle=ytitle, left=left, right=right)
  renderX(xax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  invisible(retval)
}
)

#' @rdname xyPlot-censored
#' @aliases xyPlot,Date,qw-method
setMethod("xyPlot", signature("numeric", "qw"),
function(x, y, # data
         Plot=list(name="", what="points", type="solid",
           width="standard", symbol="circle", filled=TRUE,
           size=0.09, color="black"), # plot controls
         yaxis.log=FALSE, yaxis.rev=FALSE,
         yaxis.range=c(NA, NA), # y-axis controls
				 xaxis.log=FALSE, xaxis.range=c(NA,NA), # x-axis controls
				 ylabels=7, xlabels=7, # labels
         xtitle="",
         ytitle=deparse(substitute(y)), # axis titles
         caption="", # caption 
         margin=c(NA, NA, NA, NA), # margin controls
         Censored=list(name="", what="points", symbol="circle",
           filled=FALSE, size=0.09, color="black", width="hairline",
           bar=FALSE), # additional plot controls
         ...) {
  ##
  y <- as.lcens(y)
  xyPlot(x, y, Plot=Plot, yaxis.log=yaxis.log, yaxis.rev=yaxis.rev,
  			 yaxis.range=yaxis.range, xaxis.log=xaxis.log,
  			 xaxis.range=xaxis.range,
  			 ylabels=ylabels, xlabels=xlabels, xtitle=xtitle,
  			 ytitle=ytitle, caption=caption, margin=margin,
  			 Censored=Censored, ...)
})

#' @rdname xyPlot-censored
#' @aliases xyPlot,lcens,lcens-method
setMethod("xyPlot", signature("lcens", "lcens"), 
function(x, y, # data
				 Plot=list(name="", what="points", type="solid",
				 					width="standard", symbol="circle", filled=TRUE,
				 					size=0.09, color="black"), # plot controls
				 yaxis.log=FALSE, yaxis.rev=FALSE,
				 yaxis.range=c(NA, NA), # y-axis controls
				 xaxis.log=FALSE, xaxis.range=c(NA,NA), # x-axis controls
				 ylabels=7, xlabels=7, # labels
				 xtitle=deparse(substitute(x)),
				 ytitle=deparse(substitute(y)), # axis titles
				 caption="", # caption 
				 margin=c(NA, NA, NA, NA), # margin controls
				 Censored=list(name="", what="points", symbol="circle",
				 							filled=FALSE, size=0.09, color="black", width="hairline",
				 							bar=TRUE), # additional plot controls
				 ...) {
	##
	## set up the axes
	xtitle <- xtitle
	ytitle <- ytitle
	if(dev.cur() == 1)
		setGD("XYPlot")
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
	xrange <- range(x@.Data[,1L], na.rm=TRUE) * c(.95, 1.05)
	if(is.list(xlabels)) {
		xax <- c(list(data=xrange, axis.range=xaxis.range, axis.log=xaxis.log,
									axis.rev=FALSE), xlabels)
	} else {
		xax <- list(data=xrange, axis.range=xaxis.range, axis.log=xaxis.log,
								axis.rev=FALSE, axis.labels=xlabels)
	}
	xax <- do.call("setAxis", xax)
	xax <- xax$dax
	## Prep the data, as of current version, only lcens works
	##  may need to set up for mcens method, requires S4 object
	Good <- !is.na(y) & !is.na(x)
	y <- y[Good]
	if(yaxis.log)
		y <- log10(y)
	x <- x[Good]
	if(xaxis.log)
		x <- log10(x)
	CenY <- y@censor.codes
	CenX <- x@censor.codes
	Cen <- CenY | CenX
	y <- y@.Data[,1L] # y as lcens is gone now
	x <- x@.Data[,1L] # x as lcens is gone now
	xcen <- x[Cen]
	x <- x[!Cen]
	ycen <- y[Cen]
	y <- y[!Cen]
	# Set CenY and CenX to point to the ycen and xcen values that have that respective censoring
	CenY <- CenY[Cen]
	CenX <- CenX[Cen]
	## set margins and controls
	margin.control <- setMargin(margin, yax)
	margin <- margin.control$margin
	right <- margin.control$right
	top <- margin.control$top
	left <- margin.control$left
	bot <- margin.control$bot
	par(mar=margin)
	## set graph
	plot(x, y, type='n', xlim=xax$range, xaxs='i', axes=FALSE,
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
								 yax=yax, xax=xax)
	## Add censored values
	if(any(Cen)) {
		Censored <- setDefaults(Censored, name="censored", what='points', symbol='circle',
														filled=FALSE, size=0.09, color='black', width="hairline", bar=FALSE)
		addXY(xcen, ycen, Plot=Censored, current=list(yaxis.log = FALSE, 
																									yaxis.rev = yaxis.rev, xaxis.log = FALSE))
		if(Censored$bar) {
			if(any(CenY)) {
				ymin <- rep(min(yax$range), sum(CenY))
				segments(xcen[CenY], ymin, xcen[CenY], ycen[CenY], col=Censored$color, lwd=lineWt(Censored$width))
			}
			if(any(CenX)) {
				xmin <- rep(min(xax$range), sum(CenX))
				segments(xmin, ycen[CenX], xcen[CenX], ycen[CenX], col=Censored$color, lwd=lineWt(Censored$width))
			}
		}
	}
	box(lwd=frameWt())
	## label the axes
	renderY(yax, lefttitle=ytitle, left=left, right=right)
	renderX(xax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
	invisible(retval)
}
)

#' @rdname xyPlot-censored
#' @aliases xyPlot,numeric,mcens-method
setMethod("xyPlot", signature("numeric", "mcens"), 
  function(x, y, # data
  				 Plot=list(name="", what="points", type="solid",
  				 					width="standard", symbol="circle", filled=TRUE,
  				 					size=0.09, color="black"), # plot controls
  				 yaxis.log=FALSE, yaxis.rev=FALSE,
  				 yaxis.range=c(NA, NA), # y-axis controls
  				 xaxis.log=FALSE, xaxis.range=c(NA,NA), # x-axis controls
  				 ylabels=7, xlabels=7, # labels
  				 xtitle=deparse(substitute(x)),
  				 ytitle=deparse(substitute(y)), # axis titles
  				 caption="", # caption 
  				 margin=c(NA, NA, NA, NA), # margin controls
  				 Censored=list(name="", what="points", symbol="circle",
  				 							filled=FALSE, size=0.09, color="black", width="hairline",
  				 							bar=TRUE), # additional plot controls
  				 ...) {
  	##
  	## set up the axes
  	xtitle <- xtitle
  	ytitle <- ytitle
  	if(dev.cur() == 1)
  		setGD("XYPlot")
  	## Buffer by .95, 1.05 to avoid ending at exactly rounded values
  	yrange <- range(y@.Data[is.finite(y@.Data)]) * c(.95, 1.05)
  	if(is.list(ylabels))
  		yax <- c(list(data=yrange, axis.range=yaxis.range, axis.log=yaxis.log,
  									axis.rev=yaxis.rev), ylabels)
  	else
  		yax <- list(data=yrange, axis.range=yaxis.range, axis.log=yaxis.log,
  								axis.rev=yaxis.rev, axis.labels=ylabels)
  	yax <- do.call("setAxis", yax)
  	yax <- yax$dax
  	xlabel0 <- FALSE
  	if(is.list(xlabels))
  		xax <- c(list(data=x, axis.range=xaxis.range, axis.log=xaxis.log,
  									axis.rev=FALSE), xlabels)
  	else {
  		if(is.numeric(xlabels) && length(xlabels) == 1 && xlabels <= 0) {
  			xlabel0 <- TRUE
  			xax <- list(data=x, axis.range=xaxis.range, axis.log=xaxis.log,
  									axis.rev=FALSE, axis.labels=max(2, -xlabels))
  		}
  		else
  			xax <- list(data=x, axis.range=xaxis.range, axis.log=xaxis.log,
  									axis.rev=FALSE, axis.labels=xlabels)
  	}
  	if(!is.null(Plot$what) && Plot$what == "lines")
  		xax$extend.range <- FALSE
  	xax <- do.call("setAxis", xax)
  	x <- xax$data
  	xax <- xax$dax
  	## Prep the data, as of current version, 
  	Good <- !is.na(y) & !is.na(x)
  	y <- y[Good]
  	if(yaxis.log)
  		y <- log10(y)
  	x <- x[Good]
  	CenCd <- y@censor.codes
  	Cen <- CenCd != 0L | y@interval
  	ycen <- ifelse(CenCd == -1L, y@.Data[,2L],
  								 ifelse(CenCd == 1L, y@.Data[,1L],
  								 			 ifelse(Cen, rowMeans(y@.Data), 0)))[Cen]
  	CenCd <- CenCd[Cen]
  	y <- y@.Data[,1L][!Cen] # y as mcens is gone now
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
  	## Suppress ticks and labels if requested
  	if(xlabel0)
  		bot$ticks <- bot$labels <- top$ticks <- top$labels <- FALSE
  	par(mar=margin)
  	## set graph
  	plot(x, y, type='n', xlim=xax$range, xaxs='i', axes=FALSE,
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
  								 xaxis.log=xaxis.log, explanation=explan, margin=margin,
  								 yax=yax, xax=xax)
  	## Add censored values
  	if(any(Cen)) {
  		Censored <- setDefaults(Censored, name="Left censored", what='points', symbol='circle',
  														filled=FALSE, size=0.09, color='black',  width="hairline", bar=FALSE)
  		addXY(xcen, ycen, Plot=Censored, current=list(yaxis.log = FALSE, 
  																									yaxis.rev = yaxis.rev, xaxis.log = xaxis.log))
  		if(Censored$bar) {
  			picks <- CenCd == -1L
  			ymin <- rep(min(yax$range), length(ycen))
  			segments(xcen[picks], ymin[picks], xcen[picks], ycen[picks], 
  							 col=Censored$color, lwd=lineWt(Censored$width))
  			picks <- CenCd == 1L
  			ymax <- rep(max(yax$range), length(ycen))
  			segments(xcen[picks], ymax[picks], xcen[picks], ycen[picks], 
  							 col=Censored$color, lwd=lineWt(Censored$width))	
  			
  		}
  	}
  	box(lwd=frameWt())
  	## label the axes
  	renderY(yax, lefttitle=ytitle, left=left, right=right)
  	renderX(xax, bottitle=xtitle, bottom=bot, top=top, caption=caption)
  	invisible(retval)
  }
)
