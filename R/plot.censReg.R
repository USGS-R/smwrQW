#'Diagnostic Plot 
#'
#'Plot simple AMLE/MLE model diagnostics.
#'
#'Two graphs can be produced by this function. Number 1 is a plot of fitted
#'versus actual and number 2 is a Q--normal plot. More complete diagnostic
#'plots are available from the \code{\link{summary.censReg}} output.
#'
#' @param x an object of class "censReg"---output from \code{censReg}
#' @param which either "All," 1, or 2 indicating which plot, see \bold{Details}.
#' @param set.up set up the graphics page?
#' @param \dots further arguments passed to or from other methods.
#' @return The object \code{x} is returned invisibly.
#' @seealso \code{\link{censReg}}
#' @keywords regression hplot
#' @export
#' @method plot censReg
plot.censReg <- function(x, which='All', set.up=TRUE, ...) {
  ## Coding history:
  ##    2012Dec26 DLLorenz Initial Coding
  ##    2012Dec31 DLLorenz Roxygenized
  ##    2013Jan13          This version
  ## 
  ## Identify which plots to do:
  ## 1 Fitted - Actual
  ## 2 Q - normal
  ##
  ## Set up graphics page
  if(set.up) 
    setGD("CENSREG")
  ## Set up to do all plots
  doPlot <- rep(TRUE, 2L) 
  if(is.numeric(which)) {
    if(min(which) > 0) # select which to plot
      doPlot[seq(2L)[-which]] <- FALSE
    else # which not to plot
      doPlot[-which] <- FALSE
  }
  if(doPlot[2L]) {
  	Res <- residuals(x, type="response")/rmse(x)
  	Cen <- x$CENSFLAG
  	if(is.logical(Cen)) {
  		Res <- as.lcens(Res, censor.codes=Cen)
  	} else {
  		Res <- as.mcens(Res, censor.codes=Cen)
  	}
  	qqPlot(Res, Plot=list(what="points", size=0.05),
  				 ytitle="Standardized Residual",
  				 LineRef=list( what="lines"), Censored=list(size=0.05),
  				 Projected=list(what="none"))
  	refLine(coefficients=c(0,1))
  }
  if(doPlot[1L]) {
  	Res <- x$YLCAL
  	Cen <- x$CENSFLAG
  	if(is.logical(Cen)) {
  		Res <- as.lcens(Res, censor.codes=Cen)
  	} else {
  		Res <- as.mcens(Res, censor.codes=Cen)
  	}
    xyPlot(fitted(x), Res,
           Plot=list(what="points", size=0.05),
           xtitle="Fitted Values",
           ytitle="Response")
    refLine(coefficients=c(0,1))
  }
  invisible(x)
}
