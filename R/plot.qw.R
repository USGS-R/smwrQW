#'Plot Water-Quality Data
#'
#'Create a diagnostics plot of the reporting levels and observations
#'
#'
#'@aliases plot.qw
#'@param x an object of class "qw."
#'@param which which plot, ignored in current version.
#'@param set.up set up the graphics page?
#'@param \dots additional arguments passed to \code{xyPlot}.
#'@return A plot is created and the data are invisibly returned.
#'@seealso \code{\link{timePlot.lcens}}
#'@keywords hplot censored
#'@examples
#'
#'## see vignettes
#'
#'@S3method plot qw
#'@method plot qw
plot.qw <- function(x, which='All', set.up=TRUE, ...) {
  ## Coding History:
  ##    2012Sep23 DLLorenz Original Coding
  ##
  ## Set up graphics page
  if(set.up)
    setGD("QW")
  ## Set up to do the "All" plot
  Rmks <- x@remark.codes
  Vals <- x@.Data[,2L]
  N <- length(Vals)
  Ydata <- unique(x@analyte.name)
  if(length(Ydata) == 1) { # good a single analyte
    Yunits <- unique(x@reporting.units)
    if(length(Yunits) == 1)
      Ydata <- paste(Ydata, Yunits, sep=", in ")
  }
  else # unknown/mixed
    Ydata <- "Unknown/Mixed"
  ## Update for censored values
  Vals <- ifelse(Rmks == ">", x@.Data[,1L], Vals)
  Vals <- ifelse(Rmks == "I", (x@.Data[,1L] + x@.Data[,2L])/2, Vals)
  Cen <- Rmks %in% c("<", ">", "I")
  X <- seq(along=Vals)
  ## Note need to add bars to censored data, but next version!
  ## Also need debugging, but a good start
  if(any(!Cen)) { # We do have uncensored data
    AA.pl <- xyPlot(X[!Cen], Vals[!Cen], Plot=list(what="points"),
                    xaxis.range=c(0, N+1), ..., xtitle="Index Number",
                    ytitle=Ydata)
    if(any(Cen))
      AA.pl <- addXY(X[Cen], Vals[Cen], Plot=list(what="points",
                                          filled=FALSE), current=AA.pl)
  } else {
    if(any(Cen))
      AA.pl <- xyPlot(X[Cen], Vals[Cen], Plot=list(what="points"),
                      xaxis.range=c(0, N+1), ..., xtitle="Index Number",
                      ytitle=Ydata)
    else
      stop("No data")
  }
  ## Add the detection limits with methods
  DLs <- x@reporting.level
  if(all(is.na(DLs)))
    warning("No reporting levels")
  else {
    X <- c(X, N+1) - 0.5
    DLs <- c(DLs, DLs[N])
    Ms <- x@analyte.method
    Ms <- c(Ms, Ms[N])
    ## Collapse to form complete lines
    keep <- !is.na(DLs)
    X <- X[keep]
    DLs <- DLs[keep]
    Ms <- Ms[keep]
    Mu <- unique(Ms)
    Cols <- setColor(Mu)
    for(i in seq(along=Mu)) {
      X2pl <- ifelse(Ms == Mu[i], X, NA)
      addXY(X2pl, DLs, Plot=list(what="stairstep", color=Cols[i]),
            current=AA.pl)
    }
  }
  invisible(x)
}
