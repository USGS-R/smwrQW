#'Not exported
#'
#'Compute the stats for a censored boxplot
#'
#'@param x list of the lcens data
#'@param Box the Box parameters
#'@param yaxis.log is it?
#'@return Info to actually plot the boxplot.

boxPlotCensStats <- function(x, Box, yaxis.log) {
  ##    2012Apr20 DLLorenz Created function to compute quartiles, not hinges
  ##                        (copied from uncensored version)
  ##    2012Sep18 DLLorenz Added fill option
  ##    2013Jan11 DLLorenz Roxygenized ans censortype to censorstyle
  ##    2014Jan27 DLLorenz Added censor level to exaplanation
  ##
  type <- match.arg(Box$type, c("tukey", "truncated", "simple", "extended"))
  ctype <- match.arg(Box$censorstyle, c("censored", "estimated"))
  cval <- Box$censorbox
  ## The text to use for the explanation:
  explain <- list(tukey=list(z=list(stats=matrix(c(-1.2, 0.1,.8, 1.5, 3.6),
                                      ncol=1),
                               n=54, names="",
                               out=-2.4, farout=-3.7,
                               group=c(1, 1), censored=-4.9, estimated=-Inf),
                    labels=list(expression(bold("Number of values")),
                      list(expression(bold("Largest value within 1.5 times")),
                           expression(bold("  interquartile range above")),
                           expression(bold("  75th percentile"))),
                      "75th percentile",
                      "50th percentile\n  (median)",
                      "25th percentile",
                      list(expression(bold("Smallest value within 1.5 times")),
                           expression(bold("  interquartile range below")),
                           expression(bold("  25th percentile"))),
                      list(expression(paste(bold("Outside value"), symbol("\276"),
                          "Value is > 1.5 and", sep='')),
                           " < 3  times the interquartile range",
                           "  beyond either end of box"),
                      list(expression(paste(bold("Far-out value"),symbol("\276"),
                          "Value ", is >= 3, " times", sep='')),
                           "  the interquartile range beyond", "  either end of box"),
                    						expression(bold("    Censoring level"))),
                    values=c(4.5, 3.6, 1.5, .7, 0.1, -1.2, -2.4, -3.7, -4.9),
                    IQR=expression(bold("Interquartile\nrange"))),
                  truncated=list(z=list(stats=matrix(c(-3.0, -1.2 ,0, 1.5, 3.5),
                                          ncol=1),
                                   n=54, names="", censored=-4.5, estimated=-Inf),
                    labels=list(expression(bold("Number of values")),
                      as.expression(substitute(bold(x), list(x=paste(Box$truncated[2L], "th percentile", sep='')))),
                      expression(bold("75th percentile")),
                      list(expression(bold("50th percentile")),expression(bold("  (median)"))),
                      expression(bold("25th percentile")),
                      as.expression(substitute(bold(x), list(x=paste(Box$truncated[1L], "th percentile", sep='')))),
                    						expression(bold("    Censoring level"))),
                    values=c(4.2, 3.5, 1.5, 0, -1.2, -3.0, -4.5)),
                  simple=list(z=list(stats=matrix(c(-3.0, -1.2 ,0, 1.5, 3.5),
                                       ncol=1),
                                n=54, names="", censored=-4.5, estimated=-Inf),
                    labels=list(expression(bold("Number of values")),
                      expression(bold("Maximum value")),
                      expression(bold("75th percentile")),
                      list(expression(bold("50th percentile")),expression(bold("  (median)"))),
                      expression(bold("25th percentile")),
                      expression(bold("Minimum value")),
                    						expression(bold("    Censoring level"))),
                    values=c(4.2, 3.5, 1.5, 0, -1.2, -3.0, -4.5)),
                  extended=list(z=list(stats=matrix(c(-3., -1.5, 0, 1.5, 3.),
                                         ncol=1),
                                  n=54, names="", censored=-4.5, estimated=-Inf,
                                  out=c(-3.5, 4.0), group=c(1,1)),
                    labels=list(expression(bold("Number of values")),
                      list(expression(bold("Individual value above the")),
                           as.expression(substitute(bold(x), list(x=paste("  ",
                               Box$truncated[2L], "th percentile", sep=''))))),
                      as.expression(substitute(bold(x), list(x=paste(Box$truncated[2L], "th percentile", sep='')))),
                      expression(bold("75th percentile")),
                      expression(bold("50th percentile\n (median)")),
                      expression(bold("25th percentile")),
                      as.expression(substitute(bold(x), list(x=paste(Box$truncated[1L], "th percentile", sep='')))),
                      list(expression(bold("Individual value below the")),
                           as.expression(substitute(bold(x), list(x=paste("  ",
                               Box$truncated[1L], "th percentile", sep=''))))),
                    						expression(bold("    Censoring level"))),
                    values=c(4.5, 4.0, 3, 1.5, 0, -1.5, -3., -3.5, -4.5)))
  ## Note copied from boxPlotStats in USGSgraphs
  bxp.stats <- function(x, range=1.5) {
    stats <- quantile(x, type=2, names=FALSE)
    if(range > 0) {
      iqr <- diff(stats[c(2L, 4L)])
      loout <- x < stats[2L] - iqr*range
      stats[1L] <- min(x[!loout])
      hiout <- x > stats[4L] + iqr*range
      stats[5L] <- max(x[!hiout])
    }
    else {
      loout <- hiout <- rep(FALSE, length(x))
    }
    stats <- as.matrix(stats)
    return(list(stats=stats, out=x[loout | hiout], n=length(x)))
  }
  ## Create the rest of the explanation
  explan <- setExplan(list(name="boxplot", what="none", type="solid",
                           width="standard", symbol="circle", filled=TRUE,
                           size=0.09, color="black"))
  if(ctype == "estimated") {
    if(yaxis.log)
      x <- lapply(x, fillIn, method="log ROS")
    else
      x <- lapply(x, fillIn, method="ROS")
    ## Process the data min and max needed for renderBoxPlot
    minx <- min(unlist(x), na.rm=TRUE)
    maxx <- max(unlist(x), na.rm=TRUE)
    ## Convert to common log if yaxis.log
    if(yaxis.log) {
      if(minx <= 0)
        stop("All data must be greater than 0 for log scale")
      x <- lapply(x, log10)
    }
    ## Range = 0 suppresses identification of outliers for simple and truncated
    if(type == "tukey") { # Must be forced to estimated, so this will work
      resbox <- lapply(x, bxp.stats)
    }
    else
      resbox <- lapply(x, bxp.stats, range=0)
    ## Modify resbox to add needed info, taken from uncesored boxplot stats
    for(i in names(resbox)) {
      target <- resbox[[i]] # make it easy to modify
      target$group <- target$conf <- NULL
      target$farout <- numeric(0)
      target$censored <- -Inf
      target$names <- i
      target$data <- x[[i]]
      clevs <- attr(target$data, "censorlevels")
      if(is.na(cval))
        target$estimated <- quantile(clevs, probs=0.9,
                                     type=2)
      else
        target$estimated <- cval
      if(yaxis.log)
        target$estimated <- log10(target$estimated)
      ## Verify that estimated values less than the RL are not > RL
      if(clevs[1L] > -Inf)
        target$critical <- max(target$data[seq(along=clevs)])
      else
        target$critical <- -Inf
      if(type == "truncated" && target$n > Box$nobox) { # compute 10/90
        probs <- Box$truncated/100
        tobind <- quantile(target$data, probs=probs, type=2, na.rm=TRUE)
        target$stats[1L,1L] <- tobind[1L]
        target$stats[5L,1L] <- tobind[2L]
      }
      else if(type == "extended" && target$n > Box$nobox) { # compute 10/90
        probs <- Box$truncated/100
        tobind <- quantile(target$data, probs=probs, type=2, na.rm=TRUE)
        target$stats[1L,1L] <- tobind[1L]
        target$stats[5L,1L] <- tobind[2L]
        target$out <- x[[i]][x[[i]] < tobind[1L] | x[[i]] > tobind[2L]]
      }
      else if(type == "tukey" && target$n > Box$nobox) { # any far outs?
        farfence <- 3 * (target$stats[4L,1L] - target$stats[2L,1L])
        farout <- (target$out > target$stats[4L,1L] + farfence) |
        (target$out < target$stats[2L,1L] - farfence)
        if(any(farout)) { # put into the correct spot
          target$farout <- target$out[farout]
          target$out <- target$out[!farout]
        }
      }
      if(target$n <= Box$nobox) { # suppress box, plot points
        target$out <- target$data[!is.na(target$data)]
        target$stats[seq(5L), 1L] <- target$stats[3L,1L]
      }
      ## Replace the original
      resbox[[i]] <- target 
    }
  } # End of estimated processing
  else {
    ## Process the data min and max needed for renderBoxPlot
    minx <- min(sapply(x, function(xarg) min(xarg@.Data[, 1L], na.rm=TRUE)), na.rm=TRUE)
    maxx <- max(sapply(x, function(xarg) max(xarg@.Data[, 1L], na.rm=TRUE)), na.rm=TRUE)
    ## Convert to common log if yaxis.log
    if(yaxis.log) {
      if(minx <= 0)
        stop("All data must be greater than 0 for log scale")
      x <- lapply(x, log10)
    }
    ## Manually create resbox
    resbox <- list()
    for(i in names(x)) {
      target <- list() # make it easy to modify
      target$farout <- numeric(0)
      target$estimated <- -Inf
      target$censored <- cval
      target$names <- i
      target$data <- x[[i]]
      target$n <- sum(!is.na(target$data))
      if(type == "truncated" && target$n > Box$nobox) { # compute 10/90
        probs <- c(Box$truncated[1L], 25, 50, 75, Box$truncated[2L])/100
        tobind <- quantile(target$data, probs=probs, type=2, na.rm=TRUE,
                           names=FALSE)
        target$stats <- as.matrix(tobind)
        if(is.na(target$censored)) {
          sel <- which(target$data@censor.codes)
          if(length(sel) > 0)
            target$censored <- quantile(target$data@.Data[sel, 1L],
                                        probs=0.9, type=2, na.rm=TRUE)
          else
            target$censored <- -Inf
        }
        target$out <- numeric(0)
      }
      else if(type == "extended" && target$n > Box$nobox) { # compute 10/90
        probs <- c(Box$truncated[1L], 25, 50, 75, Box$truncated[2L])/100
        tobind <- quantile(target$data, probs=probs, type=2, na.rm=TRUE,
                           names=FALSE)
        target$stats <- as.matrix(tobind)
        if(is.na(target$censored)) {
          sel <- which(target$data@censor.codes == 1)
          if(length(sel) > 0)
            target$censored <- quantile(target$data@.Data[sel, 1L],
                                        probs=0.9, type=2, na.rm=TRUE)
          else
            target$censored <- -Inf
        }
        xtemp <- target$data@.Data[, 1L]
        sel <- which((xtemp < tobind[1L] | zapsmall(xtemp) > zapsmall(tobind[5L])) & xtemp > target$censored)
        target$out <- xtemp[sel]
      }
      else { # type must be simple
        probs <- c(0, .25, .50, .75, 1.0)
        tobind <- quantile(target$data, probs=probs, type=2, na.rm=TRUE,
                           names=FALSE)
        target$stats <- as.matrix(tobind)
        if(is.na(target$censored)) {
          sel <- which(target$data@censor.codes)
          if(length(sel) > 0)
            target$censored <- quantile(target$data@.Data[sel, 1L],
                                        probs=0.9, type=2, na.rm=TRUE)
          else
            target$censored <- -Inf
        }
        target$out <- numeric(0L)
      }
      if(target$n <= Box$nobox) { # suppress box, plot points
        target$out <- target$data@.Data[!is.na(target$data), 1L]
        target$stats <- matrix(median(target$out), nrow=5)
      }
      ## Fill in resbox
      resbox[[i]] <- target 
    }
    
  } # End of censored processing
  ## End of data processing
  resbox$data.range <- c(minx, maxx)
  z <- explain[[type]]
  z$fill <- Box$fill
  return(list(boxes=resbox, explan=explan, z=z))
}
