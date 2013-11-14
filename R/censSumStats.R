#'Summary Statistics
#'
#'Compute selected summary statistics for censored data.
#'
#'@param \dots any number of vectors or a data.frame of the data to
#'be summarized.
#'@param group the data to group the data in \code{\dots}.
#'@param Nums logical, include the number of observations and numbers of 
#'censored values in the output data.frame.
#'@param Stats logical, include the mean and standard deviation
#'in the output data.frame.
#'@param Probs the argument to quantile specifying the desired probabilities
#'in the output data.frame
#'@param na.rm logical, remove missing values before processing?
#'@param method the method to use for computing the statistics.
#'@return A data.frame with the desired statistcs for each variable
#'in \code{\dots} grouped by each unique value in \code{group}.
#'@keywords univar
#'@export
censSumStats <- function(..., group=NULL, Nums=TRUE, Stats=TRUE,
                     Probs=(0:4)/4, na.rm=TRUE, method="log MLE") {
  ## Coding history:
  ##    2013Feb25 DLLorenz Original Coding from sumStats
  ##    2013Feb25          This version.
  ##
  ## Internal function to comupte # left, rigth, and interval censoring
  NumCen <- function(x) {
    x <- as.mcens(x)
    NL <- sum(x@censor.codes == -1, na.rm=TRUE)
    NR <- sum(x@censor.codes == 1, na.rm=TRUE)
    NI <- sum(x@interval, na.rm=TRUE)
    return(c(NL, NR, NI))
  }
  ## Begin
  dots <- list(...)
  ndg <- length(dots)
  if(ndg == 1) {
    dotname <- deparse(substitute(...))
    dots <- dots[[1]]
    if(mode(dots) == "numeric") {
      dots <- list(dots)
      names(dots) <- dotname
    }
  }
  else { # multiple vectors were specified
    dotname <- as.list(match.call())
    ## Drop named components
    dotname$Num <- dotname$group <- dotname$Stats <- dotname$Probs <- dotname$na.rm <- NULL
    dotname <- sapply(dotname, deparse)
    names(dots) <- dotname[-1] # drop the call to sumStats
  }
  dots <- dots[sapply(dots, is.numeric)]
  ndg <- length(dots)
  ## Fix names of Probs so that they can be converted to a data.frame
  if(!is.null(Probs) && length(Probs) > 0)
    ProbNames <- paste("Pct", round(Probs * 100,
                                    if(length(Probs) > 1) 2 - log10(diff(range(Probs)))
                                    else 2), sep = ".")
  else
    ProbNames = ""
  if(!is.null(group)) {
    groups <- interaction(group, drop=TRUE)
    retval <- by(as.data.frame(dots), INDICES=groups, FUN=censSumStats, 
                 Nums=Nums, Stats=Stats, Probs=Probs, na.rm=na.rm,
                 method=method)
    retgrp <- by(as.data.frame(group, stringsAsFactors=FALSE),
                 INDICES=groups, FUN=function(x, n) {
                   xx <- x[1,,drop=FALSE]
                   if(n > 1) {
                     xx <- as.data.frame(lapply(xx, rep, times=n), stringsAsFactors=FALSE)
                   }
                   xx	
                 }, n=ndg)
    retval <- do.call("rbind", retval)
    ## The by functions appears to strip a single column data frame of its class
    if(class(retgrp[[1]]) != 'data.frame') {
      retgrp <- as.matrix(unlist(retgrp))
      colnames(retgrp) <- "Group" # assign a simple name
    }
    else
      retgrp <- do.call("rbind", retgrp)
    retval <- cbind(retgrp, retval)
    ## Fix case where there is only one variable--results in Variable == dots
    if(ndg == 1 && !is.null(dotname))
      levels(retval$Variable) <- dotname
  }
  else { # no grouping
    retval <- lapply(dots, function(x, na.rm, Nums, Stats, Probs,
                                    ProbNames, method) {
      retpart <- double()
      if(!is.null(Nums) && Nums) {
        retpart <- c(sum(!is.na(x)), NumCen(x))
        names(retpart) <- c("Num", "Nleft", "Nright", "Ninterval")
      }
      if(!is.null(Stats) && Stats) {
        retStats <- censStats(x, method=method, na.rm=na.rm)
        retpart <- c(retpart, Mean=retStats$mean, StdDev=retStats$sd,
                     MeanLog=retStats$meanlog, StdDevLog=retStats$sdlog)
      }
      if(!is.null(Probs) && length(Probs) > 0) {
        retprob <-  quantile(as.mcens(x), probs=Probs, na.rm=na.rm, type=2)
        names(retprob) <- ProbNames
        retpart <- c(retpart, retprob)
      }
      retpart
    } # end of function
                     ,na.rm=na.rm, Nums=Nums, Stats=Stats,
                     Probs=Probs, ProbNames=ProbNames, method=method)
    VarNames <- names(retval)
    retval <- as.data.frame(do.call("rbind", retval))
    if(!is.null(VarNames))
      retval <- cbind(Variable=VarNames, retval)
  } # end of else
  row.names(retval) <- seq(nrow(retval))
  return(retval)
}
