#'Diagnostic Plot 
#'
#'Plot summary AMLE/MLE model diagnostics.
#'
#'Seven  graphs can be produced by this function. If \code{which} is "All," then all plots are produced.
#'The argument \code{which} can also be the name of an explanatory variable so that a partial residual 
#'plot is created for a single variable. Or \code{which} can be any of a sequence of numbers from 1 thorugh 7.
#'Numeric values for \code{which}:
#'\enumerate{
#'\item Fitted vs. Observed
#'\item Fitted vs. Residual
#'\item S-L plot
#'\item A correlogram if dates are available in the model or in the data set
#'\item Q-normal plot
#'\item Influence plot
#'\item Partial residual plots for each explanatory variable
#'}
#'
#'@param x an object of class "censReg"---output from \code{censReg}
#'@param which either "All" or any of a sequence from 1 to 7 indicating which plot, see \bold{Details}.
#'@param set.up set up the graphics page?
#'@param span the span to use for the loess smooth. Set to 0 to suppress.
#'@param \dots further arguments passed to or from other methods.
#'@return The object \code{x} is returned invisibly.
#'@seealso \code{\link{censReg}}
#'@keywords regression hplot
#'@S3method plot summary.censReg
#'@method plot summary.censReg
plot.summary.censReg <- function(x, which='All', set.up=TRUE, span=1.0, ...) {
  ## Coding history:
  ##    2012Dec26 DLLorenz Initial Coding
  ##    2012Dec31 DLLorenz Roxygenized
  ##    2013May21 DLLorenz Added option to plot individual partial residual plots
  ## 
  ## Identify which plots to do:
  ## 1 Fitted - Actual
  ## 2 Fitted - Residual
  ## 3 S-L
  ## 4 correlogram (only if dates are available)
  ## 5 Q - normal
  ## 6 Influence
  ## 7 Residual dependence plots
  ##
  ## Set up graphics page
  if(set.up)
    setGD("censReg")
  ## Set up to do all plots
  doPlot <- rep(TRUE, 7L)
  do7 <- FALSE
  if(is.numeric(which)) {
    if(min(which) > 0) # select which to plot
      doPlot[seq(7L)[-which]] <- FALSE
    else # which not to plot
      doPlot[-which] <- FALSE
  }
  else if(is.character(which) && which[1L] != "All") {
    doPlot[1:6] <- FALSE
    xnames <- which
    do7 <- TRUE
  }
  ## Anything else produces all plots
  ## Final plot (7) residuals vs predictors
  ## Extract weights and other data needed later
  Fits <- x$diagstats$yhat # needed for later plots
  Resids <- x$diagstats$resid
  Cens <- x$diagstats$ycen
  wt.showCD <- if(is.null(x$object$weights))
    rep(1,length(Fits))
  else 
    x$object$weights
  if(doPlot[7L]) {
    xpred <- x$object$XLCAL[, -1L, drop=FALSE]
    xparm <- x$object$PARAML[-1L]
    names(xparm) <- c(dimnames(xpred)[[2L]], "Scale")
    if(!do7) # get all explanatory variable names
      xnames <- dimnames(xpred)[[2L]]
    ## Residual dependence plots
    if(do7 || length(xnames) > 1L) {
      for(i in xnames) {
        presid <- Resids + xparm[i] * xpred[, i]
        xyPlot(xpred[, i], presid,
               Plot=list(what="points", size=0.05),
               xtitle=i, ytitle="Partial Residual",
               margin=c(NA, NA, 1.5, .5))
        if(span > 0)
          addSmooth(xpred[, i], presid, family="sym", span=span)
        refLine(coefficients=c(0, xparm[i]),
                Plot=list(what="lines", width="standard", type="dashed"))
        ## The p-value of the second order fit on the residuals almost matches
        ## the p-value of adding the second order term to the regression
        nl.p <- summary(lm(presid ~ poly(xpred[,i], 2L),
                           weights=wt.showCD, model=TRUE), FALSE)
        nl.p <- nl.p$coefficients[3L, 4L]
        addTitle(Main=paste("Second order polynomial test for linearity: p=",
                   round(nl.p, 4L), sep=""))
      }
    }
  } # end of Residual dependence plots
  ## Show the approximate effect that each flagged point has on the regression
  ## (an influence plot)
  Act <- Fits + Resids # also needed later
  if(doPlot[6L]) {
    xyPlot(Fits, Act,
           Plot=list(what="points", size=0.02),
           xtitle="Fitted", ytitle="Actual")
    refLine(coefficients=c(0,1), Plot=list(what="lines", width="standard", type="dashed"))
    fg.showCD <- which(x$flagobs)
    for(j in seq(along=fg.showCD)) {
      i <- fg.showCD[j]
      ## generate a random color
      Col <- sprintf("#%02x%02x%02x", as.integer(runif(1, 10, 245)),
                     as.integer(runif(1, 10, 245)),
                     as.integer(runif(1, 10, 245)))
      refLine(coefficients=lsfit(Fits[-i], Act[-i], wt.showCD[-i])$coef,
              Plot=list(what="lines", color=Col, width="color"))
      addXY(Fits[i], Act[i],
            Plot=list(what="points", color=Col, size=0.04))
      labelPoints(Fits[i], Act[i], dimnames(x$x)[[1L]][i],
                  dir="NE", size=12, color=Col)
    }
  } # end of Influence plot
  ## Q-normal of standardized residuals (H&H criterion 4)
  if(doPlot[5L]) {
    sresid <- Resids / sqrt(x$object$PARAML[x$object$NPAR+1])
    qqPlot(sresid, Plot=list(size=0.05, filled=FALSE),
           yaxis.log=FALSE, ylabels=7,
           ytitle="Standardized Residual",
           margin=c(NA, NA, 2.4, NA), mean=0, sd=1)
    ## Add the uncensored values in solid
    ord <- order(sresid)
    ## Cens == 0 for uncensored regardless if left only or multiply censored
    xt <- qnorm(ppoints(sresid, a=0.4))
    yt <- sresid[ord]
    ct <- Cens[ord] == 0
    addXY(xt[ct], yt[ct], Plot=list(what="points", size=0.05, filled=TRUE))
  }
  ## for the next plots use weighted residuals
  Res <- Resids * sqrt(wt.showCD)
  ## If possible, plot a correlogram--requires finding 1 datelike column in
  ## the data
  if(doPlot[4L] && !is.null(x$object$call$data)) {
    data <- x$object$call$data
    if(is.name(data))
      data <- try(eval(data))
    if(class(data) != "try-error") {
      anyDate <- which(sapply(data, isDateLike))
      if(length(anyDate) == 1L) { # if more than 1, 
        Date <- dectime(data[[anyDate]])
        if(!is.null(skips <- x$object$na.action)) {
          if(attr(skips, "class") == "omit")
            Date <- Date[-skips] # Must remove if class is omit 
        }
        corGram(Date, Res)
      }
    }
  }
  ## Add details of call on regression model to next plots
  Mod <- format(x$object$call$formula)
  ## 3rd plot, S-L
  RSE <- rmse(x$object) # now the resid std. error
  if(doPlot[3L]) {
    Slres <- residuals(x$object, suppress.na.action=TRUE, type="S-L")
    xyPlot(Fits, Slres,
           Plot=list(what="points", size=0.05),
           xtitle="Fitted",
           ytitle=as.expression(substitute(sqrt(abs(YL)),
               list(YL = as.name("Residuals")))),
           margin=c(NA, NA, 2.4, NA))
    if(span > 0)
      addSmooth(Fits, Slres, family="sym", span=span)
    ## 0.82218 is the expected value of the sqrt(abs(x)) for a normal dist.:
    ## integrate(function(x) sqrt(abs(x))*dnorm(x), -Inf, Inf)
    refLine(horizontal=0.82218*sqrt(RSE), Plot=list(what="lines", width="standard", type="dashed"))
  } # end of S-L 
  ## 2nd plot response vs. fit
  if(doPlot[2L]) {
    xyPlot(Fits, Res,
           Plot=list(what="points", size=0.05),
           xtitle="Fitted",
           ytitle="Residuals")
    if(span > 0)
      addSmooth(Fits, Res, family="sym", span=span)
    refLine(horizontal=0, Plot=list(what="lines", width="standard", type="dashed"))
  }
  ## First plot is actual vs fitted, with regression details
  if(doPlot[1L]) {
    Act2 <- Fits + residuals(x$object, suppress.na.action=TRUE, type="response")
    xyPlot(Fits, Act2,
           Plot=list(what="points", size=0.07, filled=FALSE),
           xtitle=paste("Fitted:", Mod, sep=" "),
           ytitle="Response")
    if(span > 0)
      addSmooth(Fits, Act, family="sym", span=span)
    refLine(coefficients=c(0,1), Plot=list(what="lines", width="standard", type="dashed"))
    ## Add data details
    if(x$object$method == "AMLE")
      status <- 1L + x$object$CENSFLAG
    else # method is MLE
      status <- x$object$survreg$y[, 3L]
    addXY(Fits[status == 1L], Act2[status == 1L],
      Plot=list(what="points", size=0.07, filled=TRUE))
    if(any(status == 0L)) # Greater thans
      for(i in which(status == 0L))
        refLine(vertical=Fits[i], yrange=c(Act2[i], NA))
    if(any(status == 0L)) # Less thans
      for(i in which(status == 2L))
        refLine(vertical=Fits[i], yrange=c(NA, Act2[i]))
    ## Leave interval as open circle
    ## Add some details, regression eqn and RSE
    Eqn <- coefficients(x$object, summary=FALSE)
    names(Eqn)[1L] <- ""
    Eqn <- paste(as.character(round(Eqn, 3)), names(Eqn), sep=" ")
    Eqn <- paste(Eqn, collapse=" + ")
    Resp <- deparse(x$object$call$formula[[2L]])
    if(x$object$dist == "lognormal")
      Resp <- paste("log(", Resp, ")", sep="")
    else if(x$object$dist == "commonlog")
      Resp <- paste("log10(", Resp, ")", sep="")
    Resp <- as.expression(substitute(hat(R), 
        list(R=as.name(Resp))))
    Model <- expression()
    RSE <- signif(RSE, 3)
    legend("topleft", legend=c(
                        as.expression(substitute(hat(R) == EQN, 
                            list(R=as.name(deparse(x$object$call$formula[[2]])),
                                 EQN=Eqn))),
                        paste("Residual Standard Error: ", RSE, sep="")), bty="n")
  }
  invisible(x)
}
