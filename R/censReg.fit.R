#' @title Support Functions for Censored Regression
#'
#' @description These are support functions for fitting censored regression. They perform
#'either AMLE or MLE regression.
#'
#' @importFrom survival survreg Surv
#' @aliases censReg_AMLE.fit censReg_MLE.fit
#' @param Y the response data.
#' @param X a matrix of the explanatory variables, including the intercept term.
#' @param dist the distribuiton, either "normal" or "lognormal."
#' @param Wt observation weights.
#' @return a list containing the output from the FORTRAN code,or converted from
#'survreg.
#' @seealso \code{\link{censReg}}, \code{\link{survreg}}
#' @keywords censored utilities
#' @useDynLib smwrQW evalaml
#' @rdname censReg.fit
#' @export
censReg_AMLE.fit <- function(Y, X, dist="normal") {
  ## Coding history:
  ##    2012Jul23 DLLorenz Original Coding
  ##    2012Jul31 DLLorenz Modifications for prediction and load estimation
  ##    2012Sep25 DLLorenz Begin mods for various options
  ##    2012Dec28 DLLorenz Roxygenized
  ##    2012Dec28          This version
  ##
  NPAR <- ncol(X)
  NOBSC <- nrow(X)
  if(dist == "lognormal")
    Y <- log(Y)
  if(dist == "commonlog")
    Y <- log10(Y)
  fit <- .Fortran("evalaml",
                  NOBSC = as.integer(NOBSC),
                  NPAR = as.integer(NPAR),
                  XLCAL = as.matrix(X),
                  YLCAL = Y@.Data[, 1L],
                  YD = Y@.Data[, 2L],
                  CENSFLAG = Y@censor.codes,
                  PARMLE = double(NPAR + 1L),
                  PARAML = double(NPAR + 1L),
                  BIAS = double(NPAR + 1L),
                  CV = matrix(0., NPAR + 1L, NPAR + 1L),
                  SBIAS = double(NPAR + 1L),
                  SCV = matrix(0., NPAR + 1L, NPAR + 1L),
                  STDDEV = double(NPAR + 1L),
                  PVAL = double(NPAR + 1L),
                  COV = matrix(0., NPAR + 1L, NPAR + 1L),
                  RESID = double(NOBSC),
                  RSQ = double(1L),
                  LLR = double(1L),
                  SCORR = double(1L),
                  LLRAML = double(1L),
                  PLEVAML = double(1L),
                  DF = integer(1L),
                  LogNorm = dist == "lognormal",
                  YPRED = double(NOBSC),
                  AIC = double(1L),
                  SPPC = double(1L),
                  IERR = integer(1L))
  fit$weights=rep(1., NOBSC)
  fit$dist <- dist
  fit$method <- "AMLE"
  return(fit)
}

#' @rdname censReg.fit
#' @export
censReg_MLE.fit <- function(Y, X, Wt, dist="normal") {
  ## Coding history:
  ##    2012Jul23 DLLorenz Original Coding
  ##    2012Jul31 DLLorenz Modifications for prediction and load estimation
  ##    2012Sep25 DLLorenz Begin mods for various options
  ##    2012Dec28 DLLorenz Roxygenized
  ##    2012Dec28          This version
  ##
  ## Convert Y to Surv
  if(class(Y)[1L] == "lcens") {
    time <- time2 <- Y@.Data[, 1L]
    time[Y@censor.codes] <- NA
    c.codes <- 0 - Y@censor.codes # Change to mcens logic
  } else if(class(Y)[1L] == "mcens") {
    time <-  miss2na(Y@.Data[, 1L], -Inf)
    time2 <- miss2na(Y@.Data[, 2L], Inf)
    c.codes <- Y@censor.codes
  } else {
    stop("Unable to process objects of class ", class(Y))
  }
  survdist <- dist
  if(dist == "normal") {
    survdist <- "gaussian"
    Resp <- Surv(time, time2, type="interval2")
    YLCAL <- rowMeans(cbind(time, time2), na.rm=TRUE) # Use the mid range for interval censoring
  }
  else if(dist == "lognormal") {
    survdist <- "lognormal"
    Resp <- Surv(time, time2, type="interval2")
    YLCAL <- log(rowMeans(cbind(time, time2), na.rm=TRUE))
  }
  else { # Distribution must be "commonlog"
    survdist <- "gaussian"
    Resp <- Surv(log10(time), log10(time2), type="interval2")
    YLCAL <- log10(rowMeans(cbind(time, time2), na.rm=TRUE))
    }
  if(missing(Wt))
    Wt <- 1.0
  Wt <- rep(Wt, length.out = length(time))
  # Normalize weights
  Wt <- Wt/mean(Wt)
  sr <- survreg(Resp ~ X -1, weights=Wt, dist=survdist)
  ## must package into list like AMLE
  NOBSC = length(time)
  NPAR = ncol(X)
  bcor <- NOBSC/(NOBSC - NPAR)
  BIAS <- c(rep(0, NPAR), -NPAR/NOBSC)
  SBIAS <- c(rep(0, NPAR), (-.5*(NPAR - 1) + .25)/NOBSC) # For MLE, only dependent on N and P
  ll <- sr$loglik[length(sr$loglik)] # Prtect against null model
  ## Construct "working" residuals
  fits <- predict(update(sr, na.action=na.omit), type="linear") # Force no NAs
  CENSFLAG <- c.codes
  RESID <- YLCAL - fits
  sigma <- sr$scale*sqrt(bcor)
  for(i in seq(NOBSC)) {
    if(CENSFLAG[i] > 0) #right censored
      RESID[i] <- sigma*dnorm(RESID[i], 0, sigma)/(1 - pnorm(RESID[i], 0, sigma))
    else if(CENSFLAG[i] < 0) #left censored
      RESID[i] <- -sigma*dnorm(RESID[i], 0, sigma)/pnorm(RESID[i], 0, sigma)
  }
  ## Compute YPRED, at least as MLE corrected values
  YPRED <- predict(sr)
  if(dist == "lognormal") {
  	YPRED <- YPRED * exp(0.5*sr$scale^2) # MLE adj.
  } else if(dist == "commonlog") {
  	YPRED <- 10^YPRED * exp(0.5*(sr$scale*log(10))^2) # MLE adj.
  }
  vcv <- sr$var[-(NPAR+1L), -(NPAR+1L)]
  ## construct fit for prediction
  fit <- list(NOBSC = NOBSC,
              NPAR = NPAR,
              XLCAL = X,
              YLCAL = YLCAL,
              YD = double(NOBSC),
              CENSFLAG = CENSFLAG,
              PARMLE = as.double(c(sr$coefficients, sr$scale^2)),
              PARAML = as.double(c(sr$coefficients, sigma^2)),
  						BIAS = BIAS,
  						CV = cbind(rbind(vcv/sigma^2*bcor, 0), -BIAS),
  						SBIAS = SBIAS,
  						SCV = rbind(cbind(vcv/sigma^2*bcor,0), 
  												c(rep(0, NPAR), sr$var[NPAR+1L, NPAR+1L])),
  						STDDEV = c(sqrt(diag(sr$var)))*sqrt(bcor),
  						PVAL = summary(sr)$table[,"p"],
  						COV = rbind(cbind(vcv,0), -BIAS*sr$scale^4),
  						RESID = RESID,
  						RSQ = NA_real_,
  						LLR = ll,
  						SCORR = NA_real_,
              LLRAML = double(1L),
              PLEVAML = double(1L),
              DF = integer(1L),
              LogNorm = dist == "lognormal",
              YPRED = YPRED,
              AIC = 2 * (NPAR + 1 - ll),
              SPPC = log(NOBSC) * (NPAR + 1 - ll),
              IERR = integer(1L),
              dist=dist,
              weights=Wt,
              method="MLE",
              survreg=sr)
  return(fit)
}
