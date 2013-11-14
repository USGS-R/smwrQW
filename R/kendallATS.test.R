#'Kendall' tau
#'
#'Compute Kendall's tau for left-censored data and the Akritas-Theil-Sen
#'slope estimator.
#'
#'@param x a left-censored data object
#'@param y a left-censored data object
#'@param na.rm remove missing values before performing the test?
#'@return an object of class "htest" having these components:
#'\item{statistic}{ the value of the taub statistic.}
#'\item{p.value}{ the attained p-value for the test.}
#'\item{data.name}{ a character string describing the name of the data 
#'used in the test.}
#'\item{method}{ a description of the method.}
#'\item{alternative}{ a description alternate hypotheses, always "two-sided."}
#'\item{null.value}{ the value of taub for the null hypothesis.}
#'\item{estimate}{ the ATS slope, and medians of \code{x} and \code{y}.}
#'\item{coef}{ the coefficients of the monotic fit line. See \bold{Note}.}
#'\item{slope.se}{ an estimate of the standard error of the slope.}
#'@references Akritas, M.G., Murphy, S.A., and LaValley, M.P., 1995,
#'The Theil-Sen estimator with doubly-censored data and applications to
#'astronomy: Journal of the American Statistical Association, v. 90, p.
#'170--177.\cr
#'Helsel, D.R. 2005, Nondetects and data analysis: New York, Wiley, 266 p.
#'
#'@note The intercept is computed as slope * -median.x + median.y. The coef
#'vector is designed to graphically represent the relation between x and y.
#'It is not intended for prediction proposes.
#'@keywords htest censored
#'@examples
#'# Simple no censoring
#'set.seed(450)
#'tmp.X <- rnorm(25)
#'tmp.Y <- tmp.X/2 + rnorm(25)
#'cor.test(tmp.X, tmp.Y, method="k", exact=FALSE, continuity=TRUE)
#'kendallATS.test(tmp.X, tmp.Y)
#'# Some censoring
#'kendallATS.test(as.lcens(tmp.X, -1), as.lcens(tmp.Y, -1))
#'
#'@useDynLib USGSwsQW ktau
#'@export
kendallATS.test <- function(x, y, na.rm=TRUE) {
  ## Coding History:
  ##    2005Jun30 DLLorenz original code modifed from Dennis Helsel Minitab 
  ##    2005Jul14 DLLorenz date fix
  ##    2006Apr11 DLLorenz Modifed to print slope and medians
  ##    2006Jun05 DLLorenz Modifed bounds on slope estimates
  ##    2006Jun26 DLLorenz Bug fix on modified bounds
  ##    2006Jul21 DLLorenz Bug fix to Ktau
  ##    2008Nov21 DLLorenz Name change--appended .test
  ##    2009Jan02 DLLorenz Modified order of medians on output
  ##    2010Feb17 DLLorenz Modified return value
  ##    2013Feb21 DLLorenz Conversion to R
  ##
  ## Basic data checks.
  data.name <-  paste(deparse(substitute(x)), "and",deparse(substitute(y)))
  x <- as.lcens(x)
  y <- as.lcens(y)
  ## Remove NAs if desired.
  if(na.rm) {
    OKs <- !is.na(y) & !is.na(x)
    y <- y[OKs,]
    x <- x[OKs,]
  }
  else
    if(any(is.na(x) | is.na(y)))
      stop("Missing values not allowed unless na.rm=TRUE")
  n <- length(x)
  if(n < 3)
    stop("y and x should effectively be longer than 2.")
  ## Median ovalues.
  median.y <-median(y)
  median.x <- median(x)
  xx <- x@.Data[, 1L]
  cx <- x@censor.codes
  yy <- y@.Data[, 1L]
  cy <- y@censor.codes
  delx <- min(diff(sort(unique(xx)))) / 1000.
  dely <- min(diff(sort(unique(yy)))) / 1000.
  dupx <- xx - delx * cx
  diffx <- outer(dupx, dupx, "-")
  diffcx <- outer(cx, cx, "-")
  xplus <- outer(cx, -cx, "-")
  dupy <- yy - dely * cy
  diffy <- outer(dupy, dupy, "-")
  diffcy <- outer(cy, cy, "-")
  yplus <- outer(cy, -cy, "-")
  signyx <- sign(diffy * diffx)
  tt <- (sum(1-abs(sign(diffx)))-n)/2     # no. of pairwise ties in x
  uu <- (sum(1-abs(sign(diffy)))-n)/2     # no. of pairwise ties in y
  cix <- sign(diffcx)*sign(diffx)
  cix <- ifelse(cix <= 0, 0, 1)
  tt <- tt + sum(cix) / 2
  signyx <- signyx * (1 - cix)
  ciy <- sign(diffcy)*sign(diffy)
  ciy <- ifelse(ciy <= 0, 0, 1)
  uu <- uu + sum(ciy) / 2
  signyx <- signyx * (1 - ciy)
  xplus <- ifelse(xplus <= 1, 0, 1)
  yplus <- ifelse(yplus <= 1, 0, 1)
  diffx <- abs(sign(diffx))
  diffy <- abs(sign(diffy))
  tplus <- xplus*diffx
  uplus <- yplus*diffy
  tt <- tt + sum(tplus) / 2
  uu <- uu + sum(uplus) / 2
  itot <- sum(signyx * (1- xplus) * (1 - yplus))
  kenS <- itot/2   # Kendall's S for original data
  tau <- (itot)/(n*(n-1))      # Kendall's tau-a for original data
  J <- n * (n - 1) / 2
  taub <- kenS/(sqrt(J - tt) * sqrt(J - uu))
  varS <- n*(n-1)*(2*n+5)/18     # var without tie correction
  ##  adjust varS for ties
  ##  taken from tiesboth.mac by Ed Gilroy
  ##  and modified for censored data by Dennis Helsel.
  ##  extracted from ckend.mac
  ## variance adjusted for ties between censored observations
  intg <- 1:n
  dupx <- xx - delx * cx   # offset censored values by a small amount
  dupy <- yy - dely * cy
  dorder <- order(dupx)
  dxx <- dupx[dorder]
  dcx <- cx[dorder]
  dorder <- order(dupy)
  dyy <- dupy[dorder]
  dcy <- cy[dorder]
  tmpx <- dxx - intg * (1 - dcx) * delx
  tmpy <- dyy - intg * (1 - dcy) * dely
  rxlng <- rle(rank(tmpx))$lengths
  nrxlng <- table(rxlng)
  rxlng <- as.integer(names(nrxlng))
  x1 <- nrxlng * rxlng * (rxlng - 1) * (2 * rxlng + 5)
  x2 <- nrxlng * rxlng * (rxlng - 1) * (rxlng - 2)
  x3 <- nrxlng * rxlng * (rxlng - 1)
  rylng <- rle(rank(tmpy))$lengths
  nrylng <- table(rylng)
  rylng <- as.integer(names(nrylng))
  y1 <- nrylng * rylng * (rylng - 1) * (2 * rylng + 5)
  y2 <- nrylng * rylng * (rylng - 1) * (rylng - 2)
  y3 <- nrylng * rylng * (rylng - 1)
  delc <- (sum(x1) + sum(y1)) / 18 -
    sum(x2) * sum(y2) / (9 * n * (n - 1) * (n - 2)) -
      sum(x3) * sum(y3) / ( 2 * n * (n - 1))
  ## Correction for C-C pairs double counted as UC-C pairs
  ## made to deluc
  x4 <- nrxlng * (rxlng - 1)
  y4 <- nrylng * (rylng - 1)
  ## Variance adjusted for ties in censored and uncensored obs.
  tmpx <- intg * dcx - 1
  tmpx <- ifelse(tmpx < 0, 0, tmpx)
  nrxlng <- sum(tmpx)
  rxlng <- 2
  x1 <- nrxlng * rxlng * (rxlng - 1) * (2 * rxlng + 5)
  x2 <- nrxlng * rxlng * (rxlng - 1) * (rxlng - 2)
  x3 <- nrxlng * rxlng * (rxlng - 1)
  tmpy <- intg * dcy - 1
  tmpy <- ifelse(tmpy < 0, 0, tmpy)
  nrylng <- sum(tmpy)
  rylng <- 2
  y1 <- nrylng * rylng * (rylng - 1) * (2 * rylng + 5)
  y2 <- nrylng * rylng * (rylng - 1) * (rylng - 2)
  y3 <- nrylng * rylng * (rylng - 1)
  deluc <- (sum(x1) + sum(y1)) / 18 -
    sum(x2) * sum(y2) / (9 * n * (n - 1) * (n - 2)) -
      sum(x3) * sum(y3) / ( 2 * n * (n - 1)) - (sum(x4) + sum(y4))
  ## Variance adjustment for ties between uncensored obs.
  dxx <- dxx - intg * dcx * delx
  dyy <- dyy - intg * dcy * dely
  rxlng <- rle(rank(dxx))$lengths
  nrxlng <- table(rxlng)
  rxlng <- as.integer(names(nrxlng))
  x1 <- nrxlng * rxlng * (rxlng - 1) * (2 * rxlng + 5)
  x2 <- nrxlng * rxlng * (rxlng - 1) * (rxlng - 2)
  x3 <- nrxlng * rxlng * (rxlng - 1)
  rylng <- rle(rank(dyy))$lengths
  nrylng <- table(rylng)
  rylng <- as.integer(names(nrylng))
  y1 <- nrylng * rylng * (rylng - 1) * (2 * rylng + 5)
  y2 <- nrylng * rylng * (rylng - 1) * (rylng - 2)
  y3 <- nrylng * rylng * (rylng - 1)
  delu <- (sum(x1) + sum(y1)) / 18 -
    sum(x2) * sum(y2) / (9 * n * (n - 1) * (n - 2)) -
      sum(x3) * sum(y3) / ( 2 * n * (n - 1))
  varS <- varS - delc - deluc - delu
  p.val <- 2 * (1 - pnorm((abs(kenS - sign(kenS)))/sqrt(varS)))
  ## Prepare for ATS slope estimate
  flipx <- max(xx) - xx + 1
  cx <- 1 - cx
  flipy <- max(yy) - yy + 1
  cy <- 1 - cy
  slopes <- unlist(lapply(seq(along=xx), function(i, y, x)
                          ((y[i] - y[1L:i]) / (x[i] - x[1L:i])),
                          yy * cy, xx * cx))   ## substitute zeros for censored values
  bounds <- range(slopes[is.finite(slopes)])
  iter <- 1000L
  result <- .Fortran("ktau",
                     x = as.double(flipx), y = as.double(flipy),
                     icx = as.integer(cx), icy = as.integer(cy),
                     num = as.integer(n), slope = double(1L),
                     lbound = as.double(bounds[1L]),
                     ubound = as.double(bounds[2L]),
                     dev = double(1L), iter = iter)
  sen.slope <- result$slope
  if(result$iter >= iter)
    warning("Slope estimation did not converge")
  ## A line representing the trend of the data then is given by
  ##
  ##    y(t) = sen.slope*(t-med.time)+med.data
  ##
  ##    This line has the slope of the trend and passes through
  ##       the point (t=med.time, y=med.data)
  ## Compute the coefficients for the line
  coef <- c(sen.slope*(-median.x)+median.y, sen.slope)   ## intercept and slope
  ## Return the statistics.
  names(taub) <- "taub"
  zero <- 0
  names(zero) <- "slope"
  est <- c(sen.slope, median.x, median.y)
  names(est) <- c("slope", "median.x", "median.y")
  method <- "Kendall's tau with the ATS slope estimator"
  z <- list(method = method, statistic = taub, p.value = p.val,
            estimate = est, alternative="two.sided", null.value=zero,
            coef=coef, slope.se = result$dev, data.name=data.name)
  oldClass(z) <- "htest"
  return(z)
}
