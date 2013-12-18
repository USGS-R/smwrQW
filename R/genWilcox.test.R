#' Test for difference in left-censored samples
#'
#' This function tests for differences in left-censored samples from 2
#'groups.
#'
#' If \code{y} is either type character or factor, then it is assumed to be a group
#'identifier. Anything else is treated as another set of sample and forced to class
#'"lcens."
#'
#' @param x the samples from each group. Forced to class "lcens." Missing values
#'are removed before the analysis.
#' @param y either another set of samples or a group identifier with exactly
#'two groups. Missing values are removed before the analysis. See \bold{Details}.
#' @param alternative character string describing the alternative hypothesis.
#'Must be one of "two.sided, "greater," or "less."
#' @param data.names character string to be used to explain the data. Default
#'names are derived from the data arguments.
#' @return An object of class "htest" that inherits "genWilcox."
#' @note The \code{genWilcox.test} 
#'function uses the \code{survfit} function. Helsel (2012) describes flipping
#'the left-censored data so that small values become large and left-censored
#'values become right-censored values and adapt nonparametric techniques from
#'survival analysis. The results from \code{survfit} are printed as the sample
#'estimates when printing the output, the important columns are records,
#'the numner in each group; events, the number of uncensored values; and 
#'median, the group median.\cr
#'A \code{plot} method is supported for the returned object.
#'
#' @section Null Hypothesis: The null hypothesis is that the
#'distributions are not different from one another.
#' @seealso \code{\link{survdiff}}, \code{\link{survfit}},
#'\code{\link{lcens-class}}
#' @references Gehan, E.A., 1965, A generalized Wilcoxon test for comparing
#'arbitraritly singly censored samples: Biometrika, v. 52, p. 203-223.\cr
#'
#'Harrington, D.P., and Fleming, T.R., 1982, A class of rank test procedures
#'for censored survival data: Biometrika, v. 69, p. 553-566.\cr
#'
#'Helsel, D.R. 2012, Statistics for Censored Environmental Data Using Minitab
#'and R: New York, Wiley, 324 p.\cr
#'
#'Peto, R., and Peto, J., 1972, Asymptotically efficient rank invariant test
#'procedures (with discussion): Journal of the Royal Statistical Society,
#'Series A v. 135, p. 185-206.\cr
#'
#'Prentice, R.L. 1978, Linear rank tests with right-censored data: Biometika, v
#'65, p 167-179.\cr
#'
#'Prentice, R.L., and Marke, P., 1979, A qualitative discrepancy between
#'censored data rank tests: Biometrika, v. 35, p. 861-867.\cr
#'
#' @keywords censored htest
#' @examples
#'
#'# Compare uncensored results
#'# First for grouped data
#'set.seed(69)
#'Xu <- rlnorm(22, 0, 1)
#'Yu <- rlnorm(22, .6, 1)
#'genWilcox.test(Xu, Yu)
#'wilcox.test(Xu, Yu)
#'# Compare effect of censoring
#'genWilcox.test(as.lcens(Xu, 1), Yu)
#'genWilcox.test(as.lcens(Xu, 1), as.lcens(Yu, 1))
#'
#' @export
genWilcox.test <- function(x, y, alternative="two.sided", data.names) {
  ## Coding history:
  ##    2005Mar08 DLLorenz Initial Coding.
  ##    2005Jul14 DLLorenz Fixed date
  ##    2007Jan29 DLLorenz Modified to fail on infoRich coding
  ##    2012Aug13 DLLorenz Conversion to R and split ppw test
  ##    2013Jan01 DLLorenz Roxygenized
  ##    2013Sep03 DLLorenz Bug Fix for missing values
	##    2013Dec06 DLLorenz Added computations of estimates (median)
  ##
  ## Error checks:
  if(missing(data.names)) {
    if(class(y) == "factor") {
      data.names <- levels(y)
      data.names <- paste(data.names[1L], "and", data.names[2L], sep=" ")
      num.y <- FALSE
    } else if(class(y) == "character") {
      data.names <- unique(y)
      data.names <- paste(data.names[1L], "and", data.names[2L], sep=" ")
      num.y <- FALSE
    } else { # get names from x and y
    data.names <- paste(deparse(substitute(x)), "and",
                        deparse(substitute(y)), sep=" ")
    num.y <- TRUE
    }
  }
  x <- as.lcens(x)
  ## Create stacked data and group column
  if(num.y) {
    y <- as.lcens(y)
    ## Remove missings
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
    group = factor(c(rep('x', length(x)), rep('y', length(y))), levels=c('x', 'y'))
    data <- c(x, y)
  }
  else { # Use y as group
    ## Remove missings
    keep <- !(is.na(x) | is.na(y))
    x <- x[keep]
    y <- y[keep]
    data <- x
    group <- as.factor(y) # make it a factor
    if(length(levels(group)) != 2L)
      stop("y must have exactly two levels")
  }
  alternative <- pmatch(alternative, c('two.sided', 'greater', 'less'))
  if(is.na(alternative))
    stop("Invalid choice for alternative, must match 'two.sided', 'greater', or 'less'")
  ## define the tests:
  PetoPrentice.test <- function(values, cenflag, group) {
    ## Perform the Kaplan-Meier analysis
    ## Note that the value is not important, and ranking preserves the link
    values <- rank(values)
    ## Required adjustment to values
    values <- ifelse(cenflag, values-.1, values)
    df <- data.frame(values=-values, cenflag=!cenflag)
    kmout <- survfit(Surv(values, cenflag) ~ 1, data=df, conf.type="none")
    kmout$time <- - kmout$time # convert back to actual ranks
    ## Define the link between the observed data and the K-M table
    ## and compute needed stats.
    link <- match(values, kmout$time)
    St <- kmout$surv[link]
    Stm1 <- c(1,kmout$surv) # dummy to create valid value for t=1
    Stm1 <- Stm1[link] # now associated with correct value
    U <- ifelse(cenflag, Stm1 - 1, St + Stm1 - 1)
    nums <- tapply(U, group, length)
    retval <- list(W=tapply(U, group, sum),
                   VarW=sum(U^2)*nums[1L]*nums[2L]/length(U)/(length(U)-1),
                   Ngroup=nums)
    ## retval is the computed W and the variance of W.
    return(retval)

  } # End of P-P.test
  ret1 <- PetoPrentice.test(data@.Data[,1L], data@censor.codes, group)
  stat <- ret1$W[1L] / sqrt(ret1$VarW)
  names(stat) <- "Peto-Prentice Z"
  meth <- "Peto-Prentice generalized Wilcoxon test"
  param <- ret1$Ngroup
  names(param) <- c("n", "m")
  ## Add finishing touches
  if(alternative == 1L)
    pvalue <- (1. - pnorm(abs(stat))) * 2.
  else if(alternative == 2)
    pvalue <- 1. - pnorm(stat)
  else # alternative is less than
    pvalue <- pnorm(stat)
  mu <- 0
  names(mu) <- "difference"
  ## Compute the median for each group
  ret2 <- survfit(Surv(-data@.Data[,1L], !data@censor.codes) ~ group, conf.type="none")
  ret2$time <- - ret2$time # convert back to actual values
  ret2$call <- NULL # remove unecessary info
  retval <- list(statistic = stat, parameters = param,
                 p.value = pvalue, null.value = mu,
                 alternative = c('two.sided', 'greater', 'less')[alternative],
                 method = meth, data.name = data.names,
  							 estimate=ret2)
  oldClass(retval) <- c("htest", "genWilcox")
  return(retval)
}
