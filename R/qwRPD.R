#' Relative Differences
#'
#' Computes the relative percent differences (RDP) between samples and replicates for data of
#'class "qw."
#'
#' @param x sample data of class "qw."
#' @param y replicate data of class "qw."
#' @param incomparables logical, \code{NA} means the RPD for any pair that includes
#'left-censored values is \code{NA} (default), \code{FALSE} means the RPD for any pair
#'that includes left-censored values is the minimum possible value, and \code{TRUE} means
#'the RPD for any pair that includes includes left-censored values returns the 
#'range of possible values. See \code{Notes}.
#' @param plotit logical, if \code{TRUE}, then create a Bland-Altman mean-difference plot 
#'(Bland and Altman, 1986); otherwise no plot is created. If \code{incomparables} is set to
#'\code{TRUE}, then the values corresponding to the respective maximums are plotted.
#' @return A vector of the relative percent differences for each paired sample and duplicate.
#'If \code{incomparables} is \code{TRUE}, then the vector is class "mcens," otherwise the vector
#'id numeric.
#' @note This function works differently from other functions that process data of class 
#'"qw"---it ignores the reporting level information and uses the data as they are recorded. 
#'The treatment of interval-censored data varies depending on \code{incomparables}. If
#'\code{incomparables} is set to \code{NA} or \code{FALSE}, then interval-censored data are set to
#'the midrange, otherwise the data are retained as interval-censored data. Right-censored data
#'always return NA regardless of \code{incomparables}.
#'
#'Inverval-censored data are not common and require much additional processing to get
#'accurate ranges for the RPD. Approximate values are returned in this version.
#' @seealso \code{\link[smwrStats]{rpd}}
#' @references
#' Bland J.M. and Altman D.G., 1986 Statistical methods for assessing agreement between two 
#' methods of clinical measurement: Lancet, i, p. 307--310.
#' @importFrom smwrStats rpd
#' @export
qwRPD <- function(x, y, incomparables=NA, plotit=FALSE) {
	# get these first
	xname <- deparse(substitute(x))
	yname <- deparse(substitute(y))
	## Check, then proceed
	if(class(x)[1L] != "qw")
		stop("x is not class 'qw'")
	if(class(y)[1L] != "qw")
		stop("y is not class 'qw'")
	# Initial processing
	xmin <- x@.Data[, 1L]
	xmax <- x@.Data[, 2L]
	ymin <- y@.Data[, 1L]
	ymax <- y@.Data[, 2L]
	xrmk <- x@remark.codes
	yrmk <- y@remark.codes
	if(is.na(incomparables)) { # set to NA
		# repeated in next block
		xIval <- which((xmin < xmax) && !(xrmk %in% c("<", ">")))
		if(length(xIval)) {
			xmed <- (xmin[xIval] + xmax[xIval])/2
			xmin[xIval] <- xmed
			xmax[xIval] <- xmed
		}
		yIval <- which((ymin < ymax) && !(yrmk %in% c("<", ">")))
		if(length(yIval)) {
			ymed <- (ymin[yIval] + ymax[yIval])/2
			ymin[xIval] <- ymed
			ymax[xIval] <- ymed
		}
		# Code for computation, the min dif is based only on the maximum values
		xmax[xrmk %in% c("<", ">")] <- NA
		ymax[yrmk %in% c("<", ">")] <- NA
		retval <- rpd(xmax, ymax, plotit=FALSE)
	} else if(!incomparables) { # set to min
		# repeated from previous block
		xIval <- which((xmin < xmax) && !(xrmk %in% c("<", ">")))
		if(length(xIval)) {
			xmed <- (xmin[xIval] + xmax[xIval])/2
			xmin[xIval] <- xmed
			xmax[xIval] <- xmed
		}
		yIval <- which((ymin < ymax) && !(yrmk %in% c("<", ">")))
		if(length(yIval)) {
			ymed <- (ymin[yIval] + ymax[yIval])/2
			ymin[xIval] <- ymed
			ymax[xIval] <- ymed
		}
		# Code for computation, the min dif is based only on the maximum values
		xmax[xrmk == ">"] <- NA
		ymax[yrmk == ">"] <- NA
		retval <- rpd(xmax, ymax, plotit=FALSE)
	} else { # return intervals
		# Code for computation, the min dif is based only on the maximum values,
		# except when both are left censored, or if one is 
		# left censored and the other is quantified at less than the detection limit,
		# when the minimum difference is zero  (and for interval-censored data, see below)
		xmax[xrmk == ">"] <- NA
		ymax[yrmk == ">"] <- NA
		retmin <- rpd(xmax, ymax, plotit=FALSE)
		retmin[xrmk == "<" & yrmk == "<"] <- 0
		retmin[xrmk == "<" & ymax <= xmax] <- 0
		retmin[yrmk == "<" & xmax <= ymax] <- 0
		# The max dif is based on the smallest minimum and the largest maximum,
		# except when one is interval censored and the other value is within the
		# interval (if anyone wants to fix for interval-censored data they can,
		# as for now, the note in the documentation covers the minor discrepancies)
		maxs <- pmax(xmax, ymax)
		mins <- pmin(xmin, ymin)
		retmax <- rpd(mins, maxs, plotit=FALSE)
		retval <- as.mcens(retmin, retmax)
	}
	if(plotit) {
		# proects agains subsetting, etc.
		xname <- make.names(xname)
		yname <- make.names(yname)
		# create call and list for evaluation
		rpdcall <- paste0("rpd(", xname, ",", yname, ",plotit=TRUE)")
		rpdlist <- list(xmax, ymax)
		names(rpdlist) <- c(xname, yname)
		eval(parse(text=rpdcall), envir=rpdlist)
	}
	return(retval)
}
