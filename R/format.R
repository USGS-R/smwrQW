#'Encode in a Common Format
#'
#'Format a censored or water-quality data object for pretty printing.
#'
#'
#' @aliases format.lcens format.mcens format.qw
#' @param x the censored data object to format.
#' @param digits how many significant digits are to be used for numbers?
#' @param \dots further arguments passed to or from other methods.
#' @param units include the units of the data?
#' @param round round the data before formating? Can also be a numeric vector of
#'length 2 containing the maximum number of significant digits and the maximum
#'number of decimal digits.
#' @return A character representations of the elements of \code{x}.
#' @keywords manip
#' @examples
#'
#'format(as.lcens(1:3, 1))
#'format(as.mcens(1:3, 1:3))
#'
#' @rdname format
#' @export
#' @method format lcens
format.lcens <- function(x, digits=NULL, ...) {
  if(!is.null(digits))
    x@.Data <- signif(x@.Data, digits)
  retval <- as.character(x)
  return(format(retval, ...))
}

#' @rdname format
#' @export
#' @method format mcens
format.mcens <- function(x, digits=NULL, ...) {
  if(!is.null(digits))
    x@.Data <- signif(x@.Data, digits)
  retval <- as.character(x)
  return(format(retval, ...))
}

#' @rdname format
#' @export
#' @method format qw
format.qw <- function(x, round=TRUE, units=FALSE, ...) {
  xval <- x@.Data
  xrl <- x@reporting.level
  xrmk <- x@remark.codes
  xrmk[is.na(xrmk)] <- " " # fix NAs in the remark codes
  ## Force detection limit on left-censored data
  fdl <- ifelse(is.na(xrl), FALSE,
                     ifelse(xval[, 2L] >= xrl, FALSE, TRUE))
  fdl <- na2miss(fdl, FALSE)
  if(any(fdl)) {
    xval[fdl, 2L] <- xrl[fdl]
    xrmk[fdl] <- "<"
  }
  if(is.logical(round)) {
    if(round) {
      rnd <- x@rounding
      xval <- round(signif(xval, rnd[1L]), rnd[2L])
    }
  }
  else if(length(round) == 2L) { # Standard qw usage
    xval <- round(signif(xval, round[1L]), round[2L])
  }
  else {
    xval <- round(xval, round)
  }
  xval <- format(xval, ...) # xval is now character
  ## Interval censoring
  xsho <- ifelse(xrmk == "I", xval[, 2L], xval[, 1L])
  xrmk <- ifelse(xrmk == "I", paste(xval[, 1L], "-", sep=''), xrmk)
  ## Left censoring
  xsho <- ifelse(xrmk == "<", xval[, 2L], xsho)
  retval <- paste(xrmk, xsho, sep='')
  if(units)
    retval <- paste(retval, x@reporting.units, sep=' ')
  return(retval)
}
