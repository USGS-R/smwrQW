#' Print Values
#'
#' Print representations of censored or water-quality data values.
#'
#' @aliases print.lcens print.mcens print.qw 
#' @param x the object to print.
#' @param digits the number of significant digits for numbers.
#' @param \dots further arguments passed to or from other methods.
#' @return The object \code{x} is returned invisibly.
#' @keywords print
#' @rdname printData
#' @S3method print lcens
#' @method print lcens
print.lcens <- function(x, digits=4, ...) {
  xtemp <- x
  xtemp@.Data <- signif(xtemp@.Data, digits)
  print(as.character(xtemp), quote=FALSE)
  invisible(x)
}

#' @rdname printData
#' @S3method print mcens
#' @method print mcens
print.mcens <- function(x, digits=4, ...) {
  xtemp <- x
  xtemp@.Data <- signif(xtemp@.Data, digits)
  print(as.character(xtemp), quote=FALSE)
  invisible(x)
}

#' @rdname printData
#' @S3method print qw
#' @method print qw
print.qw <- function(x, ...) {
  retval <- as.character(x)
  print(retval, quote=FALSE)
  invisible(x)
}
