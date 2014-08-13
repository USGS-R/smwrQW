#' @title Mathematical Transform Methods for \code{lcens} objects
#'
#' @description Some limited mathematical methods are possible and well-defined for
#'censored or water-quality data.
#'
#'
#' @name Math-censored
#' @include mcens-class.R lcens-class.R qw-class.R
#' @param x the object to transform
#' @keywords methods manip
#' @exportMethod  Math
#' @examples
#'log(as.lcens(c(1, 3), 2))
#' 

#' @rdname Math-censored
#' @aliases Math,lcens-method
setMethod("Math", "lcens", function(x) {
  if(length(x) == 0L)
    return(x) # Do nothing
  switch(.Generic,
         log={lt0 <- which(x@.Data <= 0)
              tmp <- log(pmax(x@.Data, 0))
              tmp[lt0] <- -100
              x@.Data <- tmp
              x},
         log10={lt0 <- which(x@.Data <= 0)
              tmp <- log10(pmax(x@.Data, 0))
              tmp[lt0] <- -100
              x@.Data <- tmp
                x},
         sqrt={lt0 <- which(x@.Data < 0)
               tmp <- log10(pmax(x@.Data, 0))
               tmp[lt0] <- -100
               x@.Data <- tmp
               x},
         exp={x@.Data <- exp(x@.Data)
              x},
         stop(gettextf("'%s' not defined for lcens objects", .Generic),
              domain=NA))
  }
)
