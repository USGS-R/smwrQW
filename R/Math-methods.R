#'Mathematical Transform Methods for \code{lcens} objects
#'
#'Some limited mathematical methods are possible and well-defined for
#'censored or water-quality data.
#'
#'
#'@name Math-methods
#'@aliases Math-methods Math,lcens-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{list("signature(x = \"lcens\")")}{ The functions \code{log},
#'\code{log10}, \code{exp}, and \code{sqrt} are defined for objects of class
#'"lcens." } }
#'@keywords methods
#'@exportMethod  Math
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
