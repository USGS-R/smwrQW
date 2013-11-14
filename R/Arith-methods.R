#'Arithmetic Methods for \code{lcens} objects
#'
#'Some limited arithemtic methods are possible and well-defined for
#'left-censored data. 
#'
#'@export
#'@name Arith-methods
#'@aliases Arith-methods Arith,lcens,numeric-method Arith,numeric,lcens-method
#'Arith,lcens,lcens-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{list("signature(e1 = \"lcens\", e2 = \"numeric\")")}{ Multiplication
#'and division (positive values only) and addition and subtraction are defined.
#'}
#'
#'\item{list("signature(e1 = \"numeric\", e2 = \"lcens\")")}{ Multiplication
#'(positive values only) and addition are defined for this combination. }
#'
#'\item{list("signature(e1 = \"lcens\", e2 = \"lcens\")")}{ Addition
#'only is defined for this combination. } }
#'@keywords methods manip
setMethod("Arith", signature(e1="lcens", e2="numeric"), function(e1, e2) {
  if(.Generic %in% c("%%", "%/%", "^"))
    stop(gettextf("'%s' not defined for 'lcens' objects", .Generic),
              domain=NA)
  if(.Generic %in% c("*", "/") && any(e2 <= 0))
    stop(gettextf("'%s' only valid for positive values", .Generic),
              domain=NA)
  e1@.Data[, 1] = callGeneric(e1@.Data[, 1], e2)
  e1@.Data[, 2] = callGeneric(e1@.Data[, 2], e2)
  e1}
)
  
#'@export
setMethod("Arith", signature(e1="numeric", e2="lcens"), function(e1, e2) {
  if(.Generic %in% c("%%", "%/%", "^", "-", "/"))
    stop(gettextf("'%s' not defined for 'lcens' objects", .Generic),
              domain=NA)
  if(.Generic %in% c("*") && any(e1 <= 0))
    stop(gettextf("'%s' only valid for positive values", .Generic),
              domain=NA)
  e2@.Data[, 1] = callGeneric(e1, e2@.Data[, 1])
  e2@.Data[, 2] = callGeneric(e1, e2@.Data[, 2])
  e2}
)

#'@export
setMethod("Arith", signature(e1="lcens", e2="lcens"), function(e1, e2) {
  if(.Generic == "+") {
    e1@censor.codes <- e1@censor.codes | e2@censor.codes
    e1@.Data[, 1] = e1@.Data[, 1] + e2@.Data[, 1]
    e1@.Data[, 2] = pmax(e1@.Data[, 2] + e2@.Data[, 2],
              e1@.Data[, 2], e2@.Data[, 2], ifelse(e1@censor.codes,
                                                   e1@.Data[, 1], -101))
    return(e1)
  }
  else
    stop(gettextf("'%s' not defined for 'lcens' objects", .Generic),
         domain=NA)
})
