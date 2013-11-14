#'Class \code{"mcens"}
#'
#'Class "mcens" describes multiply-censored data.
#'
#'
#'@name mcens-class
#'@rdname mcens-class
#'@docType class
#'@section Objects from the Class: Objects can be created by calls of the form
#'\code{as.mcens(lower.val, upper.val, censor.codes)}.
#'@references Lorenz, D.L., in preparation
#'@keywords classes
#'@exportClass mcens
#'@examples
#'
#'showClass("mcens")
#'
setClass("mcens", representation(censor.codes="integer", interval="logical",
                                 names="character"),
         contains="matrix")
