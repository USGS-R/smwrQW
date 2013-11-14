#'Class \code{"lcens"}
#'
#'Class "lcens" describes left-censored data.
#'
#'
#'@name lcens-class
#'@rdname lcens-class
#'@docType class
#'@section Objects from the Class: Objects can be created by calls of the form
#'\code{as.lcens(values, detlim, censor.codes)}.
#'@references Lorenz, D.L., in preparation
#'@keywords classes
#'@examples
#'
#'showClass("lcens")
#'
#'@exportClass lcens
setClass("lcens", representation(censor.codes="logical", names="character"),
         contains="matrix")
