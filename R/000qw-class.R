#'Class \code{"qw"}
#'
#'Class "qw" describes water-quality data.
#'
#'
#'@name qw-class
#'@rdname qw-class
#'@docType class
#'@section Objects from the Class: Objects can be created by calls of the form
#'\code{as.qw(values, remark.codes, value.codes, reporting.level,
#'reporting.method, reporting.units, analyte.method,
#'analyte.name, unique.code, value2)}.
#'@references Lorenz, D.L., in preparation
#'@keywords classes
#'@exportClass qw
#'@examples
#'
#'showClass("qw")
#'
setClass("qw", representation(remark.codes="character", value.codes="character",
                              reporting.level="numeric", reporting.method="character",
                              reporting.units="character", analyte.method="character",
                              analyte.name="character", rounding="numeric",
                              unique.code="character", names="character"),
         contains="matrix")
