#'Show Methods for \code{WSqw} objects
#'
#'Prints the object.
#'
#'@name show-methods
#'@aliases show-methods show,lcens-method show,mcens-method
#'show,qw-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{list("signature(object = \"lcens\")")}{ Prints the value and detection 
#'limit }
#'
#'\item{list("signature(object = \"mcens\")")}{ Prints the minimum and maximum
#'values }
#'
#'\item{list("signature(object = \"qw\")")}{ Prints the minimum and maximum
#'values, and other attributes of the data } }
#'@keywords methods manip
#'@exportMethod show
setMethod("show",  "lcens", function(object) {
  if(length(object)) {
    vals <- as.character(signif(object@.Data[, 1], 4))
    rmks <- ifelse(object@censor.codes, "<", " ")
    rmks <- ifelse(is.na(rmks), " ", rmks) # Fix NANA in output
    dl <- as.character(signif(object@.Data[, 2], 4))
    xtemp <- cbind(Values=paste(rmks, vals, sep=""), Limits=dl)
    ## make rownames that look like they would work for extraction
    rownames(xtemp) <- paste("[", seq(along=dl), "]", sep="")
    print(xtemp, quote=FALSE)
  } else
    cat("lcens(0)\n")
  invisible(object) }
)

#'@exportMethod show
setMethod("show",  "mcens", function(object) {
  if(length(object)) {
    Lower <- as.character(signif(object@.Data[, 1L], 4))
    Upper <- as.character(signif(object@.Data[, 2L], 4))
    xtemp <- cbind(Lower=Lower, Upper=Upper)
    ## make rownames that look like they would work for extraction
    rownames(xtemp) <- paste("[", seq(along=Lower), "]", sep="")
    print(xtemp, quote=FALSE)
  } else
    cat("mcens(0)\n")
  invisible(object) }
)

#'@exportMethod show
setMethod("show", "qw", function(object) {
  if(length(object)) {
    xval <- object@.Data
    xrmk <- object@remark.codes
    xval[, 1L] <- ifelse(xrmk == "<", 0, xval[, 1L])
    xval <- matrix(as.character(signif(xval, 4L)), ncol=2L) # need to force back to matrix
    xval[, 2L] <- ifelse(xrmk == ">", "+", xval[, 2L])
    xval[, 1L] <- ifelse(xval[, 1L] == xval[, 2L], " ", xval[, 1L]) # make it easy to read
    xval <- format(xval)
    ## Make rownames that look like they would work for extraction
    rownames(xval) <- paste("[", seq(nrow(xval)), "]", sep="")
    colnames(xval) <- c("Lower", "Upper")
    ## Append values codes if not all blank
    xvc <- object@value.codes
    if(!all(xvc %in% c("", " ")))
      xval <- cbind(xval, Codes=xvc)
    xnm <- object@analyte.name
    if(length(unique(xnm)) > 1L)
      xval <- cbind(xval, Analyte=xnm)
    print(xval, quote=FALSE)
  } else
    cat("qw(0)\n")
  invisible(object) }
)
