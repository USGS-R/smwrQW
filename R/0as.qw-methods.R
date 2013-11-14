#'Methods for Function \code{as.qw}
#'
#'Valid conversion methods for function \code{as.qw}.
#'
#'
#'@name as.qw-methods
#'@rdname as.qw-methods
#'@aliases as.qw-methods
#'as.qw,numeric,character,character,numeric,character,character,character,character,character,missing-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{list("signature(values = \"numeric\", remark.codes = \"character\",
#'value.codes = \"character\", reporting.level = \"numeric\",
#'reporting.method = \"character\", reporting.units = \"character\", 
#'analyte.method = \"character\", analyte.name = \"character\", unique.code =
#'\"character\", value2 = \"missing\")")}{ All of the data and meta data
#'are required for objects of class "qw."}}
#'@keywords methods manip
setMethod("as.qw", signature(values="numeric", remark.codes="character",
          value.codes="character",
          reporting.level="numeric", reporting.method="character",
          reporting.units="character", analyte.method="character",
          analyte.name="character", unique.code="character", value2="missing"),
function(values, remark.codes, value.codes, reporting.level, reporting.method,
         reporting.units,analyte.method, analyte.name, unique.code, value2) {
  ## Length of remark.codes must match length of values
  N <- length(values)
  if(length(remark.codes) != N)
    stop("lengths of values and remark.codes must match")
  ## In practical terms, it is conceivable that single values could be
  ##  supplied by the rest
  value.codes <- rep(value.codes, length.out=N)
  reporting.level <- rep(reporting.level, length.out=N)
  reporting.method <- rep(reporting.method, length.out=N)
  reporting.units <- rep(reporting.units, length.out=N)
  analyte.method <- rep(analyte.method, length.out=N)
  analyte.name <- rep(analyte.name, length.out=N)
  unique.code <- rep(unique.code, length.out=N)
  value2 <- values
  ## Logic check on remarks--only "<", ">", and " " make sense
  remark.codes[is.na(remark.codes)] <- " "
  remark.codes[remark.codes == ""] <- " "
  remarks.uniq <- unique(remark.codes)
  remarks.ignore <- !(remarks.uniq %in% c("<", ">", " "))
  if(any(remarks.ignore))
    warning("Special remark.codes: ", paste(remarks.uniq[remarks.ignore], collapse=', '),
            " retained, but may require interpretation by the user.")
  ## Modify value2 for > and values for <
  value2[remark.codes == ">"] <- Inf
  values[remark.codes == "<"] <- 0
  ## Change lower limit if logged--will this ever happen?
  LogVal <- grepl("^log\\(", reporting.units)
  if(any(LogVal))
    values[remark.codes == "<" & LogVal] <- 0
  ## Pack it up and ship it off
  mat <- cbind(values=values, value2=value2)
  retval <- new("qw", mat, remark.codes=remark.codes,
                value.codes=value.codes,
                reporting.level=reporting.level,
                reporting.method=reporting.method,
                reporting.units=reporting.units,
                analyte.method=analyte.method, analyte.name=analyte.name,
                unique.code=unique.code, rounding=c(2L,3L),
                names=as.character(seq(N)))
  return(retval)
})
