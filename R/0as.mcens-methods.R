#'Methods for Function \code{as.mcens}
#'
#'Valid conversion methods for function \code{as.mcens}.
#'
#'
#'@name as.mcens-methods
#'@rdname as.mcens-methods
#'@aliases as.mcens-methods as.mcens,mcens,missing,missing-method 
#'as.mcens,lcens,missing,missing-method
#'as.mcens,numeric,missing,missing-method
#'as.mcens,numeric,numeric,missing-method
#'as.mcens,numeric,missing,numeric-method
#'as.mcens,numeric,missing,character-method
#'as.mcens,qw,missing,missing-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{list("signature(lower.val = \"lcens\", upper.val = \"missing\",
#'censor.codes = \"missing\")")}{ Objects of class "lcens" are converted to
#'"mcens." }
#'
#'\item{list("signature(lower.val = \"mcens\", upper.val = \"missing\",
#'censor.codes = \"missing\")")}{ No change to objects of class "mcens." }
#'
#'\item{list("signature(lower.val = \"numeric\", upper.val = \"missing\",
#'censor.codes = \"character\")")}{ The values in \code{lower.val} are
#'interpreted by \code{censor.codes}: "<", the actual value is less than
#'\code{lower.val}; ">" the actual value is greater than \code{lower.val}; any
#'other value for \code{censor.codes} the value is uncensored. }
#'
#'\item{list("signature(lower.val = \"numeric\", upper.val = \"missing\",
#'censor.codes = \"missing\")")}{ All values in \code{lower.val} are treated as
#'uncensored. }
#'
#'\item{list("signature(lower.val = \"numeric\", upper.val = \"missing\",
#'censor.codes = \"numeric\")")}{ The values in \code{lower.val} are
#'interpreted by \code{censor.codes}: -1, the actual value is less than
#'\code{lower.val}; 1 the actual value is greater than \code{lower.val}; and 0
#'the value is uncensored. }
#'
#'\item{list("signature(lower.val = \"numeric\", upper.val = \"numeric\",
#'censor.codes = \"missing\")")}{ The values in \code{lower.val} are the lowest
#'value and the values in \code{upper.val} are the upper value representing
#'each vlaue. } \item{list("signature(lower.val = \"qw\", upper.val =
#'\"missing\", censor.codes = \"missing\")")}{ Objects of class "qw" are
#'converted to "mcens." } }
#'@keywords methods manip
#'@examples
#'
#'## The first value is left-censored at 2, second is interval,
#'##  third is observed, fourth is right-censored, last is missing
#'as.mcens(c(-Inf,2,3,3, NA), c(2,3,3,NA, NA))
#'
setMethod("as.mcens", signature(lower.val="mcens", upper.val="missing",
                                censor.codes="missing"),
          function(lower.val, upper.val, censor.codes)
          return(lower.val))

setMethod("as.mcens", signature(lower.val="lcens", upper.val="missing",
                                censor.codes="missing"),
function(lower.val, upper.val, censor.codes) {
  ## Remember lower.val is of class lcens, so it looks funny
  upper.val <- lower.val@.Data[, 1L]
  censor.codes <- as.integer(0L - lower.val@censor.codes)
  lower.val <- ifelse(censor.codes == -1L, -Inf, upper.val)
  mat <- cbind(lower.val=lower.val, upper.val=upper.val)
  retval <- new("mcens", mat, censor.codes=censor.codes,
                interval=rep(FALSE, length(lower.val)),
                names="")
  ## This convention must be used to avoid doubling of the length of names
  retval@names <- as.character(seq(nrow(mat)))
  return(retval) } )

setMethod("as.mcens", signature(lower.val="numeric", upper.val="missing",
                                censor.codes="missing"),
function(lower.val, upper.val, censor.codes) {
  mat <- cbind(lower.val=lower.val, upper.val=lower.val)
  retval <- new("mcens", mat,
                censor.codes=as.integer(rep(0L, length(lower.val))),
                interval=rep(FALSE, length(lower.val)),
                names="")
  retval@names <- as.character(seq(nrow(mat)))
  return(retval) } )

setMethod("as.mcens", signature(lower.val="numeric", upper.val="numeric",
                                censor.codes="missing"),
function(lower.val, upper.val, censor.codes) {
  if(length(lower.val) != length(upper.val))
    stop("lengths of lower.val and upper.val must match")
  ## convert NA to Proper Inf value, preserving both missing
  NAs <- is.na(lower.val) & is.na(upper.val)
  lower.val <- na2miss(lower.val, -Inf)
  upper.val <- na2miss(upper.val, Inf)
  if(any(lower.val > upper.val))
    stop("All lower.val must be less than or equal to upper.val")
  censor.codes <- ifelse(lower.val == -Inf, -1L,
                         ifelse(upper.val == Inf, 1L, 0L))
  interval <- censor.codes == 0L & upper.val > lower.val
  lower.val[NAs] <- NA
  upper.val[NAs] <- NA
  censor.codes[NAs] <- NA
  interval[NAs] <- NA
  mat <- cbind(lower.val=lower.val, upper.val=upper.val)
  retval <- new("mcens", mat, censor.codes=as.integer(censor.codes),
                interval=interval,
                names="")
  retval@names <- as.character(seq(nrow(mat)))
  return(retval) } )

setMethod("as.mcens", signature(lower.val="numeric", upper.val="missing",
                                censor.codes="numeric"),
function(lower.val, upper.val, censor.codes) {
  if(length(lower.val) != length(censor.codes))
    stop("lengths of lower.val and censor.codes must match")
  upper.val <- lower.val
  censor.codes <- as.integer(sign(censor.codes))
  lower.val[censor.codes == -1L] <- -Inf
  upper.val[censor.codes == 1L] <- Inf
  mat <- cbind(lower.val=lower.val, upper.val=upper.val)
  retval <- new("mcens", mat, censor.codes=censor.codes,
             interval=rep(FALSE, length(lower.val)),
                names="")
  retval@names <- as.character(seq(nrow(mat)))
  return(retval) } )

setMethod("as.mcens", signature(lower.val="numeric", upper.val="missing",
                                censor.codes="character"),
function(lower.val, upper.val, censor.codes) {
  if(length(lower.val) != length(censor.codes))
    stop("lengths of lower.val and censor.codes must match")
  cc <- unique(censor.codes)
  bad <- which(!(cc %in% c("<", ">", "", " ")))
  if(any(bad))
    warning(paste("censor.codes ignored:", cc[bad], collapse=" "))
  ## Done checking, get on with it
  upper.val <- lower.val
  censor.codes <- ifelse(censor.codes == "<", -1L,
                         ifelse(censor.codes == ">", 1L, 0L))
  lower.val[censor.codes == -1L] <- -Inf
  upper.val[censor.codes == 1L] <- Inf
  mat <- cbind(lower.val=lower.val, upper.val=upper.val)
  retval <- new("mcens", mat, censor.codes=censor.codes,
                interval=rep(FALSE, length(lower.val)),
                names="")
  retval@names <- as.character(seq(nrow(mat)))
  return(retval) } )

setMethod("as.mcens", signature(lower.val="qw", upper.val="missing",
                                censor.codes="missing"),
function(lower.val, upper.val, censor.codes) {
  ## Make copy so that lower.val, etc have ususal meanings
  temp <- lower.val 
  lower.val <- temp@.Data[, 1L]
  upper.val <- temp@.Data[, 2L]
  Cc <- temp@remark.codes
  Dl <- temp@reporting.level
  ## Fix less-thans
  Pick <- na2miss(Dl > upper.val, FALSE)
  upper.val[Pick] <- Dl[Pick]
  lower.val[Pick | Cc == "<"] <- -Inf
  ## Fix greater-thans
  upper.val[Cc == ">"] <- Inf
  ## Compute the censor.codes and interval
  censor.codes <- match(Cc, c("<", " ", ">"), nomatch=2L) - 2L
  interval <- Cc == "I"
  mat <- cbind(lower.val=lower.val, upper.val=upper.val)
  retval <- new("mcens", mat, censor.codes=as.integer(censor.codes),
                interval=interval,
                names="")
  retval@names <- as.character(seq(nrow(mat)))
  return(retval) } )
