#'Methods for Function \code{as.lcens}
#'
#'Valid conversion methods for function \code{as.lcens}.
#'
#'
#'@name as.lcens-methods
#'@rdname as.lcens-methods
#'@aliases as.lcens-methods as.lcens,lcens,missing,missing-method
#'as.lcens,numeric,missing,character-method
#'as.lcens,numeric,missing,logical-method
#'as.lcens,numeric,missing,missing-method
#'as.lcens,numeric,numeric,character-method
#'as.lcens,numeric,numeric,logical-method
#'as.lcens,numeric,numeric,missing-method 
#'as.lcens,qw,missing,missing-method
#'as.lcens,qw,numeric,missing-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{list("signature(values = \"lcens\", detlim = \"missing\", censor.codes
#'= \"missing\")")}{ No change to the "lcens" object. }
#'
#'\item{list("signature(values = \"numeric\", detlim = \"missing\",
#'censor.codes = \"character\")")}{ Valid \code{censor.codes} are "<" for
#'left-censored values, and " " for uncensored values. All others are ignored.
#'The detection limits are infered from the values for censored values. }
#'
#'\item{list("signature(values = \"numeric\", detlim = \"missing\",
#'censor.codes = \"logical\")")}{ A \code{censor.codes} of \code{TRUE}
#'indicates a left-censored value, \code{FALSE} indicates uncensored. The
#'detection limits are infered from the values for censored values. }
#'
#'\item{list("signature(values = \"numeric\", detlim = \"missing\",
#'censor.codes = \"missing\")")}{ No censoring is possible. }
#'
#'\item{list("signature(values = \"numeric\", detlim = \"numeric\",
#'censor.codes = \"character\")")}{ Any \code{values} less than \code{detlim}
#'is left-censored. }
#'
#'\item{list("signature(values = \"numeric\", detlim = \"numeric\",
#'censor.codes = \"logical\")")}{ Any \code{values} less than \code{detlim} is
#'left-censored, ties resolved by \code{censor.codes}. }
#'
#'\item{list("signature(values = \"numeric\", detlim = \"numeric\",
#'censor.codes = \"missing\")")}{ Any \code{values} less than \code{detlim} is
#'left-censored, ties resolved by \code{censor.codes}. }
#'
#'\item{list("signature(values = \"qw\", detlim = \"missing\", censor.codes =
#'\"missing\")")}{ The object of class "qw" is convereted if it contains only
#'uncensored or left-censored data. }
#'
#'\item{list("signature(values = \"qw\", detlim = \"numeric\", censor.codes =
#'\"missing\")")}{ The object of class "qw" is convereted if it contains only
#'uncensored or left-censored data and force a minimum detection limit. }  }
#'@keywords methods manip
#'@examples
#'
#'## The first value is censored at 2, the second is not
#'as.lcens(c(1,3), 2)
#'
setMethod("as.lcens", signature(values="lcens", detlim="missing",
                                censor.codes="missing"),
          function(values, detlim, censor.codes)
          return(values))

setMethod("as.lcens", signature(values="numeric", detlim="missing",
                                censor.codes="missing"),
function(values, detlim, censor.codes) {
  ## detlim is missing, so set as less than the minimum values (no censored values)
  if(all(is.na(values))) # Need to protext against a single NA or NA vector
    detlim <- 0
  else
    detlim <- min(values, na.rm=TRUE)
  detlim <- floor(detlim - 100)
  detlim <- rep(detlim, length(values))
  mat <- cbind(values=values, detlim=detlim)
  retval <- new("lcens", mat, censor.codes=values < detlim,
                names="")
  ## This convention must be used to avoid doubling of the length of names
  retval@names <- as.character(seq(nrow(mat)))
  return(retval) })

setMethod("as.lcens", signature(values="numeric", detlim="numeric", censor.codes="missing"),
function(values, detlim, censor.codes) {
  detlim <- rep(detlim, length.out=length(values))
  ## easiest one to make from scratch
  mat <- cbind(values=pmax(values, detlim), detlim=detlim)
  retval <- new("lcens", mat, censor.codes=values < detlim,
                names="")
  retval@names <- as.character(seq(nrow(mat)))
  return(retval) })

setMethod("as.lcens", signature(values="numeric", detlim="numeric",
                                censor.codes="logical"),
function(values, detlim, censor.codes) {
  ## There are 4 possible conditions between values and detlim:
  ##  value detlim result         comment
  ##    V     DL     max(V, DL)   censored if DL > V, ties resolved by censor.codes
  ##   NA     NA     NA
  ##   NA     DL     DL           left-censored at DL
  ##    V     NA     V            detlim left as NA and warning issued
  ## There is 1 additional possibility to protect against:
  ##  a value reported as <V where V is in elevated RL, but detlim is the
  ##  method reporting limit
  detlim <- rep(detlim, length.out=length(values)) ## Rare, but possible
  censored <- values < detlim
  vals <- pmax(values, detlim)
  ## Check for 3rd possibility (modern databases often store censored values
  ##  as NA and rely on some kind of censoring indicator and the detection/
  ##  reporting limit)
  if(any(sel <- (is.na(values) & !is.na(detlim) & !is.na(censor.codes)))) {
    censored[sel] <- TRUE
    vals[sel] <- detlim[sel]
  }
  ## check for the 4th possibility No detection/reporting limit
  if(any(sel <- (!is.na(values) & is.na(detlim)))) {
    censored[sel] <- FALSE
    vals[sel] <- values[sel]
    warning("values with missing detlim--retained as uncensored value")
  }
  ## This fixes ties and additional issues
  good <- which(censor.codes) # Protect against NAs
  censored[good] <- TRUE
  detlim[good] <- pmax(vals, detlim)[good]
  mat <- cbind(values=vals, detlim=detlim)
  retval <- new("lcens", mat, censor.codes=censored,
                names="")
  retval@names <- as.character(seq(nrow(mat)))
  return(retval) })

setMethod("as.lcens", signature(values="numeric", detlim="numeric",
                                censor.codes="character"),
function(values, detlim, censor.codes) {
  ## There are 4 possible conditions between values and detlim:
  ##  see method of censor.codes = "logical"
  ## Check for < and convert < to T
  cc <- unique(censor.codes)
  bad <- which(!(cc %in% c("<", "", " ")))
  if(any(bad))
    warning(paste("censor.codes ignored:", cc[bad], collapse=" "))
  censor.codes <- censor.codes == "<"
  detlim <- rep(detlim, length.out=length(values))
  censored <- values < detlim
  vals <- pmax(values, detlim)
  ## Check for 3rd possibility
  if(any(sel <- (is.na(values) & !is.na(detlim) & !is.na(censor.codes)))) {
    censored[sel] <- TRUE
    vals[sel] <- detlim[sel]
  }
  ## Check for the 4th possibility No detection/reporting limit
  if(any(sel <- (!is.na(values) & is.na(detlim)))) {
    censored[sel] <- FALSE
    vals[sel] <- values[sel]
    warning("values with missing detlim--retained as uncensored value")
  }
  ## This fixes ties and additional issues
  good <- which(censor.codes) # Protect against NAs
  censored[good] <- TRUE
  detlim[good] <- pmax(vals, detlim)[good]
  mat <- cbind(values=vals, detlim=detlim)
  retval <- new("lcens", mat, censor.codes=censored,
                names="")
  retval@names <- as.character(seq(nrow(mat)))
  return(retval) } )

setMethod("as.lcens", signature(values="numeric", detlim="missing",
                                censor.codes="logical"),
function(values, detlim, censor.codes) {
  detlim <- dlimit(values, censor.codes)
  mat <- cbind(values=values, detlim=detlim)
  retval <- new("lcens", mat, censor.codes=censor.codes,
                names="")
  retval@names <- as.character(seq(nrow(mat)))
  return(retval) } )

setMethod("as.lcens", signature(values="numeric", detlim="missing",
                                censor.codes="character"),
function(values, detlim, censor.codes) {
  ##
  detlim <- dlimit(values, censor.codes)
  mat <- cbind(values=values, detlim=detlim)
  retval <- new("lcens", mat, censor.codes=censor.codes=="<",
                names="")
  retval@names <- as.character(seq(nrow(mat)))
  return(retval) } )

setMethod("as.lcens", signature(values="qw", detlim="missing",
                                censor.codes="missing"),
function(values, detlim, censor.codes) {
  ## Convert qw object to lcens if possible
  if(censoring(values) == "multiple")
    stop("The qw object does not contain only left-censored data")
  Val <- values@.Data[, 2L]
  Dl <- values@reporting.level
  Cen <- values@remark.codes == "<"
  ## Determine if any Dls need to be estimated
  picks <- !is.na(Val) & is.na(Dl)
  if(any(picks)) {
    if(any(Cen))
      def <-  min(Val[picks])
    else
      def <- 1.e-25
    Guess <- dlimit(Val[picks], Cen[picks], def)
    Dl[picks] <- Guess
  }
  ## Censor Val < Dl
  Cen <- Cen | (Val < Dl)
  Val <- pmax(Val, Dl)
  mat <- cbind(values=Val, detlim=Dl)
  retval <- new("lcens", mat, censor.codes=Cen,
                names="")
  retval@names <- as.character(seq(nrow(mat)))
  return(retval) } )

setMethod("as.lcens", signature(values="qw", detlim="numeric",
                                censor.codes="missing"),
function(values, detlim, censor.codes) {
  ## Convert qw object to lcens if possible
  if(censoring(values) == "multiple")
    stop("The qw object does not contain only left-censored data")
  Val <- values@.Data[, 2L]
  ## Force censoring at minimum DL
  Dl <- pmax(values@reporting.level, detlim)
  Cen <- values@remark.codes == "<" | na2miss(Val, detlim + 2) < detlim
  Val <- pmax(Val, detlim)
  ## Determine if any Dls need to be estimated
  picks <- !is.na(Val) & is.na(Dl)
  if(any(picks)) {
    if(any(Cen))
      def <-  min(Val[picks])
    else
      def <- 1.e-25
    Guess <- dlimit(Val[picks], Cen[picks], def)
    Dl[picks] <- Guess
  }
  ## Censor Val < Dl
  Cen <- Cen | (Val < Dl)
  Val <- pmax(Val, Dl)
  mat <- cbind(values=Val, detlim=Dl)
  retval <- new("lcens", mat, censor.codes=Cen,
                names="")
  retval@names <- as.character(seq(nrow(mat)))
  return(retval) } )
