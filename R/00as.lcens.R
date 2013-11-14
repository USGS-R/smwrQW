#'Left-censored Data Conversion
#'
#'Convert from various formats to a left-censored data (\code{lcens}) object.
#'
#'@name as.lcens
#'@rdname as.lcens
#'@param values numeric values representing "raw" values from a sensor.
#'@param detlim the corresponding detection limit of the sensor.
#'@param censor.codes optional codes indicating a left-censored value. If class
#'"logical," then \code{TRUE} indicates left-censored. If class "character,"
#'then "<" indicates left-censored and anything other than "" or " " generates
#'a warning.
#'@return An S4 object of class "lcens" having these slots:
#'\item{.Data}{ a matrix of the the censored values and the detection
#'limits.}
#'\item{censor.codes}{ logical value indicating a left-censored value.}
#'@note All methods force \code{values} to be no greater than the corresponding
#'\code{detlim}. This ensures unbiased analytical results.
#'@exportMethod as.lcens
#'@keywords manip
setGeneric("as.lcens", function(values, detlim, censor.codes) standardGeneric("as.lcens")
           ## Coding history:
           ##    2012Mar01 DLLorenz Conversion to R/new style
           ##    2012Mar22 DLLorenz Conversion to S4
           ##    2012Jul24 DLLorenz Modified dfns to produce typeof = "numeric"
           ##    2012Aug03 DLLorenz Added detlim="missing" signatures to as.lcens.
           ##    2012Sep17 DLLorenz Added values="qw" signature to as.lcens
           ##    2013Jan13 DLLorenz Roxygenized
           ##    2013Jan13          This version
)
