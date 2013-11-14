#'Water-quality Data Conversion
#'
#'Convert the data and meta data to a water-quality data (\code{mcens}) object.
#'
#'@name as.qw
#'@rdname as.qw
#'@param values the uncensored values and optionally the censored values.
#'@param remark.codes the remarks codes: " " or "" indicated uncensored, "<"
#'indicates left-censoring, ">" indicates right censoring. Other values have a
#'special meaning that can be modified by \code{value.codes}. The value "I"
#'indicates interval censoring and requires values for \code{value2}.
#'@param value.codes special value qualifier codes. See Lorenz and others, 2014
#'for a description of these codes and the special \code{remark.codes}.
#'@param reporting.level the reporting level associated with each value.
#'@param reporting.method the method used to compute the reporting level. These
#'can be arbitrary codes, known only to the user.
#'@param reporting.units the reporting units. A few defined units such as
#'"mg/l" are recognized and used by the software.
#'@param analyte.method the method used to compute the concentration. These can
#'be arbitrary codes, known only to the user.
#'@param analyte.name the name of the analyte. A few names are recongnized such
#'as "chloride" and used by the software.
#'@param unique.code an arbitrary code identifying unique instances of data.
#'For data retreived from the US Geological Survey, this should be the 5-digit
#'parameter code.
#'@param value2 for interval censored data, the upper value.
#'@return An object of class "qw" that contains the numeric data in a
#'two-column matrix and the meta data as additional slot information.
#'@note The class "qw" is intended to be a flexible and user-friendly
#'way to store and manage water-quality data. The meta data that are stored
#'with the numeric and remark data are essential for unbiased statistical
#'analysis and for reducing other errors in interpretation.
#'@seealso \code{\link{importNWISqw}}
#'@references Lorenz, D.L., in preparation
#'@exportMethod as.qw
#'@keywords manip
setGeneric("as.qw", function(values, remark.codes, value.codes, reporting.level,
                             reporting.method, reporting.units, analyte.method,
                             analyte.name, unique.code, value2)
           standardGeneric("as.qw")
  ## Arguments:
  ##  values (numeric vector) the numeric data
  ##  remark.codes (character vector) the remark code
  ##  value.codes (character vector) the value qualifier codes
  ##  reporting.level (numeric vector)the reporting level associated with each value
  ##  reporting.method (character vector) the reporting level type
  ##  reporting.units (character vector) the units of the values
  ##  analyte.method (character vector) the method used to compute the value
  ##  analyte.name (character vector) the name of the analyte
  ##  unique.code (character vector) any code (like parameter code) for identifying a
  ##    unique group of data
  ##  value2 (numeric vector) the numeric values associated with interval censored data
)
