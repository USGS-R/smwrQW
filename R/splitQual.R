#' @title Split Leading Qualifiers
#'
#' @description Split vector of strings of combined data into numeric and remark columns
#'
#' @param x a character vector, missing values are permitted and result in missing values in the output
#' @param name a character string giving the names of the output columns.
#' @return A data frame with 2 columns, containing the numeric values and the qualification codes.
#' @import USGSwsGraphs
#' @examples
#'splitQual(c("<1", "1", "<1", "1", "2"), name="Dummy")
#'
#' @export
splitQual <- function(x, name = deparse(substitute(x))) {
  qual.index <- regexpr("[0-9.]", x)
  ## Find first numeric character
  qual.codes <- substring(x, 1, qual.index - 1)
  qual.codes <- ifelse(qual.index > 1, qual.codes, " ")
  ## Replace incorrect and force to character (strip.blanks in in USGSwsGraphs)
  qual.codes <- strip.blanks(qual.codes)
  qual.codes <- as.vector(qual.codes)
  ## Remove unnecessary blanks
  qual.value <- substring(x, qual.index)
  qual.value <- as.double(qual.value)
  retval <- paste("data.frame(", name, "=qual.value, ",
                  paste(name, ".rmk", sep = ""), "=qual.codes, stringsAsFactors=F)")
  retval <- eval(parse(text = retval))
  ## evaluate the character string
  return(retval)
}
