#'Water-Quality Data
#'
#'Import discrete sample water-quality data from NWISweb.
#'
#'Valid groups are "All," "information," "physical," "cations," "anions,"
#'"nutrients," "microbiological," "biological," "metals," "nonmetals,"
#'"toxicity," "pesticides," "pcbs," "other organics," "radio chemicals,",
#'"stable isotopes," "sediment," and "population/community."
#'
#'@param sites a vector of the USGS station identifiers.
#'@param params A character string contains the name of a group of parameter
#'codes, or a vector of parameter codes. See \bold{Details}.
#'@param begin.date the earliest date for data, must be a character with the
#'format "YYYY-mm-dd."
#'@param end.date the latest date for data, must be a character with the format
#'"YYYY-mm-dd."
#'@param keep a character vector for any additional columns to retain from the
#'retrieved data.
#'@param use.pnames create colummn names based on pcode (like P00000)? Default
#'is to generate more English-like names.
#'@return A data frame of the water-quality data of class "qw" organized by
#'sample. Column names for the water-quality constituents are generated 
#'automatically, but can be set by the user,
#'see \code{\link{makeColNames}}.
#'@note The sample information columns that are automatically retained are the 
#'USGS station identifier, sample date, sample time, time zone code, and medium
#'code. If composite samples are found in the data, then sample end date and 
#'sample end time are also included. Use the \code{keep} argument to retain more if needed.
#'@seealso \code{\link{readNWISqw}}, \code{\link{makeColNames}}
#'@references Lorenz, D.L., 2014, USGSwsQW OFR.\cr See information about discrete
#'samples at \url{http://nwis.waterdata.usgs.gov/usa/nwis/qw}.
#'@keywords datasets IO
#'@examples
#'
#'\dontrun{
#'importNWISqw("05330000", "00608") # Ammonia samples from the Minnesota River at Jordan.
#'}
#'
#'@export
importNWISqw <- function(sites, params="All", begin.date="", end.date="",
                         keep=NULL, use.pnames=FALSE) {
  ## Coding history:
  ##    2012Sep17 DLLorenz original Coding
  ##    2013Jan26 DLLorenz Added option to use Pcode names
  ##
  ByResult <- readNWISqw(sites, params, begin.date, end.date)
  if(use.pnames) {
    Extra <- pcodeNWISqw(params, group=FALSE, name=FALSE, CASRN=FALSE,
                       short=TRUE, units=TRUE, col.name=FALSE)
    Extra$col_name <- paste("P", Extra$parameter_cd, sep="")
  }
  else
    Extra <- pcodeNWISqw(params, group=FALSE, name=FALSE, CASRN=FALSE,
                         short=TRUE, units=TRUE, col.name=TRUE)
  ByResult <- merge(ByResult, Extra, by.x="parm_cd", by.y="parameter_cd")
  ## Create the qw column and the by sample dataset
  ByResult$qw <- as.qw(ByResult$result_va, ByResult$remark_cd,
                       ByResult$val_qual_tx,
                       ByResult$rpt_lev_va, ByResult$rpt_lev_cd,
                       ByResult$parameter_units, ByResult$meth_cd,
                       ByResult$srsname, ByResult$parm_cd)
  ## The function group2row cannot handle complicated data structures like qw
  ##  work around by creating index to values and then extract the actual data
  ByResult$Seq <- seq(nrow(ByResult))
  ## If composite samples are found in the mix, retain the end dates, times and tz
  if(any(!is.na(ByResult[["sample_end_dt"]]))) 
    keep <- c("sample_end_dt", "sample_end_tm", keep)
  Carry <- c("site_no", "sample_dt", "sample_tm", "sample_start_time_datum_cd", 
    "medium_cd", keep)
  retval <- group2row(ByResult, Carry, "col_name", "Seq")
  names(retval)[4L] <- "tzone_cd" # Make it simple
  for(i in grep(".Seq", names(retval), value=TRUE, fixed=TRUE))
    retval[[i]] <- ByResult$qw[retval[[i]]]
  names(retval) <- gsub(".Seq", "", names(retval), fixed=TRUE)
  ## Sort by date
  Seq <- order(retval$sample_dt, retval$sample_tm, na.last=TRUE)
  retval <- retval[Seq,]
  return(retval)
}
