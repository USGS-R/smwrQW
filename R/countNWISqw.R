#'Count Water-Quality Records
#'
#'Returns the number of observations
#'
#'
#'@param sites the USGS station identifiers.
#'@param by character string indicating how to summarize the counts. Must be
#'one of "summary," "total," "group," or "pcode" indicating summary output,
#'the total number of observations, the number of observations by parameter
#'group, or the number of observations for each parameter code.
#'@return If \code{by} is "summary," then a data frame of the station
#'identifiers, the begin and end dates of the samples, and the total number of
#'samples. Otherwise, a list, organized by each site in \code{sites}, of the
#'begin and end dates of the sampled data and a table of the counts.
#'Verify if counts of samples or results for other than summary.
#'@note The summary retrieval is a fast, and easy way to see how many samples
#'have been taken at at site.  The other options can require several seconds to
#'retrieve and process the data and provide a count for each parameter, not
#'sample.
#'@keywords misc
#'@examples
#'
#'\dontrun{
#'countNWISqw("05330000")
#'}
#'
#'@export
countNWISqw <- function(sites, by="summary") {
  ## Coding history:
  ##    2012Sep07 DLLorenz original Coding
  ##    2012Dec28 DLLorenz Roxygenized
  ##    2012Dec28          This version
  ##
  ## Pack sites and get the data
  sitel <- paste(sites, collapse="%0D%0A")
  by <- match.arg(by, c("summary", "total", "group", "pcode"))
  if(by == "summary") {
    ## Just get the summary info is a nice little data set.
    myurl <- url(paste("http://nwis.waterdata.usgs.gov/usa/nwis/qwdata?multiple_site_no=",
                       sitel,
                       "&sort_key=site_no&group_key=NONE&format=sitefile_output",
                       "&sitefile_output_format=rdb&column_name=site_no",
                       "&column_name=qw_begin_date&column_name=qw_end_date",
                       "&column_name=qw_count_nu&inventory_output=0",
                       "&rdb_inventory_output=file&TZoutput=0",
                       "&pm_cd_compare=Greater than&radio_parm_cds=all_parm_cds",
                       "&qw_attributes=0&qw_sample_wide=wide&rdb_qw_attributes=0",
                       "&date_format=YYYY-MM-DD&rdb_compression=file",
                       "&list_of_search_criteria=multiple_site_no", sep=""))
    retval <- importRDB(myurl)
    close(myurl)
    return(retval)
  }
  ## Continue with details
  myurl <- url(paste("http://nwis.waterdata.usgs.gov/nwis/qwdata?multiple_site_no=", sitel,
                     "&sort_key=site_no&group_key=NONE&inventory_output=0",
                     "&format=rdb_inventory&rdb_inventory_output=value",
                     "&radio_parm_cds=all_parm_cds", sep=""))
  count <- importRDB(myurl)
  close(myurl)
  ## Some checks
  cksite <- !(sites %in% unique(count$site_no))
  if(all(cksite))
    stop("no count information retrieved--check for valid sites")
  if(any(cksite))
    warning("no count information for: ", paste(sites[cksite], collapse=", "))
  if(length(by) > 1)
    ckby <- 0
  else
    ckby <- pmatch(by, c("total", "group", "pcode"), nomatch=0)
  if(ckby == 0) # extract specific pcodes
    count <- count[count$parm_cd %in% by, ]
  else if(ckby == 1)
    count$parm_cd <- "total"
  else if(ckby == 2) {
    retgrp <- pcodeNWISqw("All", name=FALSE, short=FALSE, units=FALSE)
    count$parm_cd <- retgrp$parameter_group_nm[match(count$parm_cd, retgrp$parameter_cd)]
    count$parm_cd <- factor(count$parm_cd, levels=levels(retgrp$parameter_group_nm))
  }
  ## OK the groups are set divie it up
  retval <- by(count, count$site_no, function(df) {
    df.agg <- tapply(df$count_nu, df$parm_cd, sum)
    df.agg <- df.agg[!is.na(df.agg)]
    list(First.Sample = df$begin_dt[1],
         Last.Sample = df$end_dt[1],
         Table = df.agg)})
  return(retval)
}
