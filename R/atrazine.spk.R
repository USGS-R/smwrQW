#' Atrazine Spike Data

#' A dataset of atrazine environmental and spike samples collected by the U.S.
#'Geological Survey from October 1, 2001 through September 30, 2012.
#'
#' @details A map of the study units can be found at 
#'\url{http://water.usgs.gov/nawqa/nawqamap.html}. Some of the study unit
#'codes represent combined study units from that map.
#'
#' @name atrazine.spk
#' @docType data
#' @usage atrazine.spk
#' @format Data frame with 596 rows and 22 columns\cr
#'\tabular{lll}{
#'Name \tab Type \tab Description\cr
#'pstaid \tab character \tab Not sure why we need this\cr
#'suid \tab character \tab Study Unit Identifier, see \bold{Details}\cr
#'Date \tab Date \tab Date of sample\cr
#'PARAMETER_CD \tab character \tab Parameter code\cr
#'PARAMETER_NM \tab character \tab Parameter name\cr
#'spkTime \tab character \tab Time of spike sample\cr
#'spkMEDIUM_CD \tab character \tab Spike sample medium code\cr
#'spkREMARK_CD \tab character \tab Spike sample remark code for value\cr
#'spkRESULT_VA \tab numeric \tab Spike sample result value\cr
#'spkSched \tab character \tab Spike sample schedule code\cr
#'envTime \tab character \tab Time of environmental sample\cr
#'envMEDIUM_CD \tab character \tab Environmental sample medium code\cr
#'envREMARK_CD \tab character \tab Environmental sample remark code for value\cr
#'envRESULT_VA \tab numeric \tab Environmental sample result value\cr
#'envSched \tab character \tab Environmental sample schedule code\cr
#'spkLotNo \tab numeric \tab Spike sample lot number\cr
#'spkConc \tab numeric \tab Spike sample concentration\cr
#'spkVol \tab numeric \tab Spike sample volume\cr
#'envVol \tab numeric \tab Environmental sample volume\cr
#'Recovery \tab numeric \tab Spike recovery, in percent\cr
#'sname \tab character \tab USGS station name\cr
#'staid \tab character \tab USGS station number\cr
#'}
#' @source Data retreived from the national water information system and processed 
#'by Jeffery Martin of the U.S. Geological Survey.
#' @keywords datasets
#' @examples
#' data(atrazine.spk)
#' # The mean spike recovery
#' mean(atrazine.spk$Recovery)
NULL
