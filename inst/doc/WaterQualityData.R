### R code from vignette source 'WaterQualityData.Rnw'

###################################################
### code chunk number 1: WaterQualityData.Rnw:33-37
###################################################
# Load the smwrQW package
library(smwrQW)
# print the slot names of the class "qw"
slotNames("qw")


###################################################
### code chunk number 2: WaterQualityData.Rnw:45-51
###################################################
# get the data
WFBC.NH3 <- importNWISqw("0531656290", "00608", end.date="2006-09-30")
# print the structure
str(WFBC.NH3)
# Now get the nutrient data for Little Cobb
LCobb.nuts <- importNWISqw("05320270", "NUT")


###################################################
### code chunk number 3: WaterQualityData.Rnw:56-63
###################################################
# get the data
WFBC.wqp <- readWQPqw("USGS-0531656290", "00608", endDate="2006-09-30")
# print the structure, note that the output is captured and modified to fit 
# in a narrow output; the additional attribute data frames are also stripped
Tmp <- capture.output(str(data.frame(WFBC.wqp), vec.len=1))
Tmp <- sapply(Tmp, sub, pattern=":", replacement="\n   ", fixed=TRUE)
cat(Tmp, sep="\n")


###################################################
### code chunk number 4: WaterQualityData.Rnw:70-95
###################################################
# Combine the numeric data and convert the remark codes
WFBC.wqp <- transform(WFBC.wqp, 
  values=ifelse(ResultDetectionConditionText == "Not Detected",
    DetectionQuantitationLimitMeasure.MeasureValue, ResultMeasureValue),
  remark.codes = ifelse(ResultDetectionConditionText == "Not Detected",
    "<", "")) # Everything else is passed through as uncensored
# Convert the data
WFBC.nh3 <- importQW(WFBC.wqp, keep=c("MonitoringLocationIdentifier",
    "ActivityStartDate", "ActivityStartTime.Time", "ActivityEndDate",
    "ActivityEndTime.Time", "ActivityStartTime.TimeZoneCode", "ActivityMediaName"),
  values="values",
  remark.codes="remark.codes",
  value.codes="ResultCommentText",
  reporting.level="DetectionQuantitationLimitMeasure.MeasureValue",
  reporting.method="DetectionQuantitationLimitTypeName",
  reporting.units="DetectionQuantitationLimitMeasure.MeasureUnitCode",
  analyte.method="ResultAnalyticalMethod.MethodIdentifier",
  analyte.name="CharacteristicName",
  unique.code="USGSPCode")
# And show what we've got
# print the structure, note that the output is captured and modified to fit 
# in a narrow output
Tmp <- capture.output(str(WFBC.nh3, vec.len=2))
Tmp <- sapply(Tmp, sub, pattern=":", replacement="\n   ", fixed=TRUE)
cat(Tmp, sep="\n")


###################################################
### code chunk number 5: WaterQualityData.Rnw:105-111
###################################################
# Print an example of censored dissolved organic nitrogen
LCobb.nuts[10, c("NitrogenOrg", "Ammonia.N", "Kjeldahl.N.00623")]
# Recompute censored dissolved organic nitrogen
LCobb.nuts$NitrogenOrg <- with(LCobb.nuts, add(Kjeldahl.N.00623, -Ammonia.N, 
  analyte="Organic nitrogen", pcode="00607"))
LCobb.nuts[10, c("NitrogenOrg", "Ammonia.N", "Kjeldahl.N.00623")]


###################################################
### code chunk number 6: WaterQualityData.Rnw:116-122
###################################################
# Subset the data to create ademonstration data set.
LCobb.sub <- LCobb.nuts[1:20, c("Phosphorus.P", "Phosphorus_WW.P")]
# Compute the ratio
LCobb.sub$Ratio <- with(LCobb.sub, ratio(Phosphorus.P, Phosphorus_WW.P))
# Print the results
LCobb.sub


###################################################
### code chunk number 7: WaterQualityData.Rnw:131-149
###################################################
# Use the Ammonia data from 0531656290
NH3 <- WFBC.NH3$Ammonia.N
# Print the values, specifically calling print makes it more readable
# The "n" following E 0.005 is a non-blank value qualifying code, verifying 
# that the E means the the value is greater than the detection limit, but 
# less than the reporting level
print(NH3)
# Equality and inequality:
NH3 == 0.026
NH3 != 0.026
# Greater than and greater than or equal to
NH3 > .1
NH3 >= .1
# Less than and less that or equal to
NH3 < 0.04
NH3 <= 0.04
# And the range checker
NH3 %~=% 0.02


###################################################
### code chunk number 8: WaterQualityData.Rnw:157-170
###################################################
# Retrieve alkalinity data for a couple of sites.
sites <- c("01493112", "01632900")
# The parameter codes representthe preferred order for computing alkalinity
# according to NWIS
PC <- c("39086", "29802", "39036", "00418", "39087", "29803", "29801", "00421")
# Get the data
Alk <- importNWISqw(sites, PC, begin.date="2011-01-01", end.date="2013-12-31", 
    use.pnames=TRUE)
# Note only parameter codes 29801 and 39806 were retrieved for these sites
# Compute alkalinity
Alk <- transform(Alk, Alk=as.numeric(qwCoalesce(P39086, P29801)))
# Print the first 10 rows of the data
head(Alk, 10)


###################################################
### code chunk number 9: WaterQualityData.Rnw:175-184
###################################################
# Print the summary information for WFBC.NH3.
# Nobs is the number of non-missing values
summary(WFBC.NH3)
# Compare to str
str(WFBC.NH3)
# More details can be extracted from the summary of the qw data
summary(NH3, details=TRUE)
# In this output, the units, and all of the analytical methods and reporting 
# methods are returned, rather than "many" in the previous call to summary.


###################################################
### code chunk number 10: WaterQualityData.Rnw:189-205
###################################################
# Print NH3.
print(NH3)
# select the first 3 values
print(NH3[1:3])
# skip the first value
print(NH3[-1])
# extract using a logical vector
print(NH3[WFBC.NH3$sample_dt < "2006-01-01"])
# Use subset to extract the data associated with an analytical method
print(subset(NH3, analyte.method == "CL037"))
# Make a temporary copy of NH3 to demonstrate assignment
Tmp <- NH3
# Must treat as a matrix to set the necessary meta data to NA
Tmp[2,] <- NA
print(Tmp)
rm(Tmp)


###################################################
### code chunk number 11: WaterQualityData.Rnw:215-222
###################################################
# Set up the graphics environment, the equivalent call for an on screen
#  device would be setPage("square")
setSweave("graph01", 6 ,6)
# Plot the data
plot(NH3, set.up=FALSE)
# Required call to close PDF output graphics
graphics.off()


###################################################
### code chunk number 12: WaterQualityData.Rnw:234-241
###################################################
# The raw data--the n following the E 0.005 value is a qualification code
# indicating that the reported value is less than the reporting level
print(NH3)
# And converted to lcens
print(as.lcens(NH3))
# And converted to mcens
print(as.mcens(NH3))


###################################################
### code chunk number 13: WaterQualityData.Rnw:246-262
###################################################
# Create a dataset that can be easily manipulated
NH3.df <- as.data.frame(NH3, expand=TRUE)
print(NH3.df)
# reset the reporting level, column suffix .rlv
NH3.df$NH3.rlv[NH3.df$NH3.mth == "00048"] <- 0.005
# and recensor the data in columnm suffix .va2
# Must be careful not to recensor elevated censored values
NH3.df$NH3.va2[NH3.df$NH3.mth == "00048" & NH3.df$NH3.rmk == "<" &
    NH3.df$NH3.va2 == 0.01] <- 0.005
# Print to verify
print(NH3.df)
# And convert back to water-quality data (as a data.frame)
NH3.df <- convert2qw(NH3.df, "qw")
print(NH3.df)
# verify the conversion
print(as.lcens(NH3.df$NH3))


###################################################
### code chunk number 14: WaterQualityData.Rnw:267-271
###################################################
# Print the data
print(NH3)
# And convert to numeric values
print(as.numeric(NH3))


###################################################
### code chunk number 15: WaterQualityData.Rnw:276-280
###################################################
# Print the data
print(NH3)
# And convert to numeric values
print(qw2mcens(NH3))


###################################################
### code chunk number 16: WaterQualityData.Rnw:288-299
###################################################
# Print the data
print(NH3)
# Compute the natural log, data converted to lcens
print(log(NH3))
# Compute the square root, data converted to lcens
print(sqrt(NH3))
# The default output from pow, using an exponent to mimic the square root
print(pow(NH3, .5))
# And forced to lcens
print(pow(NH3, .5, out="lcens"))
# Note that the last 2 are simply twice the result using sqrt


