### R code from vignette source 'QualityControl.Rnw'

###################################################
### code chunk number 1: QualityControl.Rnw:37-50
###################################################
# Load the smwrQW package
library(smwrQW)
# Generate normal data
set.seed(132)
Xn <- rnorm(26, mean=5)
# And left-censor it at 2 levels
Xc <- as.lcens(Xn, rep(c(4.5, 5.0), 13))
# And log-normal data
Xln <- rlnorm(26, mean=0.1, sd=0.3)
# And left censor it at 2 levels
Xlc <- as.lcens(Xln, rep(c(0.8, 1.2), 13))
# Multiply censor the data
Xmc <- censor(Xln, 0.8, 1.75)


###################################################
### code chunk number 2: QualityControl.Rnw:71-77
###################################################
# The t.test for confidence interval of the mean
t.test(Xn, conf.level=.9)
# The adjusted maximum likelihood
censMean.CI(Xn, method="AMLE", CI=.9)
# Compare to the left-censored data
censMean.CI(Xc, method="AMLE", CI=.9)


###################################################
### code chunk number 3: QualityControl.Rnw:82-90
###################################################
# The AMLE, uncensored
censMean.CI(Xln, method="log AMLE", CI=.9)
# The AMLE, left censored
censMean.CI(Xlc, method="log AMLE", CI=.9)
# MLE for multiply censored
censMean.CI(Xmc, method="log MLE", CI=.9)
# and ROS
censMean.CI(Xmc, method="log ROS", CI=.9)


###################################################
### code chunk number 4: QualityControl.Rnw:97-107
###################################################
# Uncensored data, by default, 90% CI for median
qtiles.CI(Xln)
censQtiles.CI(Xln)
# Compare to censored data
censQtiles.CI(Xlc)
# Compute the upper confidence interval for selected probabilities
censQtiles.CI(Xlc, probs=c(.75, .9, .95), bound="upper")
# Compare to "filled in" values
Xlc.f <- fillIn(Xlc, method="log ROS")
qtiles.CI(Xlc.f, probs=c(.95), bound="upper")


###################################################
### code chunk number 5: QualityControl.Rnw:116-124
###################################################
# What is the maximum detection limit?
Xlc.max <- max(censorLevels(Xlc))
Xlc.max
# What percentage?
percentile(Xlc, 1.2)
# Compute the confidence interval
binom.test(sum(Xlc >= Xlc.max),
	length(Xlc), percentile(Xlc, 1.2, percent=FALSE))


###################################################
### code chunk number 6: QualityControl.Rnw:144-208
###################################################
# Get the sample and the reporting level data (supplied with smwrQW)
data(atrazine.df)
data(atrazine.dl)
# Change the schedule code (sched) in the sample data to match the 
# LabCode in the reporting level data (easy for these data, others 
# could be more complicated).
atrazine.df$LabCode <- sub("NWQL", "1", atrazine.df$sched)
# The reportling level data do not report values for LabCode 12033 prior
# to 2005-03-04, but that LabCode is recorded in the sample data. 
# LabCode 12033 is equivalent to 12003, so recode 
atrazine.df$LabCode[atrazine.df$LabCode == "12033" & 
    atrazine.df$Date < "2005-03-04"] <- "12003"
# Extract the method code from the TestID in the reporting level data
atrazine.dl$QW_METHOD_CD <- substring(atrazine.dl$TestID, 6)
# For these data the reporting level codes agree, but in some case, the
# reporting level data uses lower case.
# The data are now ready for processing. They cannot be merged because
# there is no exact match for dates. This requires a relatively slow 
# loop to get the 1 to 1 match.
# First set up vector of detection limits
DL <- numeric(nrow(atrazine.df))
# Then loop through each row
for(i in seq(DL)) {
	# match method code
	tmp.mc <- atrazine.df$QW_METHOD_CD[i] == atrazine.dl$QW_METHOD_CD 
	# match reporting level codes
	tmp.rlc <- atrazine.df$RPT_LEV_CD[i] == atrazine.dl$ReportLevelCode
	# and finally the dates
	tmp.dt <- atrazine.df$Date[i] >= atrazine.dl$StartDate & 
		atrazine.df$Date[i] <= atrazine.dl$EndDate
	# match lab code only if necessary--more than one detection limit
	# This works when the LabCode is not recorded in the sample data or 
	# for different DLs among the LabCodes
	tmp.sel <- tmp.mc & tmp.rlc & tmp.dt
	if(sum(tmp.sel) > 1) {
		# extract the value(s)
		tmp.val <- unique(atrazine.dl[tmp.sel, "DetectionLevel"])
		if(length(tmp.val) > 1L) {
			tmp.sel <- tmp.sel & atrazine.df$LabCode[i] == atrazine.dl$LabCode
			tmp.val <- atrazine.dl[tmp.sel, "DetectionLevel"]
		}
	} else { # found a single or none
		tmp.val <- atrazine.dl[tmp.sel, "DetectionLevel"]
	}
	if(length(tmp.val)) {
		DL[i] <- tmp.val
	} else {
		DL[i] <- NA
	}
}
# clean up
rm(tmp.dt, tmp.mc, tmp.rlc, tmp.sel, tmp.val, i)
# Do the missing values make sense?
Miss <- which(is.na(DL))
atrazine.df[Miss, c("Date", "sched", "RPT_LEV_VA", "RPT_LEV_CD", "QW_METHOD_CD")]
#  The values have no code for the reporting level code and the reporting
# levels are different from those in the reporting level data--OK.
#
# Update the reporting level value and code where DL is not missing.
atrazine.df$DL <- ifelse(is.na(DL), atrazine.df$RPT_LEV_VA, DL)
atrazine.df$DL_CD <- ifelse(is.na(DL), atrazine.df$RPT_LEV_CD, "DL")
# Update the censored values in RESULT_VA--this works only for left-censored
atrazine.df$RESULT_VA <- ifelse(atrazine.df$REMARK_CD == "<", 
    atrazine.df$DL, atrazine.df$RESULT_VA)


###################################################
### code chunk number 7: QualityControl.Rnw:213-248
###################################################
# Subset the atrazine sample data and rename the columns
# Blank data
atrazine.oaq <- subset(atrazine.df, MEDIUM_CD == "OAQ", 
  select = c("staid", "Date", "MEDIUM_CD", "SAMP_TYPE_CD", 
    "RESULT_VA", "REMARK_CD", "DL", "DL_CD", "QW_METHOD_CD",
    "Units", "PARAMETER_CD"))
names(atrazine.oaq)[5:11] <- paste0("atrazine", c("", ".rmk",
  ".rlv", ".rmt", ".mth", ".unt", ".pcd"))
# Check if practcal to reset the DL for the blank data by method
with(subset(atrazine.oaq, atrazine < atrazine.rlv), 
     table(atrazine, atrazine.mth))
# there is only 1 value less than the detection limit, so the payback for 
# resetting the detection limits is small.
# Convert the atrazine to "qw"
atrazine.oaq <- convert2qw(atrazine.oaq, scheme="partial")
#
# Replicate data
atrazine.wsq <- subset(atrazine.df, MEDIUM_CD == "WSQ", 
  select = c("staid", "Date", "MEDIUM_CD", "SAMP_TYPE_CD", 
    "RESULT_VA", "REMARK_CD", "DL", "DL_CD", "QW_METHOD_CD",
    "Units", "PARAMETER_CD"))
names(atrazine.wsq)[5:11] <- paste0("atrazine", c("", ".rmk",
  ".rlv", ".rmt", ".mth", ".unt", ".pcd"))
# Convert the atrazine to "qw"
atrazine.wsq <- convert2qw(atrazine.wsq, scheme="partial")
#
# Envirionmental sample data
atrazine.ws <- subset(atrazine.df, MEDIUM_CD == "WS", 
  select = c("staid", "Date", "MEDIUM_CD", "SAMP_TYPE_CD", 
    "RESULT_VA", "REMARK_CD", "DL", "DL_CD", "QW_METHOD_CD",
    "Units", "PARAMETER_CD"))
names(atrazine.ws)[5:11] <- paste0("atrazine", c("", ".rmk",
  ".rlv", ".rmt", ".mth", ".unt", ".pcd"))
# Convert the atrazine to "qw"
atrazine.ws <- convert2qw(atrazine.ws, scheme="partial")


###################################################
### code chunk number 8: QualityControl.Rnw:258-260
###################################################
# Print the blank data exceeding the DL
subset(atrazine.oaq, atrazine >= 0.004)


###################################################
### code chunk number 9: QualityControl.Rnw:265-294
###################################################
# Initial processing
# The range of percentages for the envirovnemtal data
Pct <- seq(60, 100, by=0.5)
# The quantiles of the environmental data
Env.Q <- quantile(atrazine.ws$atrazine, probs=Pct/100)
# The range for the balnks, no reason to compute for probibilities
# More than two times 1.2 percent smaller than 1
# The confidence interval cannot be computed for 1, (100 percent)
# set the largest percent to 99.9
Pct.90 <- seq(97.6, 99.9, by=.1)
# The upper 90 confidence limit of the blanks
Bnk.90 <- censQtiles.CI(atrazine.oaq$atrazine, probs=Pct.90/100,
  CI=0.9, bound="upper")
# set up the graphics environment: setSweave used in vignettes
setSweave("graph01", 6, 6)
AA.pl <- xyPlot(Pct, Env.Q, Plot=list(name="Environmental Samples",
  what="lines", color="orange", width="color"), yaxis.log=TRUE,
  yaxis.range=c(0.001, 100), ytitle="Concentration, in micrograms per liter",
  xaxis.range=c(60, 100), xtitle="Cumulative Percent of Distribtution")
# Add minor ticks for detail on the x-axis
addMinorTicks("x", AA.pl)
# Add the detection limit and annotation
refLine(horizontal=0.004, current=AA.pl)
addAnnotation(65, 0.004, "Maximum Long-term Method Detection Limit", current=AA.pl)
# Add the UCL--Bnk.90 is a matrix, extract the ucl column
AA.pl <- addXY(Pct.90, Bnk.90[,"ucl"], Plot=list(name="90 Percent UCL for Blanks",
  what="lines", color="red", width="color"), current=AA.pl)
# And the explanation
addExplanation(AA.pl, "ul")


