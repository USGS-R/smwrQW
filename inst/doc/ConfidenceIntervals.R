### R code from vignette source 'ConfidenceIntervals.Rnw'

###################################################
### code chunk number 1: ConfidenceIntervals.Rnw:30-37
###################################################
# Load the smwrQW package
library(smwrQW)
# And the data
data(Golden, package="NADA")
# Convert and subset the data
Golden <- with(subset(Golden, DosageGroup ==  "Low"),
  data.frame(Liver=as.lcens(Liver, censor.codes=LiverCen)))


###################################################
### code chunk number 2: ConfidenceIntervals.Rnw:47-57
###################################################
# first assess the log-normality
censPPCC.test(log(Golden$Liver))
# compute the 90 percent (default) two-sided confidence intervals
# The default log AMLE method
censMean.CI(Golden$Liver)
# The log MLE method
censMean.CI(Golden$Liver, method="log MLE")
# The log ROS method
set.seed(123)
censMean.CI(Golden$Liver, method="log ROS")


###################################################
### code chunk number 3: ConfidenceIntervals.Rnw:68-74
###################################################
# The two-sided confidence intervals
censQtiles.CI(Golden$Liver, probs=c(.25, .5, .75))
# and the upper confidence interval
censQtiles.CI(Golden$Liver, probs=c(.25, .5, .75), bound="upper", CI=0.95)
# Example where the maximum value is reported for the upper limit
censQtiles.CI(Golden$Liver, probs=c(.9), bound="upper", CI=0.95)


