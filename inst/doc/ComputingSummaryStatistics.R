### R code from vignette source 'ComputingSummaryStatistics.Rnw'

###################################################
### code chunk number 1: ComputingSummaryStatistics.Rnw:21-27
###################################################
# Load the smwrQW package
library(smwrQW)
# And the data
data(Oahu, package="NADA")
# Convert the data
Oahu <- transform(Oahu, As=as.lcens(As, censor.codes=AsCen))


###################################################
### code chunk number 2: ComputingSummaryStatistics.Rnw:37-41
###################################################
# The mean and standard deviation.
with(Oahu, censStats(As, method="log MLE"))
# And the quantiles
with(Oahu, quantile(As, method="log MLE"))


###################################################
### code chunk number 3: ComputingSummaryStatistics.Rnw:46-48
###################################################
# The mean and standard deviation.
with(Oahu, censStats(As, method="log AMLE"))


###################################################
### code chunk number 4: ComputingSummaryStatistics.Rnw:56-60
###################################################
# The mean and standard deviation.
with(Oahu, censStats(As, method="flipped K-M"))
# And the quantiles
with(Oahu, quantile(As, method="flipped K-M"))


###################################################
### code chunk number 5: ComputingSummaryStatistics.Rnw:65-73
###################################################
# Modify the Oahu dataset, censoring the first 0.5 value
OahuX <- Oahu
OahuX$AsCen[which(OahuX$As == 0.5)[1]] <- TRUE
# The mean and standard deviation.
with(OahuX, censStats(As, method="flipped K-M"))
# And the quantiles
with(OahuX, quantile(As, method="flipped K-M"))
with(OahuX, censQuantile(As, method="flipped K-M"))


###################################################
### code chunk number 6: ComputingSummaryStatistics.Rnw:81-85
###################################################
# The mean and standard deviation.
with(Oahu, censStats(As, method="log ROS"))
# And the quantiles
with(Oahu, quantile(As, method="log ROS"))


