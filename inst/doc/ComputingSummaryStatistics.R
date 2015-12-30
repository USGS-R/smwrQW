### R code from vignette source 'ComputingSummaryStatistics.Rnw'

###################################################
### code chunk number 1: ComputingSummaryStatistics.Rnw:32-38
###################################################
# Load the smwrQW package
library(smwrQW)
# And the data
data(Oahu, package="NADA")
# Convert the data
Oahu <- transform(Oahu, As=as.lcens(As, censor.codes=AsCen))


###################################################
### code chunk number 2: ComputingSummaryStatistics.Rnw:48-52
###################################################
# The mean and standard deviation.
with(Oahu, censStats(As, method="log MLE"))
# And the quantiles
with(Oahu, quantile(As, method="log MLE"))


###################################################
### code chunk number 3: ComputingSummaryStatistics.Rnw:57-59
###################################################
# The mean and standard deviation.
with(Oahu, censStats(As, method="log AMLE"))


###################################################
### code chunk number 4: ComputingSummaryStatistics.Rnw:67-71
###################################################
# The mean and standard deviation.
with(Oahu, censStats(As, method="flipped K-M"))
# And the quantiles
with(Oahu, quantile(As, method="flipped K-M"))


###################################################
### code chunk number 5: ComputingSummaryStatistics.Rnw:76-84
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
### code chunk number 6: ComputingSummaryStatistics.Rnw:92-96
###################################################
# The mean and standard deviation.
with(Oahu, censStats(As, method="log ROS"))
# And the quantiles
with(Oahu, quantile(As, method="log ROS"))


