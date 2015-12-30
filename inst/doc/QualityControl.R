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


