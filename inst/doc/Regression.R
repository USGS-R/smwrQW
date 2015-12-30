### R code from vignette source 'Regression.Rnw'

###################################################
### code chunk number 1: Regression.Rnw:31-39
###################################################
# Load the smwrQW package
library(smwrQW)
# And the data
data(TCEReg, package="NADA")
# Convert the data to column TCE
# For these data, force the reporting limit to be 1 unless the censoring
# limit is specified.
TCEReg <- transform(TCEReg, TCE=as.lcens(TCEConc, 1, censor.codes=TCECen))


###################################################
### code chunk number 2: Regression.Rnw:49-57
###################################################
# Append a column of 0/1 values to the data
# The maximum censored values is 5, which is the default criterion
TCEReg <- cbind(TCEReg, with(TCEReg, code01(TCE01=TCE)))
# Build the regression model
TCE.lr <- glm(TCE01 ~ Depth + PopDensity, data=TCEReg,
  family=binomial)
# Detailed output from the logistic regression
binaryReg(TCE.lr)


###################################################
### code chunk number 3: Regression.Rnw:62-81
###################################################
# Update the regression model
TCE.lr <- update(TCE.lr, ~ . - Depth)
# Detailed output from the logistic regression
binaryReg(TCE.lr)
# Print the farthest outlier
TCEReg[70, ]
# Plot the data and fit
setSweave("graph01", 6 ,6)
# Create the graph, 
# first create a jittered column to see more individual points
TCEReg <- transform(TCEReg, 
  TCEjit=TCE01 + runif(nrow(TCEReg), -.1, .1))
with(TCEReg, xyPlot(PopDensity, TCEjit,
  ytitle="Probability of exceeding criterion"))
# Make a prediction data set and fill in the predicted probabilities
TCEPred <- data.frame(PopDensity=1:19)
TCEPred$Pred <- predict(TCE.lr, newdata=TCEPred, type="response")
with(TCEPred, addXY(PopDensity, Pred))
graphics.off()


###################################################
### code chunk number 4: Regression.Rnw:94-104
###################################################
setSweave("graph02", 6 ,6)
# Create the graphs
AA.lo <- setLayout(num.cols=2, num.rows=2)
setGraph(1, AA.lo)
with(TCEReg, xyPlot(PopDensity, TCE, yaxis.log=TRUE))
setGraph(2, AA.lo)
with(TCEReg, xyPlot(Depth, TCE, yaxis.log=TRUE))
setGraph(3, AA.lo)
with(TCEReg, xyPlot(PctIndLU, TCE, yaxis.log=TRUE))
graphics.off()


###################################################
### code chunk number 5: Regression.Rnw:114-117
###################################################
# The censored regression model.
TCE.cr <- censReg(TCE ~ PopDensity + Depth + PctIndLU, data=TCEReg, dist="lognormal")
print(TCE.cr)


###################################################
### code chunk number 6: Regression.Rnw:122-130
###################################################
setSweave("graph03", 6 ,6)
# Create the graphs
AA.lo <- setLayout(num.rows=2)
setGraph(1, AA.lo)
plot(TCE.cr, which=1, set.up=FALSE)
setGraph(2, AA.lo)
plot(TCE.cr, which=2, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 7: Regression.Rnw:142-153
###################################################
# The summary output.
TCE.crsum <- summary(TCE.cr)
print(TCE.crsum)
setSweave("graph04", 6 ,6)
# Create the graphs
AA.lo <- setLayout(num.rows=2)
setGraph(1, AA.lo)
plot(TCE.crsum, which="PopDensity", set.up=FALSE)
setGraph(2, AA.lo)
plot(TCE.crsum, which="Depth", set.up=FALSE)
graphics.off()


###################################################
### code chunk number 8: Regression.Rnw:163-185
###################################################
# The revised censored regression model.
TCE.cr <- censReg(TCE ~ log(PopDensity) + Depth, data=TCEReg, dist="lognormal")
# The summary output.
TCE.crsum <- summary(TCE.cr)
print(TCE.crsum)
# Overall and Q-normal plots
setSweave("graph05", 6 ,6)
# Create the graphs
AA.lo <- setLayout(num.rows=2)
setGraph(1, AA.lo)
plot(TCE.cr, which=1, set.up=FALSE)
setGraph(2, AA.lo)
plot(TCE.cr, which=2, set.up=FALSE)
graphics.off()
setSweave("graph06", 6 ,6)
# Parial residual plots
AA.lo <- setLayout(num.rows=2)
setGraph(1, AA.lo)
plot(TCE.crsum, which="log(PopDensity)", set.up=FALSE)
setGraph(2, AA.lo)
plot(TCE.crsum, which="Depth", set.up=FALSE)
graphics.off()


