### R code from vignette source 'PlottingCensoredData.Rnw'

###################################################
### code chunk number 1: PlottingCensoredData.Rnw:29-39
###################################################
# Load the smwrQW package
library(smwrQW)
# And get the data
data(Atra, package="NADA")
data(AtraAlt, package="NADA")
# Convert the data
Atra <- transform(Atra, June=as.lcens(June, censor.codes=JuneCen),
  Sept=as.lcens(Sept, censor.codes=SeptCen))
AtraAlt <- transform(AtraAlt, June=as.lcens(June, censor.codes=JuneCen),
  Sept=as.lcens(Sept, censor.codes=SeptCen))


###################################################
### code chunk number 2: PlottingCensoredData.Rnw:53-76
###################################################
# setSweave is a specialized function that sets up the graphics page for
# Sweave scripts. It should be replaced by a call to setPage or setPDF 
# in a regular script.
setSweave("graph01", 6 ,6)
# Set layout for 4 graphs
AA.lo <- setLayout(num.cols=2, num.rows=2)
# Set up and graph the data
AA.gr <- setGraph(1, AA.lo)
with(Atra, boxPlot(June, Sept, Box=list(type="simple")))
addTitle("Censored Box Plot, Single Reporting Level", Bold=FALSE)
AA.gr <- setGraph(2, AA.lo)
with(AtraAlt, boxPlot(June, Sept, Box=list(type="simple")))
addTitle("Censored Box Plot, Multiple Reporting Level", Bold=FALSE)
AA.gr <- setGraph(3, AA.lo)
with(Atra, boxPlot(June, Sept, 
  Box=list(type="simple", censorstyle="estimated")))
addTitle("Estimated Box Plot, Single Reporting Level", Bold=FALSE)
AA.gr <- setGraph(4, AA.lo)
with(AtraAlt, boxPlot(June, Sept, 
  Box=list(type="simple", censorstyle="estimated")))
addTitle("Estimated Box Plot, Multiple Reporting Level", Bold=FALSE)
# Required call to close PDF output graphics
graphics.off()


###################################################
### code chunk number 3: PlottingCensoredData.Rnw:91-100
###################################################
setSweave("graph02", 6 ,6)
# Set layout for 2 graphs
AA.lo <- setLayout(num.rows=2)
# Create the graphs
setGraph(1, AA.lo)
with(Atra, ecdfPlot(June, xtitle="June Atrazine"))
setGraph(2, AA.lo)
with(Atra, ecdfPlot(June, xtitle="June Atrazine", xaxis.log=FALSE))
graphics.off()


###################################################
### code chunk number 4: PlottingCensoredData.Rnw:115-134
###################################################
setSweave("graph03", 6 ,6)
# Set layout for 2 graphs
AA.lo <- setLayout(num.rows=2)
# Create the graphs
setGraph(1, AA.lo)
# Compute the statistics using "log ROS"
AA.st <-  with(Atra, censStats(June, method="log ROS"))
print(AA.st)
with(Atra, probPlot(June, ytitle="June Atrazine", ylabels=5,
  mean=AA.st$meanlog, sd=AA.st$sdlog))
addTitle("Statistics using log ROS", Bold=FALSE)
setGraph(2, AA.lo)
# Compute the statistics using "log MLE"
AA.st <-  with(Atra, censStats(June, method="log MLE"))
print(AA.st)
with(Atra, probPlot(June, ytitle="June Atrazine",  ylabels=5,
  mean=AA.st$meanlog, sd=AA.st$sdlog))
addTitle("Statistics using log MLE", Bold=FALSE)
graphics.off()


###################################################
### code chunk number 5: PlottingCensoredData.Rnw:149-155
###################################################
setSweave("graph04", 6 ,6)
# The Q-Q plot
qqPlot(Atra$June, AtraAlt$June,
  xtitle="Atrazine", ytitle="Altered Atrazine",
  xaxis.log=TRUE, yaxis.log=TRUE)
graphics.off()


###################################################
### code chunk number 6: PlottingCensoredData.Rnw:165-169
###################################################
setSweave("graph05", 6 ,6)
# The Q-normal plot
qqPlot(Atra$June, ytitle="Atrazine", yaxis.log=TRUE)
graphics.off()


###################################################
### code chunk number 7: PlottingCensoredData.Rnw:182-188
###################################################
setSweave("graph06", 6 ,6)
# The x-axis data must be numeric, and add droplines (bar)
with(Atra, xyPlot(seq(1, nrow(Atra), by=1), June,
  xtitle="Sequential Number", ytitle="Atrazine in June",
  Censored=list(bar=TRUE)))
graphics.off()


