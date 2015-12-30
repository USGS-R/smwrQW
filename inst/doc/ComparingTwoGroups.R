### R code from vignette source 'ComparingTwoGroups.Rnw'

###################################################
### code chunk number 1: ComparingTwoGroups.Rnw:30-36
###################################################
# Load the smwrQW package
library(smwrQW)
# And the data
data(CuZn, package="NADA")
# Convert the Zn data, Cu is not used
CuZn <- transform(CuZn, Zn=as.lcens(Zn, censor.codes=ZnCen))


###################################################
### code chunk number 2: ComparingTwoGroups.Rnw:46-55
###################################################
# Subset the data to remove the Cu (columns 1 and 2) and the missing values
ZnData <- na.omit(CuZn[, 3:5])
# Add a column of 0/1 values
ZnData <- transform(ZnData, Zn01=code01(Zn)[[1]])
# Tabulate the 0/1 data and print it
ZnTbl <- with(ZnData, table(Zone, Zn01))
print(ZnTbl)
# And the test
prop.test(ZnTbl)


###################################################
### code chunk number 3: ComparingTwoGroups.Rnw:66-83
###################################################
setSweave("graph01", 6 ,6)
# Set layout for 2 graphs
AA.lo <- setLayout(num.cols=2, num.rows=2)
# Create the graphs
setGraph(1, AA.lo)
with(subset(CuZn, Zone=="AlluvialFan"), qqPlot(Zn,
  ytitle="Alluvial Zinc", yaxis.log=FALSE))
setGraph(2, AA.lo)
with(subset(CuZn, Zone=="BasinTrough"), qqPlot(Zn,
  ytitle="BasinTrough Zinc", yaxis.log=FALSE))
setGraph(3, AA.lo)
with(subset(CuZn, Zone=="AlluvialFan"), qqPlot(Zn,
  ytitle="Alluvial Zinc", yaxis.log=TRUE))
setGraph(4, AA.lo)
with(subset(CuZn, Zone=="BasinTrough"), qqPlot(Zn,
  ytitle="BasinTrough Zinc", yaxis.log=TRUE))
graphics.off()


###################################################
### code chunk number 4: ComparingTwoGroups.Rnw:93-95
###################################################
# The two-sample test:
censReg(Zn ~ Zone, data=CuZn, dist="lognormal")


###################################################
### code chunk number 5: ComparingTwoGroups.Rnw:105-109
###################################################
# The Gehan two-sample test:
with(CuZn, genWilcox.test(Zn, Zone, method="gehan"))
# The Peto-Prentice two-sample test:
with(CuZn, genWilcox.test(Zn, Zone, method="peto"))


###################################################
### code chunk number 6: ComparingTwoGroups.Rnw:114-118
###################################################
# The Peto type two-sample test
with(CuZn, censKSample.test(Zn, Zone, type="Peto"))
# The log-rank type two-sample test
with(CuZn, censKSample.test(Zn, Zone, type="log-rank"))


