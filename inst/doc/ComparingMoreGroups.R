### R code from vignette source 'ComparingMoreGroups.Rnw'

###################################################
### code chunk number 1: ComparingMoreGroups.Rnw:30-36
###################################################
# Load the smwrQW package
library(smwrQW)
# And the data
data(TCE, package="NADA")
# Convert the data to column TCE
TCE <- transform(TCE, TCE=as.lcens(TCEConc, censor.codes=TCECen))


###################################################
### code chunk number 2: ComparingMoreGroups.Rnw:46-55
###################################################
# Create Density as a factor ordered Low-Medium-High
TCE <- transform(TCE, Density=factor(Density, levels=c("Low", "Medium", "High")))
# Append a column of 0/1 values to the data
TCE <- cbind(TCE, with(TCE, code01(TCE01=TCE)))
# Tabulate the 0/1 data and print it
TCETbl <- with(TCE, table(Density, TCE01))
print(TCETbl)
# And the test
prop.test(TCETbl)


###################################################
### code chunk number 3: ComparingMoreGroups.Rnw:66-77
###################################################
setSweave("graph01", 3 ,6)
# Set layout for 2 graphs
AA.lo <- setLayout(num.cols=2, num.rows=1)
# Create the graphs
setGraph(1, AA.lo)
with(subset(TCE, Density=="Medium"), qqPlot(TCE,
  ytitle="TCE in Medium", yaxis.log=TRUE))
setGraph(2, AA.lo)
with(subset(TCE, Density=="High"), qqPlot(TCE,
  ytitle="TCE in High", yaxis.log=TRUE))
graphics.off()


###################################################
### code chunk number 4: ComparingMoreGroups.Rnw:87-89
###################################################
# The ANOVA analogue test:
censReg(TCE ~ Density, data=TCE, dist="lognormal")


###################################################
### code chunk number 5: ComparingMoreGroups.Rnw:99-103
###################################################
# The Peto type two-sample test
with(TCE, censKSample.test(TCE, Density, type="Peto"))
# The log-rank type two-sample test
with(TCE, censKSample.test(TCE, Density, type="log-rank"))


###################################################
### code chunk number 6: ComparingMoreGroups.Rnw:113-115
###################################################
# The Peto type two-sample test
with(TCE, censMulticomp.test(TCE, Density))


