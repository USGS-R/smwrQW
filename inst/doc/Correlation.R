### R code from vignette source 'Correlation.Rnw'

###################################################
### code chunk number 1: Correlation.Rnw:20-29
###################################################
# Load the smwrQW package
library(smwrQW)
# And the psych package, required for the phi function
library(psych)
# And the data
data(Atra, package="NADA")
# Convert the data
Atra <- with(Atra, data.frame(June=as.lcens(June, censor.codes=JuneCen),
  Sept=as.lcens(Sept, censor.codes=SeptCen)))


###################################################
### code chunk number 2: Correlation.Rnw:39-46
###################################################
# Create data frame of 0/1 data for June and September
Atra.01 <- with(Atra, code01(June, Sept))
# Tabulate the 0/1 data and print it
Atra.tbl <- table(Atra.01)
print(Atra.tbl)
# And compute the phi coefficient
phi(Atra.tbl, digits=4)


###################################################
### code chunk number 3: Correlation.Rnw:57-61
###################################################
setSweave("graph01", 6 ,6)
# Create the graph
with(Atra, xyPlot(June, Sept, yaxis.log=TRUE, xaxis.log=TRUE))
graphics.off()


###################################################
### code chunk number 4: Correlation.Rnw:71-73
###################################################
# The Pearson correlation:
with(Atra, censCor(log(June), log(Sept)))


###################################################
### code chunk number 5: Correlation.Rnw:81-87
###################################################
# The Spearman correlation:
# Step 1: U code the data, and print part
AtraU <- with(Atra, codeU(June, Sept))
head(AtraU)
# Compute rho and the test
with(AtraU, cor.test(June, Sept, method='spearman'))


###################################################
### code chunk number 6: Correlation.Rnw:92-94
###################################################
# Compute tau and the test
with(Atra, kendallATS.test(June, Sept))


