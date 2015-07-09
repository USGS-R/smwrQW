### R code from vignette source 'ComparingPairedData.Rnw'

###################################################
### code chunk number 1: ComparingPairedData.Rnw:19-25
###################################################
# Load the smwrQW package
library(smwrQW)
# And the data
data(Atra, package="NADA")
head(Atra, 2)
print(head(Atra, 2), digits=11)


###################################################
### code chunk number 2: ComparingPairedData.Rnw:37-50
###################################################
# First compute the interval-censored values for June and September
# Set the minimum value to 0 for censored values
June <- with(Atra, as.mcens(ifelse(JuneCen, 0, June), June))
Sept <- with(Atra, as.mcens(ifelse(SeptCen, 0, Sept), Sept))
# The parametric paired-data test, save for plot and print it
Atra.pt <- censReg(Sept-June ~ 1)
print(Atra.pt)
# setSweave required for vignettes, use setPage or setPDF otherwise
setSweave("graph01", 6 ,6)
# graph number 2 is the probability plot
plot(Atra.pt, which=2, set.up=FALSE)
# Reuired to close graph for the vignette
graphics.off()


###################################################
### code chunk number 3: ComparingPairedData.Rnw:60-69
###################################################
# The revised parametric paired-data test, save for plot and print it
Atra.pt <- censReg(pow(Sept, .04) - pow(June, .04) ~ 1)
print(Atra.pt)
# setSweave required for vignettes, use setPage or setPDF otherwise
setSweave("graph02", 6 ,6)
# graph number 2 is the probability plot
plot(Atra.pt, which=2, set.up=FALSE)
# Reuired to close graph for the vignette
graphics.off()


###################################################
### code chunk number 4: ComparingPairedData.Rnw:85-98
###################################################
# First compute the left-censored values for June and September
# Set the minimum value to 0 for censored values
June <- with(Atra, as.lcens(June, censor.codes=JuneCen))
Sept <- with(Atra, as.lcens(Sept, censor.codes=SeptCen))
# The PPW test, save for plot and print it
Atra.ppw <- ppw.test(Sept, June)
print(Atra.ppw)
# setSweave required for vignettes, use setPage or setPDF otherwise
setSweave("graph03", 6 ,6)
# the graphs are the scaled and actual numeric differences
plot(Atra.ppw, set.up=FALSE)
# Reuired to close graph for the vignette
graphics.off()


###################################################
### code chunk number 5: ComparingPairedData.Rnw:114-124
###################################################
# The left-censored values for June and September are from the previous example
# The Pratt test, save for plot and print it
Atra.prt <- pairedPratt.test(Sept, June)
print(Atra.prt)
# setSweave required for vignettes, use setPage or setPDF otherwise
setSweave("graph04", 6 ,6)
# the graphs are the scaled and actual numeric differences
plot(Atra.prt, set.up=FALSE)
# Reuired to close graph for the vignette
graphics.off()


