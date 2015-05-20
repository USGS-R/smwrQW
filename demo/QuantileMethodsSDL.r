# A Monte Carlo simulation for quaantile methods (single DL). 
library(smwrQW)
# Enter the random seed for reproducibility
set.seed(341)
# Enter the number simulations each set of controls
NR <- 500
# Enter a vector of the (integer) number of samples in each group
NS <- c(16L, 48L)
# Enter the nominal censoring level
Cen <- c(.5)
# Enter the log standard deviations of the data
Sd <- c(.25,  .75)
# Enter the skewness values of the data (0 is log-normal)
Skew <- c(-.5, -.25, 0, .25, .5)
# Done with Specs
# Crete the storage arrays
Length <- NR*length(NS)*length(Cen)*length(Sd)*length(Skew)
# The size of the sample
MCN <- integer(Length)
# The population stats (of the logs)
# Suffix M is mean, suffix S is std. dev., K is skewness, C is censoring
MCPS <- MCPK <- MCPC <- double(Length)
# Suffix Mn is min, suffix Q is 1st quartile
# The sample stats (of the data)
MCSMn <- MCSQ <- MCSC <- double(Length)
# The log ROS estimates (of the data)
LRMn <- LRQ <- double(Length)
# The log MLE estimates (of the data)
MLMn <- MLQ <- double(Length)
# The counter
i <- 0L
## The loops
for(Sk1 in Skew) {
	for(Sd1 in Sd) {
		for(Ce1 in Cen) {
  		for(NS1 in NS) {
				for(Index in seq(NR)) {
					i <- i + 1L
					# Generate the random seq
					Xrand <- rlpearsonIII(NS1, 0, Sd1, Sk1)
					# This censoring level should provide a mechanism that censors at
					# the correct percentage, but is not driven by a single value and thus biased.
					Xsrt <- sort(Xrand)
					Pick <- as.integer(NS1*Ce1)
					CenLev <- Xsrt[Pick] + runif(1)*(Xsrt[Pick + 1L] - Xsrt[Pick])
					# populate the initial values
					MCN[i] <- NS1
					MCPS[i] <- Sd1
					MCPK[i] <- Sk1
					MCPC[i] <- Ce1
					# The samples stats
					MCSMn[i] <- min(Xrand)
					MCSQ[i] <- quantile(Xrand, probs=.25)
					MCSC[i] <- sum(Xrand < CenLev)/NS1
					# Now make the computations
					Xtmp <- quantile(as.lcens(Xrand, CenLev), probs=c(0, .25), method="log ROS")
					LRMn[i] <- Xtmp[1L]
					LRQ[i] <- Xtmp[2L]
					Xtmp <- quantile(as.lcens(Xrand, CenLev), probs=c(0, .25), method="log MLE")
					MLMn[i] <- Xtmp[1L]
					MLQ[i] <- Xtmp[2L]
				}
			}
		}
	}
}

# Construct dataset of all results
AllQuantsSDL <- data.frame(MCN, MCPS, MCPK, MCPC, MCSMn, MCSQ, MCSC, LRMn, LRQ, 
													MLMn, MLQ)


