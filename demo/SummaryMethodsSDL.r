# A Monte Carlo simulation for summary statistical methods (single DL). 
library(USGSwsQW)
# Enter the random seed for reproducibility
set.seed(340)
# Enter the number simulations each set of controls
NR <- 500
# Enter a vector of the (integer) number of samples in each group
NS <- c(15L, 25L, 50L)
# Enter the nominal censoring level
Cen <- c( .1, .25, .5)
# Enter the log standard deviations of the data
Sd <- c(.25, .5, .75)# Enter the skewness values of the data (0 is log-normal)
Skew <- c(-.5, -.25, 0, .25, .5)
# Done with Specs
# Create a function to convert the "mcens" output to simple numeric
simplify <- function(x) rowMeans(x@.Data)
# Crete the storage arrays
Length <- NR*length(NS)*length(Cen)*length(Sd)*length(Skew)
# The size of the sample
MCN <- integer(Length)
# The population stats (of the logs)
MCPM <- MCPS <- MCPK <- MCPC <- double(Length)
# The sample stats (of the data)
MCSM <- MCSS <- MCSC <- double(Length)
# The log ROS estimates (of the logs and the data)
LRLM <- LRLS <- LRM <- LRS <- double(Length)
# The log MLE estimates (of the logs and the data)
MLLM <- MLLS <- MLM <- MLS <- double(Length)
# The log AMLE estimates (of the logs and the data)
AMLM <- AMLS <- AMM <- AMS <- double(Length)
# The flipped Kaplan-Meier estimates (of the data)
KMM <- KMS <- double(Length)
# The simple substitution method
SSM <- SSS <- double(Length)
# The Fill method from Gleit
SDM <- SDS <- double(Length)
# The triangular fill method based on Quinn and Keogh
TFM <- TFS <- double(Length)
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
					MCSM[i] <- mean(Xrand)
					MCSS[i] <- sd(Xrand)
					MCSC[i] <- sum(Xrand < CenLev)/NS1
					# Now make the computations
					Xtmp <- censStats(as.lcens(Xrand, CenLev), method="log ROS")
					LRLM[i] <- Xtmp$meanlog
					LRLS[i] <- Xtmp$sdlog
					LRM[i] <- Xtmp$mean
					LRS[i] <- Xtmp$sd
					Xtmp <- censStats(as.lcens(Xrand, CenLev), method="log MLE")
					MLLM[i] <- Xtmp$meanlog
					MLLS[i] <- Xtmp$sdlog
					MLM[i] <- Xtmp$mean
					MLS[i] <- Xtmp$sd
					Xtmp <- censStats(as.lcens(Xrand, CenLev), method="log AMLE")
					AMLM[i] <- Xtmp$meanlog
					AMLS[i] <- Xtmp$sdlog
					AMM[i] <- Xtmp$mean
					AMS[i] <- Xtmp$sd
					Xtmp <- censStats(as.lcens(Xrand, CenLev), method="flipped K-M")
					KMM[i] <- simplify(Xtmp$mean)
					KMS[i] <- simplify(Xtmp$sd)
					## These are appropriate for single DL only
					# Simple substituion at 1/2 DL
					Xtmp <- ifelse(Xrand < CenLev, CenLev/2, Xrand)
					SSM[i] <- mean(Xtmp)
					SSS[i] <- sd(Xtmp)
					# The sdl fill method
					Xtmp <- sdlFill(as.lcens(Xrand, CenLev), method="log fill")
					SDM[i] <- mean(Xtmp$fitted)
					SDS[i] <- sd(Xtmp$fitted)
					# The triangular fill method
					Xtmp <- fillIn(as.lcens(Xrand, CenLev), method="tri")
					TFM[i] <- mean(Xtmp)
					TFS[i] <- sd(Xtmp)
				}
			}
		}
	}
}

# Construct dataset of all results
AllStatsSDL <- data.frame(MCN, MCPS, MCPK, MCPC, MCSM, MCSS, MCSC, LRLM, LRLS, LRM, LRS, 
													MLLM, MLLS, MLM, MLS, AMLM, AMLS, AMM, AMS, KMM, KMS, SSM, SSS, SDM, SDS)


