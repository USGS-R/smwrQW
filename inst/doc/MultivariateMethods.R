### R code from vignette source 'MultivariateMethods.Rnw'

###################################################
### code chunk number 1: MultivariateMethods.Rnw:30-65
###################################################
# Load the smwrQW package
library(smwrQW)
# Create the dataset omitting Age, code as character
DDT <- data.frame(Site=1:32,
  opDDD=c("<5","<5","5.3","<5","<5","<5","<5","<5",
          "<5","5.1","<5","<5","<5","5.1","<5","<5",
          "9","<5","9.8","<5","<5","5.1","8","<5",
          "<5","<5","5.7","<5","<5","<5","<5","<5"),
  ppDDD=c("<5","42","38","12","<5","<5","14","15",
          "12","39","5.7","9.4","18","27","10","7.6",
          "46","22","41","13","26","24","100","<5",
          "<5","<5","27","15","20","22","31","15"),
  opDDE=c("<5","8.4","<5","<5","<5","<5","<5","<5",
          "<5","<5","<5","<5","<5","<5","<5","<5",
          "<5","<5","6.9","<5","<5","250","8","<5",
          "<5","<5","<5","6","7.5","<5","<5","<5"),
  ppDDE=c("14","130","250","57","16","<5","52","48",
          "110","100","87","53","210","140","24","15",
          "110","51","50","66","110","38","160","23",
          "17","16","140","8.1","22","190","42","23"),
  opDDT=c("<5","<5","<5","<5","<5","<5","<5","<5",
          "<5","<5","18","<5","<5","<5","<5","<5",
          "<5","<5","<5","<5","<5","<5","<5","<5",
          "<5","<5","<5","<5","<5","5.2","<5","<5"),
  ppDDT=c("<5","31","11","<5","<5","<5","14","<5",
          "20","24","<5","7.3","30","33","5.8","10",
          "21","7.4","11","8.6","26","11","<5","<5",
          "<5","<5","14","<5","6.4","27","25","5.6"),
  stringsAsFactors=FALSE)
# Convert concentrations to class "lcens"
DDT <- transform(DDT, opDDD=as.lcens(opDDD), ppDDD=as.lcens(ppDDD),
  opDDE=as.lcens(opDDE), ppDDE=as.lcens(ppDDE),
  opDDT=as.lcens(opDDT), ppDDT=as.lcens(ppDDT))
# Load the libraries
library(cluster)


###################################################
### code chunk number 2: MultivariateMethods.Rnw:75-82
###################################################
# Compute the 0/1 values
DDT01 <- with(DDT, code01(opDDD, ppDDD, opDDE, ppDDE, opDDT, ppDDT))
# Compute the dissimilarities using simple matching
DDT.diss <- daisy(DDT01, metric="gower", type=list(symm=1:6))
# Show the first 10 rows of the 0/1 data and the distances
head(DDT01, 10)
print(as.dist(as.matrix(DDT.diss)[1:10,1:10]), digits=4)


###################################################
### code chunk number 3: MultivariateMethods.Rnw:87-96
###################################################
# The cluster analysis
DDT.hclust <- hclust(DDT.diss, method="average") 
# Details for the cluster merging sequence
cbind(DDT.hclust$merge, DDT.hclust$height)
# Plot the dendrogram
setSweave("graph01", 6 ,6)
# Create the graph, 
dendGram(DDT.hclust, ytitle="Average Merging Dissimilarity")
graphics.off()


###################################################
### code chunk number 4: MultivariateMethods.Rnw:111-118
###################################################
# Compute the u scores
DDTU <- with(DDT, codeU(opDDD, ppDDD, opDDE, ppDDE, opDDT, ppDDT))
# Compute the dissimilarities using simple matching
DDTU.dist <- dist(DDTU)
# Show the first 8 rows of the 0/1 data and the distances
head(DDTU, 8)
print(as.dist(as.matrix(DDTU.dist)[1:8,1:8]), digits=4)


###################################################
### code chunk number 5: MultivariateMethods.Rnw:123-131
###################################################
# The cluster analysis
DDTU.hclust <- hclust(DDTU.dist, method="average") 
# Plot the dendrogram
setSweave("graph02", 6 ,6)
# Create the graph, 
dendGram(DDTU.hclust, ytitle="Average Merging Distance", 
         xtitle="Site Number")
graphics.off()


###################################################
### code chunk number 6: MultivariateMethods.Rnw:142-156
###################################################
# The PCA.
DDTU.pca <- princomp(DDTU, cor=TRUE)
print(DDTU.pca)
print(loadings(DDTU.pca), digits=4, cutoff=0)
# Plot the PCA
setSweave("graph03", 6 ,6)
# Create the graph, 
AA.pl <- biPlot(DDTU.pca, Scale="distance", range.factor=1.1, 
     obsPlotLabels=list(labels="none"))
# Manually assign label direction to reduce ambiguity
dir <- c("N","NE","NW","S","N","E","W","NE","E","E","NE","SW","NE","E","S","S",
  "N","NE","NE","NE","NE","NE","NW","SE","NW","SE","N","NE","NE","NE","NE","NW")
with(AA.pl, labelPoints(x, y, labels=1:32, dir=dir, offset=0.5, current=AA.pl))
graphics.off()


###################################################
### code chunk number 7: MultivariateMethods.Rnw:173-204
###################################################
# Create the altered dataset, code as character
DDTalt <- data.frame(Site=1:32,
  opDDD=c("<2","4","5.3","2","<2","<2","2.3","2.3",
          "2.1","5.1","<2","<2","2.8","5.1","<2","<2",
          "9","2.8","9.8","2.1","3.3","5.1","8","<2",
          "<2","<2","5.7","2.1","2.6","3","3.5","2.2"),
  ppDDD=c("3.1","42","38","12","3.1","2.4","14","15",
          "12","39","5.7","9.4","18","27","10","7.6",
          "46","22","41","13","26","24","100","3.2",
          "3.1","3.1","27","15","20","22","31","15"),
  opDDE=c("2.1","8.4","<2","<2","<2","4.9","<2","<2",
          "<2","<2","<2","<2","<2","<2","<2","<2",
          "<2","<2","6.9","<2","<2","250","8","<2",
          "<2","<2","<2","6","7.5","<2","<2","<2"),
  ppDDE=c("14","130","250","57","16","3.4","52","48",
          "110","100","87","53","210","140","24","15",
          "110","51","50","66","110","38","160","23",
          "17","16","140","8.1","22","190","42","23"),
  opDDT=c("<2","3.9","2.4","2","<2","<2","2.4","2",
          "2.5","2.6","18","2.2","2.6","2.7","2.1","2.2",
          "4.9","2.3","2.4","2.3","2.6","2.4","2.3","<2",
          "<2","<2","2.5","2","2.2","5.2","2.6","2.2"),
  ppDDT=c("2.2","31","11","3.2","2.3","<2","14","3.2",
          "20","24","3","7.3","30","33","5.8","10",
          "21","7.4","11","8.6","26","11","3.9","2.5",
          "2.3","2.3","14","2.6","6.4","27","25","5.6"),
  stringsAsFactors=FALSE)
# Convert concentrations to class "lcens"
DDTalt <- transform(DDTalt, opDDD=as.lcens(opDDD), ppDDD=as.lcens(ppDDD),
  opDDE=as.lcens(opDDE), ppDDE=as.lcens(ppDDE),
  opDDT=as.lcens(opDDT), ppDDT=as.lcens(ppDDT))


###################################################
### code chunk number 8: MultivariateMethods.Rnw:209-223
###################################################
# Impute the less-than values, multRepl required because opDDE is heavily censored
DDTaltImp <- with(DDTalt, imputeLessThans(opDDD, ppDDD, opDDE, ppDDE, opDDT, ppDDT,
  initial="multRepl"))
# Print alternate and imputed values
head(DDTalt)
head(DDTaltImp)
# The PCA.
DDTaltImp.pca <- prcomp(log(DDTaltImp), center=TRUE, scale=TRUE)
print(DDTaltImp.pca)
# Plot the PCA
setSweave("graph04", 6 ,6)
# Create the graph, 
biPlot(DDTaltImp.pca, Scale="distance", range.factor=1.1)
graphics.off()


