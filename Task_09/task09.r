library(phytools)
setwd("~/Desktop/Evolution/Tasks/Task_09")
AnolisTree <- force.ultrametric(read.tree("https://jonsmitchell.com/data/anolis.tre"))
par(las=1)
hist(AnolisTree$edge.length, col='black', border='white', main="", xlab="edge lengths for the Anolis tree", ylim=c(0, 50), xlim=c(0, 6))
tipEdges <- which(AnolisTree$edge[, 2] <= Ntip(AnolisTree))
Lengths <- AnolisTree$edge.length
names(Lengths) <- AnolisTree$tip.label
names(Lengths)[which(Lengths == min(Lengths))]
plot(AnolisTree, cex=0.25)
Labs <- sapply(AnolisTree$edge.length, round, digits=2)
edgelabels(text=Labs, cex=0.25)
plot(AnolisTree, type="fan")
#Q1- 82 tips and yes there are branch lengths
data <- read.csv("https://jonsmitchell.com/data/svl.csv", stringsAsFactors=F, row.names=1)
#Q2- list of raw data 2 columns 144 rows
svl <- setNames(data$svl, rownames(data))
Ancestors <- fastAnc(AnolisTree, svl, vars=TRUE, CI=TRUE)
Ancestors
#Q3- stored via name Ancestors and it is the 95% confidence intervals
#Q4- trait is continuous and that the probability the trait is gained or lost is equal
par(mar=c(0.1, 0.1, 0.1, 0.1))
plot(AnolisTree, type="fan", lwd=2, show.tip.label=F)
tiplabels(pch=16, cex=0.25*svl[AnolisTree$tip.label])
nodelabels(pch=16, cex=0.25*Ancestors$ace)
obj <- contMap(AnolisTree, svl, plot=F)
plot(obj, type="fan", legend=0.7*max(nodeHeights(AnolisTree)), sig=2, fsize=c(0.7, 0.9))
fossilData <- data.frame(svl=log(c(25.4, 23.2, 17.7, 19.7, 24, 31)), tip1=c("Anolis_aliniger", "Anolis_aliniger", "Anolis_occultus", "Anolis_ricordii", "Anolis_cristatellus", "Anolis_occultus"), tip2=c("Anolis_chlorocyanus", "Anolis_coelestinus", "Anolis_hendersoni", "Anolis_cybotes", "Anolis_angusticeps", "Anolis_angusticeps"))
#Q5
fossilNodes <-c()
nodeN <- c()
head(fossilData)
for (i in 1:6) {
  Node <- fastMRCA(AnolisTree, fossilData[i, "tip1"], fossilData[i, "tip2"])
  fossilNodes[i] <- fossilData[i, "svl"]
  nodeN[i] <- Node
  
}
names(fossilNodes) <- nodeN
Ancestors_withFossils <- fastAnc(AnolisTree, svl, anc.states=fossilNodes, CI=TRUE, var=TRUE)
#Q7- Fossils will increase the overall ancestral size 
#Q8-10
install.packages("geiger")
library(geiger)
brownianFit <- fitContinuous(AnolisTree, data, model="BM")
lambdaFit <- fitContinuous(AnolisTree, data, model="lambda")
ebFit <- fitContinuous(AnolisTree, data, model="EB")
kappaFit <- fitContinuous(AnolisTree, data, model="kappa")
ouFit <-   fitContinuous(AnolisTree, data, model="OU")
ratetrendFit <-  fitContinuous(AnolisTree, data, model="rate_trend") 
deltaFit <-  fitContinuous(AnolisTree, data, model="delta") 
meantrendFit <-   fitContinuous(AnolisTree, data, model="mean_trend")
whiteFit <- fitContinuous(AnolisTree, data, model="white")
brownianFit
lambdaFit
ebFit #lowest AIC best fit
kappaFit
ouFit
ratetrendFit
deltaFit
meantrendFit
whiteFit
