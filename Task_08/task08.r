library(phytools)
text.string<-
  "(((((((cow, pig), whale), (bat, (lemur, human))), (robin, iguana)), coelacanth), (gold_fish, trout)), shark);"
vert.tree <- read.tree(text=text.string)
plot(vert.tree, edge.width=2)
nodelabels(frame="circle", bg='white', cex=1)
#Q1- Humans are more closely related. Humans and gold fish - node 14, goldfish and shark - node 13 
vert.tree
str(vert.tree)
#Q2- yes, there are branch lengths
tree <- read.tree(text="(((A, B), (C, D)), E);")
plotTree(tree, offset=1)
tiplabels(frame="circle", bg='lightblue', cex=1)
nodelabels(frame="circle", bg='white', cex=1)
tree$tip.label
tree$edge
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
?plot.phylo
#Q3
plot(AnolisTree, cex=0.25, show.tip.label=FALSE)
#Q4
plot(AnolisTree, type="fan", cex=0.25, show.tip.label=FALSE)
#Q5
plot(AnolisTree, type="fan", cex=0.25, tip.color = 'red')
?which
which.min(AnolisTree == 'minedge.length')
#Q6-8
which.min(AnolisTree$edge.length) #82
newAnolisTree <- drop.tip(AnolisTree, 82) 
plot(newAnolisTree, cex=0.25)
ltt(AnolisTree)
abline(0, 1, lwd=2, col='red', lty=2)
#line never goes down meaning lineage is growing, slope changes and is overall positive showing a contuation in the lineage
#Q10
?fit.bd
fit.bd(AnolisTree, rho=0.2)
#birth rate 0.8031 and no extinction