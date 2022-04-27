setwd("~/Desktop")
library(fishtree)
library(phytools)
setwd("~/Desktop/Evolution/Tasks/Project/data")
Data1 <- read.csv("NZ-FISH-TRAITS-Raw-Trait-Data.csv")

#fullTree <- read.tree("actinopt_full.trees")
#T1 <- fullTree[[1]]
#write.tree(T1, "fulltree1.tre")
#Missing <- setdiff(Data1$Genus.Species, T1$tip.label)
#DropFromTree <- setdiff(T1$tip.label, Data1$Genus.Species)
#T2 <- drop.tip(T1, DropFromTree)
#write.tree(T2, "tree2.tre")
##Tree2 <- fishtree_phylogeny(unique(c(Tree$tip.label, Missing)))
##Tree3 <- fishtree_phylogeny(Missing)

Tree <- read.tree("tree2.tre")
Missing <- setdiff(Data1$Genus.Species, Tree$tip.label)
head(Data1)

Drop <- as.numeric(Data1$Genus.Species %in% Missing)
Data2 <- Data1[-which(Drop==1),]

Jaw <- Data2$Lj
names(Jaw) <- Data2$Genus.Species
Length <- Data2$TL
names(Length) <- Data2$Genus.Species

X <- cbind(Jaw, Length)
X <- X[Tree$tip.label,]

Obj <- phyl.vcv(X, vcv(Tree), 1)
PearsonR <- cov2cor(Obj$R)["Jaw", "Length"]

phylomorphospace(Tree, X, label="off")

