setwd("~/Desktop")
library(fishtree)
library(phytools)
setwd("~/Downloads/doi_10")
Data1 <- read.csv("NZ-FISH-TRAITS-Raw-Trait-Data.csv")

setwd("/Users/spencercutlip/Downloads")
untar("actinopt_full.trees.xz")

Tree <- fishtree_phylogeny(rank="Acanthomorpha")
Missing <- setdiff(Data1$Genus.Species, Tree$tip.label)
Tree2 <- fishtree_phylogeny(unique(c(Tree$tip.label, Missing)))
Tree3 <- fishtree_phylogeny(Missing)

Jaw <- Data1$Lj
names(Jaw) <- Data1$Genus.Species
Length <- Data1$TL
names(Length) <- Data1$Genus.Species

X <- cbind(Jaw, Length)
X <- X[Tree$tip.label,]

Obj <- phyl.vcv(X, vcv(Tree), 1)
PearsonR <- cov2cor(Obj$R)["Jaw", "Length"]

phylomorphospace(Tree, X, label="off")

