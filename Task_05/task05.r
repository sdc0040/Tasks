library(learnPopGen)
coalescent.plot(n=5, ngen=10, colors=NULL, lwd=2, col.order="sequential")
#each simulation begins with 5 alleles you can change that by changing the
#number of haploid individuals (n)
#on average it took 7 generations to go to fixation
#the average offspring is 1 for each allele and the variance is 13
#fitness plays no role in these simulations
#no the most recent common ancestor wasn't typically alive in generation 0
var(c(8, 3, 10))
install.packages("rehh", dep=T)
install.packages("assertthat", dep=T)
install.packages("RcppArmadillo", dep=T)
install.packages("https://cran.r-project.org/src/contrib/Archive/scrm/scrm_1.7.3-1.tar.gz", repos=NULL, type="source")
install.packages("https://cran.r-project.org/src/contrib/Archive/coala/coala_0.6.0.tar.gz", repos=NULL, type="source")
install.packages("phytools")
library(coala)
library(phytools)
model <- coal_model(sample_size = 5, loci_number = 10, loci_length = 500, ploidy = 2) +
  feat_mutation(10) +
  feat_recombination(10) +
  sumstat_trees() +
  sumstat_nucleotide_div()
stats <- simulate(model, nsim = 1)
Diversity <- stats$pi
Nloci <- length(stats$pi)
t1 <- read.tree(text=stats$trees[[1]][1])
plot(t1)
axisPhylo()
#the tips are showing common ancestors as well
Agel <- max(nodeHeights(t1))
t2 <- read.tree(text=stats$trees[[2]][1])
plot(t2)
axisPhylo()
#no they do not match
par(mfrow=c(1,2))
plot(t1)
axisPhylo()
plot(t2)
axisPhylo()
compare.chronograms(t1, t2)
t1_1 <- read.tree(text=stats$trees[[1]][1])
t1_2 <- read.tree(text=stats$trees[[1]][2])
compare.chronograms(t1_1, t1_2)
for(locus in 1:Nloci) {
  ntrees <- length(stats$trees[[locus]])
  for(n in 1:ntrees) {
    if (locus == 1 && n == 1) {
      outPhy <- read.tree(text=stats$trees[[locus]][n])
    }
    else {
      outPhy <- ape:::c.phylo(outPhy, read.tree(text=stats$trees[[locus]][n]))
    }
  }  
}
par(mfrow=c(1, 1))
densityTree(outPhy)
model3 <- coal_model(10, 50) +
  feat_mutation(par_prior("theta", sample.int(100, 1))) +
  sumstat_nucleotide_div()
stats <- simulate(model3, nsim = 40)
mean_pi <- sapply(stats, function(x) mean(x$pi))
theta <- sapply(stats, function(x) x$pars[["theta"]])
plot(c(mean_pi, theta))
lm(mean_pi ~ theta)
abline(lm(mean_pi ~ theta))