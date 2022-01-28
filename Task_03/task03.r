trueMean1 <- 5
trueSD1 <- 5
population1 <- rnorm(1e6, trueMean1, trueSD1)
trueMean2 <- 4
trueSD2 <- 5
population2 <- rnorm(1e6, trueMean2, trueSD2)
Size <- 50
Sample1 <- sample(population1, Size)
Sample2 <- sample(population2, Size)
boxplot(Sample1, Sample2)

source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandma <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
PatGrandpa <- makeFounder("grandpa_da")
head(MatGrandma)
Alan <- makeBaby(PatGrandma, PatGrandpa)
head(Alan)
Brenda <- makeBaby(MatGrandma, MatGrandpa)
head(Brenda)
Focus <- makeBaby(Brenda, Alan)
ToMom <- length(grep("mom", Focus)) / length(Focus)
ToMomMom <- length(grep("grandma_mom", Focus)) / length(Focus)
ToMomDad <- length(grep("grandpa_mom", Focus)) / length(Focus)
Sibling_01 <- makeBaby(Brenda, Alan)
ToSib <- length(intersect(Focus, Sibling_01)) / length(Focus)
ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan)))) / length(Focus)
quantile(ManySiblings)
mean(ManySiblings)
plot(density(ManySiblings), main="", xlab="proportion shared genes")
#They usually share 50%, but this can range due to relatedness
HWE <- function(p) {
  aa <- p^2
  ab <- 2 * p * (1 - p)
  bb <- (1 - p) ^2
  return(c(aa=aa, ab=ab, bb=bb))
}
HWE(0.5)
plot(1, 1, type="n", xlim=c(0, 1), ylim=c(0, 1), xlab="freq. allele a", ylab="geno. freq")
p <- seq(from = 0, to =1, by = 0.1)
GenoFreq <- t(sapply(p, HWE))
lines(p, GenoFreq[,"aa"], lwd=2, col="red")
#As aa frequency increases so does the allele frequency
#They are directly related so as one decreases so does the other
#No time or geographic space shown
lines(p, GenoFreq[,"ab"], lwd=2, col="purple")
lines(p, GenoFreq[,"bb"], lwd=2, col="blue")
legend("top", legend=c("aa", "ab", "bb"), col=c("red", "purple", "blue"), lty=1, lwd=2, bty="n")
Pop <- simPop(500)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/500, pch=21, bg="red")
#Yes, all points were on red line grouped around the 0.5 mark
Pop <- simPop(50)
points(Pop[,"freqa"], Pop[,"Genotypes.aa"]/50, pch=22, bg="red")
#Points are much more spaced out along line, possibly due to a magnified look on a smaller pop.

library(learnPopGen)
x <- genetic.drift(Ne=200, nrep=5, pause=0.01)
#Changing Ne didn't change the lines just the magnification of the graph
PopSizes <- 5:50
Samples <- rep(PopSizes, 5)
tExt <- sapply(Samples, function(x) nrow(simPop(x, 500)))
Line <- lm(tExt ~ Samples)
summary(Line)
Line$coef
plot(Samples, tExt)
abline(Line)
Line2 <-lm(tExt~Samples + 0)
abline(Line2)
#As you increase in Sample size the farther the points spread and the farther
#they move away from the line
install.packages("MASS")
library(MASS)
LineA <- rlm(tExt ~ Samples)
abline(LineA)
LineA$coef
summary(LineA)
