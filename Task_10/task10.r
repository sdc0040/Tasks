library(phytools)
trees <- list()
births <- vector()
Fractions <- vector()
netdiversification <- vector()
speciationrate <- vector()
avgbranch <- vector()

for (i in 1:100) {
  births [i]<- runif(1, 0, 1)
  Fractions [i]<- runif(1, 0, 1)
  trees[[i]]<- pbtree(n=100, b=births [i], d=Fractions [i] * births [i])
  netdiversification[i] <- (births [i] - Fractions[i] * births[i])
  speciationrate[i] <- births[i]
  avgbranch[[i]] <- mean(trees[[i]] $edge.length)
}

totaltreetips <- log(sapply(trees, Ntip))
#Q4
plot(netdiversification, totaltreetips)
line <- lm(totaltreetips ~ netdiversification)
abline(line)
#As net diversification increases log of total tips increases
#Q5
plot(speciationrate, avgbranch)
#speciation rate increases and avg branch length decreases
#Q6
cor(speciationrate, avgbranch)
#-0.4500243
#Q7
trees
trees[2]
Tree <- trees[[2]]
plot(Tree)
rates <- vector()
traits <- list()
for (i in 1:100) {
  rates[i] <- runif(1)
  traits[[i]] <- fastBM(Tree, sig2=rates[i])
}
#Q8
cor(mean(traits), rates)
head(rates)
head(traits)
length(traits)
head(traits[[i]])
mtraits <- sapply(traits, mean)
Q8c <- cor(mtraits, rates)
plot(mtraits, rates)
pdf("Q8 cor mtraits vs rates.pdf")
plot(mtraits, rates)
dev.off()
#there is no correlation between mean of traits and rates 0.008
#Q9
vartraits <- sapply(traits, var)
Q9c <- cor(vartraits, rates)
plot(vartraits, rates)
pdf("Q9 cor vartraits, rates.pdf")
plot(vartraits, rates)
dev.off()
#positive variation between the correlation between the variation of traits and rates 0.78
#Q10
element1 <- sapply(traits, "[[",1)
element2 <- sapply(traits, "[[",2)
traitMat <- cbind(element1, element2)
plot(element1, element2)
Q10c <- cor(element1, element2)
pdf("Q10 element1 vs element2.pdf")
plot(element1, element2)
dev.off()
#there is a positive correlation between element 1 and element 2 of the traits 0.375
#No it is not significant as it randomly simulates the elements and it is positively correlated each time
#meaning overall there is no significance in a positive correlation value

