x <- rnorm(100, 5, 2)
y <- (x * 5) + 2 + runif(100, min=0, max=0.1)
lm(y~x) #slope is 5.001 and y intercept is 2.045
#The slope and intercept seem to be dependent on restrictions of the mean and variance
slope <- c()
yint <- c()
x <- c()
y <- c()
z <- c()
for (i in 1:100)
  {
  x[i] <- rnorm(100, 5, 2)
  z[i] <- rnorm(1)
  q[i] <- x * z
  y[i] <- (q * 5) + 2 + runif(100, min=0, max=0.1)
  mod <- lm(y~x)
  cf <- coef(mod)
  slope[i] <- cf["x"]
}

plot(z,slope) #reveals a stable slope that is non-dependent on z value

#Part 2
library(dplyr)
library(ggplot2)

doors <- 1:3
sample_doors <- function() { return(sample(doors, size = 1000, replace = TRUE))}
games <- data.frame(prize = sample_doors(), pick = sample_doors())

games$strategy <- factor(ifelse(games$prize == games$pick, 'stay', 'switch'))

monte_show <- function(prize, pick) {
  remaining <- setdiff(doors, c(prize, pick))
  return(ifelse(length(remaining)==1,
                remaining,
                sample(remaining, 1)))
}

games <- games %>%
  rowwise %>%
  mutate(shown = monte_show(prize, pick),
         stay = pick,
         switch = setdiff(doors, c(pick, shown)),
         strategy = factor(ifelse(prize == stay, 'stay', 'switch')))

print(summary(games$strategy) / nrow(games))

qplot(strategy, data = games, fill = strategy, geom = 'bar') + 
  xlab('Winning Strategy') +
  ggtitle('Monty Hall Problem Simulation')

#part 3
install.packages("meme")
library(meme)
u2 <- "http://i0.kym-cdn.com/entries/icons/mobile/000/000/745/success.jpg"
v <- meme(u2, "Did You", "Come To Office Hours")
plot(v)