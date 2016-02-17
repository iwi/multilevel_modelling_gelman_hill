# Chapter 7.3 Simulation for nonlinear predictions:
# congressional elections

# Page 144

## Data are at http://www.stat.columbia.edu/~gelman/arm/examples/...

# dependencies
library(arm)
library(foreign)
library(dplyr)

## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/cong3

congress <- vector ("list", 49)
for (i in 1:49){
  year <- 1896 + 2 * (i - 1)
  file <- paste("cong3/", year, ".asc", sep="")
  data.year <- matrix(scan (file),
                      byrow = TRUE,
                      ncol = 5)
  data.year <- cbind(rep(year, nrow(data.year)),
                     data.year)
  congress[[i]] <- data.year
}

# Clean the data
i86 <- (1986 - 1896) / 2 + 1
cong86 <- congress[[i86]]
cong88 <- congress[[i86 + 1]]
cong90 <- congress[[i86 + 2]]

process_election <- function(election){
  v <- election[, 5] / (election[, 5] + election[, 6])
  bad <- election[, 5] == -9 | election[, 6] == -9
  v[bad] <- NA
  contested <- v > 0.1 & v < 0.9
  inc <- election[, 4]

  return(list(v = v,
              contested = contested,
              inc = inc))
}

outcome86 <- process_election(cong86)
outcome88 <- process_election(cong88)
outcome90 <- process_election(cong90)

str(outcome86)

# A function to add some jitter for plotting
jitt <- function (x, delta) {x + runif(length(x), -delta, delta)}

## Plot Figure 7.3

outcome88$v.hist <- with(outcome88,
                         ifelse (v < 0.1, 0.0001,
                                 ifelse (v > 0.9, 0.9999, v)))

hist(outcome88$v.hist,
     breaks = seq(0, 1, 0.05),
     xlab = "Democratic share of the two-party vote", ylab = "", yaxt = "n",
     cex.axis = 1.1, cex.lab = 1.1, cex.main = 1.2,
     main = "Congressional elections in 1988")

# Many elections were uncontested in 1986, we input 0.25 instead of 0 and
# 0.75 instead of 1. This intends to represent the proportion of votes received
# by the Democratic candidate, had the election actually been contested.
outcome86$v.adjusted <- with(outcome86,
                             ifelse(v < 0.1, 0.25,
                                    ifelse(v > 0.9, 0.75, v)))
outcome86$vote <- outcome86$v.adjusted[outcome88$contested]
outcome88$incumbency <- outcome88$inc[outcome88$contested]
outcome88$vote <- outcome88$v[outcome88$contested]

# Fitting the model
fit.88 <- lm(outcome88$vote ~ outcome86$vote + outcome88$incumbency)
display(fit.88)

## Figure 7.4
# 7.4 (a)
par(mfrow = c(1, 1))
par(pty = "s", mar = c(5, 5, 4, 1) + 0.1)
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), type = "n",
     xlab = "Democratic vote share in 1986",
     ylab = "Democratic vote share in 1988",
     cex.lab = 1)
abline(0, 1, lwd = 0.5)
j.v86 <- ifelse(outcome86$contested, outcome86$v, jitt(outcome86$v, 0.02))
j.v88 <- ifelse(outcome88$contested, outcome88$v, jitt(outcome88$v, 0.02))
points(j.v86[outcome88$inc == 0], j.v88[outcome88$inc == 0], pch = 1)
points(j.v86[outcome88$inc == 1], j.v88[outcome88$inc == 1], pch = 16)
points(j.v86[outcome88$inc == -1], j.v88[outcome88$inc == -1], pch = 4)
mtext("Raw data (jittered at 0 and 1)", line = 1, cex = 1.2)

# 7.4 (b)
par(pty = "s", mar = c(5, 5, 4, 1) + 0.1)
plot(0, 0, xlim = c(0, 1), ylim = c(0, 1), type = "n",
     xlab = "Democratic vote share in 1986",
     ylab = "Democratic vote share in 1988",
     cex.lab = 1)
abline(0, 1, lwd = 0.5)
v86.adjusted <- ifelse(outcome86$v < 0.1, 0.25,
                       ifelse(outcome86$v > 0.9, 0.75, outcome86$v))
vote.86 <- v86.adjusted[outcome88$contested]
vote.88 <- outcome88$v[outcome88$contested]
incumbency.88 <- outcome88$inc[outcome88$contested]

points(vote.86[outcome88$incumbency == 0], vote.88[outcome88$incumbency == 0], pch = 1)
points(vote.86[outcome88$incumbency == 1], vote.88[outcome88$incumbency == 1], pch = 16)
points(vote.86[outcome88$incumbency == -1], vote.88[outcome88$incumbency == -1], pch = 4)
mtext("Adjusted data (imputing 0's and 1's to .75)", line = 1, cex = 1.2)

# note that the mode has issues as can be seen by looking at the
# residuals plot ?? the jump between the average y-values just
# below and just above x = 0.5 is not completely fit by the incumbency88
# predictor.
res <- residuals(fit.88)[outcome88$contested]
plot(vote.88, res)

# Simulation for inferences and predictions of new data points
# page 146

# We prepare some simplified vars
incumbency.90 <- outcome90$inc
vote.88 <- outcome88$v

# We create a matrix of predictors X
n.tilde <- length(vote.88)
X.tilde <- cbind(rep(1, n.tilde),
                 vote.88,
                 incumbency.90)

# Then we simulate 1000 predictive simulations ...
n.sims <- 1000
sim.88 <- sim(fit.88, n.sims)

# and we calculate the prediction of the 435 districts for the 1000
# simulations
y.tilde <- array(data = NA,
                 dim = c(n.sims, n.tilde))
for (s in 1:n.sims){
  pred <- X.tilde %*% coef(sim.88)[s,]
  ok <- !is.na(pred)
  y.tilde[s, ok] <- rnorm(n = sum(ok),
                          mean = pred[ok],
                          sd = sigma.hat(sim.88)[s])
}

# Predictive simulation for a nonlinear function of new data

# we count the districts in which Dems win (> 0.5)
# These values **could not** have been measured from the estimates
# and uncertainties for the individual districts.
# NAs are not added to the totals
y.tilde2 <- ifelse(is.na(y.tilde), 0, y.tilde)
dems.tilde <- rowSums(y.tilde2 > 0.5)
summary(dems.tilde)
sd(dems.tilde)

# simulation and regression show similar results
summary(coef(sim.88))
display(fit.88)

# Alternative implementation:
pred.88 <- function(X.pred, lm.fit){
  n.pred <- dim(X.pred)[1]
  sim.88 <- sim(lm.fit, 1)
  ok <- !is.na(pred)
  y.pred <- replicate(NA, n.pred)
  y.pred[ok] <- rnorm(n = sum(ok),
                      mean = (X.pred %*% t(coef(sim.88)))[ok],
                      sd = sigma.hat(sim.88))
  return(y.pred)
}

y.tilde <- replicate(1000, pred.88(X.tilde, fit.88))
dems.tilde <- replicate(1000, pred.88(X.tilde, fit.88) > 0.5)
dems.tilde <- rowSums(y.tilde > 0.5, na.rm = TRUE)
summary(dems.tilde)
