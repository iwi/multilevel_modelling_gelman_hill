# Chapter 7.4 Predictive simulation for generalised linear models

# Page 148

# dependencies
library(arm)
library(foreign)
library(dplyr)

# Read in the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/arsenic
wells <- read.table("wells.dat")

# Logistic regression
fit.1 <- with(wells,
              glm(switch ~ dist, family = binomial(link = "logit")))

display(fit.1)

# Simulating the uncertainty in the estimated coefficients
# Figure 7.6 (a)
sim.1 <- sim(fit.1, n.sims = 1000)
plot(x = coef(sim.1)[, 1],
     y = coef(sim.1)[, 2],
     xlab = expression(beta[0]),
     ylab = expression(beta[1]),
     pch = 20)

# Figure 7.6 (b)
with(wells,
     plot(x = dist,
          y = switch,
          xlab="Distance (in meters) to the nearest safe well",
          ylab="Pr(switching)"))
for (s in 1:200){
  curve(invlogit(coef(sim.1)[s, 1] +
                 coef(sim.1)[s, 2] * x),
        col = "gray",
        add = TRUE) }
curve(invlogit(
  coef(fit.1)[1] +
  coef(fit.1)[2] * x),
  add = TRUE)

# Predictive simulation using the binomial distribution


