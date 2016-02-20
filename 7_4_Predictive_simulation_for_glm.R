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
n.sims <- 1000
X.tilde <- cbind(1, wells$dist)  # Using the modelled data
X.tilde <- cbind(1, rnorm(n = 10, mean = 40, sd = 20))  # Using a new set
n.tilde <- nrow(X.tilde)
y.tilde <- array(NA, c(n.sims, n.tilde))
for (s in 1:n.sims){
  p.tilde <- invlogit(X.tilde %*% coef(sim.1)[s,])
  y.tilde <- rbinom(n.tilde, 1, p.tilde)
}

# The simulated proportion of changes
summary(y.tilde)[4]
sum(y.tilde) / length(y.tilde)

# Predictive simulation using the latent logistic distribution
# build the logit function
logit <- function (a) {log(a/(1-a))}

n.sims <- 1000
X.tilde <- cbind(1, wells$dist)  # Using the modelled data
X.tilde <- cbind(1, rnorm(n = 10, mean = 40, sd = 20))  # Using a new set
n.tilde <- nrow(X.tilde)

y.tilde <- array(NA, c(n.sims, n.tilde))
for (s in 1:n.sims){
  # epsilon.tilde are independent errors that we add to the
  # linear predictor
  epsilon.tilde <- logit(runif(n = n.tilde,
                               min = 0,
                               max = 1))
  z.tilde <- X.tilde %*% coef(sim.1)[s,] + epsilon.tilde
  # and we convert to binary if z.tilde is positive
  y.tilde[s,] <- ifelse(z.tilde > 0, 1, 0)
}

summary(y.tilde)[4]

# Alternative using matrix algebra
epsilon.tilde <- array(logit(runif(n = n.sims * n.tilde,
                                   min = 0,
                                   max = 1)),
                        c(n.sims, n.tilde))
z.tilde <- coef(sim.1) %*% t(X.tilde) + epsilon.tilde
y.tilde <- ifelse(z.tilde > 0, 1, 0)

