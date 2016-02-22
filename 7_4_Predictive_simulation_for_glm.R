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

# Compound models
# page 150

# Read earnings data from 6.7
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/earnings

heights <-
  read.dta("~/llibres/llibres_tecnics/regression_multilevel_gelman_hill_2006/ARM_Data/earnings/heights.dta",
           convert.factors = TRUE
  )

str(heights)

# Prepare the data
heights$male <- 2 - heights$sex
ok <- !is.na(heights$earn +
               heights$height +
               heights$male)
heights.clean <- as.data.frame(cbind(earn = heights$earn,
                                     height = heights$height,
                                     male = heights$male)[ok,])
str(heights.clean)
heights.clean$earn.pos <- ifelse(heights.clean$earn > 0, 1, 0)

# Model earnings in two steps
# First model if earnings are > 0
fit.1a <- with(heights.clean,
               glm(earn.pos ~
                     height +
                     male,
                   family = binomial(link = "logit")))
display(fit.1a)

# Then model log(earnings)
heights.clean$log.earn <- log(heights.clean$earn)
fit.1b <- with(heights.clean,
               lm(log.earn ~
                    height +
                    male,
                  subset = earn > 0))
display(fit.1b)

# First we simulate a 68y/o man ignoring uncertainty in
# the regression coefficients
x.new <- c(1, 68, 1)  # constant term = 1, height = 68, male = 1

n.sims <- 1000
# Point estimate of the probability
prob.earn.pos <- invlogit(coef(fit.1a) %*% x.new)

# Simulation
earn.pos.sim <- rbinom(n = n.sims,
                       size = 1,
                       prob = prob.earn.pos)
earn.sim <- ifelse(earn.pos.sim == 0,
                   0,
                   exp(rnorm(n = n.sims,
                             mean = coef(fit.1b) %*% x.new,
                             sd = sigma.hat(fit.1b))))

# More generally we can use simulated values of the coefficient estimates:
sim.1a <- sim(fit.1a, n.sims)
sim.1b <- sim(fit.1b, n.sims)
prob.earn.pos <- invlogit(coef(sim.1a) %*% x.new)
earn.pos.sim <- rbinom(n = n.sims,
                       size = 1,
                       prob = prob.earn.pos)
earn.sim <- ifelse(earn.pos.sim == 0,
                   0,
                   exp(rnorm(n = n.sims,
                             mean = coef(sim.1b) %*% x.new,
                             sd = sigma.hat(sim.1b))))


# Let's calculate the mean predicted earnings by height and sex






