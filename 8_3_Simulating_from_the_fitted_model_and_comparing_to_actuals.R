# Chapter 8.3 Simulating from the fitted model and comparing to actual data

# Page 159

# EXAMPLE: Comparing data to replications from a fitted normall distribution

# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/lightspeed
library ("arm")

y <- scan("~/llibres/llibres_tecnics/regression_multilevel_gelman_hill_2006/ARM_Data/lightspeed/lightspeed.dat",
           skip=4)

hist(y)
str(y)

light <- lm(y ~ 1)
display(light)

# We simulate 1000 replicatons from the parameter in the fitted model
n.sims <- 1000
sim.light <- sim(light, n.sims)

# We create 1000 fake datasets of 66 observations each
n <- length(y)
y.rep <- array(data = NA, dim = c(n.sims, n))
dim(y.rep)
for (s in 1:n.sims){
  y.rep[s,] <- rnorm(n = n,
                     mean = coef(sim.light)[s],
                     sigma.hat(sim.light)[s])
}

# Comparing simulated datasets to the original data
par(mfrow = c(5, 4))
hist(y)
for (s in 1:19){
  hist(y.rep[s,])
}

# Test with min()
test <- function(y){min(y)}

test.rep <- rep(NA, n.sims)
for (s in 1:n.sims){
  test.rep[s] <- test(y.rep[s,])
}
par(mfrow = c(1,1))
hist(test.rep,
     xlim = range(test(y), test.rep))
lines(rep(test(y), 2), c(0, n))

# EXAMPLE: Zeroes in count data

# page 161

roach_data <- read.csv("~/llibres/llibres_tecnics/regression_multilevel_gelman_hill_2006/ARM_Data/roaches/roachdata.csv")
str(roach_data)
# '@y number of roaches caught in a set of traps
# '@roach1 pre-treatment roach level
# '@treatment
# '@senior
# '@log(exposure2) is the offset - see model 6.3 on page 111

glm.1 <- with(roach_data,
              glm(y ~
                    roach1 +
                    treatment +
                    senior,
                  family = poisson,
                  offset = log(exposure2)))
display(glm.1)

# Now we simulate new datasets that could be seen if the model were
# true and the study were performed again

n <- length(roach_data$y)
X <- with(roach_data,
          cbind(rep(1, n), roach1, treatment, senior))
y.hat <- roach_data$exposure2 * exp(X %*% coef(glm.1))
y.rep <- rpois(n, y.hat)
roachdata$y
# We compare the replicated data with the original
# and see that if the model was true there would be no appartments
# with zero roaches, whilst 36% are in that situation in the real
# data
print(mean(roach_data$y == 0))
print(mean(y.rep == 0))

# Comparing the data y to 1000 replicated datasets y.rep
n.sims <- 1000
sim.1 <- sim(glm.1, n.sims)
y.rep <- array(NA, c(n.sims, n))
for (s in 1:n.sims){
  y.hat <- roach_data$exposure2 * exp(X %*% coef(sim.1)[s,])
  y.rep[s,] <- rpois(n, y.hat)
}

Test <- function(y){mean(y == 0)}

test.rep <- rep(NA, n.sims)
for (s in 1:n.sims){
  test.rep[s] <- Test(y.rep[s,])
}

summary(test.rep)
# Which is way below 36%

# Checking the overdispersed model
glm.2 <- with(roach_data,
              glm(y ~
                    roach1 +
                    treatment +
                    senior,
                  family = quasipoisson,
                  offset = log(exposure2)))

# Same coefficients but much larger standard errors
display(glm.2)
display(glm.1)


# Comparing the data y to 1000 replicated datasets y.rep
n.sims <- 1000
sim.2 <- sim(glm.2, n.sims)
y.rep <- array(NA, c(n.sims, n))
for (s in 1:n.sims){
  y.hat <- roach_data$exposure2 * exp(X %*% coef(sim.2)[s,])
  overdisp <- summary(glm.2)$dispersion
  a <- y.hat / (overdisp - 1)
  y.rep[s,] <- rnegbin(n, y.hat, a)
}

Test <- function(y){mean(y == 0)}

test.rep <- rep(NA, n.sims)
for (s in 1:n.sims){
  test.rep[s] <- Test(y.rep[s,])
}

summary(test.rep)
quantile(test.rep, c(0.025, 0.975))

