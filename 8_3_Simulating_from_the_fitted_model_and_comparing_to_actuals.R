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
