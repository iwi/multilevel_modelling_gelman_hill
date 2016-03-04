# Chapter 8.4 Using predictive simulation to check the fit of a time-series model
# Page 163

# Data are at http://www.stat.columbia.edu/~gelman/arm/unemployment/unemployment.dat
library ("arm")

unemployment <- read.table("~/llibres/llibres_tecnics/regression_multilevel_gelman_hill_2006/ARM_Data/unemployment/unemployment.dat",
                           header = TRUE)

glimpse(unemployment)
str(unemployment)[1]


n <- dim(unemployment)[1]
y <- unemployment$unemployed.pct

# Plot of the unemployment rate
plot_unemployment <- function(year, unemployment){
  par(mar = c(4, 4, 2, 2))
  plot(year,
       unemployment,
       type = "l",
       ylab = "unemployment",
       xlab = "year",
       yaxs="i",
       ylim = c(0, max(y) * 1.05),
       yaxt = "n",
       mgp = c(2, 0.5, 0),
       cex.axis = 1.2,
       cex.lab = 1.2)
  axis(2,
       c(0, 5, 10),
       paste(c(0, 5, 10), "%", sep = ""),
       mgp = c(2, 0.5, 0),
       cex.axis = 1.2)
}

plot_unemployment(unemployment$year, y)

y.lag <- c(NA, y[1:n - 1])
lm.lag <- lm(y ~ y.lag)
display(lm.lag)

# To examine the fit of the model we will simulate replicated
# data from the fitted model.

# Simulating replicated datasets using a point estimate of the fitted model
b.hat <- coef(lm.lag)
s.hat <- sigma.hat(lm.lag)

n.sims <- 100
y.rep <- array(NA, c(n.sims, n))
dim(y.rep)
for (s in 1:n.sims){
  y.rep[s, 1] <- y[1]
  for (t in 2:n){
    prediction <- c(1, y.rep[s, t - 1]) %*% b.hat
    y.rep[s, t] <- rnorm(1, prediction, s.hat)
  }
}

y.rep
plot_unemployment(unemployment$year, y.rep[1,])

# Including the uncertainty in the estimated parameters
lm.lag.sim <- sim(lm.lag, n.sims)

for (s in 1:n.sims){
  y.rep[s, 1] <- y[1]
  for (t in 2:n){
    prediction <- c(1, y.rep[s, t - 1]) %*% coef(lm.lag.sim)[t,]
    y.rep[s, t] <- rnorm(1, prediction, sigma.hat(lm.lag.sim)[t])
  }
}

y.rep
plot_unemployment(unemployment$year, y.rep[10,])
summary(t(y.rep))
unemployment$year

# plot from book code
par(mfrow=c(5,3), mar=c(4,4,2,2))
for (s in 1:15){
  plot (unemployment$year, y.rep[s,], type="l", ylab="unemployment", xlab="year", yaxs="i",
        ylim=c(0, max(y)*1.05), yaxt="n", mgp=c(2,.5,0),
        main=paste("simulation #", s, sep=""), cex.main=0.95)
  axis (2, c(0,5,10), paste (c(0,5,10), "%", sep=""), mgp=c(2,.5,0))
}


# Numerical model check
# Test evaluates the number of cases where the trend from i-1 to i
# is the same as from i-2 to i-1
# The book puts it like:
  # Looking carefully at Figure 8.7, we see one pattern in all these replicated data
  # that was not in the original data in 8.6, and that is a jaggedness, a level of short-
  #   term ups and downs that contrasts to the smoother appearance of the actual time
  # series.
  # To quantify this discrepancy, we define a test statistic that is the frequency of
  # “switches”—the number of years in which an increase in unemployment is immedi-
  #   ately followed by a decrease, or vice versa:
Test <- function (y){
  n <- length(y)
  y.lag <- c(NA, y[1:(n - 1)])
  y.lag2 <- c(NA, NA, y[1:(n - 2)])
  sum(sign(y - y.lag) != sign(y.lag - y.lag2), na.rm = TRUE)
}

n.sims <- 100
print(Test(y))
test.rep <- rep(NA, n.sims)
for (s in 1:n.sims){
  test.rep[s] <- Test(y.rep[s,])
}

print(paste(sum(test.rep > Test(y)), 'out of', n.sims, 'simulations
            had more TRUE tests than the original data'))
print(quantile(test.rep, c(0.025, 0.5, 0.975)))
