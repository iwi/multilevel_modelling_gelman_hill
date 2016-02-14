# Chapter 7.2 Summarizing linear regressions using simulation:
# an informal Bayesian approach

# Page 140

## Data are at http://www.stat.columbia.edu/~gelman/arm/examples/earnings

# Simulation to represent predicitve uncertainty

# Rebuilding here the model 4.2 from page 63

# dependencies
library(arm)
library(foreign)
library(dplyr)

# raw data
heights <- read.dta("heights.dta")

# Clean the data
# recode sex variable
heights$male <- 2 - heights$sex

# (for simplicity) remove cases with missing data
heights %>%
  filter(!is.na(earn),
         !is.na(height),
         !is.na(sex),
         earn > 0) %>%
  select(earn,
         height,
         male) %>%
  mutate(log.earn = log(earn)) ->
  heights.clean

## Model fit
earn.logmodel.3 <- with(heights.clean,
                        lm(log.earn ~
                          height +
                          male +
                          height:male))

################## start actual example

# Obtaining the point and interval predictions automatically
x.new <- data.frame(height = 68,
                    male = 1)

pred.interval <- predict(earn.logmodel.3,
                         x.new,
                         interval = "prediction",
                         level = 0.95)

exp(pred.interval)

# Now obtaining the predicitive interval using simulation

# or with exp -> 21,000 with sd 2.4

# The model:
disp <- display(earn.logmodel.3)
sd <- disp$sigma.hat

# The point estimate for log earnings is:
predict_log_earn <- function(height, male) {
  outcome <-
    earn.logmodel.3$coef[1] +
    earn.logmodel.3$coef[2] * height +
    earn.logmodel.3$coef[3] * male +
    earn.logmodel.3$coef[4] * height * male
  return(outcome)
}

predict_earn(68, 1)

# with a sd of:
sd  # 0.88

# If we undo the log
exp(predict_earn(68, 1))
exp(sd)

# 68% CI [21000/2.4, 21000 * 2.4]
# 95% CI [21000/2.4^2, 21000 * 2.4^2]

# The prediction
pred <- exp(rnorm(n = 1000,
                  mean = predict_log_earn(68, 1),
                  sd = sd))

# The outcome of the prediction ignoring uncertainty in the regression params
mean(pred)
median(pred)
quantile(pred, c(0.25, 0.75))  # 50% interval
quantile(pred, c(0.025, 0.075))  # 95% interval

# A more complex estimation where simulation is useful
# Estimating the difference between 68-inch-tall man and woman
# (just the difference of point estimates for each)

point_diff <- exp(predict_log_earn(68, 1)) - exp(predict_log_earn(68, 0))

# To get the uncertainty of the combination, simulation helps:
pred.man <- exp(rnorm(n = 1000,
                      mean = predict_log_earn(68, 1),
                      sd = sd))

pred.woman <- exp(rnorm(n = 1000,
                      mean = predict_log_earn(68, 0),
                      sd = sd))

pred.diff <- pred.man - pred.woman

pred.ratio <- pred.man / pred.woman

# In summary
hist(pred.man)
mean(pred.man)
hist(pred.woman)
mean(pred.woman)
hist(pred.diff)
mean(pred.diff)
median(pred.diff)
quantile(pred.diff, c(0.25, 0.75))
quantile(pred.diff, c(0.025, 0.975))

# Simulation to represent uncertainty in regression coefficients
# page 142

# Using sim() to include the coefficients uncertainty

n.sims <- 1000
sim.1 <- sim(earn.logmodel.3, n.sims)

# Check the structure
str(sim.1)

# Note that to extract the coefficients and sigma we need to use coef() and
# sigma.hat() -- the book's way doesn't work
str(coef(sim.1))
head(coef(sim.1)[,2])

# Verify that these simulations are equivalent to the regression computations
disp <- display(earn.logmodel.3)
disp$coef[3]
disp$se[3]  # which should be similar to:

mean(coef(sim.1)[,3])
sd(coef(sim.1)[,3])

# Let's look at the coefficient of height among men
# there is no direct way from the regression output to compute its se
# but can be easily done via simulation

beta.height <- coef(sim.1)[, 2]  # height coefficient
beta.height_men <- coef(sim.1)[, 4]  # height:men coefficient
height.for.men.coef <- beta.height + beta.height_men

# The 95% CI is:
quantile(height.for.men.coef, c(0.025, 0.975))







