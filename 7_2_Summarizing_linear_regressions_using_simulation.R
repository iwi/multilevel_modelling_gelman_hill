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
# The point estimate for log earnings is:
# 8.4 + 0.017 * 68 - 0.079 * 68 * 1 = 9.95
# with a sd of 0.88
# or with exp -> 21,000 with sd 2.4

display(earn.logmodel.3)
earn.logmodel.3$coef[2]

# 68% CI [21000/2.4, 21000 * 2.4]
# 95% CI [21000/2.4^2, 21000 * 2.4^2]

# The prediction
pred <- exp(rnorm(n = 1000,
                  mean = 9.95,
                  sd = 0.88))

# The outcome of the prediction ignoring uncertainty in the regression params
mean(pred)
median(pred)
quantile(pred, c(0.25, 0.75))  # 50% interval
quantile(pred, c(0.025, 0.075))  # 95% interval

# A more complex estimation where simulation is useful
# Estimating the difference between 68-inch-tall man and woman

# point estimate = 6900 (just the difference of point estimates for each)








