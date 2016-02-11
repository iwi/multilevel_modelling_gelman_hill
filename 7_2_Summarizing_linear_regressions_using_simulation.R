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

earn.logmodel.3(68, 1)

pred <- exp(rnorm(1000, 9.95, 0.88))

