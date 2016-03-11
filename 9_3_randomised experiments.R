# Chapter 9. Basic analyisis of a completely randomized experiment

# Page 157

# dependencies
library ("arm")

# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/electric.company
electric <- read.table("~/llibres/llibres_tecnics/regression_multilevel_gelman_hill_2006/ARM_Data/electric.company/electric.dat",
                     header=TRUE)
str(electric)

# Basic analysis of a completely randomized experiment
post.test <- with(electric,
                           c(treated.Posttest, control.Posttest))
pre.test <- with(electric,
                 c(treated.Pretest, control.Pretest))
graded <- as.factor(rep(electric$Grade, 2))
treatment <- rep(c(1, 0), rep(length (electric$treated.Posttest), 2))

resh_electric <- data.frame(
  post.test = post.test,
  pre.test = pre.test,
  grade = graded,
  treatment = treatment
)
g <- 1
n <- length(post.test)
subset_lm <- function(g) {
  lin_mod <- lm(post.test ~ treatment,
     subset = (graded == g))
}

for (k in 1:4) {
  print(paste("grade is ", k))
  display(subset_lm(k))
}

lm.1 <- subset_lm(1)
lm.2 <- subset_lm(2)
lm.3 <- subset_lm(3)
lm.4 <- subset_lm(4)

intercepts <- c(coef(lm.1)[1],
                coef(lm.2)[1],
                coef(lm.3)[1],
                coef(lm.4)[1])
slopes <- c(coef(lm.1)[2],
            coef(lm.2)[2],
            coef(lm.3)[2],
            coef(lm.4)[2])

coefficients <- data.frame(
  intercept = intercepts,
  slope = slopes,
  grade = as.factor(seq(1:4))
)

cre <- merge(resh_electric, coefficients)
View(cre)
library(dplyr)
library(ggplot2)
str(cre)
cre
# this plot needs to be fixed...
ggplot(cre,
       aes(x = pre.test,
       y = post.test)) +
  geom_point(aes(color = as.factor(treatment))) +
  geom_abline(intercept = mean(intercept),
              slope = mean(slope)) +
  theme_bw() +
  facet_wrap(~ grade)

