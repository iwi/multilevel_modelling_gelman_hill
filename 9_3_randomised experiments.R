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
  lin_mod <- lm(post.test ~
                  treatment +
                  pre.test,
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

display(lm.1)


library(dplyr)
library(ggplot2)
str(resh_electric)

# this plot needs to be fixed...
ggplot(resh_electric,
       aes(x = pre.test,
           y = post.test)) +
  geom_point(aes(color = as.factor(treatment))) +
  geom_smooth(method = "lm",
              formula = y ~ x) +
  theme_bw() +
  facet_wrap(~ grade)

resh_electric$gain <- resh_electric$post.test - resh_electric$pre.test

lm.gain <- with(resh_electric,
                lm(gain ~ treatment))

display(lm.gain)


lm.gg <- with(resh_electric,
                lm(gain ~
                     treatment +
                     grade +
                     treatment * grade))

display(lm.gg)






# 9.4 Treatment interactions and postratification
# Interactions of treatment effect with pre-treatment inputs
lm.4.a <- with(resh_electric,
               lm(post.test ~ treatment,
                  subset = (grade == 4)))

display(lm.4.a)

lm.4.b <- with(resh_electric,
               lm(post.test ~
                    treatment +
                    pre.test,
                  subset = (grade == 4)))

display(lm.4.b)

lm.4.c <- with(resh_electric,
               lm(post.test ~
                    treatment +
                    pre.test +
                    treatment * pre.test,
                  subset = (grade == 4)))

display(lm.4.c)

lm.4.c.sim <- sim(lm.4.c, 1000)
str(lm.4.c.sim)

# Filter out those that are not in grade 4
resh_electric %>%
  filter(grade == 4) ->
  re4

# Take a look at the domain (80 - 125)
hist(re4$pre.test)

# Calculate the treatment effect with the linear model
re4$treatment_effect <-
  coef(lm.4.c)[2] +
  coef(lm.4.c)[4] * re4$pre.test

# Plot the pre-test outcome vs. the treatment effect and show (in gray)
# the uncertainty brought by the coefficients
p <- ggplot(re4,
            aes(x = pre.test,
                y = treatment_effect)) +
  geom_point() +
  theme_bw()

for (i in 1:500) {
  p <- p +  geom_abline(intercept = coef(lm.4.c.sim)[i, 2],
                          slope = coef(lm.4.c.sim)[i, 4],
                          colour = 'grey')
}
p
p <- p +  geom_abline(intercept = coef(lm.4.c)[2],
                        slope = coef(lm.4.c)[4],
                        colour = 'red')
p
