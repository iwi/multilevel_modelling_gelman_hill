# 16.7 - Fake data simulation

library(rstan)
library(arm)

# Set the true parameters
# we have taken the rounded parameters from one of the models from 16.5
b_true <- -0.7
g_0_true <- 1.5
g_1_true <- 0.7
sigma_y_true <- 0.8
sigma_a_true <- 0.2

# Simulate the varying parameters based on the true parameters
J <- 85
a_true <- rep(NA, J)
a_true <- rnorm(g_0_true + g_1_true * u, sigma_a_true)

# Simulate the fake dataset
n <- 1000
y_fake <- rep(NA, n)
y_fake <- rnorm(n, a_true[county] + b_true * x, sigma_y_true)
str(y_fake)

