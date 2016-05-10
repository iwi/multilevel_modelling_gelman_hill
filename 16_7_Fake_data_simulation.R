# 16.7 - Fake data simulation

library(rstan)
library(arm)
library(ggplot2)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## Data
source("~/R/x86_64-pc-linux-gnu-library/3.2/rstan/include/example-models-master/ARM/Ch.16/radon.data.R", echo = TRUE)

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
for (j in 1:J){
  a_true[j] <- rnorm(1, g_0_true + g_1_true * u[j], sigma_a_true)
}

median(a_true)
# Simulate the fake dataset
n <- 919
y_fake <- rep(NA, n)
for (i in 1:n) {
  y_fake[i] <- rnorm(1, a_true[county[i]] + b_true * x[i], sigma_y_true)
}
str(y_fake)

## Run stan model
set.seed(1)
radon.data <- c("N", "J", "y_fake", "x", "county", "u")
radon.fake <- stan(file = '~/projectes/multilevel_modelling_gelman_hill/16_7_fake_data_model.stan',
                   data = radon.data,
                   iter = 200,
                   chains = 4)

print(radon.fake, digits = 1)
plot(radon.fake, pars = c('a[1]', 'a[85]', 'b', 'g_0','g_1', 'sigma_y', 'sigma_a'))#, 'lp__'))
sims.fake <- extract(radon.fake)
sims.fake

median(sims.fake$b)
median(sims.fake$b - b_true)
median(sims.fake$g_0)
median(sims.fake$g_0 - g_0_true)
median(sims.fake$g_1)
median(sims.fake$g_1 - g_1_true)
median(sims.fake$sigma_y)
median(sims.fake$sigma_y - sigma_y_true)
median(sims.fake$sigma_a)
median(sims.fake$sigma_a - sigma_a_true)


qplot(y_fake, geom = 'histogram')
qplot(y, geom = 'histogram')
