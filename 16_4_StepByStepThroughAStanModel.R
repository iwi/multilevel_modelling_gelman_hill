# 16 but with STAN instead of BUGS

library(rstan)
library(arm)
library(ggplot2)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

## Data
source("~/R/x86_64-pc-linux-gnu-library/3.2/rstan/include/example-models-master/ARM/Ch.16/radon.data.R", echo = TRUE)

# (a) Start by fitting classical regression using lm() function.

classical_pooled <- lm(y ~ x)
display(classical_pooled)

classical_unpooled <- lm(y ~ x + factor(county))
display(classical_unpooled)

classical_unpooled_no_intercept <- lm(y ~ x + factor(county) - 1)
display(classical_unpooled_no_intercept)

## PartiCall Stan from R
set.seed(1)
radon.data <- c("N", "J", "y", "x", "county")
radon.1.sf <- stan(file = '~/R/x86_64-pc-linux-gnu-library/3.2/rstan/include/example-models-master/ARM/Ch.16/radon.1.stan',
                   data = radon.data,
                   iter = 100,
                   chains = 4)

# Complete pooling with stan
radon.data <- c("N", "J", "y", "x")
radon.1.pooled.sf <- stan(file = '~/projectes/multilevel_modelling_gelman_hill/16_5_radon.1.complete_pooling.stan',
                   data = radon.data,
                   iter = 100,
                   chains = 4)

# No pooling with stan
radon.data <- c("N", "J", "y", "x", "county")
radon.1.no.pooling.sf <- stan(file = '~/projectes/multilevel_modelling_gelman_hill/16_5_radon.1.no_pooling.stan',
                   data = radon.data,
                   iter = 100,
                   chains = 4)




print(radon.1.sf)
plot(radon.1.sf, pars = c('a[1]', 'a[85]', 'mu_a', 'b', 'sigma_y', 'sigma_a'))#, 'lp__'))
print(radon.1.sf, digits = 1)

print(radon.1.pooled.sf)
plot(radon.1.pooled.sf, pars = c('a', 'b', 'sigma_y', 'lp__'))
print(radon.1.pooled.sf, digits = 1)

print(radon.1.no.pooling.sf)
plot(radon.1.no.pooling.sf, pars = c('a[1]', 'a[85]', 'b', 'sigma_y', 'lp__'))
print(radon.1.no.pooling.sf, digits = 1)

# Plot Figure 16.2
size = 30
mu_a.sample <- extract(radon.1.sf,
                       pars = "mu_a",
                       permuted = FALSE,
                       inc_warmup = T)  # T to see difference?
str(mu_a.sample)
View(mu_a.sample)

n.chains <- dim(mu_a.sample)[2]
value <- matrix(mu_a.sample[1:size, , 1],
                ncol = 1)
trace.ggdf <- data.frame(chain = rep(1:n.chains, each = size),
                         iteration = rep(1:size, n.chains),
                         value)
p1 <- ggplot(trace.ggdf) +
  geom_path(aes(x = iteration,
                y = value,
                group = chain)) +
  ylab(expression(mu[alpha]))
print(p1)
hist(trace.ggdf$value, breaks = seq(1.2, 1.7, 0.025))


## Accessing the simulations
sims_m <- extract(radon.1.sf)
a <- sims_m$a
b <- sims_m$b
sigma.y <- sims_m$sigma_y

sims_no_p <- extract(radon.1.no.pooling.sf)
a_no_p <- sims_no_p$a
b_no_p <- sims_no_p$b
sigma.y_no_p <- sims_no_p$sigma_y



# 90% CI for beta
quantile(b, c(0.05, 0.95))

# Prob. avg radon levels are higher in county 36 than in county 26
mean(a[,36] > a[,26])
mean(a_no_p[,36] > a_no_p[,26])

## Fitted values, residuals and other calculations
a.multilevel <- rep(NA, J)
for (j in 1:J) {
    a.multilevel[j] <- median(a[,j])
}
b.multilevel <- median(b)

y.hat <- a.multilevel[county] + b.multilevel * x
y.resid <- y - y.hat

qplot(y.hat, y.resid)

# numeric calculations
n.sims <- 100
lqp.radon <- rep(NA, n.sims)
hennepin.radon <- rep(NA, n.sims)
for (s in 1:n.sims) {
  lqp.radon[s] <- exp(rnorm(1, a[s,36] + b[s], sigma.y[s]))
  hennepin.radon[s] <- exp(rnorm(1, a[s,26] + b[s], sigma.y[s]))
}
radon.diff <- lqp.radon - hennepin.radon
p2 <- ggplot(data.frame(radon.diff),
             aes(x = radon.diff)) +
  geom_histogram(color = "black", fill = "gray", binwidth = 0.75)
print(p2)
print(mean(radon.diff))
print(sd(radon.diff))
