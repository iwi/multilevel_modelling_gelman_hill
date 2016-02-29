# Chapter 8.1 Fake data simulation

# Page 155

# Create fake data for a linear regression:
# initial x = 1, 2, 3, 4, 5

a <- 1.4
b <- 2.3
sigma <- 0.9
x <- 1:5
n <- length(x)

y <- a + b * x + rnorm(n, 0, sigma)


# Now we build a model based on the data x and the outcomes y
lm.1 <- lm(y ~ x)
display(lm.1)

# We check whether the true coefficient b is in the interval
# we calculated

b.hat <- coef(lm.1)[2]
b.se <- se.coef(lm.1)[2]

cover.68 <- abs(b - b.hat) < b.se
cover.95 <- abs(b - b.hat) < 1.96 * b.se

cat(paste("68% coverage: ", cover.68, "\n"))
cat(paste("95% coverage: ", cover.95, "\n"))

n.fake <- 1000
cover.68 <- rep(NA, n.fake)
cover.95 <- rep(NA, n.fake)

for (s in 1:n.fake){
  y <- a + b * x + rnorm(n, 0, sigma)
  lm.1 <- lm(y ~ x)

  b.hat <- coef(lm.1)[2]
  b.se <- se.coef(lm.1)[2]

  cover.68[s] <- abs(b - b.hat) < b.se
  cover.95[s] <- abs(b - b.hat) < 1.96 * b.se
}

# The intervals don't work because we're using a Normal distribution
# when whe should be using a t, as the sample size is small.
summary(cover.68)
summary(cover.95)
cat(paste("68% coverage: ", mean(cover.68), "\n"))
cat(paste("95% coverage: ", mean(cover.95), "\n"))

# With the t
for (s in 1:n.fake){
  y <- a + b * x + rnorm(n, 0, sigma)
  lm.1 <- lm(y ~ x)

  b.hat <- coef(lm.1)[2]
  b.se <- se.coef(lm.1)[2]

  cover.68[s] <- abs(b - b.hat) < qt(0.84, n - 2) * b.se
  cover.95[s] <- abs(b - b.hat) < qt(0.975, n - 2) * b.se
}

# And now it works
summary(cover.68)
summary(cover.95)
cat(paste("68% coverage: ", mean(cover.68), "\n"))
cat(paste("95% coverage: ", mean(cover.95), "\n"))


