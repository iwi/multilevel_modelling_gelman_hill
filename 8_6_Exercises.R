# Chapter 8.6 Exercises

# Page 165

# 1
# (a)
n <- 100
x1 <- seq(1:n)
x2 <- rbinom(n, 1, 0.5)
y <- 3 + 0.1 * x1 + 0.5 * x2 + rt(n = n, df = 4, ncp = 5)

lm_normal <- lm(y ~ x1 + x2)
display(lm_normal)

b <- c(3, 0.1, 0.5)

b.hat <- coef(lm_normal)
b.se <- se.coef(lm_normal)

cover.68 <- abs(b - b.hat) < b.se

# (b)
n.fakes <- 1000
cover.68 <- array(NA, c(n.fakes, 3))
for (fkd in 1:n.fakes) {
  x1 <- seq(1:n)
  x2 <- rbinom(n, 1, 0.5)
  y <- 3 + 0.1 * x1 + 0.5 * x2 + rt(n = n, df = 4, ncp = 5)

  lm_normal <- lm(y ~ x1 + x2)
  b.hat <- coef(lm_normal)
  b.se <- se.coef(lm_normal)
  cover.68[fkd,] <- abs(b - b.hat) < b.se
}

summary(cover.68)

# (c)
cover.68 <- array(NA, c(n.fakes, 3))
for (fkd in 1:n.fakes) {
  x1 <- seq(1:n)
  x2 <- rbinom(n, 1, 0.5)
  y <- 3 + 0.1 * x1 + 0.5 * x2 + rt(n = n, df = 4, ncp = 5)

  lm_normal <- lm(y ~ x1 + x2)
  b.hat <- coef(lm_normal)
  b.se <- se.coef(lm_normal)
  cover.68[fkd,] <- abs(b - b.hat) < qt(p = 0.84, df = n - 4) * b.se
}

summary(cover.68)





