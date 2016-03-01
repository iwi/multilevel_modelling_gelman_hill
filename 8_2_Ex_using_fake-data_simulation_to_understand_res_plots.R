# Chapter 8.2 Fake data simulation

# Page 157

# dependencies
library ("arm")

# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/simulation
grades <- read.table("~/llibres/llibres_tecnics/regression_multilevel_gelman_hill_2006/ARM_Data/simulation/gradesW4315.dat",
                      header=TRUE)
str(grades)
midterm <- grades$Midterm
final <- grades$Final

lm.1 <- lm(final ~ midterm)
display(lm.1)

# Now let's build the residual plots
n <- length(final)
X <- cbind(rep(1, n), midterm)
predicted <- X %*% coef(lm.1)

lm.1_residuals <- residuals(lm.1)
# Which is the same as:
final - predicted

# Residuals vs. fitted values
# don't show any interesting trends
plot(predicted, lm.1_residuals,
     xlab = "predicted value",
     ylab = "residual",
     main = "Residuals vs. predicted values",
     pch = 20)
abline (0, 0, col = "gray", lwd = 0.5)

# Residuals vs. observed values
# show that there is a clear pattern, however this is
# expected as the residuals need to be independent from the
# x not from the y. The predicted are a linear combination of
# the x, the observed are not.
plot(final, lm.1_residuals,
     xlab="observed value",
     ylab="residual",
     main="Residuals vs. observeed values",
     pch=20)
abline (0, 0, col = "gray", lwd = 0.5)

# Now with fake data - let's simulate reasonable values
a <- 65
b <-  0.7
sigma <- 15
y.fake <- a + b * midterm + rnorm(n, 0, 15)

# we fit the model
lm.fake <- lm(y.fake ~ midterm)
predicted.fake <- X %*% coef(lm.fake)
resid.fake <- y.fake - predicted.fake
plot(predicted.fake, residuals(lm.fake),
     xlab = "predicted value",
     ylab = "residual",
     main = "Residuals vs. predicted values",
     pch = 20)
abline(0, 0, col = "gray", lwd = 0.5)

# the fitted() function is another way of getting the
# predicted values
summary(abs(fitted(lm.fake) - predicted.fake) < 0.001)
head(fitted(lm.fake))
head(predicted.fake)
