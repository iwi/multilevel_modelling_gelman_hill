# Chapter 12.2

# Repeated measurements, time-series cross sections, and other
# non-nested structures

# Page 253

# dependencies
library ("arm")
library("dplyr")

# get radon data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/radon
srrs2 <- read.table ("~/llibres/llibres_tecnics/regression_multilevel_gelman_hill_2006/ARM_Data/radon/srrs2.dat",
                     header = TRUE,
                     sep = ",")

as_data_frame(srrs2)
summary(srrs2)
glimpse(srrs2)


# create extra variables for the model
mn <- srrs2$state == "MN"  # Minessota indicator
radon <- srrs2$activity[mn]
log.radon <- log(ifelse(radon == 0, 0.1, radon))
floor <- srrs2$floor[mn]       # 0 for basement, 1 for first floor
n <- length(radon)
y <- log.radon
x <- floor

# create county sequential code
county.name <- as.vector(srrs2$county[mn])
uniq <- unique(county.name)
J <- length(uniq)
county <- rep(NA, J)
for (i in 1:J){
  county[county.name == uniq[i]] <- i
}


## Varying-intercept model w/ no predictors
M0 <- lmer(y ~ 1 + (1 | county))
display(M0)

## Including x as a predictor
M1 <- lmer(y ~ x + (1 | county))
display(M1)

# estimated regression coefficicents
coef(M1)

# fixed and random effects
fixef(M1)
re <- ranef(M1)
head(re$county$`(Intercept)`)
re$county$`(Intercept)`[1:10]

# uncertainties in the estimated coefficients
se.fixef(M1)
se_re <- se.ranef(M1)
se_re$county[1:10]
str(se_re$county)

# 95% CI for the slope
fixef(M1)["x"] + c(-2, 2) * se.fixef(M1)["x"]
# or
fixef(M1)[2] + c(-2, 2) * se.fixef(M1)[2]

# 95% CI for the intercept in county 26
coef(M1)$county[26, 1] + c(-2, 2) * se.ranef(M1)$county[26]

# 95% CI for the error in the intercept in county 26
as.matrix(ranef(M1)$county)[26] + c(-2, 2) * se.ranef(M1)$county[26]

# to plot Figure 12.4
a.hat.M1 <- coef(M1)$county[, 1]                # 1st column is the intercept
b.hat.M1 <- coef(M1)$county[, 2]                # 2nd element is the slope

x.jitter <- x + runif(n, -0.05, 0.05)
display8 <- c(36, 1, 35, 21, 14, 71, 61, 70)  # counties to be displayed
y.range <- range(y[!is.na(match(county,display8))])

par(mfrow = c(2, 4))
for (j in display8){
  plot (x.jitter[county==j], y[county==j], xlim=c(-.05,1.05), ylim=y.range,
        xlab="floor", ylab="log radon level", main=uniq[j],cex.lab=1.2,
        cex.axis=1.1, pch=20, mgp=c(2,.7,0), xaxt="n", yaxt="n", cex.main=1.1)
  axis (1, c(0,1), mgp=c(2,.7,0), cex.axis=1)
  axis (2, c(-1,1,3), mgp=c(2,.7,0), cex.axis=1)
  curve (coef(lm.pooled)[1] + coef(lm.pooled)[2]*x, lty=2, col="gray10", add=TRUE)
  curve (coef(lm.unpooled)[j+1] + coef(lm.unpooled)[1]*x, col="gray10", add=TRUE)
  curve (a.hat.M1[j] + b.hat.M1[j]*x, lwd=1, col="black", add=TRUE)
}

## Multilevel model ests vs. sample size (plot on the right on figure 12.3)
a.se.M1 <- se.coef(M1)$county

par (mar=c(5,5,4,2)+.1)
plot (sample.size.jittered, t(a.hat.M1), cex.lab=1.2, cex.axis=1.1,
      xlab="sample size in county j", ylab=expression (paste
                                                       ("est. intercept, ", alpha[j], "   (multilevel model)")),
      pch=20, log="x", ylim=c(.15,3.5), yaxt="n", xaxt="n")
axis (1, c(1,3,10,30,100), cex.axis=1.1)
axis (2, seq(0,3), cex.axis=1.1)
for (j in 1:J){
  lines (rep(sample.size.jittered[j],2),
         as.vector(a.hat.M1[j]) + c(-1,1)*a.se.M1[j], lwd=.5, col="gray10")
}
abline (coef(lm.pooled)[1], 0, lwd=.5)

