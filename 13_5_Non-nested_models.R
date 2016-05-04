## Read the pilots data & define variables
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/pilots
library("arm")
pilots <- read.table ("~/llibres/llibres_tecnics/regression_multilevel_gelman_hill_2006/Book_Codes/Ch.13/pilots.dat",
                      header = TRUE)

str(pilots)

group.names <- as.vector(unique(pilots$group))
scenario.names <- as.vector(unique(pilots$scenario))
n.group <- length(group.names)
n.scenario <- length(scenario.names)
successes <- NULL
failures <- NULL
group.id <- NULL
scenario.id <- NULL
for (j in 1:n.group){
  for (k in 1:n.scenario){
    ok <- pilots$group == group.names[j] & pilots$scenario == scenario.names[k]
    successes <- c(successes, sum(pilots$recovered[ok] == 1, na.rm = TRUE))
    failures <- c(failures, sum(pilots$recovered[ok] == 0, na.rm = TRUE))
    group.id <- c(group.id, j)
    scenario.id <- c(scenario.id, k)
  }
}

successes
failures
group.id
scenario.id

y <- successes / (successes + failures)
y.mat <- matrix(y, n.scenario, n.group)
sort.group <- order(apply(y.mat, 2, mean))
sort.scenario <- order(apply(y.mat, 1, mean))

group.id.new <- sort.group[group.id]
scenario.id.new <- sort.scenario[scenario.id]
y.mat.new <- y.mat[sort.scenario, sort.group]

scenario.abbr <- c("Nagoya", "B'ham", "Detroit", "Ptsbgh", "Roseln", "Chrlt", "Shemya", "Toledo")

## Model fit
M1 <- lmer (y ~ 1 + (1 | group.id) + (1 | scenario.id))
display (M1)

## Plot figure 13.8
par(mfrow = c(1, 1))
image(y.mat.new,
      col = gray((1:11) / 12),
      xaxt = "n",
      yaxt = "n")
axis(2,
     seq(0, 1, length = n.group),
     group.names[sort.group],
     tck = 0,
     cex.axis = 1.2)
axis (3,
      seq(0, 1, length = n.scenario),
      scenario.abbr[sort.scenario],
      tck = 0,
      cex.axis = 1.2)
for (x in seq(0.5,n.group - 0.5, 1) / (n.group - 1)) {
  lines (c(-10,10),rep(x,2),col="white", lwd=.5)
}

for (x in seq(0.5,n.scenario - 0.5, 1) / (n.scenario - 1)) {
  lines (rep(x,2),c(-10,10),col="white", lwd=.5)
}

########################################################################################################
## Read the data
# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/earnings

# The R codes & data files should be saved in the same directory for
# the source command to work
library(foreign)
heights <- read.dta("~/llibres/llibres_tecnics/regression_multilevel_gelman_hill_2006/Book_Codes/Ch.13/heights.dta")

str(heights)

# define variables
age <- 90 - heights$yearbn                     # survey was conducted in 1990
age[age < 18] <- NA
age.category <- ifelse(age < 35, 1,
                       ifelse(age < 50, 2, 3))
eth <- ifelse(heights$race == 2, 1,
              ifelse(heights$hisp == 1, 2,
                     ifelse(heights$race == 1, 3, 4)))
male <- 2 - heights$sex

# (for simplicity) remove cases with missing data
ok <- !is.na(heights$earn + heights$height + heights$sex) & heights$earn > 0 & heights$yearbn > 25
heights.clean <- data.frame(earn = heights$earn,
                            height = heights$height,
                            sex = heights$sex,
                            race = heights$race,
                            hisp = heights$hisp,
                            ed = heights$ed,
                            age,
                            age.category,
                            eth,
                            male)[ok,]

head(heights.clean)

height.jitter.add <- runif(n, -0.2, 0.2)

# rename variables
y <- log(heights.clean$earn)
x <- heights.clean$height
n <- length(y)
n.age <- 3
n.eth <- 4
age <- heights.clean$age.category
eth <- heights.clean$eth

## Regression centering the predictors
x.centered <- x - mean(x)
x.centered.jitter <- x.jitter - mean(x)

M1 <- lmer (y ~ x.centered + (1 + x.centered | eth) + (1 + x.centered | age) +
   (1 + x.centered | eth:age))
display (M1)

# plot figure 13.10 ???????????

par (mfrow=c(3,4), mar=c(4,4,3,1), oma=c(1,1,2,1))
for (j in 1:3){
  for (k in 1:4){
     plot (x.jitter[age==j&eth==k], y[age==j&eth==k], xlab="height (inches from mean)",
       ylab="log earnings", cex.lab=1.2, cex.axis=1.2, pch=20, cex=.6, cex.main=1.5,
       xlim=range(x), ylim=range(y), mgp=c(2,.7,0), yaxt="n", main=paste(eth.label[k], ", ",
     age.label[j], sep=""))
     axis (2, seq(6,12,2))
     a.00 <- height.2$median$b[1,j,k]
     b.00 <- height.2$median$b[2,j,k]
     for (i in 21:40)
       curve (b[i,1,j,k] + b[i,2,j,k]*x, lwd=.5, col="gray", add=T)
     curve (a.00 + b.00*x, lwd=2, add=T)
  }
}



