# Chapter 11

# Repeated measurements, time-series cross sections, and other
# non-nested structures

# Page 241

# dependencies
library ("arm")

# Data are at http://www.stat.columbia.edu/~gelman/arm/examples/electric.company
smoking2a <- read.table("~/llibres/llibres_tecnics/regression_multilevel_gelman_hill_2006/ARM_Data/smoking/smoke2a.dat",
                     header=TRUE)
smoking2b <- read.table("~/llibres/llibres_tecnics/regression_multilevel_gelman_hill_2006/ARM_Data/smoking/smoke2b.dat",
                        header=TRUE)
smoking2c <- read.table("~/llibres/llibres_tecnics/regression_multilevel_gelman_hill_2006/ARM_Data/smoking/smoke2c.dat",
                        header=TRUE)
smoking_pub <- read.table("~/llibres/llibres_tecnics/regression_multilevel_gelman_hill_2006/ARM_Data/smoking/smoke_pub.dat",
                        header=TRUE)

# smoking pub is the data that has already been prepared to be analysed
summary(smoking_pub)


