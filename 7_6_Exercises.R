# 7.6 Exercices
# page 152

# 1
# (a)
prob.of.basket <- 0.6

shoot_in <- function(){
  return(rbinom(n = 1, 1, prob.of.basket) == 1)
}

play <- function() {
  ith <- 0
  double <- FALSE
  while (double == FALSE) {
    ith <- ith + 1
    if (!shoot_in()) {
      ith <- ith + 1
      if (!shoot_in()) {
        double <- TRUE
      }
    }
  }
  return(ith)
}

# (b)
games <- replicate(n = 1000, play())
hist(games)
summary(games)
sd(games)


# (c)
play2 <- function() {
  ith <- 0
  basket <- 0
  missed <- 0
  double <- FALSE
  while (double == FALSE) {
    ith <- ith + 1
    if (!shoot_in()) {
      ith <- ith + 1
      missed <- missed + 1
      if (!shoot_in()) {
        double <- TRUE
        missed <- missed + 1
      } else {
        basket <- basket + 1
      }
    } else {
      basket <- basket + 1
    }
  }
  return(list(ith = ith,
                    basket = basket,
                    missed = missed))
}

games <- data.frame(ind = seq(1:1000))
games$ith <- unlist(replicate(n = 1000, play2()[1]))
games$bsket <- unlist(replicate(n = 1000, play2()[2]))
games$missed <- unlist(replicate(n = 1000, play2()[3]))
hist(games$missed)

summary(games)

# 2
weight_man <- function(){
  return(exp(rnorm(n = 1, mean = 5.13, sd = 0.17)))
}

weight_woman <- function(){
  return(exp(rnorm(n = 1, mean = 4.96, 0.2)))
}

is_man <- function(){
  return(rbinom(1, 1, 0.48) == 1)
}

total_people_weight <- function(number_of_adults){
  total <- 0
  for (i in 1:number_of_adults){
    if (is_man()) {
      total <- total + weight_man()
    } else {
      total <- total + weight_woman()
    }
  }
  return(total)
}

max_lift_weight <- 1750
n.sims <- 1000

lift_fails_prob <- function(n.sims){
  lift_uses <- replicate(n.sims,
                         total_people_weight(10) > max_lift_weight)

  prob <- sum(lift_uses)/n.sims
  return(prob)
}

means_lift_fails <- replicate(n.sims, lift_fails_prob(1000))
hist(means_lift_fails)
summary(means_lift_fails)
df <- data.frame(means = means_lift_fails)
ggplot(df, aes(x = means)) +
  geom_histogram()

# 3
savings_per_unit <- function(){rnorm(1, 5, 4)}
size_of_market <- function(){rnorm(1, 40000, 10000)}

total_saved <- function(){
  return(savings_per_unit() * size_of_market())
  }

total_saved_dist <- replicate(1000, total_saved())
summary(total_saved_dist)
hist(total_saved_dist)

# 4

# 5
library("arm")
# Simulate linear data
size <- 100
# xs
happiness <- rbinom(size, 10, 0.7)
weight <- rnorm(size, mean = 65, sd = 10)
noise <- rnorm(size, 0, 5)

data <- data.frame(happiness = happiness,
                   weight = weight,
                   noise = noise)


# y
data$success <- happiness * 5 - weight * 3 + noise

fit_test <- lm(success ~ happiness + weight,
               data = data)
display(fit_test)

sim_test <- sim(fit_test, 1000)

happ_f <- 12
weight_f <- 75
data_point <- array(c(1, happ_f, weight_f))

regress <- function(models, data_point){
  success <- coef(models) %*% data_point
  return(success)
}

simulations <- regress(sim_test, data_point)
regress(fit_test, data_point)

hist(simulations)
sd(simulations)
mean(simulations)
quantile(simulations, c(0.025, 0.975))

# 6
# create data
str(data)

data$happ_and_weight <- with(data, happiness * 0.05 - weight * 0.1 + noise)

# invlogit to convert to probability
# {
#   1/(1 + exp(-x))
# }

data$success_prob <- invlogit(data$happ_and_weight)

with(data, plot(happ_and_weight, success_prob))

rbinom_1_1 <- function(p){rbinom(1, 1, p)}

data$success_bin <- sapply(data$success_prob, FUN = rbinom_1_1)

with(data, plot(success_bin, success_prob))


mean(data$happiness)
sum(data$success_bin)

fit_test_2 <- with(data,
                glm(success_bin ~ happiness + weight,
                    family = binomial(link = "logit"))
)
display(fit_test_2)

coef(fit_test_2)
plot(data$weight, data$success_bin)
curve(invlogit(coef(fit_test_2)[1] + 7 * coef(fit_test_2)[2] + x * coef(fit_test_2)[3]),
      from = 0.5,
      lwd = 0.5,
      add = TRUE)

sim_test_2 <- sim(fit_test_2, 1000)

data_point <- c(1, 6, 55)
regress <- function(model, data_point){
  success_prob <- invlogit(coef(model) %*% data_point)
  return(success_prob)
}

success_probabilies <- regress(sim_test_2, data_point)

hist(success_probabilies)


