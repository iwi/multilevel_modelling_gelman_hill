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
df <- data.frame(means = means_lift_fails)
ggplot(df, aes(x = means)) +
  geom_histogram()

# 3
