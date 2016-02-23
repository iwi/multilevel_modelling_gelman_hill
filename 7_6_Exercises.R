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
