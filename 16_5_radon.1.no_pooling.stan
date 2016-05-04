// This is a model for a simple regression
data {
  int<lower=0> N;
  int<lower=0> J;
  vector[N] y;
  int<lower=0,upper=1> x[N];
  int<lower=1,upper=85> county[N];
}
parameters {
  real a;
  real b;
  real<lower=0> sigma_y;
}
model {
  for (n in 1:N)
    y[n] ~ normal(a + b * x[n], sigma_y);
}  
