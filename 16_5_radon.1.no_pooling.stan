// This is a model for a simple regression
data {
  int<lower=0> N;
  int<lower=0> J;
  vector[N] y;
  int<lower=0,upper=1> x[N];
  int<lower=1,upper=85> county[N];
}
parameters {
  real a[J];
  real b;
  real<lower=0> sigma_y;
}
model {
  for (j in 1:J)
    a[j] ~ normal(0, 1000);
  b ~ normal(0, 1000);
  sigma_y ~ uniform(0, 100);
  for (n in 1:N)
    y[n] ~ normal(a[county[n]] + b * x[n], sigma_y);
}  
