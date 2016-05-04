// This is a model for a simple regression
data {
  int<lower=0> N;
  int<lower=0> J;
  vector[N] y;
  int<lower=0,upper=1> x[N];
}
parameters {
  real a;
  real b;
  real<lower=0> sigma_y;
}
model {
  a ~ normal(0, 1000); // this is unnecessary, without this there is a default
  b ~ normal(0, 1000); //
  sigma_y ~ uniform(0, 100);
  for (n in 1:N)
    y[n] ~ normal(a + b * x[n], sigma_y);
}  
