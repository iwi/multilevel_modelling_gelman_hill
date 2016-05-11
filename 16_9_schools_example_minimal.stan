data {
  int<lower=0> N;
  int<lower=0> J;
  vector[N] y;
  int<lower=0,upper=1> x[N];
  int <lower=1> school[N];
  vector[J] treatment;
}
parameters {
  real a[J];
  real b;
  real g_0;
  real g_1;
  real<lower=0> sigma_y;
  real<lower=0> sigma_a;
}
model {
  g_0 ~ normal(0, 1000)
  g_1 ~ normal(0, 1000)
  sigma_a ~ uniform(0, 100)
  sigma_y ~ uniform(0, 100)
  b ~ normal(0, 1000)
  for (j in 1:J)
    a[j] ~ normal(g_0 + g_1 * treatment[j], sigma_a);
  for (n in 1:N)
    y[n] ~ normal(a[school[n]] + b * x[n], sigma_y);
}  
