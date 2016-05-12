data {
  int<lower=0> N;
  int<lower=0> J;
  vector[N] y;
  int<lower=0,upper=1> x[N];
  int county[N];
  vector[J] u;
}
parameters {
  real a[J];
  real b[J];
  real g_0;
  real g_1;
  real mu_a;
  real mu_b;
  real<lower=0> sigma_y;
  real<lower=0> sigma_a;
}
model {
  for (j in 1:J)
    a[j] ~ normal(a_hat[j], sigma_a)
    b[j] ~ normal(b_hat[j], sigma_b);

  // we don't include mu_a, mu_b directly to prepare for future complexity
    
  a_hat[j] = mu_a
  b_hat[j] = mu_b
  mu_a ~ normal(0, 1000);
  mu_b ~ normal(0, 1000);
  sigma_a ~ uniform(0, 100);
  sigma_b ~ uniform(0, 100);

  for (n in 1:N)
    y[n] ~ normal(a[county[n]] + b[county[n]] * x[n], sigma_y);

}  
