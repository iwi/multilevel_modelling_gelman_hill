data {
  int<lower=0> N;
  int<lower=0> J;
  vector[N] y;
  int<lower=0,upper=1> x[N];
  int county[N];
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
    a[j] ~ B[j, 1]
    b[j] ~ B[j, 2]
    B.hat[j, 1:2] ~ normal(B.hat[j,], sigma_B[,])
    B.hat[j, 1] = mu_a
    B.hat[j, 2] = mu_b;
    
  mu_a ~ normal(0, 1000);
  mu_b ~ normal(0, 1000);
  sigma_B[1, 1] ~ uniform(0, 100);
  sigma_B[2, 2] ~ uniform(0, 100);
  sigma_B[1, 2] ~ rho * sigma_B[1, 1] * sigma_B[2, 2];
  sigma_B[2, 1] ~ sigma_B[1, 2] 
  rho ~ uniform(-1, 1)

  for (n in 1:N)
    y[n] ~ normal(a[county[n]] + b[county[n]] * x[n], sigma_y);

}  
