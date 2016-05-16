data {
  int<lower=0> N;
  int<lower=0> J;
  vector[N] y;
  int<lower=0,upper=1> x[N];
  int county[N];
}
parameters {
  // These parameters contain prior knowledge. That is are defined with a
  // prior distribution in the model.
  real mu_a;
  real mu_b;
  real<lower=0> sigma_y;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
  matrix[J, 2] B_hat;
}
model {
  // These are the parameters built with parameters defined above
  matrix[2, 2] sigma_B;
  vector[J] a;
  vector[J] b;
  matrix[J, 2] B;

  // Description of the alpha (a) and beta (b) for each j in J as well as of
  // the beta matrix (B)
  for (j in 1:J) {
    a[j] ~ B[j, 1];
    b[j] ~ B[j, 2];
    B_hat[j, 1:2] ~ multi_normal(B_hat[j,], sigma_B[,]);
    B_hat[j, 1] = mu_a;
    B_hat[j, 2] = mu_b;
  }

  mu_a ~ normal(0, 1000);
  mu_b ~ normal(0, 1000);

  // Definition of the covariance matrix
  sigma_B[1, 1] ~ pow(sigma_a, 2);
  sigma_B[2, 2] ~ pow(sigma_b, 2);
  sigma_B[1, 2] ~ rho * sigma_a * sigma_b;
  sigma_B[2, 1] ~ sigma_B[1, 2];

  sigma_a ~ uniform(0, 100);
  sigma_b ~ uniform(0, 100);

  rho ~ uniform(-1, 1);

  // Definition of the higher level model
  for (n in 1:N)
    y[n] ~ normal(a[county[n]] + b[county[n]] * x[n], sigma_y);

  sigma_y ~ uniform(0, 100);
}
