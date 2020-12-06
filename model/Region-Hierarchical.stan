data {
  int < lower =0> N;//the number of matches
  int < lower =0> J;//the number of teams in this region
  vector [J] y[N];//the performance score
}

parameters {
  real mu_p;
  real < lower =0 > sigma_p;
  vector [J] theta;
  real < lower =0 > sigma ;
}

model {
  // priors
  mu_p ~ normal (0, 10);
  sigma_p ~ inv_chi_square (0.1);
  
  for (j in 1:J){
    theta[j] ~ normal (mu_p, sigma_p);
  }
  sigma ~ inv_chi_square (0.1);

  // likelihood
  for (j in 1:J)
    y[,j] ~ normal (theta[j], sigma);
}

generated quantities {
  vector[J] ypred;
  vector[J*N] log_lik;
  // Compute predictive distribution (not needed here)
  for (j in 1:J)
    ypred[j] = normal_rng(theta[j], sigma);
  for (j in 1:J){
    for (n in 1:N ) {
      // n+(j-1)*N gives values between 1 and J*N
      log_lik[n + (j-1)*N] = normal_lpdf(y[n, j] | theta[j], sigma);
    }
  }
}
