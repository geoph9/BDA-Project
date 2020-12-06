data {
  int < lower =0> N;//the number of matches
  int < lower =0> J;//the number of teams 
  vector [J] y[N];//the performance score
}

parameters {
  vector [J] mu;
  vector < lower =0 >[J] sigma ;
}

model {
  for (j in 1:J){
    mu[j] ~ normal (0, 10);
    sigma [j] ~ inv_chi_square (0.1);
  }

  // likelihood
  for (j in 1:J)
    y[,j] ~ normal (mu[j], sigma [j]);
}

generated quantities {
  vector [J] ypred;
  // Compute predictive performance for each team
  for (j in 1:J)
    ypred[j] = normal_rng (mu [j] , sigma [j]);
}

