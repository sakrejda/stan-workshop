data {
  int N; int K; int M;

  real y[N*M];
  int widget[N*M];
  int batch[N*M];
  int team[N*M];

  int batch_team_index[M];
  int widget_batch_index[N*M];
}


parameters {
  // real mu;
  real<lower=0> sigma;
  real team_effects[K];
}

transformed parameters {
  real batch_mean[M];
  for (m in 1:M) {
    // batch_mean[m] <- mu + team_effects[batch_team_index[m]];
    batch_mean[m] <- team_effects[batch_team_index[m]];
  }
}

model {
  // mu ~ normal(0,5);
  team_effects ~ normal(0,5);
  sigma ~ gamma(2,.1);
  for (i in 1:(M*N)) { 
    real mu_local;
    mu_local <- batch_mean[widget_batch_index[i]];
    increment_log_prob(normal_log(y[i], mu_local, sigma));
  }
}

