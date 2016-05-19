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
  real mu;
  real<lower=0> sigma;
  real team_effects_raw[K];
  real<lower=0> team_sigma;
}

transformed parameters {
  real batch_mean[M];
  for (m in 1:M) {
    batch_mean[m] <- mu + team_effects_raw[batch_team_index[m]]*team_sigma;
  }
}

model {
  mu ~ normal(0,5);
  team_effects_raw ~ normal(0,1);
  team_sigma ~ gamma(2,1);
  sigma ~ gamma(2,.1);
  for (i in 1:(M*N)) { 
    real mu_local;
    mu_local <- batch_mean[widget_batch_index[i]];
    increment_log_prob(normal_log(y[i], mu_local, sigma));
  }
}

generated quantities {
  real team_effects[K];
  vector[K] team_means;
  for (k in 1:K) {
    team_effects[k] <- team_effects_raw[k]*team_sigma;
  }
  team_means <- mu + to_vector(team_effects);
}

