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
  real<lower=-pi()/2, upper=pi()/2> team_effects_raw[K];
  real<lower=0> team_sigma;
}

transformed parameters {
  real batch_mean[M];
  for (m in 1:M) {
    batch_mean[m] <- mu + tan(team_effects_raw[batch_team_index[m]])*team_sigma;
  }
}

model {
  mu ~ normal(0,5);
  // implied: team_effects_raw ~ uniform(-.5*pi(),.5*pi);
  team_sigma ~ gamma(2,.1);
  sigma ~ gamma(2,.1);
  for (i in 1:(M*N)) { 
    real mu_local;
    mu_local <- batch_mean[widget_batch_index[i]];
    increment_log_prob(normal_log(y[i], mu_local, sigma));
  }
}

generated quantities {
  real team_effects[K];
  for (k in 1:K) {
    team_effects[k] <- tan(team_effects_raw[k])*team_sigma;
  }
}

