data {
  int N; int K; int M;

  real y[N*M];
  int widget[N*M];
  int batch[N*M];
  int team[N*M];
}

parameters {
  real<lower=0> sigma;
  real team_means[K];
}

model {
  team_means ~ normal(0,5);
  sigma ~ gamma(2,.1);
  for (i in 1:(M*N)) { 
    real mu_local;
    mu_local <- team_means[team[i]];
    increment_log_prob(normal_log(y[i], mu_local, sigma));
  }
}

generated quantities {
  vector[K] team_effects;
  team_effects <- to_vector(team_means) - mean(team_means);
}

