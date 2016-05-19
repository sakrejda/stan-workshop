data {
  int N; int K; int M;

  real y[N*M];
  int widget[N*M];
  int batch[N*M];
  int team[N*M];
}

parameters {
  real mu;
  real<lower=0> sigma;
  real team_effects[K];
  real<lower=0> team_sigma;
}

model {
  mu ~ normal(0,5);
  team_effects ~ normal(0,team_sigma);
  team_sigma ~ gamma(2,1);
  sigma ~ gamma(2,.1);
  for (i in 1:(M*N)) { 
    real mu_local;
    mu_local <- mu + team_effects[team[i]];
    increment_log_prob(normal_log(y[i], mu_local, sigma));
  }
}

generated quantities {
  vector[K] team_means;
  team_means <- mu + to_vector(team_effects);
}

