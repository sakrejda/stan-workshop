data {
  int N; int K; int M;
  real mu;
  real<lower=0> sigma;
  real team_effects[K];
  int batch_team_index[M];
  int widget_batch_index[N*M];
}

transformed data {
  real batch_mean[M];
  for (m in 1:M) {
    int team;
    team <- batch_team_index[m];
    batch_mean[m] <- mu + team_effects[team];
  }
}

parameters {
  real y[N*M];
}

model {
  for (i in 1:(M*N)) { 
    real mu_local;
    mu_local <- batch_mean[widget_batch_index[i]];
    increment_log_prob(normal_log(y[i], mu_local, sigma));
  }
}

