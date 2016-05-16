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
  real x;   // keep shinystan happy.
}

model {
  x ~ normal(0,1);   // keep shinystan happy.
}

generated quantities {
  real y[N*M];
  for (i in 1:(M*N)) { 
    real mu_local;
    mu_local <- batch_mean[widget_batch_index[i]];
    y[i] <- student_t_rng(1, mu_local, sigma);
  }
}

