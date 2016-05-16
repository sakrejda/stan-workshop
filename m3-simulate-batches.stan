data {
  int M; // number of batches
  int N; // number of widgets per batch
  int K; // number of teams
  real mu;
  real<lower=0> sigma;
  real team_effects[K];
  simplex[K] team_weights;
}

parameters {
  real<lower=0, upper=1> z;    // Just to humor the tools.
}

model { }

generated quantities {
  int batch_team_index[M];
  int widget_batch_index[M*N];
  real y[N*M];
  for (m in 1:M) {
    real batch_mean;
    batch_team_index[m] <- categorical_rng(team_weights);
    batch_mean <- mu + team_effects[batch_team_index[m]];
    for (n in 1:N) {
      int i;
      i <- n+(m-1)*N;
      widget_batch_index[i] <- m;
      y[i] <- normal_rng(batch_mean, sigma);
    }
  }
}

