data {
  int N;
  real mu;
  real<lower=0> sigma;
}

parameters {
  real y[N];
}

model {
  y ~ normal(mu, sigma);
}

