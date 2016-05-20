data {
  real r_mu;
  real r_sd;
}

parameters {
  real x;
  real y;
}

model {
  real r;
  r <- sqrt(x^2+y^2);
  r ~ normal(r_mu, r_sd);
}



