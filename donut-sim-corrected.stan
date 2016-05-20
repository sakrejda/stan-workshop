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
  matrix[2,2] J;
  real lad_J;
  r <- sqrt(x^2+y^2);
  r ~ normal(r_mu, r_sd);
  
  J[1,1] <- x/sqrt(x^2+y^2);
  J[1,2] <- y/sqrt(x^2+y^2);
  J[2,1] <- (x^2*(x^2+y^2)^(-3.0/2.0) - (x^2+y^2)^(-1.0/2.0)) / sqrt(1-(x^2)/(x^2+y^2));
  J[2,2] <- (x*y) / ((x^2+y^2)^(3.0/2.0) * sqrt(1-(x^2)/(x^2+y^2)));
  lad_J <- log(fabs(J[1,1]*J[2,2] - J[1,2]*J[2,1]));
  increment_log_prob(lad_J);
}



