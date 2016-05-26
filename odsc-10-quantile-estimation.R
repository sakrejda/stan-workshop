## Quantiles are often used to calculate estimates from
## Bayesian samples and they are often easily applied.
## Quantiles also have a few properties that make them
## a little touchy to use.  This script addresses one
## particular issue: commonly used 2.5%/97.5% quantile
## estimates are very noise when estimated from samples
## making it critical that end-users understand the estimate
## they are producing.  In general the tail weight of the
## distribution giving rise to the samples is critical 
## in determining the noise level, but here we do a 
## simulation study for the normal distribution and 
## the usual 2.5%/97.5% quantiles.

library(rstan); library(shinystan)  # 
library(dplyr); library(tidyr)

# We are simulating M samples {m:1<=M}, with N=m^2 draws
# per sample from a standard normal.
M <- 10^3
mu <- 0
sigma <- 1

s15 <- list()
for (m in 1:M) {
  s15[[m]] <- rnorm(m^2,0,1)
}

e15 <- sapply(s15, quantile, prob=0.975)

plot( x=(1:M)^2, y=e15/qnorm(0.975), ylim=c(0.9,1.1), pch='.');
abline(h=1); abline(v=10^3, col='red'); abline(v=10^4, col='red')




