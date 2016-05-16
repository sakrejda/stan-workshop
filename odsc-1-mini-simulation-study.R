## The "Hello world" of simulation studies.  The simulation 
## script simulates from a normal with the usual mean-standard deviation
## parameterization, and each _iteration_ outputs one draw from
## this distribution.  The estimation script is very similar to
## the simulation script but 'mu', and 'sigma' move from the 
## data block to the parameter block, whereas the array of 'y'
## moves from the parameter block to the data block.  

library(rstan); library(magrittr); library(dplyr); library(tidyr)
library(shinystan)

# We are simulating M data sets of N data points with a
# mean of mu and standard deviation of sigma from the 
# normal distribution.
M <- 50  
N <- 15
mu <- 3.52
sigma <- 7.11

## This stan call uses the no-U-turn sampler (NUTS) to
## sample from a normal distribution.  This is complete
## overkill, but useful for thinking about Stan programs.
m1 <- stan('m1-simulate-normal.stan', data=list(N=N, mu=mu, sigma=sigma),
  iter=M+300, warmup=300, chains=1)

## Extract the data sets in ggplot style.
y <- rstan::extract(m1, pars='y') %>% data.frame %>% 
  mutate(group=1:n()) %>% gather(point, y, -group)

## Calculate group statistics.
group_stats <- y %>% group_by(group) %>% 
  summarise(mean=mean(y), sigma=sd(y))

## Plot summaries:
#  x-axis: simulated value.
#  y-axis: data set index (there should be M of them).
#  red dots: data set mean.
#  blue line: true mean.
pl <- ggplot(data=y, aes(x=y, y=group)) + 
  geom_jitter(shape=4) + 
  geom_point(data=group_stats, aes(x=mean, y=group), 
    color="red", size=3) +
  geom_vline(xintercept=mu, color="blue") + theme_minimal()

## Let's get the batch means and their credible
## intervals (80%) as our estimates, with our simple
## Stan program, we need to call this once for each data
## set whose mean we want to estimate.:
group_means <- matrix(data=NA, nrow=4, ncol=M)
rownames(group_means) <- c('group','10%','mu','90%')
y <- rstan::extract(m1, pars="y")[['y']]
for (m in 1:M) {
  m2 <- stan('m2-estimate-normal.stan', data=list(N=N, y=y[m,]), chains=1)
  group_means[1,m] <- m
  group_means[2:4,m] <- rstan::extract(m2, pars='mu')[['mu']] %>%
    quantile(probs=c(0.1,.5,.9))
  group_means[3,m] <- rstan::extract(m2, pars='mu')[['mu']] %>%
    mean
}

## Plot estimates:
#  x-axis: simulated value.
#  y-axis: data set index (there should be M of them).
#  red dots: data set mean.
#  blue line: true mean.
## The means mostly match up with the calculated batch means, and
## the 80% credible intervals cover the true mean for about 
## 80% of batches. That counts as success.
pl_est <- pl + geom_errorbarh(
  data=group_means %>% t %>% data.frame(check.names=FALSE),
  aes(xmin=`10%`, x=mu, xmax=`90%`, y=group), color='blue') +
geom_point(
  data=group_means %>% t %>% data.frame(check.names=FALSE),
  aes(x=mu, y=group), color='blue')

print(pl_est)


