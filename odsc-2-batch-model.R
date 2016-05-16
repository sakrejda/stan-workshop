## Estimating batch means (e.g.-quality control scenario,
## we're manufacturing widgets, we'd like to know if the
## widget teams are keep quality up, so we measure quality 
## of each widget and evaluate the quality of a batch using
## its mean).  Let's call it a pre-hierarchical model because
## it has all the right structure but we don't add it yet.

library(rstan); library(magrittr); library(dplyr); library(tidyr)
library(shinystan)

# We are simulating data for M batches of N quality measurements
# with a mean of mu and standard deviation of sigma from the 
# normal distribution. Any given batch can be produced by any
# of K teams
K <- 4
M <- 30  
N <- 15
mu <- 3.52
sigma <- 7.11
team_effects <- seq(from=-2, to=2, length.out=K)

## Assign batch to team, with uneven probability.
batch_team_map <- data.frame(
  batch=1:M, 
  team=sample(x=1:K, size=M, replace=TRUE, 
    prob=(1:K)^2)  #### UNEVEN probabilities!!
)

## Assign widget to batch:
widget_batch_map <- data.frame(
  widget=1:(N*M),
  batch=sample(x=1:M, size=N*M, replace=TRUE)
)

## Again, simulate from this model:
m1 <- stan('simulate-batch-drama.stan', data=list(
  N=N, K=K, M=M, mu=mu, sigma=sigma, team_effects=team_effects,
  batch_team_index=batch_team_map[['team']],
  widget_batch_index=widget_batch_map[['batch']]),
  iter=100+500, warmup=500, chains=1)

## Assemble the data set:
data <- data.frame(
  y=rstan::extract(m1, pars='y')[['y']][100,,drop=TRUE],
  widget_batch_map) %>% left_join(batch_team_map)

## Calculate batch and team statistics.
batch_stats <- data %>% group_by(team, batch) %>% 
  summarise(mean=mean(y), sigma=sd(y)) %>% ungroup() %>%
  arrange(batch)

team_stats <- data %>% group_by(team) %>%
  summarise(mean=mean(y), sigma=sd(y))

## Plot summaries:
#  x-axis: simulated value.
#  y-axis: data set index (there should be M of them).
#  red dots: data set mean.
#  blue line: true mean.
#  color: team index.
pl <- ggplot(data=data, 
  aes(x=y, y=batch, color=factor(team))) + 
  geom_point(shape=2) + 
  geom_point(data=batch_stats, aes(x=mean, y=batch), 
    color="red", size=3) +
  geom_vline(xintercept=mu, color="blue") + theme_minimal()

## Let's get the batch means and their credible
## intervals (80%) as our estimates:
m2 <- stan('estimate-batch-drama.stan', 
  data=list(N=N, K=K, M=M, y=data[['y']], widget=data[['widget']],
    batch=data[['batch']], team=data[['team']], 
    batch_team_index=batch_team_map[['team']],
    widget_batch_index=widget_batch_map[['batch']]), 
    chains=5, iter=200+1000, warmup=1000)

data <- rstan::extract(m2)[['team_effects']] %>%
  data.frame(check.names=FALSE) %>% mutate(iteration=1:nrow(.)) %>% 
  gather(team, quality, -iteration) %>% group_by(team) %>%
  summarise(`team_10%`=quantile(quality,.1), team_mean=mean(quality), 
    `team_90%`=quantile(quality,.9)) %>% 
  right_join(data %>% mutate(team=as.character(team)),
    by='team')


## Plot estimates:
#  x-axis: simulated value.
#  y-axis: data set index (there should be M of them).
#  red dots: data set mean.
#  blue line: true mean.
pl_team_est <- pl + geom_errorbarh(
  data=data,
  aes(xmin=`team_10%`, x=team_mean, xmax=`team_90%`, y=batch), color='blue') +
geom_point(
  data=data,
  aes(x=team_mean, y=batch), color='blue')


stop("STOPPING HERE ON PURPOSE.")
print(pl)
launch_shinystan(m2)
print(pl_team_est)


