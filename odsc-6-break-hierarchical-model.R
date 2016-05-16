## Still talking about widgets, this time let's estimate the
## variance among teams.  Let's call it a hierarchical model because
## it has all the right structure, but it only has one level
## so you could also call it a random effects model.

library(rstan); library(magrittr); library(dplyr); library(tidyr)
library(shinystan)

# We are simulating data for M batches of N quality measurements
# with a mean of mu and standard deviation of sigma from the 
# normal distribution. Any given batch can be produced by any
# of K teams
K <- 6
M <- 100
N <- 5
mu <- 3.52
sigma <- 7.11
team_effects <- seq(from=-2, to=2, length.out=K)

## Assign batch to team, with uneven probability, 
## make one team only have one data point:
good <- FALSE
while(!good) {
  batch_team_map <- data.frame(
    batch=1:M, 
    team=sample(x=1:K, size=M, replace=TRUE)
  )
  good <- all(1:K %in% batch_team_map[['team']])
}
team_1 <- which(batch_team_map[['team']] == 1)
batch_team_map[team_1,'team'] <- 2
batch_team_map[team_1[1],'team'] <- 1


## Assign widget to batch:
widget_batch_map <- data.frame(
  widget=1:(N*M),
  batch=sample(x=1:M, size=N*M, replace=TRUE)
)

## Again, simulate from this model:
m6 <- stan('simulate-batch-t-drama.stan', data=list(
  N=N, K=K, M=M, mu=mu, sigma=sigma, team_effects=team_effects,
  batch_team_index=batch_team_map[['team']],
  widget_batch_index=widget_batch_map[['batch']]),
  iter=100+500, warmup=500, chains=1)

## Assemble the data set:
data <- data.frame(
  y=rstan::extract(m6, pars='y')[['y']][100,,drop=TRUE],
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
m7 <- stan('estimate-batch-hierarchy.stan', 
  data=list(N=N, K=K, M=M, y=data[['y']], widget=data[['widget']],
    batch=data[['batch']], team=data[['team']], 
    batch_team_index=batch_team_map[['team']],
    widget_batch_index=widget_batch_map[['batch']]), 
    chains=5, iter=200+1000, warmup=1000)

data <- rstan::extract(m7)[['team_effects']] %>%
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
  aes(x=team_mean, y=batch), color='blue') +
facet_wrap( ~ team, ncol=1)


stop("STOPPING HERE ON PURPOSE.")
# Show data:
print(pl)

# Find team we have the least data on:
data %>% group_by(team) %>% summarise(n=n()) %>% ungroup() %>% arrange(as.numeric(team))

# Find estimate of sigma vs. team_sigma, show
# the semi-funnel (bivariate plot, x-axis: team_effects[1],
# y-axis team_sigma, semi-funnel points out the issue of
# estimating a team effect for a fixed team_sigma rather 
# than estimating a marginal of the joint distribution. 
# #BayesPays
launch_shinystan(m7)


print(pl_team_est)


