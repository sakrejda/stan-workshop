## New scenario and a single model for multiple batches: 

## Scenario: some sort of quality control situation where
## we're manufacturing widgets and each widget is scored 
## on quality after manufacturing.  Widgets are manufactured
## in batches and each batch is produced by a single 
## manufacturing team.
##
## Data: We would like to evaluate the teams on the quality of their
## manufacturing over time and also track particularly low/high
## quality batches, so in addition to the quality score for
## each widget we record: 
##         1) the quality score,
##         2) the widget id, 
##         3) the batch id, and 
##         4) the team id

## I'm calling this a pre-hierarchical model because
## it has all the right structure but we don't use
## that in the estimation.

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
team_weights <- ((1:K)^2) / sum((1:K)^2)


## Again, simulate from this model:
m3 <- stan('m3-simulate-batches.stan', data=list(
  N=N, K=K, M=M, mu=mu, sigma=sigma, team_effects=team_effects,
  team_weights=team_weights),
  iter=100+500, warmup=500, chains=1)

## Assign batch to team, with uneven probability.
batch_team_map <- data.frame(
  batch=1:M, 
  team=rstan::extract(m3)[['batch_team_index']][100,,drop=TRUE]
)

## Assign widget to batch:
widget_batch_map <- data.frame(
  widget=1:(N*M),
  batch=rstan::extract(m3)[['widget_batch_index']][100,,drop=TRUE]
)

## Assemble the data set:
data <- data.frame(
  y=rstan::extract(m3)[['y']][100,,drop=TRUE],
  widget=1:(N*M)) %>%
  left_join(widget_batch_map, by='widget') %>% 
  left_join(batch_team_map, by='batch') %>%
  mutate(team_f = factor(team))

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
  geom_vline(xintercept=mu, color="blue") + theme_minimal() +
  facet_wrap( ~ team_f, ncol=1)

## Let's get the batch means and their credible
## intervals (80%) as our estimates:
m4 <- stan('m4-estimate-batches.stan', 
  data=list(N=N, K=K, M=M, y=data[['y']], widget=data[['widget']],
    batch=data[['batch']], team=data[['team']], 
    batch_team_index=batch_team_map[['team']],
    widget_batch_index=widget_batch_map[['batch']]), 
    chains=5, iter=200+1000, warmup=1000)

data <- rstan::extract(m4)[['team_effects']] %>%
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
launch_shinystan(m4)
print(pl_team_est)


