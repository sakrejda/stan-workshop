## Old scenario with a twist and a single model for multiple batches: 

## The twist: one of the teams
## just doesn't care anymore and while they've continued to
## produce widgets the quality just isn't there.
##
## Data: We would like to evaluate the teams on the quality of their
## manufacturing over time and also track particularly low/high
## quality batches, so in addition to the quality score for
## each widget we record: 
##         1) the quality score,
##         2) the widget id, 
##         3) the batch id, and 
##         4) the team id

library(rstan); library(shinystan)
library(magrittr); library(dplyr); library(tidyr)

# We are simulating data for M batches of N quality measurements
# with a mean of mu and standard deviation of sigma from the 
# normal distribution. Any given batch can be produced by any
# of K teams
K <- 10 
M <- 150  
N <- 5
mu <- 3.52
sigma <- 4.11
batch_sigma <- 3
team_effects <- seq(from=-2, to=2, length.out=K-1) %>%
  c(-16)
team_weights <- rep(1,K)/K 

## Again, simulate from this model:
m8 <- stan('m3-simulate-batches.stan', data=list(
  N=N, K=K, M=M, mu=mu, sigma=sigma, batch_sigma=batch_sigma,
  team_effects=team_effects,
  team_weights=team_weights),
  iter=100+500, warmup=500, chains=1)

s8 <- rstan::extract(m8)

## Assign batch to team, with uneven probability.
slacking_batch_team_map <- data.frame(
  batch=1:M, 
  team=s8[['batch_team_index']][100,,drop=TRUE]
)

## Assign widget to batch:
slacking_widget_batch_map <- data.frame(
  widget=1:(N*M),
  batch=s8[['widget_batch_index']][100,,drop=TRUE]
)

## Assemble the data set:
slacking_data <- data.frame(
  y=s8[['y']][100,,drop=TRUE],
  widget=1:(N*M)) %>%
  left_join(slacking_widget_batch_map, by='widget') %>% 
  left_join(slacking_batch_team_map, by='batch') %>%
  mutate(batch_f=factor(batch), team_f = factor(team))

## Calculate batch and team statistics.
slacking_batch_stats <- slacking_data %>% group_by(team, batch) %>% 
  summarise(mean=mean(y), sigma=sd(y)) %>% ungroup %>%
  mutate(batch_f=factor(batch), team_f=factor(team)) %>%
  arrange(batch)

slacking_team_stats <- slacking_data %>% group_by(team) %>%
  summarise(mean=mean(y), sigma=sd(y), se=sd(y)/sqrt(n())) %>% ungroup %>%
  mutate(team_f=factor(team))

## Plot summaries:
#  x-axis: simulated value.
#  y-axis: data set index (there should be M of them).
#  red dots: data set mean.
#  blue line: true mean.
#  color: team index.
pl <- ggplot(data=slacking_data, 
  aes(x=y, y=batch, color=factor(team))) + 
  geom_point(shape=2) + 
  geom_point(data=slacking_batch_stats, aes(x=mean, y=batch), 
    color="red", size=3) +
  geom_vline(xintercept=mu, color="blue") + theme_minimal() +
  facet_wrap( ~ team_f, ncol=1)


print(pl)

