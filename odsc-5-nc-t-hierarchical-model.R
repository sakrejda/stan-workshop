## Still talking about the hierarchical model, this time with a 
## cauchy-distributed random effect, touch on centered and
## non-centered parameterization.  Talk about parameterizations.
## Re-using data from odsc-3!!! Show funnel vs. inverted funnel.
## Show heavier tails ("explore in shinystan") on team_effects[1].

## Using the original simulated batch data, we run a slightly
## modified Stan program to estimate team effects.
m7 <- stan('m7-estimate-nc-hierarchy.stan', 
  data=list(N=N, K=K, M=M, y=data[['y']], widget=data[['widget']],
    batch=data[['batch']], team=data[['team']], 
    batch_team_index=batch_team_map[['team']],
    widget_batch_index=widget_batch_map[['batch']]), 
    chains=5, iter=200+1000, warmup=1000)

s7 <- rstan::extract(m7)

e7 <- s7[['team_means']] %>% data.frame(check.names=FALSE) %>% 
  mutate(iteration=1:nrow(.)) %>% gather(team, quality, -iteration) %>% 
  group_by(team) %>% summarise(
    `team_10%`=quantile(quality,.1), 
     team_mean=mean(quality), 
    `team_90%`=quantile(quality,.9)) %>% 
  mutate(team_f = factor(team), team=as.numeric(team))

## Plot estimates:
#  x-axis: simulated value.
#  y-axis: data set index (there should be M of them).
#  red dots: data set mean.
#  blue line: true mean.
pl <- ggplot() + geom_point(
  data=left_join(data, e7, c('team','team_f')), 
  aes(x=y, y=batch_f, color=team_f), shape=2
) + geom_point(
  data=batch_stats, 
  aes(x=mean, y=batch_f, group=team_f), color="red", size=3
) + geom_errorbarh(
  data=right_join(e7, batch_stats, c('team','team_f')),
  aes(xmin=`team_10%`, x=team_mean, xmax=`team_90%`, 
      y=batch_f, group=team_f), color='blue'
) + geom_point(
  data=right_join(e7, batch_stats, c('team','team_f')),
  aes(x=team_mean, y=batch_f), color='blue'
) + geom_vline(xintercept=mu, color="blue") + 
    theme_minimal() + 
    facet_wrap( ~ team_f, ncol=1, drop=TRUE, 
      shrink=TRUE, scales='free_y')


stop("STOPPING HERE ON PURPOSE.")

# Find team we have the least data on:
data %>% group_by(team) %>% summarise(n=n()) %>% ungroup() %>% arrange(as.numeric(team))

# Find estimate of sigma vs. team_sigma, show
# the semi-funnel (bivariate plot, x-axis: team_effects[1],
# y-axis team_sigma, then show how semi-funnel disappears
# for team_effects[1], but reappears inverted for team_effects_raw[2]...
launch_shinystan(m7)

print(pl)

# Estimate of sqrt(sigma^2 + team_sigma^2) should be total 
# sigma from simulation.
