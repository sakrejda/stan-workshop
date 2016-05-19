## We continue with the widget scenario and use that
## data to estimate a team-level quality effect. We 
## don't bother estimating a batch effect (get to that
## later) although we could by ignoring one of the batches
## (setting a 'default' level to avoid confounding parameters.

# Run Stan to estimate team mean effects. We run 1000 iterations
# and five chains which is about right to get good estimates of 
# means and medians and barely enough to get extreme quantiles
# for 80-95% credible intervals.
m4 <- stan('m4-estimate-batches.stan', 
  data=list(N=N, K=K, M=M, y=data[['y']], widget=data[['widget']],
    batch=data[['batch']], team=data[['team']], 
    batch_team_index=batch_team_map[['team']],
    widget_batch_index=widget_batch_map[['batch']]), 
    chains=5, iter=200+1000, warmup=1000)


s4 <- rstan::extract(m4)

e4 <- s4[['team_means']] %>% data.frame(check.names=FALSE) %>% 
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
  data=left_join(data, e4, c('team','team_f')), 
  aes(x=y, y=batch_f, color=team_f), shape=2
) + geom_point(
  data=batch_stats, 
  aes(x=mean, y=batch_f, group=team_f), color="red", size=3
) + geom_errorbarh(
  data=right_join(e4, batch_stats, c('team','team_f')),
  aes(xmin=`team_10%`, x=team_mean, xmax=`team_90%`, 
      y=batch_f, group=team_f), color='blue'
) + geom_point(
  data=right_join(e4, batch_stats, c('team','team_f')),
  aes(x=team_mean, y=batch_f), color='blue'
) + geom_vline(xintercept=mu, color="blue") + 
    theme_minimal() + 
    facet_wrap( ~ team_f, ncol=1, drop=TRUE, 
      shrink=TRUE, scales='free_y')


stop("STOPPING HERE ON PURPOSE.")

# Find estimates of team effects, find estimate of sigma
# which should be the simulation sqrt(sigma^2+batch_sigma^2)
launch_shinystan(m4)

print(pl)


