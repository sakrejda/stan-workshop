## Still talking about widgets and the hierarchical model, but touch on
## non-centered parameterization.  Talk about parameterizations.
## Show funnel vs. inverted funnel.

## Using the original simulated batch data, we run a slightly
## modified Stan program to estimate team effects.
m10 <- stan('m7-estimate-nc-t-hierarchy.stan', 
  data=list(N=N, K=K, M=M, 
    y=slacking_data[['y']], 
    widget=slacking_data[['widget']],
    batch=slacking_data[['batch']], 
    team=slacking_data[['team']], 
    batch_team_index=slacking_batch_team_map[['team']],
    widget_batch_index=slacking_widget_batch_map[['batch']]), 
    chains=5, iter=1000+1000, warmup=1000, thin=20)

s10 <- rstan::extract(m10)

e10 <- s10[['team_means']] %>% data.frame(check.names=FALSE) %>% 
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
  data=left_join(slacking_data, e10, c('team','team_f')), 
  aes(x=y, y=batch_f, color=team_f), shape=2
) + geom_point(
  data=slacking_batch_stats, 
  aes(x=mean, y=batch_f, group=team_f), color="red", size=3
) + geom_errorbarh(
  data=right_join(e10, slacking_batch_stats, c('team','team_f')),
  aes(xmin=`team_10%`, x=team_mean, xmax=`team_90%`, 
      y=batch_f, group=team_f), color='blue'
) + geom_point(
  data=right_join(e10, slacking_batch_stats, c('team','team_f')),
  aes(x=team_mean, y=batch_f), color='blue'
) + geom_vline(xintercept=mu, color="blue") + 
    theme_minimal() + 
    facet_wrap( ~ team_f, ncol=1, drop=TRUE, 
      shrink=TRUE, scales='free_y')


# Find estimate of sigma vs. team_sigma, show
# the semi-funnel (bivariate plot, x-axis: team_effects[1],
# y-axis team_sigma, then show how semi-funnel disappears
# for team_effects[1], but reappears inverted for team_effects_raw[2]...
#
# The main point here is to show the "ALL team_effects" estimates and
# point out that the non-slacking teams effects are over-estimated, 
# the standard deviation parameter is bogus (huge), and the mean is
# also uncertain.
print(pl)

# Find team we have the least data on:
slacking_data %>% group_by(team) %>% summarise(n=n()) %>% 
  ungroup() %>% arrange(as.numeric(team))


# Estimate of sqrt(sigma^2 + batch_sigma^2) should be total 
# sigma from simulation.
simulation_sigma <- sqrt(sigma^2+batch_sigma^2)
estimate_sigma_7 <- s10[['sigma']] %>% quantile(probs=c(0.025,0.5,0.075))


launch_shinystan(m10)




