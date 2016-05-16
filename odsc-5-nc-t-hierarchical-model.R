## Still talking about the hierarchical model, this time with a 
## cauchy-distributed random effect, touch on centered and
## non-centered parameterization.  Talk about parameterizations.
## Re-using data from odsc-3!!! Show funnel vs. inverted funnel.
## Show heavier tails ("explore in shinystan") on team_effects[1].


## Let's get the batch means and their credible
## intervals (80%) as our estimates:
m4 <- stan('estimate-batch-nc-t-hierarchy.stan', 
  data=list(N=N, K=K, M=M, y=data[['y']], widget=data[['widget']],
    batch=data[['batch']], team=as.numeric(data[['team']]), 
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
  aes(x=team_mean, y=batch), color='blue') +
facet_wrap( ~ team, ncol=1)


stop("STOPPING HERE ON PURPOSE.")

# Find team we have the least data on:
data %>% group_by(team) %>% summarise(n=n()) %>% ungroup() %>% arrange(as.numeric(team))

# Find estimate of sigma vs. team_sigma, show
# the semi-funnel (bivariate plot, x-axis: team_effects[1],
# y-axis team_sigma, then show how semi-funnel disappears
# for team_effects[1], but reappears inverted for team_effects_raw[2]...
launch_shinystan(m4)

