library("ggplot2"); library("tidyr"); library("rstan"); library(dplyr)

r_mu = 8
r_sd = 2


#' bivariate donut
#' 
#' \theta ~ U(0,2*pi)
#' r ~ N(1,1/4)
#' 
#' x = f(\theta,r) = r*cos(\theta)
#' y = f(\theta,r) = r*sin(\theta)
#' 
#' r = f(x,y) = sqrt(x^2+y^2)
#'
#' theta = f(x,y) = arcsin(y/r) = arccos(x/r)
r_bivariate_donut <- function(n, r_mu=1, r_sd=r_mu/4) {
  thetas <- runif(n,0,2*pi)
  rs <- rnorm(n, r_mu, r_sd)
  x <- rs * cos(thetas)
	y <- rs * sin(thetas)
	return(cbind(x=x, y=y))
}

d_bivariate_donut <- function(x, y, r_mu=1, r_sd=r_mu/4, log=TRUE) {
  r <- sqrt(x^2+y^2)
#	theta <- asin(y/r)
# dunif(theta, 0,2*pi)
  cds <- dnorm(r,r_mu,r_sd,log=log)
  if (isTRUE(log)) 
		return(sum(cds))
	else
		return(prod(cds))
}

#' What does direct simulation do?
draw_donut <- data.frame(r_bivariate_donut(10^3, r_mu, r_sd))

pl_sim <- ggplot(data=draw_donut, aes(x=x, y=y)) + 
	geom_point() + geom_density2d() +
	theme_minimal() +   coord_fixed(xlim=c(-1.5*r_mu,1.5*r_mu), ylim=c(-1.5*r_mu,1.5*r_mu))

pl_sim_only_density <- ggplot(data=draw_donut, aes(x=x, y=y)) + 
	geom_density2d() +
	theme_minimal()

print(pl_sim)
print(pl_sim_only_density)


#' What does Gibbs do?
#' Later 
 

#' What does Metropolis do?

#' Proposal p(x,y|x*,y*)
r_metropolis_proposal <- function(xy, sd) {
  xy <- c(x=rnorm(1,xy['x'],sd), y=rnorm(1,xy['y'],sd))
  return(xy)
}

d_metropolis_proposal <- function(xy_star, xy, sd, log=TRUE) {
  d_xy <- c(dnorm(xy_star['x'],xy['x'],sd,log), dnorm(xy_star['y'],xy['y'],sd,log))
  if (isTRUE(log))
    d_xy <- sum(d_xy)
  else
    d_xy <- prod(d_xy)
  return(d_xy)
}


metropolis_acceptance <- function(xy, xy_star, theta, sd_proposal) {
  log_d_xy <- d_bivariate_donut(xy['x'], xy['y'], theta['r_mu'], theta['r_sd'], log=TRUE)
  log_d_xy_star <- d_bivariate_donut(xy_star['x'], xy_star['y'], theta['r_mu'], theta['r_sd'], log=TRUE)
  log_d_xy_proposal <- d_metropolis_proposal(xy, xy_star, sd_proposal, log=TRUE)
  log_d_xy_star_proposal <- d_metropolis_proposal(xy_star, xy, sd_proposal, log=TRUE)
  a_ratio <- exp((log_d_xy_star + log_d_xy_proposal) - (log_d_xy + log_d_xy_star_proposal))
  a_prob <- ifelse(a_ratio > 1, 1, a_ratio)
  return(a_prob)
}

donut_metropolis <- function(n_iter, xy_init, theta, sd_proposal) {
  samples <- matrix(data=NA, nrow=7, ncol=n_iter+1)
  rownames(samples) <- c('iter','x','y','r','s','log_d', 'accept')
  r <- sqrt(xy_init['x']^2+xy_init['y']^2)
  s <- acos(xy_init['x']/r)
  samples[,1] <- c(1,xy_init['x'], xy_init['y'], r, s, d_bivariate_donut(xy_init['x'],xy_init['y'], theta['r_mu'], theta['r_sd'], log=TRUE),NA)
  xy <- xy_init
  for(i in 2:n_iter+1) {
    xy_star <- r_metropolis_proposal(xy, sd_proposal)
    a_prob <- metropolis_acceptance(xy, xy_star, theta, sd_proposal)
    if (a_prob > runif(1)) {
      xy <- xy_star
      accept <- 1
    } else {
      accept <- 0
    }
    r <- sqrt(xy['x']^2+xy['y']^2)
    s <- acos(xy['x']/r)
    samples[,i] <- c(i, xy['x'],xy['y'],r,s,d_bivariate_donut(xy['x'], xy['y'], theta['r_mu'], theta['r_sd'], log=TRUE),accept) 
  }
  return(samples)
}

#' sample:
o <- donut_metropolis(200, c(x=-r_mu, y=0), c(r_mu=r_mu,r_sd=r_sd),2.5) %>% t %>% data.frame

#' define a x/y grid:
a_grid <- expand.grid(x=seq(-1.5*r_mu,1.5*r_mu,l=50), y=seq(-1.5*r_mu,1.5*r_mu,l=50)) %>% data.frame
a_grid[['log_d']] <- apply(a_grid, 1, function(row) d_bivariate_donut(row['x'], row['y'],r_mu, r_sd,log=FALSE))
pl_donut_density <- ggplot() + geom_contour(data=a_grid, aes(x=x, y=y, z=log_d)) + geom_raster(data=a_grid, aes(x=x, y=y, fill=log_d)) + 
  theme_minimal() +   coord_fixed(xlim=c(-1.5*r_mu,1.5*r_mu), ylim=c(-1.5*r_mu,1.5*r_mu))

#' plot metropolis attempt
pl_sad_metropolis <- ggplot() + 
  geom_contour(data=a_grid, aes(x=x, y=y, z=log_d)) + 
  geom_path(data=o, aes(x=x, y=y, colour=log_d)) + 
  geom_point(data=o, aes(x=x, y=y)) + theme_minimal() + 
  coord_fixed(xlim=c(-1.5*r_mu,1.5*r_mu), ylim=c(-1.5*r_mu,1.5*r_mu))

pl_sad_metropolis_traces <- ggplot(data=o %>% gather(parameter, value, x, y, r, s, log_d), aes(x=iter, y=value)) +
  facet_grid( parameter ~ ., scales='free_y') + geom_line() + theme_minimal() + 
  theme(strip.text.y=element_text(angle=0))

print(pl_sad_metropolis)
print(pl_sad_metropolis_traces)

#' Would adaptive Metropolis do better?
print(cov(draw_donut))

#' How does NUTS do?
m1 <- stan('donut-sim.stan', chains=1, data=list(r_mu=r_mu, r_sd=r_sd), iter=200)
s1 <- extract(m1, inc_warmup=TRUE, permuted=FALSE)
s1df <- s1[,1,] %>% data.frame %>% mutate(iteration=1:nrow(.), jacobian="missing",r=sqrt(x^2+y^2), theta=acos(x/r))

pl_naive_stan <- ggplot() + 
  geom_contour(data=a_grid, aes(x=x, y=y, z=log_d)) + 
  geom_path(data=s1df, aes(x=x, y=y, colour=lp__)) + 
  geom_point(data=s1df, aes(x=x, y=y)) + theme_minimal() + 
  coord_fixed(xlim=c(-1.5*r_mu,1.5*r_mu), ylim=c(-1.5*r_mu,1.5*r_mu))

pl_naive_stan_traces <- ggplot(data=s1df %>% gather(parameter, value, theta, r, x, y, lp__), aes(x=iteration, y=value)) +
  facet_grid( parameter ~ ., scales='free_y') + geom_line() + theme_minimal() + 
  theme(strip.text.y=element_text(angle=0))

print(pl_naive_stan)
print(pl_naive_stan_traces)

#' How does NUTS do with the 'right' model...?
m2 <- stan('donut-sim-corrected.stan', chains=1, data=list(r_mu=r_mu, r_sd=r_sd), iter=200)
s2 <- extract(m2, inc_warmup=TRUE, permuted=FALSE)
s2df <- s2[,1,] %>% data.frame %>% mutate(iteration=1:nrow(.), jacobian="present",
  r=sqrt(x^2+y^2), theta=acos(x/r)) 

pl_J_stan <- ggplot() + 
  geom_contour(data=a_grid, aes(x=x, y=y, z=log_d)) + 
  geom_path(data=s2df, aes(x=x, y=y, colour=lp__)) + 
  geom_point(data=s2df, aes(x=x, y=y)) + theme_minimal() + 
  coord_fixed(xlim=c(-1.5*r_mu,1.5*r_mu), ylim=c(-1.5*r_mu,1.5*r_mu))

pl_J_stan_traces <- ggplot(data=s2df %>% gather(parameter, value, theta, r, x, y, lp__), 
  aes(x=iteration, y=value)) +
  facet_grid( parameter ~ ., scales='free_y') + geom_line() + theme_minimal() + 
  theme(strip.text.y=element_text(angle=0))

print(pl_J_stan)
print(pl_J_stan_traces)





#' NUTS to NUTS comparison:
#' 
m1 <- stan('donut-sim.stan', chains=1, data=list(r_mu=r_mu, r_sd=r_sd), iter=5000)
s1 <- extract(m1, inc_warmup=TRUE, permuted=FALSE)
s1df <- s1[,1,] %>% data.frame %>% mutate(iteration=1:nrow(.), jacobian="missing",r=sqrt(x^2+y^2), theta=acos(x/r))

#' How does NUTS do with the 'right' model...?
m2 <- stan('donut-sim-corrected.stan', chains=1, data=list(r_mu=r_mu, r_sd=r_sd), iter=5000)
s2 <- extract(m2, inc_warmup=TRUE, permuted=FALSE)
s2df <- s2[,1,] %>% data.frame %>% mutate(iteration=1:nrow(.), jacobian="present",r=sqrt(x^2+y^2), theta=acos(x/r)) 

pl_compare <- ggplot(
  data=rbind(s1df, s2df) %>% gather(parameter, value, theta, r, x, y, lp__) %>% filter(parameter %in% 'r'),
  aes(x=value)
) + geom_histogram(binwidth=.1) + facet_grid(jacobian ~ parameter, scales='free') + 
    theme_minimal() +  theme(strip.text.y=element_text(angle=0))


#' Produce plots:

output_store <- '~/output-store/stan-talk'
pdf(file=file.path(output_store,'donut-density.pdf'), width=8, height=6, useDingbats=FALSE)
print(pl_donut_density); dev.off()

pdf(file=file.path(output_store,'sad-metropolis.pdf'), width=8, height=6, useDingbats=FALSE)
print(pl_sad_metropolis); dev.off()

pdf(file=file.path(output_store,'J-stan.pdf'), width=8, height=6, useDingbats=FALSE)
print(pl_J_stan); dev.off()


pdf(file=file.path(output_store,'Jacobian-adjustemnt.pdf'), width=8, height=6, useDingbats=FALSE)
print(pl_compare); dev.off()


