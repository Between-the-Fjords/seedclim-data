#########################################
### Code for analysis                 ###
### Seedclim climate difference paper ###
### Lynn et al.                       ###
#########################################

# load in required packages
library(R2jags);library(loo)

## read in data
dat <- read.csv("ClimDiff_LynnETAL_data.csv")

# First, models of colonzation

# group level effects- used for all models
site <- factor(dat$siteID)
block <- factor(dat$blockID)
turf <- factor(dat$turfID) 
spp <- factor(dat$species)
fam <- factor(dat$family)

# independent variables
# switch out model matrix to test each of the six models run in paper
comdat$warm <- factor(dat$warm)
comdat$wet <- factor(dat$wet)

### standardize range size (ahull) to mean of 0 and sd=0.5
standard <- function(x) (x - mean(x, na.rm=T)) / (2* (sd(x, na.rm=T)))
dat$rsize <- standard(dat$ahull)

# specify model matrix
treatmat <- model.matrix(~wet*warm*mat_meandiff*ap_meandiff, dat)
#treatmat <- model.matrix(~wet*warm*rsize, dat)
#treatmat <- model.matrix(~wet*warm*mat_meandiff*ap_meandiff+wet*warm*rsize, dat)

#dependent variable
col <- dat$colon
N <- as.numeric(length(col))

jags.data <- list("site", "block","fam", "turf", "spp","treatmat", "col", "N")
jags.param <- c("ab", "rss","rss.new", "prec1","prec2", "prec3","prec4",
                "prec5","zlog.lik")

colmod <- function(){ 
  # group effects
  for (j in 1:12){nettstedet[j]~dnorm(0, prec1)}
  for (j in 1:60){blokkere[j]~dnorm(0, prec2)}
  for (j in 1:173){torv[j]~dnorm(0, prec3)}
  for (j in 1:149){arter[j]~dnorm(0, prec4)}
  for (j in 1:42){familien[j]~dnorm(0, prec5)}
  #likelihood
  for (i in 1:N){
    col[i]~dbern(mu[i])
    logit(mu[i]) <- inprod(ab,treatmat[i,])+ nettstedet[site[i]]+ 
      blokkere[block[i]]+ torv[turf[i]]+ arter[spp[i]]+familien[fam[i]]
    
    # for model selection
    zlog.lik[i] <- logdensity.bern(col[i], mu[i])
    
    # sum of squared residuals
    res[i] <- pow(col[i]-mu[i], 2)
    col.new[i] ~ dbern(mu[i])
    res.new[i] <- pow(col.new[i]-mu[i],2)
  }
  # priors
  for(j in 1:16){ab[j]~dnorm(0, 1.0E-6)} # for climate difference model
  #for(j in 1:8){ab[j]~dnorm(0, 1.0E-6)} # for range size model
  #for(j in 1:20){ab[j]~dnorm(0, 1.0E-6)} # for interaction model
  prec1~dgamma(0.001,0.001)
  prec2~dgamma(0.001,0.001)
  prec3~dgamma(0.001,0.001)
  prec4~dgamma(0.001,0.001)
  prec5~dgamma(0.001,0.001)
  # derived params for PPC
  rss <- sum(res[])
  rss.new <- sum(res.new[])
}

# run the model
colres <- jags.parallel(data=jags.data,inits=NULL, parameters.to.save=jags.param,
                        n.iter=50000 ,model.file=colmod, n.thin=5, n.chains=3)

colres

# Model selection criteria
### pull out simulations
paramlist <- colres$BUGSoutput$sims.list

# get log-likelihoods for model
loglik <- paramlist$zlog.lik # loglikelihood value for each of n simulations

# use waic() and loo() function from loo package
col.waic <- waic(loglik)
col_eff <- relative_eff(exp(loglik), chain_id=rep(1:1, each=15000))
col.loo <- loo(loglik, r_eff=col_eff)

col.waic
col.loo



# Second, models of extinction

# specify the model matrix
treatmat <- model.matrix(~wet*warm*mat_meandiff*ap_meandiff, dat)
#treatmat <- model.matrix(~wet*warm*rsize, dat)
#treatmat <- model.matrix(~wet*warm*mat_meandiff*ap_meandiff+wet*warm*rsize, dat)

#dependent variable
ext <- dat$extinct
N <- as.numeric(length(ext))

jags.data <- list("site", "block","fam", "turf", "spp","treatmat", "ext", "N")
jags.param <- c("ab", "rss","rss.new", "prec1","prec2", "prec3","prec4",
                "prec5", "zlog.lik")

extmod <- function(){ 
  # group effects
  for (j in 1:12){nettstedet[j]~dnorm(0, prec1)}
  for (j in 1:60){blokkere[j]~dnorm(0, prec2)}
  for (j in 1:173){torv[j]~dnorm(0, prec3)}
  for (j in 1:149){arter[j]~dnorm(0, prec4)}
  for (j in 1:42){familien[j]~dnorm(0, prec5)}
  #likelihood
  for (i in 1:N){
    ext[i]~dbern(mu[i])
    logit(mu[i]) <- inprod(ab,treatmat[i,])+ nettstedet[site[i]]+ 
      blokkere[block[i]]+ torv[turf[i]]+ arter[spp[i]]+familien[fam[i]]
    
    # for model selection
    zlog.lik[i] <- logdensity.bern(ext[i], mu[i])
    
    # sum of squared residuals
    res[i] <- pow(ext[i]-mu[i], 2)
    ext.new[i] ~ dbern(mu[i])
    res.new[i] <- pow(ext.new[i]-mu[i],2)
  }
  # priors
  for(j in 1:16){ab[j]~dnorm(0, 1.0E-6)} # for climate difference model
  #for(j in 1:8){ab[j]~dnorm(0, 1.0E-6)} # for range size model
  #for(j in 1:20){ab[j]~dnorm(0, 1.0E-6)} # for interaction model
  prec1~dgamma(0.001,0.001)
  prec2~dgamma(0.001,0.001)
  prec3~dgamma(0.001,0.001)
  prec4~dgamma(0.001,0.001)
  prec5~dgamma(0.001,0.001)
  # derived params for PPC
  rss <- sum(res[])
  rss.new <- sum(res.new[])
}

# run the model
extres <- jags.parallel(data=jags.data,inits=NULL, parameters.to.save=jags.param,
                        n.iter=50000 ,model.file=extmod, n.thin=5, n.chains=3)

extres

# Model selection criteria
### pull out simulations
paramlist <- extres$BUGSoutput$sims.list

# get log-likelihoods for model
loglik <- paramlist$zlog.lik # loglikelihood value for each of n simulations

# use waic() and loo() function from loo package
ext.waic <- waic(loglik)
ext_eff <- relative_eff(exp(loglik), chain_id=rep(1:1, each=15000))
ext.loo <- loo(loglik, r_eff=ext_eff)

ext.waic
ext.loo

# Third, models of change in cover

# specify the model matrix
treatmat <- model.matrix(~wet*warm*mat_meandiff*ap_meandiff, dat)
#treatmat <- model.matrix(~wet*warm*rsize, dat)
#treatmat <- model.matrix(~wet*warm*mat_meandiff*ap_meandiff+wet*warm*rsize, dat)

#dependent variable
cover <- comdat2$dif2
N <- as.numeric(length(cover))

jags.data <- list("site", "block","fam", "turf", "spp","treatmat", "cover", "N")
jags.param <- c("ab", "rss","rss.new", "prec1","prec2", "prec3","prec4",
                "prec5", "prec6","zlog.lik")

covmod <- function(){ 
  # group effects
  for (j in 1:12){nettstedet[j]~dnorm(0, prec1)}
  for (j in 1:60){blokkere[j]~dnorm(0, prec2)}
  for (j in 1:173){torv[j]~dnorm(0, prec3)}
  for (j in 1:149){arter[j]~dnorm(0, prec4)}
  for (j in 1:42){familien[j]~dnorm(0, prec5)}
  #likelihood
  for (i in 1:N){
    cover[i]~dnorm(mu[i], prec6)
    mu[i] <- inprod(ab,treatmat[i,])+ nettstedet[site[i]]+ 
      blokkere[block[i]]+ torv[turf[i]]+ arter[spp[i]]+familien[fam[i]]
    
    # for model selection
    zlog.lik[i] <- logdensity.norm(cover[i], mu[i], prec6)
    
    # sum of squared residuals
    res[i] <- pow(cover[i]-mu[i], 2)
    cov.new[i] ~ dnorm(mu[i], prec6)
    res.new[i] <- pow(cov.new[i]-mu[i],2)
  }
  # priors
  for(j in 1:16){ab[j]~dnorm(0, 1.0E-6)} # for climate difference model
  #for(j in 1:8){ab[j]~dnorm(0, 1.0E-6)} # for range size model
  #for(j in 1:20){ab[j]~dnorm(0, 1.0E-6)} # for interaction model
  prec1~dgamma(0.001,0.001)
  prec2~dgamma(0.001,0.001)
  prec3~dgamma(0.001,0.001)
  prec4~dgamma(0.001,0.001)
  prec5~dgamma(0.001,0.001)
  prec6~dgamma(0.001,0.001)
  # derived params for PPC
  rss <- sum(res[])
  rss.new <- sum(res.new[])
}

# run the model
covres <- jags.parallel(data=jags.data,inits=NULL, parameters.to.save=jags.param,
                        n.iter=50000 ,model.file=covmod, n.thin=5, n.chains=3)

covres

# Model selection criteria
### pull out simulations
paramlist <- covres$BUGSoutput$sims.list

# get log-likelihoods for model
loglik <- paramlist$zlog.lik # loglikelihood value for each of n simulations

# use waic() and loo() function from loo package
cov.waic <- waic(loglik)
cov_eff <- relative_eff(exp(loglik), chain_id=rep(1:1, each=15000))
cov.loo <- loo(loglik, r_eff=cov_eff)

cov.waic
cov.loo

### end script