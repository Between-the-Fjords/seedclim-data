###### bayesian analysis #####

# 1) Save a description of the model in JAGS syntax to working directory
sink("~/OneDrive - University of Bergen/Research/FunCaB/SeedClim-Climate-Data/ibuttonModel.txt")
cat(
  "model{
  #likelihood
  for(datIter in 1:nData){
  obs[datIter] ~ dpois(mu[datIter], tau)
  mu[datIter] <- intercept + treatment[dataIter] + round[dataIter] + temp[datIter] + precip[dataIter] + eps[datIter]
  }
  
  
  
  #random effects
  for(site in 1:siteID){
  for(block in 1:blockID){
  eps[site[block]] ~ dnorm(0, tau.block)
  eps[site] ~ dnorm(0, tau.site)
  }
  }
  
  
  #priors
  tau.block ~ dnorm(0, 0.001)
  tau.site ~ dnorm(0, 0.001)
  tau.obs ~ dnorm(0, 0.001)
  sigma.block ~ dnorm(0, 0.001)
  sigma.site ~ dunif(0,100)
  sigma.obs ~ dunif(0,100)
  treatment ~ dnorm(0, 0.001)
  time ~ dnorm(0, 0.001)
  temp ~ dnorm(0, 0.001)
  
  
  
  }
  ", fill = TRUE
  
)

# 2) Set up a list that contains all the necessary data
Data <- list(N = Dat$N,
             Veg = Dat$Veg, Veg2 = Dat$Veg^2,
             i.max = nrow(Dat))

# 3) Specify a function to generate inital values for the parameters
inits.fn <- function() list(alpha = rnorm(1,0,1),
                            beta.Veg = rnorm(1,0,1),
                            beta.Veg2 = rnorm(1,0,1),
                            tau.eps = dunif(1,0,10),
                            alpha.Inc = rnorm(1,0,1),
                            beta.Inc.Veg = rnorm(1,0,1),
                            Inc = rep(1,Data$i.max)
)

# Compile the model and run the MCMC for an adaptation (burn-in) phase
jagsModel <- jags.model(file= "LME.txt", data=Data, init = inits.fn, n.chains = 3, n.adapt= 1000)
# Specify parameters for which posterior samples are saved
para.names <- c("a","b","sigma","sigma.J")
# Continue the MCMC runs with sampling
Samples <- coda.samples(jagsModel, variable.names = para.names, n.iter = 50000)

# Plot the mcmc chain and the posterior sample for p
plot(Samples)

# convergence check
gelman.diag(Samples)

# Statistical summaries of the posterior sample
summary(Samples)

