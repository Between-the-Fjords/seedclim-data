### Log transforming ###
library(e1071)

### Bootstraping the community weighted means and other things ###

CWM_Bootstrapping <- function(community, trait, nrep = 100, samplesize = 200){
  comm <- community %>% 
    filter(!cover == 0)
  
  t_dat <- trait %>% filter(!is.na(Value))
  
  TraitWeights <- comm %>% 
    left_join(t_dat, by = c("Site" = "Site", "species" = "Species")) %>% 
    group_by(Site, Block, turfID, Trait) %>% 
    mutate(n = n())

  
  BootstrapMoments_All <- map_df(
    1:nrep,
    ~(sample_n(TraitWeights, replace = TRUE, size = samplesize, weight = cover_species)) %>%
    # get all the happy moments
    summarise(Mean = mean(Value), Variance = var(Value), Skewness = skewness(Value), Kurtosis = kurtosis(Value)))
  
  
  
  return(BootstrapMoments_All)
}

# #Site level weights and traits  
#   
#   comm <- community_cover
#   t_dat <- traitdata_1
#   
#   TraitWeights <- comm %>% 
#     left_join(t_dat, by = c("Site" = "Site", "species" = "Species")) %>% 
#     group_by(Site, Block, turfID, Trait)
#   
#   
#   # Regional level weights and traits
#   TraitWeights_regional <- comm %>% 
#     left_join(trait, by = c("species" = "Species")) %>% 
#     group_by(Country, Year, Gradient, Taxon, Trait) %>% 
#     mutate(weight = Cover/n()) %>% 
#     group_by(Country, Year, Gradient, Trait) 
#   
#   
#   TraitWeights_all <- bind_rows(plot = TraitWeights_plot, site = TraitWeights_site, global = TraitWeights_global, .id = "level") %>% 
#     mutate(level = factor(level, levels = c("plot", "site", "global"), ordered = TRUE)) %>%
#     filter(!is.na(Value)) %>% 
#     group_by(Country, Year, Site, Gradient, BlockID, PlotID, Trait, Taxon) %>% 
#     filter(level == min(level)) %>% 
#     group_by(Country, Year, Site, Gradient, BlockID, PlotID, Trait)
#   


SummarizeBootMoments <- function(BootstrapMoments_All){
  # calculate means and 
  BootstrapMoments <- BootstrapMoments_All %>% 
    group_by(Country, Year, Site, Gradient, BlockID, PlotID, Trait) %>% 
    summarise(n = n(),
              meanMean = mean(Mean), CIlow.Mean = meanMean - sd(Mean), CIhigh.Mean = meanMean + sd(Mean),
              meanVar = mean(Variance), CIlow.Var = meanVar - sd(Variance), CIhigh.Var = meanVar + sd(Variance),
              meanSkew = mean(Skewness), CIlow.Skew = meanSkew - sd(Skewness), CIhigh.Skew = meanSkew + sd(Skewness),
              meanKurt = mean(Kurtosis), CIlow.Kurt = meanKurt - sd(Kurtosis), CIhigh.Kurt = meanKurt + sd(Kurtosis)) 
  
  return(BootstrapMoments)
}
