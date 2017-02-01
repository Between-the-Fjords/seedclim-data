##############################################################################
#Script for paper on effect of graminoid removal on plant community properties
##############################################################################

library(dplyr)
library(DBI)
library(tidyr)
con <- dbConnect(RMySQL::MySQL(), group = "seedclim")
setwd("/Users/fja062/Documents/seedclimComm/seedclimComm/")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############### Cover data ###############

## ---- my.GR.data.import ---- 

my.GR.data <- dbGetQuery(con, paste("SELECT sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, turfs.GRtreat, turfCommunity.Year, turfCommunity.species, turfCommunity.cover, sites.Temperature_level, sites.Precipitation_level
FROM taxon INNER JOIN ((sites INNER JOIN ((blocks INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.destinationPlotID) ON sites.siteID = blocks.siteID) INNER JOIN turfCommunity ON turfs.turfID = turfCommunity.turfID) ON taxon.species = turfCommunity.species
GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, turfs.GRtreat, turfCommunity.Year, turfCommunity.species, turfCommunity.cover, sites.Temperature_level, sites.Precipitation_level
HAVING turfCommunity.Year>2009 AND (turfs.TTtreat='ttc' OR (turfs.GRtreat)='rtc' OR (turfs.GRtreat)='ttc');"))


str(my.GR.data)
head(my.GR.data)

levels(my.GR.data$TTtreat) <- c(levels(my.GR.data$TTtreat),levels(my.GR.data$GRtreat))
my.GR.data$TTtreat[my.GR.data$TTtreat == ""| is.na(my.GR.data$TTtreat)] <- my.GR.data$GRtreat[my.GR.data$TTtreat == ""| is.na(my.GR.data$TTtreat)] # merge the GRtreat and TTtreat into one column
my.GR.data$GRtreat <- NULL
my.GR.data$TTtreat <- factor(my.GR.data$TTtreat)
my.GR.data$Year <- factor(my.GR.data$Year)
my.GR.data <- my.GR.data[!(my.GR.data$blockID == "Gud5" & my.GR.data$Year == 2010), ]
my.GR.data$Year[my.GR.data$Year == 2010] <- 2011
my.GR.data$Year <- droplevels(my.GR.data$Year)
my.GR.data$turfID <- plyr::mapvalues(my.GR.data$turfID, from = "Ram4RTCx", to = "Ram4RTC")

#Remove TTCs not included in the data set 
remsites <- c("Skj11", "Skj12", "Gud11", "Gud12", "Gud13")
my.GR.data <- my.GR.data[!my.GR.data$blockID %in% remsites,] 

my.GR.data$ID <- as.factor(paste(my.GR.data$turfID, my.GR.data$Year, sep = "_"))

## ---- my.GR.data.end ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############### TRAITS ###############

## ---- Traits.data.import ---- 

#make fat table
cover <- xtabs(cover ~ paste(turfID, Year, sep = "_") + species, data = my.GR.data)
cover <- as.data.frame(unclass(cover))
cover <- cover[,colSums(cover > 0) > 0] #remove empty spp
head(cover)

# source Ragnhild's trait data
source("ragnhild_trait_data/load_traits.R")
source("ragnhild_trait_data/load_CN.R")

#load from data base
traits <- dbGetQuery(con, paste('SELECT taxon.* , Lower, Nem, BNem, SBor, MBor, NBor, LAlp, MAlp, HAlp, Upper, Min_height, Max_height
                                FROM taxon LEFT JOIN moreTraits ON taxon.species = moreTraits.species
                                ORDER BY taxon.species;'))

traits <- traits[traits$species %in% names(cover),]
traits$HAlp <- as.numeric(traits$HAlp)
traits[,(12:19)][is.na(traits[,12:19])] <- 0
traits <- traits %>% 
  rowwise() %>% 
  mutate(abundance = sum(Nem, BNem, SBor, MBor, NBor, LAlp, MAlp, HAlp, na.rm = TRUE), functionalgroup = as.factor(functionalGroup)) %>%
  mutate(specialism = factor(ifelse(
    Nem == 0 & BNem == 0 & SBor == 0 & LAlp == 1, "alpine", 
    ifelse(HAlp == 0 & MAlp == 0 & LAlp == 0 & Nem == 1, "lowland", 
           ifelse(abundance > 5.5, "generalist", "other"))))) %>% # assigning specialisms to species based on range limits
  select(species, functionalgroup, height:SLA, specialism, Max_height, Min_height)

head(traits)

identical(as.character(traits$species), names(cover)) #this should be identical

# adding traits to my.GR.data
my.GR.data <- my.GR.data %>%
  full_join(traits, by = "species") %>%
  left_join(traitdata, by = c("species", "siteID")) %>%
  mutate(biomass = Height_mean*cover)
  
my.GR.data$prec <- as.factor(c(0.6,1.2,2.0,2.7)[my.GR.data$Precipitation_level])
my.GR.data$temp <- c(6.5,8.5,10.5)[my.GR.data$Temperature_level]

#comment these out depending on the analysis you want to run
my.GR.data$TTtreat <- factor(as.character(my.GR.data$TTtreat), levels = c("TTC", "RTC"))
my.GR.data$functionalgroup <- plyr::mapvalues(my.GR.data$functionalgroup, from = "pteridophyte", to = "forb")
my.GR.data$functionalgroup <- plyr::mapvalues(my.GR.data$functionalgroup, from = "woody", to = "forb")
my.GR.data$specialism <- plyr::mapvalues(my.GR.data$specialism, from = "lowland", to = "other")

## ---- Traits.data.end ---- 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############### DIVERSITY MEASURES ###############

## ---- Diversity.data.import ---- 

#Species richness
library(vegan)
forbs <- my.GR.data %>%
  filter(functionalgroup != "graminoid")

forbcover <- xtabs(cover ~ paste(turfID, Year, sep = "_") + species, data = forbs)
forbcover <- as.data.frame(unclass(forbcover))
forbcover <- forbcover[,colSums(forbcover > 0) > 0] #remove empty spp
head(forbcover)

diversity.freq <- rowSums(forbcover > 0)

#Shannon's diversity index
diversity.freq <- data.frame(richness = diversity.freq, diversity = diversity(forbcover, index = "shannon")) 
rnames <- rownames(forbcover)
diversity.freq$ID <- as.factor(rnames)

#Species evenness
diversity.freq <- cbind(diversity.freq, evenness = diversity.freq$diversity/log(diversity.freq$richness), expdiversity = exp(diversity.freq$diversity))
forbs$evenness <- diversity.freq$evenness[match(forbs$ID, diversity.freq$ID)]
forbs$richness <- diversity.freq$richness[match(forbs$ID, diversity.freq$ID)]
forbs$diversity <- diversity.freq$diversity[match(forbs$ID, diversity.freq$ID)]
forbs$expdiversity <- diversity.freq$expdiversity[match(forbs$ID, diversity.freq$ID)]

head(forbs)

diversity <- forbs %>%
  distinct(ID, .keep_all = TRUE) %>%
  select(ID, (evenness:expdiversity)) %>%
  as.data.frame()

diversity <- diversity[rowSums(is.na(diversity)) != ncol(diversity),]

## ---- Diversity.data.end ---- 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############## WEIGHTED MEANS ###############

## ---- Community.mean.weighting ---- 

###### weighted means for whole community
wholecom <- my.GR.data %>% 
  group_by(ID, functionalgroup) %>%
  mutate(wmean_height = weighted.mean(height, cover, na.rm = TRUE), wmean_leafSize = weighted.mean(LA_mean, cover, na.rm = TRUE), wmean_seedMass = weighted.mean(seedMass, cover, na.rm = TRUE), wmean_SLA = weighted.mean(SLA_mean, cover, na.rm = TRUE), wmean_maxheight = weighted.mean(Max_height, cover, na.rm = TRUE), wmean_minheight = weighted.mean(Min_height, cover, na.rm = TRUE), Year = Year, biomass = sqrt(biomass), sumcover = sum(cover)) %>%
  mutate(wmean_LDMC_local = weighted.mean(LDMC_mean, cover, na.rm = TRUE), wmean_SLA_local = weighted.mean(SLA_mean, cover, na.rm = TRUE), wmean_LTH_local = weighted.mean(Lth_mean, cover, na.rm = TRUE), wmean_LA_local = weighted.mean(LA_mean, cover, na.rm = TRUE), wmean_height_local = weighted.mean(Height_mean, cover, na.rm = TRUE)) %>%
  select(-(Temperature_level:Precipitation_level), -(SLA:seedMass), -species, -specialism) %>%
  filter(!(functionalgroup == "graminoid" & Year == 2012) & !(functionalgroup == "graminoid" & Year == 2013) & !(functionalgroup == "graminoid" & Year == 2015) & !(functionalgroup == "graminoid" & Year == 2016)) %>%
  distinct(ID, functionalgroup, .keep_all = TRUE) %>%
  as.data.frame()

wholecom <- wholecom %>%
  full_join(diversity, wholecom, by = "ID")


wholecom$funYear <- as.factor(paste(wholecom$functionalgroup, wholecom$Year, sep = "_"))


###### weighted means by specialism
specialism <- my.GR.data %>%
  group_by(ID, specialism) %>%
  filter(functionalgroup != "graminoid") %>%
  mutate(wmean_height = weighted.mean(height, cover), wmean_leafSize = weighted.mean(leafSize, cover), wmean_seedMass = weighted.mean(seedMass, cover), wmean_SLA = weighted.mean(SLA, cover), Year = Year, sumcover = sum(cover)) %>%
  mutate(wmean_LDMC_local = weighted.mean(LDMC_mean, cover, na.rm = TRUE), wmean_SLA_local = weighted.mean(SLA_mean, cover, na.rm = TRUE), wmean_LTH_local = weighted.mean(Lth_mean, cover, na.rm = TRUE), wmean_LA_local = weighted.mean(LA_mean, cover, na.rm = TRUE), wmean_height_local = weighted.mean(Height_mean, cover, na.rm = TRUE)) %>%
  distinct(ID, specialism, .keep_all = TRUE) %>%
  select(-(functionalgroup:seedMass), -(Temperature_level:Precipitation_level), -species) %>%
  as.data.frame()

specialism$specYear <- as.factor(paste(specialism$specialism, specialism$Year, sep = "_"))


## ---- Community.mean.weighting.end ---- 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
################## WHOLECOM:: CALCULATING TRAITS DELTA ###################

## ---- Explanatory.variable.deltas ---- 

deltacalc <- sapply(1:nrow(wholecom[wholecom$TTtreat == "RTC",]), function(i){
  R <- wholecom[wholecom$TTtreat == "RTC",][i,]
  #browser()
  cols <- c("wmean_height","wmean_SLA","wmean_leafSize", "sumcover", "wmean_seedMass", "wmean_maxheight", "wmean_minheight", "diversity", "richness", "biomass", "wmean_LDMC_local", "wmean_SLA_local", "wmean_LTH_local", "wmean_LA_local", "wmean_height_local")
  friend <- wholecom$Year == R$Year & wholecom$blockID == R$blockID & wholecom$functionalgroup == R$functionalgroup & wholecom$TTtreat == "TTC"
  if(all (!friend)) {print(R$turfID)
    return(rep(NA, length(cols)))}
  stopifnot(sum(friend) == 1)
  
  f <- wholecom[friend,]
  x <- R[,cols] - f[,cols]
  unlist(x)
})
deltacalc <- as.data.frame(t(deltacalc))
colnames(deltacalc) <- paste0("delta", colnames(deltacalc))
rtcmeta <- cbind((wholecom[wholecom$TTtreat == "RTC",]), deltacalc)
rtcmeta$Year <- factor(rtcmeta$Year)
#rtcmeta<-rtcmeta[rtcmeta$Year!=2011,]

################## FUNCTIONAL:: CALCULATING TRAITS DELTA ###################

special <- sapply(1:nrow(specialism[specialism$TTtreat == "RTC",]), function(i){
  R <- specialism[specialism$TTtreat == "RTC",][i,]
  #browser()
  cols <- c("wmean_height","wmean_SLA","wmean_leafSize", "wmean_seedMass", "sumcover", "biomass", "wmean_LDMC_local", "wmean_SLA_local", "wmean_LTH_local", "wmean_LA_local", "wmean_height_local")
  friend <- specialism$Year == R$Year & specialism$blockID == R$blockID & specialism$specialism == R$specialism & specialism$TTtreat == "TTC"
  if(all (!friend)) {print(R$turfID)
    return(rep(NA, length(cols)))}
  stopifnot(sum(friend) == 1)
  
  f <- specialism[friend,]
  x <- R[,cols] - f[,cols]
  unlist(x)
})
special <- as.data.frame(t(special))
colnames(special) <- paste0("delta", colnames(special))
special <- cbind((specialism[specialism$TTtreat == "RTC",]), special)

######### CALCULATING TIME DELTA #######

#timedeltacalc <- sapply(1:nrow(wholecom[wholecom$Year != 2011,]), function(i){
 # R <- wholecom[wholecom$Year != 2011,][i,]
 #  #browser()
 # cols <- c("richness", "evenness", "diversity", "Height", "SLA", "leafSize", "sumcover", "seedMass")
 #  friend <- wholecom$turfID == R$turfID & wholecom$Year == 2011
 # if(all (!friend)) {print(R$turfID)
 #    return(rep(NA, length(cols)))}
 #  stopifnot(sum(friend) == 1)
 #  
 #  f <- wholecom[friend,]
 #  x <- R[,cols] - f[,cols]
 #  unlist(x)
 #})
#timedeltacalc <- as.data.frame(t(timedeltacalc))
#colnames(timedeltacalc) <- paste0("delta", colnames(timedeltacalc))
#timedelta <- cbind((cover.meta[cover.meta$Year != 2011,]), timedeltacalc)

## ---- Explanatory.variable.deltas.end ---- 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###### SEEDLINGS ########

## ---- Seedling.data.import ---- 

# we only have data for seedlings in 2013, so this is purely a control-treatment analysis
# source seedling data import
source("inst/graminoidRemovals/Seedlings.R")

## ---- Seedling.data.end ---- 
