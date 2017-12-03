##############################################################################
#Script for paper on effect of graminoid removal on plant community properties
##############################################################################

library(tidyverse)
library(stringr)
library(DBI)
library(dbplyr)
con <- src_mysql(group = "seedclim", dbname = "seedclimComm", password = "password")
setwd("/Users/fja062/Documents/seedclimComm/seedclimComm/")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############### Cover data ###############

## ---- my.GR.data.import ---- 
problems <- read.csv("speciesCorrections.csv", sep = ";", stringsAsFactors = FALSE) %>%
  filter(!old %in% c("Vio.can", "Com.ten", "Sel.sel")) %>%
  filter(cover != "WHAT HAPPENED") %>%
  mutate(cover = as.numeric(cover))

problems.cover <- filter(problems, !is.na(cover)) %>%
  select(turfID, year = Year, species = old, cover)


my.GR.data <-tbl(con, "subTurfCommunity") %>%
  group_by(turfID, year, species) %>% 
  summarise(n_subturf = n()) %>% 
  collect() %>% 
  full_join(tbl(con, "turfCommunity") %>% collect()) %>%
  full_join(problems.cover, by = c("year", "turfID", "species"), suffix = c(".community", ".problems")) %>%
  mutate(cover = if_else(is.na(cover.community),
                         cover.problems,
                         cover.community)) %>% 
  left_join(tbl(con, "taxon"), copy = TRUE) %>%
  left_join(tbl(con, "turfs"), copy = TRUE) %>%
  left_join(tbl(con, "plots"), by = c("destinationPlotID" = "plotID"), copy = TRUE) %>%
  left_join(tbl(con, "blocks"), by = "blockID", copy = TRUE) %>%
  left_join(tbl(con, "sites"), by = "siteID", copy = TRUE) %>%
  left_join(tbl(con, "turfEnvironment"), copy = TRUE) %>%
  filter(year > 2009, TTtreat == "TTC"|GRtreat == "RTC"|GRtreat == "TTC") %>%
  select(siteID, blockID, plotID = destinationPlotID, turfID, TTtreat, GRtreat, Year = year, species, cover, Temperature_level, Precipitation_level, recorder, totalVascular, totalBryophytes, functionalGroup, vegetationHeight, mossHeight) %>%
  mutate(TTtreat = factor(TTtreat), GRtreat = factor(GRtreat))

my.GR.data

my.GR.data <- my.GR.data %>%
  mutate(functionalGroup = if_else(species %in% c("Gen.sp.", "Cre.pal", "Frag.vir", "Sch.gig", "Ste.bor", "Hie.ore", "Sel.sel."), "forb", functionalGroup),
         functionalGroup = if_else(species %in% c("Agr.can", "Phl.sp"), "graminoid", functionalGroup))

levels(my.GR.data$TTtreat) <- c(levels(my.GR.data$TTtreat),levels(my.GR.data$GRtreat))
my.GR.data$TTtreat[my.GR.data$TTtreat == ""| is.na(my.GR.data$TTtreat)] <- my.GR.data$GRtreat[my.GR.data$TTtreat == ""| is.na(my.GR.data$TTtreat)] # merge the GRtreat and TTtreat into one column
my.GR.data$GRtreat <- NULL
my.GR.data <- my.GR.data[!(my.GR.data$blockID == "Gud5" & my.GR.data$Year == 2010), ]
my.GR.data <- my.GR.data[!(my.GR.data$turfID == "Fau1RTC" & my.GR.data$Year == 2010), ]
my.GR.data <- my.GR.data[!(my.GR.data$functionalGroup == "graminoid" & my.GR.data$Year >2011 & my.GR.data$TTtreat == "RTC"), ]
my.GR.data$Year[my.GR.data$Year == 2010] <- 2011


my.GR.data$turfID <- plyr::mapvalues(my.GR.data$turfID, from = "Ram4RTCx", to = "Ram4RTC")
my.GR.data$turfID <- plyr::mapvalues(my.GR.data$turfID, from = "Ram5RTCx", to = "Ram5RTC")

my.GR.data$ID <- as.factor(paste(my.GR.data$turfID, my.GR.data$Year, sep = "_"))

my.GR.data$recorder[is.na(my.GR.data$recorder)] <- "unknown botanist"
my.GR.data$cover[my.GR.data$recorder == "PM"] <- my.GR.data$cover[my.GR.data$recorder=="PM"]*1.20

siri <- my.GR.data %>%
  filter(recorder == "Siri") %>%
  group_by(turfID, Year) %>%
  mutate(SumOfcover = sum(cover)) %>%
  filter(SumOfcover/totalVascular < 1.35)

siri.fix <- paste(as.character(my.GR.data$turfID), my.GR.data$Year) %in% paste(siri$turfID, siri$Year)
my.GR.data$cover[siri.fix] <- my.GR.data$cover[siri.fix]*1.3

owen <- my.GR.data %>% 
  filter(recorder == "Owen") %>% 
  group_by(turfID, Year) %>% 
  mutate(sumOfCover = sum(cover)) %>% 
  filter(sumOfCover/totalVascular > 1.5)

owen.fix <- paste(as.character(my.GR.data$turfID), my.GR.data$Year) %in% paste(owen$turfID, owen$Year)
my.GR.data$cover[owen.fix] <- my.GR.data$cover[owen.fix]/1.5


my.GR.data <- my.GR.data %>% 
  filter(!(blockID == "Fau1" & Year == "2011")) %>% 
  filter(!(blockID == "Fau4" & Year == "2011")) %>% 
  filter(!(blockID %in% c("Skj11", "Skj12", "Gud11", "Gud12", "Gud13")))

# replace species names where mistakes have been found in database
prob.sp <- problems %>%
  filter(!is.na(Year))

for(i in 1:nrow(prob.sp)) {
  my.GR.data$species[my.GR.data$Year == prob.sp$Year[i] & my.GR.data$turfID == prob.sp$turfID[i] & my.GR.data$species == prob.sp$old[i]] <- prob.sp$new[i]
}

#A function that takes a dataframe, and a list of problems and resolutions, and runs them through replace_all() 
probfixes=function(df, old, new){
  for(i in 1:length(old)){
    df=replace_all(df,old[i],new[i])
  }
  return(df)
}

#A function that finds and replaces a string (using regex) in a dataframe
replace_all <- function(df, pattern, replacement) {
  char <- vapply(df, function(x) is.factor(x) || is.character(x), logical(1))
  df[char] <- lapply(df[char], str_replace_all, pattern, replacement)  
  df
}

prob.sp.name <- problems %>% 
  filter(is.na(Year))

my.GR.data <- probfixes(my.GR.data, prob.sp.name$old, prob.sp.name$new)

#gridded temperature etc
source("inst/graminoidRemovals/weather.R")

my.GR.data <- my.GR.data %>%
  left_join(weather, by = "siteID") 

## ---- my.GR.data.end ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############### TRAITS ###############

## ---- Traits.data.import ---- 

# source Ragnhild's trait data
source("ragnhild_trait_data/load_traits.R") # warning here is fine, it just means those spp didn't have CN data collected

#load from data base
con <- dbConnect(RMySQL::MySQL(), group = "seedclim")
traits <- dbGetQuery(con, paste('SELECT taxon.* , Lower, Nem, BNem, SBor, MBor, NBor, LAlp, MAlp, HAlp, Upper, Min_height, Max_height
                                FROM taxon LEFT JOIN moreTraits ON taxon.species = moreTraits.species
                                ORDER BY taxon.species;'))

traits <- traits[traits$species %in% my.GR.data$species,]
traits$HAlp <- as.numeric(traits$HAlp)
traits[,(12:19)][is.na(traits[,12:19])] <- 0 #change this to something more interpretable!
traits <- traits %>% 
  rowwise() %>% 
  mutate(abundance = sum(Nem, BNem, SBor, MBor, NBor, LAlp, MAlp, HAlp, na.rm = TRUE)) %>%
  mutate(specialism = factor(ifelse(
    Nem == 0 & BNem == 0 & SBor == 0 & LAlp == 1, "alpine", 
    ifelse(HAlp == 0 & MAlp == 0 & LAlp == 0 & Nem == 1, "lowland", 
           ifelse(abundance > 5.5, "generalist", "other"))))) %>% # assign specialisms to species based on range limits
  select(species, height:seedMass, specialism, Max_height, Min_height)

head(traits)

identical(as.character(traits$species), my.GR.data$species) #this should be identical, but if it's not it just means we are lacking the trait information for some species
identical(as.character(traitdata$species), my.GR.data$species) #this should be identical

# "Agr.can"  "Cre.pal"  "Frag.vir" "Gen.sp."  "Hie.ore"  "Sch.gig" the species that don't appear in the traits but do appear in cover


# adding traits to my.GR.data
my.GR.data <- my.GR.data %>%
  left_join(traits, by = "species") %>%
  left_join(traitdata, by = c("species", "siteID"))
  
my.GR.data$Precipitation_level <- c(0.6,1.2,2.0,2.7)[my.GR.data$Precipitation_level]
my.GR.data$Temperature_level <- c(6.5,8.5,10.5)[my.GR.data$Temperature_level]

#comment these out depending on the analysis you want to run
my.GR.data$TTtreat <- factor(as.character(my.GR.data$TTtreat), levels = c("TTC", "RTC"))
my.GR.data$functionalGroup <- plyr::mapvalues(my.GR.data$functionalGroup, from = "pteridophyte", to = "forb")
my.GR.data$functionalGroup <- plyr::mapvalues(my.GR.data$functionalGroup, from = "woody", to = "forb")
my.GR.data$specialism <- plyr::mapvalues(my.GR.data$specialism, from = "lowland", to = "other")


my.GR.data %>%
  filter(TTtreat == "TTC") %>% 
  ggplot(aes(x = as.factor(Precipitation_level), y = mossHeight)) +
  stat_summary(fun.data = "mean_cl_boot") +
  stat_summary(fun.data = "mean_cl_boot", geom = "line") +
  theme_classic() +
  axis.dim +
  labs(x = "Annual rainfall (m)", y = "Moss depth (cm)") +
  ggsave(filename = "moss_depth_precip.jpg", path = "/Users/fja062/Documents/seedclimComm/figures")



my.GR.data %>% ggplot(aes(x = Year, y = totalBryophytes, colour = TTtreat, group = TTtreat)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line") +
  facet_grid(. ~ Precipitation_level)

## ---- Traits.data.end ---- 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############### DIVERSITY MEASURES ###############

## ---- Diversity.data.import ---- 

#Species richness
library(vegan)

my.GR.data <- my.GR.data %>%
  group_by(turfID, Year, functionalGroup) %>%
  mutate(richness = sum(n_distinct(species))) %>% 
  mutate(diversity = diversity(cover, index = "shannon")) %>% 
  mutate(evenness = (diversity/log(richness)))


## ---- Diversity.data.end ---- 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############## WEIGHTED MEANS ###############

## ---- Community.mean.weighting ---- 

###### weighted means for whole community
wholecom <- my.GR.data %>% 
  group_by(ID, functionalGroup) %>%
  mutate(wmean_height = weighted.mean(height, cover, na.rm = TRUE),
         wmean_leafSize = weighted.mean(leafSize, cover, na.rm = TRUE),
         wmean_seedMass = weighted.mean(seedMass, cover, na.rm = TRUE),
         wmean_maxheight = weighted.mean(Max_height, cover, na.rm = TRUE),
         wmean_minheight = weighted.mean(Min_height, cover, na.rm = TRUE),
         sumcover = sum(cover),
         wmean_LDMC_local = weighted.mean(LDMC_mean, cover, na.rm = TRUE),
         wmean_SLA_local = weighted.mean(SLA_mean, cover, na.rm = TRUE),
         wmean_LTH_local = weighted.mean(Lth_mean, cover, na.rm = TRUE),
         wmean_LA_local = weighted.mean(LA_mean, cover, na.rm = TRUE),
         wmean_height_local = weighted.mean(Height_mean, cover, na.rm = TRUE),
         wmean_CN_local = weighted.mean(CN_mean, cover, na.rm = TRUE),
         wmean_LDMC_global = weighted.mean(LDMC_mean_global, cover, na.rm = TRUE),
         wmean_SLA_global = weighted.mean(SLA_mean_global, cover, na.rm = TRUE),
         wmean_CN_global = weighted.mean(CN_mean_global, cover, na.rm = TRUE),
         wmean_LTH_global = weighted.mean(Lth_mean_global, cover, na.rm = TRUE),
         wmean_LA_global = weighted.mean(LA_mean_global, cover, na.rm = TRUE),
         wmean_height_global = weighted.mean(Height_mean_global, cover, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-(height:seedMass), -(Max_height:SLA), -recorder, -species, -(SLA_mean_global:LA_mean_global)) %>%
  #filter(!(functionalgroup == "graminoid" & Year == 2012) & !(functionalgroup == "graminoid" & Year == 2013) & !(functionalgroup == "graminoid" & Year == 2015) & !(functionalgroup == "graminoid" & Year == 2016)) %>%
  distinct(ID, functionalGroup, .keep_all = TRUE) %>%
  as.data.frame()

wholecom$funYear <- as.factor(paste(wholecom$functionalGroup, wholecom$Year, sep = "_"))

forbcom <- wholecom %>%
  filter(functionalGroup == "forb") 


## ---- Community.mean.weighting.end ---- 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
################## WHOLECOM:: CALCULATING TRAITS DELTA ###################

## ---- Explanatory.variable.deltas ---- 

deltacalc <- sapply(1:nrow(forbcom[forbcom$TTtreat == "RTC",]), function(i){
  R <- forbcom[forbcom$TTtreat == "RTC",][i,]
  #browser()
  cols <- c("wmean_height","wmean_leafSize", "sumcover", "totalBryophytes", "wmean_seedMass", "wmean_maxheight", "wmean_minheight", "diversity", "richness", "evenness", "wmean_LDMC_global", "wmean_SLA_global", "wmean_LTH_global", "wmean_LA_global", "wmean_height_global", "wmean_CN_global", "wmean_LDMC_local", "wmean_SLA_local", "wmean_LTH_local", "wmean_LA_local", "wmean_height_local", "wmean_CN_local")
  friend <- forbcom$Year == R$Year & forbcom$blockID == R$blockID & forbcom$functionalGroup == R$functionalGroup & forbcom$TTtreat == "TTC"
  if(all (!friend)) {print(R$turfID)
    return(rep(NA, length(cols)))}
  stopifnot(sum(friend) == 1)
  
  f <- forbcom[friend,]
  x <- R[,cols] - f[,cols]
  unlist(x)
})
deltacalc <- as.data.frame(t(deltacalc))
colnames(deltacalc) <- paste0("delta", colnames(deltacalc))
rtcmeta <- cbind((forbcom[forbcom$TTtreat == "RTC",]), deltacalc)
rtcmeta$Year <- factor(rtcmeta$Year)
#rtcmeta<-rtcmeta[rtcmeta$Year!=2011,]


######### CALCULATING TIME DELTA #######

timedeltacalc <- sapply(1:nrow(forbcom[forbcom$Year != 2011,]), function(i){
 R <- forbcom[forbcom$Year != 2011,][i,]
   #browser()
  cols <- c("wmean_height","wmean_leafSize", "sumcover", "totalBryophytes", "wmean_seedMass", "wmean_maxheight", "wmean_minheight", "diversity", "richness", "evenness", "wmean_LDMC_global", "wmean_SLA_global", "wmean_LTH_global", "wmean_LA_global", "wmean_height_global", "wmean_CN_global", "wmean_LDMC_local", "wmean_SLA_local", "wmean_LTH_local", "wmean_LA_local", "wmean_height_local", "wmean_CN_local")
   friend <- forbcom$turfID == R$turfID & forbcom$Year == 2011
  if(all (!friend)) {print(R$turfID)
     return(rep(NA, length(cols)))}
   stopifnot(sum(friend) == 1)
   
   f <- forbcom[friend,]
   x <- R[,cols] - f[,cols]
   unlist(x)
 })
timedeltacalc <- as.data.frame(t(timedeltacalc))
colnames(timedeltacalc) <- paste0("delta", colnames(timedeltacalc))
timedelta <- cbind((forbcom[forbcom$Year != 2011,]), timedeltacalc)

wholecom <- wholecom %>% 
  mutate(intra_SLA = wmean_SLA_local - wmean_SLA_global)
