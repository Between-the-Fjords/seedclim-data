##############################################################################
#Script for paper on effect of graminoid removal on plant community properties
##############################################################################

library(tidyverse)
library(DBI)
library(dbplyr)
library(SDMTools)
con <- src_mysql(group = "seedclim", dbname = "seedclimComm", password = "password")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############### Cover data ###############
## ---- my.GR.data.import ---- 
problems <- read.csv("~/OneDrive - University of Bergen/Research/FunCaB/Data/speciesCorrections.csv", sep = ";", stringsAsFactors = FALSE) %>%
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
  select(siteID, blockID, plotID = destinationPlotID, turfID, TTtreat, GRtreat, Year = year, species, cover, Temperature_level, Precipitation_level, recorder, totalVascular, totalBryophytes, functionalGroup, vegetationHeight, mossHeight, litter) %>%
  mutate(TTtreat = factor(TTtreat), GRtreat = factor(GRtreat)) %>%
  ungroup()

my.GR.data <- my.GR.data %>% filter(Year > 2009, TTtreat == "TTC"|GRtreat == "RTC"|GRtreat == "TTC")

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
#save(weather, file = "~/Desktop/weather.Rdata")

my.GR.data <- my.GR.data %>%
  left_join(weather, by = "siteID") 


#### code to get rid of turfs that have been attacked by ants or cows ####
lowcover <- my.GR.data %>% group_by(turfID, Year, functionalGroup) %>% mutate(sumcover = sum(cover)) %>% filter(sumcover< 25, functionalGroup == "forb") %>% distinct(siteID, turfID,Year,sumcover)

#my.GR.data %>% filter(turfID %in% c("Ovs2RTC", "Ovs3RTC", "126 TTC")) %>% group_by(turfID, Year) %>% mutate(sumcover = sum(cover)) %>% distinct(siteID, turfID,Year, sumcover)

#my.GR.data <- filter(my.GR.data, !siteID %in% c("Ovs2RTC", "Ovs3RTC"))
## ---- my.GR.data.end ----

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############### TRAITS ###############

## ---- Traits.data.import ---- 

# source Ragnhild's trait data
source("ragnhild_trait_data/load_traits.R") # warning here is fine, it just means those spp didn't have CN data collected

#load from data base
traits <- tbl(con, "taxon") %>% 
  collect() %>% 
  left_join(tbl(con, "moreTraits"), copy = TRUE, by = "species") %>% 
  select(species, Lower, Nem, BNem, SBor, MBor, Nbor, LAlp, MAlp, HAlp, Upper, Min_height, Max_height, height, leafSize, seedMass)

traits <- traits[traits$species %in% my.GR.data$species,]
traits$HAlp <- as.numeric(traits$HAlp)
traits[,(4:11)][is.na(traits[,4:11])] <- 0 #change this to something more interpretable!
traits <- traits %>% 
  rowwise() %>% 
  mutate(abundance = sum(Nem, BNem, SBor, MBor, Nbor, LAlp, MAlp, HAlp, na.rm = TRUE)) %>%
  mutate(specialism = factor(case_when(
    Nem == 0 & BNem == 0 & SBor == 0 & LAlp == 1 ~ "alpine", 
    HAlp == 0 & MAlp == 0 & LAlp == 0 & Nem == 1 ~ "lowland", 
    abundance > 5.5 ~ "generalist",
    abundance < 5.5 ~ "other"))) %>% 
  select(-c(Lower, Nem, BNem, SBor, MBor, Nbor, LAlp, MAlp, HAlp, Upper))

head(traits)

identical(as.character(traits$species), my.GR.data$species) #this should be identical, but if it's not it just means we are lacking the trait information for some species
identical(as.character(traitdata$species), my.GR.data$species) #this should be identical

# "Agr.can"  "Cre.pal"  "Frag.vir" "Gen.sp."  "Hie.ore"  "Sch.gig" the species that don't appear in the traits but do appear in cover


# adding traits to my.GR.data
my.GR.data <- my.GR.data %>%
  left_join(traits, by = "species") %>%
  left_join(traitdata, by = c("species", "siteID")) %>%
  mutate(temp = recode(siteID, Ulvhaugen = 6.5, Lavisdalen = 6.5,  Gudmedalen = 6.5, Skjellingahaugen = 6.5, Alrust = 8.5, Hogsete = 8.5, Rambera = 8.5, Veskre = 8.5, Fauske = 10.5, Vikesland = 10.5, Arhelleren = 10.5, Ovstedal = 10.5)) %>%
  mutate(Temperature_level = recode(siteID, Ulvhaugen=6.17, Lavisdalen=6.45, Gudmedalen=5.87, Skjellingahaugen=6.58, Alrust=9.14, Hogsete=9.17, Rambera=8.77, Veskre=8.67, Fauske=10.3, Vikesland=10.55, Arhelleren=10.60, Ovstedal=10.78))%>%
  mutate(Precipitation_level= recode(siteID, Ulvhaugen=596, Lavisdalen=1321, Gudmedalen=1925, Skjellingahaugen=2725, Alrust=789, Hogsete=1356, Rambera=1848, Veskre=3029, Fauske=600, Vikesland=1161, Arhelleren=2044, Ovstedal=2923))%>%
  mutate(precip = recode(siteID, Ulvhaugen = 600, Alrust = 600, Fauske = 600, Lavisdalen = 1200, Hogsete = 1200, Vikesland = 1200, Gudmedalen = 2000, Rambera = 2000, Arhelleren = 2000, Skjellingahaugen = 2700, Veskre = 2700, Ovstedal = 2700)) 
  
#comment these out depending on the analysis you want to run
my.GR.data$TTtreat <- factor(as.character(my.GR.data$TTtreat), levels = c("TTC", "RTC"))
my.GR.data$functionalGroup <- plyr::mapvalues(my.GR.data$functionalGroup, from = "pteridophyte", to = "forb")
my.GR.data$functionalGroup <- plyr::mapvalues(my.GR.data$functionalGroup, from = "woody", to = "forb")
my.GR.data$specialism <- plyr::mapvalues(my.GR.data$specialism, from = "lowland", to = "other")


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
  mutate(sumcover = sum(cover),
         wmeanLDMC_local = weighted.mean(LDMC_mean, cover, na.rm = TRUE),
         wmeanSLA_local = weighted.mean(SLA_mean, cover, na.rm = TRUE),
         wmeanLTH_local = weighted.mean(Lth_mean, cover, na.rm = TRUE),
         wmeanLA_local = weighted.mean(LA_mean, cover, na.rm = TRUE),
         wmeanheight_local = weighted.mean(Height_mean, cover, na.rm = TRUE),
         wmeanCN_local = weighted.mean(CN_mean, cover, na.rm = TRUE),
         wmeanseedMass_local = weighted.mean(seedMass, cover, na.rm = TRUE),
         cwvLDMC_local = wt.var(LDMC_mean, wt = cover),
         cwvSLA_local = wt.var(SLA_mean, wt = cover),
         cwvLTH_local = wt.var(Lth_mean, wt = cover),
         cwvLA_local = wt.var(LA_mean, wt = cover),
         cwvheight_local = wt.var(Height_mean, wt = cover),
         cwvCN_local = wt.var(CN_mean, wt = cover),
         cwvseedMass_local = wt.var(seedMass, wt = cover)) %>% 
  ungroup() %>%
  select(-(height:seedMass), -(Max_height:SLA), -recorder) %>%
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
  cols <- c("sumcover", "wmeanseedMass_local", "diversity", "richness", "evenness", "wmeanLDMC_local", "wmeanSLA_local", "wmeanLTH_local", "wmeanLA_local", "wmeanheight_local", "wmeanCN_local", "cwvLDMC_local", "cwvSLA_local", "cwvLTH_local", "cwvLA_local", "cwvheight_local", "cwvCN_local", "cwvseedMass_local")
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
  cols <- c("sumcover", "wmeanseedMass_local", "diversity", "richness", "evenness", "wmeanLDMC_local", "wmeanSLA_local", "wmeanLTH_local", "wmeanLA_local", "wmeanheight_local", "wmeanCN_local", "cwvLDMC_local", "cwvSLA_local", "cwvLTH_local", "cwvLA_local", "cwvheight_local", "cwvCN_local", "cwvseedMass_local")
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
timedelta <- filter(timedelta, deltasumcover > -80)

