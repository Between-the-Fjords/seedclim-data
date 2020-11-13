#################################
#################################
 ### PHENOLOGY DATA (2014)   ###
#################################
#################################

library("tidyverse")
library("lubridate")

source("MyPhenologyFunctions2014_DOY.R")

#### READ IN DATA ####
## Turfs 2014
turfs.14 <- read.table("Data/turfs.csv", sep=";", header=TRUE)
turfs.14 <- turfs.14 %>%
  as.tibble() %>% 
  mutate(Prec_value.sc = scale(Prec_value),
         Temp_value.sc = scale(Temp_value),
         destPrec_value.sc = scale(destPrec_value),
         destTemp_value.sc = scale(destTemp_value)) %>% 
  mutate(d.date.osm = dmy(d.date.osm),
         o.date.osm = dmy(o.date.osm),
         d.dosm = yday(d.date.osm),
         o.dosm = yday(o.date.osm)) %>% 
  mutate(SMDiff = d.dosm - o.dosm) %>% 
  filter(Year != 2015)


## Trait data
traits <- read.table("Data/trait.csv", sep=";", header=TRUE)


## Phenology data
data <- read.table("Data/Phenology2014_1.csv", sep=";", header=TRUE, fill=TRUE) # data with 1 for vegetative
data.long <- data %>% 
  # convert dates, doy, weeknr
  mutate(date = dmy(date),
         doy = yday(date),
         week.nr = substring(week, 2, 3)) %>% 
  # Remove RTC plot from data set, with level
  filter(TTtreat != "RTC") %>% 
  #### CALCULATE SUM OF BUD, FLOWER, SEED AND RIPE SEED ####
  # By turfID
  # paste(c(names(data[23:247])), collapse = ", ") # get list of species with comma
  select(-SiteShort, -destSiteShort, -Temperature_level, -Precipitation_level, -destT_level, -destP_level, -destblock, -destBlockID, -plot.id, -originPlotID, -destPlotID, -treatment, -TTtreat, -siteID, -destSiteID, -Year, -date, -week, -weather, -name, -seed.coll, -week.nr) %>% 
  gather(key = "species", value = "value", -turfID, -subplot, -doy) %>%
  filter(!is.na(value)) %>% 
  group_by(turfID, species, doy) %>%
  summarise(n = n(), sum = sum(value, na.rm=TRUE)) %>% 
  # separate species and phenological stage (bud, flower, seed and ripe seed)
  ungroup() %>% 
  mutate(pheno.stage = substr(species, nchar(species), nchar(species)),
         species = substr(species, 1, nchar(species)-2))


#### CALCULATE FIRST, END, PEAK AND DURATION OF EACH PHENO.STAGE ####
pheno.variables <- data.long %>%
  filter(pheno.stage != "v") %>%              # do not need vegetative
  group_by(turfID, species, pheno.stage) %>%  # group by turfID, species and phenological stage to calculate first, end etc for each stage
  mutate(minDoy = min(doy, na.rm = TRUE)) %>% # calculate min doy
  group_by(minDoy, add = TRUE) %>% # add variable but remember the previous groups
  filter(sum > 0) %>%
  summarize(first = first(doy), end = last(doy), peak = doy[which.max(sum)]) %>%
  mutate(duration = end-(first-1)) %>%           # calculate duration
  mutate(first = ifelse(first <= minDoy, NA, first)) %>%  # remove if plant is flowering in the first week
  ungroup() %>% 
  select(-minDoy)  # remove this variable

# run this if only include community data
# pheno.var <- pheno.var[pheno.var$turfID %in% turfs.14$turfID,]

# get rid of invaders (plants that were not there in 2009)
#pheno.variables2 <- get.rid.of.invaders(pheno.variables)
#head(pheno.variables2)



#### MAKE LONG DATA SET
# Make long data set for pheno.var: first, end, peak, duration
# used for analysis
pheno.long <- pheno.variables %>%
  gather(key = pheno.var, value = value, -turfID, -species, -pheno.stage) %>% 
  filter(!is.na(value)) %>% 
  # add meta data from turfs and traits
  left_join(turfs.14, by = c("turfID")) %>% 
  
  #### CALCULATE EVENT IN DAYS SINCE SNOWMELT ####
  select(turfID, blockID, destSiteID, destBlockID, species, TTtreat, value, pheno.var, pheno.stage, d.dosm, SMDiff) %>%
  mutate(dssm = value - d.dosm) %>% 
  rename(doy = value) %>% 
  # pheno.unit
  gather(key = pheno.unit, value = value, -turfID, -blockID, -destSiteID, -destBlockID, -species, -TTtreat, -pheno.var, -pheno.stage, -d.dosm, -SMDiff) %>% 
  
  ### PRETTIFY THE DATA
  # add meta data from turfs and traits
  left_join(turfs.14, by = c("turfID", "blockID", "TTtreat", "destSiteID", "destBlockID", "SMDiff", "d.dosm")) %>% 
  mutate(TTtreat = plyr::mapvalues(TTtreat, c("TTC", "TT2", "TT3", "TT4"), c("Control", "Warmer", "LaterSM", "WarmLate")),
         TTtreat = factor(TTtreat, levels = c("Control", "Warmer", "LaterSM", "WarmLate"))) %>% 
  mutate(newTT = plyr::mapvalues(newTT, c("control", "TT2", "TT3", "TT4"), c("Control", "Warmer", "LaterSM", "WarmLate")),
         newTT = factor(newTT, levels = c("Control", "Warmer", "LaterSM", "WarmLate"))) %>%
  mutate(siteID = factor(siteID, levels = c("Ulvhaugen", "Alrust", "Lavisdalen", "Gudmedalen", "Skjellingahaugen", "Hogsete", "Rambera", "Veskre"))) %>%
  mutate(pheno.stage = plyr::mapvalues(pheno.stage, c("b", "f", "s", "r"), c("Bud", "Flower", "Seed", "RipeSeed"))) %>%
  mutate(pheno.stage = factor(pheno.stage, levels = c("Bud", "Flower", "Seed", "RipeSeed"))) %>% 
  mutate(pheno.unit = plyr::mapvalues(pheno.unit, c("doy", "dssm"), c("DOY", "DaysSinceSM")))

save(pheno.long, file = "Phenology2014.Rdata")
