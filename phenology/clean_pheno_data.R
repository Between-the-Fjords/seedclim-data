##################################
  #### CLEAN PHENOLOGY DATA ####
##################################

# load libraries
library("tidyverse")
library("lubridate")
library("dataDownloader")
library("RSQLite")

source("phenology/pheno_functions.R")

## DOWNLOAD DATA
# Use this code to download the data directly from OSF and unzip the file.
# Alternatively you can find the raw data here: https://osf.io/npfa9/

# phenology
# get_file(node = "npfa9",
#          file = "Community_phenology_raw_2014-2015.zip",
#          path = "phenology/data",
#          remote_path = "Phenology_data/Raw_data")

#unzip("phenology/data/Community_phenology_raw_2014-2015.zip")

# community data
# get_file(node = "npfa9",
#          file = "seedclim.sqlite",
#          path = "phenology/data",
#          remote_path = "Community_data")


## IMPORT AND CLEAN DATA

#### 2014
# Turfs 2014
turfs.14 <- read_delim(file = "phenology/data/2014/Community_phenology_turf_table_2014.csv", delim = ";", col_names = TRUE) %>% 
  select(turfID, blockID, d.date.osm) %>% 
  mutate(d.date.osm = dmy(d.date.osm)) %>% 
  rename(originBlockID = blockID, snowmelt_date = d.date.osm)

## Phenology 2014 data
pheno14_raw <- read_delim(file = "phenology/data/2014/Community_phenology_2014.csv", delim =";", col_names = TRUE) # data with 1 for vegetative
phenology_2014 <- pheno14_raw %>% 
  # remove unused cols
  select(-SiteShort, - destSiteShort, -destT_level, -destP_level, -destblock, -plot.id, -originPlotID, -destPlotID, -treatment, -seed.coll) %>% 
  # Remove RTC plot from data set, with level
  filter(TTtreat != "RTC") %>% 
  rename(originSiteID = siteID, destinationBlockID = destBlockID, temperature_level = Temperature_level, precipitation_level = Precipitation_level, destinationSiteID = destSiteID, year = Year) %>% 
  # CALCULATE SUM OF BUD, FLOWER, SEED AND RIPE SEED ####
pivot_longer(cols = Bis.viv.v:Tha.alp.r, names_to = "stage", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  group_by(originSiteID, destinationSiteID, temperature_level, precipitation_level, year, date, week, weather, name, destinationBlockID, turfID, TTtreat, stage) %>%
  summarise(value = sum(value, na.rm=TRUE)) %>% 
  # separate species and phenological stage (bud, flower, seed and ripe seed)
  ungroup()  %>% 
  mutate(pheno_stage = substr(stage, nchar(stage), nchar(stage)),
         pheno_stage = recode(pheno_stage, "v" = "vegetative", "b" = "bud", "f" = "flower", "s" = "seed", "r" = "ripe_Seed"),
         species = substr(stage, 1, nchar(stage)-2),
         treatment = recode(TTtreat, "TTC" = "control", "TT2" = "warm", "TT3" = "wet", "TT4" = "warm_wet"),
         date = dmy(date),
         doy = yday(date)) %>% 
  select(-stage) %>% 
  left_join(turfs.14, by = "turfID")


## Phenology 2015 data
#Head
dath1 <- ReadInHeadPhenology15("phenology/data/2015/DataSheet2015Hog.csv", "Hogsete") %>% as_tibble()
dath2 <- ReadInHeadPhenology15("phenology/data/2015/DataSheet2015Ram.csv", "Rambaera")  %>% as_tibble()
dath3 <- ReadInHeadPhenology15("phenology/data/2015/DataSheet2015Ves.csv", "Veskre")  %>% as_tibble()
dath4 <- ReadInHeadPhenology15("phenology/data/2015/DataSheet2015Lav.csv", "Lavisdalen")  %>% as_tibble()
dath5 <- ReadInHeadPhenology15("phenology/data/2015/DataSheet2015Gud.csv", "Gudmedalen")  %>% as_tibble()
dath6 <- ReadInHeadPhenology15("phenology/data/2015/DataSheet2015Skj.csv", "Skjellingahaugen")  %>% as_tibble()
meta.pheno <- rbind(dath1, dath2, dath3, dath4, dath5, dath6) %>% 
  mutate(date = dmy(date),
         doy = as.numeric(doy)) %>% 
  rename(destinationSiteID = Site) %>% 
  filter(!is.na(date))

#Body
dat1 <- ReadInBodyPhenology15("phenology/data/2015/DataSheet2015Hog.csv", "Hogsete")
dat2 <- ReadInBodyPhenology15("phenology/data/2015/DataSheet2015Ram.csv", "Rambaera")
dat3 <- ReadInBodyPhenology15("phenology/data/2015/DataSheet2015Ves.csv", "Veskre")
dat4 <- ReadInBodyPhenology15("phenology/data/2015/DataSheet2015Lav.csv", "Lavisdalen")
dat5 <- ReadInBodyPhenology15("phenology/data/2015/DataSheet2015Gud.csv", "Gudmedalen")
dat6 <- ReadInBodyPhenology15("phenology/data/2015/DataSheet2015Skj.csv", "Skjellingahaugen")
pheno15_raw <- rbind(dat1, dat2, dat3, dat4, dat5, dat6) %>% 
  as_tibble()

# Turfs 2015
turfs.15 <- read_delim(file = "phenology/data/2015/turfs.csv", delim = ";", col_names = TRUE) %>% 
  select(siteID, TTtreat, blockID, turfID, destBlockID, Temperature_level, Precipitation_level, destBlockID, destSiteID, newTT, d.date.osm) %>% 
  rename(originSiteID = siteID, originBlockID = blockID, destinationBlockID = destBlockID, temperature_level = Temperature_level, precipitation_level = Precipitation_level, destinationSiteID = destSiteID, treatment = newTT, snowmelt_date = d.date.osm) %>% 
  mutate(treatment = recode(treatment, "TT2" = "warm", "TT3" = "wet", "TT4" = "warm_wet"),
         snowmelt_date = dmy(snowmelt_date)) %>% 
  filter(!is.na(snowmelt_date))
  

#### CALCULATE SUM OF BUD, FLOWER, SEED AND RIPE SEEDS PER TURFID AND SPECIES ####
phenology_2015 <- CalcSums(pheno15_raw) %>% 
  select(Site, turfID, species, doy, week, nr.b, nr.f, nr.s, nr.r) %>% 
  mutate(Site = recode(Site, "Rambaera" = "Rambera")) %>% 
  rename(destinationSiteID = Site, bud = nr.b, flower = nr.f, seed = nr.s, ripe_seed = nr.r) %>% 
  pivot_longer(cols = c("bud", "flower", "seed", "ripe_seed"), names_to = "pheno_stage", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(year = 2015) %>% 
  left_join(turfs.15, by = c("turfID", "destinationSiteID")) %>% 
  left_join(meta.pheno, by = c("week", "doy", "destinationSiteID"))

# community data
# get all species that were in each turf in 2009, no invaders wanted
con <- dbConnect(SQLite(), dbname = "database/seedclim.sqlite")

originial_taxa <- tbl(con, "turf_community") %>% 
  left_join(tbl(con, "turfs"), by = "turfID") %>% 
  filter(year == 2009) %>% 
  select(-RTtreat, -GRtreat, -destinationPlotID, -originPlotID, -cf) %>% 
  collect() %>% 
  distinct(turfID, species) %>% 
  mutate(status = "original")

# Combine the two datasets
phenology <- phenology_2014 %>% 
  bind_rows(phenology_2015) %>% 
  select(originBlockID, originSiteID, turfID, destinationBlockID, destinationSiteID, year, date, doy, week, TTtreat, treatment, species, pheno_stage, value, temperature_level, precipitation_level, snowmelt_date, weather, name) %>% 
  # flag status of species
  left_join(originial_taxa, by = c("turfID", "species")) %>% 
  mutate(status = if_else(is.na(status), "invader", status)) %>% 
  rename(blockID_origin = originBlockID, SiteID_origin = originSiteID, blockID_dest = destinationBlockID, siteID_dest = destinationSiteID, temperature_level_origin = temperature_level, precipitation_level_origin = precipitation_level)
  
dir.create("phenology/clean_data")
write_csv(phenology, path = "phenology/clean_data/Community_phenology_2014-2015.csv")

