##### Biomass data #####
library(tidyverse)

biomass_others <- read.csv("Traits/data/biomass_2016_others.csv", header=TRUE, sep=";")
str(biomass)

biomass_others <- biomass_others%>%
  select(siteID, plotID, functional.group, dry.weight)%>%
  mutate(site = recode(siteID, Ulvehaugen = "Ulv", Alrust = "Alr", Fauske = "Fau", Lavisdalen = "Lav", Hogsete = "Hog", Vikesland = "Vik", Gudmedalen = "Gud", Rambera = "Ram", Arhelleren = "Arh", Skjellingahaugen = "Skj", Veskre = "Ves", Oustedal = "Ovs")) %>%
  mutate(Block= substr(plotID, 3,3))%>%
  mutate(turf = substr(plotID, 1,2))%>%
  mutate(turfID=paste0(site, Block, turf))%>%
  select(-Block, -turf)

biomass_forbs <- read.csv("Traits/data/biomass_2016_forbs.csv", header=TRUE, sep=";")

biomass_forbs <- biomass_forbs%>%
  select(siteID, plotID, functional.group, dry.weight)%>%
  mutate(site = recode(siteID, Ulvehaugen = "Ulv", Alrust = "Alr", Fauske = "Fau", Lavisdalen = "Lav", Hogsete = "Hog", Vikesland = "Vik", Gudmedalen = "Gud", Rambera = "Ram", Arhelleren = "Arh", Skjellingahaugen = "Skj", Veskre = "Ves", Oustedal = "Ovs")) %>%
  mutate(Block= substr(plotID, 3,3))%>%
  mutate(turf = substr(plotID, 1,2))%>%
  mutate(turfID=paste0(site, Block, turf))%>%
  select(-Block, -turf)%>%
  group_by(turfID)%>%
  mutate(sum.weight=sum(dry.weight))%>%
  select(-dry.weight)%>%
  rename(dry.weight=sum.weight)%>%
  ungroup()%>%
  distinct()


biomass <- rbind(biomass_others, biomass_forbs)

biomass <- biomass %>%
  group_by(turfID)%>%
  mutate(total.biomass=sum(dry.weight))

