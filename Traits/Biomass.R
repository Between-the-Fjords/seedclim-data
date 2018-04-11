##### Biomass data #####
library(tidyverse)

biomass_others <- read.csv("Traits/data/biomass_2016_others.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)

biomass_others <- biomass_others%>%
  select(siteID, plotID, functional.group, dry.weight)%>%
  mutate(functional.group = replace(functional.group, functional.group=="bryophtyes", "bryophytes"))


biomass_forbs <- read.csv("Traits/data/biomass_2016_forbs.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)

biomass_forbs <- biomass_forbs%>%
  select(siteID, plotID, functional.group, dry.weight)%>%
  group_by(siteID, plotID)%>%
  summarise(dry.weight=sum(dry.weight))%>%
  ungroup()%>%
  mutate(functional.group="forb")

# Merge the two biomass datasets together #

biomass <- bind_rows(biomass_others, biomass_forbs)%>%
  mutate(site = recode(siteID, Ulvehaugen = "Ulv", Alrust = "Alr", Fauske = "Fau", Lavisdalen = "Lav", Hogsete = "Hog", Vikesland = "Vik", Gudmedalen = "Gud", Rambera = "Ram", Arhelleren = "Arh", Skjellingahaugen = "Skj", Veskre = "Ves", Oustedal = "Ovs")) %>%
  mutate(Block= substr(plotID, 3,3))%>%
  mutate(turf = substr(plotID, 1,2))%>%
  mutate(turfID=paste0(site, Block, turf))%>%
  select(-Block, -turf)%>%
  group_by(turfID)%>%
  filter(functional.group!="litter")%>%
  mutate(total.biomass=sum(dry.weight))%>%
  ungroup()

biomass %>%
  slice(which.max(total.biomass))

