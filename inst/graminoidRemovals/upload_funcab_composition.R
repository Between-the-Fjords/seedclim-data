library(tidyverse)
library(dplyr)
library(readxl)
#library(DBI)
library(dbplyr)

dict_TTC_turf <- read_delim(delim = ";", file = 
"TTtreat;turfID
51 TTC;Fau1C
57 TTC;Fau2C
68 TTC;Fau4C
73 TTC;Fau5C
29 TTC;Alr1C
31 TTC;Alr2C
134 TTC;Vik2C
140 TTC;Vik3C
141 TTC;Vik4C
146 TTC;Vik5C
101 TTC;Hog1C
110 TTC;Hog2C
115 TTC;Hog3C
286 TTC;Ovs1C
291 TTC;Ovs2C
297 TTC;Ovs3C
211 TTC;Arh1C
222 TTC;Arh3C
226 TTC;Arh4C
263 TTC;Ves1C
281 TTC;Ves4C
194 TTC;Ram4C
198 TTC;Ram5C
6 TTC;Ulv2C
11 TTC;Ulv3C
236 TTC;Skj1C
243 TTC;Skj2C
246 TTC;Skj3C
251 TTC;Skj4C
506 TTC;Gud5C
511 TTC;Gud12C
516 TTC; Gud13C
")


con <- src_mysql(group = "seedclim", dbname = "seedclimComm", password = "password")
#setwd("/Users/fja062/Documents/seedclimComm/seedclimComm/")

gudfun2015 <- read_excel("/Volumes/fja062/PhD/Data/funcab/funcab_Gudmedalen.xlsx", col_types = "text")


funcab_2015 <- read_delim("/Volumes/fja062/PhD/Data/funcab/funcab_composition_2015-utenGud.csv", delim = ";", col_types = cols(.default = "c")) # or \t

funcab_2016 <- read_delim("/Volumes/fja062/PhD/Data/funcab/funcab_composition_2016.csv", delim = ";", col_types = cols(.default = "c"))

funcab_2017 <- read_delim("~/Documents/seedclimComm/seedclimComm/ragnhild_trait_data/funcab_composition_2017.csv", delim = ";", col_types = cols(.default = "c"))


composition <- funcab_2016 %>% 
  bind_rows(funcab_2015) %>% 
  bind_rows(gudfun2015) %>% 
  bind_rows(funcab_2017) %>% 
  select(c(siteID:year), c(totalGraminoids:mossHeight), litter, acro, pleuro, c(`Ach mil`:`Vis vul`)) %>%
  select_if(colSums(!is.na(.)) > 0) %>% 
  gather(c("Ach mil":"Vio sp"), key = "species", value = "cover") %>% 
  mutate(species = gsub("\\ |\\_", ".", species)) %>% 
  filter(subPlot == "%") %>% 
  mutate(turfID = plyr::mapvalues(turfID, from = dict_TTC_turf$TTtreat, to = dict_TTC_turf$turfID)) %>% 
  mutate(turfID = if_else(blockID == 16 & siteID == "Gudmedalen", gsub("16", "5", turfID), turfID),
         blockID = if_else(blockID == 16 & siteID == "Gudmedalen", gsub("16", "5", blockID), blockID)
  ) %>% 
  mutate_at(vars(cover, year, totalGraminoids:pleuro), as.numeric) %>% 
  mutate(blockID = paste0(str_sub(siteID, 1, 3), blockID))



#### #### #### #### 
#### CLEANING ####
composition <- composition %>% 
  rename(Year = year) %>% 
  mutate(turfID = recode(turfID, "Alr4FGB" = "Alr5C")) %>% 
  filter(!(blockID == "Alr4" & Year == 2015 & siteID == "Alrust")) %>% 
  mutate(turfID = if_else(blockID == "Alr3" & Year == 2015 & Treatment == "C", "Alr3C", turfID))


# replace species names where mistakes have been found in database
problems <- read.csv("~/Documents/seedclimComm/seedclimComm/speciesCorrections.csv", sep = ";", stringsAsFactors = FALSE) %>%
  filter(!old %in% c("Vio.can", "Com.ten", "Sel.sel")) %>%
  filter(cover != "WHAT HAPPENED") %>%
  mutate(cover = as.numeric(cover))

prob.sp <- problems %>%
  filter(!is.na(Year)) %>% 
  select(-functionalGroup)

# merger dictionary from database
mergedictionary <- tbl(con, "mergedictionary") %>% 
  collect() %>% 
  rename("old" = "oldID", "new" = "newID")

taxon <-tbl(con, "taxon") %>% 
  select(species, functionalGroup) %>% 
  collect()

prob.sp.name <- problems %>% 
  filter(is.na(Year)) %>% 
  select(old, new) %>% 
  bind_rows(mergedictionary)

composition <- composition %>% 
  left_join(prob.sp, by = c("Year", "turfID", "siteID", "species" = "old"), suffix = c("", ".new")) %>%
  mutate(species = coalesce(new, species),
         cover = coalesce(cover.new, cover)) %>% 
  select(-new, -cover.new, -subPlot) %>% 
  mutate(species = plyr::mapvalues(species, from = prob.sp.name$old, to = prob.sp.name$new)) %>% 
  left_join(taxon) %>%
  group_by_at(vars(-cover)) %>% 
  summarise(cover = sum(cover, na.rm = TRUE)) %>% 
  ungroup()


metadata <- composition %>% 
  filter(Treatment == "FGB", Year > 2015) %>% 
  select(-species, -cover, -functionalGroup) %>% 
  distinct()

ttcs <- composition %>% 
  filter(Treatment == "C") %>% 
  group_by(Year, turfID) %>% 
  mutate(sumcover = sum(cover)) %>% 
  filter(sumcover == 0) %>% 
  ungroup() %>% 
  select(-species, -cover, -functionalGroup) %>% 
  distinct()


# collect gr data from database
my.GR.data <- my.GR.data %>% 
  filter(Year > 2014, TTtreat == "TTC") %>% 
  filter(turfID %in% dict_TTC_turf$TTtreat) %>% # or semi_join()
  mutate(Treatment = "C", TTtreat = turfID, turfID = paste0(blockID, Treatment)) %>%
  select(-c(plotID, Temperature_level, Precipitation_level, recorder, totalVascular, ID)) %>% 
  filter(!is.na(cover)) #### fix this!
  

comp2 <- composition %>% 
  filter(cover > 0) %>% 
  full_join(my.GR.data, by = c("siteID", "blockID", "turfID", "Treatment", "Year", "functionalGroup", "species"), suffix = c("", ".new")) %>% 
  bind_rows(metadata) %>% 
  left_join(ttcs) %>% 
  mutate(cover = if_else(cover == 0|is.na(cover), cover.new, cover),
         mossHeight = if_else(mossHeight == 0|is.na(mossHeight), mossHeight.new, mossHeight),
         vegetationHeight = if_else(vegetationHeight == 0|is.na(vegetationHeight), vegetationHeight.new, vegetationHeight),
         totalBryophytes = if_else(totalBryophytes == 0|is.na(totalBryophytes), totalBryophytes.new, totalBryophytes),
         litter = if_else(litter == 0|is.na(litter), litter.new, litter)) %>% 
  select(-totalBryophytes.new, -vegetationHeight.new, -mossHeight.new, -cover.new, -litter.new)

### still dropping plots by having the cover > 0 in the code.   
  
# .Last.value
comp2$functionalGroup <- plyr::mapvalues(comp2$functionalGroup, from = "pteridophyte", to = "forb")
comp2$functionalGroup <- plyr::mapvalues(comp2$functionalGroup, from = "woody", to = "forb")


comp2 <- comp2 %>% 
  filter(!grepl("RTC", turfID)) %>% 
  group_by(turfID, Year) %>% 
  mutate(totalBryophytes = if_else(is.na(totalBryophytes), pleuro + acro, totalBryophytes)) %>% 
  ungroup() %>% 
  mutate(turfID = if_else(grepl("TTC", turfID), turfID, substring(turfID, 4, n())),
         Treatment = gsub("FG^", "GF", Treatment),
         Treatment = gsub(" ", "", Treatment),
         blockID = gsub("[^[:digit:]]", "", blockID),
         turfID = paste0(str_sub(siteID, 1, 3), turfID),
         species = gsub(" ", ".", species),
         bryophyteCov = if_else(grepl("B", Treatment), 0, totalBryophytes),
         forbCov = if_else(grepl("F", Treatment), 0, totalForbs),
         graminoidCov = if_else(grepl("G", Treatment), 0, totalGraminoids)) %>% 
  select(-totalGraminoids, -totalForbs, -totalBryophytes)


# source Ragnhild's trait data
source("~/Documents/seedclimComm/seedclimComm/ragnhild_trait_data/load_traits.R") # warning here is fine, it just means those spp didn't have CN data collected

traitdata <- traitdata %>% 
  select(siteID, species, Height_mean, LA_mean)

# adding traits to my.GR.data
composition <- comp2 %>%
  left_join(traitdata, by = c("species", "siteID")) %>%
  mutate(functionalGroup = if_else(is.na(functionalGroup), "forb", functionalGroup))

#Species richness
library(vegan)

composition <- composition %>%
  group_by(turfID, Year, functionalGroup) %>%
  mutate(richness = sum(n_distinct(species))) %>% 
  mutate(diversity = diversity(cover, index = "shannon")) %>% 
  mutate(evenness = (diversity/log(richness)))

composition <- composition %>%
  group_by(turfID, siteID, functionalGroup, Year) %>% 
  filter(!is.na(cover)) %>%
  mutate(wmH= weighted.mean(Height_mean, cover, na.rm=TRUE)) %>% #, wmLA= weighted.mean(LA_mean, cover, na.rm=TRUE)
  select(-Height_mean, -LA_mean, -TTtreat, -species, -cover) %>% 
  distinct(turfID, Year, functionalGroup, .keep_all = TRUE) %>% 
  group_by(turfID, siteID, Year) %>% 
  spread(key =functionalGroup, value = wmH)



