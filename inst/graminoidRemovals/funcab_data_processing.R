##############################################################################
# Community data for all funcab analyses
##############################################################################

library(tidyverse)
library(DBI)
library(dbplyr)
library(SDMTools)
library(readxl)

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ---- database.controls.import ---- 
# replace species names where mistakes have been found in database
problems <- read.csv("~/OneDrive - University of Bergen/Research/FunCaB/Data/speciesCorrections.csv", sep = ";", stringsAsFactors = FALSE) %>%
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
  ungroup() %>% 
  filter(Year > 2014, TTtreat == "TTC"|GRtreat == "TTC")


my.GR.data <- my.GR.data %>%
  mutate(functionalGroup = if_else(species %in% c("Gen.sp.", "Cre.pal", "Frag.vir", "Sch.gig", "Ste.bor", "Hie.ore", "Sel.sel."), "forb", functionalGroup),
         functionalGroup = if_else(species %in% c("Agr.can", "Phl.sp"), "graminoid", functionalGroup)) %>% 
  mutate(vegetationHeight = if_else(Year == 2015, vegetationHeight*10, vegetationHeight),
         mossHeight = if_else(Year == 2015, mossHeight*10, mossHeight))

levels(my.GR.data$TTtreat) <- c(levels(my.GR.data$TTtreat),levels(my.GR.data$GRtreat))
my.GR.data$TTtreat[my.GR.data$TTtreat == ""| is.na(my.GR.data$TTtreat)] <- my.GR.data$GRtreat[my.GR.data$TTtreat == ""| is.na(my.GR.data$TTtreat)] # merge the GRtreat and TTtreat into one column
my.GR.data$GRtreat <- NULL


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
  filter(turfID %in% dict_TTC_turf$TTtreat) %>% # or semi_join()
  mutate(Treatment = "C", TTtreat = turfID, turfID = paste0(blockID, Treatment)) %>%
  select(-c(plotID, Temperature_level, Precipitation_level, recorder, totalVascular)) %>% 
  filter(!is.na(cover))

# replace species names where mistakes have been found in database
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

my.GR.data <- probfixes(my.GR.data, prob.sp.name$old, prob.sp.name$new)

##########################
#### load funcab data ####

gudfun2015 <- read_excel("~/OneDrive - University of Bergen/Research/FunCaB/Data/funcab_Gudmedalen.xlsx", col_types = "text")

funcab_2015 <- read_delim("~/OneDrive - University of Bergen/Research/FunCaB/Data/funcab_composition_2015-utenGud.csv", delim = ";", col_types = cols(.default = "c")) # ; or \t

funcab_2016 <- read_delim("~/OneDrive - University of Bergen/Research/FunCaB/Data/funcab_composition_2016.csv", delim = ";", col_types = cols(.default = "c"))

funcab_2017 <- read_delim("~/OneDrive - University of Bergen/Research/FunCaB/Data/funcab_composition_2017.csv", delim = ";", col_types = cols(.default = "c"))

scBryo <- read_excel("~/OneDrive - University of Bergen/Research/FunCaB/Data/2017seedclimBryophyte.xlsx")
## ---- funcab.data.import ---- 

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
  mutate(blockID = paste0(str_sub(siteID, 1, 3), blockID)) %>% 
  rename(Year = year) %>% 
  mutate(turfID = recode(turfID, "Alr4FGB" = "Alr5C")) %>% 
  filter(!(blockID == "Alr4" & Year == 2015 & siteID == "Alrust")) %>% 
  mutate(turfID = if_else(blockID == "Alr3" & Year == 2015 & Treatment == "C", "Alr3C", turfID))


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
  filter(Treatment %in% c("FGB", "GF"), Year > 2015) %>% 
  select(-species, -cover, -functionalGroup) %>% 
  distinct()

ttcs1516 <- composition %>% 
  filter(Treatment == "C", !Year == 2017) %>% 
  group_by(Year, turfID) %>% 
  mutate(sumcover = sum(cover)) %>% 
  filter(sumcover == 0) %>% 
  ungroup() %>% 
  select(-species, -cover, -functionalGroup, -sumcover, -pleuro, -acro) %>% 
  distinct()

ttcs17 <- composition %>% 
  filter(Treatment == "C", Year == 2017) %>% 
  group_by(Year, turfID) %>% 
  mutate(sumcover = sum(cover)) %>% 
  filter(sumcover == 0) %>% 
  ungroup() %>% 
  select(-species, -cover, -functionalGroup) %>% 
  distinct() %>% 
  full_join(scBryo, by = "turfID", suffix = c(".old", "")) %>% 
  select(-totalBryophytes.old, -mossHeight.old, -vegetationHeight.old)

####################
#### clean data ####

comp2 <- composition %>% 
  filter(cover > 0) %>% 
  full_join(my.GR.data, by = c("siteID", "blockID", "turfID", "Treatment", "Year", "functionalGroup", "species"), suffix = c("", ".new")) %>% 
  mutate(cover = if_else(cover == 0|is.na(cover), cover.new, cover),
         mossHeight = if_else(mossHeight == 0|is.na(mossHeight), mossHeight.new, mossHeight),
         vegetationHeight = if_else(vegetationHeight == 0|is.na(vegetationHeight), vegetationHeight.new, vegetationHeight),
         totalBryophytes = if_else(totalBryophytes == 0|is.na(totalBryophytes), totalBryophytes.new, totalBryophytes),
         litter = if_else(litter == 0|is.na(litter), litter.new, litter)) %>% 
  select(-totalBryophytes.new, -vegetationHeight.new, -mossHeight.new, -cover.new, -litter.new) %>% 
  bind_rows(metadata) %>% 
  left_join(ttcs1516, by = c("siteID", "blockID", "Treatment", "turfID", "Year"), suffix = c("", ".new")) %>% 
  mutate(mossHeight = if_else(mossHeight == 0|is.na(mossHeight), mossHeight.new, mossHeight),
         vegetationHeight = if_else(vegetationHeight == 0|is.na(vegetationHeight), vegetationHeight.new, vegetationHeight),
         totalBryophytes = if_else(is.na(totalBryophytes), totalBryophytes.new, totalBryophytes),
         litter = if_else(is.na(litter), litter.new, litter),
         totalGraminoids = if_else(is.na(totalGraminoids), totalGraminoids.new, totalGraminoids),
         totalForbs = if_else(is.na(totalForbs), totalForbs.new, totalForbs)) %>% 
  select(-totalBryophytes.new, -vegetationHeight.new, -mossHeight.new, -litter.new, -totalForbs.new, -totalGraminoids.new) %>%
  bind_rows(ttcs17)

comp2 <- within(comp2, mossHeight[turfID == 'Alr1F' & Year == "2017"] <- 0,
                      mossHeight[turfID == 'Alr3G' & Year == 2017] <- 8.6,
                      mossHeight[turfID == 'Alr5F' & Year == 2017] <- 1.75,
                      mossHeight[turfID == 'Alr5G' & Year == 2017] <- 1.75, 
                      mossHeight[turfID == 'Fau2F' & Year == 2017] <- 4,
                      mossHeight[turfID == 'Fau2G' & Year == 2017] <- 4,
                      mossHeight[turfID == 'Fau5F' & Year == 2017] <- 0,
                      mossHeight[turfID == 'Skj2F' & Year == 2017] <- 7,
                      mossHeight[turfID == 'Ulv3F' & Year == 2017] <- 3,
                      mossHeight[turfID == 'Ulv4C' & Year == 2017] <- 0,
                      mossHeight[turfID == 'Alr2GF' & Year == 2017] <- 3,
                      mossHeight[turfID == 'Hog4GF' & Year == 2017] <- 18,
                      mossHeight[turfID == 'Alr1C' & Year == 2017] <- 1.5,
                      vegetationHeight[turfID == 'Alr1C' & Year == 2017] <- 135,
                      vegetationHeight[turfID == 'Alr3GB' & Year == 2017] <- 65,
                      vegetationHeight[turfID == 'Fau4F' & Year == 2017] <- 70,
                      vegetationHeight[turfID == 'Ulv3B' & Year == 2017] <- 44.5,
                      vegetationHeight[turfID == 'Ulv3GB' & Year == 2017] <- 30,
                      vegetationHeight[turfID == 'Ves1FB' & Year == 2017] <- 65,
                      vegetationHeight[turfID == 'Ves2GB' & Year == 2017] <- 40,
                      vegetationHeight[turfID == 'Ves3FB' & Year == 2017] <- 50,
                      totalBryophytes[turfID == 'Alr1F' & Year == 2015] <- 0,
                      totalBryophytes[turfID == 'Alr1FGB' & Year == 2015] <- 0,
                      totalBryophytes[turfID == 'Alr1GB' & Year == 2015] <- 0,
                      totalBryophytes[turfID == 'Alr1GF' & Year == 2015] <- 0,
                      totalBryophytes[turfID == 'Alr3G' & Year == 2015] <- 0,
                      totalBryophytes[turfID == 'Fau2G' & Year == 2015] <- 0,
                      totalBryophytes[turfID == 'Ovs1C' & Year == 2015] <- 100,
                      totalGraminoids[turfID == 'Fau2C' & Year == 2015] <- 40,
                      totalForbs[turfID == 'Fau2C' & Year == 2015] <- 65,
                      totalGraminoids[turfID == 'Gud12C' & Year == 2015] <- 22,
                      totalForbs[turfID == 'Gud12C' & Year == 2015] <- 70,
                      totalGraminoids[turfID == 'Vik2C' & Year == 2015] <- 30,
                      totalForbs[turfID == 'Vik2C' & Year == 2015] <- 60)


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
         graminoidCov = if_else(grepl("G", Treatment), 0, totalGraminoids),
         vegetationHeight = if_else(Treatment == "FGB", 0, vegetationHeight),
         mossHeight = if_else(Treatment == "FGB", 0, mossHeight)) %>% 
  select(-totalGraminoids, -totalForbs, -totalBryophytes)


# .Last.value
comp2$functionalGroup <- plyr::mapvalues(comp2$functionalGroup, from = "pteridophyte", to = "forb")
comp2$functionalGroup <- plyr::mapvalues(comp2$functionalGroup, from = "woody", to = "forb")

comp2 <- comp2 %>% 
  filter(!is.na(Treatment))

save(comp2, file = "~/OneDrive - University of Bergen/Research/FunCaB/Data/funcabCompdatKonsta.RData")

# source Ragnhild's trait data
source("~/OneDrive - University of Bergen/Research/FunCaB/seedclimComm/ragnhild_trait_data/load_traits.R") # warning here is fine, it just means those spp didn't have CN data collected

traitdata <- traitdata %>% 
  select(siteID, species, Height_mean, LA_mean, SLA_mean, LDMC_mean, CN_mean, Lth_mean)


traits <- tbl(con, "taxon") %>% 
  collect() %>% 
  left_join(tbl(con, "moreTraits"), copy = TRUE, by = "species") %>% 
  select(species, seedMass)

# adding traits to my.GR.data
composition <- comp2 %>%
  left_join(traitdata, by = c("species", "siteID")) %>%
  left_join(traits, by = "species") %>% 
  mutate(functionalGroup = if_else(is.na(functionalGroup), "forb", functionalGroup))

library(vegan)

composition <- composition %>%
  group_by(turfID, Year) %>%
  mutate(richness = sum(n_distinct(species))) %>% 
  mutate(diversity = diversity(cover, index = "shannon")) %>% 
  mutate(evenness = (diversity/log(richness))) %>% 
  filter(!is.na(cover)) %>%
  group_by(turfID, siteID, Year) %>% 
  mutate(wmH = weighted.mean(Height_mean, cover, na.rm=TRUE),
         wmSM = weighted.mean(seedMass, cover, na.rm=TRUE),
         wmSLA = weighted.mean(SLA_mean, cover, na.rm=TRUE),
         wmLA = weighted.mean(LA_mean, cover, na.rm=TRUE),
         wmLDMC = weighted.mean(LDMC_mean, cover, na.rm=TRUE),
         wmLTH = weighted.mean(Lth_mean, cover, na.rm=TRUE),
         wmCN = weighted.mean(CN_mean, cover, na.rm=TRUE)) %>% #, 
  select(-Height_mean, -LA_mean, -SLA_mean, -seedMass, -Lth_mean, -LDMC_mean, -CN_mean, -TTtreat, -species, -cover) %>% 
  distinct(turfID, Year, functionalGroup, .keep_all = TRUE) %>% 
  group_by(turfID, siteID, Year) %>% 
  spread(key =functionalGroup, value = wmH) %>% 
  mutate(mossHeight = if_else(grepl("B", Treatment), 0, mossHeight),
         forb = if_else(grepl("F", Treatment), 0, forb),
         graminoid = if_else(grepl("G", Treatment), 0, graminoid)) %>% 
  ungroup()

#save(composition, file = "/Volumes/Macintosh HD/Users/fja062/Desktop/funcabComp.RData")