# Litter Transplant data cleaning

library(lubridate)
library(tidyverse)

#load litter transplant data
Litter_raw <- read.table("decomposition/data/LitterTransplant_14082017.txt", header=TRUE, dec= ",") %>% 
  as_tibble()

#Remove faulty data, but keeping possible outliers
Litter_clean <- Litter_raw %>% 
  filter(!DataCheck %in% c("bad")) %>% 
  mutate(BurialDate = dmy(BurialDate),
         RecoveryDate = dmy(RecoveryDate)) %>% 
  mutate(OriginSite = recode(OriginSite, 
                         "ARH"="Arhelleren", 
                         "ALR"="Alrust", 
                         "FAU"="Fauske", 
                         "GUD"="Gudmedalen", 
                         "HOG"= "Hogsete", 
                         "LAV"="Lavisdalen", 
                         "OVS"= "Ovstedal", 
                         "RAM"="Rambera", 
                         "SKJ"= "Skjelingahaugen", 
                         "ULV"="Ulvehaugen", 
                         "VES"="Veskre", 
                         "VIK"="Vikesland"),
         DestinationSite = recode(DestinationSite, 
                             "ARH"="Arhelleren", 
                             "ALR"="Alrust", 
                             "FAU"="Fauske", 
                             "GUD"="Gudmedalen", 
                             "HOG"= "Hogsete", 
                             "LAV"="Lavisdalen", 
                             "OVS"= "Ovstedal", 
                             "RAM"="Rambera", 
                             "SKJ"= "Skjelingahaugen", 
                             "ULV"="Ulvehaugen", 
                             "VES"="Veskre", 
                             "VIK"="Vikesland"),
         # make treatment comparable
         TTtreat = recode(Treatment, 
                          "C"="TTC", 
                          "WA"="TT2", 
                          "WE"="TT3", 
                          "WA+WE"="TT4"), 
         Treatment = recode(Treatment, 
                            "C"="control", 
                            "WA"="warm", 
                            "WE"="wet", 
                            "WA+WE"="warm_wet"), 
         year = 2016) %>% 
  select(year, siteID_origin = OriginSite, siteID_dest = DestinationSite, temperature_level = T_level, precipitation_level = P_level, treatment = Treatment, BagID = Bag, W_loss, L_startweight, L_endweight, Time, BurialDate, RecoveryDate, Timestep, TTtreat, DataCheck)
  # # new column with 1/0 for warmer or wetter treatment
  # mutate(warmer = ifelse(grepl("WA", Treatment, fixed = TRUE), "1", "0"),
  #        wetter = ifelse(grepl("WE", Treatment, fixed = TRUE), "1", "0"))

write_csv(Litter_clean, "decomposition/Decomposition_litter_2016_clean.csv")
