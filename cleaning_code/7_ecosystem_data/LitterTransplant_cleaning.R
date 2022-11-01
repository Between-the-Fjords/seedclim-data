# Litter Transplant data cleaning

library(lubridate)
library(tidyverse)

#load litter transplant data
Litter_raw <- read.table("cleaning_code/7_ecosystem_data/data/LitterTransplant_14082017.txt", header=TRUE, dec= ",") %>% 
  as_tibble()

# litter collection dates
litter_collection_date <- tribble(
  ~siteID_dest, ~litter_collection_date,
  "Arhelleren", "20/05/2014",
  "Alrust", "21/05/2014",
  "Fauske", "21/05/2014",
  "Gudmedalen", "08/08/2013",
  "Hogsete", "22/05/2014",
  "Lavisdalen", "09/08/2013",
  "Ovstedal", "19/05/2014",
  "Rambera", "09/08/2013",
  "Skjelingahaugen", "18/07/2013",
  "Ulvehaugen", "06/08/2013",
  "Veskre", "26/06/2014",
  "Vikesland", "22/05/2014") %>% 
  mutate(litter_collection_date = dmy(litter_collection_date))


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
                         "OVS"= "Ovstedalen", 
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
                             "OVS"= "Ovstedalen", 
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
  select(year, siteID_origin = OriginSite, siteID_dest = DestinationSite, 
         temperature_level = T_level, precipitation_level = P_level, 
         treatment = Treatment, bagID = Bag, weight_loss = W_loss, 
         start_weight = L_startweight, end_weight = L_endweight, 
         time = Time, burial_date = BurialDate, recovery_date = RecoveryDate, 
         timestep = Timestep, TTtreat, data_check = DataCheck) %>% 
  left_join(litter_collection_date, by = "siteID_dest")
  # # new column with 1/0 for warmer or wetter treatment
  # mutate(warmer = ifelse(grepl("WA", Treatment, fixed = TRUE), "1", "0"),
  #        wetter = ifelse(grepl("WE", Treatment, fixed = TRUE), "1", "0"))

write_csv(Litter_clean, "cleaning_code/7_ecosystem_data/data/VCG_clean_decomposition_litter_2016.csv")
