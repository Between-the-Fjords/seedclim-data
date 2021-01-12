###################################
#### CLEAN SEED PREDATION DATA ####
###################################

# load libraries
library("tidyverse")
library("lubridate")
library("dataDownloader")

## DOWNLOAD DATA
# Use this code to download the data directly from OSF and unzip the file.
# Alternatively you can find the raw data here: https://osf.io/npfa9/

# get_file(node = "npfa9",
#          file = "SeedPredation_2018_SG_18.csv",
#          path = "seed_predation/data",
#          remote_path = "Seed_predation_data/Raw_data")


## CLEAN DATA

predation_raw <- read_delim(file = "seed_predation/data/SeedPredation_2018_SG_18.csv", delim = ",")

predation <- predation_raw %>% 
  rename(siteID = site, temperature_level = temp, precipitation_level = precipitation, treatment = treat) %>% 
  mutate(siteID = recode(siteID, "ulv" = "Ulvehaugen",
                         "ulv" = "Ulvehaugen",
                         "lav" = "Lavisdalen",
                         "gud" = "Gudmedalen",
                         "skj" = "Skjelingahaugen",
                         "alr" = "Alrust",
                         "hog" = "Hogsete",
                         "ram" = "Rambera",
                         "ves" = "Veskre",
                         "fau" = "Fauske",
                         "vik" = "Vikesland",
                         "arn" = "Arhelleren",
                         "ovs" = "Ovstedalen"))

write_csv(predation, path = "seed_predation/data/Seed_predation_2018.csv")
