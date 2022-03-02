# SeedClim Soil Chemistry Data Documentation (SG.106)
# Author: Sonya Geange


#packages
library(tidyverse)
library(readxl)
library(RSQLite)


con <- dbConnect(SQLite(), dbname = "database/seedclim.sqlite")

# Load taxon and trait data
turf_table <- tbl(con, "turfs") %>% 
  left_join(tbl(con, "plots"), by = c("originPlotID" = "plotID")) %>%
  select(-RTtreat, -GRtreat, -aspect, -slope) %>% 
  left_join(tbl(con, "blocks") %>% 
              select(blockID, siteID), by = "blockID") %>% 
  collect() %>% 
  mutate(originPlotID = as.numeric(originPlotID),
         originPlotID = if_else(originPlotID < 0, originPlotID * -1, originPlotID),
         destinationPlotID = as.numeric(destinationPlotID),
         destinationPlotID = if_else(destinationPlotID < 0, destinationPlotID * -1, destinationPlotID))

#############################################################
# Read in Kari's SeedClim data from 2009 (pre transplants)
#############################################################
soil_chem_2009_raw <- read_excel("soil_chemistry/data/Species and env pre-experiment.xls", sheet = "Env var part filled")

# Relabel variables
soil_chem_2009 <- soil_chem_2009_raw %>% 
  rename(LOI = `LOI_(%)`, water_content_percent = `water_cont._(%)`) %>% 
  # Add site column
  mutate(siteID = case_when(Low1 == 1 ~ "Fauske",
                            Low2 == 1 ~ "Vikesland",
                            Low3 == 1 ~ "Arhelleren",
                            Low_4 == 1 ~ "Ovstedalen",
                            Int1 == 1 ~ "Alrust",
                            Int2 == 1 ~ "Hogsete",
                            Int3 == 1 ~ "Rambera",
                            Int4 == 1 ~ "Veskre",
                            Alp1 == 1 ~ "Ulvehaugen",
                            Alp2 == 1 ~ "Lavisdalen",
                            Alp3 == 1 ~ "Gudmedalen",
                            Alp4 == 1 ~ "Skjelingahaugen",
                            TRUE ~ NA_character_)) %>% 
  # Add year column, 2009
  mutate(year = 2009) %>% 
  # select important columns
  select(year, siteID, plot, Plot_id,  pH, LOI) %>% 
  # Drop extra rows
  filter(!is.na(siteID)) %>% 
  # split Plot_id
  mutate(Plot_id = str_remove(Plot_id, "\\?"),
         tmp_chunks = str_split(Plot_id, "/", 2),
         originPlotID = map_chr(tmp_chunks, 1),
         destinationPlotID = map_chr(tmp_chunks, 2),
         originPlotID = as.numeric(originPlotID),
         destinationPlotID = as.numeric(destinationPlotID),
         destinationPlotID = if_else(is.na(destinationPlotID), originPlotID, destinationPlotID)) %>% 
  select(-tmp_chunks) %>% 
  # fix wrong plotID
  mutate(originPlotID = if_else(originPlotID == 207, 209, originPlotID),
         originPlotID = if_else(is.na(originPlotID) & destinationPlotID == 215, 179, originPlotID),
         originPlotID = if_else(is.na(originPlotID) & destinationPlotID == 28, 1, originPlotID),
         originPlotID = if_else(is.na(originPlotID) & destinationPlotID == 7, 10, originPlotID),
         destinationPlotID = if_else(destinationPlotID == 3, 1, destinationPlotID),
         originPlotID = if_else(originPlotID == 256, 260, originPlotID),
         destinationPlotID = if_else(destinationPlotID == 256, 260, destinationPlotID),
         originPlotID = if_else(originPlotID == 174, 169, originPlotID),
         destinationPlotID = if_else(destinationPlotID == 207, 170, destinationPlotID)) %>%  
  left_join(turf_table, by = c("siteID", "originPlotID", "destinationPlotID")) %>% 
  mutate(TTtreat = if_else(is.na(TTtreat) & turfID == "507 TTC", "TTC", TTtreat)) %>% 
  select(year, destinationSiteID = siteID, destinationBlockID = blockID, originPlotID, destinationPlotID, turfID, TTtreat, pH, LOI)



#############################################################
# Read in 2010 and 2012 soil chemistry datasets 2010 is from Farinas Msc Thesis,
# and 2012 is from Farinas PhD Thesis Proposal
#############################################################

soil_chem_1012_raw <- read_excel("soil_chemistry/data/SFarinas_Compiled_data_for_SEEDCLIM_2010_and_2012.xlsx", 
                                 sheet = "Compiled data") 

soil_chem_2010 <- soil_chem_1012_raw %>% 
  slice(-1) %>% 
  select(siteID, blockID, plotID, turfID, year, NO3N, NH4N, AvailN) %>% 
  mutate(siteID = recode(siteID,
      "Ulvhaugen" = "Ulvehaugen",
      "Skjellingahaugen" = "Skjelingahaugen",
      "Ovstedal" = "Ovstedalen"),
      NO3N = as.numeric(NO3N),
      NH4N = as.numeric(NH4N),
      AvailN = as.numeric(AvailN)) %>% 
  select(year, destinationSiteID = siteID, destinationPlotID = plotID, plotID, turfID, NO3N, NH4N, available_N = AvailN) %>% 
  # remove 2012, seems to be strange summary of 2010
  filter(year == 2010)



############################################################
## Based off Farinas PhD Thesis Proposal, dated 2013

soil_chem_2013_raw <- read_excel("soil_chemistry/data/Seedclim-rootgrowth-tested-variables.xlsx",
    sheet = 1,
    col_names = TRUE)

soil_chem_2013 <- soil_chem_2013_raw %>% 
  select(destinationSiteID = Site, soil_depth = depth2013, NO3N = NO3, NH4N = NH4, available_N = N, pH, LOI) %>% 
  mutate(destinationSiteID = recode(destinationSiteID,
      "Ulvhaugen" = "Ulvehaugen",
      "Skjellingahaugen" = "Skjelingahaugen"),
      year = 2013) %>% 
  select(year, everything())


############################################################

### 2015 soil C and N content

soil_chem_2015_raw <- read_delim("soil_chemistry/data/soilCN_2015.txt", locale = locale(decimal_mark = ","))

soil_chem_2015 <- soil_chem_2015_raw %>% 
  mutate(year = 2015,
         siteID = recode(siteID, 
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
         blockID = paste0(substr(siteID, 1, 3), blockID)) %>% 
  select(year, destinationSiteID = siteID, destinationBlockID = blockID, soil_depth = depth, N_content = N, C_content = C, CN_ratio = CN) %>% 
  filter(!is.na(N_content))


# Merge all dataset
soil_chem <- bind_rows(soil_chem_2009, soil_chem_2010, soil_chem_2013, soil_chem_2015) %>% 
  select(year:TTtreat, soil_depth, everything()) %>% 
  pivot_longer(cols = c(soil_depth:CN_ratio), names_to = "variable", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  mutate(unit = case_when(variable %in% c("LOI", "N_content", "C_content") ~ "percent",
                          variable %in% c("NO3N", "NH4N", "available_N") ~ "μg per g resin/bag/day",
                          variable == "soil_depth" ~ "cm",
                          TRUE ~ NA_character_))

# save file
write_csv(soil_chem, "soil_chemistry/VCG_clean_soil_chemistry_2009_2010_2013_2015.csv")


# Remarks
# LOI: Values seem to differ between 2009 and 2013, so keep both
# pH: Values seem to differ between 2009 and 2013, so keep both
# For both NO3N and NH4N, it seems odd that they appear identical here, given the difference in sample sizes, and raw data points as well?? 
# NO3N: 2010/2012 appear identical (bar sample size), but 2013 is different
# NH4N: 2012/2012 appear identical (bar sample size), but 2013 is different
# How convinced are we about the 2012 dataset? Do we not include 2012 therefore in the final output file?
# Aud: remove 2012, seems to be a strange summary of 2010

