###################################
#### CLEAN SOIL STRUCTURE DATA ####
###################################

# load libraries
library(dataDownloader)
library(tidyverse)
library(readxl)
library("lubridate")


## DOWNLOAD DATA
# Use this code to download the data directly from OSF and unzip the file.
# Alternatively you can find the raw data here: https://osf.io/npfa9/

# get_file(node = "npfa9",
#          file = "SeedPredation_2018_SG_18.csv",
#          path = "seed_predation/data",
#          remote_path = "Seed_predation_data/Raw_data")


## CLEAN DATA

BD_raw <- read_excel("soil_structure/data/BulkDensity_FunCaB.xlsx", col_types = c("text", "text", "numeric", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", rep("skip", 7)))

# Assumption stone denistiy is 2.65 g cm–³
stone_density = 2.65
# Volum: h = 4cm, d = 5.6 (r^2 * h * pi)
soil_core_volume = (5.6 / 2)^2 * 4 * 3.14159
# 98.52026

# stone density = stone mass / stone volume


BD <- BD_raw %>% 
  # remove na in data
  filter(!is.na(tray_DWsoil)) %>% 
  select(siteID:Roots, rocks = "Other (rock etc)") %>% 
  mutate(wet_mass_g = tray_WWsoil - Tray,
         dry_mass_g = tray_DWsoil - Tray - Roots - rocks) %>% 
  select(siteID, sampleID = SampleID, depth = Depth, wet_mass_g, dry_mass_g, roots_g = Roots, rocks_g = rocks) %>% 
  mutate(stone_volume = rocks_g / 2.65,
         bulk_density = (dry_mass_g - rocks_g) / (98.52026 - stone_volume)) %>% 
  # remove negative bulk density, if stones are > 100% of soil mass then chuck!
  filter(bulk_density > 0) %>% 
  mutate(siteID = recode(siteID, 
                         "ULV" = "Ulvehaugen",
                         "LAV" = "Lavisdalen",
                         "GUD" = "Gudmedalen",
                         "SKJ" = "Skjelingahaugen",
                         "ALR" = "Alrust",
                         "HOG" = "Hogsete",
                         "RAM" = "Rambera",
                         "VES" = "Veskre",
                         "FAU" = "Fauske",
                         "VIK" = "Vikesland",
                         "ARN" = "Arhelleren",
                         "OVS" = "Ovstedalen"),
         variable = "bulk_density") %>%
  select(siteID, sampleID, depth, variable, value = bulk_density)

# check
# ggplot(BD, aes(x = siteID, y = bulk_density)) +
#   geom_boxplot()


soil_texture_raw <- read_excel("soil_structure/data/soil_characteristics2012_2014 John+Serge.xlsx", sheet = "microsom sites")

soil_texture <- soil_texture_raw %>% 
  rename(variable = "...1") %>% 
  filter(variable %in% c("% Silt", "% Clay", "% Sand")) %>% 
  pivot_longer(cols = ULV:SKJ, names_to = "siteID", values_to = "value") %>% 
  select(siteID, variable, value) %>% 
  mutate(siteID = recode(siteID, 
                         "ULV" = "Ulvehaugen",
                         "LAV" = "Lavisdalen",
                         "GUD" = "Gudmedalen",
                         "SKJ" = "Skjelingahaugen"),
         variable = recode(variable, 
                         "% Clay" = "clay_percent",
                         "% Silt" = "silt_percent",
                         "% Sand" = "sand_percent"),
         year = 2013)

# 2014 data
SD1_raw <- read_excel("soil_structure/data/soil-grass-heath-2014.xlsx", skip = 1, sheet = "grassland")

# 2013 data only subalpine and alpine sites
SD2_raw <- read_excel("soil_structure/data/SFarinas-Compiled data for SEEDCLIM-2010 and 2012.xlsx", sheet = "Compiled data")

SD1 <- SD1_raw %>% 
  select(Site, Block, depth1:depth3) %>% 
  rename(siteID = Site, blockID = Block) %>% 
  mutate(year = 2014,
         siteID = recode(siteID, 
                         "Ulv" = "Ulvehaugen",
                         "Lav" = "Lavisdalen",
                         "Gud" = "Gudmedalen",
                         "Skj" = "Skjelingahaugen",
                         "Alr" = "Alrust",
                         "Hog" = "Hogsete",
                         "Ram" = "Rambera",
                         "Ves" = "Veskre",
                         "Fau" = "Fauske",
                         "Vik" = "Vikesland",
                         "Ahr" = "Arhelleren",
                         "Ovs" = "Ovstedalen"),
         blockID = paste0(substr(siteID, 1, 3), blockID)) %>% 
  pivot_longer(cols = depth1:depth3, names_to = "variable", values_to = "value") %>% 
  mutate(sampleID = str_extract(variable, "\\d"),
         variable = paste0("soil_", str_remove(variable, "\\d"))) %>% 
  select(year, siteID, blockID, variable, sampleID, value)


SD2 <- SD2_raw %>% 
  slice(-1) %>% 
  select(siteID, blockID, soil_depth1 = "Soil depth 1", soil_depth2 = "Soil depth 2") %>% 
  filter(!is.na(soil_depth1)) %>% 
  mutate(year = 2013,
         siteID = recode(siteID, 
                         "Ulvhaugen" = "Ulvehaugen",
                         "Skjellingahaugen" = "Skjelingahaugen")) %>% 
  pivot_longer(cols = c(soil_depth1, soil_depth2), names_to = "variable", values_to = "value") %>% 
  mutate(value = as.numeric(value),
         sampleID = str_extract(variable, "\\d"),
         variable = str_remove(variable, "\\d")) %>% 
  select(year, siteID, blockID, variable, sampleID, value)
  
soil_depth <- bind_rows(SD1, SD2)
  

soil_strucutre <- bind_rows(soil_depth, soil_texture, BD)

write_csv(soil_strucutre, "soil_structure/Soil_structure_2013-2014_clean.csv")
