# Make data dictionaries

# load libraries
library("RSQLite")
library("tidyverse")
library("readxl")
library(dataDocumentation)

# load clean data
#source("data_dictionary/download_clean_data.R")

# read in data description table
description_table <- read_excel("data_dictionary/data_description.xlsx")

#************************************************************************
#************************************************************************
### 1 SEED DATA
# Seedbank
seedbank1 <- read_excel("cleaning_code/1_seed_data/data/VCG_clean_seedbank.xlsx", sheet = "seedbank")

seedbank_s_dic <- make_data_dictionary(data = seedbank1,
                                      description_table = description_table,
                                      table_ID = "seedbank")

seedbank2 <- read_excel("cleaning_code/1_seed_data/data/VCG_clean_seedbank.xlsx", sheet = "vegetation")

seedbank_v_dic <- make_data_dictionary(data = seedbank2,
                                     description_table = description_table,
                                     table_ID = "seedbank")

# Seedrain
seedrain <- read_csv("cleaning_code/1_seed_data/data/VCG_clean_seedrain.csv")


seedseedrain_dic <- make_data_dictionary(data = seedrain,
                                      description_table = description_table,
                                      table_ID = "seedrain")

# Seed predation
seed_pred <- read_csv("cleaning_code/1_seed_data/data/VCG_clean_seed_predation_2018.csv")


seed_pred_dic <- make_data_dictionary(data = seed_pred,
                                      description_table = description_table,
                                      table_ID = "seed_predation")


#************************************************************************
#************************************************************************
### 2 POPULATION DATA

# Demo transplant
demo_transplant <- read_delim("cleaning_code/2_population/data/VCG_clean_transplant_demography_2009-2012.txt")

demo_transplant_dic <- make_data_dictionary(data = demo_transplant,
                                       description_table = description_table,
                                       table_ID = "transplant_demography")


# Demo graminoid removal
demo_graminoid_removal <- read_delim("cleaning_code/2_population/data/VCG_clean_graminoidRemoval_demography_2011-2013.txt")

demo_graminoid_removal_dic <- make_data_dictionary(data = demo_graminoid_removal,
                                            description_table = description_table,
                                            table_ID = "graminoidRemoval_demography")


# Demo seed production
demo_seed_production <- read_delim("cleaning_code/2_population/data/VCG_clean_seed_production_2011.txt")

demo_seed_production_dic <- make_data_dictionary(data = demo_seed_production,
                                                   description_table = description_table,
                                                   table_ID = "seed_production")


# Demo seed burial
demo_seed_burial <- read_delim("cleaning_code/2_population/data/VCG_clean_seedBurial_2011.txt")

demo_seed_burial_dic <- make_data_dictionary(data = demo_seed_burial,
                                                 description_table = description_table,
                                                 table_ID = "seed_burial")


# Demo seed sowing field
demo_seed_sowing_field <- read_delim("cleaning_code/2_population/data/VCG_clean_seed_sowing_field_2010-2011.txt")

demo_seed_sowing_field_dic <- make_data_dictionary(data = demo_seed_sowing_field,
                                             description_table = description_table,
                                             table_ID = "seed_germination_field")

# Demo seed sowing lab
demo_seed_sowing_lab <- read_delim("cleaning_code/2_population/data/VCG_clean_seed_sowing_lab_2010.txt")

demo_seed_sowing_lab_dic <- make_data_dictionary(data = demo_seed_sowing_lab,
                                                   description_table = description_table,
                                                   table_ID = "seed_germination_laboratory")


#************************************************************************
#************************************************************************
### 3 COMMUNITY DATA

con <- dbConnect(SQLite(), dbname = "database/seedclim.sqlite")

# read in attribute table and reformat
attribute_table <- tbl(con, "attributes") %>% 
  rename("Variable name" = attribute,
         "Units/treatment level" = unit,
         "Description" = description,
         "How measured" = how_measured) %>% 
  collect() %>% 
  mutate(TableID = NA_character_)


database <- dbListTables(con) %>%
  set_names() %>%
  map(tbl, src = con)


#************************************************************************
### SITE

site_dic <- make_data_dictionary(data = database$sites,
                                 description_table = attribute_table,
                                 table_ID = "site",
                                 keep_table_ID = TRUE)

#************************************************************************
### BLOCK

block_dic <- make_data_dictionary(data = database$blocks,
                                  description_table = attribute_table,
                                  table_ID = "blocks",
                                  keep_table_ID = TRUE)

#*************************************************************************
### PLOTS

plot_dic <- make_data_dictionary(data = database$plots,
                                  description_table = attribute_table,
                                  table_ID = "plots",
                                 keep_table_ID = TRUE)

#*************************************************************************
### TURFS

turf_dic <- make_data_dictionary(data = database$turfs,
                                 description_table = attribute_table,
                                 table_ID = "turfs",
                                 keep_table_ID = TRUE)

#*************************************************************************
### TAXON

taxon_dic <- make_data_dictionary(data = database$taxon %>% as_tibble() %>% 
                                    filter(species_name != "???",
                                           !family %in% c("??", "???")),
                                  description_table = attribute_table,
                                  table_ID = "taxon",
                                  keep_table_ID = TRUE)

#*************************************************************************
### TURF COMMUNITY

turf_community_dic <- make_data_dictionary(data = database$turf_community %>%
                                             as_tibble() %>% 
                                             filter(!species %in% c("X....1", "X...")),
                                           description_table = attribute_table,
                                           table_ID = "turf_community",
                                           keep_table_ID = TRUE)

#*************************************************************************
### SUBTURF COMMUNITY

subturf_community_dic <- make_data_dictionary(data = database$subturf_community %>%
                                             as_tibble() %>% 
                                             filter(!species %in% c("X....1", "X...")),
                                           description_table = attribute_table,
                                           table_ID = "subturf_community",
                                           keep_table_ID = TRUE)

#*************************************************************************
### TURF ENVIRONMENT

turf_environment_dic <- make_data_dictionary(data = database$turf_environment,
                                 description_table = attribute_table,
                                 table_ID = "turf_environment",
                                 keep_table_ID = TRUE)

#*************************************************************************
### SUBTURF ENVIRONMENT

subturf_environment_dic <- make_data_dictionary(data = database$subturf_environment,
                                             description_table = attribute_table,
                                             table_ID = "subturf_environment",
                                             keep_table_ID = TRUE)


#*************************************************************************
### SITE ATTRIBUTES

a_range <- database$site_attributes %>% 
  as_tibble() %>% 
  select(-siteID) %>% 
  group_by(attribute) %>% 
  summarise(
    across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
    across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
  ) %>% 
  mutate(value_character = if_else(value_character == "NA - NA", NA_character_, value_character),
         range = coalesce(value_character, value_numeric)) %>% 
  select(attribute, range) %>% 
  rename("Variable name" = "attribute", "Variable range or levels" = "range")
  
range_site_attributes <- database$site_attributes %>%
  as_tibble() %>% 
  select(siteID) %>% 
  summarise(
    across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
    across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels") %>% 
  bind_rows(a_range)

site_attributes_dic <- map_df(database$site_attributes %>% as_tibble, ~class(.x)) %>%
  select(siteID) %>% 
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>% 
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>%
  full_join(range_site_attributes, by = "Variable name") %>%
  left_join(attribute_table, by = "Variable name") %>% 
  mutate(`Variable type` = if_else(`Variable name` %in% c("aspect", "slope", "solar_radiation", "total_N_red_oxi"), "numeric", "categorical")) %>% 
  mutate(TableID = "site_attributes")

#****************************************************************************
### SPECIES ATTRIBUTES
sp_range <- database$species_attributes %>% 
  as_tibble() %>% 
  select(-species) %>% 
  group_by(attribute) %>% 
  summarise(
    across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
    across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
  ) %>% 
  mutate(value_character = if_else(value_character == "NA - NA", NA_character_, value_character),
         range = coalesce(value_character, value_numeric)) %>% 
  select(attribute, range) %>% 
  rename("Variable name" = "attribute", "Variable range or levels" = "range")

range_species_attributes <- database$species_attributes %>%
  as_tibble() %>% 
  select(species) %>% 
  summarise(
    across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
    across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels") %>% 
  bind_rows(sp_range)

species_attributes_dic <- map_df(database$species_attributes %>% as_tibble, ~class(.x)) %>%
  select(species) %>% 
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>%
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>%
  full_join(range_species_attributes, by = "Variable name") %>%
  left_join(attribute_table, by = "Variable name") %>% 
  mutate(`Variable type` = case_when(`Variable name` == "comment" ~ NA_character_,
                                     `Variable name` %in% c("flowering_finish", "flowering_start", "functional_group", "habitat", "lifespan", "lowervegetation_zone", "norwegian_name", "rarity", "soil_type", "uppervegetation_zone") ~ "categorical",
                                     TRUE ~ "numeric")) %>% 
  mutate(TableID = "species_attributes")

#***********************************************************************
# Merge all community data dics
community_dic <- bind_rows(site_dic,
                           block_dic,
                           plot_dic,
                           turf_dic,
                           taxon_dic,
                           turf_community_dic,
                           subturf_community_dic,
                           turf_environment_dic,
                           subturf_environment_dic,
                           site_attributes_dic,
                           species_attributes_dic)


#************************************************************************
#************************************************************************

#### 4 PHENOLOGY DATA #### 
# Community phenology
phenology <- read_csv("cleaning_code/4_phenology_data/data/VCG_clean_community_phenology_2014-2015.csv")

phenology_dic <- make_data_dictionary(data = phenology,
                                      description_table = description_table,
                                      table_ID = "phenology")


#************************************************************************
#************************************************************************
#### 5 TRAIT DATA #### 
# Leaf traits
leaf_trait <- read_csv("cleaning_code/5_trait_data/data/VCG_clean_trait_data_2012-2016.csv")

leaf_trait_dic <- make_data_dictionary(data = leaf_trait,
                                       description_table = description_table,
                                       table_ID = "leaf_traits")



#************************************************************************
#************************************************************************
#### 6 BIOMASS #### 
# SG8 Biomass
biomass_allocation <- read_csv("cleaning_code/6_biomass_data/data/VCG_clean_biomass_allocation_2009.csv")

biomass_allocation_dic <- make_data_dictionary(data = biomass_allocation,
                                       description_table = description_table,
                                       table_ID = "SG8_biomass")

# SG9 Biomass FG
biomass_fg <- read_csv("cleaning_code/6_biomass_data/data/VCG_clean_functional_group_biomass_2010_2013-2015.csv")

biomass_fg_dic <- make_data_dictionary(data = biomass_fg,
                                       description_table = description_table,
                                       table_ID = "SG9_biomass_fg")


# SG9 Biomass SPECIES
biomass_sp <- read_csv("cleaning_code/6_biomass_data/data/VCG_clean_species_level_biomass_2013.csv")

biomass_sp_dic <- make_data_dictionary(data = biomass_sp,
                                       description_table = description_table,
                                       table_ID = "SG9_biomass_sp")

# Belowground biomass and productivitiy
biomass_below <- read_csv("cleaning_code/6_biomass_data/data/VCG_clean_belowground_biomass_2013-2014.csv")

biomass_below_dic <- make_data_dictionary(data = biomass_below,
                                       description_table = description_table,
                                       table_ID = "biomass_below")


# GR BIOMASS
biomass_gr <- read_csv("cleaning_code/6_biomass_data/data/VCG_clean_graminoid_removal_biomass_2011-2018.csv")

biomass_gr_dic <- make_data_dictionary(data = biomass_gr,
                                       description_table = description_table,
                                       table_ID = "biomass_gr")

#************************************************************************
#************************************************************************
#### 6 ECOSYSTEM #### 
# Decomposition
litter <- read_csv("cleaning_code/7_ecosystem_data/data/VCG_clean_decomposition_litter_2016.csv")

litter_dic <- make_data_dictionary(data = litter,
                                   description_table = description_table,
                                   table_ID = "litterbags")

teabag <- read_csv("cleaning_code/7_ecosystem_data/data/VCG_clean_decomposition_teabag_2014.csv")

teabag_dic <- make_data_dictionary(data = teabag,
                                   description_table = description_table,
                                   table_ID = "teabag")


litter_cn <- read_csv("cleaning_code/7_ecosystem_data/data/VCG_clean_litter_cn_2016.csv")

litter_cn_dic <- make_data_dictionary(data = litter_cn,
                                   description_table = description_table,
                                   table_ID = "litter_cn")


#************************************************************************
#************************************************************************
#### 8 ENVIRONMENTAL DATA #### 
# Temperature
temperature <- read_csv("cleaning_code/8_environment_data/climate/data/VCG_clean_temperature.csv")

temperature_dic <- make_data_dictionary(data = temperature,
                                       description_table = description_table,
                                       table_ID = "temperature")

# Soilmoisture
soilmoisture <- read_csv("cleaning_code/8_environment_data/climate/data/VCG_clean_soil_moisture.csv")

soilmoisture_dic <- make_data_dictionary(data = soilmoisture,
                                       description_table = description_table,
                                       table_ID = "soilmoisture")

# Soilmoisture
soilmoisture_plot <- read_csv("cleaning_code/8_environment_data/climate/data/VCG_clean_soilmoisture_plotlevel_2015-2018.csv")

soilmoisture_plot_dic <- make_data_dictionary(data = soilmoisture_plot,
                                       description_table = description_table,
                                       table_ID = "soilmoisture_plot")


# Gridded climate data
climate <- read_csv("cleaning_code/8_environment_data/climate/data/VCG_clean_gridded_daily_climate_2008-2022.csv")

climate_plot_dic <- make_data_dictionary(data = climate,
                                              description_table = description_table,
                                              table_ID = "climate")


# Soil structure
soil_structure <- read_csv("cleaning_code/8_environment_data/soil_structure/data/VCG_clean_soil_structure_2013_2014_2018.csv")

soil_structure_dic <- make_data_dictionary(data = soil_structure,
                                              description_table = description_table,
                                              table_ID = "soil_structure")

# Soil chemistry
soil_chemistry <- read_csv("cleaning_code/8_environment_data/soil_chemistry/data/VCG_clean_soil_chemistry_2009_2010_2013_2015.csv")

soil_chemistry_dic <- make_data_dictionary(data = soil_chemistry,
                                              description_table = description_table,
                                              table_ID = "soil_chemistry")

