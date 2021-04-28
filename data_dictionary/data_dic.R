# Make data dictionaries

# load libraries
library("RSQLite")
library("tidyverse")
library("readxl")

# load clean data
#source("data_dictionary/download_clean_data.R")

# data dictionary function
source("data_dictionary/make_data_dictionary.R")

# read in data description table
description_table <- read_excel("data_dictionary/data_description.xlsx")

#************************************************************************
#************************************************************************
### 1 SEED DATA
# Seed predation
seed_pred <- read_csv("seed_predation/data/Seed_predation_2018.csv")


seed_pred_dic <- make_data_dictionary(data = seed_pred,
                                      description_table = description_table,
                                      table_ID = "seed_predation")


#************************************************************************
#************************************************************************
### 2 POPULATION DATA





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
phenology <- read_csv("phenology/clean_data/Community_phenology_2014-2015.csv")

phenology_dic <- make_data_dictionary(data = phenology,
                                      description_table = description_table,
                                      table_ID = "phenology")


#************************************************************************
#************************************************************************
#### 5 TRAIT DATA #### 
# Leaf traits
leaf_trait <- read_csv("plant_traits/data/SeedClim_Trait_data_2012_2016.csv")

leaf_trait_dic <- make_data_dictionary(data = leaf_trait,
                                       description_table = description_table,
                                       table_ID = "leaf_traits")
