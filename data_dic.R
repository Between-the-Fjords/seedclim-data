# install.packages("devtools")
#devtools::install_github("Between-the-Fjords/dataDownloader")
library("dataDownloader")
library("RSQLite")
library("tidyverse")

#download database
#Download community data from OSF
# get_file(node = "npfa9",
#          file = "seedclim.sqlite",
#          path = "database",
#          remote_path = "Community_data")


con <- dbConnect(SQLite(), dbname = "database/seedclim.sqlite")

# read in attribute table and reformat
attribute_table <- tbl(con, "attributes") %>% 
  rename("Variable name" = attribute,
         "Units/treatment level" = unit,
         "Description" = description,
         "How measured" = how_measured) %>% 
  collect()

database <- dbListTables(con) %>%
  set_names() %>%
  map(tbl, src = con)


#***********************************************************************************************
### SITE
range_site <- database$sites %>% 
  as_tibble() %>% 
  summarise(
    across(where(is.character), ~ paste(min(.), max(.), sep = " - ")),
    across(where(is.numeric), ~paste(min(.), max(.), sep = " - "))
    ) %>% 
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")


site_dic <- map_df(database$sites %>% as_tibble, class) %>% 
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>% 
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>% 
  left_join(range_site, by = "Variable name") %>% 
  left_join(attribute_table, by = "Variable name")

#***********************************************************************************************
### BLOCK
range_block <- database$blocks %>%
  as_tibble() %>% 
  summarise(
    across(where(is.character), ~ paste(min(.), max(.), sep = " - ")),
    across(where(is.numeric), ~paste(min(.), max(.), sep = " - "))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")

block_dic <- map_df(database$blocks %>% as_tibble, class) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>%
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>%
  left_join(range_block, by = "Variable name") %>%
  left_join(attribute_table, by = "Variable name")


#***********************************************************************************************
### PLOTS
range_plots <- database$plots %>%
  as_tibble() %>% 
  summarise(
    across(where(is.character), ~ paste(min(.), max(.), sep = " - ")),
    across(where(is.numeric), ~paste(min(.), max(.), sep = " - "))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")

plot_dic <- map_df(database$plots %>% as_tibble, class) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>%
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>%
  left_join(range_plots, by = "Variable name") %>%
  left_join(attribute_table, by = "Variable name")


#***********************************************************************************************
### TURFS
range_turf <- database$turfs %>%
  as_tibble() %>% 
  summarise(
    across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
    across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")

turf_dic <- map_df(database$turfs %>% as_tibble, class) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>%
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>%
  left_join(range_turf, by = "Variable name") %>%
  left_join(attribute_table, by = "Variable name")


#***********************************************************************************************
### TAXON
range_taxon <- database$taxon %>%
  as_tibble() %>% 
  summarise(
    across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
    across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")

taxon_dic <- map_df(database$taxon %>% as_tibble, class) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>%
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>%
  left_join(range_taxon, by = "Variable name") %>%
  left_join(attribute_table, by = "Variable name")


#***********************************************************************************************
### TURF COMMUNITY
range_turf_community <- database$turf_community %>%
  as_tibble() %>% 
  summarise(
    across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
    across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")

turf_community_dic <- map_df(database$turf_community %>% as_tibble, class) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>%
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>%
  left_join(range_turf_community, by = "Variable name") %>%
  left_join(attribute_table, by = "Variable name")


#***********************************************************************************************
### SUBTURF COMMUNITY
range_subturf_community <- database$subturf_community %>%
  as_tibble() %>% 
  summarise(
    across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
    across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")

subturf_community_dic <- map_df(database$subturf_community %>% as_tibble, class) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>%
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>%
  left_join(range_subturf_community, by = "Variable name") %>%
  left_join(attribute_table, by = "Variable name")


#***********************************************************************************************
### TURF ENVIRONMENT
range_turf_environment <- database$turf_environment %>%
  as_tibble() %>% 
  summarise(
    across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
    across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")

turf_environment_dic <- map_df(database$turf_environment %>% as_tibble, class) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>%
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>%
  left_join(range_turf_environment, by = "Variable name") %>%
  left_join(attribute_table, by = "Variable name")


#***********************************************************************************************
### SUBTURF ENVIRONMENT
range_subturf_environment <- database$subturf_environment %>%
  as_tibble() %>% 
  summarise(
    across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
    across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")

subturf_environment_dic <- map_df(database$subturf_environment %>% as_tibble, class) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>%
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>%
  left_join(range_subturf_environment, by = "Variable name") %>%
  left_join(attribute_table, by = "Variable name")


#***********************************************************************************************
### SITE ATTRIBUTES
range_site_attributes <- database$site_attributes %>%
  as_tibble() %>% 
  summarise(
    across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
    across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")

site_attributes_dic <- map_df(database$site_attributes %>% as_tibble, class) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>%
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>%
  left_join(range_site_attributes, by = "Variable name") %>%
  left_join(attribute_table, by = "Variable name")

#***********************************************************************************************
### SPECIES ATTRIBUTES
range_species_attributes <- database$species_attributes %>%
  as_tibble() %>% 
  summarise(
    across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
    across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")

species_attributes_dic <- map_df(database$species_attributes %>% as_tibble, class) %>%
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>%
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>%
  left_join(range_species_attributes, by = "Variable name") %>%
  left_join(attribute_table, by = "Variable name")