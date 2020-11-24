library("RSQLite")
library("tidyverse")
con <- dbConnect(SQLite(), dbname = "database/seedclim.sqlite")

turfs <- tbl(con, "turfs") %>% collect()
plots <- tbl(con, "plots") %>% collect()
blocks <- tbl(con, "blocks") %>% collect()
sites <- tbl(con, "sites") %>% collect() 


subTurfCommunity <- tbl(con, "subTurfCommunity") %>% collect()
turfCommunity <- tbl(con, "turfCommunity") %>% collect()

subTurfEnvironment <- tbl(con, "subTurfEnvironment") %>% collect()
turfEnvironment <- tbl(con, "turfEnvironment") %>% collect()

taxon <- tbl(con, "taxon") %>% collect()
site_attributes <- tbl(con, "site_attributes") %>% collect()
character_traits <- tbl(con, "character_traits") %>% collect() 
numeric_traits <- tbl(con, "numeric_traits") %>% collect()



### SITE
range_site <- sites %>% 
  summarise(
    across(where(is.character), ~ paste(min(.), max(.), sep = " - ")),
    across(where(is.numeric), ~paste(min(.), max(.), sep = " - "))
    ) %>% 
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")


site_dic <- map_df(sites, class) %>% 
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>% 
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>% 
  left_join(range_site, by = "Variable name") %>% 
  mutate("How measured" = c(rep("defined", 3), rep("measured", 6), rep("defined", 1), rep("measured", 4)),
         "Units/formats/treatment level coding" = c("Alrust, Vikesland", "Ålrust, Vikesland", "alp1, sub2, bor4", rep("decimal degree", 2), rep("UTM", 2), "m a.s.l.", "mm", NA, "°C", NA, NA, NA))
 

### BLOCK
range_block <- blocks %>% 
  summarise(
    across(where(is.character), ~ paste(min(.), max(.), sep = " - ")),
    across(where(is.numeric), ~paste(min(.), max(.), sep = " - "))
  ) %>% 
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")

block_dic <- map_df(blocks, class) %>% 
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>% 
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>% 
  left_join(range_block, by = "Variable name") %>%
  mutate("How measured" = c(rep("defined", 2), rep("measured", 2), NA),
         "Units/formats/treatment level coding" = c("Alr1, Vik5", "Alrust, Vikesland", "degree", "degree", NA))
  

### PLOTS
range_plots <- plots %>% 
  summarise(
    across(where(is.character), ~ paste(min(.), max(.), sep = " - ")),
    across(where(is.numeric), ~paste(min(.), max(.), sep = " - "))
  ) %>% 
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")

plot_dic <- map_df(plots, class) %>% 
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>% 
  mutate(`Variable type` = case_when(`Variable type` == "character" ~ "categorical",
                                     `Variable type` %in% c("integer", "numeric") ~ "numeric")) %>% 
  left_join(range_plots, by = "Variable name") %>%
  mutate("How measured" = c(rep("defined", 2), rep("measured", 2)),
         "Units/formats/treatment level coding" = c("1, 2, 3", "Alr1, Vik5", "degree", "degree"))

