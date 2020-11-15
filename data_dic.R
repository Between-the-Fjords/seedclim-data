library("RSQLite")

con <- dbConnect(SQLite(), dbname = "~/Dropbox/Bergen/seedclimComm/database/seedclim.sqlite")

turfs <- tbl(con, "turfs") %>% collect()
plots <- tbl(con, "plots") %>% collect()
blocks <- tbl(con, "blocks") %>% collect() %>% 
  select(blockID, siteID)
sites <- tbl(con, "sites") %>% select(siteID, siteCode, latitude, longitude, elevation = `altitude(DEM)`, annualPrecipitation_gridded, temperature_level, summerTemperature_gridded, precipitation_level, geology) %>% collect() 


subTurfCommunity <- tbl(con, "subTurfCommunity") %>% collect()
turfCommunity <- tbl(con, "turfCommunity") %>% collect()

subTurfEnvironment <- tbl(con, "subTurfEnvironment") %>% collect()
turfEnvironment <- tbl(con, "turfEnvironment") %>% collect()

taxon <- tbl(con, "taxon") %>% collect()
site_attributes <- tbl(con, "site_attributes") %>% collect()
character_traits <- tbl(con, "character_traits") %>% collect()
numeric_traits <- tbl(con, "numeric_traits") %>% collect()



### SITE
min <- map_df(sites, min) %>% 
  mutate(latitude = as.character(latitude),
         longitude = as.character(longitude),
         elevation = as.character(elevation),
         annualPrecipitation_gridded = as.character(annualPrecipitation_gridded),
         temperature_level = as.character(temperature_level),
         summerTemperature_gridded = as.character(summerTemperature_gridded),
         precipitation_level = as.character(precipitation_level),
         geology = as.character(geology)) %>% 
  pivot_longer(cols = everything(), names_to = "Variable_name", values_to = "min")

max <- map_df(sites, max) %>% 
  mutate(latitude = as.character(latitude),
         longitude = as.character(longitude),
         elevation = as.character(elevation),
         annualPrecipitation_gridded = as.character(annualPrecipitation_gridded),
         temperature_level = as.character(temperature_level),
         summerTemperature_gridded = as.character(summerTemperature_gridded),
         precipitation_level = as.character(precipitation_level),
         geology = as.character(geology)) %>% 
  pivot_longer(cols = everything(), names_to = "Variable_name", values_to = "max")


site_dic <- map_df(sites, class) %>% 
  pivot_longer(cols = everything(), names_to = "Variable_name", values_to = "Variable_type") %>% 
  left_join(min, by = "Variable_name") %>% 
  left_join(max, by = "Variable_name") %>% 
  mutate(Range = paste(min, max, sep = " - ")) %>% 
  select(-min, -max) %>% 
  mutate(How_measured = c(rep("defined", 2), rep("measured", 8)),
         Unit = c("Alrust, Vikesland", "alp1, sub2, bor4", rep("decimal degree", 2), "m a.s.l.", "mm", NA, "°C", NA, NA)) %>% 
  rename("Variable name" = "Variable_name",
         "Variable type" = "Variable_type",
         "Variable range or levels" = "Range",
         "How measured" = "How_measured",
         "Units/formats/treatment level coding" = "Unit")
 

### BLOCK
block_dic <- map_df(blocks, class) %>% 
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>% 
  mutate("Variable range or levels" = c("Alr1 - Vik5", "Alrust - Vikesland"),
         "How measured" = c(rep("defined", 2)),
         "Units/formats/treatment level coding" = c("Alr1, Vik5", "Alrust, Vikesland"))
  

### PLOTS
plot_dic <- map_df(plots, class) %>% 
  pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable type") %>% 
  mutate("Variable range or levels" = c("1 - 552", "Alr1 - Vik5", "0", "0"),
         "How measured" = c(rep("defined", 2), rep("measured", 2)),
         "Units/formats/treatment level coding" = c("1, 2, 3", "Alr1, Vik5", "degree", "degree"))

