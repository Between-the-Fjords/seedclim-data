### READ IN GRIDDED CLIMATE DATA 2009 - XXX

# packagage
library("dataDownloader")
library("lubridate")
library("tidyverse")


# download data from OSF
# get_file(node = "npfa9",
#          file = "Gridded_Climate_Data_2009-2019.zip",
#          path = "climate/data/",
#          remote_path = "8_Environmental_data/Raw_data")
# 
# zipFile <- "climate/data/Gridded_Climate_Data_2009-2019.zip"
# outDir <- "climate/data/"
# unzip(zipFile, exdir = outDir)

# FUNCTIONS
# Function to read in the data
ReadInFiles <- function(textfile){
  dd <- read.table(textfile, colClasses = "character")
  colnames(dd) <- c("siteID", "year", "month", "day", "temperature", "rel_air_moisture", "wind", "cloud_cover", "precipitation") # rename variables
  dd <- dd[-1,] # remove first columne
  dd$date <- ymd(paste(dd$year, dd$month, dd$day)) # create date object
  return(dd)
}

# list files
myfiles <- list.files(path ="climate/data/AH2019/", pattern='\\.dat$', recursive = TRUE, full.names = TRUE)

# make a list of textfiles
climate_gridded_raw <- plyr::ldply(myfiles, ReadInFiles)

climate_gridded <- climate_gridded_raw %>% 
  as.tibble() %>% 
  # replace site names by real names
  mutate(siteID = plyr::mapvalues(siteID, c("888001", "888002", "888003", "888004", "888005", "888006", "888007", "888008", "888009", "888010", "888011", "888012", "888191", "888192"), 
                                  c("Alrust", "Arhelleren", "Fauske", "Gudmedalen", "Hogsete", "Lavisdalen", "Ovstedalen", "Rambera", "Skjelingahaugen", "Ulvehaugen", "Veskre", "Vikesland", "Joasete", "Liahovden"))) %>% 
  mutate(siteID = factor(siteID, levels = c("Ulvehaugen", "Lavisdalen", "Gudmedalen", "Skjelingahaugen", "Alrust", "Hogsete", "Rambera", "Veskre", "Fauske", "Vikesland", "Arhelleren", "Ovstedalen", "Joasete", "Liahovden"))) %>% 
  mutate(across(c(temperature:precipitation), as.numeric)) %>% 
  select(date, siteID, temperature:precipitation) %>% 
  pivot_longer(cols = temperature:precipitation, names_to = "variable", values_to = "value")

climate <- climate_gridded %>% 
  filter(!siteID %in% c("Joasete", "Liahovden"))

climate_threeD <- climate_gridded %>% 
  filter(siteID %in% c("Vikesland", "Joasete", "Liahovden"))

# Change directory
write_csv(climate, path = "climate/data/GriddedDailyClimateData2009-2019.csv", col_names = TRUE)
write_csv(climate_threeD, path = "climate/data/THREE_D_GriddedDailyClimateData2009-2019.csv", col_names = TRUE)

