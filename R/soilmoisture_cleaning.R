# This script is to clean the soil moisture point measurements
library("dataDownloader")
library(tidyverse)
# import the data from OSF and read data


soilmoisture_raw <- read_csv("SeedClim_Plot_SoilMoisture.csv", col_types = "ffDffd") %>% 
  rename(
    "ID" = "X1" # unique ID for each measurement
  ) 

# graph soil moisture vs date with site as fill and plotID as point label

ggplot(soilmoisture_raw, aes(x = date, y = soil_moisture, fill = site, colour = site)) +
  # geom_bar()
  geom_point()
