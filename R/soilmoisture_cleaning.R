# This script is to clean the soil moisture point measurements
library("dataDownloader")
library(tidyverse)
library(lubridate)
# import the data from OSF and read data

soilmoisture1516 <- read_csv("SeedClim_Plot_SoilMoisture/Soilmoisture_1516.csv", col_types = "cffffffdddddffc", na = c("#DIV/0!", "NA")) %>% 
  rename( #renaming the measurements because I want to do a pivot later and I think it makes more sense to have them as replicate number
    "1" = "M1",
    "2" = "M2",
    "3" = "M3",
    "4" = "M4",
    "turfID" = "TurfID",
    "blockFC" = "block"
  ) %>% 
  mutate(
    date = mdy(date)
  ) %>% 
  select(!Moisture) #column Moisture is the mean of the four measurements, we can calculate it later if needed but for now I want raw data
# line 2175: manually changed "24,6" into 24.6 in colum M1

soilmoisture_2015_2016 <- read_csv("SeedClim_Plot_SoilMoisture/soilMoisture_2015-2016.csv", col_types = "cffffffdddddffc", na = c("#DIV/0!", "NA")) %>% 
  rename(
    "1" = "M1",
    "2" = "M2",
    "3" = "M3",
    "4" = "M4",
    "blockFC" = "block"
  ) %>% 
  mutate(
    date = mdy(date)
  ) %>% 
  select(!Moisture)
# line 2175: manually changed "24,6" into 24.6 in colum M1
# It might be that soilmoisture_2015_2016 is similar to soilmoisture1516, I'll check later

soilmoisture2017 <- read_csv("SeedClim_Plot_SoilMoisture/Soilmoisture2017.csv", col_types = "cfffffddddff", na = c("na", "NA")) %>% 
  rename(
    "date" = "Date",
    "site" = "Site",
    "turfID" = "Turf ID",
    "treatment" = "Treatment",
    "blockSC" = "SCBlock", #I will need to figure out why there is a second a priori identical column
    # "blockSC2" = "SCBlock",
    "1" = "Measurement1",
    "2" = "Measurement2",
    "3" = "Measurement3",
    "4" = "Measurement4",
    "weather" = "Weather",
    "recorder" = "Recorder"
  )

# soilmoisture_raw <- read_csv("SeedClim_Plot_SoilMoisture.csv", col_types = "ffDffd") %>% 
#   rename(
#     "ID" = "X1" # unique ID for each measurement
#   ) 

# graph soil moisture vs date with site as fill and plotID as point label

ggplot(soilmoisture_raw, aes(x = date, y = soil_moisture)) + #, fill = site, colour = site
  # geom_bar()
  geom_point() +
  facet_wrap(vars(site))
