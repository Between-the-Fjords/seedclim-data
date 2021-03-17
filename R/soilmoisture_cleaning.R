# This script is to clean the soil moisture point measurements
library("dataDownloader")
library(tidyverse)
library(lubridate)
# import the data



soilmoisture1516 <- read_csv("SeedClim_Plot_SoilMoisture/Soilmoisture_1516.csv", col_types = "cffffffdddddffc", na = c("#DIV/0!", "NA")) %>% 
  rename( #renaming the measurements because I want to do a pivot later and I think it makes more sense to have them as replicate number
    # "1" = "M1",
    # "2" = "M2",
    # "3" = "M3",
    # "4" = "M4",
    "turfID" = "TurfID",
    "blockFC" = "block",
    "blockSC" = "blockSD"
  ) %>% 
  mutate(
    # date = str_replace_all(date,"\\.", "/") #some of the dates were using the format mm/dd/yyyy. Eeer
    # date = mdy(date)
    date = if_else( #dates using . instead of / are in dmy and those using / are in mdy
      str_detect(date, "..\\...\\....."), #detect the pattern with the .
      dmy(date), #if TRUE, date is dmy
      mdy(date) # if false, date is mdy
    )
  ) %>% 
  select(!Moisture) #column Moisture is the mean of the four measurements, we can calculate it later if needed but for now I want raw data
# line 2175: manually changed "24,6" into 24.6 in colum M1
# 59 entries missing date

soilmoisture_2015_2016 <- read_csv("SeedClim_Plot_SoilMoisture/soilMoisture_2015-2016.csv", col_types = "cffffffdddddffc", na = c("#DIV/0!", "NA")) %>% 
  rename(
    # "1" = "M1",
    # "2" = "M2",
    # "3" = "M3",
    # "4" = "M4",
    "blockFC" = "block",
    "blockSC" = "blockSD"
  ) %>% 
  mutate(
    # date = str_replace(date, "[.]", "/"), #some of the dates were using the format mm/dd/yyyy. Eeer
    # date = mdy(date),
    date = if_else( #dates using . instead of / are in dmy and those using / are in mdy
      str_detect(date, "..\\...\\....."), #detect the pattern with the .
      dmy(date), #if TRUE, date is dmy
      mdy(date) # if false, date is mdy
    ),
    site = as_factor(str_to_upper(site))
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
    "M1" = "Measurement1",
    "M2" = "Measurement2",
    "M3" = "Measurement3",
    "M4" = "Measurement4",
    "weather" = "Weather",
    "recorder" = "Recorder"
  ) %>% 
  mutate(
    # date_indic = case_when(str_detect(date, ".."))
    # date = str_replace_all(date, "\\.", "/"), #some of the dates were using the format mm/dd/yyyy. Eeer
    # date1 = dmy(date), #dates were recorded using different formats
    # date2 = mdy(date),
    date = if_else( #dates using . instead of / are in dmy and those using / are in mdy
      str_detect(date, "..\\...\\....."), #detect the pattern with the .
      dmy(date), #if TRUE, date is dmy
      mdy(date) # if false, date is mdy
    )
  ) %>% 
  select(!SCBlock_1)

soilmoisture2018 <- read_csv("SeedClim_Plot_SoilMoisture/Soilmoisture2018.csv", col_types = "cfffffddddff", na = c("na", "NA", "u")) %>% 
  rename(
    "date" = "Date",
    "site" = "Site",
    "treatment" = "Treatment",
    "blockSC" = "SCBlock",
    "blockFC" = "FCBlock",
    "M1" = "Measurement1",
    "M2" = "Measurement2",
    "M3" = "Measurement3",
    "M4" = "Measurement4",
    "weather" = "Weather",
    "recorder" = "Recorder"
  ) %>% 
  mutate(
    date = if_else( #dates using . instead of / are in dmy and those using / are in mdy
      str_detect(date, "..\\...\\....."), #detect the pattern with the .
      dmy(date), #if TRUE, date is dmy
      mdy(date) # if false, date is mdy
    )
  )

soilmoisture_raw <- full_join(soilmoisture_2015_2016, soilmoisture2017) %>% 
  full_join(soilmoisture1516) %>% 
  full_join(soilmoisture2018) %>% 
  select(!c(treatment, removal)) %>% #those treatments and removal are confusing and using various labelling. TurfId and site name is enough to add the treatment later if needed.
  distinct(date, site, blockSC, M1, M2, M3, M4, .keep_all = TRUE) %>% 
  rename(
    "1" = "M1",
    "2" = "M2",
    "3" = "M3",
    "4" = "M4"
  ) %>% 
  pivot_longer(cols = c("1":"4"), names_to = "replicate", values_to = "soil_moisture")

# soilmoisture_raw <- read_csv("SeedClim_Plot_SoilMoisture.csv", col_types = "ffDffd") %>% 
#   rename(
#     "ID" = "X1" # unique ID for each measurement
#   ) 

soilmoisture_avg <- group_by(soilmoisture_raw, date, site, turfID) %>% 
  summarise(
    avg = mean(soil_moisture)
  )

# graph soil moisture vs date with site as fill and plotID as point label

ggplot(soilmoisture_avg, aes(x = date, y = avg)) + #, fill = site, colour = site
  # geom_bar()
  geom_point(size = 0.001) +
  facet_wrap(vars(site))
