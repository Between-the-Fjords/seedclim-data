# This script is to clean the soil moisture point measurements
library("dataDownloader")
library(tidyverse)
library(lubridate)

# import the raw data
get_file(node = "npfa9",
         file = "Plot_SoilMoisture.zip",
         path = "data",
         remote_path = "8_Environmental_data/Raw_data")

#unziping
zipFile <- "data/Plot_SoilMoisture.zip"
if(file.exists(zipFile)){
  outDir <- "data"
  unzip(zipFile, exdir = outDir)
}

#importing the data

soilmoisture1516 <- read_csv("cleaning_code/8_environment_data/climate/data/SeedClim_Plot_SoilMoisture/Soilmoisture_1516.csv", col_types = "cffffffdddddffc", na = c("#DIV/0!", "NA")) %>% 
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



soilmoisture_2015_2016 <- read_csv("cleaning_code/8_environment_data/climate/data/SeedClim_Plot_SoilMoisture/soilMoisture_2015-2016.csv", col_types = "cffffffdddddffc", na = c("#DIV/0!", "NA")) %>% 
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
# It might be that soilmoisture_2015_2016 is similar to soilmoisture1516, I'll do a distinct() later



soilmoisture2017 <- read_csv("cleaning_code/8_environment_data/climate/data/SeedClim_Plot_SoilMoisture/Soilmoisture2017.csv", col_types = "cfffffddddff", na = c("na", "NA")) %>% 
  rename(
    "date" = "Date",
    "site" = "Site",
    "turfID" = "Turf ID",
    "treatment" = "Treatment",
    "blockSC" = "SCBlock...5",
    # "blockSC2" = "SCBlock", #There are two SCBlock columns in the raw file, but it seems there are identical
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
  select(!SCBlock...6)



soilmoisture2018 <- read_csv("cleaning_code/8_environment_data/climate/data/SeedClim_Plot_SoilMoisture/Soilmoisture2018.csv", col_types = "cfffffddddff", na = c("na", "NA", "u")) %>% 
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
# dates were missing (only the year was written) and added manually based on the fieldwork plan: 2018-07-02 (ARH), 2018-07-01 (OVS)



soilmoisture_extra <- read_csv("cleaning_code/8_environment_data/climate/data/SeedClim_Plot_SoilMoisture/SeedClim_soilmoisture_archive.csv", col_types = "cfffffddddffcc", na = c("na", "NA", "u")) %>% 
  mutate(
    date = mdy(date)
  )

# 
# soilmoisture_raw <- full_join(soilmoisture_2015_2016, soilmoisture2017) %>% 
#   full_join(soilmoisture1516) %>% 
#   full_join(soilmoisture2018) %>% 
  
soilmoisture_raw  <- bind_rows(
  soilmoisture1516,
  soilmoisture_2015_2016,
  soilmoisture2017,
  soilmoisture2018,
  soilmoisture_extra
) %>% 
  select(!c(treatment, removal)) %>% #those treatments and removal are confusing and using various labelling. TurfId and site name is enough to add the treatment later if needed.
  mutate(
    site = str_replace_all(site, c( #need to replace site name with full names
      "ULV" = "Ulvehaugen",
      "LAV" = "Lavisdalen",
      "HOG" = "Hogsete",
      "Hosgete" = "Hogsete",
      "VIK" = "Vikesland",
      "GUD" = "Gudmedalen",
      "RAM" = "Rambera",
      "ARH" = "Arhelleren",
      "SKJ" = "Skjelingahaugen",
      "Skjellingahaugen" = "Skjelingahaugen",
      "VES" = "Veskre",
      "ALR" = "Alrust",
      "OVS" = "Ovstedalen",
      "FAU" = "Fauske"
    )), 
    year = year(date),
  blockID = str_c(str_sub(site, 1, 3), blockSC, sep = "", collapse = NULL), #blockID needs to be in the format [first 3 letters of site][block number]
  turfID = str_replace_all(turfID, c(" " = ""))
    ) %>% 
  distinct(date, site, blockID, M1, M2, M3, M4, .keep_all = TRUE) %>% 
  rename(
    "1" = "M1",
    "2" = "M2",
    "3" = "M3",
    "4" = "M4",
    "transcriber_comment" = "transcriber's comments",
    "siteID" = "site",
    # "blockID" = "blockSC",
    "blockID_FC" = "blockFC"
  ) %>% 
  pivot_longer(cols = c("1":"4"), names_to = "replicate") %>% 
  select(year, date, siteID, turfID, blockID, blockID_FC, replicate, value, weather, recorder, comments, transcriber_comment)

write_csv(soilmoisture_raw, "cleaning_code/8_environment_data/climate/data/VCG_clean_soilmoisture_plotlevel_1010_2015-2018.csv")


# making a graph to have an overview of the campaigns and see if something is missing

soilmoisture_avg <- group_by(soilmoisture_raw, date, siteID, turfID) %>% 
  summarise(
    avg = mean(value)
  )

# graph soil moisture vs date with site as fill and plotID as point label

ggplot(soilmoisture_avg, aes(x = date, y = avg)) + #, fill = site, colour = site
  # geom_bar()
  geom_point(size = 0.001) +
  facet_wrap(vars(siteID))



