#gridded temp and precip data
#load data monthlyClimate
load("/Volumes/fja062/PhD/Projects/2017_temperature_regulation_of_functional_groups/SeedClim-Climate-Data/GriddedMonth_AnnualClimate2009-2016.Rdata")
head(monthlyClimate)

library(lubridate)

#### temperature ####
temperature <- monthlyClimate %>%
  filter(Logger == "Temperature") %>%
  mutate(siteID = as.character(plyr::mapvalues(Site, from = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs"), to = c("Ulvhaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen", "Alrust", "Hogsete", "Rambera", "Veskre", "Fauske", "Vikesland", "Arhelleren", "Ovstedal")))) %>%
  mutate(Month = month(dateMonth), Year = year(dateMonth)) %>%
  filter(Month > 5, Month < 10) %>%
  filter(Year > 2009) %>% 
  group_by(siteID) %>%
  mutate(summer_temp = mean(value)) %>%
  select(c(siteID, summer_temp)) %>%
  distinct(siteID, .keep_all = TRUE) %>%
  as.data.frame()

temperature

load("/Volumes/fja062/PhD/Projects/2017_temperature_regulation_of_functional_groups/SeedClim-Climate-Data/GriddedDailyClimateData2009-2016.Rdata")
head(climate)

#### precipitation ####  
precipitation <- climate %>%
  mutate(siteID = as.character(plyr::mapvalues(Site, from = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs"), to = c("Ulvhaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen", "Alrust", "Hogsete", "Rambera", "Veskre", "Fauske", "Vikesland", "Arhelleren", "Ovstedal")))) %>%
  #ungroup() %>%
  #arrange(dateMonth) %>%
  #mutate(Month = as.numeric(substr(dateMonth, 6,7)), Year = year(dateMonth)) %>% 
  #mutate(ID = paste(siteID, Year, sep = "_")) %>%
  #group_by(ID) %>%
  #mutate(annPrecip2nd = sum(value[Month == c(1:9)]), annPrecip1st = sum(value[Month == c(10:12)])) %>%
  filter(Year > 2009) %>% 
  group_by(siteID, Year) %>%
  mutate(annprecip = sum(Precipitation)) %>%
  ungroup() %>%
  group_by(siteID) %>%
  mutate(annPrecip = mean(annprecip)) %>%
  select(c(siteID, annPrecip)) %>%
  distinct(siteID, .keep_all = TRUE) %>%
  #mutate(annPrecip = lag(annPrecip1st) + annPrecip2nd) %>%
  as.data.frame()

precipitation


weather <- full_join(precipitation, temperature, by = "siteID")
