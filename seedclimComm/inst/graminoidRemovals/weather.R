#gridded temp and precip data
#load data monthlyClimate
load("~/Documents/SeedClim-Climate-Data/GriddedMonth_AnnualClimate2009-2016.Rdata")
str(monthlyClimate)

library(lubridate)

temperature <- monthlyClimate %>%
  filter(Logger == "Temperature") %>%
  mutate(siteID = as.character(plyr::mapvalues(Site, from = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs"), to = c("Ulvhaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen", "Alrust", "Hogsete", "Rambera", "Veskre", "Fauske", "Vikesland", "Arhelleren", "Ovstedal")))) %>%
  mutate(Month = month(dateMonth), Year = year(dateMonth)) %>%
  filter(Month > 5, Month < 9) %>%
  group_by(Year, siteID) %>%
  mutate(summer_temp = mean(value)) %>%
  select(-c(dateMonth:value), -Month) %>%
  distinct(Year, siteID, .keep_all = TRUE)

  
precipitation <- monthlyClimate %>%
  filter(Logger == "Precipitation") %>%
  mutate(siteID = as.character(plyr::mapvalues(Site, from = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs"), to = c("Ulvhaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen", "Alrust", "Hogsete", "Rambera", "Veskre", "Fauske", "Vikesland", "Arhelleren", "Ovstedal")))) %>%
  ungroup() %>%
  arrange(dateMonth) %>%
  mutate(Month = as.numeric(substr(dateMonth, 6,7)), Year = year(dateMonth)) %>% 
  mutate(ID = paste(siteID, Year, sep = "_")) %>%
  group_by(ID) %>%
  mutate(annPrecip2nd = sum(value[Month == c(1:9)]), annPrecip1st = sum(value[Month == c(10:12)])) %>%
  arrange(siteID) %>%
  distinct(ID, .keep_all = TRUE) %>%
  ungroup() %>%
  mutate(annPrecip = lag(annPrecip1st) + annPrecip2nd) %>%
  select(c(siteID, Year, annPrecip))


weather <- full_join(precipitation, temperature, by = c("Year", "siteID")) %>%
  mutate(Year = as.factor(Year))
