#gridded temp and precip data
#load data monthlyClimate
load("~/OneDrive - University of Bergen/Research/FunCaB/SeedClim-Climate-Data/data/GriddedMonth_AnnualClimate2009-2016.Rdata")
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
  mutate(temp0916 = mean(value)) %>%
  select(c(siteID, temp0916)) %>%
  distinct(siteID, .keep_all = TRUE) %>%
  as.data.frame()

temperature

load("~/OneDrive - University of Bergen/Research/FunCaB/SeedClim-Climate-Data/data/GriddedDailyClimateData2009-2016.Rdata")
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
  mutate(precip0916 = sum(Precipitation)) %>%
  ungroup() %>%
  group_by(siteID) %>%
  mutate(precip0916 = mean(precip0916)) %>%
  select(c(siteID, precip0916)) %>%
  distinct(siteID, .keep_all = TRUE) %>%
  #mutate(annPrecip = lag(annPrecip1st) + annPrecip2nd) %>%
  as.data.frame()

precipitation


weather <- full_join(precipitation, temperature, by = "siteID") %>%
  mutate(tempLevel = recode(siteID, Ulvhaugen = 6.5, Lavisdalen = 6.5,  Gudmedalen = 6.5, Skjellingahaugen = 6.5, Alrust = 8.5, Hogsete = 8.5, Rambera = 8.5, Veskre = 8.5, Fauske = 10.5, Vikesland = 10.5, Arhelleren = 10.5, Ovstedal = 10.5)) %>%
  mutate(temp7010 = recode(siteID, Ulvhaugen=6.17, Lavisdalen=6.45, Gudmedalen=5.87, Skjellingahaugen=6.58, Alrust=9.14, Hogsete=9.17, Rambera=8.77, Veskre=8.67, Fauske=10.3, Vikesland=10.55, Arhelleren=10.60, Ovstedal=10.78)) %>%
  mutate(precip7010= recode(siteID, Ulvhaugen=596, Lavisdalen=1321, Gudmedalen=1925, Skjellingahaugen=2725, Alrust=789, Hogsete=1356, Rambera=1848, Veskre=3029, Fauske=600, Vikesland=1161, Arhelleren=2044, Ovstedal=2923)) %>%
  mutate(precipLevel = recode(siteID, Ulvhaugen = 600, Alrust = 600, Fauske = 600, Lavisdalen = 1200, Hogsete = 1200, Vikesland = 1200, Gudmedalen = 2000, Rambera = 2000, Arhelleren = 2000, Skjellingahaugen = 2700, Veskre = 2700, Ovstedal = 2700))
