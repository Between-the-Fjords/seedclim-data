### READ IN GRIDDED CLIMATE DATA 2009 - XXX

# Explanation for variables
# TAM er temperatur (døgnmiddel)
# UUM - rel.luftfuktighet
# FFM - middelvind
# NNM - midlere skydekke (i 8-deler)
# RR - døgnnedbør

# LIBRARIES
library("dataDownloader")
library("lubridate")
library("tidyverse")


# download data from OSF
# get_file(node = "npfa9",
#          file = "Gridded_Climate_Data_2009-2019.zip",
#          path = "climate/data/",
#          remote_path = "8_Environmental_data/Raw_data")
# 
# zipFile <- "climate/data/VCG_raw_gridded_climate_2008-2022.zip"
# outDir <- "climate/data/"
# unzip(zipFile, exdir = outDir)

# FUNCTIONS
# Function to read in the data
ReadInFiles <- function(textfile){
  dd <- read.table(textfile, colClasses = "character")
  colnames(dd) <- c("siteID", "year", "month", "day", "temperature", "cloud_cover", "rel_air_moisture", "wind", "precipitation") # rename variables
  dd <- dd[-1,] # remove first columne
  dd$date <- ymd(paste(dd$year, dd$month, dd$day)) # create date object
  return(dd)
}

# Connect to data on P drive
myfiles <- list.files(path ="cleaning_code/8_environment_data/climate/data/AH2022/", pattern='\\.dat$', recursive = TRUE, full.names = TRUE)

# make a list of textfiles
climate_gridded_raw <- plyr::ldply(myfiles, ReadInFiles)

climate_gridded <- climate_gridded_raw %>% 
  as_tibble() %>% 
  # replace site names by real names
  mutate(siteID = plyr::mapvalues(siteID, c("888001", "888002", "888003", "888004", "888005", "888006", "888007", "888008", "888009", "888010", "888011", "888012", "888191", "888192"), 
                                  c("Alrust", "Arhelleren", "Fauske", "Gudmedalen", "Hogsete", "Lavisdalen", "Ovstedalen", "Rambera", "Skjelingahaugen", "Ulvehaugen", "Veskre", "Vikesland", "Joasete", "Liahovden"))) %>% 
  mutate(siteID = factor(siteID, levels = c("Ulvehaugen", "Lavisdalen", "Gudmedalen", "Skjelingahaugen", "Alrust", "Hogsete", "Rambera", "Veskre", "Fauske", "Vikesland", "Arhelleren", "Ovstedalen", "Joasete", "Liahovden"))) %>% 
  mutate(across(c(temperature:precipitation), as.numeric)) %>% 
  select(date, siteID, temperature:precipitation) %>% 
  pivot_longer(cols = temperature:precipitation, names_to = "variable", values_to = "value") |> 
  # remove wrong precip values
  filter(value > -9000)

climate <- climate_gridded %>% 
  filter(!siteID %in% c("Joasete", "Liahovden"))

climate_threeD <- climate_gridded %>% 
  filter(siteID %in% c("Vikesland", "Joasete", "Liahovden"))

# Change directory
write_csv(climate, file = "cleaning_code/8_environment_data/data/VCG_clean_gridded_daily_climate_2008-2022.csv", col_names = TRUE)
write_csv(climate_threeD, file = "cleaning_code/8_environment_data/data/THREE_D_GriddedDailyClimateData2008-2022.csv", col_names = TRUE)

# Calculate Monthly Mean
monthlyClimate <- climate %>%
  #select(-Year, -Month, -Day) %>% 
  #gather(key = Logger, value = value, -Site, -Date) %>% 
  mutate(dateMonth = dmy(paste0("15-",format(date, "%b.%Y")))) %>%
  group_by(dateMonth, variable, siteID) %>%
  summarise(n = n(), value = mean(value), sum = sum(value)) %>% 
  mutate(value = ifelse(variable == "precipitation", sum, value)) %>% 
  select(-n, -sum)


climate %>%
  # mutate(dateMonth = dmy(paste0("15-",format(Date, "%b.%Y")))) %>%
  # group_by(Site, dateMonth) %>% 
  # summarise(value = sum(Precipitation)) %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Year, Site) %>% 
  summarise(value = sum(precipitation)) %>% 
  # filter(Logger == "Precipitation") %>% 
  # mutate(year = year(dateMonth)) %>%
  # group_by(Site, year) %>%
  # summarise(value = sum(value)) %>%
  ggplot(aes(x = Year, y = value)) +
  geom_point() +
  facet_wrap(~ Site)


# Calculate Annual Means
annualClimate <- monthlyClimate %>% 
  mutate(dateYear = year(dateMonth)) %>%
  group_by(dateYear, variable, siteID) %>%
  summarise(annualMean = mean(value))

#save(monthlyClimate, annualClimate, file = paste0("GriddedMonth_AnnualClimate2009-2017", ".Rdata"))
#load(file = "GriddedMonth_AnnualClimate2009-2016.RData", verbose = TRUE)


# Making Figures
# Temperature
climate %>% 
  filter(variable == "temperature") %>% 
  ggplot(aes(x = date, y = value, color = siteID)) +
  geom_line() +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~ siteID) + 
  theme(legend.position="none") +
  ggtitle("Temperature")


# RelAirMoisture
climate %>% 
  filter(variable == "rel_air_moisture") %>%
  ggplot(aes(x = date, y = value, color = siteID)) +
  geom_line() +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~ siteID) + 
  theme(legend.position="none") +
  ggtitle("Relative Air Moisture")

# Wind
climate |> 
  filter(variable == "wind") |> 
  ggplot(aes(x = date, y = value, color = siteID)) +
  geom_line() +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~ siteID) + 
  theme(legend.position="none") +
  ggtitle("Wind")

# CloudCover
climate |> 
  filter(variable == "cloud_cover") |> 
  ggplot(aes(x = date, y = value, color = siteID)) +
  geom_line() +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~ siteID) + 
  theme(legend.position="none") +
  ggtitle("Cloud Cover")

# Precipitation
climate |> 
  filter(variable == "precipitation") |> 
  ggplot(aes(x = date, y = value, color = siteID)) +
  geom_line() +
  scale_colour_brewer(palette="Paired") +
  facet_wrap(~ siteID) + 
  theme(legend.position="none") +
  ggtitle("Precipitation")


