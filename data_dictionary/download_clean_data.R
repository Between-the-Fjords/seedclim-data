# Download clean data from OSF
# install.packages("devtools")
#devtools::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)

node <- "npfa9"

# 1 Seed data
get_file(node = node,
         file = "Seed_predation_2018.csv",
         path = "seed_predation/data/",
         remote_path = "1_Seed_data")


# 2 Population data

# 3 Community
get_file(node = node,
         file = "seedclim.2020.4.15.zip",
         path = "community/data/",
         remote_path = "3_Community_data")


# 4 Phenology data
get_file(node = node,
         file = "Community_phenology_2014-2015.csv",
         path = "phenology/clean_data/",
         remote_path = "4_Phenology_data")


# 5 Trait data
get_file(node = node,
         file = "SeedClim_Trait_data_2012_2016.csv",
         path = "plant_traits/data/",
         remote_path = "5_Trait_data")

# 8 Environment data

# Temperature
get_file(node = node,
         file = "Temperature.csv.zip",
         path = "climate/data/",
         remote_path = "8_Environmental_data")

zipFile <- "climate/data/Temperature.csv.zip"
outDir <- "climate/data/"
unzip(zipFile, exdir = outDir)

# Precipitation
get_file(node = node,
         file = "Precipitation.csv.zip",
         path = "climate/data/",
         remote_path = "8_Environmental_data")

zipFile <- "climate/data/Precipitation.csv.zip"
outDir <- "climate/data/"
unzip(zipFile, exdir = outDir)

# Soilmoisture
get_file(node = node,
         file = "SoilMoisture.csv.zip",
         path = "climate/data/",
         remote_path = "8_Environmental_data")

zipFile <- "climate/data/SoilMoisture.csv.zip"
outDir <- "climate/data/"
unzip(zipFile, exdir = outDir)

# Soilmoisture point measurement
get_file(node = node,
         file = "seedclim_soilmoisture_plotlevel.csv",
         path = "climate/data/",
         remote_path = "8_Environmental_data")


# Gridded climate data
get_file(node = node,
         file = "GriddedDailyClimateData2009-2019.csv",
         path = "climate/data/",
         remote_path = "8_Environmental_data")
