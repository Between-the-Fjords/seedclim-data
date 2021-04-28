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
