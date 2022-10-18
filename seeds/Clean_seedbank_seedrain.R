### seed

library(tidyverse)
library(readxl)
library(writexl)

#######################################################################################
#### SEEDBANK DATA ####
seedbank_raw <- read_excel("seeds/data/Vandvik_OIKOS.xlsx", sheet = "Vandvik_SB")
vegetation_raw <- read_excel("seeds/data/Vandvik_OIKOS.xlsx", sheet = "Vandvik_Veg")


seedbank <- seedbank_raw |>
  pivot_longer(cols = Achillea.millefolium:Viola.tricolor, names_to = "species", values_to = "nr_seeds") |> 
mutate(Site = recode(Site, 
                     "Arh"="Arhelleren", 
                     "Alr"="Alrust", 
                     "Fau"="Fauske", 
                     "Gud"="Gudmedalen", 
                     "Hog"= "Hogsete", 
                     "Lav"="Lavisdalen", 
                     "Ovs"= "Ovstedalen", 
                     "Ram"="Rambera", 
                     "Skj"= "Skjelingahaugen", 
                     "Ulv"="Ulvehaugen", 
                     "Ves"="Veskre", 
                     "Vik"="Vikesland"),
       year = 2009) |> 
  select(year, siteID = Site, replicate = Replicate, sampling_scale_cm2 = Scale, nr_individual = Nind, nr_species = Nsp, species, nr_seeds)
  

vegetation <- vegetation_raw |> 
  pivot_longer(cols = Achillea.millefolium:Viola.tricolor, names_to = "species", values_to = "presence") |> 
  mutate(Site = recode(Site, 
                           "Arh"="Arhelleren", 
                           "Alr"="Alrust", 
                           "Fau"="Fauske", 
                           "Gud"="Gudmedalen", 
                           "Hog"= "Hogsete", 
                           "Lav"="Lavisdalen", 
                           "Ovs"= "Ovstedalen", 
                           "Ram"="Rambera", 
                           "Skj"= "Skjelingahaugen", 
                           "Ulv"="Ulvehaugen", 
                           "Ves"="Veskre", 
                           "Vik"="Vikesland"),
       year = 2009) |> 
  select(year, siteID = Site, replicate = Replicate, sampling_scale_cm2 = Scale, nr_species = Nsp, species, presence)


write_xlsx(x = list(seedbank = seedbank, 
                    vegetation = vegetation), 
           path = "seeds/data/VCG_clean_seedbank.xlsx")

#######################################################################################

seedrain_raw <- read_excel("seeds/data/SEEDCLIM seedrain 2010 data.xlsx", sheet = "seedrain and vegetation data")


seedrain <- seedrain_raw |> 
  pivot_longer(cols = Ach.mil:Viol.sp, names_to = "species", values_to = "value") |> 
  mutate(year = 2010) |> 
  select(year, siteID = ...2, blockID = ...1, treatment = `S/V`, species, value) |> 
  # remove row with sums
  filter(!is.na(siteID)) |> 
  pivot_wider(names_from = treatment, values_from = value) |> 
  mutate(siteID = recode(siteID, 
                       "Arh"="Arhelleren", 
                       "Alr"="Alrust", 
                       "Fau"="Fauske", 
                       "Gud"="Gudmedalen", 
                       "Hog"= "Hogsete", 
                       "Lav"="Lavisdalen", 
                       "Ovs"= "Ovstedalen", 
                       "Ram"="Rambera", 
                       "Skj"= "Skjelingahaugen", 
                       "Ulv"="Ulvehaugen", 
                       "Ves"="Veskre", 
                       "vik"="Vikesland")) |> 
  mutate(blockID = str_remove(blockID, "block"),
         blockID = paste0(substr(siteID, 1, 3), blockID)) |> 
  rename(nr_seed = S, cover = v)


write_csv(seedrain, file = "seeds/data/VCG_clean_seedrain.csv")


#######################################################################################
