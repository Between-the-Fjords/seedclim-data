# TBI data cleaning
library(readxl) #require packages
library(tidyverse)
library(janitor)

# TBI data
TBI_raw <- read_excel("decomposition/data/TBI_141516_07082017.xlsx")

#change particular columns to factors 
TBI <- TBI_raw %>% 
  clean_names() |> 
  mutate(siteID = recode(site, 
                         "Arh"="Arhelleren", 
                         "Alr"="Alrust", 
                         "Fau"="Fauske", 
                         "Gud"="Gudmedalen", 
                         "Hog"= "Hogsete", 
                         "Lav"="Lavisdalen", 
                         "Ovs"= "Ovstedal", 
                         "Ram"="Rambera", 
                         "Skj"= "Skjelingahaugen", 
                         "Ulv"="Ulvehaugen", 
                         "Ves"="Veskre", 
                         "Vik"="Vikesland"),
         year = as.factor(year)) %>%
  # remove where tea bag is missing
  filter(!comment %in% c("missing")) %>% 
  select(year, siteID, temperature_level = temp, precipitation_level = prec, 
         bagID = id, S = s, k, burial_date, 
         initial_weight_gteabag:final_weight_rtea, 
         Ag = ag, Ar = ar, Wt = wt, time, comment)

write_csv(TBI, "decomposition/data/VCG_clean_decomposition_teabag_2014.csv")

