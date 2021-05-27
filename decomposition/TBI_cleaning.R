# TBI data cleaning
library(readxl) #require packages
library(tidyverse)

# TBI data
TBI_raw <- read_excel("decomposition/data/TBI_141516_07082017.xlsx")

#change particular columns to factors 
TBI <- TBI_raw %>% 
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
  select(year, siteID, temperature_level = Temp, precipitation_level = Prec, BagID = ID, S, k, BurialDate, InitialWeight_Gteabag = `InitialWeight Gteabag`, InitialWeight_Rteabag = `InitialWeight Rteabag`, InitialWeight_Gtea = `InitialWeight Gtea`, InitialWeight_Rtea = `InitialWeight Rtea`, RecoveryDate, FinalWeight_Gteabag = `FinalWeight Gteabag`, FinalWeight_Rteabag = `FinalWeight Rteabag`, FinalWeight_Gtea = `FinalWeight Gtea`, FinalWeight_Rtea = `FinalWeight Rtea`, Ag, Ar, Wt, Time, comment)

write_csv(TBI, "decomposition/Decomposition_teabag_2014_clean.csv")








