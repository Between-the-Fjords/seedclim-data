#### SeedClim trait data cleaning ####

#### Load libraries ####
library(tidyverse)
library(lubridate)
library("dataDownloader")

#### Trait data from 2012 ####

## Read in data ##

my_sla         <- read.csv('Plant traits/Data/RawTraitData_SLA.csv', header=TRUE, stringsAsFactors = FALSE)
my_leaf_chem   <- read.csv('Plant traits/Data/raw_data_CN_2014Sept15.csv', header=TRUE, stringsAsFactors = FALSE)


## Site name dictionary ##

dict_Site_2012 <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
                          "old new
  Arh Arhelleren
  Øvs Ovstedalen
  Ves Veskre
  Skj Skjelingahaugen
  Låv Lavisdalen
  Gud Gudmedalen
  Ulv Ulvehaugen
  Vik Vikesland
  Høg Hogsete
  Ålr Alrust
  Fau Fauske
  Ram Rambera")

dict_Site_mistakes <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
                          "old new
  Ovstedal Ovstedalen
  Skjellingahaugen Skjelingahaugen
  Ulvhaugen Ulvehaugen")


## Clean and combine data sets ##

my_sla <- my_sla %>% 
  mutate(leaf_area_cm2 = true.leaf.area..cm2. + true.petiole.area,
         dry_weight_g = Weight*0.001,
         SLA_cm2_g = leaf_area_cm2/dry_weight_g) %>% 
  select(-Weight, -true.leaf.area..cm2., -true.petiole.area, -SLA..m2.kg.1.) %>% 
  mutate(siteID = plyr::mapvalues(Site, from = dict_Site_mistakes$old, to = dict_Site_mistakes$new)) %>% 
  separate(Species, c("Genus","species"), sep = ' ') %>% 
  mutate(Genus = substr(Genus, 1,3)) %>% 
  mutate(species = substr(species, 1,3)) %>% 
  mutate(species = ifelse(is.na(species), "sp", species)) %>% 
  mutate(species = paste0(Genus,".", species)) %>% 
  select(-Genus, -Site) %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate(year = "2012") %>% 
  rename(date = Date, dry_mass_g = dry_weight_g, individual = Individual) %>% 
  mutate(individual = as.character(individual)) %>% 
  select(-photo)


my_leaf_chem <- my_leaf_chem %>% 
  rename(CN_ratio=CN, 
         d13C=d13C.UCD, 
         d15N= d15N.UCD) %>% 
  filter(!Site == "Apple") %>% 
  mutate(Site = plyr::mapvalues(Site, from = dict_Site_2012$old, to = dict_Site_2012$new)) %>% 
  rename(siteID = Site, species = Species) %>% 
  mutate(year = "2012")



#### Trait data from 2016 & 2017 ####
 
#### Load trait data ####
 
 traits <- read.csv("Plant traits/Data/LeafTraits_SeedClim.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE)
 LA <- read.csv2("Plant traits/Data/Leaf_area_total.csv", stringsAsFactors = FALSE)
 CN <- read.csv2("Plant traits/Data/CNratio.csv", dec=".", sep=";")
 
#### Dictionaries ####
 
dict_CN <- read.csv2("Plant traits/Data/Dict_CN.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)

dict_Site_CN_2016 <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
                           "old new
  AR Arhelleren
  OV Ovstedalen
  VE Veskre
  SK Skjelingahaugen
  LA Lavisdalen
  GU Gudmedalen
  UL Ulvehaugen
  VI Vikesland
  HO Hogsete
  AL Alrust
  FA Fauske
  RA Rambera")

dict_Site_2016 <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
                                  "old new
Arh Arhelleren
Ovs Ovstedalen
Ves Veskre
Skj Skjelingahaugen
Lav Lavisdalen
Gud Gudmedalen
Ulv Ulvehaugen
Vik Vikesland
Hog Hogsete
Alr Alrust
Fau Fauske
Ram Rambera")
 
 
#### Cleaning the trait data before merging ####
 
 traits <- traits %>%
   rename(Height=Height..mm., Lth_1=Lth.1..mm., Lth_2= Lth.2..mm., Lth_3= Lth.3..mm., Wet_mass=Wet.mass..g., Dry_mass=Dry.mass..g., Site=Location) %>% #Renaming weird named columns
   select(-Lth.average..mm.) %>% #removing this column, as we make it again later
   mutate(Date = mdy(Date)) %>% #formating the date column
  mutate(year = year(Date)) %>% 
  mutate(year = as.character(year)) %>% 
   mutate(Site = factor(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs"))) %>% #Ordering the sites from cold to warm and dry to wet
  mutate(ID_LA = paste0(Site, "_", Species, "_", Individual, ".jpg")) %>%
  mutate(Site_sp=paste0(Site,"_", Species)) %>%
   mutate(Site = plyr::mapvalues(Site, from = dict_Site_2016$old, to = dict_Site_2016$new)) %>%
  mutate(ID_CN = paste0(Site, "_", Species, "_", Individual, ".jpg")) %>%
  separate(Species, c("Genus","species"), sep = '_') %>% 
  mutate(species = paste0(Genus,".", species)) %>% 
   mutate(Dry_mass = replace(Dry_mass, Dry_mass < 0.0005, NA)) %>% #Replacing anything that us under 0.0005 grams with NA because these values are so low and not very trustworthy (outside of the margin of error of the balance)
   mutate(Wet_mass = replace(Wet_mass, Wet_mass < 0.0005, NA)) %>% #Same as dry mass
   mutate(Lth_3 = replace(Lth_3, Lth_3 < 0.9, NA)) #One outlier that is very far off measurement 1 and 2, replacing with NA.
 
 
 #### Cleaning the leaf area data before merging ####
 
 LA<-transform(LA, Leaf_area = as.numeric(Leaf_area))
 
 LA <- LA %>%
   filter(Leaf_area > 0.1) #Lower threshold for data that is within the error of the scanner and the area calculations
 
 
 #### Merge the trait data and the leaf area data ####
 
 traitdata <- traits %>%
   left_join(LA, by=c("ID_LA"="Image_file"))
 
 
 #### Changes to the CN data before merging ####
 
 CN <- CN %>%
   mutate(Site= substr(Name, 1,2)) %>%
   mutate(Species = substr(Name, 3,6)) %>%
   mutate(Individual = substr(Name, 7,8)) %>%
   mutate(Species = plyr::mapvalues(Species, from = dict_CN$CN_ab, to = dict_CN$Species)) %>%
   mutate(Site = plyr::mapvalues(Site, from = dict_Site_CN_2016$old, to = dict_Site_CN_2016$new)) %>%
   mutate(ID = paste0(Site, "_", Species, "_", Individual, ".jpg")) %>%
   mutate(Species = gsub("_", "\\.", Species)) %>% 
   filter(!(Name=="VECAR101")) %>%  #Because it was a to small sample to get good data from it
   select(-Humidity.., -Name, -Weight, -Method, -N.Factor, -C.Factor, -N.Blank, -C.Blank, -Memo, -Info, -Date..Time, -N.Area, -C.Area) %>% 
   rename(C_percent=C.., N_percent = N.., CN_ratio = CN.ratio)
 
 #### Merge the trait data and the CN data ####
 
 traitdata <- traitdata %>%
   full_join(CN, by=c("ID_CN"="ID", "Site"="Site", "species"="Species", "Individual"="Individual"))
 
 #### Clean data set by removing outliers and setting thresholds ###
 
 traitdata <- traitdata %>% 
   filter(!(ID_LA =="Alr_Agr_cap_9.jpg")) %>%  #Looks damaged from picture
   filter(!(ID_LA == "Ves_Leo_aut_6.jpg")) %>%  #Looks damaged from scan of leaf area
   mutate(Dry_mass = replace(Dry_mass, Dry_mass < 0.0005, NA)) %>% #Replacing anything that us under 0.0005 grams with NA because these values are so low and not very trustworthy (outside of the margin of error of the balance)
   mutate(Wet_mass = replace(Wet_mass, Wet_mass < 0.0005, NA)) %>% #Same as for dry mass
   mutate(Lth_3 = replace(Lth_3, Lth_3 < 0.9, NA)) #One outlier that is very far off measurment 1 and 2, replacing with NA.

 
 ### Calculate traits and transform traits ###
 
 traitdata <- traitdata %>% 
   mutate(SLA = Leaf_area/Dry_mass) %>%
   mutate(LDMC = Dry_mass/Wet_mass)%>%
   mutate(leaf_thickness = rowMeans(select(traitdata, starts_with("Lth")), na.rm = TRUE)) %>% #Make the numbers only with four digits
   filter(LDMC<1) %>% 
   select(-Lth_1, -Lth_2, -Lth_3)
 
 #### Creating comments for flagged values ####
 
 traitdata <- traitdata %>% 
   group_by(Site, species) %>% 
   mutate(n_individuals = length(Individual)) %>% 
   mutate(flag = ifelse(n_individuals < 5, "Less then 5 individuals", NA)) %>% 
   select(-n_individuals)
 
 #### Changing names of columns to match SeedClim standards ####
 
 traitdata <- traitdata %>% 
   rename(siteID = Site, date = Date, individual = Individual, height_mm = Height, fresh_mass_g = Wet_mass, dry_mass_g = Dry_mass, leaf_area_cm2 = Leaf_area, SLA_cm2_g = SLA, LDMC_g_g = LDMC) %>% 
   select(-Image, -Comment, -ID_LA, -ID_CN, -Site_sp)
 
 #### Merging the 2012, 2016&2017 data together ####
 
 traitdata_full <- traitdata %>% 
   bind_rows(my_sla) %>% 
   bind_rows(my_leaf_chem)

  # traitdata_full %>% filter(!year == "2017") %>% ggplot(aes(x = siteID, y = height_mm, fill = year)) + geom_boxplot() 
 
#### Making table of the content of the dataset ####
 
range_table <- traitdata_full %>% 
   as_tibble() %>% 
   summarise(
     across(where(is.character), ~ paste(min(.), max(.), sep = " - ")),
     across(where(is.numeric), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
   ) %>% 
   pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")
 
 