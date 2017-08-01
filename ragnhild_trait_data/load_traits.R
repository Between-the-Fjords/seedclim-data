#### Libraries ####
library(lubridate)
library(ggplot2)
library(stringr)

#########################################################################

# loading the traits data for Ragnhild's traits

#########################################################################

# 1. RAGNHILD
#### Load trait data ####
traits <-read.csv("ragnhild_trait_data/leaftraits2016csv.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE)

#head(traits)

traits <- traits %>%
  rename(Height = Height..mm., Lth_1 = Lth.1..mm., Lth_2 = Lth.2..mm., Lth_3 = Lth.3..mm., Wet_mass = Wet.mass..g., Dry_mass = Dry.mass..g., siteID = Location, species = Species) %>%
  select(-Lth.average..mm.)%>%
  mutate(Date = mdy(Date))%>%
  #mutate(Dry_mass o= ifelse(Dry_mass == 0,yes = min(Dry_mass[Dry_mass > 0], na.rm = TRUE), no = Dry_mass)) %>% # accounting for zero recorded balance weight
  mutate(siteID = factor(siteID, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs"))) %>%
  mutate(LDMC = Dry_mass/Wet_mass) %>%
  mutate(Lth_ave = rowMeans(select(traits, matches("^Lth\\.\\d")), na.rm = TRUE)) %>%
  mutate(Dry_mass = replace(Dry_mass, Dry_mass < 0.0005, NA)) # this is the equivalent of the error/uncertainty in the balance.

#############################
###### LEAF AREA START ######

LA <- read.csv2("ragnhild_trait_data/Leaf area.csv", stringsAsFactors = FALSE)

LA <- transform(LA, Leaf_area = as.numeric(Leaf_area)) %>%
  rename(ID = Image_file)

#removal threshold
#choosing a lower threshold
LA <- LA %>%
  filter(Leaf_area > 0.1)


remspecies <- c("Eup_sp", "Tar_sp", "Alc_sp")
#traits <- traits[!traits$species %in% remspecies,] 

####### LEAF AREA END #######
#############################


################################
######## CN RATIO START ########

CN <-read.csv2("ragnhild_trait_data/CN_Ragnhild.csv", header=TRUE, sep=";", dec = ".", stringsAsFactors = FALSE)
dict_CN <- read.csv2("Ragnhild_trait_data/Dict_CN.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)
CN$Info <- as.character(CN$Info)


CN <- CN %>%
  mutate(siteID = substr(Name, 1,2), species = as.factor(substr(Name, 3,6)), Individual = substr(Name, 7,8), CN.ratio = as.numeric(CN.ratio)) %>%
  mutate(species = plyr::mapvalues(species, from = dict_CN$CN_ab, to = dict_CN$Species)) %>%
  filter(species != "etan") %>%
  mutate(siteID = plyr::mapvalues(siteID, from = c("UL", "LA", "GU", "SK", "AL", "HO", "RA", "VE", "FA", "VI", "AR", "OV"), to = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs"))) %>%
  mutate(ID = paste0(siteID, "_", species, "_", Individual, ".jpg")) %>%
  #mutate(N_weight = (N../100)*Weight, C_weight = (C../100)*Weight) %>%
  filter(!(ID == "Ves_Car_vag_1.jpg"))%>%
  select(CN.ratio, ID) %>%
  as.data.frame()

##### CN RATIO END #####
########################


#####################################################
###### Merge trait, CN, and the leaf area data ######

traitdata <- traits %>%
  mutate(ID = paste0(siteID, "_", species, "_", Individual, ".jpg")) %>%
  filter(!(ID == "Ves_Leo_aut_6.jpg")) %>%
  mutate(Site_sp = paste0(siteID,"_", species)) %>%
  full_join(LA, by = "ID") %>%
  full_join(CN, by = "ID") %>%
  mutate(SLA = Leaf_area/Dry_mass) %>%
  group_by(species) %>%
  mutate(
    CN_mean_global = mean(CN.ratio, na.rm = TRUE),
    SLA_mean_global = mean(SLA, na.rm = TRUE),
    Lth_mean_global = mean(Lth_ave, na.rm = TRUE),
    Height_mean_global = mean(Height, na.rm = TRUE),
    LDMC_mean_global = mean(LDMC, na.rm = TRUE),
    LA_mean_global = mean(Leaf_area, na.rm = TRUE) #,
    #count = n()
  ) %>%
  ungroup() %>%
  # include this section if you're interested in local means
  group_by(siteID, species) %>%
  mutate(
    SLA_mean = mean(SLA, na.rm = TRUE),
    Lth_mean = mean(Lth_ave, na.rm = TRUE),
    Height_mean = mean(Height, na.rm = TRUE),
    LDMC_mean = mean(LDMC, na.rm = TRUE),
    LA_mean = mean(Leaf_area, na.rm = TRUE),
    CN_mean = mean(CN.ratio, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(species = gsub("_", ".", species), siteID = as.character(plyr::mapvalues(siteID, from = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs"), to = c("Ulvhaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen", "Alrust", "Hogsete", "Rambera", "Veskre", "Fauske", "Vikesland", "Arhelleren", "Ovstedal")))) %>%
  select(siteID, species, Height, LDMC, Lth_ave, Leaf_area:CN_mean) %>%
  mutate(species = plyr::mapvalues(species, from = c("Hyp.mac", "Emp.nig"), to = c("Hype.mac", "Emp.her"))) %>%
  #rename(heightR = Height, Leaf_areaR = Leaf_area) %>%
  distinct(siteID, species, .keep_all = TRUE)

#traitdata <- traitdata %>%
#  mutate(SLA_mean = plyr::mapvalues(SLA_mean, from = "Inf", to = "NA")) %>%
#  mutate(SLA_mean_global = plyr::mapvalues(SLA_mean_global, from = "Inf", to = "NA"))

