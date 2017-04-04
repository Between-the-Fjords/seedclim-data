#### Libraries ####
library("tidyr")
library("dplyr")
library("ggplot2")
library("lubridate")
library("mosaic")

#### Load trait data ####
traits <-read.csv("Traits/data/leaftraits2016csv.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE)
#head(traits)
#str(traits)


#### Cleaning the trait data ####

#head(traits)

traits <- traits %>%
  rename(Height=Height..mm., Lth_1=Lth.1..mm., Lth_2= Lth.2..mm., Lth_3= Lth.3..mm., Wet_mass=Wet.mass..g., Dry_mass=Dry.mass..g., Site=Location) %>%
  select(-Lth.average..mm.)%>%
  mutate(Date = mdy(Date))%>%
  #mutate(Dry_mass = ifelse(Dry_mass == 0,yes = min(Dry_mass[Dry_mass > 0], na.rm = TRUE), no = Dry_mass)) %>% #accounting for zero recorded balance weight
  mutate(Site = as.character(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs"))) %>%
  mutate(T_level = recode(Site, Ulv = "1", Lav = "1",  Gud = "1", Skj = "1", Alr = "2", Hog = "2", Ram = "2", Ves = "2", Fau = "3", Vik = "3", Arh = "3", Ovs = "3")) %>%
  mutate(Temp = recode(Site, Ulv=6.17, Lav=6.45, Gud=5.87, Skj=6.58, Alr=9.14, Hog=9.17, Ram=8.77, Ves=8.67, Fau=10.3, Vik=10.55, Arh=10.60, Ovs=10.78))%>%
  mutate(Precip= recode(Site, Ulv=596, Lav=1321, Gud=1925, Skj=2725, Alr=789, Hog=1356, Ram=1848, Ves=3029, Fau=600, Vik=1161, Arh=2044, Ovs=2923))%>%
  mutate(P_level = recode(Site, Ulv = "1", Alr = "1", Fau = "1", Lav = "2", Hog = "2", Vik = "2", Gud = "3", Ram = "3", Arh = "3", Skj = "4", Ves = "4", Ovs = "4")) %>%
  mutate(LDMC=Dry_mass/Wet_mass)%>%
  mutate(Lth_ave=rowMeans(select(traits, matches("^Lth\\.\\d")), na.rm = TRUE)) %>%
  mutate(Dry_mass = replace(Dry_mass, Dry_mass < 0.0002, NA)) # this is the equivalent of the error/uncertainty in the balance.


#### Load leaf area data ####

LA <- read.csv2("Traits/data/Leaf area.csv", stringsAsFactors = FALSE)


LA<-transform(LA, Leaf_area = as.numeric(Leaf_area))
LA <- LA %>%
  filter(Leaf_area > 0.1)

#### Merge the trait data and the leaf area data and make the means ####

traitdata <- traits %>%
  mutate(ID = paste0(Site, "_", Species, "_", Individual, ".jpg")) %>%
  mutate(Site_sp=paste0(Site,"_", Species)) %>%
  full_join(LA, by=c("ID"="Image_file")) %>%
  mutate(SLA=Leaf_area/Dry_mass) %>%
  group_by(Species) %>%
  mutate(
    SLA_mean_global = mean(SLA, na.rm = TRUE),
    Lth_mean_global = mean(Lth_ave, na.rm = TRUE),
    Height_mean_global = mean(Height, na.rm = TRUE),
    LDMC_mean_global = mean(LDMC, na.rm = TRUE),
    LA_mean_global = mean(Leaf_area, na.rm = TRUE) #,
    #count = n()
  ) %>%
  ungroup() %>%
  group_by(Site, Species) %>%
  mutate(
    SLA_mean = mean(SLA, na.rm = TRUE),
    Lth_mean = mean(Lth_ave, na.rm = TRUE),
    Height_mean = mean(Height, na.rm = TRUE),
    LDMC_mean = mean(LDMC, na.rm = TRUE),
    LA_mean = mean(Leaf_area, na.rm = TRUE)
  ) %>%
  ungroup()


#This was used to see which of the leaves in the leaf area dataset was not present in the traits dataset
#traitdata %>%
#filter(is.na(Location))%>%
#select(ID, Leaf_area)


#### Load CN data ####

CN <- read.csv2("Traits/data/CNratio.csv", dec=".", sep=";")
dict_CN <- read.csv2("Traits/data/Dict_CN.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)

dict_Site <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
  "old new
  AR Arh
  OV Ovs
  VE Ves
  SK Skj
  LA Lav
  GU Gud
  UL Ulv
  VI Vik
  HO Hog
  AL Alr
  FA Fau
  RA Ram")

CN<-CN %>%
  mutate(Site= substr(Name, 1,2))%>%
  mutate(Species = substr(Name, 3,6)) %>%
  mutate(Individual = substr(Name, 7,8))%>%
  mutate(Species = plyr::mapvalues(Species, from = dict_CN$CN_ab, to = dict_CN$Species))%>%
  mutate(Site = plyr::mapvalues(Site, from = dict_Site$old, to = dict_Site$new))%>%
  mutate(ID = paste0(Site, "_", Species, "_", Individual, ".jpg"))%>%
  mutate(N_weight = (N../100)*Weight)%>%
  mutate(C_weight = (C../100)*Weight)%>%
  filter(!(Name=="VECAR101"))


#### Merge the trait data and the CN data ####

traitdata <- traitdata %>%
  full_join(CN, by=c("ID"="ID"))

traitdata<-traitdata%>%
  select(-Humidity.., -Name, -Weight, -Method, -N.Factor, -C.Factor, -N.Blank, -C.Blank, -Memo, -Info, -Date..Time, -Site.y, -Species.y, -Individual.y, -N.., -C.., -N.Area, -C.Area) %>%
  rename(Site = Site.x, Species = Species.x)%>%
  group_by(Site, Species) %>%
  mutate(CN_ratio_mean = mean(CN.ratio, na.rm = TRUE))%>%
  ungroup()



#### Add info about species ####

species_info<- read.csv2("Traits/data/species_info.csv", sep=";")

species_info <- species_info %>%
  select(species, family, functionalGroup, lifeSpan, occurrence, occurrence.2) %>%
  mutate(species=gsub("\\.", "_", species))

traitdata <- traitdata %>%
  left_join(species_info, by = c("Species"="species"))


#### Finding errors ####


#traitdata %>%
  #filter(Dry_mass > 8.5)%>%
  #filter(is.na(Site))%>% 
  #filter(Dry_mass > 8.5) %>%
  #filter(Lth_ave > 60) %>%
  #filter(traitdata, SLA>750)

LDMC_mistakes<- traitdata%>%
  group_by(Individual.x)%>%
  filter(Dry_mass>Wet_mass)

# I am not sure that I trust all of these measurements as they are super large. The once who actually are succulents are okay, or rolled leaves. But there might just be some of the leaf thickness measurement people who didn't do it correctly
Succulents<-traitdata%>%
  filter(Lth_ave>0.5)


bla<-traitdata%>%
  filter(SLA>600)
bla <- bla%>%
  select(Site, Species, Individual.x, Wet_mass, Dry_mass, Leaf_area, SLA)

#There are som mistakes in here... Arh_Ant_odo_5 probably does not have a so big leaf area.. Ram_Hie_pil_1 burde kanskje fjernes da den er helt ødelagt..

ggplot(traitdata, aes(x = log(Dry_mass), y = log(Leaf_area))) +
  geom_point()

#Looking at the relationships between leaf area and dry mass. This looks ok, maybe some of the smaller leaves are a little bit strange, think about cutting out leaves at a higher threshold then 0.0002.


#### WEIGHTED MEANS ####

# Reading in and cleaning the community data so that it is ready to be used only for cover

community <-read.csv2("Traits/data/funcab_composition_2016.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)

community<-community %>%
  filter(Site!="")%>%
  mutate(Site= substr(Site, 1,3))%>%
  filter(Measure == "Cover")

community_cover<-community%>%
  select(-subPlot, -year, -date, -Measure, -recorder, -Nid.herb, -Nid.gram, -Nid.rosett, -Nid.seedling, -liver, -lichen, -litter, -soil, -rock, -X.Seedlings) %>%
  select(-TotalGraminoids, -totalForbs, -totalBryophytes, -vegetationHeight, -mossHeight, -comment, -ver.seedl, -canum, -totalVascular, -totalBryophytes.1, -acro, -pleuro, -totalLichen)%>%
  gather(species, cover, Ach.mil:Vis.vul)%>%
  mutate(cover = as.numeric(cover))%>%
  filter(!is.na(cover))%>%  #Takes out the species that is not present in the dataset
  mutate(species=gsub("\\.", "_", species))%>%
  mutate(Site = factor(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs")))


community_cover<-community_cover%>%
  group_by(Site, species)%>%
  mutate(mean_cover=mean(cover, na.rm=TRUE))
#If you want the turf data use mutate, if you want the site data use summarise


#### Making means of the trait data set ####

#Use this code when you need to have this for every individual
#traitdata_test<-traitdata%>%
  #group_by(Site, Species)%>%
  #mutate(SLA_mean=mean(SLA))%>%
  #mutate(Lth_mean=mean(Lth_ave))%>%
  #mutate(Height_mean=mean(Height))%>%
  #mutate(LDMC_mean=mean(LDMC))%>%
  #mutate(LA_mean=mean(Leaf_area))%>%
  #transform(Species = as.character(Species))


#This was used to calculate the means before I put it in the code further up
#traitdata_test <- traitdata %>%
  #group_by(Site, Species) %>%
  #summarise(
    #SLA_mean = mean(SLA, na.rm = TRUE),
    #Lth_mean = mean(Lth_ave, na.rm = TRUE),
    #Height_mean = mean(Height, na.rm = TRUE),
    #LDMC_mean = mean(LDMC, na.rm = TRUE),
    #LA_mean = mean(Leaf_area, na.rm = TRUE)
  #)

        
#### Joining the datasets and making that ready for analysis ####

wcommunity <- full_join(community_cover, traitdata, by=c( "Site"="Site", "species"="Species"))

dict_com <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
"old new
  Nar_stri Nar_str
  Tarax Tar_sp
  Euph_sp Eup_sp
  Phle_alp Phl_alp
  Rhin_min Rhi_min
  Rum_ac_la Rum_acl
  Trien_eur Tri_eur
  Rub_idae Rub_ida
  Saus_alp Sau_alp
  Ave__pub Ave_pub
  Car_atra Car_atr
  Hypo_rad Hyp_rad
  Bart_alp Bar_alp
  Car_pulic Car_pul
  Carex_sp Car_sp
  Hier_sp Hie_sp
  Salix_sp Sal_sp")


#### Weighting the traits data by the community ####


wcommunity_df <- wcommunity %>%
  group_by(turfID)%>%
  filter(!is.na(mean_cover)) %>%
  mutate(Wmean_LDMC= weighted.mean(LDMC_mean, cover, na.rm=TRUE),
            Wmean_Lth= weighted.mean(Lth_mean, cover, na.rm=TRUE),
            Wmean_LA= weighted.mean(LA_mean, cover, na.rm=TRUE),
            Wmean_SLA= weighted.mean(SLA_mean, cover, na.rm=TRUE),
            Wmean_Height= weighted.mean(Height_mean, cover, na.rm=TRUE),
            Wmean_CN = weighted.mean(CN_ratio_mean, cover, na.rm=TRUE))%>%
  mutate(P_level = recode(Site, Ulv = "1", Alr = "1", Fau = "1", Lav = "2", Hog = "2", Vik = "2", Gud = "3", Ram = "3", Arh = "3", Skj = "4", Ves = "4", Ovs = "4")) %>%
  mutate(T_level = recode(Site, Ulv = "Alpine", Lav = "Alpine",  Gud = "Alpine", Skj = "Alpine", Alr = "Sub-alpine", Hog = "Sub-alpine", Ram = "Sub-alpine", Ves = "Sub-alpine", Fau = "Boreal", Vik = "Boreal", Arh = "Boreal", Ovs = "Boreal"))%>%
  ungroup()
  
wcommunity_df <-wcommunity_df %>%  
mutate(species = plyr::mapvalues(species, from = dict_com$old, to = dict_com$new))



#### Tried a new way to make the P-level and T-level, but it was not working the second time around, but here is the code  

  #mutate(T_level = case_when(.$Site %in% c("Ulv", "Lav", "Gud", "Skj") ~ "Alpine",
                             #.$Site %in% c("Alr", "Hog", "Ram", "Ves") ~ "Intermediate",
                   #.$Site %in% c("Fau", "Vik", "Arh", "Ovs") ~ "Lowland")) %>%
  #mutate(P_level = case_when(.$Site %in% c("Ulv", "Alr", "Fau") ~ "1",
                   #.$Site %in% c("Lav", "Hog", "Vik") ~ "2",
                   #.$Site %in% c("Gud", "Ram", "Arh") ~ "3",
                   #.$Site %in% c("Skj", "Ves", "Ovs") ~ "4"))
