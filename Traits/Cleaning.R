#### Libraries ####
library("tidyverse")
library("lubridate")
#library("mosaic")

#### Load trait data ####

traits <-read.csv("Traits/data/leaftraits2016csv.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE)


#### Cleaning the trait data ####

traits <- traits %>%
  rename(Height=Height..mm., Lth_1=Lth.1..mm., Lth_2= Lth.2..mm., Lth_3= Lth.3..mm., Wet_mass=Wet.mass..g., Dry_mass=Dry.mass..g., Site=Location) %>%
  select(-Lth.average..mm.)%>%
  mutate(Date = mdy(Date))%>%
  mutate(T_level = recode(Site, Ulv = 6.5, Lav = 6.5,  Gud = 6.5, Skj = 6.5, Alr = 8.5, Hog = 8.5, Ram = 8.5, Ves = 8.5, Fau = 10.5, Vik = 10.5, Arh = 10.5, Ovs = 10.5)) %>%
  mutate(Temp = recode(Site, Ulv=6.17, Lav=6.45, Gud=5.87, Skj=6.58, Alr=9.14, Hog=9.17, Ram=8.77, Ves=8.67, Fau=10.3, Vik=10.55, Arh=10.60, Ovs=10.78))%>%
  mutate(Precip= recode(Site, Ulv=596, Lav=1321, Gud=1925, Skj=2725, Alr=789, Hog=1356, Ram=1848, Ves=3029, Fau=600, Vik=1161, Arh=2044, Ovs=2923))%>%
  mutate(P_level = recode(Site, Ulv = 600, Alr = 600, Fau = 600, Lav = 1200, Hog = 1200, Vik = 1200, Gud = 2000, Ram = 2000, Arh = 2000, Skj = 2700, Ves = 2700, Ovs = 2700)) %>%
  mutate(LDMC=Dry_mass/Wet_mass)%>%
  mutate(Site = factor(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs"))) %>%
  mutate(Lth_ave=rowMeans(select(traits, matches("^Lth\\.\\d")), na.rm = TRUE)) %>%
  mutate(Dry_mass = replace(Dry_mass, Dry_mass < 0.0005, NA))%>%
  filter(!LDMC>1)

#### Load leaf area data ####

LA <- read.csv2("Traits/data/Leaf area.csv", stringsAsFactors = FALSE)

LA<-transform(LA, Leaf_area = as.numeric(Leaf_area))

LA <- LA %>%
  filter(Leaf_area > 0.1)


#### Merge the trait data and the leaf area data and make the means ####

traitdata <- traits %>%
  mutate(ID = paste0(Site, "_", Species, "_", Individual, ".jpg")) %>%
  filter(!(ID=="Ves_Leo_aut_6.jpg"))%>%
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


#### Load CN data ####

CN <- read.csv2("Traits/data/CNratio.csv", dec=".", sep=";")

#Making a dictionary for the CN name abreviations

dict_CN <- read.csv2("Traits/data/Dict_CN.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)

#Making a dictionary for the site names in the CN file

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
  full_join(CN, by=c("ID"="ID"))%>%
  select(-Humidity.., -Name, -Weight, -Method, -N.Factor, -C.Factor, -N.Blank, -C.Blank, -Memo, -Info, -Date..Time, -Site.y, -Species.y, -Individual.y, -N.., -C.., -N.Area, -C.Area) %>%
  rename(Site = Site.x, Species = Species.x, Individual=Individual.x)%>%
  group_by(Species) %>%
  mutate(CN_ratio_mean_global = mean(CN.ratio, na.rm = TRUE))%>%
  ungroup()%>%
  group_by(Site, Species)%>%
  mutate(CN_ratio_mean = mean(CN.ratio, na.rm= TRUE))%>%
  ungroup%>%
  filter(!(Species=="Hyp_mac" & Site=="Alr"))%>%
  filter(!(Species=="Agr_cap" & Site =="Alr" & Individual=="9"))%>%
  filter(!(Species=="Car_vag" & Site == "Ves"))%>%
  filter(!(Species=="Fes_rub" & Site == "Ulv"))%>%
  filter(!(Species=="Fes_rub" & Site == "Gud"))%>%
  filter(!(Species=="Hie_pil" & Site == "Gud"))%>%
  filter(!(Species=="Pot_cra" & Site == "Gud"))%>%
  filter(!(Species=="Ran_acr" & Site == "Skj"))%>%
  filter(!(Species=="Sax_aiz"))%>%
  filter(!(Species=="Hie_pil" & Site == "Gud"))%>%
  filter(!(Species=="Vac_myr" & Site == "Ves"))%>%
  filter(!(Species=="Ver_alp" & Site == "Ves"))


#### Add info about species ####


systematics_species<- read.csv2("Traits/data/systematics_species.csv", sep=";", stringsAsFactors = FALSE)

species_info<- read.csv2("Traits/data/species_info.csv", sep=";", stringsAsFactors = FALSE)

species_info <- species_info%>%
  select(species, functionalGroup, lifeSpan, occurrence)%>%
  mutate(species=gsub("\\.", "_", species))
  


traitdata <- traitdata %>%
  left_join(systematics_species, by = c("Species"="Species"))

traitdata <- traitdata %>%
  left_join(species_info, by =c("Species" = "species"))


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
  mutate(Site = as.character(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs")))

#Adding a dictionary to make the names in the traits dataset and the community datafram match
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
                       Salix_sp Sal_sp
                       Vio_can Vio_riv")



community_cover<-community_cover%>%
  group_by(Site, species)%>%
  mutate(mean_cover=mean(cover, na.rm=TRUE))%>% #If you want the turf data use mutate, if you want the site data use summarise
  ungroup()%>%
  mutate(species = plyr::mapvalues(species, from = dict_com$old, to = dict_com$new))%>%
  mutate(Site = factor(Site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs")))

        
#### Joining the datasets and making that ready for analysis ####

wcommunity <- left_join(traitdata, community_cover, by=c( "Site"="Site", "Species"="species"))

#### Weighting the traits data by the community ####

# If I just want the averages I must use summerise and not mutate


wcommunity_df <- wcommunity %>%
  group_by(turfID, Site)%>%
  filter(!is.na(mean_cover)) %>%
  mutate(Wmean_LDMC= weighted.mean(LDMC_mean, cover, na.rm=TRUE),
            Wmean_Lth= weighted.mean(Lth_mean, cover, na.rm=TRUE),
            Wmean_LA= weighted.mean(LA_mean, cover, na.rm=TRUE),
            Wmean_SLA= weighted.mean(SLA_mean, cover, na.rm=TRUE),
            Wmean_Height= weighted.mean(Height_mean, cover, na.rm=TRUE),
            Wmean_CN = weighted.mean(CN_ratio_mean, cover, na.rm=TRUE))%>%
  mutate(Wmean_global_LDMC= weighted.mean(LDMC_mean_global, cover, na.rm=TRUE),
         Wmean_global_Lth= weighted.mean(Lth_mean_global, cover, na.rm=TRUE),
         Wmean_global_LA= weighted.mean(LA_mean_global, cover, na.rm=TRUE),
         Wmean_global_SLA= weighted.mean(SLA_mean_global, cover, na.rm=TRUE),
         Wmean_global_Height= weighted.mean(Height_mean_global, cover, na.rm=TRUE),
         Wmean_global_CN = weighted.mean(CN_ratio_mean_global, cover, na.rm=TRUE))%>%
  ungroup()%>%
  select(Site, Species, T_level, P_level, Temp, Precip, SLA, LDMC, Lth_ave, Leaf_area, Height, CN.ratio, SLA_mean, LDMC_mean, Lth_mean, LA_mean, Height_mean, CN_ratio_mean, Genus, Family, Order, LDMC_mean_global, Lth_mean_global, SLA_mean_global, LA_mean_global, CN_ratio_mean_global, Height_mean_global, Wmean_LDMC, Wmean_Lth, Wmean_LA, Wmean_SLA, Wmean_Height, Wmean_CN, Wmean_global_CN, Wmean_global_Height, Wmean_global_SLA, Wmean_global_LA, Wmean_global_Lth, Wmean_global_LDMC, occurrence, functionalGroup)


#Used to make the dataset to feed into the CSR excel sheet

#CSR_df<-traitdata%>%
#  filter(!LDMC>1)%>%
#  select(Species, Family, Leaf_area, Wet_mass, Dry_mass)%>%
#  mutate(Leaf_area=Leaf_area*100)%>%
#  mutate(Wet_mass=Wet_mass*1000)%>%
#  mutate(Dry_mass=Dry_mass*1000)%>%
#  group_by(Species)%>%
#  summarise(Leaf_area = mean(Leaf_area, na.rm = TRUE),
#            Wet_mass = mean(Wet_mass, na.rm = TRUE),
#            Dry_mass = mean(Dry_mass, na.rm = TRUE))
  





#### Finding errors ####


#### Checking that I have 85% of the community ####

check_community_df <- wcommunity_df %>%
  group_by(Site, Species, turfID)%>%
  select(Site, turfID, species, cover, SLA_mean, Lth_mean, Height_mean, LDMC_mean, LA_mean, CN_ratio_mean)%>%
  unique()%>%
  group_by(Site, turfID)%>%
  mutate(cover_100 = (cover/(sum(cover)))*100)%>%
  filter(!is.na(SLA_mean))

uncomplete_turf <- check_community_df%>%
  #group_by(Site, turfID)%>%
  mutate(sumcover= sum(cover_100))%>%
  filter(sumcover<80) %>% distinct(turfID)

#### Checking which species I need to do ####

NA_community <- wcommunity_df %>%
  group_by(Site, species, turfID)%>%
  select(Site, turfID, species, cover, SLA_mean, Lth_mean, Height_mean, LDMC_mean, LA_mean, CN_ratio_mean)%>%
  unique()%>%
  group_by(Site, turfID)%>%
  mutate(cover_100 = (cover/(sum(cover)))*100)%>%
  filter(is.na(SLA_mean))%>%
  semi_join( uncomplete_turf, by="turfID")%>%
  ungroup()%>%
  select(-SLA_mean, -Lth_mean, -Height_mean, -LDMC_mean, -LA_mean, -CN_ratio_mean)%>%
  group_by(Site, turfID)%>%
  mutate(sumcover=sum(cover_100))


#### Tried a new way to make the P-level and T-level, but it was not working the second time around, but here is the code  

  #mutate(T_level = case_when(.$Site %in% c("Ulv", "Lav", "Gud", "Skj") ~ "Alpine",
                             #.$Site %in% c("Alr", "Hog", "Ram", "Ves") ~ "Intermediate",
                   #.$Site %in% c("Fau", "Vik", "Arh", "Ovs") ~ "Lowland")) %>%
  #mutate(P_level = case_when(.$Site %in% c("Ulv", "Alr", "Fau") ~ "1",
                   #.$Site %in% c("Lav", "Hog", "Vik") ~ "2",
                   #.$Site %in% c("Gud", "Ram", "Arh") ~ "3",
                   #.$Site %in% c("Skj", "Ves", "Ovs") ~ "4"))


#### LDMCS mistakes ####

LDMC_mistakes<- traitdata%>%
  group_by(Individual)%>%
  filter(Dry_mass>Wet_mass)

ggplot(traitdata, aes(x = log(Wet_mass), y = log(Dry_mass))) +
  geom_point()+
  geom_abline(data = Wet_mass/Dry_mass)

# I am not sure that I trust all of these measurements as they are super large. The once who actually are succulents are okay, or rolled leaves. But there might just be some of the leaf thickness measurement people who didn't do it correctly
Succulents<-traitdata%>%
  filter(Lth_ave>0.5)


bla<-traitdata%>%
  filter(SLA>500)%>%
  select(Site, Species, Individual, Wet_mass, Dry_mass, Leaf_area, SLA)

#There are som mistakes in here... Arh_Ant_odo_5 probably does not have a so big leaf area.. Ram_Hie_pil_1 burde kanskje fjernes da den er helt ødelagt..

ggplot(traitdata, aes(x = log(Dry_mass), y = log(Leaf_area))) +
  geom_point()

#Looking at the relationships between leaf area and dry mass. This looks ok, maybe some of the smaller leaves are a little bit strange, think about cutting out leaves at a higher threshold then 0.0002.
