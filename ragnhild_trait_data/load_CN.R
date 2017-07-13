CN <-read.csv2("ragnhild_trait_data/CN_Ragnhild.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)
dict_CN <- read.csv2("Ragnhild_trait_data/Dict_CN.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)
CN$Info <- as.character(CN$Info)


CN <- CN %>%
  filter(Info =="")%>%
  mutate(siteID = substr(Name, 1,2), species = as.factor(substr(Name, 3,6)), Individual = substr(Name, 7,8), CN.ratio = as.numeric(CN.ratio)) %>%
  filter(species != "etan") %>%
  mutate(species = plyr::mapvalues(species, from = dict_CN$CN_ab, to = dict_CN$Species)) %>%
  mutate(siteID = plyr::mapvalues(siteID, from = c("UL", "LA", "GU", "SK", "AL", "HO", "RA", "VE", "FA", "VI", "AR", "OV"), to = c("Ulvhaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen", "Alrust", "Hogsete", "Rambera", "Veskre", "Fauske", "Vikesland", "Arhelleren", "Ovstedal"))) %>%
  mutate(ID = paste0(siteID, "_", species, "_", Individual, ".jpg")) %>%
  group_by(species) %>%
  as.data.frame()





#### Merge the trait data and the CN data ####

traitdata <- traitdata %>%
  full_join(CN, by=c("ID"="ID"))

traitdata<-traitdata%>%
  select(-Humidity.., -Name, -Weight, -Method, -N.Factor, -C.Factor,
         -N.Blank, -C.Blank, -Memo, -Info, -Date..Time, -Site.y, -Species.y) %>%
  rename(Site = Site.x, Species = Species.x)%>%
  group_by(Site, Species) %>%
  mutate(CN_ratio_mean = mean(CN.ratio, na.rm = TRUE))%>%
  ungroup()