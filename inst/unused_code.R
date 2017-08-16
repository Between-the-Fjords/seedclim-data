##### UNUSED CODE #####
my.GR.data <- dbGetQuery(con, paste("SELECT sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, turfs.GRtreat, turfCommunity.Year, turfCommunity.species, turfCommunity.cover, sites.Temperature_level, sites.Precipitation_level
FROM taxon INNER JOIN ((sites INNER JOIN ((blocks INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.destinationPlotID) ON sites.siteID = blocks.siteID) INNER JOIN turfCommunity ON turfs.turfID = turfCommunity.turfID) ON taxon.species = turfCommunity.species
                                    GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, turfs.GRtreat, turfCommunity.Year, turfCommunity.species, turfCommunity.cover, sites.Temperature_level, sites.Precipitation_level
                                    HAVING turfCommunity.Year > 2009 AND (turfs.TTtreat = 'ttc' OR (turfs.GRtreat) = 'rtc' OR (turfs.GRtreat) = 'ttc');"))



# MARTA
marta.height = read.csv("Plant traits/MartaRawTraitData.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

marta.height <- marta.height %>%
  select(Site = SITE, Species = SPECIE, length = LENGTH, height = DISP.HEIGHT) %>%
  mutate(Site = plyr::mapvalues(Site, from = c("Ulv (Alp1)", "LAv (Alp2)", "Gud (Alp3)", "Skj (Alp4)", "Alr (Int1)", "Hog (Int2)", "Ram(Int3)", "Ves (Int4)", "Fau (Low1)", "Vik (Low2)", "Arh (Low3)", "Ovs (Low4)"), to = c("Ulvhaugen", "Lavisdalen", "Gudmedalen", "Skjellingahaugen", "Alrust", "Hogsete", "Rambera", "Veskre", "Fauske", "Vikesland", "Arhelleren", "Ovstedal"))) %>%
  filter(Species != "") %>%
  separate(Species, into = c("genus", "species")) %>%
  mutate(species = ifelse(is.na(species), "sp", species)) %>%
  mutate(genus = substr(genus, 1,3), species = substr(species, 1,3)) %>%
  unite(species, genus, species, sep = ".") %>%
  as.data.frame()

#check to see what the deal is with length and height...
marta.height %>% gather(heightGather, scale, length:height) %>% ggplot(aes(x = Site, y = scale, colour = heightGather)) +geom_boxplot()

marta.height <- marta.height %>%
  mutate(Site_sp = paste0(Site,"_", species)) %>%
  group_by(species) %>%
  mutate(marta.height_mean_global = mean(height, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Site, species) %>%
  mutate(marta.height_mean_local = mean(height, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(Site, species, .keep_all = TRUE) %>%
  select(- height)

#check to see how the global vs local traits compare
marta.height %>% gather(heightGather, scale, marta.height_mean_global:marta.height_mean_local) %>% ggplot(aes(x = Site, y = scale, colour = heightGather)) +geom_boxplot()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############### Hypervolume analysis ###############

library(psych)
library(broom)
library(hypervolume)

## ---- hypervolume start ---- 

# checking out what the deal is with the hypervolume package
forbs <- forbs[complete.cases(forbs[,20:24]),]
forbs <- forbs %>%
  filter(TTtreat == "RTC")

hv1 <- hypervolume(subset(forbs, Year == 2011 & temp == 6.5)[,14:17], bandwidth = estimate_bandwidth(forbs[,14:15]), name = '2011_alpine')
hv2 <- hypervolume(subset(forbs, Year == 2016 & temp == 6.5)[,14:17], bandwidth = estimate_bandwidth(forbs[,14:15]), name = '2016_alpine')
hv3 <- hypervolume(subset(forbs, Year == 2011 & temp == 10.5)[,14:17], bandwidth = estimate_bandwidth(forbs[,14:15]), name = '2011_lowland')
hv4 <- hypervolume(subset(forbs, Year == 2016 & temp == 10.5)[,14:17], bandwidth = estimate_bandwidth(forbs[,14:15]), name = '2016_lowland')

hv2 <- hypervolume(subset(forbs, specialism == "alpine")[,14:17], bandwidth = 0.25, name = 'alpines')
hv3 <- hypervolume(subset(forbs, specialism == "lowland")[,14:17], bandwidth = 0.25, name = 'lowlands')

hv_all <- hypervolume_join(hv1, hv2)
plot(hv_all)

species_list = as.character(unique(forbs$Year))
num_species = length(species_list)  
trait_axes <- c("SLA_mean","Height_mean","LDMC_mean","LA_mean")

# compute hypervolumes for each species  
hv_specialism_list = new("HypervolumeList")
hv_specialism_list@HVList = vector(mode="list",length=num_species)
for (i in 1:num_species)
{
  # keep the trait data 
  data_this_specialism = forbs[forbs$Year==species_list[i],trait_axes]
  # log-transform to rescale
  # data_this_species_log <- log10(data_this_species)
  
  # make a hypervolume using auto-bandwidth
  hv_specialism_list@HVList[[i]] <- hypervolume(data_this_specialism, bandwidth = estimate_bandwidth(data_this_specialism), name = as.character(species_list[i]), warn = FALSE)
}

# compute all pairwise overlaps
overlap = matrix(NA, nrow = num_species, ncol = num_species)
dimnames(overlap) = list(species_list, species_list)
for (i in 1:num_species)
{
  for (j in i:num_species)
  {
    if (i!=j)
    {
      # compute set operations on each pair
      this_set = hypervolume_set(hv_specialism_list@HVList[[i]], hv_specialism_list@HVList[[j]], check_memory = FALSE)
      # calculate a Sorensen overlap index (2 x shared volume / sum of |hv1| + |hv2|)
      overlap[i,j] = hypervolume_sorensen_overlap(this_set)
    }
  }   
}

# show all hypervolumes
plot(hv_specialism_list)

# show pairwise overlaps - note that actually very few species overlap in four dimensions
+   op <- par(mar=c(10,10,1,1))
+   image(x=1:nrow(overlap), y=1:nrow(overlap), z=overlap,axes=F,xlab='',ylab='',col=rainbow(5))
+   box()
+   axis(side=1, at=1:(length(dimnames(overlap)[[1]])),dimnames(overlap)[[1]],las=2,cex.axis=0.75)
+   axis(side=2, at=1:(length(dimnames(overlap)[[2]])),dimnames(overlap)[[2]],las=1,cex.axis=0.75)
+   par(op)


## ---- hypervolume end ---- 

############### DIVERSITY MEASURES AND TRAITS ###############
######################### leave this section for the moment ##################################

#Remove some plots so that 2011 values can be subtracted from the 2012 and 2013 data 
diversity.data.2011<-diversity.data[diversity.data$Year==2011,]
diversity.data.2011<-diversity.data.2011[-48,]
dim(diversity.data.2011)
diversity.data.2012<-diversity.data[diversity.data$Year==2012,]
diversity.data.2012<-diversity.data.2012[-c(70,94:95),]
dim(diversity.data.2012)
diversity.data.2013<-diversity.data[diversity.data$Year==2013,]
diversity.data.2013<-diversity.data.2013[-c(70,94:95),]
dim(diversity.data.2013)

#Check if plot ID matches
diversity.data.2012[,5] == diversity.data.2013[,5] #should be TRUE
diversity.data.2011[,5] == diversity.data.2012[,5] #here you'll get a couple of FALSEs due to different naming of the same plots... 

#Do the subtraction
rich.div.12<-diversity.data.2012[,12] - diversity.data.2011[,12]
rich.div.13<-diversity.data.2013[,12] - diversity.data.2011[,12]

#Put it all back together
diversity.data.2011<-cbind(diversity.data.2011,rich.div.12)
colnames(diversity.data.2011)[17]<-"rich.div"
diversity.data.2012<-cbind(diversity.data.2012,rich.div.13)
colnames(diversity.data.2012)[17]<-"rich.div"
diversity.data.new<-rbind(diversity.data.2011,diversity.data.2012) #NB! Note that the years now say 2011 and 2012 in stead of 2012 and 2013. It doesn't matter for the analyses, but it's good to be aware of



hp<-function(site,dat=freqsubturf,ord=nmds, ...){
  
  keep<-cover.meta$siteID==site
  
  TT<-cover.meta$TTtreat[keep]
  year<-cover.meta$Year[keep]
  blockID<-cover.meta$blockID[keep]
  siteID<-cover.meta$siteID[keep]
  
  
  dat<-dat[keep,]
  #browser()  
  #run ord
  mod<-ord(dat, ...)
  
  #rotate? so temperature on Y axis 
  #plot analysis   #expect missing values
  plot(mod, display="sites",type="n")
  
  plotsubset<-function(TTtreat, col=1, pch=20){
    points(mod, choices=1:2,display="sites",select=TT==TTtreat, col=col, pch=pch)
    sapply(unique(blockID), function(bID){
      k=TT==TTtreat&blockID==bID
      if(sum(k)>0){
        points(mod, choices=1:2,display="sites",select=k, col=col, type="l")
        points(mod, choices=1:2,display="sites",select=k&year==2012, col=col, type="p", pch=pch, cex=1.5)
      }
    })
  }
  
  plotsubset(TTtreat="TTC",col="grey70", pch=20 )
  plotsubset(TTtreat="RTC",col="red", pch=20 )
  #plotsubset(TTtreat="TT2",col="red" )
  #plotsubset(TTtreat="TT3",col="blue" )
  #plotsubset(TTtreat="TT4",col="purple" )
  
}


Q
par(mar=c(3,3,1,1), mgp=c(1.5,.5,0))
x11();
hp("Hogsete", dat=freqsubturf)

sapply(levels(cover.meta$siteID), function(siteID){
  x11();
  hp(siteID, dat=cover, ord=metaMDS)
  title(main=siteID)
})


hogsete <- cover.meta[cover.meta$siteID=="Hogsete",]
p <- ggplot(hogsete, aes(x=fyear, y=diversity))
p + geom_boxplot()



#Correcting small mistakes where spp have switched: head(cover)
#cover["Gud2RTC_2010","Sal.sp"]<-0
#cover["Gud2RTC_2010","Sal.her"]<-3

#cover["Lav3RTC_2010","Sal.sp"]<-0
#cover["Lav3RTC_2010","Sal.her"]<-1

#cover["Lav5RTC_2010","Sal.sp"]<-0
#cover["Lav5RTC_2010","Sal.her"]<-1

#cover["Ulv2RTC_2012","Sal.sp"]<-0
#cover["Ulv2RTC_2012","Sal.her"]<-2

#cover["222 TTC_2013","Hypo.mac"]<-0
#cover["222 TTC_2013","Hypo.rad"]<-3

############### SUBPLOT FREQUENCIES ###############

## ---- subturf.GR.import ----

subturf.GR <- dbGetQuery(con, paste("SELECT sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, turfs.GRtreat, subTurfCommunity.subTurf, subTurfCommunity.Year, subTurfCommunity.species, sites.Temperature_level, sites.Precipitation_level
                                    FROM taxon INNER JOIN ((sites INNER JOIN ((blocks INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) ON sites.siteID = blocks.siteID) INNER JOIN subTurfCommunity ON turfs.turfID = subTurfCommunity.turfID) ON taxon.species = subTurfCommunity.species
                                    WHERE Not taxon.functionalGroup='graminoid'
                                    GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, turfs.GRtreat, subTurfCommunity.subTurf, subTurfCommunity.Year, subTurfCommunity.species, sites.Temperature_level, sites.Precipitation_level
                                    HAVING subTurfCommunity.Year>2009 AND (turfs.TTtreat='ttc' OR (turfs.GRtreat)='rtc' OR (turfs.GRtreat)='ttc');"))

head(subturf.GR)

levels(subturf.GR$TTtreat) <- c(levels(subturf.GR$TTtreat),levels(subturf.GR$GRtreat))
subturf.GR$TTtreat[subturf.GR$TTtreat == ""| is.na(subturf.GR$TTtreat)] <- subturf.GR$GRtreat[subturf.GR$TTtreat == ""| is.na(subturf.GR$TTtreat)]
subturf.GR$GRtreat <- NULL

subturf.GR$TTtreat <- factor(subturf.GR$TTtreat)
subturf.GR <- subturf.GR[!(subturf.GR$blockID == "Gud5" & subturf.GR$Year == 2010),]
#subturf.GR <- subturf.GR[!subturf.GR$turfID=="Arh5RTC",]
#subturf.GR <- subturf.GR[!subturf.GR$turfID=="Ovs4RTC",]
#subturf.GR <- subturf.GR[!subturf.GR$turfID=="Ovs5RTC",]
#subturf.GR <- subturf.GR[!subturf.GR$species=="NID.seedling",]
#subturf.GR <- subturf.GR[!subturf.GR$species=="NID.herb",]

subturf.GR$Year <- factor(subturf.GR$Year)
subturf.GR$Year[subturf.GR$Year == 2010] <- 2011
subturf.GR$Year <- droplevels(subturf.GR$Year)
subturf.GR$turfID <- plyr::mapvalues(subturf.GR$turfID, from = "Ram4RTCx", to = "Ram4RTC")
subturf.GR$turfID <- plyr::mapvalues(subturf.GR$turfID, from = "Ram5RTCx", to = "Ram5RTC")

subturf.GR$ID <- as.factor(paste(subturf.GR$turfID, subturf.GR$Year, sep = "_"))
subturf.GR <- subturf.GR[!subturf.GR$blockID %in% remsites,] 


## ---- subturf.GR.end ----

#make fat table        
subturf <- xtabs(rep(1, nrow(subturf.GR)) ~ paste(turfID, subTurf, Year, sep = "_") + species, data = subturf.GR)
subturf <- as.data.frame(unclass(subturf))
head(subturf)

#Meta-data
subturf.meta <- unique(subturf.GR[,c("siteID", "TTtreat", "Year", "blockID", "turfID","subTurf","Temperature_level",  "Precipitation_level")])
subturf.meta <- subturf.meta[order(paste(subturf.meta$turfID, subturf.meta$subTurf, subturf.meta$Year, sep="_" )),] #make sure the order is right
subturf.meta[1:25,]

all(paste(subturf.meta$turfID, subturf.meta$subTurf, subturf.meta$Year, sep = "_") == rownames(subturf))
#if everything is correct, this should be TRUE!

#Calculate subplot frequencies
fsubturf <- by(subturf, paste(subturf.meta$turfID, subturf.meta$Year, sep = "_"), colSums)
fsubturf <- t(sapply(fsubturf,I))
fsubturf <- as.data.frame(fsubturf)
fsubturf[1:25,1:5]
dim(fsubturf)

freqsubturf <- fsubturf/25

#Fixing some records... 
freqsubturf["Fau1RTC_2010","Vio.pal"]<-0
freqsubturf["Fau1RTC_2010","Vio.tri"]<-0.04

freqsubturf["Fau1RTC_2015","Pla.lan"]<-0.08
freqsubturf["Fau1RTC_2015","Pla.med"]<-0

freqsubturf["Fau2RTC_2013","Pla.lan"]<-0
freqsubturf["Fau2RTC_2013","Pla.med"]<-0.32

freqsubturf["Fau3RTC_2010","Hie.vul"]<-0
freqsubturf["Fau3RTC_2010","Hie.pil"]<-0.04

freqsubturf["Fau5RTC_2012","Hypo.rad"]<-0
freqsubturf["Fau5RTC_2012","Hypo.mac"]<-0.08

freqsubturf["260 TTC_2013","Pyr.sp"]<-0
freqsubturf["260 TTC_2013","Pyr.min"]<-0.08

#freqsubturf["506 TTC_2010","Pyr.sp"]<-0
#freqsubturf["506 TTC_2010","Pyr.min"]<-0.12

freqsubturf["Gud2RTC_2010","Pyr.sp"]<-0
freqsubturf["Gud2RTC_2010","Pyr.min"]<-0.16

freqsubturf["Lav3RTC_2010","Pyr.sp"]<-0
freqsubturf["Lav3RTC_2010","Pyr.min"]<-0.04

freqsubturf["Lav5RTC_2010","Pyr.sp"]<-0
freqsubturf["Lav5RTC_2010","Pyr.min"]<-0.04

freqsubturf["528 TTC_2013","Pyr.rot"]<-0
freqsubturf["528 TTC_2013","Pyr.min"]<-0.04

#freqsubturf["222 TTC_2013","Geu.riv"]<-0
#freqsubturf["222 TTC_2013","Geu.urb"]<-0.08


#identical(cover.meta$turfID, rownames(freqsubturf))

all(paste(cover.meta$turfID, cover.meta$Year, sep = "_") == rownames(freqsubturf))
#if everything is correct, this should be TRUE!


#cover.meta$unique.block<-paste(cover.meta$Year, cover.meta$blockID)



#This was used to see which of the leaves in the leaf area dataset was not present in the traits dataset
#traitdata %>%
#filter(is.na(Location))%>%
#select(ID, Leaf_area)


# Finding errors
#traitdata %>%
#filter(Dry_mass > 8.5)%>%
#filter(is.na(Site))%>% 
#filter(Dry_mass > 8.5) %>%
#filter(Lth_ave > 60) %>%
#filter(traitdata, SLA>750)


#### WEIGHTED MEANS ####

# Reading in and cleaning the community data so that it is ready to be used only for cover

community <-read.csv2("ragnhild_trait_data/funcab_composition_2016.csv", header=TRUE, sep=";", stringsAsFactors = FALSE)

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


traitdata_test <- traitdata %>%
  group_by(Site, Species) %>%
  summarise(
    SLA_mean = mean(SLA, na.rm = TRUE),
    Lth_mean = mean(Lth_ave, na.rm = TRUE),
    Height_mean = mean(Height, na.rm = TRUE),
    LDMC_mean = mean(LDMC, na.rm = TRUE),
    LA_mean = mean(Leaf_area, na.rm = TRUE)
  )


#### Joining the datasets and making that ready for analysis ####

wcommunity <- full_join(community_cover, traitdata_test, by=c( "Site"="Site", "species"="Species"))

#Making a dictonary to translate the names of the species when they are different in one dataset compared to the other one

#k<-wcommunity_df %>% filter(is.na(mean_cover)) %>% ungroup() %>%select(species) %>% count(species) %>% arrange(desc(n))
#This will make a list of all the names we have in the traitsdataset and is not in the community dataset

#l<-wcommunity %>% filter(is.na(SLA_mean)) %>% ungroup() %>%select(species) %>% count(species) %>% arrange(desc(n))
#This will make a list of all the names we have in the community dataset which doesn't have a values for SLA (which could be because the names are different, or because we don't have a value for SLA)

dict <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
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

dict<-dict%>%
  as.character(dict$old)%>%
  as.character(dict$new)


#### Weighting the traits data by the community ####


wcommunity_df <- wcommunity %>%
  group_by(turfID)%>%
  filter(!is.na(mean_cover)) %>%
  mutate(Wmean_LDMC= weighted.mean(LDMC_mean, cover, na.rm=TRUE),
         Wmean_Lth= weighted.mean(Lth_mean, cover, na.rm=TRUE),
         Wmean_LA= weighted.mean(LA_mean, cover, na.rm=TRUE),
         Wmean_SLA= weighted.mean(SLA_mean, cover, na.rm=TRUE),
         Wmean_Height= weighted.mean(Height_mean, cover, na.rm=TRUE))%>%
  mutate(P_level = recode(Site, Ulv = "1", Alr = "1", Fau = "1", Lav = "2", Hog = "2", Vik = "2", Gud = "3", Ram = "3", Arh = "3", Skj = "4", Ves = "4", Ovs = "4")) %>%
  mutate(T_level = recode(Site, Ulv = "Alpine", Lav = "Alpine",  Gud = "Alpine", Skj = "Alpine", Alr = "Sub-alpine", Hog = "Sub-alpine", Ram = "Sub-alpine", Ves = "Sub-alpine", Fau = "Boreal", Vik = "Boreal", Arh = "Boreal", Ovs = "Boreal"))%>%
  mutate(species = plyr::mapvalues(species, from = dict$old, to = dict$new))



#### Tried a new way to make the P-level and T-level, but it was not working the second time around, but here is the code  

#mutate(T_level = case_when(.$Site %in% c("Ulv", "Lav", "Gud", "Skj") ~ "Alpine",
#.$Site %in% c("Alr", "Hog", "Ram", "Ves") ~ "Intermediate",
#.$Site %in% c("Fau", "Vik", "Arh", "Ovs") ~ "Lowland")) %>%
#mutate(P_level = case_when(.$Site %in% c("Ulv", "Alr", "Fau") ~ "1",
#.$Site %in% c("Lav", "Hog", "Vik") ~ "2",
#.$Site %in% c("Gud", "Ram", "Arh") ~ "3",
#.$Site %in% c("Skj", "Ves", "Ovs") ~ "4"))
