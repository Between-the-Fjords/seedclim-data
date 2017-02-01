##############################################################################
#Script for paper on effect of graminoid removal on plant community properties
##############################################################################

library(dplyr)
library(DBI)
library(tidyr)
con <- dbConnect(RMySQL::MySQL(), group = "seedclim")

############### Cover data ###############

## ---- my.GR.data.import ----

my.GR.data <- dbGetQuery(con, paste("SELECT sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, turfs.GRtreat, turfCommunity.Year, turfCommunity.species, turfCommunity.cover, sites.Temperature_level, sites.Precipitation_level
FROM taxon INNER JOIN ((sites INNER JOIN ((blocks INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.destinationPlotID) ON sites.siteID = blocks.siteID) INNER JOIN turfCommunity ON turfs.turfID = turfCommunity.turfID) ON taxon.species = turfCommunity.species
GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, turfs.GRtreat, turfCommunity.Year, turfCommunity.species, turfCommunity.cover, sites.Temperature_level, sites.Precipitation_level
HAVING turfCommunity.Year>2009 AND (turfs.TTtreat='ttc' OR (turfs.GRtreat)='rtc' OR (turfs.GRtreat)='ttc');"))


str(my.GR.data)
head(my.GR.data)

levels(my.GR.data$TTtreat) <- c(levels(my.GR.data$TTtreat),levels(my.GR.data$GRtreat))
my.GR.data$TTtreat[my.GR.data$TTtreat == ""| is.na(my.GR.data$TTtreat)] <- my.GR.data$GRtreat[my.GR.data$TTtreat == ""| is.na(my.GR.data$TTtreat)]
my.GR.data$GRtreat <- NULL
my.GR.data$TTtreat <- factor(my.GR.data$TTtreat)
my.GR.data <- my.GR.data[!(my.GR.data$blockID == "Gud5" & my.GR.data$Year == 2010), ]
my.GR.data$Year[my.GR.data$Year == 2010] <- 2011


remsites <- c("Skj11", "Skj12", "Gud11", "Gud12", "Gud13")

#Remove TTCs not included in the data set 
my.GR.data <- my.GR.data[!my.GR.data$blockID %in% remsites,] 

my.GR.data$ID <- as.factor(paste(my.GR.data$turfID, my.GR.data$Year, sep = "_"))

## ---- my.GR.data.end ----

############### TRAITS ###############

#make fat table
cover <- xtabs(cover ~ paste(turfID, Year, sep = "_") + species, data = my.GR.data)
cover <- as.data.frame(unclass(cover))
cover <- cover[,colSums(cover > 0) > 0] #remove empty spp
head(cover)

#load from data base
traits <- dbGetQuery(con, paste("SELECT taxon.* , Lower, Nem, BNem, SBor, MBor, NBor, LAlp, MAlp, HAlp, Upper
                                FROM taxon LEFT JOIN moreTraits ON taxon.species = moreTraits.species
                                ORDER BY taxon.species;"))

traits <- traits[traits$species %in% names(cover),]
traits$HAlp <- as.numeric(traits$HAlp)
traits[,(12:19)][is.na(traits[,12:19])] <- 0
traits <- traits %>% 
  rowwise() %>% 
  mutate(abundance = sum(Nem, BNem, SBor, MBor, NBor, LAlp, MAlp, HAlp, na.rm = TRUE)) %>%
  mutate(specialism = factor(ifelse(
    Nem == 0 & BNem == 0 & SBor == 0 & LAlp == 1, "alpine", 
    ifelse(HAlp == 0 & MAlp == 0 & LAlp == 0 & Nem == 1, "lowland", 
           ifelse(abundance < 5.5, "generalist", "other")))))

str(traits)

identical(as.character(traits$species), names(cover)) #this should be identical

my.GR.data$specialism <- traits$specialism[match(my.GR.data$species, traits$species)]
my.GR.data$functionalgroup <- as.factor(traits$functionalGroup[match(my.GR.data$species, traits$species)])
my.GR.data$SLA <- traits$SLA[match(my.GR.data$species, traits$species)]
my.GR.data$Height <- traits$height[match(my.GR.data$species, traits$species)]
my.GR.data$leafSize <- traits$leafSize[match(my.GR.data$species, traits$species)]
my.GR.data$seedMass <- traits$seedMass[match(my.GR.data$species, traits$species)]

my.GR.data$prec <- c(0.6,1.2,2.0,2.7)[my.GR.data$Precipitation_level]
my.GR.data$temp <- c(6.5,8.5,10.5)[my.GR.data$Temperature_level]

my.GR.data$TTtreat <- factor(as.character(my.GR.data$TTtreat), levels = c("TTC", "RTC"))


############### DIVERSITY MEASURES ###############

#Species richness
library(vegan)

diversity.freq <- rowSums(cover > 0)

#Shannon's diversity index
diversity.freq <- data.frame(richness = diversity.freq, diversity = diversity(cover, index = "shannon")) 
diversity.freq$ID <- rnames

#Species evenness
diversity.freq <- cbind(diversity.freq, evenness = diversity.freq$diversity/log(diversity.freq$richness), expdiversity = exp(diversity.freq$diversity))
my.GR.data$evenness <- diversity.freq$evenness[match(my.GR.data$ID, diversity.freq$ID)]
my.GR.data$richness <- diversity.freq$richness[match(my.GR.data$ID, diversity.freq$ID)]
my.GR.data$diversity <- diversity.freq$diversity[match(my.GR.data$ID, diversity.freq$ID)]
my.GR.data$expdiversity <- diversity.freq$expdiversity[match(my.GR.data$ID, diversity.freq$ID)]

head(my.GR.data)


############## WEIGHTED MEANS ###############

###### weighted means for whole community
wholecom <- my.GR.data %>% 
  group_by(ID) %>%
  mutate(wmean_height = weighted.mean(Height, cover), wmean_leafSize = weighted.mean(leafSize, cover), wmean_seedMass = weighted.mean(seedMass, cover), wmean_SLA = weighted.mean(SLA, cover)) %>%
  distinct(ID, .keep_all = TRUE) %>%
  select(-(Temperature_level:seedMass), -(fwmean_height:swmean_SLA), -species) %>%
  as.data.frame()

###### weighted means for functional groups
functionals <- my.GR.data %>% 
  group_by(ID, functionalgroup) %>%
  mutate(fwmean_height = weighted.mean(Height, cover), fwmean_leafSize = weighted.mean(leafSize, cover), fwmean_seedMass = weighted.mean(seedMass, cover), fwmean_SLA = weighted.mean(SLA, cover)) %>%
  distinct(ID, functionalgroup, .keep_all = TRUE) %>%
  select(-(SLA:wmean_SLA), -(swmean_height:swmean_SLA), -(Temperature_level:specialism), -species) %>%
  as.data.frame()

###### weighted means by specialism
specialism <- my.GR.data %>%
  group_by(ID, specialism) %>%
  mutate(swmean_height = weighted.mean(Height, cover), swmean_leafSize = weighted.mean(leafSize, cover), swmean_seedMass = weighted.mean(seedMass, cover), swmean_SLA = weighted.mean(SLA, cover)) %>%
  distinct(ID, functionalgroup, .keep_all = TRUE) %>%
  select(-(SLA:wmean_SLA), -(swmean_height:swmean_SLA), -(Temperature_level:specialism), -species) %>%
  as.data.frame()


################## CALCULATING TRAITS DELTA ###################
my.GR.data$sumcover <- rowSums(cover)

deltacalc <- sapply(1:nrow(cover.meta[cover.meta$TTtreat == "RTC",]), function(i){
  R <- cover.meta[cover.meta$TTtreat == "RTC",][i,]
  #browser()
  cols <- c("richness","evenness","expdiversity","diversity","Height","SLA","leafSize","sumcover", "seedMass")
  friend <- cover.meta$Year==R$Year & cover.meta$blockID == R$blockID & cover.meta$TTtreat == "TTC"
  if(all (!friend)) {print(R$turfID)
    return(rep(NA, length(cols)))}
  stopifnot(sum(friend) == 1)
  
  f <- cover.meta[friend,]
  x <- R[,cols] - f[,cols]
  unlist(x)
})
deltacalc <- as.data.frame(t(deltacalc))
colnames(deltacalc) <- paste0("delta", colnames(deltacalc))
rtcmeta <- cbind((cover.meta[cover.meta$TTtreat == "RTC",]), deltacalc)
#rtcmeta<-rtcmeta[rtcmeta$Year!=2011,]

################## CALCULATING TIME DELTA ###################

timedeltacalc <- sapply(1:nrow(cover.meta[cover.meta$Year != 2011,]), function(i){
  R <- cover.meta[cover.meta$Year != 2011,][i,]
  #browser()
  cols <- c("richness", "evenness", "diversity", "Height", "SLA", "leafSize", "sumcover", "seedMass")
  friend <- cover.meta$turfID == R$turfID & cover.meta$Year == 2011
  if(all (!friend)) {print(R$turfID)
    return(rep(NA, length(cols)))}
  stopifnot(sum(friend) == 1)
  
  f <- cover.meta[friend,]
  x <- R[,cols] - f[,cols]
  unlist(x)
})
timedeltacalc <- as.data.frame(t(timedeltacalc))
colnames(timedeltacalc) <- paste0("delta", colnames(timedeltacalc))
timedelta <- cbind((cover.meta[cover.meta$Year != 2011,]), timedeltacalc)

################## SEEDLING DATA #######################

#seedlingGR <- read.csv("Query4.csv", header = TRUE, sep = ";")

#seedlingGR<- "SELECT sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, turfs.GRtreat, subTurfCommunity.subTurf, subTurfCommunity.Year, subTurfCommunity.species, subTurfCommunity.seedlings, subTurfCommunity.juvenile, subTurfCommunity.fertile, subTurfCommunity.dominant, sites.Temperature_level, sites.Precipitation_level
#FROM (sites INNER JOIN ((blocks INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) ON sites.siteID = blocks.siteID) INNER JOIN subTurfCommunity ON turfs.turfID = subTurfCommunity.turfID
#GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, turfs.GRtreat, subTurfCommunity.subTurf, subTurfCommunity.Year, subTurfCommunity.species, subTurfCommunity.seedlings, subTurfCommunity.juvenile, subTurfCommunity.fertile, subTurfCommunity.dominant, sites.Temperature_level, sites.Precipitation_level
#HAVING subTurfCommunity.Year=2013 AND (turfs.TTtreat='ttc' OR (turfs.GRtreat)='rtc' OR (turfs.GRtreat)='ttc');"

#seedlings
#seedlingGR<-sqlQuery(db,seedlingGR)
#head(seedlingGR)

#levels(seedlingGR$TTtreat)<-c(levels(seedlingGR$TTtreat),levels(seedlingGR$GRtreat))
#seedlingGR$TTtreat[seedlingGR$TTtreat==""|is.na(seedlingGR$TTtreat)]<-seedlingGR$GRtreat[seedlingGR$TTtreat==""|is.na(seedlingGR$TTtreat)]
#seedlingGR$GRtreat<-NULL
#seedlingGR$TTtreat<-factor(seedlingGR$TTtreat)
#seedlingGR<-seedlingGR[!seedlingGR$blockID%in%remsites,]

#fat table      
#seedlings<-xtabs(seedlings~paste(turfID, Year, sep="_")+species, data=seedlingGR)
#seedlings<-as.data.frame(unclass(seedlings))
#head(seedlings)
#seedlings.meta<-cover.meta[cover.meta$Year==2013,]
#seedlings<-seedlings[-c(47,50,51,54,55),] #remove some TTCs not included in this experiment

#all(paste(seedlings.meta$turfID, seedlings.meta$Year, sep="_")==rownames(seedlings)) #If everything's correct, this should be TRUE

# Juveniles
#Fat table
#juveniles<-xtabs(juvenile~paste(turfID, Year, sep="_")+species, data=seedlingGR)
#juveniles<-as.data.frame(unclass(juveniles))
#head(juveniles)
#juveniles.meta<-cover.meta[cover.meta$Year==2013,]
#all(paste(seedlings.meta$turfID, seedlings.meta$Year, sep="_")==rownames(juveniles))
#Should be TRUE

#Calculating number of seedlings
#no.seedlings<-matrix(nrow=nrow(seedlings),ncol=1)
#for(i in 1:nrow(seedlings)){
#  no.seedlings[i]<-sum(seedlings[i,])
  
#}

#Calculating number of juveniles 
#no.juveniles<-matrix(nrow=nrow(juveniles),ncol=1)
#for(i in 1:nrow(juveniles)){
#  no.juveniles[i]<-sum(juveniles[i,])
  
#}

#Combining the two
#recruitment<-data.frame(seedlings=no.seedlings,juveniles=no.juveniles)
#recruitment.data<-cbind(seedlings.meta, recruitment)
#head(recruitment.data)


#dat3$s.size.cat <- as.numeric(cut(dat3$sample.size, c(0,5,10,20,40,52))) # divide sample size into categorise
# define colours; alpha value makes the colour transparent
#sig <- rgb(red=0, green=0, blue=0, alpha=170, max=255)
#non.sig <- rgb(red=140, green=140, blue=140, alpha=100, max=255)
# different symbols for different p values
#dat3$pch.nr <- ifelse(dat3$p.slope<0.05, 16, ifelse(dat3$p.slope==5,8,16))
#dat3$col.nr <- ifelse(dat3$p.slope<0.05, sig, ifelse(dat3$p.slope==5,1,non.sig))

# legend
#legend(1,1, c("0 - 5", "5 - 10", "11 - 20", "20 - 40", "> 40"), pch=1, pt.cex=c(0.8,1.6,2.4,3.2,4), bty="n")
#legend(2,1, c("P > 0.05", "P < 0.05", "NA"), pch=c(16,16,8), col=c(non.sig, sig,1), bty="n")

# plot
#plot(slopes ~ jitter(as.numeric(breed), 0.6), dat3, pch = dat3$pch.nr, col = dat3$col.nr, axes=FALSE, ylab="",xlab="", cex=dat3$s.size.cat*0.8)
#box(); axis(1, at=1:nlevels(dat3$breed), labels=levels(dat3$breed)); axis(2,labels=TRUE)




##### UNUSED CODE #####


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

#subturf.GR$Year[subturf.GR$Year==2010] <- 2011

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

