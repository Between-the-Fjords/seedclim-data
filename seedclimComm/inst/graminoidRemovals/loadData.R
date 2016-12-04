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
#graminoids <- my.GR.data[my.GR.data$functionalGroup == "graminoid",]
#my.GR.data <- my.GR.data[!(my.GR.data$functionalGroup == "graminoid"),]
my.GR.data$Year[my.GR.data$Year == 2010] <- 2011


remsites <- c("Skj11", "Skj12", "Gud11", "Gud12", "Gud13")
#Remove TTCs not included in the data set 
my.GR.data <- my.GR.data[!my.GR.data$blockID %in% remsites,] 

my.GR.data$ID <- as.factor(paste(my.GR.data$turfID, my.GR.data$Year, sep = "_"))
my.GR.data$funID <- as.factor(paste(my.GR.data$turfID, my.GR.data$Year, my.GR.data$functionalgroup, sep = "_"))

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


#head(traits)
#nrow(traits)

identical(as.character(traits$species), names(cover)) #this should be identical

my.GR.data$specialism <- traits$specialism[match(my.GR.data$species, traits$species)]
my.GR.data$functionalgroup <- as.factor(traits$functionalGroup[match(my.GR.data$species, traits$species)])
my.GR.data$SLA <- traits$SLA[match(my.GR.data$species, traits$species)]
my.GR.data$Height <- traits$height[match(my.GR.data$species, traits$species)]
my.GR.data$leafSize <- traits$leafSize[match(my.GR.data$species, traits$species)]
my.GR.data$seedMass <- traits$seedMass[match(my.GR.data$species, traits$species)]


###### generalists vs specialists ######
funcover <- my.GR.data %>% group_by(ID, functionalgroup) %>% select(-species) %>% mutate(functcover = mean(cover)) %>% distinct(ID, functionalgroup, .keep_all = TRUE)

funcover <- xtabs(cover ~ paste(turfID, functionalgroup, Year, sep = "_") + species, data = my.GR.data)
funcover <- as.data.frame(unclass(funcover))
funcover <- cover[,colSums(cover > 0) > 0] #remove empty spp
head(funcover)

weighted.means <- data.frame(matrix(nrow = nrow(my.GR.data), ncol = 4))
colnames(weighted.means) <- c("Height", "leafSize", "seedMass", "SLA") # add later -> , "Minheight", "Maxheight"
weighted.means$funID <- my.GR.data$funID
weighted.means <- unique(weighted.means)

head(weighted.means)

for(i in levels(my.GR.data$funID)) {
    weighted.means$Height[my.GR.data$funID == "i","Height"] <- weighted.mean(my.GR.data$Height[my.GR.data$funID == "i"], my.GR.data$cover[my.GR.data$funID == "i"], na.rm = TRUE)
    weighted.means$leafSize[my.GR.data$funID == "i", "leafSize"] <- weighted.mean(my.GR.data$leafSize[my.GR.data$funID == "i"], my.GR.data$cover[my.GR.data$funID == "i"], na.rm = TRUE)
    weighted.means$seedMass[my.GR.data$funID == "i", "seedMass"] <- weighted.mean(my.GR.data$seedMass[my.GR.data$funID == "i"], my.GR.data$cover[my.GR.data$funID == "i"], na.rm = TRUE)
    weighted.means$SLA[my.GR.data$funID == "i", "SLA"] <- weighted.mean(my.GR.data$SLA[my.GR.data$funID == "i"], my.GR.data$cover[my.GR.data$funID == "i"], na.rm = TRUE)
    
}


(my.GR.data[my.GR.data$functionalgroup == "graminoid", 14:17], weighted.mean, my.GR.data[my.GR.data$functionalgroup == "graminoid", "cover"], na.rm=TRUE)


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


##### calculate weighted means #####
weighted.means <- matrix(nrow = nrow(cover), ncol = 4)
colnames(weighted.means) <- c("Height", "leafSize", "seedMass", "SLA") # add later -> , "Minheight", "Maxheight"
head(weighted.means)
cover[is.na(cover)] <- 0
for (i in 1:nrow(cover)){
  
  weighted.means[i,1] <- weighted.mean(traits$height, unlist(cover[i,]), na.rm = TRUE)
  weighted.means[i,2] <- weighted.mean(traits$leafSize, cover[i,], na.rm = TRUE)
  weighted.means[i,3] <- weighted.mean(traits$seedMass, cover[i,], na.rm = TRUE)
  weighted.means[i,4] <- weighted.mean(traits$SLA, cover[i,], na.rm = TRUE)
  #weighted.means[i,5] <- weighted.mean(traits$Min.height, cover[i,], na.rm = TRUE)
  #weighted.means[i,6] <- weighted.mean(traits$Max.height, cover[i,], na.rm = TRUE)
}

head(weighted.means)
weighted.means <- as.data.frame(weighted.means)
weighted.means$ID <- row.names(cover)

my.GR.data <- full_join(my.GR.data, weighted.means, by = "ID")


all.cover.meta$prec <- c(0.6,1.2,2.0,2.7)[all.cover.meta$Precipitation_level]
all.cover.meta$temp <- c(6.5,8.5,10.5)[all.cover.meta$Temperature_level]

#Meta data  
cover.meta <- unique(my.GR.data[,c("siteID", "TTtreat", "Year", "blockID", "turfID","Temperature_level", "Precipitation_level")])

cover.meta <- cover.meta[order(paste(cover.meta$turfID, cover.meta$Year)),] #make sure plots are in the same order as the cover data 
all(paste(cover.meta$turfID, cover.meta$Year, sep = "_") == rownames(cover)) #if everything is correct, this should be TRUE! 

cover.meta$TTtreat <- factor(as.character(cover.meta$TTtreat), levels = c("TTC", "RTC"))



############### CALCULATING DIVERSITY MEASURES ###############

#Species richness
library(vegan)

diversity.freq <- rowSums(cover > 0)

#Shannon's diversity index
diversity.freq <- data.frame(richness = diversity.freq, diversity = diversity(cover, index = "shannon")) #requires vegan

#Species evenness
diversity.freq <- cbind(diversity.freq, evenness = diversity.freq$diversity/log(diversity.freq$richness), expdiversity = exp(diversity.freq$diversity))
cover.meta <- cbind(cover.meta, diversity.freq)
head(cover.meta)


################## CALCULATING TRAITS DELTA ###################
cover.meta$sumcover <- rowSums(cover)

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

my.GR.data <- merge(my.GR.data, traits, by = "species")


for (i in 1:nrow(my.GR.data)){
  
  weighted.means[i,1] <- weighted.mean(traits$height, my.GR.data$cover[i,], na.rm = TRUE)
  weighted.means[i,2] <- weighted.mean(traits$leafSize, my.GR.data$cover[i,], na.rm = TRUE)
  weighted.means[i,3] <- weighted.mean(traits$seedMass, my.GR.data$cover[i,], na.rm = TRUE)
  weighted.means[i,4] <- weighted.mean(traits$SLA, my.GR.data$cover[i,], na.rm = TRUE)
  
}


################## CALCULATING TRAITS DELTA ###################
cover.meta$sumcover <- rowSums(cover)

deltacalc <- sapply(1:nrow(my.GR.data[my.GR.data$TTtreat == "RTC",]), function(i){
  R <- my.GR.data[my.GR.data$TTtreat == "RTC",][i,]
  #browser()
  cols <- c("Height", "SLA", "leafSize", "seedMass")
  friend <- my.GR.data$Year == R$Year & my.GR.data$blockID == R$blockID & my.GR.data$TTtreat == "TTC"
  if(all (!friend)) {print(R$turfID)
    return(rep(NA, length(cols)))}
  stopifnot(sum(friend) == 1)
  
  f <- my.GR.data[friend,]
  x <- R[,cols] - f[,cols]
  unlist(x)
})
deltacalc <- as.data.frame(t(deltacalc))
colnames(deltacalc) <- paste0("delta", colnames(deltacalc))
rtcmeta <- cbind((cover.meta[cover.meta$TTtreat == "RTC",]), deltacalc)
#rtcmeta<-rtcmeta[rtcmeta$Year!=2011,]



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

