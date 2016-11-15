##############################################################################
#Script for paper on effect of graminoid removal on plant community properties
##############################################################################

#Important bit to make the connection to the database work: 
#You can override which version of R is used via General panel of the RStudio Options dialog. This dialog allows you to specify that RStudio 
#should always bind to the default 32 or 64-bit version of R, or to specify a different version altogether: Tools -> Options -> General -> R version -> Change

#load library to connect to database

#connect to database
library(DBI)
con <- dbConnect(RMySQL::MySQL(), group = "seedclim")

############### Cover data ###############
#RTCs
myGRdata <- dbGetQuery(con, paste("SELECT sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.GRtreat, new_TurfCommunity.Year, new_TurfCommunity.species, new_TurfCommunity.cover, sites.Temperature_level, sites.Precipitation_level
FROM (((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.destinationPlotID) INNER JOIN new_TurfCommunity ON turfs.turfID = new_TurfCommunity.turfID
GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.GRtreat, new_TurfCommunity.Year, new_TurfCommunity.species, new_TurfCommunity.cover, sites.Temperature_level, sites.Precipitation_level
HAVING (((turfs.GRtreat) Is Not Null));"))

#If something goes wrong with extracting the data from the database, you may not get an error... so have a look to make sure everything's all right: 

str(myGRdata)
head(myGRdata)
dim(myGRdata)

myGRdata<-myGRdata[!is.na(myGRdata$GRtreat),] #Remove non-RTC entries 
dim(myGRdata)
myGRdata[1:100,]

#TTCs
myTTdata <- dbGetQuery(con, paste("SELECT sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, new_TurfCommunity.Year, new_TurfCommunity.species, new_TurfCommunity.cover, sites.Temperature_level, sites.Precipitation_level
FROM (((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.destinationPlotID) INNER JOIN new_TurfCommunity ON turfs.turfID = new_TurfCommunity.turfID
GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, new_TurfCommunity.Year, new_TurfCommunity.species, new_TurfCommunity.cover, sites.Temperature_level, sites.Precipitation_level;"))

#If something goes wrong with extracting the data from the database, you may not get an error... so have a look to make sure everything's all right: 

str(myTTdata)
head(myTTdata)
dim(myTTdata)

myTTdata <- myTTdata[!is.na(myTTdata$TTtreat),]
dim(myTTdata)
myTTdata[1:100,]

myTTdata<-myTTdata[myTTdata$Year!="2009",] #remove 2009 data 
dim(myTTdata)

myTTdata<-myTTdata[myTTdata$TTtreat == "TTC",] #remove other treatments 
dim(myTTdata)

names(myTTdata) <- gsub("TTtreat", "GRtreat", names(myTTdata)) #rename column before merging RTCs and TTCs
head(myTTdata)

mydata<-rbind(myGRdata,myTTdata)
dim(myGRdata)
dim(myTTdata)
dim(mydata)
head(mydata)

#make fat table
cover <- xtabs(cover ~ paste(turfID, Year, sep = "_") + species, data = mydata)
cover <- as.data.frame(unclass(cover))
dim(cover)
head(cover)
cover[1:100,]
cover[1:20,1:10]
cover[,1:10]

cover[1,] #see which species are graminoids 3,4,10,14,22:38,45,47,56:58,79:81,86:88,94,95,105,106,112,113,169,170,172,184,185,188,192
coverForbsOnly<-cover[,-c(3,4,10,14,22:38,44,46,55:57,78:80,85:87,93,94,104,105,111,112,168,169,171,184,185,188,192)] #remove data for graminoids (we don't consider them, since they were removed...)

dim(coverForbsOnly)
coverForbsOnly<-coverForbsOnly[,order(names(coverForbsOnly))] #new data set with only forbs 

#These need to be fixed: 
coverForbsOnly["Fau4RTC_2013",3]<-0
coverForbsOnly["Fau4RTC_2013",4]<-60
coverForbsOnly["Fau4RTC_2013",3:4]

coverForbsOnly["260 TTC_2013",100]<-0
coverForbsOnly["260 TTC_2013",98]<-1
coverForbsOnly["260 TTC_2013",98:100]

coverForbsOnly["506 TTC_2010",100]<-0
coverForbsOnly["506 TTC_2010",98]<-3
coverForbsOnly["506 TTC_2010",98:100]

coverForbsOnly["528 TTC_2013",99]<-0
coverForbsOnly["528 TTC_2013",98]<-4
coverForbsOnly["528 TTC_2013",98:100]

coverForbsOnly["Gud2RTC_2010",100]<-0
coverForbsOnly["Gud2RTC_2010",98]<-3
coverForbsOnly["Gud2RTC_2010",98:100]

coverForbsOnly["Lav3RTC_2010",100]<-0
coverForbsOnly["Lav3RTC_2010",98]<-1
coverForbsOnly["Lav3RTC_2010",98:100]

coverForbsOnly["Lav5RTC_2010",100]<-0
coverForbsOnly["Lav5RTC_2010",98]<-1
coverForbsOnly["Lav5RTC_2010",98:100]

coverForbsOnly["Ulv2RTC_2012",100]<-0
coverForbsOnly["Ulv2RTC_2012",98]<-2
coverForbsOnly["Ulv2RTC_2012",98:100]

coverForbsOnly["222 TTC_2013",53]<-0
coverForbsOnly["222 TTC_2013",54]<-3
coverForbsOnly["222 TTC_2013",52:56]

#Meta data  
cover.meta<-unique(mydata[,c("siteID", "GRtreat", "Year", "blockID", "turfID", "Temperature_level", "Precipitation_level")])
cover.meta<-cover.meta[-392,] #remove row with only NAs

cover.meta<-cover.meta[order(paste(cover.meta$turfID, cover.meta$Year)),] #make sure plots are in the same order as the cover data... 
all(paste(cover.meta$turfID, cover.meta$Year, sep="_")==rownames(cover.forbs.only)) #if everything is correct, this should be TRUE! 

cover.meta$GRtreat<-factor(as.character(cover.meta$GRtreat), levels=c("TTC","RTC"))
dim(cover.meta)

############### SUBPLOT FREQUENCIES ###############

#RTCs
subturfGRthin <- dbGetQuery(con, paste("SELECT sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.GRtreat, newSubTurfCommunity.subTurf, newSubTurfCommunity.Year, newSubTurfCommunity.species, sites.Temperature_level, sites.Precipitation_level
FROM (((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) INNER JOIN newSubTurfCommunity ON turfs.turfID = newSubTurfCommunity.turfID
GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.GRtreat, newSubTurfCommunity.subturf, newSubTurfCommunity.Year, newSubTurfCommunity.species, sites.Temperature_level, sites.Precipitation_level
HAVING (((turfs.GRtreat) Is Not Null));"))

head(subturfGRthin)
dim(subturfGRthin)
subturfGRthin[1:100,]

GRsubturf<-subturfGRthin[subturfGRthin$GRtreat!="",]
head(GRsubturf)
dim(GRsubturf)
GRsubturf[1:100,]

#TTCs
subturfTTthin <- dbGetQuery(con, paste("SELECT sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, newSubTurfCommunity.subTurf, newSubTurfCommunity.Year, newSubTurfCommunity.species, sites.Temperature_level, sites.Precipitation_level
FROM (((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) INNER JOIN newSubTurfCommunity ON turfs.turfID = newSubTurfCommunity.turfID
GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, newSubTurfCommunity.subturf, newSubTurfCommunity.Year, newSubTurfCommunity.species, sites.Temperature_level, sites.Precipitation_level;"))

head(subturfTTthin)
dim(subturfTTthin)
subturfTTthin[1:100,]

TTsubturf<-subturfTTthin[subturfTTthin$TTtreat!="",]
dim(TTsubturf)
TTsubturf[1:100,]

TTsubturf<-TTsubturf[TTsubturf$Year!="2009",] #remove 2009 data 
dim(TTsubturf)
TTsubturf[1:100,]

TTsubturf<-TTsubturf[TTsubturf$TTtreat!="TT1",] #remove other treatments 
dim(TTsubturf)
TTsubturf[1:100,]
TTsubturf<-TTsubturf[TTsubturf$TTtreat!="TT2",]
dim(TTsubturf)
TTsubturf[1:100,]
TTsubturf<-TTsubturf[TTsubturf$TTtreat!="TT3",]
dim(TTsubturf)
TTsubturf[1:100,]
TTsubturf<-TTsubturf[TTsubturf$TTtreat!="TT4",]
dim(TTsubturf)
TTsubturf[1:100,]

names(TTsubturf) <- gsub("TTtreat", "GRtreat", names(TTsubturf)) #rename column before merging RTCs and TTCs
head(TTsubturf)

myturfdata<-rbind(GRsubturf,TTsubturf)
dim(GRsubturf)
dim(TTsubturf)
dim(myturfdata)
head(myturfdata)

#make fat table        
subturf<-xtabs(rep(1, nrow(myturfdata))~paste(turfID, subTurf, Year, sep="_")+species, data=myturfdata)
subturf<-as.data.frame(unclass(subturf))
dim(subturf)
subturf[1:100,]

#Meta-data
subturf.meta<-unique(myturfdata[,c("siteID", "GRtreat", "Year", "blockID", "turfID","subTurf","Temperature_level",  "Precipitation_level")])
subturf.meta<-subturf.meta[order(paste(subturf.meta$turfID, subturf.meta$subTurf, subturf.meta$Year, sep="_" )),] #make sure the order is right
subturf.meta[1:25,]
dim(subturf.meta)

subturf.meta<-subturf.meta[-9345,] #remove row with only NAs
dim(subturf.meta)

all(paste(subturf.meta$turfID, subturf.meta$subTurf, subturf.meta$Year, sep="_")==rownames(subturf))
#if everything is correct, this should be TRUE!

#Calculate subplot frequencies
fsubturf<-by(subturf, paste(subturf.meta$turfID, subturf.meta$Year, sep="_"), colSums)
fsubturf<-t(sapply(fsubturf,I))
fsubturf<-as.data.frame(fsubturf)
fsubturf[1:25,1:5]
dim(fsubturf)

freqsubturf<-fsubturf[, 1:202]/25
freqsubturf[1:3,1:202]
dim(freqsubturf)

freqsubturf[1,]
freqForbsOnly<-freqsubturf[,-c(3,4,10,14,22:38,45,47,56:58,79:81,86:88,94,95,105,106,112,113,169,170,172,184,185,188,192)] #remove graminoids
dim(freqForbsOnly)
freqForbsOnly[1:25,1:5]

#Fixing some record... 
freqForbsOnly["Fau4RTC_2013",3]<-0
freqForbsOnly["Fau4RTC_2013",4]<-1
freqForbsOnly["Fau4RTC_2013",3:4]

freqForbsOnly["260 TTC_2013",74]<-0
freqForbsOnly["260 TTC_2013",72]<-0.08
freqForbsOnly["260 TTC_2013",72:74]

freqForbsOnly["506 TTC_2010",74]<-0
freqForbsOnly["506 TTC_2010",72]<-0.12
freqForbsOnly["506 TTC_2010",72:74]

freqForbsOnly["528 TTC_2013",73]<-0
freqForbsOnly["528 TTC_2013",72]<-0.04
freqForbsOnly["528 TTC_2013",72:74]

freqForbsOnly["Gud2RTC_2010",74]<-0
freqForbsOnly["Gud2RTC_2010",72]<-0.16
freqForbsOnly["Gud2RTC_2010",72:74]

freqForbsOnly["Lav3RTC_2010",74]<-0
freqForbsOnly["Lav3RTC_2010",72]<-0.04
freqForbsOnly["Lav3RTC_2010",72:74]

freqForbsOnly["Lav5RTC_2010",74]<-0
freqForbsOnly["Lav5RTC_2010",72]<-0.04
freqForbsOnly["Lav5RTC_2010",72:74]

freqForbsOnly["Ulv2RTC_2012",74]<-0
freqForbsOnly["Ulv2RTC_2012",72]<-0.08
freqForbsOnly["Ulv2RTC_2012",72:74]

freqForbsOnly["222 TTC_2013",42]<-0
freqForbsOnly["222 TTC_2013",43]<-0.08
freqForbsOnly["222 TTC_2013",41:45]

all(paste(cover.meta$turfID, cover.meta$Year, sep="_")==rownames(freqForbsOnly))
#if everything is correct, this should be TRUE!

#### Fixing data sets ####

#Remove TTCs not included in the data set 
freqForbsOnly<-freqForbsOnly[-c(44:51,107:110,111:113),] 
coverForbsOnly<-coverForbsOnly[-c(44:51,107:110,111:113),] 
cover.meta<-cover.meta[-c(44:51,107:110,111:113),] 

cover.meta$treat<-c(rep(0,times=241), rep(1,times=177))
cover.meta$block<-substring(cover.meta$blockID, 4,4)
cover.meta$fblock<-as.factor(cover.meta$block)

cover.meta$unique.block<-paste(cover.meta$Year, cover.meta$blockID)
cover.meta$Year <- gsub("2010", "2011", cover.meta$Year) #RTCs were analyzed in 2010, but to make things easier for the analyses, rename to 2011
cover.meta$Year<-as.numeric(cover.meta$Year)

############### CALCULATING DIVERSITY MEASURES ###############

#Species richness
library(vegan)

diversity.freq<-matrix(nrow=nrow(freqForbsOnly),ncol=1)
for(i in 1:nrow(freqForbsOnly)){
  diversity.freq[i]<-sum(freqForbsOnly[i,]>0)
}

#Shannon's diversity index
diversity.freq<-(cbind(diversity.freq,diversity(freqForbsOnly, index = "shannon"))) #requires vegan

#Species evenness
diversity.freq<-cbind(diversity.freq,diversity.freq[,2]/log(diversity.freq[,1]))

#Combine the three... 
colnames(diversity.freq)<-c("richness", "diversity", "evenness")

diversity.freq<-as.data.frame(diversity.freq)

diversity.data<-cbind(cover.meta, diversity.freq)
head(diversity.data)

############### TRAITS ###############

#load from data base
traits <- dbGetQuery(con, paste("SELECT taxon.*, [more traits].* FROM taxon LEFT JOIN [more traits] ON taxon.species = [more traits].species order by taxon.species asc;"))

head(traits)
names(traits) <- make.names(names(traits))
nrow(traits)

traits <- traits[traits$species%in%names(freqForbsOnly),]
sortfreqForbsOnly<-freqForbsOnly[,sort(names(freqForbsOnly))]
identical(as.character(traits$species), names(sortfreqForbsOnly)) #this should be identical 

traits<-traits[traits$species%in%names(sortfreqForbsOnly),]
identical(as.character(traits$species), names(sortfreqForbsOnly)) #this should be identical 

#calculate weighted means 
weighted.means<-matrix(nrow=nrow(freqForbsOnly), ncol=6)
colnames(weighted.means)<-c("Height", "leafSize", "seedMass", "SLA", "Min.height", "Max.height")
dim(weighted.means)
head(weighted.means)

for (i in 1:nrow(freqForbsOnly)){
  
  weighted.means[i,1] <- weighted.mean(traits$height, sortfreqForbsOnly[i,], na.rm = TRUE)
  weighted.means[i,2] <- weighted.mean(traits$leafSize, sortfreqForbsOnly[i,], na.rm = TRUE)
  weighted.means[i,3] <- weighted.mean(traits$seedMass, sortfreqForbsOnly[i,], na.rm = TRUE)
  weighted.means[i,4] <- weighted.mean(traits$SLA, sortfreqForbsOnly[i,], na.rm = TRUE)
  weighted.means[i,5] <- weighted.mean(traits$Min.height, sortfreqForbsOnly[i,], na.rm = TRUE)
  weighted.means[i,6] <- weighted.mean(traits$Max.height, sortfreqForbsOnly[i,], na.rm = TRUE)
  
}

head(weighted.means)

############### SEEDLING DATA ###############

#RTCs
seedlingGR <- dbGetQuery(con, paste("SELECT sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.GRtreat, newSubTurfCommunity.subTurf, newSubTurfCommunity.Year, newSubTurfCommunity.species, newSubTurfCommunity.seedlings, newSubTurfCommunity.juvenile, newSubTurfCommunity.fertile, newSubTurfCommunity.dominant, sites.Temperature_level, sites.Precipitation_level
FROM (((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) INNER JOIN newSubTurfCommunity ON turfs.turfID = newSubTurfCommunity.turfID
GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.GRtreat, newSubTurfCommunity.subturf, newSubTurfCommunity.Year, newSubTurfCommunity.species, newSubTurfCommunity.seedlings, newSubTurfCommunity.juvenile, newSubTurfCommunity.fertile, newSubTurfCommunity.dominant, sites.Temperature_level, sites.Precipitation_level
HAVING (((turfs.GRtreat) Is Not Null));"))

head(seedlingGR)
dim(seedlingGR)

seedlingGRdata <- seedlingGR[seedlingGR$GRtreat != "",]
head(seedlingGRdata)
dim(seedlingGRdata)
seedlingGRdata[1:100,]

seedlingGRdata <- seedlingGRdata[seedlingGRdata$Year != "2009",]
seedlingGRdata <- seedlingGRdata[seedlingGRdata$Year != "2010",]
seedlingGRdata <- seedlingGRdata[seedlingGRdata$Year != "2011",]
seedlingGRdata <- seedlingGRdata[seedlingGRdata$Year != "2012",]
head(seedlingGRdata)
seedlingGRdata[1:100,]

#TTCs
seedlingTT <- dbGetQuery(con, paste("SELECT sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, newSubTurfCommunity.subTurf, newSubTurfCommunity.Year, newSubTurfCommunity.species, newSubTurfCommunity.seedlings, newSubTurfCommunity.juvenile, newSubTurfCommunity.fertile, newSubTurfCommunity.dominant, sites.Temperature_level, sites.Precipitation_level
FROM (((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) INNER JOIN newSubTurfCommunity ON turfs.turfID = newSubTurfCommunity.turfID
GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, newSubTurfCommunity.subturf, newSubTurfCommunity.Year, newSubTurfCommunity.species, newSubTurfCommunity.seedlings, newSubTurfCommunity.juvenile, newSubTurfCommunity.fertile, newSubTurfCommunity.dominant, sites.Temperature_level, sites.Precipitation_level;"))

head(seedlingTT)
dim(seedlingTT)
seedlingTT[1:100,]

seedlingTTdata <- seedlingTT[seedlingTT$TTtreat!="",]
dim(seedlingTTdata)
seedlingTTdata[1:100,]

seedlingTTdata <- seedlingTTdata[seedlingTTdata$Year!="2009",]
dim(seedlingTTdata)
seedlingTTdata[1:100,]
seedlingTTdata <- seedlingTTdata[seedlingTTdata$Year!="2010",]
dim(seedlingTTdata)
seedlingTTdata[1:100,]
seedlingTTdata <- seedlingTTdata[seedlingTTdata$Year!="2011",]
dim(seedlingTTdata)
seedlingTTdata[1:100,]
seedlingTTdata <- seedlingTTdata[seedlingTTdata$Year!="2012",]
dim(seedlingTTdata)
seedlingTTdata[1:100,]

seedlingTTdata <- seedlingTTdata[seedlingTTdata$TTtreat!="TT1",]
dim(seedlingTTdata)
seedlingTTdata[1:100,]
seedlingTTdata <- seedlingTTdata[seedlingTTdata$TTtreat!="TT2",]
dim(seedlingTTdata)
seedlingTTdata[1:100,]
seedlingTTdata <- seedlingTTdata[seedlingTTdata$TTtreat!="TT3",]
dim(seedlingTTdata)
seedlingTTdata[1:100,]
seedlingTTdata <- seedlingTTdata[seedlingTTdata$TTtreat!="TT4",]
dim(seedlingTTdata)
seedlingTTdata[1:100,]

names(seedlingTTdata) <- gsub("TTtreat", "GRtreat", names(seedlingTTdata))
head(seedlingTTdata)

myseedlingdata <- rbind(seedlingGRdata,seedlingTTdata)
dim(seedlingGRdata)
dim(seedlingTTdata)
dim(myseedlingdata)
head(myseedlingdata)

#fat table       
seedlings <- xtabs(seedlings ~ paste(turfID, Year, sep="_") + species, data = myseedlingdata)
seedlings <- as.data.frame(unclass(seedlings))
dim(seedlings)
seedlings[1:100,]

seedlings <- seedlings[-c(47,50,51,54,55),] #remove some TTCs not included in this experiment

seedlings.meta <- cover.meta[cover.meta$Year == 2013,]

all(paste(seedlings.meta$turfID, seedlings.meta$Year, sep="_") == rownames(seedlings))
#If everything's correct, this should be TRUE

#Juveniles 
juveniles <- xtabs(juvenile~paste(turfID, Year, sep = "_") + species, data = myseedlingdata)
juveniles <- as.data.frame(unclass(juveniles))
dim(juveniles)
juveniles[1:100,]
juveniles.meta <- cover.meta[cover.meta$Year == 2013,]

juveniles <- juveniles[-c(47,50,51,54,55),]
all(paste(seedlings.meta$turfID, seedlings.meta$Year, sep="_") == rownames(juveniles))
#Should be TRUE

#Calculating number of seedlings
no.seedlings <- matrix(nrow = nrow(seedlings), ncol = 1)
for(i in 1:nrow(seedlings)){
  no.seedlings[i] <- sum(seedlings[i,])
  
}

#Calculating number of juveniles 
no.juveniles <- matrix(nrow = nrow(juveniles), ncol = 1)
for(i in 1:nrow(juveniles)){
  no.juveniles[i]<-sum(juveniles[i,])
  
}

#Combining the two
recruitment <- cbind(no.seedlings,no.juveniles)

colnames(recruitment) <- c("seedlings", "juveniles")

recruitment <- as.data.frame(recruitment)

recruitment.data <- cbind(seedlings.meta, recruitment)
head(recruitment.data)
