##############################################################################
#Script for paper on effect of graminoid removal on plant community properties
##############################################################################

#Important bit to make the connection to the database work: 
#You can override which version of R is used via General panel of the RStudio Options dialog. This dialog allows you to specify that RStudio 
#should always bind to the default 32 or 64-bit version of R, or to specify a different version altogether: Tools -> Options -> General -> R version -> Change

#load library to connect to database
library(RODBC)

#connect to database
db<-"O:\\eir.uib.no\\home\fja062\\seedclim_2014-5-20 - Copy.mdb" #edit this line with correct file name and location
con<-odbcConnectAccess2010(db)
sqlTables(con)

############### Cover data ###############
#RTCs
query1<-"SELECT sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.GRtreat, new_TurfCommunity.Year, new_TurfCommunity.species, new_TurfCommunity.cover, sites.Temperature_level, sites.Precipitation_level
FROM (((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.destinationPlotID) INNER JOIN new_TurfCommunity ON turfs.turfID = new_TurfCommunity.turfID
GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.GRtreat, new_TurfCommunity.Year, new_TurfCommunity.species, new_TurfCommunity.cover, sites.Temperature_level, sites.Precipitation_level
HAVING (((turfs.GRtreat) Is Not Null));"

my.GR.data<-sqlQuery(con,query1)
#If something goes wrong with extracting the data from the database, you may not get an error... so have a look to make sure everything's all right: 
head(my.GR.data)
dim(my.GR.data)
my.GR.data[1:100,]

my.GR.data<-my.GR.data[my.GR.data$GRtreat!="",] #Remove non-RTC entries 
dim(my.GR.data)
my.GR.data[1:100,]

#TTCs
query2<-"SELECT sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, new_TurfCommunity.Year, new_TurfCommunity.species, new_TurfCommunity.cover, sites.Temperature_level, sites.Precipitation_level
FROM (((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.destinationPlotID) INNER JOIN new_TurfCommunity ON turfs.turfID = new_TurfCommunity.turfID
GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, new_TurfCommunity.Year, new_TurfCommunity.species, new_TurfCommunity.cover, sites.Temperature_level, sites.Precipitation_level;"

my.TT.data<-sqlQuery(con,query2)
#If something goes wrong with extracting the data from the database, you may not get an error... so have a look to make sure everything's all right: 
head(my.TT.data)
dim(my.TT.data)
my.TT.data[1:100,]

my.TT.data<-my.TT.data[my.TT.data$TTtreat!="",]
dim(my.TT.data)
my.TT.data[1:100,]

my.TT.data<-my.TT.data[my.TT.data$Year!="2009",] #remove 2009 data 
dim(my.TT.data)
my.TT.data[1:100,]

my.TT.data<-my.TT.data[my.TT.data$TTtreat!="TT1",] #remove other treatments 
dim(my.TT.data)
my.TT.data[1:100,]
my.TT.data<-my.TT.data[my.TT.data$TTtreat!="TT2",]
dim(my.TT.data)
my.TT.data[1:100,]
my.TT.data<-my.TT.data[my.TT.data$TTtreat!="TT3",]
dim(my.TT.data)
my.TT.data[1:100,]
my.TT.data<-my.TT.data[my.TT.data$TTtreat!="TT4",]
dim(my.TT.data)
my.TT.data[1:100,]

names(my.TT.data) <- gsub("TTtreat", "GRtreat", names(my.TT.data)) #rename column before merging RTCs and TTCs
head(my.TT.data)

mydata<-rbind(my.GR.data,my.TT.data)
dim(my.GR.data)
dim(my.TT.data)
dim(mydata)
head(mydata)

#make fat table
cover<-xtabs(cover~paste(turfID, Year, sep="_")+species, data=mydata)
cover<-as.data.frame(unclass(cover))
dim(cover)
head(cover)
cover[1:100,]
cover[1:20,1:10]
cover[,1:10]

cover[1,] #see which species are graminoids 
cover.forbs.only<-cover[,-c(2,3,9,13,21:37,43,45,52:54,75:77,82:84,88:89,98:99,105:106,161:162,164)] #remove data for graminoids (we don't consider them, since they were removed...)

dim(cover.forbs.only)
cover.forbs.only<-cover.forbs.only[,order(names(cover.forbs.only))] #new data set with only forbs 

#These need to be fixed: 
cover.forbs.only["Fau4RTC_2013",3]<-0
cover.forbs.only["Fau4RTC_2013",4]<-60
cover.forbs.only["Fau4RTC_2013",3:4]

cover.forbs.only["260 TTC_2013",100]<-0
cover.forbs.only["260 TTC_2013",98]<-1
cover.forbs.only["260 TTC_2013",98:100]

cover.forbs.only["506 TTC_2010",100]<-0
cover.forbs.only["506 TTC_2010",98]<-3
cover.forbs.only["506 TTC_2010",98:100]

cover.forbs.only["528 TTC_2013",99]<-0
cover.forbs.only["528 TTC_2013",98]<-4
cover.forbs.only["528 TTC_2013",98:100]

cover.forbs.only["Gud2RTC_2010",100]<-0
cover.forbs.only["Gud2RTC_2010",98]<-3
cover.forbs.only["Gud2RTC_2010",98:100]

cover.forbs.only["Lav3RTC_2010",100]<-0
cover.forbs.only["Lav3RTC_2010",98]<-1
cover.forbs.only["Lav3RTC_2010",98:100]

cover.forbs.only["Lav5RTC_2010",100]<-0
cover.forbs.only["Lav5RTC_2010",98]<-1
cover.forbs.only["Lav5RTC_2010",98:100]

cover.forbs.only["Ulv2RTC_2012",100]<-0
cover.forbs.only["Ulv2RTC_2012",98]<-2
cover.forbs.only["Ulv2RTC_2012",98:100]

cover.forbs.only["222 TTC_2013",53]<-0
cover.forbs.only["222 TTC_2013",54]<-3
cover.forbs.only["222 TTC_2013",52:56]

#Meta data  
cover.meta<-unique(mydata[,c("siteID", "GRtreat", "Year", "blockID", "turfID", "Temperature_level", "Precipitation_level")])
cover.meta<-cover.meta[-299,] #remove row with only NAs

cover.meta<-cover.meta[order(paste(cover.meta$turfID, cover.meta$Year)),] #make sure plots are in the same order as the cover data... 
all(paste(cover.meta$turfID, cover.meta$Year, sep="_")==rownames(cover.forbs.only)) #if everything is correct, this should be TRUE! 

cover.meta$GRtreat<-factor(as.character(cover.meta$GRtreat), levels=c("TTC","RTC"))
dim(cover.meta)

############### SUBPLOT FREQUENCIES ###############

#RTCs
subturfGR<-"SELECT sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.GRtreat, newSubTurfCommunity.subTurf, newSubTurfCommunity.Year, newSubTurfCommunity.species, sites.Temperature_level, sites.Precipitation_level
FROM (((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) INNER JOIN newSubTurfCommunity ON turfs.turfID = newSubTurfCommunity.turfID
GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.GRtreat, newSubTurfCommunity.subturf, newSubTurfCommunity.Year, newSubTurfCommunity.species, sites.Temperature_level, sites.Precipitation_level
HAVING (((turfs.GRtreat) Is Not Null));"

subturf.GR.thin<-sqlQuery(con, subturfGR)                              
head(subturf.GR.thin)
dim(subturf.GR.thin)
subturf.GR.thin[1:100,]

GR.subturf.data<-subturf.GR.thin[subturf.GR.thin$GRtreat!="",]
head(GR.subturf.data)
dim(GR.subturf.data)
GR.subturf.data[1:100,]

#TTCs
subturfTT<-"SELECT sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, newSubTurfCommunity.subTurf, newSubTurfCommunity.Year, newSubTurfCommunity.species, sites.Temperature_level, sites.Precipitation_level
FROM (((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) INNER JOIN newSubTurfCommunity ON turfs.turfID = newSubTurfCommunity.turfID
GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, newSubTurfCommunity.subturf, newSubTurfCommunity.Year, newSubTurfCommunity.species, sites.Temperature_level, sites.Precipitation_level
;"

subturf.TT.thin<-sqlQuery(con, subturfTT)                              
head(subturf.TT.thin)
dim(subturf.TT.thin)
subturf.TT.thin[1:100,]

TT.subturf.data<-subturf.TT.thin[subturf.TT.thin$TTtreat!="",]
dim(TT.subturf.data)
TT.subturf.data[1:100,]

TT.subturf.data<-TT.subturf.data[TT.subturf.data$Year!="2009",] #remove 2009 data 
dim(TT.subturf.data)
TT.subturf.data[1:100,]

TT.subturf.data<-TT.subturf.data[TT.subturf.data$TTtreat!="TT1",] #remove other treatments 
dim(TT.subturf.data)
TT.subturf.data[1:100,]
TT.subturf.data<-TT.subturf.data[TT.subturf.data$TTtreat!="TT2",]
dim(TT.subturf.data)
TT.subturf.data[1:100,]
TT.subturf.data<-TT.subturf.data[TT.subturf.data$TTtreat!="TT3",]
dim(TT.subturf.data)
TT.subturf.data[1:100,]
TT.subturf.data<-TT.subturf.data[TT.subturf.data$TTtreat!="TT4",]
dim(TT.subturf.data)
TT.subturf.data[1:100,]

names(TT.subturf.data) <- gsub("TTtreat", "GRtreat", names(TT.subturf.data)) #rename column before merging RTCs and TTCs
head(TT.subturf.data)

myturfdata<-rbind(GR.subturf.data,TT.subturf.data)
dim(GR.subturf.data)
dim(TT.subturf.data)
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

subturf.meta<-subturf.meta[-7121,] #remove row with only NAs
dim(subturf.meta)

all(paste(subturf.meta$turfID, subturf.meta$subTurf, subturf.meta$Year, sep="_")==rownames(subturf))
#if everything is correct, this should be TRUE!

#Calculate subplot frequencies
fsubturf<-by(subturf, paste(subturf.meta$turfID, subturf.meta$Year, sep="_"), colSums)
fsubturf<-t(sapply(fsubturf,I))
fsubturf<-as.data.frame(fsubturf)
fsubturf[1:25,1:5]
dim(fsubturf)

freqsubturf<-fsubturf[, 1:198]/25
freqsubturf[1:25,1:5]
dim(freqsubturf)

freq.forbs.only<-freqsubturf[,-c(2,3,9,13,21:37,44,45,46,53:55,76:78,83:85,89:90,99:100,106:107,162:163,165,167,175,178,180,187,192)] #remove graminoids
dim(freq.forbs.only)
freq.forbs.only[1:25,1:5]

#Fixing some record... 
freq.forbs.only["Fau4RTC_2013",3]<-0
freq.forbs.only["Fau4RTC_2013",4]<-1
freq.forbs.only["Fau4RTC_2013",3:4]

freq.forbs.only["260 TTC_2013",74]<-0
freq.forbs.only["260 TTC_2013",72]<-0.08
freq.forbs.only["260 TTC_2013",72:74]

freq.forbs.only["506 TTC_2010",74]<-0
freq.forbs.only["506 TTC_2010",72]<-0.12
freq.forbs.only["506 TTC_2010",72:74]

freq.forbs.only["528 TTC_2013",73]<-0
freq.forbs.only["528 TTC_2013",72]<-0.04
freq.forbs.only["528 TTC_2013",72:74]

freq.forbs.only["Gud2RTC_2010",74]<-0
freq.forbs.only["Gud2RTC_2010",72]<-0.16
freq.forbs.only["Gud2RTC_2010",72:74]

freq.forbs.only["Lav3RTC_2010",74]<-0
freq.forbs.only["Lav3RTC_2010",72]<-0.04
freq.forbs.only["Lav3RTC_2010",72:74]

freq.forbs.only["Lav5RTC_2010",74]<-0
freq.forbs.only["Lav5RTC_2010",72]<-0.04
freq.forbs.only["Lav5RTC_2010",72:74]

freq.forbs.only["Ulv2RTC_2012",74]<-0
freq.forbs.only["Ulv2RTC_2012",72]<-0.08
freq.forbs.only["Ulv2RTC_2012",72:74]

freq.forbs.only["222 TTC_2013",42]<-0
freq.forbs.only["222 TTC_2013",43]<-0.08
freq.forbs.only["222 TTC_2013",41:45]

all(paste(cover.meta$turfID, cover.meta$Year, sep="_")==rownames(freq.forbs.only))
#if everything is correct, this should be TRUE!

#### Fixing data sets ####

#Remove TTCs not included in the data set 
freq.forbs.only<-freq.forbs.only[-c(139:141,149:154,161:166),] 
cover.forbs.only<-cover.forbs.only[-c(139:141,149:154,161:166),] 
cover.meta<-cover.meta[-c(139:141,149:154,161:166),] 

cover.meta$treat<-c(rep(0,times=181), rep(1,times=177))
cover.meta$block<-substring(cover.meta$blockID, 4,4)
cover.meta$fblock<-as.factor(cover.meta$block)

cover.meta$unique.block<-paste(cover.meta$Year, cover.meta$blockID)
cover.meta$Year <- gsub("2010", "2011", cover.meta$Year) #RTCs were analyzed in 2010, but to make things easier for the analyses, rename to 2011
cover.meta$Year<-as.numeric(cover.meta$Year)

############### CALCULATING DIVERSITY MEASURES ###############

#Species richness
library(vegan)

diversity.freq<-matrix(nrow=nrow(freq.forbs.only),ncol=1)
for(i in 1:nrow(freq.forbs.only)){
  diversity.freq[i]<-sum(freq.forbs.only[i,]>0)
}

#Shannon's diversity index
diversity.freq<-(cbind(diversity.freq,diversity(freq.forbs.only, index = "shannon"))) #requires vegan

#Species evenness
diversity.freq<-cbind(diversity.freq,diversity.freq[,2]/log(diversity.freq[,1]))

#Combine the three... 
colnames(diversity.freq)<-c("richness", "diversity", "evenness")

diversity.freq<-as.data.frame(diversity.freq)

diversity.data<-cbind(cover.meta, diversity.freq)
head(diversity.data)

############### TRAITS ###############

#load from data base
query3<-"SELECT taxon.*, [more traits].* FROM taxon LEFT JOIN [more traits] ON taxon.species = [more traits].species order by taxon.species asc;"
traits<-sqlQuery(con,query3)

head(traits)
names(traits)<-make.names(names(traits))
nrow(traits)

traits<-traits[traits$species%in%names(freq.forbs.only),]
sort.freq.forbs.only<-freq.forbs.only[,sort(names(freq.forbs.only))]
identical(as.character(traits$species), names(sort.freq.forbs.only)) #this should be identical 

traits<-traits[traits$species%in%names(sort.freq.forbs.only),]
identical(as.character(traits$species), names(sort.freq.forbs.only)) #this should be identical 

#calculate weighted means 
weighted.means<-matrix(nrow=nrow(freq.forbs.only), ncol=6)
colnames(weighted.means)<-c("Height", "leafSize", "seedMass", "SLA", "Min.height", "Max.height")
dim(weighted.means)
head(weighted.means)

for (i in 1:nrow(freq.forbs.only)){
  
  weighted.means[i,1] <-weighted.mean(traits$height,sort.freq.forbs.only[i,],na.rm=T)
  weighted.means[i,2] <-weighted.mean(traits$leafSize,sort.freq.forbs.only[i,],na.rm=T)
  weighted.means[i,3] <-weighted.mean(traits$seedMass,sort.freq.forbs.only[i,],na.rm=T)
  weighted.means[i,4] <-weighted.mean(traits$SLA,sort.freq.forbs.only[i,],na.rm=T)
  weighted.means[i,5] <-weighted.mean(traits$Min.height,sort.freq.forbs.only[i,],na.rm=T)
  weighted.means[i,6] <-weighted.mean(traits$Max.height,sort.freq.forbs.only[i,],na.rm=T)
  
}

head(weighted.means)

############### SEEDLING DATA ###############

#RTCs
seedlingGR<-"SELECT sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.GRtreat, newSubTurfCommunity.subTurf, newSubTurfCommunity.Year, newSubTurfCommunity.species, newSubTurfCommunity.seedlings, newSubTurfCommunity.juvenile, newSubTurfCommunity.fertile, newSubTurfCommunity.dominant, sites.Temperature_level, sites.Precipitation_level
FROM (((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) INNER JOIN newSubTurfCommunity ON turfs.turfID = newSubTurfCommunity.turfID
GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.GRtreat, newSubTurfCommunity.subturf, newSubTurfCommunity.Year, newSubTurfCommunity.species, newSubTurfCommunity.seedlings, newSubTurfCommunity.juvenile, newSubTurfCommunity.fertile, newSubTurfCommunity.dominant, sites.Temperature_level, sites.Precipitation_level
HAVING (((turfs.GRtreat) Is Not Null));"

seedlingGR<-sqlQuery(con,seedlingGR)
head(seedlingGR)
dim(seedlingGR)

seedling.GR.data<-seedlingGR[seedlingGR$GRtreat!="",]
head(seedling.GR.data)
dim(seedling.GR.data)
seedling.GR.data[1:100,]

seedling.GR.data<-seedling.GR.data[seedling.GR.data$Year!="2009",]
seedling.GR.data<-seedling.GR.data[seedling.GR.data$Year!="2010",]
seedling.GR.data<-seedling.GR.data[seedling.GR.data$Year!="2011",]
seedling.GR.data<-seedling.GR.data[seedling.GR.data$Year!="2012",]
head(seedling.GR.data)
seedling.GR.data[1:100,]

#TTCs
seedlingTT<-"SELECT sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, newSubTurfCommunity.subTurf, newSubTurfCommunity.Year, newSubTurfCommunity.species, newSubTurfCommunity.seedlings, newSubTurfCommunity.juvenile, newSubTurfCommunity.fertile, newSubTurfCommunity.dominant, sites.Temperature_level, sites.Precipitation_level
FROM (((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) INNER JOIN newSubTurfCommunity ON turfs.turfID = newSubTurfCommunity.turfID
GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, newSubTurfCommunity.subturf, newSubTurfCommunity.Year, newSubTurfCommunity.species, newSubTurfCommunity.seedlings, newSubTurfCommunity.juvenile, newSubTurfCommunity.fertile, newSubTurfCommunity.dominant, sites.Temperature_level, sites.Precipitation_level
;"

seedlingTT<-sqlQuery(con, seedlingTT)                              
head(seedlingTT)
dim(seedlingTT)
seedlingTT[1:100,]

seedling.TT.data<-seedlingTT[seedlingTT$TTtreat!="",]
dim(seedling.TT.data)
seedling.TT.data[1:100,]

seedling.TT.data<-seedling.TT.data[seedling.TT.data$Year!="2009",]
dim(seedling.TT.data)
seedling.TT.data[1:100,]
seedling.TT.data<-seedling.TT.data[seedling.TT.data$Year!="2010",]
dim(seedling.TT.data)
seedling.TT.data[1:100,]
seedling.TT.data<-seedling.TT.data[seedling.TT.data$Year!="2011",]
dim(seedling.TT.data)
seedling.TT.data[1:100,]
seedling.TT.data<-seedling.TT.data[seedling.TT.data$Year!="2012",]
dim(seedling.TT.data)
seedling.TT.data[1:100,]

seedling.TT.data<-seedling.TT.data[seedling.TT.data$TTtreat!="TT1",]
dim(seedling.TT.data)
seedling.TT.data[1:100,]
seedling.TT.data<-seedling.TT.data[seedling.TT.data$TTtreat!="TT2",]
dim(seedling.TT.data)
seedling.TT.data[1:100,]
seedling.TT.data<-seedling.TT.data[seedling.TT.data$TTtreat!="TT3",]
dim(seedling.TT.data)
seedling.TT.data[1:100,]
seedling.TT.data<-seedling.TT.data[seedling.TT.data$TTtreat!="TT4",]
dim(seedling.TT.data)
seedling.TT.data[1:100,]

names(seedling.TT.data) <- gsub("TTtreat", "GRtreat", names(seedling.TT.data))
head(seedling.TT.data)

myseedlingdata<-rbind(seedling.GR.data,seedling.TT.data)
dim(seedling.GR.data)
dim(seedling.TT.data)
dim(myseedlingdata)
head(myseedlingdata)

#fat table       
seedlings<-xtabs(seedlings~paste(turfID, Year, sep="_")+species, data=myseedlingdata)
seedlings<-as.data.frame(unclass(seedlings))
dim(seedlings)
seedlings[1:100,]

seedlings<-seedlings[-c(47,50,51,54,55),] #remove some TTCs not included in this experiment

seedlings.meta<-cover.meta[cover.meta$Year==2013,]

all(paste(seedlings.meta$turfID, seedlings.meta$Year, sep="_")==rownames(seedlings))
#If everything's correct, this should be TRUE

#Juveniles 
juveniles<-xtabs(juvenile~paste(turfID, Year, sep="_")+species, data=myseedlingdata)
juveniles<-as.data.frame(unclass(juveniles))
dim(juveniles)
juveniles[1:100,]
juveniles.meta<-cover.meta[cover.meta$Year==2013,]

juveniles<-juveniles[-c(47,50,51,54,55),]
all(paste(seedlings.meta$turfID, seedlings.meta$Year, sep="_")==rownames(juveniles))
#Should be TRUE

#Calculating number of seedlings
no.seedlings<-matrix(nrow=nrow(seedlings),ncol=1)
for(i in 1:nrow(seedlings)){
  no.seedlings[i]<-sum(seedlings[i,])
  
}

#Calculating number of juveniles 
no.juveniles<-matrix(nrow=nrow(juveniles),ncol=1)
for(i in 1:nrow(juveniles)){
  no.juveniles[i]<-sum(juveniles[i,])
  
}

#Combining the two
recruitment<-cbind(no.seedlings,no.juveniles)

colnames(recruitment)<-c("seedlings", "juveniles")

recruitment<-as.data.frame(recruitment)

recruitment.data<-cbind(seedlings.meta, recruitment)
head(recruitment.data)
