require(RODBC)
#load data

subturfQ<-"SELECT sites.siteID, blocks.blockID, turfs.TTtreat, turfs.turfID, newSubTurfCommunity.subTurf, newSubTurfCommunity.Year, newSubTurfCommunity.species, newSubTurfCommunity.species, dest_blocks.blockID AS destBlockID, subTurfEnvironment.bad, sites.Temperature_level, sites.Precipitation_level, turfEnvironment.recorder
FROM (((blocks AS dest_blocks INNER JOIN plots AS dest_plots ON dest_blocks.blockID = dest_plots.blockID) INNER JOIN (((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) ON dest_plots.plotID = turfs.destinationPlotID) 
INNER JOIN (newSubTurfCommunity INNER JOIN subTurfEnvironment ON (newSubTurfCommunity.Year = subTurfEnvironment.year) AND (newSubTurfCommunity.subTurf = subTurfEnvironment.subTurf)) ON (turfs.turfID = subTurfEnvironment.turfID) AND (turfs.turfID = newSubTurfCommunity.turfID)) INNER JOIN turfEnvironment ON (turfEnvironment.year = newSubTurfCommunity.Year) AND (turfs.turfID = turfEnvironment.turfID) 
WHERE ((Not (turfs.TTtreat)='')) AND ((Not (newSubTurfCommunity.Year)=2010));"
                                                                          
subturf.thin<-sqlQuery(con, subturfQ)                              
head(subturf.thin)
       
#fat       
subturf<-xtabs(rep(1, nrow(subturf.thin))~paste(turfID, subTurf, Year, sep="_")+species,         data=subturf.thin)
subturf<-as.data.frame(unclass(subturf))
dim(subturf)

subturf.meta<-unique(subturf.thin[,c("siteID", "TTtreat", "Year", "blockID", "turfID","subTurf","Temperature_level",  "Precipitation_level", "destBlockID", "bad", "recorder")])
subturf.meta<-subturf.meta[order(paste(subturf.meta$turfID, subturf.meta$subTurf, subturf.meta$Year, sep="_" )),]
subturf.meta$TTtreat<-factor(as.character(subturf.meta$TTtreat), levels=c("TTC","TT1", "TT2", "TT3", "TT4"))

all(paste(subturf.meta$turfID, subturf.meta$subTurf, subturf.meta$Year, sep="_")==rownames(subturf))


#stomping correction
x11(6,6)
boxplot(rowSums(subturf)~bad, data=subturf.meta, notch=TRUE, names=c("good", "stomped"))
boxplot(rowSums(subturf)~recorder+Temperature_level, data=subturf.meta, notch=T, las=2)

subturf.meta[subturf.meta$bad=="x",]
#impute data from previous year for damaged subplots


#aggregate to turf
fsubturf<-by(subturf, paste(subturf.meta$turfID, subturf.meta$Year, sep="_"), colSums)
fsubturf<-t(sapply(fsubturf,I))
fsubturf<-as.data.frame(fsubturf)

#meta data
#check - should == cover.meta
fsubturf<-fsubturf[rownames(fsubturf)%in%rownames(cover),]   #remove overstomped turf
all(paste(cover.meta$turfID, cover.meta$Year, sep="_")==rownames(fsubturf))   #cover.meta is correct meta file

boxplot(rowSums(fsubturf)~recorder+Temperature_level, data=cover.meta, notch=T, las=2)
boxplot(rowSums(fsubturf>0)~recorder+Temperature_level, data=cover.meta, notch=T, las=2)


#add Val.sam
fsubturf$Val.sam<-0
fsubturf<-fsubturf[,order(names(fsubturf))]

#cleanup
rm(subturfQ, subturf.thin)
rm(subturf)
