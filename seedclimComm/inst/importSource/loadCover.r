###############################################
##cover data
cover.thin <- dbGetQuery(con, paste("SELECT sites.siteID, blocks.blockID, turfs.TTtreat,turfs.turfID, dest_blocks.blockID AS destBlockID, (SELECT Count(subTurfEnvironment.bad) AS CountOfbad
FROM subTurfEnvironment where (subTurfEnvironment.year = turfCommunity.year) AND (subTurfEnvironment.turfID = turfCommunity.turfID)
 AND ( (subTurfEnvironment.bad)='')) AS notbad, sites.Temperature_level, sites.Summertemperature_gridded as summerTemperature, sites.Annualprecipitation_gridded as annualPrecipitation, sites.Precipitation_level, turfCommunity.Year, turfCommunity.species, turfCommunity.cover, turfEnvironment.recorder , dest_blocks.siteID as destSiteID
FROM (((blocks AS dest_blocks INNER JOIN plots AS dest_plots ON dest_blocks.blockID = dest_plots.blockID) INNER JOIN (((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) 
INNER JOIN turfs ON plots.plotID = turfs.originPlotID) ON dest_plots.plotID = turfs.destinationPlotID) INNER JOIN turfCommunity ON turfs.turfID = turfCommunity.turfID) INNER JOIN turfEnvironment ON (turfEnvironment.year = turfCommunity.Year) AND (turfs.turfID = turfEnvironment.turfID)
WHERE NOT turfs.TTtreat='' AND ((Not (turfCommunity.Year)=2010));"))

       
head(cover.thin)
                                       
#correct for stomping
table(cover.thin$notbad)
stompingQ<-"SELECT blocks.siteID, blocks.blockID, turfs.turfID, subTurfEnvironment.year, turfs.TTtreat, Count(subTurfEnvironment.bad) AS CountOfbad
FROM blocks INNER JOIN (plots INNER JOIN (turfs INNER JOIN subTurfEnvironment ON turfs.turfID = subTurfEnvironment.turfID) ON plots.plotID = turfs.destinationPlotID) ON blocks.blockID = plots.blockID
GROUP BY blocks.siteID, blocks.blockID, turfs.turfID, subTurfEnvironment.year, turfs.TTtreat, subTurfEnvironment.bad
HAVING (((subTurfEnvironment.bad)='x'));"
dbGetQuery(con, stompingQ) 

 #delete turfs with too much stomping  
cover.thin <- cover.thin[cover.thin$notbad>10,]
sort(cover.thin$notbad, decreasing = TRUE)
                               
 #correct covers for stomping
max(cover.thin$cover)
cover.thin$cover <- cover.thin$cover*25/cover.thin$notbad
sort(cover.thin$cover, decreasing = TRUE)[1:10]      
cover.thin$cover[cover.thin$cover > 80] <- 80#stop doubtfully high values                                 

#correct for botanist effects
cover.thin$recorder[is.na(cover.thin$recorder)] <- "unknown botanist"
#PM
cover.thin$cover[cover.thin$recorder == "PM"] <- cover.thin$cover[cover.thin$recorder=="PM"]*1.20
#Siri
siri <- dbGetQuery(con, paste("SELECT turfs.turfID, new_TurfCommunity.Year, turfEnvironment.date, Sum(new_TurfCommunity.cover) AS SumOfcover, turfEnvironment.totalVascular, turfs.TTtreat, sites.Temperature_level
FROM ((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN ((turfs INNER JOIN turfEnvironment ON turfs.turfID = turfEnvironment.turfID) INNER JOIN new_TurfCommunity ON (new_TurfCommunity.Year = turfEnvironment.year) AND (turfs.turfID = new_TurfCommunity.turfID)) ON plots.plotID = turfs.destinationPlotID
WHERE (((turfEnvironment.recorder)='Siri'))
GROUP BY turfs.turfID, new_TurfCommunity.Year, turfEnvironment.date, turfEnvironment.totalVascular, turfs.TTtreat, sites.Temperature_level
HAVING ((Not (turfs.TTtreat)=''))
ORDER BY new_TurfCommunity.Year, turfEnvironment.date, Sum(new_TurfCommunity.cover) DESC;"))

siriLOW <- siri[siri$SumOfcover/siri$totalV < 1.35,]
siriLOW$turfID <- as.character(siriLOW$turfID)

siri.fix <- paste(as.character(cover.thin$turfID), cover.thin$Year) %in% paste(siriLOW$turfID, siriLOW$Year)

table(siri.fix,cover.thin$recorder)

cover.thin$cover[siri.fix] <- cover.thin$cover[siri.fix]*1.3



# make fat table

cover <- xtabs(cover ~ paste(turfID, Year, sep = "_") + species, data=cover.thin)
cover <- as.data.frame(unclass(cover))
dim(cover)



#make meta data
cover.meta <- unique(cover.thin[,c("siteID", "TTtreat", "Year", "blockID", "turfID","Temperature_level",  "Precipitation_level", "summerTemperature", "annualPrecipitation", "destBlockID", "recorder", "notbad", "destSiteID")])
cover.meta <- cover.meta[order(paste(cover.meta$turfID, cover.meta$Year)),]
cover.meta$TTtreat <- factor(as.character(cover.meta$TTtreat), levels=c("TTC","TT1", "TT2", "TT3", "TT4"))

turfs <- cover.meta[!duplicated(cover.meta$turfID),]
turfs <- turfs[order(turfs$turfID),]

#verify
all(paste(cover.meta$turfID, cover.meta$Year, sep = "_") == rownames(cover))

#clear up
rm(cover.thin, siri.fix, stompingQ, siriLOW, siri)
         

#John's corrections
cover['111 TT2 137_2011', 'Agr.cap'] <- 25
cover['32 TT3 109_2009', ] <- cover['32 TT3 109_2009', ] / 2
cover['32 TT3 109_2012', ] <- cover['32 TT3 109_2012', ] * 2 / 3
cover['33 TT2 58_2009', ] <- cover['33 TT2 58_2009', ] * 2 / 3
cover['34 TT1 32_2009', ] <- cover['34 TT1 32_2009', ] / 2
cover['40 TT2 62_2011', ] <- cover['40 TT2 62_2011', ] * 2 / 3 


#set NID.seedling to  0/1
#cover$NID.seedling <- ifelse(cover$NID.seedling > 0,1,0) # leave this out for the moment
         
table(cover.meta$turfID, cover.meta$Year)   
table(cover.meta$Year, cover.meta$siteID, cover.meta$TTtreat)         
                                       
alltaxa<-TRUE
propertaxa <- !names(cover) %in% c("NID.seedling", "Car.sp", "Hie.sp", "Luz.sp",  "NID.gram", "NID.herb", "NID.rosett", "Pyr.sp")
noNIDseedlings <- !names(cover) %in% c("NID.seedling")

turfs$newTT <- turfs$TTtreat  #alternative TTtreat with combined controls
levels(turfs$newTT)[1:2] <- "control"

