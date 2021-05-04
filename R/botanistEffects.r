Anonymous botanists: 1 TT2 28 probably Kari


#botanist effect on cover
#1 boxplot sumcover~botanist+year/climate/treatment

#need cover and cover.meta already loaded

library(RODBC)

#connect to database

#db<-"o:\\data\\seedclim2014\\seedclim_2014-2-25.mdb"#edit this line with correct location
db<-"o:\\data\\seedclim2014\\seedclim_2014-3-6.mdb"#edit this line with correct location

con<-odbcConnectAccess2007(db)
sqlTables(con)



#botanist effect on sum cover
query<-"SELECT sites.siteID, turfs.turfID, turfs.TTtreat, turfEnvironment.recorder, Sum(new_TurfCommunity.cover) AS SumOfcover, new_TurfCommunity.Year, sites.Temperature_level, sites.Precipitation_level
FROM ((((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.destinationPlotID) INNER JOIN new_TurfCommunity ON turfs.turfID = new_TurfCommunity.turfID) INNER JOIN turfEnvironment ON (turfEnvironment.year = new_TurfCommunity.Year) AND (turfs.turfID = turfEnvironment.turfID)
GROUP BY sites.siteID, turfs.turfID, turfs.TTtreat, turfEnvironment.recorder, new_TurfCommunity.Year, sites.Temperature_level, sites.Precipitation_level
HAVING (((turfs.TTtreat) Is Not Null));"


coverBotanist<-sqlQuery(con,query)

head(coverBotanist)

coverBotanist<-coverBotanist[coverBotanist$TTtreat!="",]
coverBotanist$recorder<-as.factor(as.character(coverBotanist$recorder))

table(coverBotanist$recorder[coverBotanist$Year>2009])
table(coverBotanist$recorder[coverBotanist$Year>2])
table(coverBotanist[coverBotanist$Year>2009, c(4,7)])
x11(6,6)
boxplot(SumOfcover~recorder, data=coverBotanist[coverBotanist$Year>2009&coverBotanist$Temp>0,], notch=TRUE, varwidth=TRUE, las=2, ylab="Sum of Cover")

##botanist effect on total cover

query3<-"SELECT sites.siteID, turfs.turfID, turfs.TTtreat, new_TurfCommunity.Year, Sum(new_TurfCommunity.cover) AS SumOfcover, turfEnvironment.totalVascular, turfEnvironment.recorder, sites.Temperature_level
FROM (sites INNER JOIN ((blocks INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.destinationPlotID) ON sites.siteID = blocks.siteID) INNER JOIN (new_TurfCommunity INNER JOIN turfEnvironment ON new_TurfCommunity.Year = turfEnvironment.year) ON (turfs.turfID = turfEnvironment.turfID) AND (turfs.turfID = new_TurfCommunity.turfID)
WHERE ((Not (turfs.tttreat) =''))
GROUP BY sites.siteID, turfs.turfID, turfs.TTtreat, new_TurfCommunity.Year, turfEnvironment.totalVascular, turfEnvironment.recorder, sites.Temperature_level;"
totCov<-sqlQuery(con,query3)
totCov$recorder<-as.factor(as.character(totCov$recorder))
table(totCov$rec)
head(totCov)

x11(6,6)
boxplot(totalVascular~recorder, data=totCov, notch=TRUE, varwidth=TRUE, las=2, ylab="Total Vascular Cover")

x11()
plot(totCov$totalVascular, totCov$SumOfcover,xlab="Total Vascular Cover", ylab="Sum Vascular Cover")
abline(0,1)

x11();par(mfrow=c(3,3), mar=c(3,3,1,1), mgp=c(1.5,.5,0))
by(totCov, totCov$recorder,function(x) {
  plot(x$totalVascular[x$Temperature>0], x$SumOfcover[x$Temperature>0], main=x$recorder[1], xlim=range(totCov$totalVascular, na.rm=TRUE), ylim=range(totCov$SumOfcover), col=c(1,2,3,4)[factor(x$Year, levels=c(2009, 2011, 2012, 2013))], xlab="Total Vascular Cover", ylab="Sum Vascular Cover")
  abline(0,1)
})


x11();par(mfrow=c(3,3), mar=c(3,3,1,1), mgp=c(1.5,.5,0))
by(totCov, totCov$recorder,function(x) {
  plot(x$totalVascular[x$Temperature>0], x$SumOfcover[x$Temperature>0], main=x$recorder[1], xlim=range(totCov$totalVascular, na.rm=TRUE), ylim=range(totCov$SumOfcover), col=c(1,2,3,4)[factor(x$Year, levels=c(2009, 2011, 2012, 2013))], xlab="Total Vascular Cover", ylab="Sum Vascular Cover")
  abline(0,1)
})


tapply(totCov$totalVascular==totCov$SumOfcover, list(totCov$recorder, totCov$Year), mean, na.rm=T)*100
(ratio<-tapply(totCov$SumOfcover/totCov$totalVascular, list(totCov$recorder, totCov$Year), mean, na.rm=T)*100)
ratio[6,]<-ratio[6,]*1.3
ratio[7,]<-ratio[7,]*1.25
ratio

#after corrections in load cover.r
totCov<-totCov[!(totCov$turfID=="521 TT1 523"&totCov$Year==2012),]#remove over stomped plot
head(totCov)
head(cover.meta)
totCov<-totCov[order(paste(totCov$turfID, totCov$Year)),]
identical(as.character(totCov$turfID), as.character(cover.meta$turfID))

x11(5,5)
plot(totCov$SumOfcover, rowSums(cover), xlab="Raw", ylab="Corrected Sum of Covers")

totCov$SumOfcover2<-rowSums(cover)
x11(5,5)
boxplot(SumOfcover2~recorder, data=totCov[totCov$Year>2009&totCov$Temp>0,], notch=TRUE, varwidth=TRUE, las=2, ylab="Sum of Cover")

x11();par(mfrow=c(3,3), mar=c(3,3,1,1), mgp=c(1.5,.5,0))
by(totCov, totCov$recorder,function(x) {
  plot(x$totalVascular[x$Temperature>0], x$SumOfcover2[x$Temperature>0], main=x$recorder[1], xlim=range(totCov$totalVascular, na.rm=TRUE), ylim=range(totCov$SumOfcover), col=c(1,2,3,4)[factor(x$Year, levels=c(2009, 2011, 2012, 2013))], xlab="Total Vascular Cover", ylab="corrected Sum Vascular Cover")
  abline(0,1)
})


#siri time
siriQ<-"SELECT turfs.turfID, new_TurfCommunity.Year, turfEnvironment.date, Sum(new_TurfCommunity.cover) AS SumOfcover, turfEnvironment.totalVascular, turfs.TTtreat, sites.Temperature_level
FROM ((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN ((turfs INNER JOIN turfEnvironment ON turfs.turfID = turfEnvironment.turfID) INNER JOIN new_TurfCommunity ON (new_TurfCommunity.Year = turfEnvironment.year) AND (turfs.turfID = new_TurfCommunity.turfID)) ON plots.plotID = turfs.destinationPlotID
WHERE (((turfEnvironment.recorder)='Siri'))
GROUP BY turfs.turfID, new_TurfCommunity.Year, turfEnvironment.date, turfEnvironment.totalVascular, turfs.TTtreat, sites.Temperature_level
HAVING ((Not (turfs.TTtreat)=''))
ORDER BY new_TurfCommunity.Year, turfEnvironment.date, Sum(new_TurfCommunity.cover) DESC;"

siri<-sqlQuery(con,siriQ)
siri$Date<-as.Date(paste(siri$date,siri$Year,sep="-"), format="%d-%b-%Y")
plot(siri$Date,siri$SumOfcover/siri$totalV, col=(1:3)[siri$Temp])

#botanist effect on cover/nsubturfs  #NB slow 
coverSubturfs<-lapply(c(2009,2011,2012,2013),function(year){ print(year)
  query2<-paste("SELECT sites.siteID, turfs.turfID, turfs.TTtreat, turfEnvironment.recorder, new_TurfCommunity.Year, new_TurfCommunity.species, First(new_TurfCommunity.cover) AS FirstOfcover, Count(newSubTurfCommunity.species) AS CountOfspecies
FROM (sites INNER JOIN ((blocks INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.destinationPlotID) ON sites.siteID = blocks.siteID) INNER JOIN ((new_TurfCommunity INNER JOIN turfEnvironment ON new_TurfCommunity.Year = turfEnvironment.year) 
INNER JOIN newSubTurfCommunity ON (new_TurfCommunity.Year = newSubTurfCommunity.Year) AND (new_TurfCommunity.species = newSubTurfCommunity.species)) ON (turfs.turfID = turfEnvironment.turfID) AND (turfs.turfID = newSubTurfCommunity.turfID) AND (turfs.turfID = new_TurfCommunity.turfID)
WHERE (((new_TurfCommunity.year)=",year,") AND ((new_TurfCommunity.Year)=",year,") AND ((turfEnvironment.year)=",year,"))
GROUP BY sites.siteID, turfs.turfID, turfs.TTtreat, turfEnvironment.recorder, new_TurfCommunity.Year, new_TurfCommunity.species
HAVING (((turfs.TTtreat) Is Not Null));")
  coverSubturfs<-sqlQuery(con,query2)
  coverSubturfs
})


coverSubturfs<-rbind(coverSubturfs[[1]],coverSubturfs[[2]],coverSubturfs[[3]],coverSubturfs[[4]])
coverSubturfs<-coverSubturfs[coverSubturfs$TTtreat!="",]

head(coverSubturfs)

plot(coverSubturfs$CountOfspecies,coverSubturfs$FirstOfcover,cex=.5)
abline(0,4)
mod<-lm(FirstOfcover~CountOfspecies, data=coverSubturfs)
summary(mod)
abline(mod)

table(coverSubturfs$rec)
x11()
plot(coverSubturfs$CountOfspecies,coverSubturfs$FirstOfcover,cex=.5, xlab="# subturfs", ylab="cover")
abline(0,4)

library(mgcv)
mapply( function(n, col){
  mod<-gam(FirstOfcover~s(CountOfspecies), data=coverSubturfs, subset=recorder==n)
  lines(0:25,predict(mod, newdata=data.frame(CountOfspecies=0:25)), col=col, lwd=2)
}, n=unique(coverSubturfs$rec),col=1:8)
legend("topleft", legend=unique(coverSubturfs$rec), col=1:8, lty=1, lwd=2)


#after corrections in load cover.r
coverSubturfs<-coverSubturfs[!(coverSubturfs$turfID=="521 TT1 523"&coverSubturfs$Year==2012),]#remove over stomped plot

coverSubturfs<-coverSubturfs[order(paste(coverSubturfs$turfID,coverSubturfs$Year, coverSubturfs$species )),]
cover.thin<-cover.thin[order(paste(cover.thin$turfID,cover.thin$Year, cover.thin$species )),]
cover.thin<-cover.thin[paste(cover.thin$turfID,cover.thin$Year, cover.thin$species )%in%paste(coverSubturfs$turfID,coverSubturfs$Year, coverSubturfs$species ),]

identical(paste(coverSubturfs$turfID,coverSubturfs$Year, coverSubturfs$species ),paste(cover.thin$turfID,cover.thin$Year, cover.thin$species ) )


x11()
plot(coverSubturfs$CountOfspecies,cover.thin$cover,cex=.5, xlab="# subturfs", ylab="cover")
abline(0,4)

library(mgcv)
mapply( function(n, col){
  mod<-gam(c2~s(CountOfspecies), data=cbind(coverSubturfs, c2=cover.thin$cover), subset=recorder==n)
  lines(0:25,predict(mod, newdata=data.frame(CountOfspecies=0:25)), col=col, lwd=2)
}, n=unique(coverSubturfs$rec),col=1:8)
legend("topleft", legend=unique(coverSubturfs$rec), col=1:8, lty=1, lwd=2)


##botanist richness
query4<-"SELECT sites.siteID, turfs.turfID, turfs.TTtreat, turfEnvironment.recorder, count(new_TurfCommunity.species) AS richness, new_TurfCommunity.Year, sites.Temperature_level, sites.Precipitation_level
FROM ((((sites INNER JOIN blocks ON sites.siteID = blocks.siteID) INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.destinationPlotID) INNER JOIN new_TurfCommunity ON turfs.turfID = new_TurfCommunity.turfID) INNER JOIN turfEnvironment ON (turfEnvironment.year = new_TurfCommunity.Year) AND (turfs.turfID = turfEnvironment.turfID)
GROUP BY sites.siteID, turfs.turfID, turfs.TTtreat, turfEnvironment.recorder, new_TurfCommunity.Year, sites.Temperature_level, sites.Precipitation_level
HAVING (((turfs.TTtreat) Is Not Null));"


richnessBotanist<-sqlQuery(con,query4)

head(richnessBotanist)

richnessBotanist<-richnessBotanist[richnessBotanist$TTtreat!="",]
richnessBotanist$recorder<-as.factor(as.character(richnessBotanist$recorder))


x11(6,6)
boxplot(richness~recorder, data=richnessBotanist[richnessBotanist$Year>2009&richnessBotanist$Temp==2&richnessBotanist$TTtreat%in%c("TTC", "TT1"),], notch=TRUE, varwidth=TRUE, las=2, ylab="Richness")

