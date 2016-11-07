#Search for curiosities in the turf and subturf data
#Functions (line 1 - 90) 
#Examples at end of file




#turf
#find species where abs(cover change) >thresh between years
getturfcover<-function(firstyear, lastyear){
  spp<-dbGetQuery(con, paste('SELECT sites.siteID, turfs.turfID, turfs.TTtreat, species, Year, cover FROM ((sites INNER JOIN (blocks INNER JOIN plots ON blocks.blockID = plots.blockID) ON sites.siteID = blocks.siteID) INNER JOIN turfs ON plots.plotID = turfs.destinationPlotID) INNER JOIN new_TurfCommunity ON turfs.turfID = new_TurfCommunity.turfID  WHERE (Year = ",firstyear," OR Year=",lastyear,");', sep=""))

    year1<-xtabs(cover~turfID+species, data=spp[spp$Year==firstyear,])
    year2<-xtabs(cover~turfID+species, data=spp[spp$Year==lastyear,])
    year1<-as.data.frame(unclass(year1))
    year2<-as.data.frame(unclass(year2))
    res<-list(year1, year2)
    names(res)<-paste("year", c(firstyear, lastyear), sep="")
    class(res)<-"turfs"
    res
}

plot.turfs<-function(x, log=FALSE){
  if(log)
  sunflowerplot(unlist(x[[1]])+1, unlist(x[[2]])+1, xlab=names(x)[1], ylab=names(x)[2], log="xy")
  else
  sunflowerplot(unlist(x[[1]]), unlist(x[[2]]), xlab=names(x)[1], ylab=names(x)[2])

}
identify.turfs<-function(x){
  p<-identify(unlist(x[[1]]), unlist(x[[2]]))
  turfs<-rep(rownames(x[[1]]), ncol(x[[1]]))[p]
  species<-rep(colnames(x[[1]]), each=nrow(x[[1]]))[p]
  cbind(turfs=turfs, species=species)
}


curious<-function(x, threshold=10, margin=1){
  xx<-(x[[1]]-x[[2]])
  res<-apply(xx, margin,function(z)data.frame(name=dimnames(xx)[[margin*-1+3]],difference=z)[which(abs(z)>threshold),,drop=FALSE])
  res[sapply(res,nrow)==0]<-NULL
  res
}


#subplot
#find species where abs(no subplots occupied) >thresh between years

getsubturfcount<-function(firstyear, lastyear, site){
  spp<-dbGetQuery(con, paste('SELECT sites.siteID, turfs.turfID, turfs.TTtreat, species, Year, Count(subturf) as nsubturf FROM ((sites INNER JOIN (blocks INNER JOIN plots ON blocks.blockID = plots.blockID) ON sites.siteID = blocks.siteID) INNER JOIN turfs ON plots.plotID = turfs.destinationPlotID) INNER JOIN newSubTurfCommunity ON turfs.turfID = newSubTurfCommunity.turfID  GROUP BY sites.siteID, turfs.turfID, turfs.TTtreat, species, Year HAVING (Year=",firstyear," OR Year=",lastyear,");', sep=""))

  print(names(spp))
if(missing(site))keep<-TRUE
else keep<-spp$siteID==site
    year1<-xtabs(nsubturf~turfID+species, data=spp[spp$Year==firstyear&keep,])
    year2<-xtabs(nsubturf~turfID+species, data=spp[spp$Year==lastyear&keep,])
    year1<-as.data.frame(unclass(year1))
    year2<-as.data.frame(unclass(year2))
    res<-list(year1, year2)
    names(res)<-paste("year", c(firstyear, lastyear), sep="")
    class(res)<-"subturfcount"    
    class(res)<-"turfs"
    res
}





#subplotmap
getsubturfspecies<-function(turf, year, species){
  subturfsp<-dbGetQuery(con, paste('SELECT sites.siteID, turfs.turfID, turfs.TTtreat, species, Year, subturf, seedlings, juvenile,adult, fertile, vegetative, dominant, cf
    FROM ((sites INNER JOIN (blocks INNER JOIN plots ON blocks.blockID = plots.blockID) ON sites.siteID = blocks.siteID) INNER JOIN turfs ON plots.plotID = turfs.destinationPlotID) INNER JOIN newSubTurfCommunity ON turfs.turfID = newSubTurfCommunity.turfID where Year=",year," AND turfs.turfID='",turf,"' AND species='",species,"' ORDER BY subturf ASC;', sep=""))
    class(subturfsp)<-c("subturfsp","data.frame")
    subturfsp
}

plot.subturfsp<-function(x){
  plot(NA, xlim=c(0,1), ylim=c(0,1), ann=F, axes=FALSE, xaxs="i", yaxs="i")
  abline(v=seq(0,1,.2), h=seq(0,1,.2))
  text(as.matrix(expand.grid(seq(.1,.9,.2), seq(.9,.1,-.2)+.05)), labels=1:25, col="grey60")
  box()
  with(x,{
    title(main=paste(siteID[1]," ", turfID[1], " ",Year[1], " ",species[1]))
    lab=rep("",nrow(x))
    lab<-paste(lab,ifelse(seedlings>0, paste("Sx", seedlings, sep=""),""), sep="")
    lab<-paste(lab,ifelse(juvenile>0, paste(" Jx",juvenile,sep=""), ""), sep="")
    lab<-paste(lab,ifelse(adult==1, " A", ""), sep="")
    lab<-paste(lab,ifelse(fertile==1, " F", ""), sep="")
    lab<-paste(lab,ifelse(vegetative==1, " V", ""), sep="")
    lab<-paste(lab,ifelse(dominant==1, " D", ""), sep="")
    lab<-paste(lab,ifelse(cf==1, " cf", ""), sep="")
    labels=rep("",25)
    labels[subturf]<-lab
    
    text(as.matrix(expand.grid(seq(.1,.9,.2), seq(.9,.1,-.2)-.05)), labels=labels) 
  })
}


########################################################################
#Examples

#connect to database

db<-file.choose()#uncomment this line and select database in dialog box
db<-"o:\\data\\seedclim2014\\seedclim_2014-2-25.mdb"#edit this line with correct location
con<-odbcConnectAccess2007(db)
sqlTables(con)




cover1213<-getturfcover(2015,2016)
x11()
plot(cover1213)#plot of cover in last year against first year
identify(cover1213)#click points of interest, then "stop" button button at top of figure. NB will only find first of overlaying points.
curious(cover1213, threshold=40, margin=1) #margin=1 to group by turfs, 2 to group by species



counts1213<-getsubturfcount(2012,2013)
plot(counts1213)
identify(counts1213)
curious(counts1213, threshold=20, margin=1)#margin=1 to group by turfs, 2 to group by species

sapply(c("Ulvhaugen","Lavisdalen","Gudmedalen","Skjellingahaugen","Alrust","Hogsete","Rambera","Veskre","Fauske","Vikesland","Arhelleren","Ovstedal"), function(n){
x11()
  counts1213<-getsubturfcount(2012,2013, n)
  plot(counts1213)
  title(main=n)
  })



getsubturfspecies(turf="92 tt1 93", year=2012, species="Car.nig")
x11();plot(getsubturfspecies(turf="92 tt1 93", year=2012, species="Car.nig"))

getsubturfspecies(turf="Fau3RTC", year=2013, species="Tri.sp")
x11();plot(getsubturfspecies(turf="73 TTC", year=2013, species="Alc.sp"))




counts1112<-getsubturfcount(2011,2012)
curious(counts1112, threshold=10, margin=1)#margin=1 to group by turfs, 2 to group by species

close(con)