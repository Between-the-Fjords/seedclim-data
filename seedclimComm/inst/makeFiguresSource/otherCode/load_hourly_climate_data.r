climQ<-"SELECT dat_clim_hourly.*, sites.Temperature_level, sites.Precipitation_level
FROM sites INNER JOIN dat_clim_hourly ON sites.siteID = dat_clim_hourly.siteID;"

clim<-sqlQuery(con,climQ)
head(clim)

clim$date<-with(clim,as.POSIXct(strptime(paste(year,"-",month,"-",day," ",time,":00:00", sep=""),"%Y-%m-%d %H:%M:%S"))) 
clim$year<-NULL
clim$month<-NULL
clim$day<-NULL
clim$time<-NULL

clim<-clim[order(clim$siteID,clim$date),]

by(clim,clim$siteID, function(x){
  x11()
  plot(x$date, x$"2mTemp",type="n", main=x$siteID[1], ylab="temperature", ylim=range(cbind(clim$"2mTemp", clim$"30cmTemp", clim$GroundTemp, clim$SoilTemp), na.rm=TRUE))  
  matlines(x$date, cbind(x$"2mTemp", x$"30cmTemp", x$GroundTemp, x$SoilTemp), col=1:4, lty=1)
  legend("topleft", legend=c("2m", "30cm", "Ground", "Soil"), lty=1, col=1:4)
  x11()
  plot(x$date, x$moist1, col=1, lty=1, type="l",main=x$siteID[1], ylab="moisture", ylim=c(0,max(max(clim$moist1, na.rm=TRUE), max(clim$moist2, na.rm=TRUE))))
  lines(x$date, x$moist2, col=2)
  legend("topleft", legend=c("moist1", "moist2"), lty=1, col=1:4)
})

by(clim,clim$siteID, function(x){
  x11()
  plot(diff(sort(x$date)))
})
  
  

