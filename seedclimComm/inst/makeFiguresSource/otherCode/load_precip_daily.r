precipQ<-"SELECT dat_prec_daily.*, sites.Temperature_level, sites.Precipitation_level
FROM sites INNER JOIN dat_prec_daily ON sites.siteID = dat_prec_daily.siteID;"

precip<-sqlQuery(con,precipQ)
head(precip)

precip$date<-with(precip,as.POSIXct(strptime(paste(AAR7,"-",MND7,"-",DAG7," 12:00:00", sep=""),"%Y-%m-%d %H:%M:%S"))) 
precip$AAR7<-NULL
precip$MND7<-NULL
precip$DAG7<-NULL       
precip$Date<-NULL

precip<-precip[order(precip$siteID,precip$date),]

by(precip,precip$siteID, function(x){
  x11()
  plot(x$date, x$"prec_gridded",type="n", main=x$siteID[1], ylab="precipitation", ylim=range(cbind(x$prec_gridded, x$prec_predGap), na.rm=TRUE))  
  matlines(x$date, cbind(x$prec_gridded, x$prec_predGap), col=1:2, lty=1)
  legend("topleft", legend=c("prec_gridded", "prec_predGap"), lty=1, col=1:2)
})

by(precip,precip$siteID, function(x){
  x11()
  plot((sort(x$date)))
})
  
  

