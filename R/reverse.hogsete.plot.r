#'Plots NMDS of transpants, origin and destinations
#'@param site which site
#'@param dat data to use - cover, subturf or presence absence
#'@param ord ordination methods to use  - should work with any from the vegan package.
#'@param \dots other arguments to ord (cannot think of anything useful) 
#'@details This plot tends to be rather ugly.
#'@import vegan
#'@export

reverse.hogsete.plot<-function(site,dat=cover,ord=metaMDS, ...){
  #collect data for site and its transplants and their target controls
  origTT1<-which(cover.meta$siteID==site&cover.meta$TTtreat=="TT1")
  TT2<-which(cover.meta$siteID==site&cover.meta$TTtreat=="TT2")
  TT3<-which(cover.meta$siteID==site&cover.meta$TTtreat=="TT3")
  TT4<-which(cover.meta$siteID==site&cover.meta$TTtreat=="TT4")
  
  #find destination sites
  TT2site<-cover.meta$siteID[as.character(cover.meta$blockID)==as.character(cover.meta$destBlockID)[which(cover.meta$siteID==site&cover.meta$TTtreat=="TT2")[1]]][1]
  TT3site<-cover.meta$siteID[as.character(cover.meta$blockID)==as.character(cover.meta$destBlockID)[which(cover.meta$siteID==site&cover.meta$TTtreat=="TT3")[1]]][1]
  TT4site<-cover.meta$siteID[as.character(cover.meta$blockID)==as.character(cover.meta$destBlockID)[which(cover.meta$siteID==site&cover.meta$TTtreat=="TT4")[1]]][1]
  
  #get destination turfs
  TT2dest<-which(cover.meta$siteID==TT2site&cover.meta$TTtreat=="TT1")
  TT3dest<-which(cover.meta$siteID==TT3site&cover.meta$TTtreat=="TT1")
  TT4dest<-which(cover.meta$siteID==TT4site&cover.meta$TTtreat=="TT1")
  
  keep<-c(origTT1, TT2, TT3, TT4, TT2dest, TT3dest, TT4dest)
  
  dat<-dat[keep,propertaxa]
  
  #run ord
  mod<-ord(dat, ...)
  
  #rotate? so temperature on Y axis 
  #plot analysis   #expect missing values
  plot(mod, display="sites",type="n")
  mapply(function(TT, col){plotsubset(mod=mod,keep=keep,site=site, TTtreat=TT,col=col)}, TT=treats[-1], col=TT.colours[-1])
  
  #plotsubset(mod=mod,keep=keep,site=site, TTtreat="TT2",col="red" )
  #plotsubset(mod=mod,keep=keep,site=site, TTtreat="TT3",col="blue" )
  #plotsubset(mod=mod,keep=keep,site=site, TTtreat="TT4",col="purple" )

  mapply(function(site, col){plotsubset(mod=mod,keep=keep,site=site, TTtreat="TT1",col=col, pch="+")},site=list(TT2site, TT3site, TT4site), col=TT.colours[-(1:2)])
  
  #plotsubset(mod=mod,keep=keep,site=TT2site, TTtreat="TT1",col="red", pch="+" )
  #plotsubset(mod=mod,keep=keep,site=TT3site, TTtreat="TT1",col="blue", pch="+" )
  #plotsubset(mod=mod,keep=keep,site=TT4site, TTtreat="TT1",col="purple", pch="+" )  
}
