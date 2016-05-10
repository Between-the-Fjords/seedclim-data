#'Plots NMDS of transpants, origin and destinations
#'@param site which site
#'@param dat data to use - cover, subturf or presence absence
#'@param ord ordination methods to use  - should work with any from the vegan package.
#'@param \dots other arguments to ord (cannot think of anything useful) 
#'@details This plot tends to be less ugly than the reverse hogsete plot.
#'
#'@export

hogsete.plot<-function(site,dat=cover,ord=metaMDS, ...){
  #collect data for site and its transplants and their target controls
  destTT1<-which(cover.meta$siteID==site&cover.meta$TTtreat=="TT1")
  destTTC<-which(cover.meta$siteID==site&cover.meta$TTtreat=="TTC")
  
  TT2<-which(cover.meta$destSiteID==site&cover.meta$TTtreat=="TT2")
  TT3<-which(cover.meta$destSiteID==site&cover.meta$TTtreat=="TT3")
  TT4<-which(cover.meta$destSiteID==site&cover.meta$TTtreat=="TT4")
  
  keep<-c(destTTC,destTT1, TT2, TT3, TT4)
  
  dat<-dat[keep,propertaxa]
  
  #run ord
  mod<-ord(dat, ...)
  
  #rotate? so temperature on Y axis 
  #plot analysis   #expect missing values
  plot(mod, display="sites",type="n")
  mapply(function(TT, col){plotsubset(mod=mod,keep=keep, TTtreat=TT,col=col)}, TT=treats, col=TT.colours)
  return(mod)
}
