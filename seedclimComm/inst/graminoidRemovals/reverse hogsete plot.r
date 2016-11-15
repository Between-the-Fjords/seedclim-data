#Hogesete plots



#reverse hogsete plot

rhp<-function(site,dat=cover,ord=metaMDS, ...){
  #collect data for site and its transplants and their target controls
  origTT1<-which(cover.meta$siteID==site&cover.meta$TTtreat=="TT1")
  TT2<-which(cover.meta$siteID==site&cover.meta$TTtreat=="TT2")
  TT3<-which(cover.meta$siteID==site&cover.meta$TTtreat=="TT3")
  TT4<-which(cover.meta$siteID==site&cover.meta$TTtreat=="TT4")

    #find destination sites
  TT2site<-cover.meta$siteID[as.character(cover.meta$blockID)==as.character(cover.meta$destBlockID)[which(cover.meta$siteID==site&cover.meta$TTtreat=="TT2")[1]]][1]
  TT3site<-cover.meta$siteID[as.character(cover.meta$blockID)==as.character(cover.meta$destBlockID)[which(cover.meta$siteID==site&cover.meta$TTtreat=="TT3")[1]]][1]
  TT4site<-cover.meta$siteID[as.character(cover.meta$blockID)==as.character(cover.meta$destBlockID)[which(cover.meta$siteID==site&cover.meta$TTtreat=="TT4")[1]]][1]
  print(TT2site)
  print(TT3site)
  print(TT4site)

    #get destination turfs

  TT2dest<-which(cover.meta$siteID==TT2site&cover.meta$TTtreat=="TT1")
  TT3dest<-which(cover.meta$siteID==TT3site&cover.meta$TTtreat=="TT1")
  TT4dest<-which(cover.meta$siteID==TT4site&cover.meta$TTtreat=="TT1")
  
  keep<-c(origTT1, TT2, TT3, TT4, TT2dest, TT3dest, TT4dest)
 # keep<-c(origTT1, TT2, TT3, TT4)
  TT<-cover.meta$TTtreat[keep]
  year<-cover.meta$Year[keep]
  blockID<-cover.meta$blockID[keep]
  siteID<-cover.meta$siteID[keep]
  
  
  dat<-dat[keep,propertaxa]
  
  #run ord
  mod<-ord(dat, ...)
  
  #rotate? so temperature on Y axis 
  #plot analysis   #expect missing values
  plot(mod, display="sites",type="n")
  
 plotsubset<-function(site, TTtreat, col=1, pch=20){
 if(is.na(site))return()
  points(mod, choices=1:2,display="sites",select=TT==TTtreat&siteID==site, col=col, pch=pch)
  sapply(unique(blockID), function(bID){
    k=TT==TTtreat&siteID==site&blockID==bID
    if(sum(k)>0){
      points(mod, choices=1:2,display="sites",select=k, col=col, type="l")
      points(mod, choices=1:2,display="sites",select=k&year==2009, col=col, type="p", pch=pch, cex=1.5)
    }
  })
  }

  plotsubset(site=site, TTtreat="TT1",col="grey70" )
  plotsubset(site=site, TTtreat="TT2",col="red" )
  plotsubset(site=site, TTtreat="TT3",col="blue" )
  plotsubset(site=site, TTtreat="TT4",col="purple" )

  plotsubset(site=TT2site, TTtreat="TT1",col="red", pch="+" )
  plotsubset(site=TT3site, TTtreat="TT1",col="blue", pch="+" )
  plotsubset(site=TT4site, TTtreat="TT1",col="purple", pch="+" )
  
}



# hogsete plot

hp<-function(site,dat=cover,ord=metaMDS, ...){
  #collect data for site and its transplants and their target controls
  destTT1<-which(cover.meta$siteID==site&cover.meta$TTtreat=="TT1")
  destTTC<-which(cover.meta$siteID==site&cover.meta$TTtreat=="TTC")

  TT2<-which(cover.meta$destSiteID==site&cover.meta$TTtreat=="TT2")
  TT3<-which(cover.meta$destSiteID==site&cover.meta$TTtreat=="TT3")
  TT4<-which(cover.meta$destSiteID==site&cover.meta$TTtreat=="TT4")
  
  keep<-c(destTTC,destTT1, TT2, TT3, TT4)
  TT<-cover.meta$TTtreat[keep]
  year<-cover.meta$Year[keep]
  blockID<-cover.meta$blockID[keep]
  siteID<-cover.meta$siteID[keep]
  
  
  dat<-dat[keep,propertaxa]
  
  #run ord
  mod<-ord(dat, ...)
  
  #rotate? so temperature on Y axis 
  #plot analysis   #expect missing values
  plot(mod, display="sites",type="n")
  
 plotsubset<-function(TTtreat, col=1, pch=20){
  points(mod, choices=1:2,display="sites",select=TT==TTtreat, col=col, pch=pch)
  sapply(unique(blockID), function(bID){
    k=TT==TTtreat&blockID==bID
    if(sum(k)>0){
      points(mod, choices=1:2,display="sites",select=k, col=col, type="l")
      points(mod, choices=1:2,display="sites",select=k&year==2009, col=col, type="p", pch=pch, cex=1.5)
    }
  })
  }

  plotsubset(TTtreat="TTC",col="black", pch=20 )
  plotsubset(TTtreat="TT1",col="grey70", pch=20 )
  plotsubset(TTtreat="TT2",col="red" )
  plotsubset(TTtreat="TT3",col="blue" )
  plotsubset(TTtreat="TT4",col="purple" )
  
}



Q
x11();rhp("Hogsete", dat=cover>0)
x11();
par(mar=c(3,3,1,1), mgp=c(1.5,.5,0))
hp("Hogsete", dat=fsubturf)

sapply(levels(cover.meta$siteID), function(siteID){
  x11();
  rhp(siteID, dat=fsubturf)
  title(main=siteID)
})
                                  
sapply(levels(cover.meta$siteID), function(siteID){
  x11();
  hp(siteID, dat=fsubturf, ord=rda)
  title(main=siteID)
})
