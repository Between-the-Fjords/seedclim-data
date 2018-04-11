
plotsubset<-function(mod,keep,site, TTtreat, col=1, pch=20){
  nosite<-missing(site)
  TT<-cover.meta$TTtreat[keep]
  year<-cover.meta$Year[keep]
  blockID<-cover.meta$blockID[keep]
  siteID<-cover.meta$siteID[keep]
  plevel<-cover.meta$Precipitation_level[keep]
  tlevel<-cover.meta$Temperature_level[keep]
  if(!nosite&&is.na(site))return()
  if(nosite){
    select=TT==TTtreat
  }else{
    select=TT==TTtreat&siteID==site
  }
  points(mod, choices=1:2,display="sites",select=select, col=col, pch=pch)
  sapply(unique(blockID), function(bID){
    if(nosite){k=TT==TTtreat&blockID==bID}
    else{k=TT==TTtreat&siteID==site&blockID==bID}
    
    if(sum(k)>0){
      points(mod, choices=1:2,display="sites",select=k, col=col, type="l")
      points(mod, choices=1:2,display="sites",select=k&year==2009, bg=ppt.colours[plevel[k&year==2009]], type="p", pch=tmp.pch[tlevel[k&year==2009]])
    }
  })

}
