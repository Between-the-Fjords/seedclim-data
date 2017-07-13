#winner & loosers                                  
source("winlose functions.r")

W<-winlose("84 TT2 108", 2009, 2011, rare=0)

w9.11<-    lapply(turfs$turfID,function(tu){
      wl<-winlose(tu,2009, 2011)
    })

plotWL(w9.11)

w9.13<-    lapply(turfs$turfID,function(tu){
      wl<-winlose(tu,2009, 2013)
    })

plotWL(w9.13)



turfs$warm=factor(ifelse(turfs$TTtreat%in%c("TT2", "TT4" ), "warm", "notwarm"))
turfs$wet=factor(ifelse(turfs$TTtreat%in%c("TT3", "TT4" ), "wet", "notwet"))

mod0<-lm(sapply(w9.13,function(w)sapply(w, length))["extinct",]~1,data=turfs)
mod<-lm(sapply(w9.13,function(w)sapply(w, length))["extinct",]~Temperature_level*Precipitation_level,data=turfs, subset=TTtreat%in%c("TT1", "TTC"))
mod<-lm(sapply(w9.13,function(w)sapply(w, length))["extinct",]~Temperature_level+Precipitation_level,data=turfs)

summary(mod)
step(mod) 

summary(mod)


library(lme4)
turfrichness<-sapply(w9.13,function(x)x$rich)
#extinctions
ex11<-sapply(w9.11,function(w)sapply(w, length))["extinct",]
ex<-sapply(w9.13,function(w)sapply(w, length))["extinct",]
x11()
plot(ex11, ex, pch=16, col=turfs$TTtreat)
abline(lm(ex~ex11), col=2)
abline(0,1)
summary(lm(ex~ex11*newTT, data=turfs))

notex<-turfrichness-ex
exn<-cbind(ex, notex)
x11();plot(turfrichness, ex, col=turfs$TTtreat, pch=16)
abline(0,1)
cor.test(turfrichness, ex)
x11();plot(turfrichness, ex/turfrichness, col=turfs$TTtreat, pch=16)
cor.test(turfrichness, ex/turfrichness)

modglmm<-glmer(exn~Temperature_level+Precipitation_level+(1|siteID),data=turfs, subset=TTtreat%in%c("TT1", "TTC"), family=binomial)
summary(modglmm)
modglmm1<-glmer(exn~Temperature_level+Precipitation_level+(1|siteID/blockID),data=turfs, subset=TTtreat%in%c("TT1", "TTC"), family=binomial)
summary(modglmm1)
modglmm<-glmer(exn~TTtreat+(1|siteID/blockID),data=turfs, subset=TTtreat%in%c("TT1", "TTC"), family=binomial)
modglmer2<-glmer(exn~Temperature_level*TTtreat+Precipitation_level*TTtreat+(1|siteID),data=turfs,  subset=Temperature_level<3&Precipitation_level<4, family=binomial)
summary(modglmer2)
modglmer2b<-glmer(exn~Temperature_level*newTT+Precipitation_level*newTT+(1|siteID),data=turfs,  subset=Temperature_level<3&Precipitation_level<4, family=binomial)
summary(modglmer2b)

modglmer2c<-glmer(exn~warm*wet*Precipitation_level+(1|siteID),data=turfs,  subset=Temperature_level<3&Precipitation_level<4, family=binomial)

sites<-levels(turfs$siteID)
names(sites)<-levels(turfs$siteID)
sitex<-lapply(sites, function(site){
  form<-formula(exn~warm*wet+(1|blockID))
  if(turfs$Temperature_level[turfs$siteID==site][1]==3)
    form<-formula(exn~wet+(1|blockID))
  if(turfs$Precipitation_level[turfs$siteID==site][1]==4)
    form<-formula(exn~warm+(1|blockID))
  if(  turfs$Temperature_level[turfs$siteID==site][1]==3&turfs$Precipitation_level[turfs$siteID==site][1]==4)
   form<-formula(exn~1+(1|blockID))
    
  
  glmer(form,data=turfs,  subset=siteID==site, family=binomial)
})
lapply(sitex, summary)

sitex2<-lapply(sites, function(site){
  form<-formula(exn~newTT+(1|blockID))
  if(  turfs$Temperature_level[turfs$siteID==site][1]==3&turfs$Precipitation_level[turfs$siteID==site][1]==4)
   form<-formula(exn~1+(1|blockID))
    
  
  glmer(form,data=turfs,  subset=siteID==site, family=binomial)
})
lapply(sitex2, summary)




summary(modglmer2c)

modglmer3<-glmer(exn~TTtreat+(1|siteID),data=turfs,  subset=Temperature_level<3&Precipitation_level<4, family=binomial)
modglmer4<-glmer(exn~Precipitation_level*TTtreat+(1|siteID),data=turfs,  subset=Temperature_level<3&Precipitation_level<4, family=binomial)
summary(modglmer4)
summary(modglmm)

#immigrants #Poisson because ...
im<-sapply(w9.13,function(w)sapply(w, length))["immigrant",]
im11<-sapply(w9.11,function(w)sapply(w, length))["immigrant",]
x11();plot(turfrichness, im, col=turfs$TTtreat, pch=16)
abline(0,1)
cor.test(turfrichness, im)
x11()
plot(jitter(im11), jitter(im), pch=16, col=turfs$TTtreat)
abline(0,1)
abline(lm(im~im11), col=2)
summary(lm(im~im11))

sitim2<-lapply(sites, function(site){
  form<-formula(im~newTT+(1|blockID))
  if(  turfs$Temperature_level[turfs$siteID==site][1]==3&turfs$Precipitation_level[turfs$siteID==site][1]==4)
   form<-formula(im~1+(1|blockID))
    
  
  glmer(form,data=turfs,  subset=siteID==site, family=poisson)
})
lapply(sitim2, summary)


modglmer2c<-glmer(im~newTT+(1|siteID),data=turfs,  subset=Temperature_level<3&Precipitation_level<4, family=poisson)
summary(modglmer2c)



modlme<-lme(im~Temperature_level+Precipitation_level,data=turfs, subset=TTtreat%in%c("TT1", "TTC"), random=~1|siteID)
summary(modlme)
modlme<-lme(im~TTtreat,data=turfs, subset=TTtreat%in%c("TT1", "TTC"), random=~1|siteID)
summary(modlme)
modlme1<-lme(im~TTtreat,data=turfs, subset=Temperature_level<3&Precipitation_level<4, random=~1|siteID)

modlme2<-lme(im~Temperature_level*TTtreat+Precipitation_level*TTtreat,data=turfs,  subset=Temperature_level<3&Precipitation_level<4, random=~1|siteID)

summary(modlme1)
summary(modlme2)

#winners
wi<-sapply(w9.13,function(w)sapply(w, length))["winner",]
modlme<-lme(wi~Temperature_level+Precipitation_level,data=turfs, subset=TTtreat%in%c("TT1", "TTC"), random=~1|siteID)
summary(modlme)
modlme<-lme(wi~TTtreat,data=turfs, subset=TTtreat%in%c("TT1", "TTC"), random=~1|siteID)
summary(modlme)
modlme1<-lme(wi~TTtreat,data=turfs, subset=Temperature_level<3&Precipitation_level<4, random=~1|siteID)

modlme2<-lme(wi~Temperature_level*TTtreat+Precipitation_level*TTtreat,data=turfs,  subset=Temperature_level<3&Precipitation_level<4, random=~1|siteID)

summary(modlme1)
summary(modlme2)

#loosers

lo<-sapply(w9.13,function(w)sapply(w, length))["losers",]
plot(ex, lo)
loo<-lo-ex
notloo<-turfrichness-loo
loon<-cbind(loo, notloo)

sitlo2<-lapply(sites, function(site){
  form<-formula(loon~newTT+(1|blockID))
  if(  turfs$Temperature_level[turfs$siteID==site][1]==3&turfs$Precipitation_level[turfs$siteID==site][1]==4)
   form<-formula(loon~1+(1|blockID))
    
  
  glmer(form,data=turfs,  subset=siteID==site, family=binomial)
})
lapply(sitlo2, summary)

#losers inclusive of extinct

notloe<-turfrichness-lo
loen<-cbind(lo, notloe)

sitloe2<-lapply(sites, function(site){
  form<-formula(loen~newTT+(1|blockID))
  if(  turfs$Temperature_level[turfs$siteID==site][1]==3&turfs$Precipitation_level[turfs$siteID==site][1]==4)
   form<-formula(loen~1+(1|blockID))
    
  
  glmer(form,data=turfs,  subset=siteID==site, family=binomial)
})
lapply(sitloe2, summary)





ge<-sapply(w9.13,function(w)sapply(w, length))["generalist",]
modlme<-lme(ge~Temperature_level+Precipitation_level,data=turfs, subset=TTtreat%in%c("TT1", "TTC"), random=~1|siteID)
plot(turfrichness, ge)
summary(modlme)
modlme<-lme(ge~TTtreat,data=turfs, subset=TTtreat%in%c("TT1", "TTC"), random=~1|siteID)
summary(modlme)
modlme1<-lme(ge~TTtreat,data=turfs, subset=Temperature_level<3&Precipitation_level<4, random=~1|siteID)

modlme2<-lme(ge~Temperature_level*TTtreat+Precipitation_level*TTtreat,data=turfs,  subset=Temperature_level<3&Precipitation_level<4, random=~1|siteID)

summary(modlme1)
summary(modlme2)







winnerslosers<-lapply(c(2011:2013), function(yr){
    lapply(turfs$turfID,function(tu){
      wl<-winlose(tu,2009, yr)
    })
  })

#winlose over time

exim<-t(sapply(turfs$turfID, function(tu){  print(as.character(tu))
  yrs<-2011:2013
  names(yrs)<-yrs
  res<-sapply(c(2011:2013), function(yr){
    wl<-winlose(tu,2009, yr)
    c(extinct=length(wl$extinct),immigrant=length(wl$immigrant))
  })
  #find target turf
  if(turfs$TTtreat[turfs$turfID==tu]%in%c("TTC","TT1"))inf<-c(NA,NA)
  else{
    target<-turfs$turfID[turfs$TTtreat=="TT1"&as.character(turfs$blockID)==as.character(turfs$destBlockID[turfs$turfID==tu])]
    print(paste("target=",target))
    #debugonce(winlose, text="infinity")
    wl<-winlose(c(as.character(tu), as.character(target)),2009,2013)
    inf<-c(extinct=length(wl$extinct),immigrant=length(wl$immigrant))
  }
  cbind(res,inf)
}))


x11();par(mfrow=c(3,2), mar=c(3,3,1.5,1), mgp=c(1.5,.5,0))
  by(cbind(as.data.frame(exim), TTtreat=turfs$TTtreat), turfs$TTtreat, function(x){
  boxplot(x[,1:8], ylim=range(exim, na.rm=TRUE), main=x$TTtreat[1], names=c("2011.l","2011.w", "2012.l","2012.w", "2013.l","2013.w", "target.l","target.w"), notch=TRUE, col=c(rep(c("pink", "yellow"),3), "pink2", "yellow2"), las=2)
  
  abline(h=0)
})


#proportion graminoid win lose
gramwl<-t(sapply(turfs$turfID, function(tu){  print(as.character(tu))
  yrs<-2011:2013
  names(yrs)<-yrs
  res<-sapply(c(2011:2013), function(yr){
    wl<-winlose(tu,2009, yr)
    c(mean(wl$lose%in%traits$species[traits$functional=="graminoid"]),   mean(wl$win%in%traits$species[traits$functional=="graminoid"]))
    
  })
  if(turfs$TTtreat[turfs$turfID==tu]%in%c("TTC","TT1"))inf<-c(NA,NA)
  else{
    target<-turfs$turfID[turfs$TTtreat=="TT1"&as.character(turfs$blockID)==as.character(turfs$destBlockID[turfs$turfID==tu])]
    print(paste("target=",target))
    #debugonce(winlose, text="infinity")
    wl<-winlose(c(as.character(tu), as.character(target)),2009,2013)
    inf<-c(mean(wl$lose%in%traits$species[traits$functional=="graminoid"]),mean(wl$win%in%traits$species[traits$functional=="graminoid"]))
  }
      orig<-mean(winlose(tu,2009,2009)$generalist%in%traits$species[traits$functional=="graminoid"])

  c(orig,as.vector(cbind(res,inf)))

}))


x11();par(mfrow=c(3,2), mar=c(3,3,1.5,1), mgp=c(1.5,.5,0))
  by(cbind(as.data.frame(gramwl), TTtreat=turfs$TTtreat), turfs$TTtreat, function(x){
  boxplot(x[,c(1,2,4,6,8,3,5,7,9)], ylim=range(gramwl, na.rm=TRUE), main=x$TTtreat[1], names=c("origin","2011.l","2011.w", "2012.l","2012.w", "2013.l","2013.w", "target.l","target.w"), notch=TRUE, col=c("white",rep("pink",3),"pink2", rep( "yellow",3), "yellow2"), las=2)
  
  abline(h=0)
})



#proportion alpine win lose
laplwl<-t(sapply(turfs$turfID, function(tu){  print(as.character(tu))
  yrs<-2011:2013
  names(yrs)<-yrs
  res<-sapply(c(2011:2013), function(yr){
    wl<-winlose(tu,2009, yr)
    c(mean(wl$lose%in%traits$species[traits$alpine]),   mean(wl$win%in%traits$species[traits$alpine]))
    
  })
  if(turfs$TTtreat[turfs$turfID==tu]%in%c("TTC","TT1"))inf<-c(NA,NA)
  else{
    target<-turfs$turfID[turfs$TTtreat=="TT1"&as.character(turfs$blockID)==as.character(turfs$destBlockID[turfs$turfID==tu])]
    print(paste("target=",target))
    #debugonce(winlose, text="infinity")
    wl<-winlose(c(as.character(tu), as.character(target)),2009,2013)
    inf<-c(mean(wl$lose%in%traits$species[traits$alpine], na.rm=TRUE),mean(wl$win%in%traits$species[traits$alpine], na.rm=TRUE))
  }
      orig<-mean(winlose(tu,2009,2009)$generalist%in%traits$species[traits$alpine])

  c(orig,as.vector(cbind(res,inf)))

}))


x11();par(mfrow=c(3,2), mar=c(3,3,1.5,1), mgp=c(1.5,.5,0))
  by(cbind(as.data.frame(laplwl), TTtreat=turfs$TTtreat), turfs$TTtreat, function(x){
  boxplot(x[,c(1,2,4,6,8,3,5,7,9)], ylim=range(laplwl, na.rm=TRUE), main=x$TTtreat[1], names=c("origin","2011.l","2011.w", "2012.l","2012.w", "2013.l","2013.w", "target.l","target.w"), notch=TRUE, col=c("white",rep("pink",3),"pink2", rep( "yellow",3), "yellow2"), las=2)
  
  abline(h=0)
})



heightwl<-t(sapply(turfs$turfID, function(tu){  print(as.character(tu))
  yrs<-2011:2013
  names(yrs)<-yrs
  sapply(c(2011:2013), function(yr){
    wl<-winlose(tu,2009, yr)
    c(-mean(traits$Max.height[traits$species%in%wl$lose]),   mean(traits$Max.height[traits$species%in%wl$win]))
    
  })
}))


x11();par(mfrow=c(3,2), mar=c(3,3,1.5,1), mgp=c(1.5,.5,0))
  by(cbind(as.data.frame(heightwl), TTtreat=turfs$TTtreat), turfs$TTtreat, function(x){
  boxplot(x[,c(1,3,5)], ylim=range(heightwl, na.rm=TRUE), main=x$TTtreat[1], names=c(2011, 2012, 2013), notch=TRUE)
  boxplot(x[,c(2,4,6)], ylim=range(heightwl, na.rm=TRUE), add=T, axes=FALSE, notch=TRUE)
  
  abline(h=0)
})


slawl<-t(sapply(turfs$turfID, function(tu){  print(as.character(tu))
  yrs<-2011:2013
  names(yrs)<-yrs
  sapply(c(2011:2013), function(yr){
    wl<-winlose(tu,2009, yr)
    c(mean(traits$SLA[traits$species%in%wl$ext]),   mean(traits$SLA[traits$species%in%wl$imm]))
    
  })
}))


x11();par(mfrow=c(3,2), mar=c(3,3,1.5,1), mgp=c(1.5,.5,0))
  by(cbind(as.data.frame(slawl), TTtreat=turfs$TTtreat), turfs$TTtreat, function(x){
  boxplot(x[,1:6], ylim=range(slawl, na.rm=TRUE), main=x$TTtreat[1], names=rep(c(2011, 2012, 2013),each=2), notch=TRUE, col=c("red", "yellow"))
  
  abline(h=0)
})

seedwl<-t(sapply(turfs$turfID, function(tu){  print(as.character(tu))
  yrs<-2011:2013
  names(yrs)<-yrs
  sapply(c(2011:2013), function(yr){
    wl<-winlose(tu,2009, yr)
    c(mean(log(traits$seedMass[traits$species%in%wl$ext]), na.rm=TRUE),   mean(log(traits$seedMass[traits$species%in%wl$imm]), na.rm=TRUE))
    
  })
}))

hist(log(traits$seedMass))

x11();par(mfrow=c(3,2), mar=c(3,3,1.5,1), mgp=c(1.5,.5,0))
  by(cbind(as.data.frame(seedwl), TTtreat=turfs$TTtreat), turfs$TTtreat, function(x){
  boxplot(x[,1:6], ylim=range(seedwl, na.rm=TRUE), main=x$TTtreat[1], names=rep(c(2011, 2012, 2013),each=2), notch=TRUE, col=c("red", "yellow"))
  
  abline(h=0)
})


