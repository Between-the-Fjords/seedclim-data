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
