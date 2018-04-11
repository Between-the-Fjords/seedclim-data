library(lme4)

imdata<-cbind(rbind(turfs, turfs, turfs), im=as.vector(im))
imdata$Year<-rep(2011:2013, each=nrow(turfs))-2009#subtract 2009 or glmer is upset about different scales

####with glmer

lapply(c("TTC","TT2","TT3", "TT4"), function(TT){# are treatment different from control
  tmp<-find.treatment.control(imdata, TT, control=TRUE)
  mod1.0<-glmer(im~1+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=poisson)
  mod1.1<-glmer(im~TTtreat+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=poisson)
  list(summary=summary(mod1.1),anova=anova(mod1.0, mod1.1))
})

lapply(list(c("TT2","TT3"),c("TT2", "TT4"), c("TT3", "TT4")), function(TT){# are treatments different from each other
  tmp<-find.treatment.control(imdata, TT, control=FALSE)
  mod1.0<-glmer(im~1+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=poisson)
  mod1.1<-glmer(im~TTtreat+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=poisson)
  list(summary=summary(mod1.1),anova=anova(mod1.0, mod1.1))
})



lapply(c("TTC","TT1","TT2","TT3", "TT4"), function(TT){#is there a change with year
  tmp<-find.treatment.control(imdata, TT, control=FALSE)
  
  mod2.1<-glmer(im~1+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=poisson)
  mod2.2<-glmer(im~Year+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=poisson)
  list(summary=summary(mod2.2),anova=anova(mod2.1, mod2.2))
})


#is there an effect of climate
lapply(c("TT1","TT2","TT3", "TT4"), function(TT){
  tmp<-find.treatment.control(imdata, TT, control=FALSE)
  tmp$Precipitation_level<-factor(tmp$Precipitation_level)
  tmp$Temperature_level<-factor(tmp$Temperature_level)
  
  anv<-list()
  mod3.1<-glmer(im~1+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=poisson)
  
  mod3.2<-glmer(im~Precipitation_level+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=poisson)
  anv$ppt<-anova(mod3.1, mod3.2)
    
  mod3.4<-glmer(im~Temperature_level+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=poisson)
  anv$tmp<-anova(mod3.1, mod3.4)
  
  #mod3.6<-glmer(im~Year+Temperature_level+Precipitation_level+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=poisson)
  #anv$tmpppt<-anova(mod3.4, mod3.6)
  #anv$ppttmp<-anova(mod3.2, mod3.6)
  
  anv
})
