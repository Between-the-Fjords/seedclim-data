library(lme4)
exdata<-cbind(rbind(turfs, turfs, turfs), ex=as.vector(ex), notex=as.vector(rich)-as.vector(ex),propex=as.vector(ex)/as.vector(rich))
exdata$Year<-rep(2011:2013, each=nrow(turfs))-2009#subtract 2009 or glmer is upset about different scales

find.treatment.control<-function(x,TT, control=TRUE){
  if(length(TT)==2){
    sites<-do.call(intersect, lapply(TT, function(i){unique(x$siteID[x$TTtreat==i])}))
  }else if(length(TT)==1){
    sites<-unique(x$siteID[x$TTtreat==TT])
  }else {
    stop("can only cope with 1 or 2 treatments + control (TT1)")
  }
  if(control){target<-c(TT,"TT1")}
  else{target<-TT}
  x[x$siteID%in%sites&x$TTtreat%in%target,]
}  
  




####with glmer

lapply(c("TTC","TT2","TT3", "TT4"), function(TT){# are treatment different from control
  tmp<-find.treatment.control(exdata, TT, control=TRUE)
  mod1.0<-glmer(cbind(ex, notex)~1+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=binomial)
  mod1.1<-glmer(cbind(ex, notex)~TTtreat+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=binomial)
  list(summary=summary(mod1.1),anova=anova(mod1.0, mod1.1))
})

lapply(list(c("TT2","TT3"),c("TT2", "TT4"), c("TT3", "TT4")), function(TT){# are treatments different from each other
  tmp<-find.treatment.control(exdata, TT, control=FALSE)
  mod1.0<-glmer(cbind(ex, notex)~1+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=binomial)
  mod1.1<-glmer(cbind(ex, notex)~TTtreat+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=binomial)
  list(summary=summary(mod1.1),anova=anova(mod1.0, mod1.1))
})



lapply(c("TT1","TT2","TT3", "TT4"), function(TT){#is there a change with year
  tmp<-find.treatment.control(exdata, TT, control=FALSE)
  
  mod2.1<-glmer(cbind(ex, notex)~1+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=binomial)
  mod2.2<-glmer(cbind(ex, notex)~Year+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=binomial)
  list(summary=summary(mod2.2),anova=anova(mod2.1, mod2.2))
})


#is there an effect of climate
lapply(c("TT1","TT2","TT3", "TT4"), function(TT){
  tmp<-find.treatment.control(exdata, TT, control=FALSE)
  tmp$Precipitation_level<-factor(tmp$Precipitation_level)
  tmp$Temperature_level<-factor(tmp$Temperature_level)
  
  anv<-list()
  mod3.1<-glmer(cbind(ex, notex)~Year+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=binomial)

  
  mod3.2<-glmer(cbind(ex, notex)~Year+Precipitation_level+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=binomial)
  anv$ppt<-anova(mod3.1, mod3.2)
  
  mod3.3<-glmer(cbind(ex, notex)~Year*Precipitation_level+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=binomial)
  anv$pptint<-anova(mod3.2, mod3.3)
  
  mod3.4<-glmer(cbind(ex, notex)~Year+Temperature_level+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=binomial)
  anv$tmp<-anova(mod3.1, mod3.4)
  mod3.5<-glmer(cbind(ex, notex)~Year*Temperature_level+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=binomial)
  anv$tmpint<-anova(mod3.4, mod3.5)
  
  #mod3.6<-glmer(cbind(ex, notex)~Year+Temperature_level+Precipitation_level+(1|siteID)+(1|factor(1:length(siteID))), data=tmp,family=binomial)
  #anv$tmpppt<-anova(mod3.4, mod3.6)
  #anv$ppttmp<-anova(mod3.2, mod3.6)
  
  anv
})


