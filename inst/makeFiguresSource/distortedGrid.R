keep<-with(cover.meta,TTtreat%in%c("TTC", "TT1")&Year==2009)
spp<-cover[keep,]>0

mod<-metaMDS(spp)
plot(mod, display="sites")
ef<-envfit(mod, data.frame(f=cover.meta$siteID[keep]))
plot(ef, add=TRUE, labels=substr(levels(cover.meta$siteID), 1,3))
ef

#make lines to join adjacent sites
par(mfrow=c(1,1))
mod2<-cca(sqrt(spp)~cover.meta$siteID[keep])
plot(mod2, display="sites")
points(mod2,display="cn", pch=4, col=2)
text(mod2,display="cn", col=2, labels=substr(levels(cover.meta$siteID), 1,3))

par(mfrow=c(1,1))
mod3<-decorana(sqrt(spp))
plot(mod3, display="sites", type="n")
points(mod3, display="sites")
ef3<-envfit(mod3, data.frame(f=cover.meta$siteID[keep]))
plot(ef3, add=TRUE, labels=substr(levels(cover.meta$siteID), 1,3))




unique(cover.meta[,c("siteID","Temperature_level",   "Precipitation_level") ])
#pairwise distances
distmat<-as.matrix(vegdist(spp))
hist(distmat)

pairwiseD<-function(t1, t2, p1, p2, plot=FALSE, median=FALSE, TTtreat=NULL){
  if(missing(t2)){
    if(TTtreat%in%c("TC", "TT1")){t2<-t1; p2<-p1}
    else if(TTtreat=="TT2"){t2<-t1+1; p2<-p1}
    else if(TTtreat=="TT3"){t2<-t1; p2<-p1+1}
    else if(TTtreat=="TT4"){t2<-t1+1; p2<-p1+1}
  }
  kr<-with(cover.meta[keep,], Temperature_level==t1&Precipitation_level==p1)
  kc<-with(cover.meta[keep,], Temperature_level==t2&Precipitation_level==p2)
  dm<-distmat[kr,kc]
  if(t1==t2&p1==p2)diag(dm)<-NA
  if(plot)hist(dm, xlim=c(min(distmat[lower.tri(distmat)]),1), main=paste("t1=",t1, "p1=",p1,"t2=",t2, "p2=",p2))
  if(median){ 
    deltaT<-with(cover.meta[keep,], (summerTemperature[kc]-summerTemperature[kr])[1])
    deltaP<-with(cover.meta[keep,], (annualPrecipitation[kc]-annualPrecipitation[kr])[1])
    if(!is.null(TTtreat)){
      dists<-pddists[pddists$Temperature_level==t1&pddists$Precipitation_level==p1&pddists$TTtreat==TTtreat,]
      tkeep<-turfs$Temperature_level==t1&turfs$Precipitation_level==p1&turfs$TTtreat==TTtreat
      extinctions<-cbind(ex/rich,rich=rich[,1])[tkeep,]
      immigration<-im[tkeep,]
  }else{ 
      dists<-NULL
      extinctions<-NULL
      immigration<-NULL
    }
    cbind(t1=t1,p1=p1,t2=t2, p2=p2,median=median(dm, na.rm=TRUE), deltaT=deltaT, deltaP=deltaP, dists=dists, extinctions=extinctions, immigration=immigration)
  }
  else dm
}

par(mfrow=c(3,3))
m3<-mapply(pairwiseD,t1=rep(1:3, each=3), t2=rep(1:3, each=3), p1=rep(1:3,3), p2=rep(2:4,3), SIMPLIFY=TRUE, plot=TRUE, median=TRUE)
m3<-mapply(pairwiseD,t1=rep(1:3, each=3), p1=rep(1:3,3), TTtreat="TT3", SIMPLIFY=FALSE, plot=TRUE, median=TRUE)
m3<-do.call(rbind,m3)

par(mfcol=c(2,4))
m2<-mapply(pairwiseD,t1=rep(1:2,4), p1=rep(1:4,each=2), TTtreat="TT2", SIMPLIFY=FALSE, plot=TRUE, median=TRUE)
m2<-do.call(rbind,m2)


par(mfcol=c(2,3))
m4<-mapply(pairwiseD,t1=rep(1:2,3), p1=rep(1:3,each=2), TTtreat="TT4", SIMPLIFY=FALSE, plot=TRUE, median=TRUE)
m4<-do.call(rbind,m4)

par(mfcol=c(3,4))
m1<-mapply(pairwiseD,t1=rep(1:3,4), t2=rep(1:3, 4), p1=rep(1:4,each=3), p2=rep(1:4,each=3), SIMPLIFY=TRUE, plot=TRUE, median=TRUE)




par(mfrow=c(3,1))
m5<-mapply(pairwiseD,t1=rep(1:3, each=1), t2=rep(1:3, each=1), p1=rep(1,3), p2=rep(4,3), SIMPLIFY=TRUE, plot=TRUE, median=TRUE)

par(mfrow=c(1,4))
m6<-mapply(pairwiseD,t1=rep(1, 4), t2=rep(3, 4), p1=rep(1:4,each=1), p2=rep(1:4,each=1), SIMPLIFY=TRUE, plot=TRUE, median=TRUE)


##################
#change~delta climate
#change~delta comm



#scaled and unscaled 
#toenail plots?

#immigration has strong community push
#extinction = competition + climate


#plot response vs delta comm
#in pa plot

#responses = extinction, immigration, turnover
#done#predictors = deltaclimate(deltatemperature, deltaprecipitation), deltacommunity, treatment
m3

#
head(m4)
cor(m2$dists.d11, m2$deltaT)
cor(m2$dists.d11, m2$dists.od)

cor(m3$dists.d11, m3$deltaP)
cor(m3$dists.d11, m3$dists.od)

mm<-rbind(m2,m3,m4)

mod0<-lm(dists.d11~1, data=mm)
mod1<-lm(dists.d11~dists.TTtreat, data=mm)
summary(mod1)
anova(mod1)
boxplot(dists.d11~dists.TTtreat, data=mm)

mod1b<-lm(dists.d11~dists.TTtreat*dists.siteID, data=mm)
anova(mod1b)
summary(mod1b)

mod1c<-lm(dists.d11~dists.siteID, data=mm)
anova(mod1c)
summary(mod1c)


mod2<-lm(dists.d11~dists.Temperature_level*dists.Precipitation_level, data=mm)
anova(mod2)
summary(mod2)

mod3<-lm(dists.d11~deltaT*deltaP, data=mm)
anova(mod3)
summary(mod3)

mod4<-lm(dists.d11~dists.od, data=mm)
anova(mod4)
summary(mod4)

mod5<-lm(dists.d11~median, data=mm)
anova(mod5)
summary(mod5)

mod6<-lm(dists.d11~median*dists.TTtreat, data=mm)
anova(mod6)
summary(mod6)

########extinction proportion
mod0e<-glm(extinctions.V1~1, data=mm, family=quasibinomial, weight=extinctions.rich)
mod1e<-glm(extinctions.V1~dists.TTtreat, data=mm, family=quasibinomial, weight=extinctions.rich)
summary(mod1e)
anova(mod1e)
boxplot(extinctions.V1~dists.TTtreat, data=mm)

mod2e<-glm(extinctions.V1~dists.Temperature_level*dists.Precipitation_level, data=mm, family=quasibinomial, weight=extinctions.rich)
anova(mod2e)
summary(mod2e)

mod3e<-glm(extinctions.V1~deltaT*deltaP, data=mm, family=quasibinomial, weight=extinctions.rich)
anova(mod3e)
summary(mod3e)

mod4e<-glm(extinctions.V1~dists.od, data=mm, family=quasibinomial, weight=extinctions.rich)
anova(mod4e)
summary(mod4e)

mod5e<-glm(extinctions.V1~median, data=mm, family=quasibinomial, weight=extinctions.rich)
anova(mod5e)
summary(mod5e)

mod6e<-glm(extinctions.V1~median+dists.TTtreat, data=mm, family=quasibinomial, weight=extinctions.rich)
anova(mod6e)
summary(mod6e)


########extinction proportion
mod0i<-glm(immigration.1~1, data=mm, family=quasipoisson)
mod1i<-glm(immigration.1~dists.TTtreat, data=mm, family=quasipoisson)
summary(mod1i)
anova(mod1i)
boxplot(immigration.1~dists.TTtreat, data=mm)

mod2i<-glm(immigration.1~dists.Temperature_level*dists.Precipitation_level, data=mm, family=quasipoisson)
anova(mod2i)
summary(mod2i)

mod3i<-glm(immigration.1~deltaT*deltaP, data=mm, family=quasipoisson)
anova(mod3i)
summary(mod3i)

mod4i<-glm(immigration.1~dists.od, data=mm, family=quasipoisson)
anova(mod4i)
summary(mod4i)

mod5i<-glm(immigration.1~median, data=mm, family=quasipoisson)
anova(mod5i)
summary(mod5i)

mod6i<-glm(immigration.1~median+dists.TTtreat, data=mm, family=quasipoisson)
anova(mod6i)
summary(mod6i)

