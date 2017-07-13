#extinct - number lost
#predictors of loss vs not lost - original abundance /number of subplots
#- present in target turf/block/site?
#         - biogeographs
#         - traits
get original turf, get 2011/12/13 turf, identify exinctions/nonextinction, make predictors build model

extinct.data<- data.frame(extinct=NA, initcover=NA,initfre=NA,targetcover=NA, height.rat=NA,SLA.rat=NA,leafSize.rat=NA,seedMass.rat=NA)
extinct.data<-extinct.data[-1,]
sapply(cover.meta$turfID[cover.meta$TTtreat=="TT2"][c(F,T,F)], function(tu){print(tu)
  #tu<-"1 TT2 28"
  originalt<-cover[cover.meta$turfID==tu&cover.meta$Year==2009,, drop=FALSE]
  newt<-cover[cover.meta$turfID==tu&cover.meta$Year==2011,, drop=FALSE]
  oldnew<-rbind(originalt, newt)
  oldnew<-oldnew[,oldnew[1,]>0]
    
  extinct<-as.vector(oldnew[2,]==0)
  initcover<-log(unlist(oldnew[1,])+1)
  initfre<-unlist(fsubturf[cover.meta$turfID==tu&cover.meta$Year==2009,originalt>0])
  targetturf<-cover.meta$turfID[cover.meta$TTtreat=="TT1"&as.character(cover.meta$blockID)==as.character(cover.meta$destBlockID)[cover.meta$turfID==tu]]
  targetcover<-unlist(fsubturf[cover.meta$turfID==targetturf&cover.meta$Year==2009,originalt>0])
  targetcover<-log(targetcover+1)
  #traits
  t2<-traits[traits$species%in%names(oldnew),]
  height.rat<-log(t2$Max.height/weighted.mean(t2$Max.height,initcover, na.rm=TRUE))
  SLA.rat<-log(t2$SLA/weighted.mean(t2$SLA,initcover, na.rm=TRUE))
  leafSize.rat<-log(t2$leafSize/weighted.mean(t2$leafSize,initcover, na.rm=TRUE))
  seedMass.rat<-log(t2$seedMass/weighted.mean(t2$seedMass,initcover, na.rm=TRUE) )
  extinct.data<<-rbind(extinct.data,data.frame(extinct=extinct, initcover=initcover,initfre=initfre, targetcover=targetcover, height.rat=height.rat,SLA.rat=SLA.rat,leafSize.rat=leafSize.rat,seedMass.rat=seedMass.rat))
  invisible()
})

head(extinct.data)
par(mfrow=c(3,3))
mapply(hist, x=extinct.data[,-1], main=names(extinct.data)[-1])


anova(glm(extinct~initcover, data=extinct.data), test="Chi")
anova(glm(extinct~initfre, data=extinct.data), test="Chi")
anova(glm(extinct~targetcover, data=extinct.data), test="Chi")
anova(glm(extinct~height.rat, data=extinct.data), test="Chi")
anova(glm(extinct~SLA.rat, data=extinct.data), test="Chi")
anova(glm(extinct~leafSize.rat, data=extinct.data), test="Chi")
anova(glm(extinct~seedMass.rat, data=extinct.data), test="Chi")

x11();par(mfrow=c(3,3), mar=c(3,3,1,1), mgp=c(1.5,.5,0))
mapply(function(x, main)plot(x, extinct.data[,1], main=main), x=extinct.data[,-1], main=names(extinct.data)[-1])

mod<-glm(extinct~., data=extinct.data, subset=rowSums(is.na(extinct.data))==0)
step(mod, direction="both")

anova(mod, test="Chi")
mod<-update(mod, .~.-seedMass.rat)
anova(mod, test="Chi")
mod<-update(mod, .~.-height.rat)

#losers vs winners
# predictors of (magnitude of) loss vs win
# presence in target turf/block site
# biogeography
#traits

winloss.data<- data.frame(winner=NA, initcover=NA,initfre=NA,targetcover=NA, height.rat=NA,SLA.rat=NA,leafSize.rat=NA,seedMass.rat=NA)
winloss.data<-winloss.data[-1,]
sapply(cover.meta$turfID[cover.meta$TTtreat=="TT2"][c(F,T,F)], function(tu){print(tu)
  #tu<-"1 TT2 28"
  originalt<-cover[cover.meta$turfID==tu&cover.meta$Year==2009,, drop=FALSE]
  newt<-cover[cover.meta$turfID==tu&cover.meta$Year==2011,, drop=FALSE]
  oldnew<-rbind(originalt, newt)
  oldnew<-oldnew[,oldnew[1,]>0&oldnew[2,]>0]
    
  winner<-as.vector(oldnew[2,]>oldnew[1,])
  initcover<-log(unlist(oldnew[1,])+1)
  initfre<-unlist(fsubturf[cover.meta$turfID==tu&cover.meta$Year==2009,originalt>0&newt>0])
  targetturf<-cover.meta$turfID[cover.meta$TTtreat=="TT1"&as.character(cover.meta$blockID)==as.character(cover.meta$destBlockID)[cover.meta$turfID==tu]]
  targetcover<-unlist(fsubturf[cover.meta$turfID==targetturf&cover.meta$Year==2009,originalt>0&newt>0])
  targetcover<-log(targetcover+1)
  #traits
  t2<-traits[traits$species%in%names(oldnew),]
  height.rat<-log(t2$Max.height/weighted.mean(t2$Max.height,initcover, na.rm=TRUE))
  SLA.rat<-log(t2$SLA/weighted.mean(t2$SLA,initcover, na.rm=TRUE))
  leafSize.rat<-log(t2$leafSize/weighted.mean(t2$leafSize,initcover, na.rm=TRUE))
  seedMass.rat<-log(t2$seedMass/weighted.mean(t2$seedMass,initcover, na.rm=TRUE) )
#browser()
  winloss.data<<-rbind(winloss.data,data.frame(winner=winner, initcover=initcover,initfre=initfre, targetcover=targetcover, height.rat=height.rat,SLA.rat=SLA.rat,leafSize.rat=leafSize.rat,seedMass.rat=seedMass.rat))
  invisible()
})

x11();par(mfrow=c(3,3), mar=c(3,3,1,1), mgp=c(1.5,.5,0))
mapply(function(x, main)plot(x, extinct.data[,1], main=main), x=extinct.data[,-1], main=names(extinct.data)[-1])

mod<-glm(winner~., data=winloss.data, subset=rowSums(is.na(winloss.data))==0)
step(mod, direction="both")


#immigrants  vs non-immigrants
#abundance in control plots
#traits
        
        
        