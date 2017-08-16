w9.11<-    lapply(turfs$turfID,function(tu){
      wl<-winlose(tu,2009, 2011)
    })

w9.12<-    lapply(turfs$turfID,function(tu){
      wl<-winlose(tu,2009, 2012)
    })
    
w9.13<-    lapply(turfs$turfID,function(tu){
      wl<-winlose(tu,2009, 2013)
    })
    
w9<-list(w9.11, w9.12,w9.13)
ex<-sapply(w9,function(W)sapply(W,function(w)w$Nextinct))
rich<-sapply(w9,function(W)sapply(W,function(w)w$rich))


im<-sapply(w9,function(W)sapply(W,function(w)w$Nimmigrant))#sapply(w, length))["immigrant",])

fex<-findfinal(TRUE)
fim<-findfinal(FALSE)
x11()
par(mfcol=c(2,2), mgp=c(1.5,.5,0), oma=c(0,0,2,0))
numberspecies(ex/rich[1,], "Proportion extinct", "Extinctions", show=c("p", "t"), final=fex/rich[,1], above=FALSE)


x11()
par(mfcol=c(2,2), mgp=c(1.5,.5,0), oma=c(0,0,2,0))
numberspecies(im, "Number immigrants", "Immigrants", show=c("p", "t"), final=fim, above=FALSE)
                  
                  
x11(height=5)
par(mfrow=c(2,5), mgp=c(1.1,.1,0), oma=c(0,0,2,0), tcl=0.2)
numberspecies(ex/rich[1,], "Proportion extinct", "Extinctions", show=c("p", "t"), final=fex/rich[,1], above=FALSE, inarow=TRUE, years=2:4)
numberspecies(im, "Number immigrants", "Immigrants", show=c("p", "t"), final=fim, above=FALSE, inarow=TRUE, bottomrow=TRUE, years=2:4)



#################
#immigrant cover
#sum cover/ subturfs in years 2011,12,13


#extinct cover
#sum cover/subturfs in 2009 of taxa extinct in 2011, 12, 13

exim.im<-exim(usecover=TRUE, immigration=TRUE)
exim.ex<-exim(usecover=TRUE, immigration=FALSE)
x11(pointsize=pointsize)
par(mfcol=c(2,2), mar=c(2,3,ifelse(above,1.5,.75),1), mgp=c(1.5,.5,0), oma=c(0,0,2,0))
numberspecies(t(exim.im), "Immigrants cover", "Immigrants cover", show=c("p", "t"))
numberspecies(t(exim.ex), "Extinct cover", "Extinct cover", show=c("p", "t"), years=c(2009, 2011, 2012))

exim.ims<-exim(usecover=FALSE, immigration=TRUE)
exim.exs<-exim(usecover=FALSE, immigration=FALSE)
x11()
par(mfcol=c(2,2), mgp=c(1.5,.5,0), oma=c(0,0,2,0))
numberspecies(t(exim.ims), "Immigrants subturfs", "Immigrants subturfs", show=c("p", "t"))
numberspecies(t(exim.exs), "Extinct subturfs", "Extinct subturfs", show=c("p", "t"), years=c(2009, 2011, 2012))

identical(colnames(cover), as.character(traits$species))

gramc<-traitselector(usecover=TRUE, keep=traits$functionalgroup=="graminoid")
x11()
par(mfcol=c(2,2), mgp=c(1.5,.5,0), oma=c(0,0,2,0))
numberspecies(t(gramc), "Graminoid cover", "Graminoid cover", show=c("p", "t"), years=c(2009, 2011, 2012, 2013))
gramst<-traitselector(usecover=FALSE, keep=traits$functionalgroup=="graminoid")
numberspecies(t(gramst), "Graminoid subturfs", "Graminoid subturfs", show=c("p", "t"), years=c(2009, 2011, 2012, 2013))


x11()
par(mfcol=c(2,2), mgp=c(1.5,.5,0), oma=c(0,0,2,0))
poa<-traitselector(usecover=TRUE,keep=traits$family=="Poaceae")
poaf<-findfinaltrait(usecover=TRUE, keep=traits$family=="Poaceae")
numberspecies(t(poa), "Poaceae cover", "Poaceae cover", show=c("p", "t"), final=poaf,years=c(2009, 2011, 2012, 2013))

x11()
par(mfcol=c(2,2), mgp=c(1.5,.5,0), oma=c(0,0,2,0))
cyp<-traitselector(usecover=TRUE,keep=traits$family=="Cyperaceae")
cypf<-findfinaltrait(usecover=TRUE, keep=traits$family=="Cyperaceae")
numberspecies(t(cyp), "Cyperaceae cover", "Cyperaceae cover", show=c("p", "t"), final=cypf,years=c(2009, 2011, 2012, 2013))

x11()
forb<-traitselector(usecover=TRUE,keep=traits$functionalGroup=="forb")
forbf<-findfinaltrait(usecover=TRUE, keep=traits$functionalGroup=="forb")
numberspecies(t(forb), "Forb cover", "Forb cover", show=c("p", "t"), final=forbf,years=c(2009, 2011, 2012, 2013))


alp<-traitselector(keep=traits$alpine==TRUE)
numberspecies(t(alp), "Alpine cover", "Alpine cover", show=c("p", "t"), years=c(2009, 2011, 2012, 2013))

lev<-factor(paste(turfs$TTtreat,turfs$Precipitation_level))
lev<-factor(lev, levels=levels(lev)[c(15:18, 1:14)])
at<-c(1:4,6:9, 11:14, 16:18, 20:22) 
col=rep(bcol,c(4,4,4,3,3))
boxplot((alp[4,]-alp[1,])~lev, subset=turfs$Temperature_level==1, at=at,notch=TRUE, las=2, col=col, main="2013-2009 Alpines", ylab="Cover")
abline(h=0, lty=2, col="grey80")


boxplot((alp[4,]-alp[1,])~turfs$TTtreat, subset=turfs$Temperature_level==1,notch=TRUE, las=2, col=bcol, main="2013-2009 Alpines", ylab="Change in Cover")
abline(h=0, lty=2, col="grey80")
summary(lm((alp[4,]-alp[1,])~turfs$TTtreat, subset=turfs$Temperature_level==1))

low<-traitselector(keep=traits$lowland==TRUE)
numberspecies(t(low), "Lowland cover", "Lowland cover", show=c("p", "t"), years=c(2009, 2011, 2012, 2013)2)



notalplow<-traitselector(keep=traits$lowland==FALSE&traits$alpine==FALSE)
numberspecies(t(notalplow), "notalplow cover", "notalplow cover", show=c("p", "t"), years=c(2009, 2011, 2012, 2013))



#environmental change

#vegetation height
#total bryophyte cover
#etc
mean(turfenv$turfID%in%turfs$turfID)
env.v<-lapply(names(turfenv)[-(1:7)], function(n){
  res<-sapply(as.character(turfs$turfID), function(id){
    tu<-turfenv[turfenv$turfID==id,n]
    if(length(tu)<4) tu<-c(tu, rep(NA, 4-length(tu)))
    tu
  })
  t(res)
})

names(env.v)<-names(turfenv)[-(1:7)]


mapply(numberspecies, x=env.v, ylab=names(env.v), main=names(env.v), MoreArgs=list(   show=c("p", "t"), years=c(2009, 2011, 2012, 2013)))



#extinction vs grass cover  etc
plot(poa[4,]-poa[1,], ex[,3]/rich[,3], col=c("black", "grey80", "red", "blue", "purple")[turfs$TTtreat], pch=16)
plot(poa[4,], ex[,3]/rich[,3], col=c("black", "grey80", "red", "blue", "purple")[turfs$TTtreat], pch=16)


plot(poa[4,], ex[,3]/rich[,3], col=turfs$Temperature_level, pch=16)

  #immigrant cover
plot(exim.im[3,], ex[,3]/rich[,3], col=c("black", "grey80", "red", "blue", "purple")[turfs$TTtreat], pch=16)
cor.test(exim.im[3,], ex[,3]/rich[,3],use="pair")

by(cbind(exim.im[3,], ex[,3]/rich[,3]), turfs$TTtreat, cor, use="pair")
x11();par(mfrow=c(3,2), mar=c(3,3,1,1))
by(cbind(exim.im[3,], ex[,3]/rich[,3]), turfs$TTtreat, plot)
