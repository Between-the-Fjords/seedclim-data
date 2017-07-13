#distance from origin

pdists<-make.dists((cover>0))

x11()
plot(pdists, temp=TRUE)
x11()
plot(pdists, temp=FALSE)

#distance towards destination

#get turf treatment T in block B site S year Y, find distance to TT1 in block B site S year Y
#find difference between d2009 & d2011 etc
#boxplot, slice and dice
pddists<-make.directional.dists(cover>0)
 pddists[apply(is.na(pddists),1,any),-7]

x11()
plot(pddists, split=FALSE)
x11()
plot(pddists, split=TRUE, temperature=TRUE)
x11(point=14)
x11(height=5, width=5, pointsize=12)
par(mar=c(3.1,1,.5,.5), mgp=c(1.1,.1,0),tcl=0.2, oma=c(0,2,0,0))
plot(pddists, split=TRUE, temperature=FALSE, draw=2013, notch=FALSE, addmain=FALSE, setpar=FALSE)
plot(pddists, split=TRUE, temperature=FALSE)

#relative distances
x11();
par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.5,.5,0))
boxplot(r11~TTtreat+Temperature_level, data=pddists[pddists$TTtreat!="TTC",], main="2009-2011", notch=TRUE, ylim=c(0.4,1.25),ylab="relative distance moved towards destination", col=TT.colours, las=2) ;abline(h=1)
boxplot(r12~TTtreat+Temperature_level, data=pddists[pddists$TTtreat!="TTC",], main="2009-2012", notch=TRUE, ylim=c(0.4,1.25),ylab="relative distance moved towards destination", col=TT.colours, las=2) ;abline(h=1)
boxplot(r13~TTtreat+Temperature_level, data=pddists[pddists$TTtreat!="TTC",], main="2009-2013", notch=TRUE, ylim=c(0.4,1.25),ylab="relative distance moved towards destination", col=TT.colours, las=2);abline(h=1)

x11();
par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.5,.5,0))
boxplot(r11~TTtreat+Precipitation_level, data=pddists[pddists$TTtreat!="TTC",], main="2009-2011", notch=TRUE, ylim=c(0.4,1.25),ylab="relative distance moved towards destination", col=TT.colours, las=2) ;abline(h=1)
boxplot(r12~TTtreat+Precipitation_level, data=pddists[pddists$TTtreat!="TTC",], main="2009-2012", notch=TRUE, ylim=c(0.4,1.25),ylab="relative distance moved towards destination", col=TT.colours, las=2) ;abline(h=1)
boxplot(r13~TTtreat+Precipitation_level, data=pddists[pddists$TTtreat!="TTC",], main="2009-2013", notch=TRUE, ylim=c(0.4,1.25),ylab="relative distance moved towards destination", col=TT.colours, las=2);abline(h=1)



x11();
boxplot(od~TTtreat, data=pddists, main="origin-destination", notch=TRUE, col=TT.colours, las=2) ;abline(h=1)


##directional difference

x11();plot(pddists$d11, pdists$d911[pdists$TTtreat!="TT1"], col=TT.colours[pddists$TTtreat], pch= 16, main="directional vs nondirectional 9-11", xlab="directed distance", ylab="undirected distance")
x11();plot(pddists$d13, pdists$d913[pdists$TTtreat!="TT1"], col=TT.colours[pddists$TTtreat], pch= 16, main="directional vs nondirectional 9-13", xlab="directed distance", ylab="undirected distance")


x11();
par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.5,.5,0))
plot(pddists$d11, pdists$d911[pdists$TTtreat!="TT1"], col=TT.colours[pddists$TTtreat], pch= c(16,NA,NA)[pddists$Temperature], main="directional vs nondirectional 9-11 - alpine transplants")
abline(0,1)
abline(v=0)
plot(pddists$d11, pdists$d911[pdists$TTtreat!="TT1"], col=TT.colours[pddists$TTtreat], pch= c(NA,16,NA)[pddists$Temperature], main="directional vs nondirectional 9-11 - intermediate transplants")
abline(0,1)
abline(v=0)
plot(pddists$d11, pdists$d911[pdists$TTtreat!="TT1"], col=TT.colours[pddists$TTtreat], pch= c(NA,NA,16)[pddists$Temperature], main="directional vs nondirectional 9-11 - lowland transplants")
abline(0,1)
abline(v=0)

x11();
par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.5,.5,0))
plot(pddists$d13, pdists$d913[dists$TTtreat!="TT1"], col=TT.colours[pddists$TTtreat], pch= c(16,NA,NA)[pddists$Temperature], main="directional vs nondirectional 9-13")
abline(0,1)
abline(v=0)
plot(pddists$d13, pdists$d913[dists$TTtreat!="TT1"], col=TT.colours[pddists$TTtreat], pch= c(NA,16,NA)[pddists$Temperature], main="directional vs nondirectional 9-13")
abline(0,1)
abline(v=0)
plot(pddists$d13, pdists$d913[dists$TTtreat!="TT1"], col=TT.colours[pddists$TTtreat], pch= c(NA,NA,16)[pddists$Temperature], main="directional vs nondirectional 9-13")
abline(0,1)
abline(v=0)

pddists[!is.na(pddists$d13),][pddists$d13[!is.na(pddists$d13)]<0&pddists$TTtreat[!is.na(pddists$d13)]!="TTC",-(6:11)]#bouncing sites

###########
x11();plot(pddists$od, pddists$d11, col=TT.colours[pddists$TTtreat], pch= c(24,16,25)[pddists$Temperature],xlab="Original distance", ylab="direction distance",main="2011")
x11();plot(pddists$od, pddists$r11, col=TT.colours[pddists$TTtreat], pch= c(24,16,25)[pddists$Temperature],xlab="Original distance", ylab="direction distance",main="2011")
x11();plot(pddists$od, pddists$r13, col=TT.colours[pddists$TTtreat], pch= c(24,16,25)[pddists$Temperature],xlab="Original distance", ylab="direction distance",main="2013")

x11();
par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.5,.5,0))
plot(pddists$od, pddists$d11, col=TT.colours[pddists$TTtreat], pch= c(16,NA,NA)[pddists$Temperature],xlab="Original distance", ylab="direction distance",main="2011 - alpine");abline(h=0)
plot(pddists$od, pddists$d11, col=TT.colours[pddists$TTtreat], pch= c(NA,16,NA)[pddists$Temperature], xlab="Original distance", ylab="direction distance",main="2011 - intermediate");abline(h=0)
plot(pddists$od, pddists$d11, col=TT.colours[pddists$TTtreat], pch= c(NA,NA,16)[pddists$Temperature],xlab="Original distance", ylab="direction distance",main="2011 - lowland");abline(h=0)


x11()
plot(ddists$od, pddists$od, col=TT.colours[pddists$TTtreat],xlab="Original distance", ylab="direction distance",main="2011 - lowland");abline(h=0)
plot(ddists$d13, pddists$d13, col=TT.colours[pddists$TTtreat],xlab="Original distance", ylab="direction distance",main="2011 - lowland");abline(h=0)
cor(ddists$d11, pddists$d11, use="pair")
cor(ddists$d12, pddists$d12, use="pair")
cor(ddists$d13, pddists$d13, use="pair")

cor(ddists$d11, fddists$d11, use="pair")
cor(ddists$d12, fddists$d12, use="pair")
cor(ddists$d13, fddists$d13, use="pair")

cor(pddists$d11, fddists$d11, use="pair")
cor(pddists$d12, fddists$d12, use="pair")
cor(pddists$d13, fddists$d13, use="pair")

by(cbind(ddists, pddists, fddists),ddists$TTtreat, function(x){
  names(x)<-paste(rep(c("", "p", "f"), each=ncol(ddists)), names(x), sep="")
#  print(names(x))

data.frame(cover.pa=c(
  c11=cor(x$d11, x$pd11, use="pair"),
  c12=cor(x$d12, x$pd12, use="pair"),
  c13=cor(x$d13, x$pd13, use="pair")
  ),
  cover.fre=c(
  cor(x$d11, x$fd11, use="pair"),
  cor(x$d12, x$fd12, use="pair"),
  cor(x$d13, x$fd13, use="pair")
  ),
  fre.pa=c(
  cor(x$fd11, x$pd11, use="pair"),
  cor(x$fd12, x$pd12, use="pair"),
  cor(x$fd13, x$pd13, use="pair")
  ))
})


lapply(1:3, function(elev){
  by(cbind(ddists, pddists, fddists)[ddists$Temperature_level==elev,],ddists$TTtreat[ddists$Temperature_level==elev], function(x){
    names(x)<-paste(rep(c("", "p", "f"), each=ncol(ddists)), names(x), sep="")
  
  m11<-cor.test(x$d11, x$pd11)
  m12<-cor.test(x$d12, x$pd12)
  m13<-cor.test(x$d13, x$pd13)
  
  
  rbind(
    c11=c(m11$estimate,m11$conf),
    c12=c(m12$estimate,m12$conf),
    c13=c(m13$estimate,m13$conf)
    )
  })
})


plot(ddists$r13, pddists$r13, col=TT.colours[pddists$TTtreat],xlab="Original distance", ylab="direction distance",main="2011 - lowland");abline(h=0)
cor(ddists$r13, pddists$r13, use="pair")