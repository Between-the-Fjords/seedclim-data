
#bcol<-c("grey40", "grey80", "red", "blue", "purple")

#distance from origin
dists<-make.dists(cover)
x11()
plot(dists, temp=TRUE)
x11()
plot(dists, temp=FALSE)

#distance towards destination

#get turf treatment T in block B site S year Y, find distance to TT1 in block B site S year Y
#find difference between d2009 & d2011 etc
#boxplot, slice and dice

ddists<-make.directional.dists(cover)
 ddists[apply(is.na(ddists),1,any),-7]

x11()
plot(ddists, split=FALSE)
x11()
plot(ddists, split=TRUE, temperature=TRUE)
x11()
plot(ddists, split=TRUE, temperature=FALSE)

x11(pointsize=13)
x11()
plot(ddists, split=TRUE, temperature=FALSE, draw=2013)


#relative distances
x11();
par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.5,.5,0))
boxplot(r11~TTtreat+Temperature_level, data=ddists[ddists$TTtreat!="TTC",], main="2009-2011", notch=TRUE, ylim=c(0.4,1.25),ylab="relative distance moved towards destination", col=TT.colours, las=2) ;abline(h=1)
boxplot(r12~TTtreat+Temperature_level, data=ddists[ddists$TTtreat!="TTC",], main="2009-2012", notch=TRUE, ylim=c(0.4,1.25),ylab="relative distance moved towards destination", col=TT.colours, las=2) ;abline(h=1)
boxplot(r13~TTtreat+Temperature_level, data=ddists[ddists$TTtreat!="TTC",], main="2009-2013", notch=TRUE, ylim=c(0.4,1.25),ylab="relative distance moved towards destination", col=TT.colours, las=2);abline(h=1)



x11(6,6);
boxplot(od~TTtreat+Temperature_level, data=ddists, main="origin-destination distance", notch=TRUE, col=TT.colours, las=2) ;abline(h=1)


##directional difference

x11();plot(ddists$d11, dists$d911[dists$TTtreat!="TT1"], col=TT.colours[ddists$TTtreat], pch= 16, main="directional vs nondirectional 9-11", xlab="directed distance", ylab="undirected distance")
x11();plot(ddists$d13, dists$d913[dists$TTtreat!="TT1"], col=TT.colours[ddists$TTtreat], pch= 16, main="directional vs nondirectional 9-13", xlab="directed distance", ylab="undirected distance")


x11();
par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.5,.5,0))
plot(ddists$d11, dists$d911[dists$TTtreat!="TT1"], col=TT.colours[ddists$TTtreat], pch= c(16,NA,NA)[ddists$Temperature], main="directional vs nondirectional 9-11 - alpine transplants")
abline(0,1)
abline(v=0)
plot(ddists$d11, dists$d911[dists$TTtreat!="TT1"], col=TT.colours[ddists$TTtreat], pch= c(NA,16,NA)[ddists$Temperature], main="directional vs nondirectional 9-11 - intermediate transplants")
abline(0,1)
abline(v=0)
plot(ddists$d11, dists$d911[dists$TTtreat!="TT1"], col=TT.colours[ddists$TTtreat], pch= c(NA,NA,16)[ddists$Temperature], main="directional vs nondirectional 9-11 - lowland transplants")
abline(0,1)
abline(v=0)

x11();
par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.5,.5,0))
plot(ddists$d13, dists$d913[dists$TTtreat!="TT1"], col=TT.colours[ddists$TTtreat], pch= c(16,NA,NA)[ddists$Temperature], main="directional vs nondirectional 9-13")
abline(0,1)
abline(v=0)
plot(ddists$d13, dists$d913[dists$TTtreat!="TT1"], col=TT.colours[ddists$TTtreat], pch= c(NA,16,NA)[ddists$Temperature], main="directional vs nondirectional 9-13")
abline(0,1)
abline(v=0)
plot(ddists$d13, dists$d913[dists$TTtreat!="TT1"], col=TT.colours[ddists$TTtreat], pch= c(NA,NA,16)[ddists$Temperature], main="directional vs nondirectional 9-13")
abline(0,1)
abline(v=0)

ddists[!is.na(ddists$d13),][ddists$d13[!is.na(ddists$d13)]<0&ddists$TTtreat[!is.na(ddists$d13)]!="TTC",-(6:11)]#bouncing sites

###########
x11();plot(ddists$od, ddists$d11, col=TT.colours[ddists$TTtreat], pch= c(24,16,25)[ddists$Temperature],xlab="Original distance", ylab="direction distance",main="2011")
x11();plot(ddists$od, ddists$r11, col=TT.colours[ddists$TTtreat], pch= c(24,16,25)[ddists$Temperature],xlab="Original distance", ylab="relative distance",main="2011")
x11();plot(ddists$od, ddists$r13, col=TT.colours[ddists$TTtreat], pch= c(24,16,25)[ddists$Temperature],xlab="Original distance", ylab="relative distance",main="2013")

x11();
par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.5,.5,0))
plot(ddists$od, ddists$d11, col=TT.colours[ddists$TTtreat], pch= c(16,NA,NA)[ddists$Temperature],xlab="Original distance", ylab="direction distance",main="2011 - alpine");abline(h=0)
plot(ddists$od, ddists$d11, col=TT.colours[ddists$TTtreat], pch= c(NA,16,NA)[ddists$Temperature], xlab="Original distance", ylab="direction distance",main="2011 - intermediate");abline(h=0)
plot(ddists$od, ddists$d11, col=TT.colours[ddists$TTtreat], pch= c(NA,NA,16)[ddists$Temperature],xlab="Original distance", ylab="direction distance",main="2011 - lowland");abline(h=0)


##############################


#distance from origin
fdists<-make.dists(fsubturf)

x11()
plot(fdists, temp=TRUE)
x11()
plot(fdists, temp=FALSE)

#distance towards destination

#get turf treatment T in block B site S year Y, find distance to TT1 in block B site S year Y
#find difference between d2009 & d2011 etc
#boxplot, slice and dice
fddists<-make.directional.dists(fsubturf)
 fddists[apply(is.na(fddists),1,any),-7]

x11()
plot(fddists, split=FALSE)
x11()
plot(fddists, split=TRUE, temperature=TRUE)
x11()
plot(fddists, split=TRUE, temperature=FALSE)


#relative distances
x11();
par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.5,.5,0))
boxplot(r11~TTtreat+Temperature_level, data=fddists[fddists$TTtreat!="TTC",], main="2009-2011", notch=TRUE, ylim=c(0.4,1.25),ylab="relative distance moved towards destination", col=TT.colours, las=2) ;abline(h=1)
boxplot(r12~TTtreat+Temperature_level, data=fddists[fddists$TTtreat!="TTC",], main="2009-2012", notch=TRUE, ylim=c(0.4,1.25),ylab="relative distance moved towards destination", col=TT.colours, las=2) ;abline(h=1)
boxplot(r13~TTtreat+Temperature_level, data=fddists[fddists$TTtreat!="TTC",], main="2009-2013", notch=TRUE, ylim=c(0.4,1.25),ylab="relative distance moved towards destination", col=TT.colours, las=2);abline(h=1)



x11();
boxplot(od~TTtreat+Temperature_level, data=fddists, main="origin-destination", notch=TRUE, col=TT.colours) ;abline(h=1)


##directional difference

x11();plot(fddists$d11, fdists$d911[fdists$TTtreat!="TT1"], col=TT.colours[fddists$TTtreat], pch= 16, main="directional vs nondirectional 9-11", xlab="directed distance", ylab="undirected distance")
x11();plot(fddists$d13, fdists$d913[fdists$TTtreat!="TT1"], col=TT.colours[fddists$TTtreat], pch= 16, main="directional vs nondirectional 9-13", xlab="directed distance", ylab="undirected distance")


x11();
par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.5,.5,0))
plot(fddists$d11, fdists$d911[fdists$TTtreat!="TT1"], col=TT.colours[fddists$TTtreat], pch= c(16,NA,NA)[fddists$Temperature], main="directional vs nondirectional 9-11 - alpine transplants")
abline(0,1)
abline(v=0)
plot(fddists$d11, fdists$d911[fdists$TTtreat!="TT1"], col=TT.colours[fddists$TTtreat], pch= c(NA,16,NA)[fddists$Temperature], main="directional vs nondirectional 9-11 - intermediate transplants")
abline(0,1)
abline(v=0)
plot(fddists$d11, fdists$d911[fdists$TTtreat!="TT1"], col=TT.colours[fddists$TTtreat], pch= c(NA,NA,16)[fddists$Temperature], main="directional vs nondirectional 9-11 - lowland transplants")
abline(0,1)
abline(v=0)

x11();
par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.5,.5,0))
plot(fddists$d13, fdists$d913[fdists$TTtreat!="TT1"], col=TT.colours[fddists$TTtreat], pch= c(16,NA,NA)[fddists$Temperature], main="directional vs nondirectional 9-13")
abline(0,1)
abline(v=0)
plot(fddists$d13, fdists$d913[fdists$TTtreat!="TT1"], col=TT.colours[fddists$TTtreat], pch= c(NA,16,NA)[fddists$Temperature], main="directional vs nondirectional 9-13")
abline(0,1)
abline(v=0)
plot(fddists$d13, fdists$d913[fdists$TTtreat!="TT1"], col=TT.colours[fddists$TTtreat], pch= c(NA,NA,16)[fddists$Temperature], main="directional vs nondirectional 9-13")
abline(0,1)
abline(v=0)

fddists[!is.na(fddists$d13),][fddists$d13[!is.na(fddists$d13)]<0&fddists$TTtreat[!is.na(fddists$d13)]!="TTC",-(6:11)]#bouncing sites

###########
x11();plot(fddists$od, fddists$d11, col=TT.colours[fddists$TTtreat], pch= c(24,16,25)[fddists$Temperature],xlab="Original distance", ylab="direction distance",main="2011")
x11();plot(fddists$od, fddists$r11, col=TT.colours[fddists$TTtreat], pch= c(24,16,25)[fddists$Temperature],xlab="Original distance", ylab="direction distance",main="2011")
x11();plot(fddists$od, fddists$r13, col=TT.colours[fddists$TTtreat], pch= c(24,16,25)[fddists$Temperature],xlab="Original distance", ylab="direction distance",main="2013")

x11();
par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.5,.5,0))
plot(fddists$od, fddists$d11, col=TT.colours[fddists$TTtreat], pch= c(16,NA,NA)[fddists$Temperature],xlab="Original distance", ylab="direction distance",main="2011 - alpine");abline(h=0)
plot(fddists$od, fddists$d11, col=TT.colours[fddists$TTtreat], pch= c(NA,16,NA)[fddists$Temperature], xlab="Original distance", ylab="direction distance",main="2011 - intermediate");abline(h=0)
plot(fddists$od, fddists$d11, col=TT.colours[fddists$TTtreat], pch= c(NA,NA,16)[fddists$Temperature],xlab="Original distance", ylab="direction distance",main="2011 - lowland");abline(h=0)

#compare cover dists and fsubturf dists

x11()
plot(ddists$od, fddists$od, col=TT.colours[fddists$TTtreat],xlab="Original distance", ylab="direction distance",main="2011 - lowland");abline(h=0)
plot(ddists$d13, fddists$d13, col=TT.colours[fddists$TTtreat],xlab="Original distance", ylab="direction distance",main="2011 - lowland");abline(h=0)
cor(ddists$d13, fddists$d13, use="pair")