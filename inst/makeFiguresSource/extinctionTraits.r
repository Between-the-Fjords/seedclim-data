#extinctions
#not all extinctions, but potential extinctions
#by treatment
#predictors, traits, initial abundance, alpine/

#for treatment turfs (and control? - don't make much sense)
#get turf 2009 community (A)
#get destination control (entire block) 2009 community (B)
#find potential extinctions (A-B)
#find actual extinctions 2013


turfsppSubsets("1 TT2 28")

expectedunexpected<-as.data.frame(t(sapply(turfs$turfID[turfs$newTT!="control"], function(tu){
  potential<-potential.extinctions(tu)
  realised<-realised.extinctions(tu)
  potential<-potential[potential!="MISSING"]
  realised<-realised[realised!="MISSING"]
  expected<-intersect(realised, potential)
  unexpected<-setdiff(realised,potential)
  c(potential=length(potential), realised=length(realised), expected=length(expected),unexpected=length(unexpected))
  })))
expectedunexpected  
quantile(expectedunexpected$potential, na.rm=TRUE)  
quantile(expectedunexpected$expected/expectedunexpected$potential, na.rm=TRUE)  
quantile(expectedunexpected$unexpected/expectedunexpected$realised, na.rm=TRUE)  
    


TT4subsets<-extinctClasses(turfs$turfID[turfs$newTT=="TT4"])
apply(TT4subsets,1,function(k)sapply(k,length))


ex.all<-extinctClasses(turfs$turfID[turfs$newTT!="control"])
table(traits.for.species("alpine",   unlist(ex.all["potentialExtinctions",])))
table(traits.for.species("alpine",   unlist(ex.all["expectedExtinctions",])))

ex.TT2<-extinctClasses(turfs$turfID[turfs$newTT=="TT2"])
table(traits.for.species("alpine",   unlist(ex.TT2$potential)))
table(traits.for.species("alpine",   unlist(ex.TT2$expected)))
table(traits.for.species("alpine",   unlist(ex.TT2$unexpected)))

ex.TT2a<-extinctClasses(turfs$turfID[turfs$newTT=="TT2"&turfs$Temperature_level==1])
table(traits.for.species("alpine",   unlist(ex.TT2a$potential)))
table(traits.for.species("alpine",   unlist(ex.TT2a$expected)))
table(traits.for.species("alpine",   unlist(ex.TT2a$unexpected)))

mean(traits.for.species("SLA", unlist(ex.TT2a$potential)), na.rm=TRUE)
mean(traits.for.species("SLA", unlist(ex.TT2a$expected)), na.rm=TRUE)
t.test(traits.for.species("SLA", unlist(ex.TT2a$potential)),traits.for.species("SLA", unlist(ex.TT2a$expected)), na.rm=TRUE)

mean(traits.for.species("Max.height", unlist(ex.TT2a$potential)), na.rm=TRUE)
mean(traits.for.species("Max.height", unlist(ex.TT2a$expected)), na.rm=TRUE)

table(traits.for.species("Soil.type",   unlist(ex.TT2a$potential)))
table(traits.for.species("Soil.type",   unlist(ex.TT2a$expected)))

sapply(ex.TT2a, function(x)median(initial.abundances(x)))
sapply(ex.TT2a, function(x)median(initial.abundances(x, use.cover=FALSE)))

#pretty plots
#get trait by siteXtreatment
#plot proportion/mean trait for potential & expected extinctions
#1:1 line

#



trait.extinction.plot(levels(turfs$siteID),treatment="TT3", trait="alpine", numeric=FALSE, plot=TRUE)
trait.extinction.plot(levels(turfs$siteID),treatment="TT3", trait="family",char="Poaceae", numeric=FALSE)
trait.extinction.plot(levels(turfs$siteID),treatment="TT3", trait="SLA", numeric=TRUE)


#extinctions

malpine<-multitraits("alpine", numeric=FALSE)

plot(malpine, main="alpine")

mlow<-multitraits("lowland", numeric=FALSE)
plot(mlow, main="low")


traits$generalist<-!traits$alpine&!traits$lowland
mgen<-multitraits("generalist", numeric=FALSE)

x11(width=7, height=5);par(mfrow=c(1,3), oma=c(0,2.6,0,0),mar=c(3.1,0,.3,.3), mgp=c(1.1,.1,0), tcl=0.2)
ylim= range(unlist(list(malpine, mlow, mgen)), na.rm=TRUE)
plot(malpine, ylim=ylim, yaxt="n")
title( main="Alpine", line=-1)
plot(mgen, main="", ylim=ylim, yaxt="n")
title( main="Generalist", line=-1)
plot(mlow,  ylim=ylim, yaxt="n")
title( main="Lowland", line=-1)
axis(2, outer=TRUE)
title(ylab="Relative risk of extinction", outer=TRUE)




mSLA<-multitraits("SLA", numeric=TRUE)
plot(mSLA, main="SLA")

mpoa<-multitraits("family",char="Poaceae", numeric=FALSE)
x11();plot(mpoa, main="Poa")

mcyp<-multitraits("family",char="Cyperaceae", numeric=FALSE)
x11();plot(mcyp, main="Cyperaceae")

mforbs<-multitraits("functionalgroup",char="forb", numeric=FALSE)
x11();plot(mforbs, main="forbs")

mgram<-multitraits("functionalgroup",char="graminoid", numeric=FALSE)
x11();plot(mgram, main="graminoid")

mcyc<-multitraits("cyc",char="1", numeric=FALSE)
x11();plot(mcyc, main="cyc")

mheight<-multitraits("Max.height", numeric=TRUE)
plot(mheight, main="maxheight")

mseed<-multitraits("seedMass", numeric=TRUE)
plot(mseed, main="seedMass")

mleaf<-multitraits("leafSize", numeric=TRUE)
plot(mleaf, main="leafsize")

mlat<-multitraits("lat",char="<0.01", numeric=FALSE)
x11();plot(mlat, main="lat")


mcgo1<-multitraits("cgo",char="1", numeric=FALSE)
x11();plot(mcgo1, main="cgo1")

mcgo9<-multitraits("cgo",char="9", numeric=FALSE)
x11();plot(mcgo9, main="cgo9")

mcgo10<-multitraits("cgo",char="10", numeric=FALSE)
x11();plot(mcgo10)

mcgo14<-multitraits("cgo",char="14", numeric=FALSE)
x11();plot(mcgo14, main="cgo14")


#########immigration
malpine<-multitraits("alpine", numeric=FALSE, type="immigrant")
plot(malpine, main="alpine")

mlow<-multitraits("lowland", numeric=FALSE, type="immigrant")
plot(mlow, main="low")

mgen<-multitraits("generalist", numeric=FALSE,type="immigrant")
plot(mgen, main="generalist")

mSLA<-multitraits("SLA", numeric=TRUE, type="immigrant")
plot(mSLA, main="SLA")

mpoa<-multitraits("family",char="Poaceae", numeric=FALSE, type="immigrant")
x11();plot(mpoa, main="Poa")

mcyp<-multitraits("family",char="Cyperaceae", numeric=FALSE, type="immigrant")
x11();plot(mcyp)

mforbs<-multitraits("functionalgroup",char="forb", numeric=FALSE, type="immigrant")
x11();plot(mforbs)

mgram<-multitraits("functionalgroup",char="graminoid", numeric=FALSE, type="immigrant")
x11();plot(mgram)

mcyc<-multitraits("cyc",char="1", numeric=FALSE, type="immigrant")
x11();plot(mcyc)

mheight<-multitraits("Max.height", numeric=TRUE, type="immigrant")
plot(mheight)

mseed<-multitraits("seedMass", numeric=TRUE, type="immigrant")
plot(mseed)

mleaf<-multitraits("leafSize", numeric=TRUE, type="immigrant")
plot(mleaf)

mlat<-multitraits("lat",char="<0.01", numeric=FALSE, type="immigrant")
x11();plot(mlat)


mcgo1<-multitraits("cgo",char="1", numeric=FALSE, type="immigrant")
x11();plot(mcgo1)

mcgo9<-multitraits("cgo",char="9", numeric=FALSE, type="immigrant")
x11();plot(mcgo9, main="cgo9")

mcgo10<-multitraits("cgo",char="10", numeric=FALSE, type="immigrant")
x11();plot(mcgo10)

mcgo14<-multitraits("cgo",char="14", numeric=FALSE, type="immigrant")
x11();plot(mcgo14, main="cgo14")


#########persistant vs unexpected
malpine<-multitraits("alpine", numeric=FALSE, type="persistant")
plot(malpine, main="alpine")

mlow<-multitraits("lowland", numeric=FALSE, type="persistant")
plot(mlow, main="low")


mgen<-multitraits("generalist", numeric=FALSE,type="persistant")
plot(mgen, main="generalist")

mSLA<-multitraits("SLA", numeric=TRUE, type="persistant")
plot(mSLA, main="SLA")

mpoa<-multitraits("family",char="Poaceae", numeric=FALSE, type="persistant")
x11();plot(mpoa, main="Poa")

mcyp<-multitraits("family",char="Cyperaceae", numeric=FALSE, type="persistant")
x11();plot(mcyp)


mforbs<-multitraits("functionalgroup",char="forb", numeric=FALSE, type="persistant")
x11();plot(mforbs)

mgram<-multitraits("functionalgroup",char="graminoid", numeric=FALSE, type="persistant")
x11();plot(mgram)

mcyc<-multitraits("cyc",char="1", numeric=FALSE, type="persistant")
x11();plot(mcyc)

mheight<-multitraits("Max.height", numeric=TRUE, type="persistant")
plot(mheight)


mseed<-multitraits("seedMass", numeric=TRUE, type="persistant")
plot(mseed, main="seed mass")

mleaf<-multitraits("leafSize", numeric=TRUE, type="persistant")
plot(mleaf, main="leaf Size")

mlat<-multitraits("lat",char="<0.01", numeric=FALSE, type="persistant")
x11();plot(mlat)

mcgo1<-multitraits("cgo",char="1", numeric=FALSE, type="persistant")
x11();plot(mcgo1)

mcgo9<-multitraits("cgo",char="9", numeric=FALSE, type="persistant")
x11();plot(mcgo9, main="cgo9")

mcgo10<-multitraits("cgo",char="10", numeric=FALSE, type="persistant")
x11();plot(mcgo10)

mcgo14<-multitraits("cgo",char="14", numeric=FALSE, type="persistant")
x11();plot(mcgo14, main="cgo14")
