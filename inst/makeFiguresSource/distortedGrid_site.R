#as distorted grid but at a site level
#data as before but ?difference between site centroids as predictor?
#aggregate data? or use (g)lmer


#aggregate
agg.mm<-by(mm, list(mm$dists.siteID, mm$dists.TTtreat), function(x){
  x2<-colMeans(x[,c("dists.d11",  "dists.d12",  "dists.d13",                
   "dists.r11", "dists.r12",  "dists.r13", "extinctions.V1", "extinctions.V2",           
   "extinctions.V3",  "extinctions.rich", "immigration.1", "immigration.2", "immigration.3")])
   
  x3<-x[1,c("t1", "p1", "t2", "p2", "median", "deltaT", "deltaP",  "dists.siteID", "dists.TTtreat", 
            "dists.Temperature_level",   "dists.Precipitation_level", "dists.summerTemperature",  
            "dists.annualPrecipitation",  "dists.newTT")]
  out<-cbind(x3, as.data.frame(t(x2)))
  # print(out)
  out
})

agg.mm<-do.call(rbind,agg.mm)

with(agg.mm, plot(median, extinctions.V1, col=TT.colours[dists.TTtreat], pch=tmp.pch[dists.Temperature_level]))
with(agg.mm, plot(deltaT, extinctions.V1, col=TT.colours[dists.TTtreat], pch=tmp.pch[dists.Temperature_level]))
with(agg.mm, plot(deltaP, extinctions.V1, col=TT.colours[dists.TTtreat], pch=tmp.pch[dists.Temperature_level]))

with(agg.mm, plot(median, dists.d11, col=TT.colours[dists.TTtreat], pch=tmp.pch[dists.Temperature_level]))


library(lme4)
lmod0<-lmer(dists.d11~1+(1|dists.siteID), data=mm)
lmod1<-lmer(dists.d11~dists.TTtreat+(1|dists.siteID), data=mm)

anova(lmod1, lmod0)
summary(lmod1)
