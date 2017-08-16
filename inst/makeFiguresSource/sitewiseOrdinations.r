#sitewise ordinations
#extract data
#
#by site
#by treatment TTC VS TT1:4 2013
#table siteXtreatment %explained + % explained + significance stars

ord.res<-sapply(levels(cover.meta$siteID), function(site){
  sapply(c("TT1","TT2","TT3","TT4"), function(treat){
    keep<-with(cover.meta,siteID==site&(TTtreat==treat|TTtreat=="TTC")&Year==2013)
    spp<-with(cover.meta,cover[keep,])
    block<-factor(cover.meta$blockID[keep])
    treat<-factor(cover.meta$TTtreat[keep])
    if(nlevels(treat)==1)return("-")

    mod<-rda(sqrt(spp)~treat)#+Condition(block))
    ex<-mod$CCA$tot/mod$tot
    p<-anova(mod)$Pr[1]
    paste(round(ex*100, 1), symnum(p, corr = FALSE, na = FALSE, 
                                cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                                symbols = c("***", "**", "*", ".", " ")))
  })
})
as.data.frame(t(ord.res))

#### for sites with all treatments
for(site in c("Alrust", "Gudmedalen", "Hogsete", "Lavisdalen", "Rambera", "Ulvhaugen")){
  print(site)
    keep<-with(cover.meta,siteID==site&TTtreat!="TTC"&Year==2013)
    spp<-cover[keep,]
    block<-factor(cover.meta$blockID[keep])
    treat<-factor(cover.meta$TTtreat[keep])
    warmer<-factor(treat%in%c("TT2", "TT4"))
    wetter<-factor(treat%in%c("TT3", "TT4"))
    
  mod<-rda(spp~warmer+wetter)#+warmer:wetter)
  print(mod)    
  print(anova(mod, by="margin"))
  mod2<-rda(spp~warmer+wetter+warmer:wetter)
  print(mod2)    
  print(anova(mod2, by="margin"))
  print("####################")
 
}



pstars<-function(x)cut(x, breaks=c(0,0.001,.01,.05,.1,1),labels=c("***", "**", "*", "."," "))



##Needs permutation tests by blocks
#### for sites with all treatments
ord.res2<-numeric(0)

for(site in c("Alrust", "Gudmedalen", "Hogsete", "Lavisdalen", "Rambera", "Ulvhaugen")){
  print(site)
  keep<-with(cover.meta,siteID==site&TTtreat!="TTC"&Year==2013)
  spp<-cover[keep,]
  block<-factor(cover.meta$blockID[keep])
  treat<-factor(cover.meta$TTtreat[keep])
  levels(treat)
  
  mod2<-rda(spp~tr+Condition(block), data=data.frame(tr=treat=="TT2", block=block))
  mod3<-rda(spp~tr+Condition(block), data=data.frame(tr=treat=="TT3", block=block))
  mod4<-rda(spp~tr+Condition(block), data=data.frame(tr=treat=="TT4", block=block))
  
  av2<-anova(mod2, by="margin")
  av3<-anova(mod3, by="margin")
  av4<-anova(mod4, by="margin")
  
  v2<-round(mod2$CCA$tot.chi/(mod2$tot.chi-mod2$pCCA$tot.chi)*100, 1)
  v3<-round(mod3$CCA$tot.chi/(mod3$tot.chi-mod3$pCCA$tot.chi)*100, 1)
  v4<-round(mod4$CCA$tot.chi/(mod4$tot.chi-mod4$pCCA$tot.chi)*100,1)

  pstars(av2$Pr[1])
  paste(c(v2,v3,v4),pstars(c(av2$Pr[1],av3$Pr[1],av4$Pr[1])))
  ord.res2<-rbind(ord.res2,c(site, paste(c(v2,v3,v4),pstars(c(av2$Pr[1],av3$Pr[1],av4$Pr[1])))))
  print("####################")
  
}
ord.res2

##Needs permutation tests by blocks
#### for sites with all treatments
ord.res2<-numeric(0)

sites <-c("Alrust", "Gudmedalen", "Hogsete", "Lavisdalen", "Rambera", "Ulvhaugen")
  keep<-with(cover.meta,siteID%in%sites&TTtreat!="TTC"&Year==2013)
  spp<-cover[keep,]
site<-factor(cover.meta$siteID[keep])
block<-factor(cover.meta$blockID[keep])
treat<-factor(cover.meta$TTtreat[keep])
  levels(treat)
  

mod<-cca(spp~treat+Condition(site))
plot(mod)
text(mod, display="cn")

screeplot(mod, bstick=TRUE)

anova(mod)


  mod2<-rda(spp~tr+Condition(block), data=data.frame(tr=treat=="TT2", block=block))
  mod3<-rda(spp~tr+Condition(block), data=data.frame(tr=treat=="TT3", block=block))
  mod4<-rda(spp~tr+Condition(block), data=data.frame(tr=treat=="TT4", block=block))
  
  av2<-anova(mod2, by="margin")
  av3<-anova(mod3, by="margin")
  av4<-anova(mod4, by="margin")
  
  v2<-round(mod2$CCA$tot.chi/(mod2$tot.chi-mod2$pCCA$tot.chi)*100, 1)
  v3<-round(mod3$CCA$tot.chi/(mod3$tot.chi-mod3$pCCA$tot.chi)*100, 1)
  v4<-round(mod4$CCA$tot.chi/(mod4$tot.chi-mod4$pCCA$tot.chi)*100,1)
  
  pstars(av2$Pr[1])
  paste(c(v2,v3,v4),pstars(c(av2$Pr[1],av3$Pr[1],av4$Pr[1])))
  ord.res2<-rbind(ord.res2,c(site, paste(c(v2,v3,v4),pstars(c(av2$Pr[1],av3$Pr[1],av4$Pr[1])))))
  print("####################")
  
}
ord.res2

