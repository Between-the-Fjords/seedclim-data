######## PCR plotting ##########
source("/Users/fja062/Desktop/New Folder With Items/data prep.R")

site <- "Ulvhaugen"

sapply(levels(cover.meta$siteID), function(site){
  species <- cover[cover.meta$siteID == site, ]
  species <- species[, colSums(species)>0]
  
  year <- as.factor(subset(cover.meta, siteID == site, Year)[, 1])
  treatment <- as.factor(subset(cover.meta, siteID == site, TTtreat)[,1])
  
  prc1 <- prc(response = sqrt(species), treatment = treatment, time = year)
  abundance <- as.data.frame(colSums(species)) # to select spp above k abundance
  plot(prc1, select = abundance > 10, main = site)
})

summary(prc1, axis = 1, scaling = 2, digits = 4)

rda1 <- rda(species ~ treatment * year + Condition(year))
coef(rda1)

ctrl <- how(within = Within(type = "series"), #, constant = TRUE <- is this necessary?
            plots = Plots(strata = explan$blockID),
            blocks = explan$siteID)
anova(prc1, permutations = ctrl)


mod <-rda(sqrt(species)~treatment, subset = year==2015)
plot(mod)
anova(mod)

#Coefficients for explan$TTtreat + explan$Year:explan$TTtreat interaction
#which are contrasts to explan$TTtreat TTC 
#rows are explan$TTtreat, columns are explan$Year
#      2011    2012 2013  2015
#RTC -2.203 -0.5149 10.7 7.943


rdafunction<-function(site, dat=species, ord=prc, ...){
  
  keep<-explan$siteID==site
  
  TT<-explan$TTtreat[keep]
  year<-explan$Year[keep]
  
  dat<-dat[keep,]
  #browser()  
  #run ord
  mod<-ord(dat, ...)
  plot(mod)
}

sapply(levels(explan$siteID), function(siteID){
  par(mfrow=c(1,1))
  rdafunction(site, dat=species, ord=prc)
  title(main=siteID)
})




#Plotting
plot(prc1, axis=1, cex.axis=1.5, cex.lab=1.5, type="b", pch=19, lwd=2, lty=c(1,5,1,5,1,5,1), legpos=NA, xlab="Year", ylab="PRC1", cex=1.2) #  select=abundance>25, col=c("black","red","red", "green","green","blue","blue")
legend("topleft", inset=.05, c("Control","Warming", "Nutrient addition","Warming + Nutrient addition", "", "Solid line = herbivore exclosure", "Dashed line = herbivores present"), pch=19, col=c(1, 2, 3, 4, 0, 0,0),horiz=FALSE,bty="n",cex=1,pt.cex=c(1.2,1.2,1.2,1.2))



Q
par(mar=c(3,3,1,1), mgp=c(1.5,.5,0))
x11();
hp("Hogsete", dat=freqsubturf)

sapply(levels(cover.meta$siteID), function(siteID){
  x11();
  hp(siteID, dat=cover, ord=metaMDS)
  title(main=siteID)
})


