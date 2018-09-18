#Seedclim: analyzing and plotting phylogenetic patterns
#Brian Maitner.  BMaitner at gmail
#####################
#Metadata:

#Treaments (TTtreat)
#Levels: TTC TT1 TT2 TT3 TT4
#warmer (TT2) 
#wetter (TT3)
#warmer and wetter (TT4) climates, 

#transplanting within blocks (to control for the transplanting itself)(TT1), 
#untouched control plot (TTC)

#####################

#load packages
library(lmerTest)
library(lme4)
library(nlme)

#read in data
cover.meta<-readRDS(file = "phylogeny/cover_phylo.rds")
cover.meta$summerTemperature<-scale(cover.meta$summerTemperature)
cover.meta$annualPrecipitation<-scale(cover.meta$annualPrecipitation)

#Abundance weighted models

vntd.abd<- lme(vntd_abd_std~ annualPrecipitation +summerTemperature+year+TTtreat+year*TTtreat,
                   random=~1|blockID,data=cover.meta)

vpd.abd<- lme(vpd_abd_std~ annualPrecipitation +summerTemperature+year+TTtreat+year*TTtreat,
                  random=~1|blockID,data=cover.meta)

pd.abd<- lme(pd_abd_std~ annualPrecipitation +summerTemperature+year+TTtreat+year*TTtreat,
                 random=~1|blockID,data=cover.meta)

mpd.abd<- lme(mpd_abd_std~ annualPrecipitation +summerTemperature+year+TTtreat+year*TTtreat,
                  random=~1|blockID,data=cover.meta)

mntd.abd<- lme(mntd_abd_std~ annualPrecipitation +summerTemperature+year+TTtreat+year*TTtreat,
                   random=~1|blockID,data=cover.meta)


summary(vntd.abd)#temp,precip, t1, t1:year
summary(vpd.abd)#temp
summary(pd.abd)#temp, t3 effects, t3:year
summary(mpd.abd)#temp,T1,T1:year
summary(mntd.abd)#temp, T4, T4:year 

library(r2glmm)


r2glmm::r2beta(model = vntd.abd)#temp 0.05
r2glmm::r2beta(model = vpd.abd)#temp 0.18
r2glmm::r2beta(model = pd.abd)#temp 0.41
r2glmm::r2beta(model = mpd.abd)#temp 0.043
r2glmm::r2beta(model = mntd.abd)#temp 0.02


plot(cover.meta$pd_abd_std~cover.meta$summerTemperature)

library(sjPlot)
library(ggplot2)
plot_model(model = pd.abd,type="pred",terms = "summerTemperature")
