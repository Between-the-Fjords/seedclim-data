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


summary(vntd.abd)#temp,precip
summary(vpd.abd)#temp
summary(pd.abd)#temp
summary(mpd.abd)#temp
summary(mntd.abd)#T4, T4:year 

library(r2glmm)


r2glmm::r2beta(model = vntd.abd)#temp 0.05
r2glmm::r2beta(model = vpd.abd)#temp 0.20
r2glmm::r2beta(model = pd.abd)#temp 0.38
r2glmm::r2beta(model = mpd.abd)#temp 0.21
r2glmm::r2beta(model = mntd.abd)#temp 0.02


plot(cover.meta$pd_abd_std~cover.meta$summerTemperature)

library(sjPlot)
library(ggplot2)
plot_model(model = pd.abd,type="pred",terms = c("summerTemperature"),axis.title = c("summer temperature","sesPD"),title = "Predicted values",axis.lim = c(-10,10))
plot_model(model = vpd.abd,type="pred",terms = c("summerTemperature"),axis.title = c("summer temperature","sesVPD"),title = "Predicted values",axis.lim = c(-10,10))
plot_model(model = mpd.abd,type="pred",terms = c("summerTemperature"),axis.title = c("summer temperature","sesMPD"),title = "Predicted values",axis.lim = c(-10,10))

?plot_model

