#load packages
library(lmerTest)
library(lme4)
library(nlme)

#read in data
cover.meta<-readRDS(file = "phylogeny/cover_phylo_trait_trait_scaled.rds")
cover.meta$summerTemperature<-scale(cover.meta$summerTemperature)
cover.meta$annualPrecipitation<-scale(cover.meta$annualPrecipitation)
load("phylogeny/Cflux_Norway_FunCaB_XC_TTC.RData")
cflux<-CO2_GPP_1516
rm(CO2_GPP_1516)
cover.meta$turfID2<-gsub(pattern = " ",replacement = "",x = cover.meta$turfID)
cflux<-cflux[c("turfID.x","year","Reco15","GPP700")]
cover.meta<-merge(x = cover.meta,y = cflux,by.x = c("turfID2","year"),by.y = c("turfID.x","year"),all.x = T)
rm(cflux)

#################################################################
#################################################################

#Which metrics best detect treament effects?


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

#
vntraitd.abd<- lme(vntraitd_abd_std~ annualPrecipitation +summerTemperature+year+TTtreat+year*TTtreat,
               random=~1|blockID,data=cover.meta)

vtraitd.abd<- lme(vtraitd_abd_std~ annualPrecipitation +summerTemperature+year+TTtreat+year*TTtreat,
              random=~1|blockID,data=cover.meta)

#pd.abd<- lme(pd_abd_std~ annualPrecipitation +summerTemperature+year+TTtreat+year*TTtreat,
#             random=~1|blockID,data=cover.meta)

mtraitd.abd<- lme(mtraitd_abd_std~ annualPrecipitation +summerTemperature+year+TTtreat+year*TTtreat,
              random=~1|blockID,data=cover.meta)

mntraitd.abd<- lme(mntraitd_abd_std~ annualPrecipitation +summerTemperature+year+TTtreat+year*TTtreat,
               random=~1|blockID,data=cover.meta)




summary(vntd.abd)#+temp,precip
summary(vntraitd.abd)#-temp

summary(vpd.abd)#-temp
summary(vtraitd.abd)#(-temp),T4,T4:year

summary(pd.abd)#temp
#summary(traitd.abd)#

summary(mpd.abd)#-temp
summary(mtraitd.abd)#-temp

summary(mntd.abd)#(+temp),year, T4, T4:year 
summary(mntraitd.abd)#+temp, T3,T3:Year





library(r2glmm)


r2glmm::r2beta(model = vntd.abd)#temp 0.05
r2glmm::r2beta(model = vpd.abd)#temp 0.20 #!Better than traits
r2glmm::r2beta(model = pd.abd)#temp 0.38
r2glmm::r2beta(model = mpd.abd)#temp 0.21
r2glmm::r2beta(model = mntd.abd)#temp 0.02


r2glmm::r2beta(model = vntraitd.abd)#temp 0.08
r2glmm::r2beta(model = vtraitd.abd)#temp 0.03
#r2glmm::r2beta(model = traitd.abd)#temp
r2glmm::r2beta(model = mtraitd.abd)#temp 0.31
r2glmm::r2beta(model = mntraitd.abd)#temp 0.10

#################################################################
#################################################################

#Which metrics best explain fluxes?

#Flux data columns: "Reco15","GPP700

#stargazer may be appropriate here










####################################################





library(ape)
tree_og<-read.tree("phylogeny/phylogenies/gbotb_base_rep_1.tre")
tree_trait<-read.tree("phylogeny/trait_scaled_phylogenies/gbotb_base_rep_1_scaled_rep_1.tre")
tree_rate<-read.tree("phylogeny/trait_rate_scaled_phylogenies/gbotb_base_rep_1_rate_scaled_rep_1.tre")

plot(tree_og,show.tip.label = F)
plot(tree_trait,show.tip.label = F)
plot(tree_rate,show.tip.label = F)


