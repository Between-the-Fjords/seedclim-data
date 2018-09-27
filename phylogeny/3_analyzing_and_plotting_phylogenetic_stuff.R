#analyzing and plotting phylogenetic patterns


#load packages
library(lmerTest)
library(lme4)
library(nlme)

#read in data
cover.meta<-readRDS(file = "phylogeny/cover_phylo.rds")

#Quick looks at non-abundance-weighted metrics
summary(lm(formula = cover.meta$mpd_std~cover.meta$TTtreat + cover.meta$precipitation_level +cover.meta$temperature_level+cover.meta$year))
summary(lm(formula = cover.meta$pd_std~cover.meta$TTtreat + cover.meta$precipitation_level +cover.meta$temperature_level+cover.meta$year))
mntd.out<- lme(mntd_std~TTtreat + precipitation_level +temperature_level+year +year*TTtreat,
               random=~1|turfID,data=cover.meta)

pd.out<- lme(pd_std~TTtreat + precipitation_level +temperature_level+year +year*TTtreat,
             random=~1|turfID,data=cover.meta)

mpd.out<- lme(mpd_std~TTtreat + precipitation_level +temperature_level+year +year*TTtreat,
              random=~1|turfID,data=cover.meta)


summary(mntd.out)
summary(pd.out)
summary(mpd.out)


mpd.abd.out<- lme(mpd_abd~TTtreat + precipitation_level +temperature_level+year +year*TTtreat,
                  random=~1|turfID,data=cover.meta)


mntd.abd.out<- lme(mntd_abd~TTtreat + precipitation_level +temperature_level+year +year*TTtreat,
                   random=~1|turfID,data=cover.meta)

pd.abd.out<- lme(pd_abd~TTtreat + precipitation_level +temperature_level+year +year*TTtreat,
                 random=~1|turfID,data=cover.meta)


summary(mpd.abd.out)
summary(mntd.abd.out)
summary(pd.abd.out)

summary(lm(formula = cover.meta$mpd_abd~cover.meta$TTtreat + cover.meta$precipitation_level +cover.meta$temperature_level+cover.meta$year))
summary(lm(formula = cover.meta$mntd_abd~cover.meta$TTtreat + cover.meta$precipitation_level +cover.meta$temperature_level+cover.meta$year))
summary(lm(formula = cover.meta$pd_abd~cover.meta$TTtreat + cover.meta$precipitation_level +cover.meta$temperature_level+cover.meta$year))
summary(lm(formula = cover.meta$vntd_abd~cover.meta$TTtreat + cover.meta$precipitation_level +cover.meta$temperature_level+cover.meta$year))
summary(lm(formula = cover.meta$vpd_abd~cover.meta$TTtreat + cover.meta$precipitation_level +cover.meta$temperature_level+cover.meta$year))



######################

#Make some figures!
