#distance travelled tests
library(lme4)
mod<-lm(d13~Precipitation_level, data=pddists, subset=TTtreat=="TT2"&!is.na(d13))
summary(mod)

mod0<-lmer(d13~1+(1|siteID), data=pddists, subset=TTtreat=="TT2"&!is.na(d13))
summary(mod1)
mod1<-lmer(d13~as.factor(Precipitation_level)+(1|siteID), data=pddists, subset=TTtreat=="TT2"&!is.na(d13))
summary(mod1)
anova(mod0, mod1)
mod2<-lmer(d13~as.factor(Precipitation_level)+(1|siteID), data=pddists, subset=TTtreat=="TT2"&!is.na(d13))


pddists[, c("siteID", "TTtreat","Year","blockID","turfID", "Temperature_level","Precipitation_level","destBlockID","destSiteID","newTT","d11")],
pddists[, c("siteID", "TTtreat","Year","blockID","turfID", "Temperature_level","Precipitation_level","destBlockID","destSiteID","newTT","d12")],
pddists[, c("siteID", "TTtreat","Year","blockID","turfID", "Temperature_level","Precipitation_level","destBlockID","destSiteID","newTT","d13")]
)
library(reshape)
pddists2<-melt(pddists[,1:15], id=names(pddists)[1:12])


mod0<-lmer(value~1+(1|variable)+(1|blockID), data=pddists2, subset=TTtreat=="TT4")
mod1<-lmer(value~Precipitation_level+(1|variable)+(1|blockID), data=pddists2, subset=TTtreat=="TT4")


anova(mod0, mod1)
