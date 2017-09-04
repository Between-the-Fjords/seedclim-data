###### Carbon flux data ######
library(tidyverse)

#### Reading in data and making dataframes ####

CO2_flux <- read.csv("Traits/data/CO2_GPP_1516Trait.csv", header=TRUE, sep=",")
#str(CO2_flux)

CO2_flux <- CO2_flux %>%
  select(site, block, treatment, vegHeight=vegHeight.x, Reco15, GPP1200)%>%
  mutate(site = recode(site, ULV = "Ulv", ALR = "Alr", FAU = "Fau", LAV = "Lav", HOG = "Hog", VIK = "Vik", GUD = "Gud", RAM = "Ram", ARH = "Arh", SKJ = "Skj", VES = "Ves", OVS = "Ovs")) %>%
  mutate(turfID=paste0(site, block, treatment))

CO2_traits_community <- full_join(wcommunity_df, CO2_flux, by=c("turfID"="turfID")) %>%
  mutate(site = factor(site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs")))%>%
  mutate(T_level = recode(site, Ulv = "Alpine", Lav = "Alpine",  Gud = "Alpine", Skj = "Alpine", Alr = "Sub-alpine", Hog = "Sub-alpine", Ram = "Sub-alpine", Ves = "Sub-alpine", Fau = "Boreal", Vik = "Boreal", Arh = "Boreal", Ovs = "Boreal"))

#### Merging with biomass data ####

CO2_mass_traits <- full_join(CO2_traits_community, distinct(biomass, turfID, total.biomass), by=c("turfID"="turfID"))

## Making subsets for comparison ##

com_CO2 <- CO2_mass_traits %>%
  select(Wmean_CN, Wmean_LDMC, Wmean_Lth, Wmean_LA, Wmean_SLA, Wmean_Height, cover, vegHeight, Reco15, GPP1200, total.biomass)



#### Correlations ####


GGally::ggpairs(com_CO2)

GGally::ggpairs(alp_com_CO2)
GGally::ggpairs(subalp_com_CO2)
GGally::ggpairs(bor_com_CO2)


#### Making linear models ####

library(lme4)
library(lmerTest)

model_0 <- lmer(GPP1200 ~ total.biomass + T_level + P_level + (1|site/turfID), data=CO2_mass_traits)

summary(model_0)
anova(model_0)

model_CN <- lmer(GPP1200 ~ total.biomass + T_level + P_level + Wmean_CN+ (1|site/turfID), data=CO2_mass_traits)

summary(model_CN)
anova(model_CN)

model_LDMC <- lmer(GPP1200 ~ total.biomass + T_level + P_level + Wmean_LDMC + (1|site/turfID), data=CO2_mass_traits)

summary(model_LDMC)
anova(model_LDMC)

model_Lth <- lmer(GPP1200 ~ total.biomass + T_level + P_level + Wmean_Lth + (1|site/turfID), data=CO2_mass_traits)

summary(model_Lth)
anova(model_Lth)

model_LA <- lmer(GPP1200 ~ total.biomass + T_level + P_level + Wmean_LA + (1|site/turfID), data=CO2_mass_traits)

summary(model_LA)
anova(model_LA)

model_SLA <- lmer(GPP1200 ~ total.biomass + T_level + P_level + Wmean_SLA + (1|site/turfID), data=CO2_mass_traits)

summary(model_SLA)
anova(model_SLA)

model_Height <- lmer(GPP1200 ~ total.biomass + T_level + P_level + Wmean_Height + (1|site/turfID), data=CO2_mass_traits)

summary(model_Height)
anova(model_Height)

model_all_traits <- lmer(GPP1200 ~ total.biomass + T_level + P_level + Wmean_CN + scale(Wmean_LDMC) + scale(Wmean_Lth) + Wmean_LA + Wmean_SLA + Wmean_Height + (1|site/turfID), data=CO2_mass_traits)

summary(model_all_traits)
anova(model_all_traits)

AIC(model_0, model_CN, model_LDMC, model_Lth, model_LA, model_SLA, model_Height, model_all_traits)



anova(model_0, model_CN)
anova(model_0, model_LDMC)
anova(model_0, model_Lth)
anova(model_0, model_LA)
anova(model_0, model_SLA)
anova(model_0, model_Height)

# According to the anova when comparing the null model with the other models, it is only CN ratio that makes it a better model.

## Plotting the model to check for things ##

par(mfrow=c(3,4))
a <- plot(model_0)
b <-plot(model_CN)
plot(model_LDMC)
plot(model_Lth)
plot(model_LA)
plot(model_SLA)
plot(model_Height)
par(mfrow=c(1,1))

gridExtra::grid.arrange(a,b)

qqnorm(residuals(model_0))
qqline(resid(model_0))

qqnorm(residuals(model_CN))
qqline(resid(model_CN))


model_CO2 <- CO2_mass_traits%>%
  filter(!is.na(total.biomass))

model_CO2$fit_0 <- predict(model_0)

ggplot(model_CO2, aes(x=total.biomass, y=GPP1200))+
  geom_point()

ggplot(model_CO2, aes(x=total.biomass, y=fit_0))+
  geom_point()


#### Making graphs to visualize ####

ggplot(CO2_mass_traits, aes(x=total.biomass, y=GPP1200, col=T_level))+
  geom_point()


ggplot(CO2_traits_community, aes(y=GPP1200, x=site, fill=T_level))+
  geom_boxplot()+
  facet_wrap(~ T_level, scales="free_x")

biomass%>%
  ggplot(aes(x=plotID, y=dry.weight, fill=functional.group))+
  geom_col()+
  facet_wrap(~site)

CO2_mass_traits%>%
  ggplot(aes(x= site, y=Wmean_CN, col= T_level))+
  geom_point()

CO2_mass_traits%>%
  ggplot(aes(x= site, y=GPP1200, col= T_level))+
  geom_point()

CO2_mass_traits%>%
  ggplot(aes(x= Wmean_CN, y=GPP1200, col= T_level))+
  geom_point()+
  geom_smooth(aes(group=1), method="lm")


