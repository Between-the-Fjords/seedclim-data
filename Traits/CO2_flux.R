###### Carbon flux data ######
library(tidyverse)

#### Reading in data and making dataframes ####

CO2_flux <- read.csv("Traits/data/CO2_GPP_1516Trait.csv", header=TRUE, sep=",")
#str(CO2_flux)

CO2_flux <- CO2_flux %>%
  select(chamber, site, block, treatment, vegHeight.x, Reco15, GPP1200)%>%
  rename(vegHeight= vegHeight.x) %>%
  mutate(site = recode(site, ULV = "Ulv", ALR = "Alr", FAU = "Fau", LAV = "Lav", HOG = "Hog", VIK = "Vik", GUD = "Gud", RAM = "Ram", ARH = "Arh", SKJ = "Skj", VES = "Ves", OVS = "Ovs")) %>%
  mutate(turfID=paste0(site, block, treatment))

CO2_traits_community <- full_join(short_wcommunity, CO2_flux, by=c("turfID"="turfID")) %>%
  mutate(site = factor(site, levels = c("Ulv", "Lav", "Gud", "Skj", "Alr", "Hog", "Ram", "Ves", "Fau", "Vik", "Arh", "Ovs")))%>%
  mutate(T_level = recode(site, Ulv = "Alpine", Lav = "Alpine",  Gud = "Alpine", Skj = "Alpine", Alr = "Sub-alpine", Hog = "Sub-alpine", Ram = "Sub-alpine", Ves = "Sub-alpine", Fau = "Boreal", Vik = "Boreal", Arh = "Boreal", Ovs = "Boreal"))

#### Merging with biomass data ####

CO2_mass_traits <- full_join(CO2_traits_community, biomass, by=c("turfID"="turfID"))

## Making subsets of the temperature levels ##

alp_com_CO2 <- CO2_mass_traits %>%
  filter(T_level=="Alpine")%>%
  select(Wmean_CN, Wmean_LDMC, Wmean_Lth, Wmean_LA, Wmean_SLA, Wmean_Height, cover, vegHeight, Reco15, GPP1200, total.biomass)

subalp_com_CO2 <- CO2_mass_traits  %>%
  filter(T_level=="Sub-alpine")%>%
  select(Wmean_CN, Wmean_LDMC, Wmean_Lth, Wmean_LA, Wmean_SLA, Wmean_Height, cover, vegHeight, Reco15, GPP1200, total.biomass)

bor_com_CO2 <- CO2_mass_traits %>%
  filter(T_level=="Boreal")%>%
  select(Wmean_CN, Wmean_LDMC, Wmean_Lth, Wmean_LA, Wmean_SLA, Wmean_Height, cover, vegHeight, Reco15, GPP1200, total.biomass)

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

model_0 <- lmer(GPP1200 ~ 1 + (1|site/species/functionalGroup), data=CO2_traits_community)

model_SLA <- lmer(GPP1200 ~ Wmean_SLA + (1|site/species/functionalGroup), data=CO2_traits_community)

model_cover <- lmer(GPP1200 ~ cover + (1|site/species/functionalGroup), data=CO2_traits_community) #Skiping this because the AIC is higher than the null model

model_LA <- lmer(GPP1200 ~ Wmean_LA + (1|site/species/functionalGroup), data=CO2_traits_community)

model_Height <- lmer(GPP1200 ~ Wmean_Height + (1|site/species/functionalGroup), data=CO2_traits_community)

model_Lth <- lmer(GPP1200 ~ Wmean_Lth + (1|site/species/functionalGroup), data=CO2_traits_community)

model_LDMC <- lmer(GPP1200 ~ Wmean_LDMC + (1|site/species/functionalGroup), data=CO2_traits_community)

model_CN <- lmer(GPP1200 ~ Wmean_CN + (1|site/species/functionalGroup), data=CO2_traits_community)


AIC(model_0, model_SLA, model_LA, model_Height, model_Lth, model_LDMC, model_CN, model_cover)


#The best model when I only used site, species as random factors was the one with height of the species, and then leaf area and then SLA. 

#The best model when I used site, species and functional group as a random factor was the one with Height, then leaf area, then SLA.



#### Maing graphs to visualize ####


ggplot(CO2_traits_community, aes(x=vegHeight, y=GPP1200))+
  geom_point()+
  geom_smooth(method="gam", formula = y ~ s(x, bs = "cs"))

ggplot(CO2_mass_traits, aes(x=total.biomass, y=GPP1200))+
  geom_point()


ggplot(CO2_traits_community, aes(y=GPP1200, x=site, fill=T_level))+
  geom_boxplot()+
  facet_wrap(~ T_level, scales="free_x")


#### Models with several traits ####

com_mod_1 <- lmer(GPP1200 ~ Wmean_Height*Wmean_LA*Wmean_SLA*Wmean_CN*Wmean_Lth*Wmean_LDMC + (1|Site/species), data=CO2_traits_community)

com_mod_1 <- lmer(GPP1200 ~ Wmean_Height*Wmean_LA*Wmean_SLA*Wmean_CN*Wmean_Lth + (1|Site/species), data=CO2_traits_community)
summary(com_mod_1)


