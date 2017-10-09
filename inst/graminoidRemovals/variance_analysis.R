library(broom)
library(lme4)
library(nlme)


#http://r.789695.n4.nabble.com/How-to-extract-parameter-estimates-of-variance-function-from-lme-fit-td2997153.html
####------------- Analyses -------------####
# testing for differences in variance between forbs before and after treatment #

# against temperature
x <- wholecom %>% 
  select(wmean_CN_local, wmean_LDMC_local, wmean_SLA_local, wmean_LTH_local, wmean_seedMass, functionalGroup, TTtreat, Year, Temperature_level, Precipitation_level, siteID, blockID, funYear) %>% 
  gather(key = wmean_trait, value = measurement, c(wmean_CN_local, wmean_LDMC_local, wmean_SLA_local, wmean_LTH_local, wmean_seedMass)) %>%
  filter(TTtreat == "RTC", !is.na(measurement)) %>% 
  group_by(wmean_trait) %>%
  do({
    mod <- lme(measurement ~ Temperature_level, data = ., subset = funYear %in% c("forb_2011", "forb_2016"), random = ~ 1|siteID, weights = varIdent(form = ~ 1|funYear), na.action = "na.omit")
    mod0 <- lme(measurement ~ Temperature_level, data = ., subset = funYear %in% c("forb_2011", "forb_2016"), random = ~ 1|siteID, na.action = "na.omit")
    anova(mod, mod0)}
    ) %>%
  select(-call)
    
wholecom %>% 
  select(wmean_CN_local, wmean_LDMC_local, wmean_SLA_local, wmean_LTH_local, wmean_seedMass, functionalGroup, TTtreat, Year, Temperature_level, Precipitation_level, siteID, blockID, funYear) %>% 
  gather(key = wmean_trait, value = measurement, c(wmean_CN_local, wmean_LDMC_local, wmean_SLA_local, wmean_LTH_local, wmean_seedMass)) %>%
  filter(TTtreat == "RTC", !is.na(measurement)) %>% 
  group_by(wmean_trait) %>%
  do({
    mod <- lme(measurement ~ Temperature_level, data = ., subset = funYear %in% c("forb_2011", "forb_2016"), random = ~ 1|siteID, weights = varIdent(form = ~ 1|funYear), na.action = "na.omit")
    mod0 <- lme(measurement ~ Temperature_level, data = ., subset = funYear %in% c("forb_2011", "forb_2016"), random = ~ 1|siteID, na.action = "na.omit")
    as.data.frame(coef(mod$modelStruct$varStruct, unconstrained = FALSE))
  })

# against precipitation
precip_var <- wholecom %>% 
  select(wmean_CN_local, wmean_LDMC_local, wmean_SLA_local, wmean_LTH_local, wmean_seedMass, functionalGroup, TTtreat, Year, Temperature_level, Precipitation_level, siteID, blockID, funYear) %>% 
  gather(key = wmean_trait, value = measurement, c(wmean_CN_local, wmean_LDMC_local, wmean_SLA_local, wmean_LTH_local, wmean_seedMass)) %>%
  filter(TTtreat == "RTC", !is.na(measurement)) %>% 
  group_by(wmean_trait) %>%
  do({
    mod <- lme(measurement ~ Precipitation_level, data = ., subset = funYear %in% c("forb_2011", "forb_2016"), random = ~ 1|siteID, weights = varIdent(form = ~ 1|funYear), na.action = "na.omit")
    mod0 <- lme(measurement ~ Precipitation_level, data = ., subset = funYear %in% c("forb_2011", "forb_2016"), random = ~ 1|siteID, na.action = "na.omit")
    anova(mod, mod0)})


# test for difference in means of forbs in 2011 and 2016 and graminoids in 2011 in TTCs and RTCs
wholecom %>% 
  select(wmean_CN_local, wmean_LDMC_local, wmean_SLA_local, wmean_LA_local, wmean_LTH_local, wmean_seedMass, functionalGroup, TTtreat, Year, Temperature_level, siteID, blockID) %>% 
  gather(key = wmean_trait, value = measurement, c(wmean_CN_local, wmean_LDMC_local, wmean_SLA_local, wmean_LA_local, wmean_LTH_local, wmean_seedMass)) %>%
  filter(TTtreat == "TTC", Year == 2011, !is.na(measurement), functionalGroup == "forb") %>% 
  group_by(wmean_trait) %>%
  do({
    mod <- lmer(measurement ~ Temperature_level + (1|siteID), data = .)
    tidy(mod)}) %>% 
  filter(term == "Temperature_level")

wholecom %>% 
  select(wmean_CN_local, wmean_LDMC_local, wmean_SLA_local, wmean_LA_local, wmean_LTH_local, wmean_seedMass, functionalGroup, TTtreat, Year, Precipitation_level, siteID, blockID) %>% 
  gather(key = wmean_trait, value = measurement, c(wmean_CN_local, wmean_LDMC_local, wmean_SLA_local, wmean_LA_local, wmean_LTH_local, wmean_seedMass)) %>%
  filter(TTtreat == "TTC", Year == 2011, !is.na(measurement), functionalGroup == "forb") %>% 
  group_by(wmean_trait) %>%
  do({
    mod <- lmer(measurement ~ Precipitation_level + (1|siteID), data = .)
    tidy(mod)}) %>% 
  filter(term == "Precipitation_level")



# test for difference in means of forbs and graminoids in TTCs in 2011
mod <- wholecom %>% 
  select(wmean_CN_local, wmean_LDMC_local, wmean_SLA_local, wmean_LA_local, wmean_LTH_local, wmean_seedMass, functionalGroup, TTtreat, Year, Temperature_level, Precipitation_level, siteID, blockID) %>% 
  gather(key = wmean_trait, value = measurement, c(wmean_CN_local, wmean_LDMC_local, wmean_SLA_local, wmean_LA_local, wmean_LTH_local, wmean_seedMass)) %>%
  filter(TTtreat == "TTC", Year == 2011, !is.na(measurement)) %>% 
  group_by(wmean_trait) %>%
  do({
    mod <- lmer(measurement ~ Precipitation_level*Temperature_level*functionalGroup + (1|siteID), data = .)
    tidy(mod)}) %>% 
  filter(term != "(Intercept)") %>% 
  arrange(desc(term))

####------------ Plots --------------####
# plot for IAVS conference
traitVariance %>% 
  filter(TTtreat == "TTC", Year == 2011, wmean_trait %in% c("wmean_SLA_local")) %>% 
  ggplot(aes(measurement, fill = factor(Temperature_level))) +
  scale_fill_manual(values = cbPalette[c(3, 4, 2,7)]) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  axis.dim +
  facet_wrap(~ functionalGroup, scales = "free") +
  labs(x = "SLA", fill = "Temperature (C)") +
  ggsave(filename = paste0("IAVS_SLA_functionalgroup_temp.jpg"), height = 4, width = 8, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")

# plot 2 for IAVS conference
wholecom %>% 
  filter(TTtreat == "RTC") %>% 
  filter(funYear %in% c("forb_2011", "forb_2016", "graminoid_2011")) %>% 
  ggplot(aes(wmean_SLA_local, fill = factor(funYear))) +
  scale_fill_manual(values = rev(cbPalette[c(10, 4, 2, 5)])) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  axis.dim +
  facet_wrap(~ Temperature_level, scales = "free") +
  labs(x = "CN ratio", fill = "Functional groups \n in 2011 and 2016") +
  ggsave(filename = paste0("IAVS_CN_functionalgroup_TTtreat.jpg"), height = 4, width = 10.5, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")




wholecom %>% 
  gather(key = wmean_trait, value = measurement, c(wmean_CN_local, wmean_LDMC_local, wmean_SLA_local, wmean_LA_local, wmean_LTH_local, wmean_seedMass)) %>%
  filter(functionalGroup == "forb", TTtreat == "TTC", Year == 2011, wmean_trait %in% c("wmean_CN_local", "wmean_SLA_local", "wmean_seedMass")) %>% 
  ggplot(aes(measurement, fill = factor(Temperature_level))) +
  scale_fill_manual(values = cbPalette[c(10, 4, 2)]) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  axis.dim +
  facet_wrap(~ wmean_trait, scales = "free")
  