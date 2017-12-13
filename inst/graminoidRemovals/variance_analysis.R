library(broom)
library(lme4)
library(nlme)

traitOrder <- c("wmeanheight", "wmeanSLA", "wmeanLTH", "wmeanLDMC", "wmeanCN", "wmeanseedMass")

traitVariance <- wholecom %>%
  mutate(wmeanseedMass_local = wmean_seedMass) %>% 
  select(TTtreat, Year, Temperature_level, Precipitation_level, siteID, blockID, turfID ,functionalGroup, c(wmeanLDMC_local:funYear), wmeanseedMass_local, species) %>% 
  select(-wmeanLA_local) %>% 
  gather(key = trait, value = measurement, c(wmeanLDMC_local:wmeanCN_local, wmeanseedMass_local))

traitTrait <- wholecom %>%
  mutate(wmeanseedMass_local = wmean_seedMass) %>% 
  select(TTtreat, Year, Temperature_level, Precipitation_level, siteID, blockID, turfID ,functionalGroup, c(wmeanLDMC_local:funYear), wmeanseedMass_local, species) %>% 
  select(-wmeanLA_local) %>% 
  gather(key = trait, value = measurement, c(wmeanLDMC_local:wmeanCN_local, wmeanseedMass_local)) %>%
  separate(trait, c("trait", "scale"), sep = "_") %>% 
  spread(trait, measurement)

traitScale <- wholecom %>%
  mutate(wmeanseedMass_local = wmean_seedMass) %>% 
  select(TTtreat, Year, Temperature_level, Precipitation_level, siteID, blockID, turfID ,functionalGroup, c(wmeanLDMC_local:funYear), wmeanseedMass_local, species) %>% 
  select(-wmeanLA_local) %>% 
  gather(key = trait, value = measurement, c(wmeanLDMC_local:wmeanCN_local, wmeanseedMass_local)) %>% 
  separate(trait, c("trait", "scale"), sep = "_") %>% 
  select(-c(wmeanLDMC_global:wmeanheight_global))

traitScale <- arrange(mutate(traitScale, trait = factor(trait,levels=traitOrder)), trait)


#http://r.789695.n4.nabble.com/How-to-extract-parameter-estimates-of-variance-function-from-lme-fit-td2997153.html

####------------- Analyses -------------####
# testing for differences in variance between forbs before and after treatment #
# against temperature
modvar1temp <- traitScale %>% 
  filter(scale == "local", TTtreat == "RTC", !is.na(measurement)) %>% 
  group_by(trait) %>%
  do({
    mod <- lme(measurement ~ Temperature_level, data = ., subset = funYear %in% c("forb_2011", "forb_2016"), random = ~ 1|siteID, weights = varIdent(form = ~ 1|funYear*Temperature_level), na.action = "na.omit")
    mod0 <- lme(measurement ~ Temperature_level, data = ., subset = funYear %in% c("forb_2011", "forb_2016"), random = ~ 1|siteID, weights = varIdent(form = ~ 1|Temperature_level), na.action = "na.omit")
    anova(mod, mod0)}
    ) %>%
  select(-call)
    
modvar2temp <- traitScale %>% 
  filter(scale == "local", TTtreat == "RTC", !is.na(measurement)) %>%
  mutate(Temperature_level = as.factor(Temperature_level)) %>% 
  group_by(trait) %>%
  do({
    mod <- lme(measurement ~ Temperature_level, data = ., subset = funYear %in% c("forb_2011", "forb_2016"), random = ~ 1|siteID, weights = varIdent(form = ~ 1|funYear*Temperature_level), na.action = "na.omit")
    mod <- mod$modelStruct$varStruct
    dat <- data_frame(var = attr(mod, "groupNames"), coef = c(1, coef(mod, unconstrained = FALSE)))
    dat
  }) %>% 
  mutate(Year = substr(var, 6, 9), Temp = substr(var, 11, nchar(var))) %>% group_by(trait, Temp) %>% mutate(varDiff = coef[Year == 2016]-coef[Year == 2011]) %>% 
  filter(Year == 2016)

par(mfrow = c(3,3)) 
modvar2atemp <- traitScale %>% 
  filter(scale == "local", TTtreat == "RTC", !is.na(measurement)) %>%
  #mutate(Temperature_level = as.factor(Temperature_level)) %>% 
  group_by(trait) %>%
  do({
    mod <- lme(measurement ~ Temperature_level, data = ., subset = funYear %in% c("forb_2011", "forb_2016"), random = ~ 1|siteID, weights = varIdent(form = ~ 1|funYear*Temperature_level), na.action = "na.omit")
    tidy(mod)
    qqnorm(residuals(mod), main = .$trait); qqline(residuals(mod))
  }) %>%
  distinct(trait, group, term, estimate) %>% 
  arrange(desc(trait))


#####
# against precipitation
modvar1precip <- traitScale %>% 
  filter(scale == "local", TTtreat == "RTC", !is.na(measurement)) %>% 
  group_by(trait) %>%
  do({
    mod <- lme(measurement ~ Precipitation_level, data = ., subset = funYear %in% c("forb_2011", "forb_2016"), random = ~ 1|siteID, weights = varIdent(form = ~ 1|funYear*Precipitation_level), na.action = "na.omit")
    mod0 <- lme(measurement ~ Precipitation_level, data = ., subset = funYear %in% c("forb_2011", "forb_2016"), random = ~ 1|siteID, weights = varIdent(form = ~ 1|Precipitation_level), na.action = "na.omit")
    anova(mod, mod0)}) %>%
  select(-call)


modvar2precip <- traitScale %>% 
  filter(scale == "local", TTtreat == "RTC", !is.na(measurement)) %>% 
  group_by(trait) %>%
  do({
    mod <- lme(measurement ~ Precipitation_level, data = ., subset = funYear %in% c("forb_2011", "forb_2016"), random = ~ 1|siteID, weights = varIdent(form = ~ 1|funYear*Precipitation_level), na.action = "na.omit")
    mod <- mod$modelStruct$varStruct
    dat <- data_frame(var = attr(mod, "groupNames"), coef = c(1,coef(mod, unconstrained = FALSE)))
    dat
  }) %>% 
  mutate(Year = substr(var, 6, 9), prec = substr(var, 11, nchar(var))) %>% group_by(trait, prec) %>% mutate(varDiff = coef[Year == 2016]-coef[Year == 2011]) %>% distinct(trait, prec, .keep_all = TRUE)

par(mfrow = c(3,3))
modvar2aprecip <- traitScale %>% 
  filter(scale == "local", TTtreat == "RTC", !is.na(measurement)) %>%
  mutate(Precipitation_level = as.factor(Precipitation_level)) %>% 
  group_by(trait) %>%
  do({
    mod <- lme(measurement ~ Precipitation_level, data = ., subset = funYear %in% c("forb_2011", "forb_2016"), random = ~ 1|siteID, weights = varIdent(form = ~ 1|funYear*Precipitation_level), na.action = "na.omit")
    tidy(mod)
    #qqnorm(residuals(mod), main = .$trait); qqline(residuals(mod))
  }) %>%
  distinct(trait, group, term, estimate) %>% 
  arrange(desc(trait))


# test for difference in means of forbs in 2011 and 2016 and graminoids in 2011 in TTCs and RTCs
#against temperature
modvar3temp <- traitScale %>% 
  filter(scale == "local", TTtreat == "TTC", Year == 2011, !is.na(measurement), functionalGroup == "forb") %>% 
  group_by(trait) %>%
  do({
    mod <- lmer(measurement ~ Temperature_level + (1|siteID), data = .)
    tidy(mod)}) %>% 
  filter(term == "Temperature_level")

#against precipitation
modvar3precip <- traitScale %>%
  filter(scale == "local", TTtreat == "TTC", Year == 2011, !is.na(measurement), functionalGroup == "forb") %>% 
  group_by(trait) %>%
  do({
    mod <- lmer(measurement ~ Precipitation_level + (1|siteID), data = .)
    tidy(mod)}) %>% 
  filter(term == "Precipitation_level")



# test for difference in means of forbs and graminoids in TTCs in 2011
modvar4 <- traitScale %>%
  filter(scale == "local", TTtreat == "TTC", Year == 2011, !is.na(measurement)) %>%
  group_by(trait) %>%
  do({
    mod <- lmer(measurement ~ Precipitation_level*Temperature_level*functionalGroup + (1|siteID), data = .)
    tidy(mod)}) %>% 
  filter(term == "Precipitation_level:Temperature_level:functionalGroupgraminoid") %>% 
  arrange(desc(term))



####------------ Plots --------------####
# plot for IAVS conference
wholecom %>% 
  gather(key = wmean_trait, value = measurement, c(wmeanCN_local, wmeanLDMC_local, wmeanSLA_local, wmeanSLA_local, wmeanLA_local, wmeanLTH_local, wmean_seedMass)) %>%
  filter(TTtreat == "TTC", Year == 2011, wmean_trait %in% c("wmeanSLA_local")) %>% 
  ggplot(aes(measurement, fill = factor(Temperature_level))) +
  scale_fill_manual(values = cbPalette[c(9,8,7)]) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  axis.dim +
  facet_wrap(~ functionalGroup) +
  labs(x = "SLA", fill = "Temperature (C)")
  ggsave(filename = paste0("IAVS_SLA_functionalgroup_temp.jpg"), height = 4, width = 8, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")

# plot 2 for IAVS conference
wholecom %>% 
  filter(TTtreat == "RTC") %>% 
  filter(funYear %in% c("forb_2011", "forb_2016", "graminoid_2011")) %>% 
  ggplot(aes(wmean_seedMass, fill = factor(funYear))) +
  scale_fill_manual(values = rev(cbPalette[c(10, 4, 2, 5)])) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  axis.dim +
  facet_grid( ~ Temperature_level, scales = "free_y") +
  labs(x = "LA", fill = "Functional groups \n in 2011 and 2016") 
  ggsave(filename = paste0("IAVS_LDMC_functionalgroup_TTtreat.jpg"), height = 4, width = 10.5, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")


legend.title.traits.precip <- "Precipitation"
legend.title.traits.temp <- "Temperature"

x <- traitScale %>% 
  filter(TTtreat == "RTC", Year == c(2011,2016)) %>% 
  mutate(Precipitation_level = as.factor(Precipitation_level), Year = as.factor(Year)) %>% 
  ggplot(aes(measurement, fill = interaction(Precipitation_level, Year), linetype = interaction(Precipitation_level, Year), alpha = interaction(Precipitation_level, Year))) +
  geom_density() +
  scale_fill_manual(legend.title.traits.precip, values = cbPalette[c(1, 1, 1, 1, 7, 2, 4, 10)]) +
  scale_linetype_manual(legend.title.traits.precip, values = c("dashed", "solid", "dotted", "longdash", "dashed", "solid", "dotted", "longdash")) +
  scale_alpha_manual(legend.title.traits.precip, values = c(0.3, 0.3, 0.3, 0.3, 0.5, 0.5, 0.5, 0.5)) +
  theme_classic() +
  axis.dim +
  theme(legend.position = "top") +
  facet_wrap(~ trait, scales = "free", ncol = 1) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())
  #ggsave(filename = paste0("fig4_traitVar_precip_gramRem.jpg"), height = 3, width = 12, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")

  
y <- traitScale %>% 
  filter(TTtreat == "RTC", Year == c(2011,2016)) %>% 
  mutate(Precipitation_level = as.factor(Temperature_level), Year = as.factor(Year)) %>% 
  ggplot(aes(measurement, fill = interaction(Temperature_level, Year), linetype = interaction(Temperature_level, Year), alpha =  interaction(Temperature_level, Year))) +
  geom_density() +
  scale_fill_manual(legend.title.traits.temp, values = cbPalette[c(1, 1, 1, 9, 8, 7)]) +
  scale_linetype_manual(legend.title.traits.temp, values = c("dashed", "solid", "dotted", "dashed", "solid", "dotted")) +
  scale_alpha_manual(legend.title.traits.temp, values = c(0.3, 0.3, 0.3, 0.5, 0.5, 0.5)) +
  theme_classic() +
  axis.dim +
  theme(legend.position = "top") +
  facet_wrap( ~ trait, scales = "free", ncol = 1, strip.position = "right")
  #ggsave(filename = paste0("fig5_traitVar_temp_gramRem.jpg"), height = 4.5, width = 12, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")


z <- plot_grid(x, y, labels = c('A', 'B'), nrow = 1, align = 'v')
ggsave(filename = paste0("fig6_traitVar_gramRem.jpg"), width = 8, height = 12, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")


my.GR.data %>% 
  filter(functionalGroup == "graminoid", Year == 2011) %>%
  ggplot(aes(x = SLA, fill = family)) + 
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = cbPalette[c(10, 4, 2, 7)]) +
  theme_bw() +
  axis.dim +
  facet_wrap( ~ Precipitation_level, scales = "free_y")
