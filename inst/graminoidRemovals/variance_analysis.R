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
  select(-c(wmeanLDMC_global:wmeanheight_global)) %>% 
  mutate(Temperature_level = factor(Temperature_level, levels = c(6.5, 8.5, 10.5))) %>% 
  mutate(Precipitation_level = factor(Precipitation_level, levels = c(0.6, 1.2, 2.0, 2.7)))

traitScale <- arrange(mutate(traitScale, trait = factor(trait,levels=traitOrder)), trait)


#http://r.789695.n4.nabble.com/How-to-extract-parameter-estimates-of-variance-function-from-lme-fit-td2997153.html

####------------- Analyses -------------####
# testing for differences in variance between forbs before and after treatment #
# against temperature
modvar1temp <- traitScale %>% 
  filter(scale == "local", TTtreat == "RTC", !is.na(measurement)) %>%
  group_by(trait) %>%
  do({
    mod <- lme(measurement ~ Temperature_level + Year, data = ., subset = funYear %in% c("forb_2011", "forb_2016"), random = ~ 1|siteID/turfID, weights = varIdent(form = ~ 1|funYear*Temperature_level), na.action = "na.omit")
    mod0 <- lme(measurement ~ Temperature_level + Year, data = ., subset = funYear %in% c("forb_2011", "forb_2016"), random = ~ 1|siteID/turfID, weights = varIdent(form = ~ 1|Temperature_level), na.action = "na.omit")
    anova(mod, mod0)}
    ) %>%
  select(-call) %>% 
  group_by(trait) %>% 
  mutate(difAIC = AIC[Model == 2] - AIC[Model == 1]) %>% 
  filter(Model == 2)
    
modvar2temp <- traitScale %>% 
  filter(scale == "local", TTtreat == "RTC", !is.na(measurement)) %>%
  filter(trait %in% c("wmeanheight", "wmeanSLA", "wmeanseedMass")) %>% 
  arrange(Temperature_level) %>% 
  filter(funYear %in% c("forb_2011", "forb_2016")) %>% 
  group_by(trait) %>%
  do({
    mod <- lme(measurement ~ Temperature_level + Precipitation_level, data = ., random = ~ 1|siteID, weights = varIdent(form = ~ 1|funYear*Temperature_level*Precipitation_level), na.action = "na.omit")
    mod <- mod$modelStruct$varStruct
    dat <- data_frame(var = attr(mod, "groupNames"), coef = c(1, coef(mod, unconstrained = FALSE)))
    dat
  }) %>% 
  mutate(Year = substr(var, 6, 9), Temperature_level = substr(var, 11, nchar(var))) %>% group_by(trait, Temperature_level) %>% mutate(varDiff = coef[Year == 2016]-coef[Year == 2011])

modvar2tempPlot<- modvar2temp %>% 
  ungroup() %>%
  select(-varDiff, -var) %>% 
  spread(Year, coef) %>% 
  ggplot(aes(x = `2011`, y = `2016`, shape = trait, colour = Temperature_level)) + 
  geom_point(size = 4) + 
  geom_abline() + 
  scale_colour_manual(values = cbPalette[c(7,12,2,8, 6,3,10,11, 4,9,1,5)]) +
  theme(legend.box = "vertical",
        legend.position = "top",
        axis.text=element_text(size=10),
        axis.title=element_text(size=15),
        axis.ticks = element_blank(),
        legend.text = element_text(size=11),
        legend.title = element_text(size=12),
        strip.text.x = element_text(size = 11),
        strip.text.y = element_text(size = 13)) +
  ggsave(filename = paste0("fig11_traitVarInteraction_gramRem.jpg"), width = 7, height = 7, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")



par(mfrow = c(3,3)) 
modvar2atemp <- traitScale %>% 
  filter(scale == "local", TTtreat == "RTC", !is.na(measurement)) %>%
  mutate(Temperature_level = factor(Temperature_level, levels = c("6.5", "8.5", "10.5"))) %>% 
  group_by(trait) %>%
  do({
    mod <- lme(measurement ~ Temperature_level, data = ., subset = funYear %in% c("forb_2011", "forb_2016"), random = ~ 1|siteID, weights = varIdent(form = ~ 1|funYear*Temperature_level), na.action = "na.omit")
    tidy(mod)
    #qqnorm(residuals(mod), main = .$trait); qqline(residuals(mod))
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
  select(-call)%>% 
  group_by(trait) %>% 
  mutate(difAIC = AIC[Model == 2] - AIC[Model == 1]) %>% 
  filter(Model == 2)


modvar2precip <- traitScale %>% 
  filter(scale == "local", TTtreat == "RTC", !is.na(measurement)) %>% 
  group_by(trait) %>%
  do({
    mod <- lme(measurement ~ Precipitation_level, data = ., subset = funYear %in% c("forb_2011", "forb_2016"), random = ~ 1|siteID, weights = varIdent(form = ~ 1|funYear*Precipitation_level), na.action = "na.omit")
    mod <- mod$modelStruct$varStruct
    dat <- data_frame(var = attr(mod, "groupNames"), coef = c(1,coef(mod, unconstrained = FALSE)))
    dat
  }) %>% 
  mutate(Year = substr(var, 6, 9), prec = substr(var, 11, nchar(var))) %>% group_by(trait, prec) %>% mutate(varDiff = coef[Year == 2016]-coef[Year == 2011])

modvar2precipPlot <- modvar2precip %>%
  ungroup() %>%
  select(-varDiff, -var) %>% 
  spread(Year, coef) %>% 
  ggplot(aes(x = `2011`, y = `2016`, shape = trait, colour = prec)) + 
  geom_point(size = 4) + 
  geom_abline() + 
  theme_classic() +
  scale_colour_manual(values = cbPalette[c(7, 2, 4, 3)]) +
  theme(legend.box = "vertical",
    legend.position = "top",
        axis.text=element_text(size=10),
        axis.title=element_text(size=15),
        axis.ticks = element_blank(),
        legend.text = element_text(size=11),
        legend.title = element_text(size=12),
        strip.text.x = element_text(size = 11),
        strip.text.y = element_text(size = 13)) +
  ggsave(filename = paste0("fig6_traitVarPrecip_gramRem.jpg"), width = 7, height = 7, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")

var <- plot_grid(modvar2tempPlot, modvar2precipPlot, labels = c("A", "B"), nrow = 1, align = "h")
ggsave(filename = paste0("fig10_traitVarCombi_gramRem.jpg"), width = 11, height = 6, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")

par(mfrow = c(3,3))
modvar2aprecip <- traitScale %>% 
  filter(scale == "local", TTtreat == "RTC", !is.na(measurement)) %>%
  mutate(Precipitation_level = factor(Precipitation_level, levels = c(0.6, 1.2, 2.0, 2.7))) %>% 
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
traitScale %>% 
  filter(scale == "local", TTtreat == "TTC", Year == 2011, trait == "wmeanLDMC") %>% 
  ggplot(aes(measurement, fill = factor(Temperature_level))) +
  scale_fill_manual(values = cbPalette[c(9,8,7)]) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  axis.dim +
  facet_wrap( ~ functionalGroup, scales = "free") +
  labs(x = "SLA", fill = "Temperature (C)") +
  ggsave(filename = paste0("IAVS_LDMC_functionalgroup_temp.jpg"), height = 4, width = 8, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")


traitScale %>% 
  filter(scale == "local", TTtreat == "TTC", Year == 2011) %>% 
  ggplot(aes(measurement, fill = factor(Precipitation_level))) +
  scale_fill_manual(values = cbPalette[c(10,4,2,5)]) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  axis.dim +
  facet_wrap(trait ~ functionalGroup, scales = "free") +
  labs(x = "SLA", fill = "Temperature (C)")
  ggsave(filename = paste0("IAVS_SLA_functionalgroup_temp.jpg"), height = 4, width = 8, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")

# plot 2 for IAVS conference
traitScale %>% 
  filter(TTtreat == "RTC", trait == "wmeanSLA") %>% 
  filter(funYear %in% c("forb_2011", "forb_2016", "graminoid_2011")) %>% 
  ggplot(aes(measurement, fill = factor(funYear))) +
  scale_fill_manual(values = rev(cbPalette[c(10, 4, 2, 5)])) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  axis.dim +
  facet_grid( ~ Precipitation_level, scales = "free_y") +
  labs(x = "SLA (UNITS)", fill = "Functional groups \n in 2011 and 2016")
  ggsave(filename = paste0("IAVS_SLA_functionalgroup_TTtreat.jpg"), height = 4, width = 10.5, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")


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
  guides(fill = guide_legend(title.position = "top")) +
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
  guides(fill = guide_legend(title.position = "top")) +
  theme(legend.position = "top") +
  facet_wrap( ~ trait, scales = "free", ncol = 1, strip.position = "right")
  #ggsave(filename = paste0("fig5_traitVar_temp_gramRem.jpg"), height = 4.5, width = 12, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")


z <- plot_grid(x, y, labels = c('A', 'B'), nrow = 1, align = 'v')
ggsave(filename = paste0("fig6_traitVar_gramRem.jpg"), width = 8.5, height = 12, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")


my.GR.data %>% 
  filter(functionalGroup == "graminoid", Year == 2011) %>%
  ggplot(aes(x = SLA, fill = family)) + 
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = cbPalette[c(10, 4, 2, 7)]) +
  theme_bw() +
  axis.dim +
  facet_wrap( ~ Precipitation_level, scales = "free_y")
