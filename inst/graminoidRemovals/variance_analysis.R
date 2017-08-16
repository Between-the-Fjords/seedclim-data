library(broom)

traitVariance <- wholecom %>% 
  gather(key = wmean_trait, value = measurement, c(wmean_CN_local, wmean_LDMC_local, wmean_SLA_local, wmean_LA_local, wmean_LTH_local, wmean_seedMass))


############# TEMPERATURE ##############
# test for difference in variance of forbs and graminoids in TTCs in 2011 with temperature
traitVariance %>%
  filter(Year == 2011, TTtreat == "TTC") %>% 
  group_by(wmean_trait, functionalGroup) %>%
  do(car::leveneTest(measurement ~ as.factor(Temperature_level), data = .)[1,]) %>% 
  arrange(functionalGroup) # levene's test probably not optimal - take a look at scale location models###########


# test for skewness and kurtosis in variance of forbs and graminoids in TTCs in 2011 with temperature
traitVariance %>% 
  filter(Year == 2011, TTtreat == "TTC") %>%
  dplyr::select(wmean_trait, functionalGroup, Temperature_level, measurement) %>% 
  group_by(wmean_trait, functionalGroup, Temperature_level) %>%
  do(kurtosis = moments::kurtosis(x = .$measurement)) %>% 
  arrange(functionalGroup)
  

# corresponding plot
# for forbs
traitVariance %>%
  filter(functionalGroup == "forb", TTtreat == "TTC", Year == 2011, wmean_trait %in% c("wmean_CN_local", "wmean_LA_local", "wmean_LDMC_local", "wmean_LTH_local", "wmean_SLA_local", "wmean_seedMass")) %>% 
  ggplot(aes(measurement, fill = factor(Temperature_level))) +
  scale_fill_manual(values = cbPalette[c(10, 4, 2, 5)]) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  axis.dim +
  facet_wrap(~ wmean_trait, scales = "free")

traitVariance %>% 
  filter(functionalGroup == "graminoid", TTtreat == "TTC", Year == 2011, wmean_trait %in% c("wmean_CN_local", "wmean_LA_local", "wmean_LDMC_local", "wmean_LTH_local", "wmean_SLA_local", "wmean_seedMass")) %>% 
  ggplot(aes(measurement, fill = factor(Temperature_level))) +
  scale_fill_manual(values = cbPalette[c(10, 4, 2, 5)]) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  axis.dim +
  facet_wrap(~ wmean_trait, scales = "free")

traitVariance %>% 
  filter(TTtreat == "TTC", Year == 2011, wmean_trait %in% c("wmean_CN_local", "wmean_SLA_local")) %>% 
  ggplot(aes(measurement, fill = factor(Temperature_level))) +
  scale_fill_manual(values = cbPalette[c(10, 4, 2, 5)]) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  axis.dim +
  facet_wrap(wmean_trait ~ functionalGroup, scales = "free")


# test for difference in variance of forbs and graminoids in TTCs in 2011 with precipitation
traitVariance %>%
  filter(Year == 2011, TTtreat == "TTC") %>% 
  group_by(wmean_trait, functionalGroup) %>%
  do(car::leveneTest(measurement ~ as.factor(Precipitation_level), data = .)[1,]) %>% 
  arrange(functionalGroup) # levene's test probably not optimal - take a look at scale location models###########

# corresponding plot
# for forbs
traitVariance %>%
  filter(functionalGroup == "forb", TTtreat == "TTC", Year == 2011, wmean_trait %in% c("wmean_CN_local", "wmean_SLA_local", "wmean_seedMass")) %>% 
  ggplot(aes(measurement, fill = factor(Temperature_level))) +
  scale_fill_manual(values = rev(cbPalette[c(10, 4, 2, 5)])) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  axis.dim +
  facet_wrap(~ wmean_trait, scales = "free")

traitVariance %>% 
  filter(functionalGroup == "graminoid", TTtreat == "TTC", Year == 2011, wmean_trait %in% c("wmean_CN_local", "wmean_SLA_local", "wmean_seedMass", "wmean_LDMC_local")) %>% 
  ggplot(aes(measurement, fill = factor(Temperature_level))) +
  scale_fill_manual(values = rev(cbPalette[c(10, 4, 2, 5)])) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  axis.dim +
  facet_wrap(~ wmean_trait, scales = "free")


# plot for IAVS conference
traitVariance %>% 
  filter(TTtreat == "TTC", Year == 2011, wmean_trait %in% c("wmean_SLA_local")) %>% 
  ggplot(aes(measurement, fill = factor(Temperature_level))) +
  scale_fill_manual(values = cbPalette[c(3, 4, 2)]) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  axis.dim +
  facet_wrap(~ functionalGroup, scales = "free") +
  labs(x = "SLA", fill = "Temperature (C)") +
  ggsave(filename = paste0("IAVS_SLA_functionalgroup.jpg"), height = 4, width = 8, dpi = 300)



########## PRECIPITATION ##############
# test for difference in variance of forbs and graminoids in TTCs in 2011 with precipitation
traitVariance %>% 
  filter(Year == 2011, TTtreat == "TTC") %>% 
  group_by(wmean_trait, functionalGroup) %>%
  do(car::leveneTest(measurement ~ as.factor(Precipitation_level), data = .)[1,]) %>%
  arrange(functionalGroup) # levene's test probably not optimal - take a look at scale location models###########

gamlss(wmean_SLA_local ~ Precipitation_level*Temperature_level*functionalGroup + (1|siteID), family = NO)


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

wholecom %>% 
  filter(Year == 2011, TTtreat == "TTC")%>% 
  gamlss(wmean_SLA_local ~ Precipitation_level*Temperature_level*functionalGroup + (1|siteID), family = "gaussian", )

# test for difference in variance of forbs in 2011 and 2016 and graminoids in 2011 in TTCs and RTCs
wholecom %>% 
  gather(key = wmean_trait, value = measurement, c(wmean_CN_local, wmean_LDMC_local, wmean_SLA_local, wmean_LA_local, wmean_LTH_local, wmean_seedMass)) %>%
  filter(funYear %in% c("forb_2011", "forb_2016", "graminoid_2011")) %>% 
  group_by(wmean_trait, TTtreat) %>%
  do(car::leveneTest(measurement ~ as.factor(Temperature_level)*funYear, data = .)[1,])



# plot 2 for IAVS conference
wholecom %>% 
  filter(funYear %in% c("forb_2011", "forb_2016", "graminoid_2011")) %>% 
  ggplot(aes(wmean_CN_local, fill = factor(funYear))) +
  scale_fill_manual(values = rev(cbPalette[c(10, 4, 2, 5)])) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  axis.dim +
  facet_wrap(~ Temperature_level, scales = "free") +
  labs(x = "CN ratio", fill = "Functional groups \n in 2011 and 2016") +
  ggsave(filename = paste0("IAVS_CN_functionalgroup_TTtreat.jpg"), height = 4, width = 10.5, dpi = 300)
  

# test for difference in means of forbs in 2011 and 2016 and graminoids in 2011 in TTCs and RTCs
wholecom %>% 
  select(wmean_CN_local, wmean_LDMC_local, wmean_SLA_local, wmean_LA_local, wmean_LTH_local, wmean_seedMass, functionalGroup, TTtreat, Year, Temperature_level, siteID, blockID) %>% 
  gather(key = wmean_trait, value = measurement, c(wmean_CN_local, wmean_LDMC_local, wmean_SLA_local, wmean_LA_local, wmean_LTH_local, wmean_seedMass)) %>%
  filter(TTtreat == "TTC", Year == 2011, !is.na(measurement)) %>% 
  group_by(wmean_trait) %>%
  do({
    mod <- lmer(measurement ~ Temperature_level*functionalGroup + (1|siteID), data = .)
    tidy(mod)}) %>% 
  filter(term == "Temperature_level")




#### Plots ###
wholecom %>% 
  gather(key = wmean_trait, value = measurement, c(wmean_CN_local, wmean_LDMC_local, wmean_SLA_local, wmean_LA_local, wmean_LTH_local, wmean_seedMass)) %>%
  filter(functionalGroup == "forb", TTtreat == "TTC", Year == 2011, wmean_trait %in% c("wmean_CN_local", "wmean_SLA_local", "wmean_seedMass")) %>% 
  ggplot(aes(measurement, fill = factor(Temperature_level))) +
  scale_fill_manual(values = cbPalette[c(10, 4, 2)]) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  axis.dim +
  facet_wrap(~ wmean_trait, scales = "free")
  

interesting_turfs <- wholecom %>% 
  filter(functionalGroup == "forb", TTtreat == "TTC", Year == 2011) %>%
  filter(wmean_CN_local > wmean_CN_local[Temperature_level == 10.5]) %>% 
  distinct(turfID)

x <- my.GR.data %>% 
  filter(turfID == "101 TTC", Year == 2011, functionalGroup == "forb") %>% 
  arrange(CN_mean)

highSLAspp <- my.GR.data %>% 
  filter(functionalGroup == "forb", TTtreat == "TTC", Year == 2011) %>% 
  filter(Temperature_level == 8.5 & CN_mean > CN_mean[Temperature_level == 10.5]) %>% 
  distinct(species, .keep_all = TRUE)


##### work in progress ####
plot.variance <- function(dat, response, explan){
  ggplot(dat, aes_string(response, fill = ("factor(explan)"))) +
    scale_fill_manual(values = cbPalette[c(10, 4, 2, 5)]) +
    geom_density(alpha = 0.5) +
    theme_bw() +
    axis.dim +
    facet_wrap(~ wmean_trait, scales = "free")
}

plot.variance(traitVariance, response = "measurement", explan = "Temperature_level")
