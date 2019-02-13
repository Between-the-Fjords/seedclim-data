# Log ratio of cumulative treatment:control divided by cumulative graminoid biomass removal
# 2016 data only

biomass <- read.csv("~/Documents/seedclimComm/GR7_Graminoid_biomass.csv", stringsAsFactors = FALSE, sep = ";")


biomass <- biomass %>%
  mutate(cutting = as.factor(cutting), year = as.factor(year)) %>%
  filter(comments == "") %>%
  group_by(turfID, year) %>%
  summarise(biomass = sum(biomass)) %>%
  arrange(turfID, year) %>%
  group_by(turfID) %>%
  mutate(cumbiomass = cumsum(biomass)) %>%
  select(-biomass) %>%
  as.data.frame() #although not necessary apparently


rtcmeta <- left_join(rtcmeta, biomass, by = c("Year" = "year", "turfID"))

# delta cover against cumulative biomass
rtcmeta %>%
  ggplot(aes(x = cumbiomass, y = deltasumcover, colour = as.factor(Precipitation_level))) +
  scale_color_manual(values = cbPalette) +
  geom_point() +
  geom_smooth(method = "gam", se = FALSE) +
  facet_grid(as.formula(. ~ Year), scales = "free_x") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed")
  

# delta cover against initial graminoid cover in treatment plots
grams <- wholecom %>%
  filter(functionalgroup == "graminoid") %>%
  select(turfID, gramcover = cover)

rtcmeta <- left_join(rtcmeta, grams)

rtcmeta %>%
  ggplot(aes(x = gramcover, y = deltasumcover, colour = Year)) +
  scale_color_manual(values = cbPalette) +
  geom_point() +
  geom_smooth(se = TRUE, method = "gam") +
  facet_grid(as.formula(. ~ Temperature_level), scales = "free_x") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed")



# delta cover against graminoid cover in control plots
ttc <- wholecom %>%
  filter(functionalgroup == "forb") %>%
  select(blockID, Year, sumcover, TTtreat, Temperature_level, Precipitation_level) %>%
  left_join(wholecom %>% filter(functionalgroup == "graminoid", TTtreat == "TTC") %>% select(blockID, sumcover, Year), by = c("blockID", "Year"), suffix = c("forb", "graminoid"))
  
control <- filter(ttc, TTtreat == "TTC")

ggplot(ttc, aes(sumcovergraminoid, sumcoverforb, colour = TTtreat)) +
  scale_color_manual(values = cbPalette) +
  geom_point() +
  geom_smooth(se = TRUE, method = "gam") +
  facet_grid(as.formula(Temperature_level ~ Year), scales = "free_x") +
  theme_bw()


rtcmeta %>%
  ggplot(aes(x = cumbiomass, y = deltasumcover, colour = as.factor(Temperature_level))) +
  scale_color_manual(values = cbPalette) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(as.formula(. ~ Year), scales = "free_x") +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = "dashed")
