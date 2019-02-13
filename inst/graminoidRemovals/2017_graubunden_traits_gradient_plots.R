#load libraries

library(tidyverse)
library(cowplot)
library(elevatr)
library(aws)

#load files and colour schemes
grau <- read.csv(file = "/Users/fja062/Documents/JAROSZYNSKA_graubunden_traits_2017.csv", header = TRUE, sep = ";")

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#1C9099", "#A6BDDB", "#ECE2F0", "orange3")

str(grau)

grau <- grau %>% 
  mutate(TRANSECT = as.numeric(substr(TRANSECT, 1, 1)))

grau %>% 
  filter(!(elevation.certain. == "n")) %>%
  filter(!(ALTITUDE == "?")) %>% 
  mutate(ALTITUDE = as.numeric(levels(ALTITUDE))[ALTITUDE]) %>% 
  gather(key = trait, value = measurement, PA_STALK_LENGTH, PA_INFLORESCENCE_LENGTH, PA_BASAL_LEAF_LENGTH, PA_STEM_LEAF_LENGTH) %>% 
  ggplot(aes(y = measurement, x = ALTITUDE, colour = TIME)) +
  geom_smooth() +
  geom_point() +
  facet_wrap(~ trait, scales = "free") +
  theme_classic() +
  axis.dim +
  scale_colour_manual(values = cbPalette) +
  ggsave(filename = "/Users/fja062/Documents/pa_trait_variance_elev.jpg")

  grau %>% 
    filter(!(elevation.certain. == "n")) %>%
    filter(!(ALTITUDE == "?")) %>% 
    mutate(ALTITUDE = as.numeric(levels(ALTITUDE))[ALTITUDE]) %>% 
    gather(key = trait, value = measurement, PV_HEIGHT.OF.LONGEST.FLOWERING.STEM, PV_LENGTH.OF.INFLORESCENCE, PV_WIDTH.OF.LARGEST.LEAF, PV_LENGTH.OF.LARGEST.LEAF) %>% 
    ggplot(aes(y = measurement, x = ALTITUDE, colour = TIME)) +
    geom_smooth() +
    geom_point() +
    facet_wrap(~ trait, scales = "free") +
    theme_classic() +
    axis.dim +
    scale_colour_manual(values = cbPalette) +
    ggsave(filename = "/Users/fja062/Documents/pv_trait_variance_elev.jpg")
  
  

  grau %>% 
    filter(!(elevation.certain. == "n")) %>%
    filter(!(ALTITUDE == "?")) %>% 
    mutate(ALTITUDE = as.numeric(levels(ALTITUDE))[ALTITUDE]) %>% 
    gather(key = trait, value = measurement, CR_PLANT_HEIGHT, CR_DIAMETER, CR_LENGTH_LARGEST_LEAF, CR_TOTAL_NO_FLOWERS, CR_LEAF_NO_IN_ROSETTE) %>% 
    ggplot(aes(y = measurement, x = ALTITUDE, colour = TIME)) +
    geom_smooth() +
    geom_point() +
    facet_wrap(~ trait, scales = "free") +
    theme_classic() +
    axis.dim +
    scale_colour_manual(values = cbPalette) +
    ggsave(filename = "/Users/fja062/Documents/cr_trait_variance_elev.jpg")
  
grau %>% 
  filter(!(elevation.certain. == "n")) %>%
  filter(!(ALTITUDE == "?")) %>%
  mutate(ALTITUDE = as.numeric(levels(ALTITUDE))[ALTITUDE]) %>% 
  filter(TIME == "new") %>% 
  gather(key = trait, value = measurement, contains("RG_")) %>% 
  ggplot(aes(y = measurement, x = ALTITUDE, colour = MICROHABITAT)) +
  geom_smooth() +
  geom_point() +
  facet_wrap(~ trait, scales = "free") +
  theme_classic() +
  axis.dim +
  scale_colour_manual(values = cbPalette)
  
  
  
  grau %>% 
  filter(!(elevation.certain. == "n")) %>%
  filter(!(ALTITUDE == "?")) %>% 
  filter(RG_NO_LEAVES < 30) %>% 
  mutate(ALTITUDE = as.numeric(levels(ALTITUDE))[ALTITUDE]) %>% 
  gather(key = trait, value = measurement, contains("RG_")) %>% 
  ggplot(aes(y = measurement, x = ALTITUDE, colour = TIME)) +
  geom_smooth() +
  geom_point() +
  facet_wrap(~ trait, scales = "free") +
  theme_classic() +
  axis.dim +
  scale_colour_manual(values = cbPalette)  +
  ggsave(filename = "/Users/fja062/Documents/rg_trait_variance_elev.jpg")


grau %>% 
  gather(key = trait, value = measurement, contains("PV_")) %>% 
  filter(SPECIES == "pol_viv") %>% 
  #filter(TTtreat == "TTC", Year == 2011) %>% 
  ggplot(aes(measurement, fill = TIME)) +
  scale_fill_manual(values = cbPalette) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  axis.dim +
  facet_wrap(~ trait, scales = "free") +
  labs(x = "SLA", fill = "Temperature (C)")
  
  grau %>% 
    gather(key = trait, value = measurement, PA_STALK_LENGTH, PA_INFLORESCENCE_LENGTH, PA_BASAL_LEAF_LENGTH, PA_STEM_LEAF_LENGTH) %>% 
    filter(SPECIES == "poa_alp") %>% 
    ggplot(aes(measurement, fill = TIME)) +
    scale_fill_manual(values = cbPalette) +
    geom_density(alpha = 0.5) +
    theme_classic() +
    axis.dim +
    facet_wrap(~ trait, scales = "free") +
    labs()
    ggsave(filename = "/Users/fja062/Documents/pa_trait_variance.jpg")
  labs(x = "SLA", fill = "Temperature (C)")
  
  grau %>% 
    gather(key = trait, value = measurement, contains("CR_")) %>% 
    filter(SPECIES == "car_res") %>% 
    #filter(TTtreat == "TTC", Year == 2011) %>% 
    ggplot(aes(measurement, fill = TIME)) +
    scale_fill_manual(values = cbPalette) +
    geom_density(alpha = 0.5) +
    theme_classic() +
    axis.dim +
    facet_wrap(~ trait, scales = "free")
  labs(x = "SLA", fill = "Temperature (C)")
  
  grau %>% 
    filter(RG_NO_LEAVES < 30) %>% 
    gather(key = trait, value = measurement, contains("RG_")) %>% 
    filter(SPECIES == "ran_glac") %>% 
    #filter(TTtreat == "TTC", Year == 2011) %>% 
    ggplot(aes(measurement, fill = TIME)) +
    scale_fill_manual(values = cbPalette) +
    geom_density(alpha = 0.5) +
    theme_classic() +
    axis.dim +
    facet_wrap(~ trait, scales = "free") +
    ggsave(filename = "/Users/fja062/Documents/rg_trait_variance.jpg")
  
labs(x = "SLA", fill = "Temperature (C)")

# analyses

modvar1 <- grau %>% 
  gather(key = trait, value = measurement, contains("PV_")) %>% 
  filter(SPECIES == "pol_viv") %>% 
  group_by(trait) %>%
  do({
    mod <- lme(measurement ~ ALTITUDE, random = ~1|TRANSECT, data = ., weights = varIdent(form = ~ 1|TIME), na.action = "na.omit")
    mod0 <- lme(measurement ~ ALTITUDE, random = ~1|TRANSECT, data = ., na.action = "na.omit")
    anova(mod, mod0)}
  ) %>%
  select(-call) %>% 
  group_by(trait) %>% 
  mutate(difAIC = AIC[Model == 2] - AIC[Model == 1]) %>% 
  filter(Model == 2)