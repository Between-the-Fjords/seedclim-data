grau <- read.csv(file = "/Users/fja062/Documents/JAROSZYNSKA_graubunden_traits_2017.csv", header = TRUE, sep = ";")


str(grau)

grau %>% 
  filter(!(elevation.certain. == "n")) %>%
  filter(!(ALTITUDE == "?")) %>% 
  mutate(ALTITUDE = as.numeric(levels(ALTITUDE))[ALTITUDE]) %>% 
  gather(key = trait, value = measurement, PA_STALK_LENGTH, PA_INFLORESCENCE_LENGTH, PA_BASAL_LEAF_LENGTH, PA_STEM_LEAF_LENGTH) %>% 
  ggplot(aes(y = measurement, x = ALTITUDE, colour = TIME)) +
  geom_point() +
  facet_wrap(~ trait, scales = "free") +
  theme_classic() +
  axis.dim +
  scale_colour_manual(values = cbPalette) +
  ggsave(filename = "/Users/fja062/Documents/pa_trait_variance_elev.jpg")

grau %>% 
  filter(!(elevation.certain. == "n")) %>%
  filter(!(ALTITUDE == "?")) %>% 
  filter(RG_NO_LEAVES < 30) %>% 
  mutate(ALTITUDE = as.numeric(levels(ALTITUDE))[ALTITUDE]) %>% 
  gather(key = trait, value = measurement, contains("RG_")) %>% 
  ggplot(aes(y = measurement, x = ALTITUDE, colour = TIME)) +
  geom_point() +
  facet_wrap(~ trait, scales = "free") +
  theme_classic() +
  axis.dim +
  scale_colour_manual(values = cbPalette) +
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
  facet_wrap(~ trait, scales = "free")
  labs(x = "SLA", fill = "Temperature (C)")
  
  grau %>% 
    gather(key = trait, value = measurement, PA_STALK_LENGTH, PA_INFLORESCENCE_LENGTH, PA_BASAL_LEAF_LENGTH, PA_STEM_LEAF_LENGTH) %>% 
    filter(SPECIES == "poa_alp") %>% 
    #filter(TTtreat == "TTC", Year == 2011) %>% 
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