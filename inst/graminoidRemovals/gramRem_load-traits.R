#gramRem_load-traits

# load data
traits <-read_delim("~/OneDrive - University of Bergen/Research/FunCaB/Data/veg_traits/SeedClim_Traits_2016.csv", delim = ";")

# create local weighted means
traitdata <- traits %>%
  rename(LA = "Leaf_area", CN = "CN_ratio", siteID = "site") %>%
  mutate(siteID = recode(siteID, "Ulvehaugen" = "Ulvhaugen")) %>% 
  group_by(species) %>%
  mutate(CN_mean_global = mean(CN, na.rm = TRUE)) %>%
  group_by(siteID, species) %>%
  mutate(
    SLA_mean = mean(SLA, na.rm = TRUE),
    Lth_mean = mean(Lth_ave, na.rm = TRUE),
    Height_mean = mean(Height, na.rm = TRUE),
    LDMC_mean = mean(LDMC, na.rm = TRUE),
    LA_mean = mean(LA, na.rm = TRUE),
    CN_mean = mean(CN, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(siteID, species, CN_mean_global:CN_mean)

# adjust species names so they parse with my.GR.data
traitdata <- traitdata %>% 
  mutate(species = gsub("_", ".", species)) %>% 
  mutate(species = recode(species, "Hyp.mac" = 'Hype.mac', "Emp.nig" = "Emp.her")) %>%
  distinct(siteID, species, .keep_all = TRUE)

# if the local CN mean is missing, fill it with the global mean for that species
traitdata <- traitdata %>% 
  group_by(siteID, species) %>% 
  mutate(CN_mean = if_else(is.na(CN_mean),
                           CN_mean_global,
                           CN_mean)) %>% 
  select(-CN_mean_global)

# not run #
#traitdata %>% filter(is.na(CN_mean)) %>% group_by(siteID) %>% summarise(n = n()) %>% head() %>% as.data.frame()