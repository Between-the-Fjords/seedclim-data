library(vegan)
library(ggvegan)
library(cowplot)

source("inst/graminoidRemovals/weather.R")

comp2 <- comp2 %>% 
  left_join(weather)

spp <- comp2 %>% 
  filter(!is.na(siteID), !is.na(cover), Year == 2016, Treatment %in% c("C", "XC")) %>% 
  select(species, turfID, cover) %>% 
  distinct(turfID, species, .keep_all = TRUE) %>% 
  spread(species, cover, fill = 0) %>% 
  select(-turfID)

turfID <- comp2 %>% 
  filter(!is.na(siteID), !is.na(cover), Year == 2016, Treatment %in% c("C", "XC")) %>% 
  select(species, turfID, cover, siteID, annPrecip, summer_temp) %>% 
  distinct(turfID, species, .keep_all = TRUE) %>% 
  spread(species, cover) %>% 
  select(annPrecip, summer_temp)

rda2 <- rda(sqrt(spp) ~ ., turfID)

autoplot(rda2, legend.position = "none")

# transform traits to scale
traits <- composition %>% 
  ungroup() %>% 
  filter(!is.na(siteID), Year == 2016, Treatment %in% c("C", "XC")) %>% 
  select(-functionalGroup) %>% 
  mutate_at(vars(forbCov:wmCN), funs(scale)) %>% 
  gather(wmSLA, wmLA, wmH, wmLDMC, wmLTH, wmCN, forbCov, graminoidCov, key = "trait", value = "Value") %>% 
  filter(!is.na(Value)) %>% 
  select(trait, turfID, Value) %>% 
  distinct(turfID, trait, .keep_all = TRUE) %>% 
  spread(trait, Value, fill = 0) %>% 
  select(-turfID)

traitsEnv <- composition %>% 
  ungroup() %>% 
  mutate(temp = recode(siteID, Ulvhaugen = 6.5, Lavisdalen = 6.5,  Gudmedalen = 6.5, Skjellingahaugen = 6.5, Alrust = 8.5, Hogsete = 8.5, Rambera = 8.5, Veskre = 8.5, Fauske = 10.5, Vikesland = 10.5, Arhelleren = 10.5, Ovstedal = 10.5),
         Temperature_level = recode(siteID, Ulvhaugen=6.17, Lavisdalen=6.45, Gudmedalen=5.87, Skjellingahaugen=6.58, Alrust=9.14, Hogsete=9.17, Rambera=8.77, Veskre=8.67, Fauske=10.3, Vikesland=10.55, Arhelleren=10.60, Ovstedal=10.78),
         Precipitation_level= recode(siteID, Ulvhaugen=596, Lavisdalen=1321, Gudmedalen=1925, Skjellingahaugen=2725, Alrust=789, Hogsete=1356, Rambera=1848, Veskre=3029, Fauske=600, Vikesland=1161, Arhelleren=2044, Ovstedal=2923),
         precip = recode(siteID, Ulvhaugen = 600, Alrust = 600, Fauske = 600, Lavisdalen = 1200, Hogsete = 1200, Vikesland = 1200, Gudmedalen = 2000, Rambera = 2000, Arhelleren = 2000, Skjellingahaugen = 2700, Veskre = 2700, Ovstedal = 2700)) %>% 
  left_join(weather) %>% 
  filter(!is.na(siteID), Year == 2016, Treatment %in% c("C", "XC")) %>% 
  mutate(annPrecip = scale(annPrecip), summer_temp = scale(summer_temp)) %>% 
  gather(wmSLA, wmLA, wmH, wmLDMC, wmLTH, wmCN, forbCov, graminoidCov, key = "trait", value = "Value") %>% 
  filter(!is.na(Value)) %>% 
  distinct(turfID, trait, .keep_all = TRUE) %>% 
  spread(trait, Value, fill = 0) %>% 
  select(annPrecip, summer_temp, temp, precip, siteID)


# split CN into C and N
# colour code plots by site


rda1 <- rda(traits ~ ., traitsEnv)

rdaEnv <- rda1 %>%
  fortify() %>% 
  filter(Score == "sites") %>% 
  bind_cols(traitsEnv)


rdaSpp <- rda1 %>%
  fortify() %>% 
  filter(Score == "species")

rdaEnv %>% 
  mutate(siteID = factor(siteID), siteID = reorder(siteID, PC1)) %>% 
  ggplot(aes(y = PC1, x = siteID)) +
  geom_boxplot()


basplot <- plot(rda1)
mult <- attributes(basplot$biplot)$arrow.mul


ggplot(rdaEnv, aes(x = RDA1, y = RDA2)) +
#  geom_point(size = 3, aes(colour = as.factor(temp), shape = as.factor(precip))) +
  coord_equal() +
  geom_segment(data = rdaSpp,
               aes(x = 0, xend = RDA1,
                   y = 0, yend = RDA2),
               arrow = arrow(length = unit(0.25, "cm")), colour = "darkgrey")


screeplot(rda1)


plot(rda1)

autoplot(rda1, axes = c(1,3), layers = c("species", "sites", "biplot"), arrows = TRUE)
