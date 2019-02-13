# seedling data cleaning #

#load packages
library(tidyverse)
library(DBI)
library(readxl)
library(cowplot)
library(lubridate)
library(wesanderson)
library(lme4)
library(MCMCglmm)


# load data
seed <- read_csv2("~/OneDrive - University of Bergen/Research/FunCaB/Data/2018_funcab_seedlings.csv")

head(seed)

# add temperature and precipitation levels per site
seed <- seed %>% 
  filter(!is.na(siteID)) %>% 
  mutate(temp = recode(siteID, Ulvhaugen = 6.5, Lavisdalen = 6.5,  Gudmedalen = 6.5, Skjellingahaugen = 6.5, Alrust = 8.5, Hogsete = 8.5, Rambera = 8.5, Veskre = 8.5, Fauske = 10.5, Vikesland = 10.5, Arhelleren = 10.5, Ovstedal = 10.5),
         Temperature_level = recode(siteID, Ulvhaugen=6.17, Lavisdalen=6.45, Gudmedalen=5.87, Skjellingahaugen=6.58, Alrust=9.14, Hogsete=9.17, Rambera=8.77, Veskre=8.67, Fauske=10.3, Vikesland=10.55, Arhelleren=10.60, Ovstedal=10.78),
         Precipitation_level= recode(siteID, Ulvhaugen=596, Lavisdalen=1321, Gudmedalen=1925, Skjellingahaugen=2725, Alrust=789, Hogsete=1356, Rambera=1848, Veskre=3029, Fauske=600, Vikesland=1161, Arhelleren=2044, Ovstedal=2923),
         precip = recode(siteID, Ulvhaugen = 600, Alrust = 600, Fauske = 600, Lavisdalen = 1200, Hogsete = 1200, Vikesland = 1200, Gudmedalen = 2000, Rambera = 2000, Arhelleren = 2000, Skjellingahaugen = 2700, Veskre = 2700, Ovstedal = 2700)) %>% 
  mutate(Date1 = dmy(Date1),
         Date2 = dmy(Date2)) %>%
  mutate(Leuc_sp = as.numeric(Leuc_sp), Tara_sp = as.numeric(Tara_sp)) %>% 
  group_by(turfID, Round) %>% 
  mutate(seedSum = sum(n())) %>% 
  ungroup() 

seed %>% group_by(turfID, seedID) %>% summarise(n = n()) %>% filter(n>1) %>% arrange(desc(as.numeric(seedID)))

seed <- seed %>% 
  group_by(turfID, seedID) %>%
  gather(unID:Viola, key = "species", value = "seedN") %>% 
  mutate(seedN = as.numeric(as.factor(seedN))) %>% 
  group_by(turfID, seedID) %>%
  mutate(seedNTest = sum(seedN, na.rm = TRUE)) %>% 
  spread(species, seedN) %>% 
  mutate(unID = if_else(seedNTest == 0, 1, unID))
  ungroup()

seed <- seed %>% 
  filter(is.na(survival)) %>% 
  filter(is.na(NS)) %>% 
  filter(!Comment %in% c("out of plot")) %>% 
  mutate(Round = factor(Round))


anti_join(seed, composition, by = "turfID") %>% distinct(turfID)
# funcab composition
composition <- filter(composition, Year == 2017) %>% 
  distinct(siteID, blockID, Treatment, turfID, vegetationHeight, .keep_all = TRUE)

seedComp <- seed %>% 
  select(-(Ach_mil:Viola)) %>% 
  mutate(blockID = as.character(blockID)) %>% 
  left_join(composition, by = c("siteID", "turfID", "Treatment"))

seedComp <- seedComp %>% 
  filter(Treatment %in% c("G", "B", "GB", "C", "FGB"))

# seedlings with temp
seedComp %>% 
  filter(Treatment %in% c("aC", "FGB", "GB", "G", "B")) %>% 
  ggplot(aes(x = factor(Round), y = seedSum, fill = factor(Round))) +
  geom_boxplot() +
  facet_grid(temp~Treatment) +
  scale_fill_manual(values = cbPalette) +
  ggsave(filename = paste0("seeds_drought_temp_byRound.jpg"), height = 7, width = 9, dpi = 300)

seedComp %>% 
  filter(Treatment %in% c("aC", "FGB", "GB", "G", "B")) %>% 
  ggplot(aes(x = factor(Round), y = seedSum, fill = Treatment)) +
  geom_boxplot() +
  scale_alpha_manual(values = c(1, 0.7)) +
  facet_grid(precip~temp) +
  scale_fill_manual(values = cbPalette) 
  ggsave(filename = paste0("seeds_drought_temp.jpg"), height = 4, width = 10.5, dpi = 300)
  

#seedlings with precip
seedComp %>% 
  filter(Treatment %in% c("aC", "FGB", "GB", "G", "B")) %>% 
  ggplot(aes(x = Treatment, y = seedSum, fill = Treatment)) +
  geom_boxplot() +
  facet_grid(Round~precip) +
  scale_fill_manual(values = cbPalette)
  ggsave(filename = paste0("seeds_drought_precip.jpg"), height = 4, width = 10.5, dpi = 300)

seedComp %>% 
  filter(Treatment %in% c("aC", "FGB", "GB", "G", "B")) %>% 
  ggplot(aes(x = Round, y = seedSum, fill = Round)) +
  geom_boxplot() +
  facet_grid(precip~Treatment) +
  scale_fill_manual(values = cbPalette)
  ggsave(filename = paste0("seeds_drought_precip_byRound.jpg"), height = 7, width = 7, dpi = 300)


seedComp <- seedComp %>% 
  mutate(Treatment = recode(Treatment, "C" = "aC")) %>% 
  mutate(Round = as.factor(Round))

glmerTEMP <- MASS::glmmPQL(seedSum ~ Treatment*scale(temp)*Round, random = ~ 1|blockID/siteID, family = "quasipoisson", data = seedComp)


bryMod <- MASS::glmmPQL(seedSum ~ totalBryophytes*scale(temp)*Round, random =  ~ 1|blockID.x/siteID, family = "quasipoisson", data = seedComp)

summary(glmerTEMP)
plot(glmerTEMP)

glmerPrecip <- glmer(seedSum ~ Treatment*scale(precip)*Round + (1|siteID), data = seedComp, family = "poisson")

summary(glmerPrecip)
plot(glmerPrecip)


modTEST <- seedComp %>% 
  filter(Treatment %in% c("aC", "B", "G", "GB", "FGB")) %>% 
  rowid_to_column(var = "ID") %>%
  mutate(ID = factor(ID)) %>% 
  MCMCglmm(seedSum ~ Treatment*scale(temp)*Round, random = ~ siteID + ID, family = "poisson", data = ., nitt = 2000, burnin = 300, thin = 5, verbose = FALSE)

summary(modTEST)
plot(modTEST$VCV)
modTEST$Residual

MCMCglmm

seedComp %>% filter(Treatment %in% c("C", "F", "G", "GF", "FGB")) %>% 
  ggplot(aes(x = totalBryophytes, y = seedSum, colour = Treatment)) +
  geom_point() +
  facet_grid(temp~Round) +
  geom_smooth(method = "glm", formula=y~poly(x, 2), method.args = list(family = "quasipoisson"))

#### MOSS ####
#seedlings with moss depth
seed %>% filter(!Treatment %in% c("B", "GB", "FB", "FGB")) %>% 
  ggplot(aes(x = mossHeight, y = seedSum)) +
  geom_point() +
  geom_smooth(method = "glm", formula=y~poly(x, 2), se = TRUE, method.args = list(family = "quasipoisson"))

#seedlings with moss cover
seed %>% filter(!Treatment %in% c("B", "GB", "FB", "FGB")) %>% 
  ggplot(aes(x = bryophyteCov, y = seedSum)) +
  geom_point() +
  geom_smooth(method = "glm", formula=y~poly(x, 2), se = TRUE, method.args = list(family = "quasipoisson"))
  ggsave(filename = paste0("bryo_drought_precip.jpg"), height = 4, width = 10, dpi = 300)



seed %>% 
  group_by(turfID) %>% 
  mutate(sumCover = forbCov + graminoidCov) %>% 
  filter(!Treatment %in% c("G", "GB", "GF", "FGB")) %>% 
  ggplot(aes(x = sumCover, y = seedSum, colour = factor(Round))) +
  geom_point() +
  #geom_smooth(method = "lm", formula=y~poly(x, 2))
#### GRAMINOIDS ####

#seedlings with graminoid cover
seed %>% 
  filter(!Treatment %in% c("G", "GB", "GF", "FGB")) %>% 
  ggplot(aes(x = graminoidCov, y = seedSum, colour = factor(Round))) +
  geom_point() +
  geom_smooth(method = "glm", formula=y~poly(x, 2), method.args = list(family = "quasipoisson")) +
  ggsave(filename = paste0("grams_drought_precip.jpg"), height = 4, width = 10, dpi = 300)

#seedlings with graminoid height
seed %>% filter(!Treatment %in% c("G", "GB", "GF", "FGB")) %>% 
  ggplot(aes(x = graminoid, y = seedSum, colour = wmSM)) +
  geom_point() +
  geom_smooth(method = "lm", formula=y~poly(x, 2))

#### FORBS ####
#seedlings with forb cover
seed %>% filter(!Treatment %in% c("F", "GF", "FB", "FGB")) %>% 
  ggplot(aes(x = forbCov, y = seedSum, colour = factor(Round))) +
  geom_point() +
  geom_smooth(method = "glm", formula=y~poly(x, 2), method.args = list(family = "quasipoisson")) 
ggsave(filename = paste0("forbs_drought_precip.jpg"), height = 4, width = 10, dpi = 300)

#seedlings with forb height
seed %>% filter(!Treatment %in% c("F", "GF", "FB", "FGB")) %>% 
  ggplot(aes(x = forb, y = seedSum, colour = factor(Round))) +
  geom_point() +
  geom_smooth(method = "glm", formula=y~poly(x, 2), method.args = list(family = "quasipoisson")) 

#### TRAITS ####
# seedlings with seed mass
seed %>% #filter(!Treatment %in% c("F", "GF", "FB", "FGB")) %>% 
  ggplot(aes(x = wmSM, y = seedSum, colour = factor(Round))) +
  geom_point() +
  geom_smooth(method = "glm", formula=y~poly(x, 2), method.args = list(family = "quasipoisson")) 


# seedlings with sla
seed %>% #filter(!Treatment %in% c("F", "GF", "FB", "FGB")) %>% 
  ggplot(aes(x = wmSLA, y = seedSum)) +
  geom_point() +
  geom_smooth(method = "lm", formula=y~poly(x, 2))
