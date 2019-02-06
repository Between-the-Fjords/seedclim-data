library(broom)
library(lme4)
library(nlme)
library(wesanderson)

traitOrder <- c("wmeanheight", "wmeanSLA", "wmeanLA", "wmeanLTH", "wmeanLDMC", "wmeanCN", "wmeanseedMass")
source("~/Documents/seedclimComm/seedclimComm/inst/graminoidRemovals/plotting_dim.R")

traitVariance <- wholecom %>%
  select(siteID:precipLevel, wmeanLDMC:wmeanseedMass, funYear) %>% 
  gather(key = trait, value = measurement, c(wmeanLDMC:wmeanCN, wmeanseedMass))


#http://r.789695.n4.nabble.com/How-to-extract-parameter-estimates-of-variance-function-from-lme-fit-td2997153.html

####------------ Plots --------------####

traitVariance %>% filter(funYear %in% c("forb_2011", "forb_2016")) %>%
  group_by(trait, Year, tempLevel, TTtreat) %>% 
  summarise(mean = mean(measurement, na.rm = TRUE), variance = var(measurement, na.rm = TRUE)) %>% 
  mutate(upper = (mean + variance), lower = (mean - variance)) %>% 
  ggplot(aes(y = mean, x = factor(tempLevel), ymin = lower, ymax = upper, colour = factor(Year))) +
  geom_errorbar(width = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5)) +
  facet_grid(trait ~ TTtreat, scales = "free")

weathervar <- bind_rows(modvar2precip, modvar2temp) %>% ungroup() %>% 
  mutate(Weather = ifelse(Weather_level %in% c("10.5", "8.5", "6.5"), "Temperature", "Precipitation"), Weather_level = as.numeric(Weather_level)) 
weathervar <- arrange(mutate(weathervar, trait = factor(trait,levels=traitOrder)), trait)


traitScale %>% 
  filter(scale == "local", TTtreat == "TTC", Year == 2011, trait == "wmeanSLA") %>% 
  ggplot(aes(measurement, fill = temp)) +
  scale_fill_manual(values = cbPalette[c(9,8,7)]) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  axis.dim +
  facet_wrap( ~ functionalGroup, scales = "free") +
  labs(x = "SLA", fill = "Temperature (C)") 
  ggsave(filename = paste0("IAVS_LDMC_functionalgroup_temp.jpg"), height = 4, width = 8, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")


traitScale %>% 
  filter(scale == "local", TTtreat == "TTC", Year == 2011) %>% 
  ggplot(aes(measurement, fill = precip)) +
  scale_fill_manual(values = cbPalette[c(10,4,2,5)]) +
  geom_density(alpha = 0.4) +
  theme_classic() +
  axis.dim +
  facet_wrap(trait ~ functionalGroup, scales = "free") +
  labs(x = "SLA", fill = "Temperature (C)")
  ggsave(filename = paste0("IAVS_SLA_functionalgroup_temp.jpg"), height = 4, width = 8, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")

  ggsave(filename = paste0("fig3_gramRem.jpg"), height = 4, width = 10.5, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")


x <- traitVariance %>% 
  filter(TTtreat == "RTC", Year == c(2011,2016)) %>% 
  mutate(Year = factor(Year), precipLevel = factor(precipLevel)) %>% 
  ggplot(aes(measurement, fill = interaction(precipLevel, Year), linetype = interaction(precipLevel, Year), alpha = interaction(precipLevel, Year))) +
  geom_density() +
  scale_fill_manual("Precipitation", values = cbPalette[c(1, 1, 1, 1, 7, 2, 4, 10)]) +
  scale_linetype_manual("Precipitation", values = c("dashed", "solid", "dotted", "longdash", "dashed", "solid", "dotted", "longdash")) +
  scale_alpha_manual("Precipitation", values = c(0.3, 0.3, 0.3, 0.3, 0.5, 0.5, 0.5, 0.5)) +
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
  mutate(Precipitation_level = as.factor(tempLevel), Year = as.factor(Year)) %>% 
  ggplot(aes(measurement, fill = interaction(tempLevel, Year), linetype = interaction(tempLevel, Year), alpha =  interaction(tempLevel, Year))) +
  geom_density() +
  scale_fill_manual("Temperature", values = cbPalette[c(1, 1, 1, 9, 8, 7)]) +
  scale_linetype_manual("Temperature", values = c("dashed", "solid", "dotted", "dashed", "solid", "dotted")) +
  scale_alpha_manual("Temperature", values = c(0.3, 0.3, 0.3, 0.5, 0.5, 0.5)) +
  theme_classic() +
  axis.dim +
  guides(fill = guide_legend(title.position = "top")) +
  theme(legend.position = "top") +
  facet_wrap( ~ trait, scales = "free", ncol = 1, strip.position = "right")
  #ggsave(filename = paste0("fig5_traitVar_temp_gramRem.jpg"), height = 4.5, width = 12, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")


z <- plot_grid(x, y, labels = c('A', 'B'), nrow = 1, align = 'v')
ggsave(z, filename = "~/OneDrive - University of Bergen/Research/FunCaB/figures/traitVariance.jpg", width = 8.5, height = 12, dpi = 300)


leg <- "Functional groups \n in 2011 and 2016"

# seed mass variance with temperature
wholecom %>% 
  gather(key = trait, value = measurement, c(wmeanLDMC:wmeanseedMass)) %>% 
  mutate(temp = if_else(grepl("6.5", tempLevel), "Alpine", if_else(grepl("8.5", tempLevel), "Sub-alpine", "Boreal"))) %>% 
  mutate(temp = factor(temp, levels = c("Alpine", "Sub-alpine", "Boreal"))) %>%
  filter(TTtreat == "RTC", trait == "wmeanLA") %>% 
  filter(funYear %in% c("forb_2011", "forb_2016", "graminoid_2011")) %>% 
  ggplot(aes(measurement, fill = factor(funYear), colour = factor(funYear), group = factor(funYear))) +
  scale_fill_manual(leg, values = c("#F0E442", "#E69F00", "#81A88D"), labels = c("forbs 2011", "forbs 2016", "graminoids 2011")) +
  scale_colour_manual(leg, values = c("#F0E442", "#E69F00", "#81A88D"), labels = c("forbs 2011", "forbs 2016", "graminoids 2011")) +
  geom_density(alpha = 0.5, trim = FALSE, size = 0.9) +
  geom_rug(aes(colour = as.factor(funYear)), size = 0.8) +
  theme_cowplot(font_family = "Helvetica") +
  axis.dim +
  facet_grid(. ~ temp) +
  theme(legend.position = c(0.8, 0.73),
        strip.background = element_blank(),
        plot.margin = unit(c(0,0,1,0), "cm")) +
  labs(x = "Community weighted mean seed mass (mg) ")

slaVar <- plot_grid(slaVar, labels = "A")

TEST <- plot_grid(slaVar, coefEst, nrow = 2, rel_heights = c(0.35,1))

ggsave(TEST, filename = "fig16coefEst_v4TEST.jpg", height = 12.5, width = 8, dpi = 300)

# SLA variance with precipitation
wholecom %>% 
  gather(key = trait, value = measurement, c(wmeanLDMC:wmeanseedMass)) %>% 
  mutate(temp = if_else(grepl("6.5", tempLevel), "Alpine", if_else(grepl("8.5", tempLevel), "Sub-alpine", "Boreal"))) %>% 
  mutate(temp = factor(temp, levels = c("Alpine", "Sub-alpine", "Boreal"))) %>% 
  filter(TTtreat == "RTC", trait == "wmeanSLA") %>% 
  filter(funYear %in% c("forb_2011", "forb_2016", "graminoid_2011")) %>% 
  ggplot(aes(measurement, fill = factor(funYear), colour = factor(funYear), group = factor(funYear))) +
  scale_fill_manual(leg, values = c("#F0E442", "#E69F00", "#81A88D"), labels = c("forbs 2011", "forbs 2016", "graminoids 2011")) +
  scale_colour_manual(leg, values = c("#F0E442", "#E69F00", "#81A88D"), labels = c("forbs 2011", "forbs 2016", "graminoids 2011")) +
  geom_density(alpha = 0.5, trim = FALSE, size = 0.9) +
  geom_rug(aes(colour = as.factor(funYear)), size = 0.8) +
  theme_cowplot(font_family = "Helvetica") +
  axis.dim +
  facet_grid(. ~ precipLevel) +
  theme(legend.position = c(0.8, 0.73),
        strip.background = element_blank(),
        plot.margin = unit(c(0,0,1,0), "cm")) +
  labs(x = expression("Community weighted mean SLA "(cm^2/g)))

slaVar <- plot_grid(slaVar, labels = "A")

TEST <- plot_grid(slaVar, coefEst, nrow = 2, rel_heights = c(0.35,1))

ggsave(TEST, filename = "fig16coefEst_v4TEST.jpg", height = 12.5, width = 8, dpi = 300)
