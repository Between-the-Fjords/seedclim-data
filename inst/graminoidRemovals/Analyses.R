#load packages
library(lme4)
library(MuMIn)
library(GGally)
library(tibble)
library(broom)

# Scaling explanatory variables
# relevel treatment so that TTC is the intercept
forbcom <- forbcom %>% 
  mutate(Sprecip0916 = as.numeric(scale(precip0916)),
         Stemp0916 = as.numeric(scale(temp0916)),
         SYear = as.numeric(scale(Year))) %>% 
  mutate(TTtreat = factor(TTtreat, levels = c("TTC", "RTC")))


# gather traits for analyses
forbcomAnalysis <- forbcom %>%
  mutate(wmeanLDMC = as.numeric(scale(wmeanLDMC)), wmeanseedMass = as.numeric(scale(wmeanseedMass)), wmeanCN = as.numeric(scale(wmeanCN)), wmeanheight = as.numeric(scale(wmeanheight)), wmeanSLA = as.numeric(scale(wmeanSLA)), wmeanLA = as.numeric(scale(wmeanLA)), wmeanLTH = as.numeric(scale(wmeanLTH)), sumcover = as.numeric(scale(sumcover)), evenness = as.numeric(scale(evenness)), richness = as.numeric(scale(richness)), cwvLDMC = as.numeric(scale(cwvLDMC)), cwvseedMass = as.numeric(scale(cwvseedMass)), cwvCN = as.numeric(scale(cwvCN)), cwvheight = as.numeric(scale(cwvheight)), cwvSLA = as.numeric(scale(cwvSLA)), cwvLA = as.numeric(scale(cwvLA)), cwvLTH = as.numeric(scale(cwvLTH))
  ) %>% 
  gather(key = trait, value = measurement, c(richness, evenness, sumcover, wmeanLDMC:cwvseedMass)) %>% 
  filter(!is.na(measurement))
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mod1temp <- forbcomAnalysis %>% 
  group_by(trait) %>%
  do({
    mod <- lmer(measurement ~ TTtreat*Stemp0916*Sprecip0916*SYear - TTtreat:Stemp0916:Sprecip0916:SYear + (1|siteID/blockID), REML = FALSE, data = .)
    tidy(mod)}) %>% 
  #filter(term %in% c("TTtreatRTC","TTtreatRTC:Stemp0916:SYear", "TTtreatRTC:Sprecip0916:SYear", "TTtreatRTC:SYear")) %>% 
  arrange(desc(trait)) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>% 
  ungroup()

mod1temp <- mod1temp %>% 
  mutate(test = case_when(
    grepl("wmean", trait) ~ "Mean",
    grepl("cwv", trait) ~ "Variance",
    grepl("^s|^r|^e", trait) ~ "Mean"),
    term = case_when(
      term == "(Intercept)" ~ "Control",
      term == "Stemp0916" ~ "t",
      term == "Sprecip0916" ~ "P",
      term == "SYear" ~ "year",
      term =="TTtreatRTC:Stemp0916" ~ "t x removal",
      term =="TTtreatRTC:Sprecip0916" ~ "P x removal",
      term =="TTtreatRTC:Stemp0916:SYear" ~ "t x year x removal",
      term =="TTtreatRTC:Sprecip0916:SYear" ~ "P x year x removal",
      term =="TTtreatRTC:Stemp0916:Sprecip0916" ~ "P x t x removal",
      term =="Stemp0916:SYear" ~ "t x year",
      term =="Sprecip0916:SYear" ~ "P x year",
      term =="Stemp0916:Sprecip0916" ~ "P x t",
      term =="Stemp0916:Sprecip0916:SYear" ~ "P x t x year",
      term =="TTtreatRTC:SYear" ~ "Year x removal",
      term == "TTtreatRTC" ~ "removal")) %>% 
  mutate(trait = if_else(grepl("wmean", trait), substr(trait, 6, n()),
                         if_else(grepl("cwv", trait), substr(trait, 4, n()), trait))) %>% 
  mutate(sign = recode(trait, sumcover = 1, evenness = 1, richness = 1, seedMass = 1, height = 0, LA = 0, LTH = 0, LDMC = 0, CN = 1, SLA = 1))

#write.csv(mod1temp, file = "~/OneDrive - University of Bergen/Research/mod1tempOUT.csv")

coefEst <- mod1temp %>%
  filter(term %in% c("P x year x removal", "t x year x removal", "Year x removal", "removal")) %>% 
  ggplot(aes(x = trait, y = estimate, ymin = lower, ymax = upper, fill = factor(term, levels = c("P x year x removal", "t x year x removal", "Year x removal", "removal")), shape = factor(term, levels = c("P x year x removal", "t x year x removal", "Year x removal", "removal")), alpha = as.factor(sign))) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5), aes(colour = factor(term, levels = c("P x year x removal", "t x year x removal", "Year x removal", "removal")))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 2.9) +
  coord_flip() +
  geom_vline(xintercept =  c(1.5,2.5,3.5,5.5,6.5,8.5,9.5), colour = "grey90") +
  geom_vline(xintercept =  7.5, colour = "black") +
  geom_vline(xintercept =  4.5, colour = "grey50") +
  scale_alpha_manual(values = c(0.6, 1), guide = FALSE) +
  scale_fill_manual(legend.title.climate, values = c("#1C9099", "#E69F00", "grey90", "black")) +
  scale_colour_manual(legend.title.climate, values = c("black", "black", "black", "black")) +
  scale_shape_manual(legend.title.climate, values = c(25, 24, 23, 21)) +
  #scale_linetype_manual(legend.title.climate, values = c(1,1,3, 21,21,23, 25)) +
  scale_x_discrete(limits = c("SLA", "CN", "LDMC", "LTH", "LA", "height", "seedMass", "richness", "evenness", "sumcover"), labels = c("SLA", "C:N ratio", "Leaf dry \n matter content", "Leaf thickness", "Leaf area", "Height", "Seed mass", "Richness", "Evenness", "Cover")) +
  facet_wrap(~test, strip.position = "top", scales = "free_x") +
  labs(y = "Standardised coefficients", x = "Leaf economic traits                 Structural traits               Community structure") +
  theme_cowplot(font_family = "Helvetica") +
  #ylim(c(-0.4, 0.6)) +
  theme(strip.background = element_rect(fill="white"),
        legend.position = "bottom",
        legend.justification = "centre",
        legend.background = element_rect(fill = "white"),
        strip.text = element_text(size = 14, hjust = 0.42),
        axis.ticks.y = element_blank()) +
  theme(axis.text.y = element_text(colour = c("black", "black", "grey40", "grey40", "grey40", "grey40", "black", "black", "black", "black")))
  
coefEst <- plot_grid(coefEst, labels = c("B                                                       C"), label_x = 0)

