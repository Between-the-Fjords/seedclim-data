#### predictions ####

#----------------------------------------
# SLA model
forbcomSLA <- filter(forbcom, !is.na(wmeanSLA))

modsla <- lmer(wmeanSLA ~ TTtreat*Stemp0916*Sprecip0916*SYear - TTtreat:Stemp0916:Sprecip0916:SYear + (1|siteID/blockID), REML = FALSE, data = forbcomSLA)

modsla %>% 
  tidy() %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

forbcomSLA$modPreds <- predict(modsla)

forbcomSLA %>% #gather(wmeanSLA, modPreds, key = Model, value = value) %>% 
  ggplot(aes(x = Year, y = modPreds, colour = TTtreat)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.1)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.1), geom = "line") +
  #geom_point(aes(y = wmeanSLA)) +
  scale_alpha_manual(values = c(0.5, 1)) + 
  scale_color_manual(values = pal1) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  facet_wrap(~ tempLevel)

rtcmeta %>% 
  filter(Year == 2016) %>% 
  ggplot(aes(x = precip7010, y = deltawmeanLA, colour = factor(tempLevel))) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line") +
  #scale_alpha_manual(values = c(0.5, 1)) + 
  scale_color_manual(values = pal1) +
  #scale_linetype_manual(values = c("dashed", "solid")) +
  geom_hline(yintercept = 0, colour = "grey60")

#----------------- Models -----------------------
# trait means
forbcomCN <- filter(forbcom, !is.na(wmeanseedMass), !is.na(wmeanCN), !is.na(wmeanLDMC), !is.na(wmeanSLA), !is.na(wmeanLA), !is.na(wmeanLTH), !is.na(wmeanheight)) %>% 
  gather(wmeanLDMC:wmeanseedMass, key = trait, value = value)

modMN <- forbcomCN %>% 
  group_by(trait) %>% 
  do({
    mod = lmer(value ~ TTtreat*Stemp0916*Sprecip0916*SYear - TTtreat:Stemp0916:Sprecip0916:SYear + (1|siteID/blockID), REML = FALSE, data = .)
    tidy(mod, data = .)
  }) %>% 
  arrange(desc(trait)) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>% 
  ungroup()


modMN <- modMN %>% 
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


modMNA <- forbcomAnalysis %>% 
  group_by(trait) %>% 
  do({
    mod = lmer(measurement ~ TTtreat*Stemp0916*Sprecip0916*SYear - TTtreat:Stemp0916:Sprecip0916:SYear + (1|siteID/blockID), REML = FALSE, data = .)
    tidy(mod, data = .)
  }) %>% 
  arrange(desc(trait)) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>% 
  ungroup()

modMNA <- modMNA %>% 
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

# run model for predictions
modMN <- forbcomCN %>% 
  group_by(trait) %>% 
  do({
    mod = lmer(value ~ TTtreat*Stemp0916*Sprecip0916*SYear - TTtreat:Stemp0916:Sprecip0916:SYear + (1|siteID/blockID), REML = FALSE, data = .)
    augment(mod, data = .)
  }) %>% 
  mutate(TTtreat = factor(TTtreat, levels = c("RTC", "TTC"))) %>% 
  rename("preds" = `.fitted`) %>% 
  gather(preds, value, key = mod, value = data)

# trait variances
forbcomVar <- filter(forbcom, !is.na(cwvseedMass), !is.na(cwvCN), !is.na(cwvLDMC), !is.na(cwvSLA), !is.na(cwvLA), !is.na(cwvLTH), !is.na(cwvheight)) %>% 
  gather(cwvLDMC:cwvseedMass, key = trait, value = value)

# run model for predictions
modVar <- forbcomVar %>% 
  group_by(trait) %>% 
  do({
    mod = lmer(value ~ TTtreat*Stemp0916*Sprecip0916*SYear - TTtreat:Stemp0916:Sprecip0916:SYear + (1|siteID/blockID), REML = FALSE, data = .)
    augment(mod, data = .)
  }) %>% 
  mutate(TTtreat = factor(TTtreat, levels = c("RTC", "TTC"))) %>% 
  rename("preds" = `.fitted`) %>% 
  gather(preds, value, key = mod, value = data)

#------------- TEMPERATURE ---------------------------
plotmodMNt

modMN %>% 
  filter(!trait %in% c("wmeanseedMass", "wmeanLA")) %>%
  filter(mod == "value") %>% 
  ggplot(aes(x = Year, y = data, colour = factor(tempLevel))) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", size = 1, mapping = aes(alpha = TTtreat)) +
  scale_colour_manual(values = pal1[c(3,5,4)]) +
  scale_alpha_manual(values = c(1, 0.6)) +
  facet_wrap(~trait, scales = "free_y", nrow = 1) +
  axis.dimLarge


plotmodVart <- modVar %>% 
  filter(!trait %in% c("cwvseedMass", "cwvLDMC")) %>% 
  filter(mod == "value") %>% 
  ggplot(aes(x = Year, y = data, colour = factor(tempLevel))) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", size = 1, mapping = aes(alpha = TTtreat)) +
  scale_colour_manual(values = pal1[c(3,5,4)]) +
  scale_alpha_manual(values = c(1, 0.6)) +
  facet_wrap(~trait, scales = "free_y", nrow = 1)  +
  axis.dimLarge

tmodPredsTraits <- plot_grid(plotmodMNt, plotmodVart, nrow = 2)
ggsave(tmodPredsTraits, filename = "~/OneDrive - University of Bergen/Research/FunCaB/paper 1/figures/tempmodelPredictionsTraits.jpg", dpi = 300, height = 6, width = 13)


#------------- PRECIPITATION ---------------------------
plotmodMNP
modMN %>% 
  filter(!trait %in% c("wmeanseedMass", "wmeanLA")) %>%
  filter(mod == "value") %>% 
  ggplot(aes(x = Year, y = data, colour = factor(precipLevel))) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", size = 1, mapping = aes(alpha = TTtreat)) +
  scale_colour_manual(values = pal2[c(7,1,4,3)]) +
  scale_alpha_manual(values = c(1, 0.6)) +
  facet_wrap(~trait, scales = "free_y", nrow = 1)  +
  axis.dimLarge

plotmodVarP <- modVar %>% 
  ggplot(aes(x = Year, y = data, colour = factor(precipLevel), alpha = mod)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", size = 1, mapping = aes(linetype = TTtreat)) +
  scale_colour_manual(values = pal2[c(7,1,4,3)]) +
  scale_alpha_manual(values = c(0.6, 1)) +
  facet_wrap(~trait, scales = "free_y", nrow = 1)  +
  axis.dimLarge


PmodPredsTraits <- plot_grid(plotmodMNP, plotmodVarP, nrow = 2)
ggsave(PmodPredsTraits, filename = "~/OneDrive - University of Bergen/Research/FunCaB/paper 1/figures/PrecipmodelPredictionsTraits.jpg", dpi = 300, height = 6, width = 14)


#------------- TIME DELTAS ---------------------------
#------------- TEMPERATURE ---------------------------

treatwmTD <- timedelta %>% 
  gather(key = trait, value = measurement, c(deltarichness, deltaevenness, deltasumcover, deltawmeanLDMC:deltacwvseedMass)) %>%
  filter(trait %in% c("deltawmeanSLA", "deltawmeanLTH", "deltawmeanCN", "deltawmeanheight", "deltawmeanLDMC")) %>%
  ggplot(aes(x = Year, y = measurement, colour = factor(tempLevel), linetype = TTtreat, alpha = TTtreat)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", size = 1) +
  scale_colour_manual(values = pal1[c(3,5,4)]) +
  scale_alpha_manual("", values = c(1, 0.6), labels = c("Removal", "Untreated")) +
  scale_linetype_discrete("", labels = c("Removal", "Untreated")) +
  facet_wrap(~trait, scales = "free_y", 
             nrow = 1) +
  geom_hline(yintercept = 0) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 10),
        legend.position = "bottom")

treatwvTD <- timedelta %>% 
  gather(key = trait, value = measurement, c(deltarichness, deltaevenness, deltasumcover, deltawmeanLDMC:deltacwvseedMass)) %>%
  filter(trait %in% c("deltacwvSLA", "deltacwvLTH", "deltacwvCN", "deltacwvheight", "deltacwvLDMC")) %>%
  ggplot(aes(x = Year, y = measurement, colour = factor(tempLevel), linetype = TTtreat, alpha = TTtreat)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", size = 1) +
  scale_colour_manual(values = pal1[c(3,5,4)]) +
  scale_alpha_manual("", values = c(1, 0.6), labels = c("Removal", "Untreated")) +
  scale_linetype_discrete("", labels = c("Removal", "Untreated")) +
  facet_wrap(~trait, scales = "free_y", 
             nrow = 1) +
  geom_hline(yintercept = 0) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = "none",
        strip.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 10))

legend <- get_legend(treatwmTD)

#------------- PRECIPITATION ---------------------------
plotModprecip <- modMNA %>% filter(term %in% c("(Intercept)", "P", "P x removal", "P x year", "P x year x removal")) %>%
  filter(trait %in% c("SLA", "LTH", "CN", "height", "LDMC")) %>%
  filter(test == "Mean") %>% 
  ggplot(aes(x = trait, y = estimate, ymin = lower, ymax = upper)) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5), size = 0.8, aes(colour = factor(term, levels = c("(Intercept)", "P", "P x removal", "P x year", "P x year x removal")))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(aes(colour = factor(term, levels = c("(Intercept)", "P", "P x removal", "P x year", "P x year x removal"))), position = position_dodge(width = 0.5), size = 5) +
  #scale_colour_manual("", values = pal2[c(7,1,4,3)]) +
  scale_colour_manual("", values = pal2[c(7,4,1,3)]) +
  theme(axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title = element_blank(),
        legend.position = "bottom") +
  scale_x_discrete(expand=c(0.07, 0.07))

legendPrecip <- get_legend(plotModprecip)

tmodPredsTraits <- plot_grid(treatwmTD + theme(legend.position = "none"), treatwvTD, plotModprecip + theme(legend.position = "none"), nrow = 3, labels = c("A", "B", "C"), align = "hv", axis = "tblr", rel_heights = c(0.35, 0.35, 0.3))
legends <- plot_grid(legend, legendPrecip, nrow = 1)
combiPlot <- plot_grid(tmodPredsTraits, legends, nrow = 2, ncol = 1, rel_heights = c(0.9,0.1))
ggsave(combiPlot, filename = "~/OneDrive - University of Bergen/Research/FunCaB/paper 1/figures/combiTest.jpg", dpi = 300, height = 8, width = 12.5)


ggsave(tmodPredsTraits, filename = "~/OneDrive - University of Bergen/Research/FunCaB/paper 1/figures/tempTdeltaTraits.jpg", dpi = 300, height = 6.5, width = 15)



plotmodMNP
timedelta %>% 
  gather(key = trait, value = measurement, c(deltarichness, deltaevenness, deltasumcover, deltawmeanLDMC:deltacwvseedMass)) %>%
  filter(trait %in% c("deltawmeanSLA", "deltawmeanLTH", "deltawmeanCN", "deltawmeanheight", "deltawmeanLDMC")) %>%
  ggplot(aes(x = Year, y = measurement, colour = factor(precipLevel), linetype = TTtreat)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", size = 1, mapping = aes(alpha = TTtreat)) +
  scale_colour_manual(values = pal2[c(7,1,4,3)]) +
  scale_alpha_manual("", values = c(1, 0.6), limits = c("Removal", "Untreated")) +
  scale_linetype_discrete("", labels = c("Removal", "Untreated")) +
  facet_wrap(~trait, scales = "free_y", nrow = 1) +
  labs(x = "") +
  geom_hline(yintercept = 0) +
  theme_cowplot() +
  theme(strip.background = element_blank())

plotmodVarP <- timedelta %>% 
  gather(key = trait, value = measurement, c(deltarichness, deltaevenness, deltasumcover, deltawmeanLDMC:deltacwvseedMass)) %>%
  filter(trait %in% c("deltacwvSLA", "deltacwvLTH", "deltacwvCN", "deltacwvheight", "deltacwvLDMC")) %>%
  ggplot(aes(x = Year, y = measurement, colour = factor(precipLevel), linetype = TTtreat)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", size = 1, mapping = aes(alpha = TTtreat)) +
  scale_colour_manual("Mean annual\n precipitation", values = pal2[c(7,1,4,3)]) +
  scale_alpha_manual("", values = c(1, 0.6), limits = c("Removal", "Untreated")) +
  scale_linetype_discrete("", labels = c("Removal", "Untreated")) +
  facet_wrap(~trait, scales = "free_y", nrow = 1) +
  labs(x = "") +
  geom_hline(yintercept = 0) +
  theme_cowplot() +
  theme(strip.background = element_blank())


#------------- TEMPERATURE RAW ---------------------------
rawWm <- forbcom %>% 
  mutate(Year = as.numeric(as.character(Year))) %>% 
  gather(key = trait, value = measurement, c(richness, evenness, sumcover, wmeanLDMC:cwvseedMass)) %>%
  filter(trait %in% c("wmeanSLA", "wmeanLTH", "wmeanCN", "wmeanheight", "wmeanLDMC")) %>%
  ggplot(aes(x = Year, y = measurement, colour = factor(tempLevel), linetype = TTtreat, alpha = TTtreat)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", size = 1) +
  scale_colour_manual("Mean summer\n temperature", values = pal1[c(3,5,4)]) +
  scale_alpha_manual("", values = c(1, 0.6), labels = c("Removal", "Untreated")) +
  scale_linetype_discrete("", labels = c("Removal", "Untreated")) +
  facet_wrap(~trait, scales = "free_y", nrow = 1) +
  labs(x = "") +
  #geom_hline(yintercept = 0) +
  theme_cowplot() +
  theme(strip.background = element_blank())

rawWv <- forbcom %>% 
  mutate(Year = as.numeric(as.character(Year))) %>% 
  gather(key = trait, value = measurement, c(richness, evenness, sumcover, wmeanLDMC:cwvseedMass)) %>%
  filter(trait %in% c("cwvSLA", "cwvLTH", "cwvCN", "cwvheight", "cwvLDMC")) %>%
  ggplot(aes(x = Year, y = measurement, colour = factor(tempLevel), linetype = TTtreat, alpha = TTtreat)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", size = 1) +
  scale_colour_manual("Mean summer\n temperature", values = pal1[c(3,5,4)]) +
  scale_alpha_manual("", values = c(1, 0.6), labels = c("Removal", "Untreated")) +
  scale_linetype_discrete("", labels = c("Removal", "Untreated")) +
  facet_wrap(~trait, scales = "free_y", nrow = 1) +
  labs(x = "") +
  #geom_hline(yintercept = 0) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = "none")

legend <- get_legend(rawWm)

trawTraits <- plot_grid(rawWm + theme(legend.position = "none"), rawWv, nrow = 2)
trawTraits <- plot_grid(trawTraits, legend, rel_widths = c(0.9,0.1))
ggsave(trawTraits, filename = "~/OneDrive - University of Bergen/Research/FunCaB/paper 1/figures/tempRawTraits.jpg", dpi = 300, height = 6.5, width = 15)


#------------- TREATMENT DELTA ------------------
treatwmD <- rtcmeta %>% 
  mutate(Year = as.numeric(as.character(Year))) %>% 
  gather(key = trait, value = measurement, c(deltarichness, deltaevenness, deltasumcover, deltawmeanLDMC:deltacwvseedMass)) %>%
  filter(trait %in% c("deltawmeanSLA", "deltawmeanLTH", "deltawmeanCN", "deltawmeanheight", "deltawmeanLDMC")) %>%
  ggplot(aes(x = Year, y = measurement, colour = factor(tempLevel))) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", size = 1) +
  scale_colour_manual("Mean summer\n temperature", values = pal1[c(3,5,4)]) +
  facet_wrap(~trait, scales = "free_y", nrow = 1) +
  labs(x = "") +
  geom_vline(xintercept = 2011.5, linetype = "dashed", colour = "grey60") +
  geom_hline(yintercept = 0) +
  theme_cowplot() +
  theme(strip.background = element_blank())

treatwvD <- rtcmeta %>% 
  mutate(Year = as.numeric(as.character(Year))) %>% 
  gather(key = trait, value = measurement, c(deltarichness, deltaevenness, deltasumcover, deltawmeanLDMC:deltacwvseedMass)) %>%
  filter(trait %in% c("deltacwvSLA", "deltacwvLTH", "deltacwvCN", "deltacwvheight", "deltacwvLDMC")) %>%
  ggplot(aes(x = Year, y = measurement, colour = factor(tempLevel))) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", size = 1) +
  scale_colour_manual("Mean summer\n temperature", values = pal1[c(3,5,4)]) +
  facet_wrap(~trait, scales = "free_y", nrow = 1) +
  labs(x = "") +
  geom_vline(xintercept = 2011.5, linetype = "dashed", colour = "grey60") +geom_hline(yintercept = 0) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = "none")

legend <- get_legend(treatwmD)


tmodPredsTraits <- plot_grid(treatwmD + theme(legend.position = "none"), treatwvD, nrow = 2)
tmodPredsTraits <- plot_grid(tmodPredsTraits, legend, rel_widths = c(0.9,0.1))
ggsave(tmodPredsTraits, filename = "~/OneDrive - University of Bergen/Research/FunCaB/paper 1/figures/tempdeltaTraits.jpg", dpi = 300, height = 6.5, width = 15)

# ------------- 2016 ONLY --------------------
## TEMP/PRECIP INTERACTION
modMN %>% filter(Year == 2016) %>% 
  filter(!trait %in% c("wmeanseedMass", "wmeanLA")) %>%
  filter(mod == "value") %>% 
  ggplot(aes(x = temp7010, y = data, colour = factor(precipLevel))) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", size = 1, mapping = aes(alpha = TTtreat)) +
  scale_colour_manual(values = pal2[c(7,1,4,3)]) +
  scale_alpha_manual(values = c(0.6, 1)) +
  facet_wrap(~trait, scales = "free_y", nrow = 1)  +
  axis.dimLarge

ggsave(tempPrecip2016, filename = "~/OneDrive - University of Bergen/Research/FunCaB/paper 1/figures/tempPrecip2016.jpg", dpi = 300, height = 4.5, width = 16)

## TEMP ALONE
modMN %>% filter(Year == 2016) %>% 
  filter(!trait %in% c("wmeanseedMass", "wmeanLA")) %>%
  filter(mod == "value") %>% 
  ggplot(aes(x = tempLevel, y = data, colour = trait)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", size = 1, mapping = aes(alpha = TTtreat)) +
  scale_colour_manual(values = pal2[c(7,1,4,3,2)]) +
  scale_alpha_manual(values = c(0.6, 1)) +
  facet_wrap(~trait, scales = "free_y", nrow = 1)  +
  axis.dimLarge

## PRECIP ALONE
modMN %>% filter(Year == 2016) %>% 
  filter(!trait %in% c("wmeanseedMass", "wmeanLA")) %>%
  filter(mod == "value") %>% 
  ggplot(aes(x = precipLevel, y = data, colour = trait)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", size = 1, mapping = aes(alpha = TTtreat)) +
  scale_colour_manual(values = pal2[c(7,1,4,3,2)]) +
  scale_alpha_manual(values = c(0.6, 1)) +
  facet_wrap(~trait, scales = "free_y", nrow = 1)  +
  axis.dimLarge

#ggsave(tempPrecip2016, filename = "~/OneDrive - University of Bergen/Research/FunCaB/paper 1/figures/tempPrecip2016.jpg", dpi = 300, height = 4.5, width = 16)
