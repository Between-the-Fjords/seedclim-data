source("~/Documents/seedclimComm/seedclimComm/inst/graminoidRemovals/plotting_dim.R")

##### FUNCTIONS FOR PLOTTING #####
# per response variable
time.plots.temp <- function(dat, response, save, ylab) {
  p <- ggplot(dat, aes_string(x = "Year", y = response, colour = "interaction(Precipitation_level, TTtreat)", alpha = "interaction(Precipitation_level, TTtreat)", shape = "interaction(Precipitation_level, TTtreat)", linetype = "interaction(Precipitation_level, TTtreat)", group = "interaction(Precipitation_level, TTtreat)")) +
    stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6)) +
    stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_grid(as.formula(paste(".~ ", "Temperature_level"))) +
    scale_alpha_manual(legend.title.prec, values = c(0.5, 0.5, 0.5, 0.5, 1, 1, 1, 1)) +
    scale_color_manual(legend.title.prec, values = cbPalette[c(1, 1, 1, 1, 7, 2, 4, 3)]) +
    scale_linetype_manual(legend.title.prec, values = c("dashed", "dashed", "dashed", "dashed", "solid", "solid", "solid", "solid")) +
    scale_shape_manual(legend.title.prec, values = c(0, 1, 2, 8, 15, 16, 17, 8)) +
    theme_classic() +
    theme(strip.background = element_blank()) +
    labs(y = if_else(ylab == TRUE,
                     paste("Δ", substr(response, 6, nchar(response))),
                     ""), colour = "") +
    #theme(legend.text = c("Control", "Treatment")) +
    axis.dim
  if(save == TRUE){ 
    ggsave(p, filename = paste0(response, "temp.jpg", sep = ""), height = 4, width = 8, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")
  }# defaults to save == FALSE, so you don't have to write that in every time
  return(p)
}

time.plots.precip <- function(dat, response, save, ylab) {
  p <- ggplot(dat, aes_string(x = "Year", y = response, colour = "interaction(Temperature_level, TTtreat)", alpha = "interaction(Temperature_level, TTtreat)", shape = "interaction(Temperature_level, TTtreat)", linetype = "interaction(Temperature_level, TTtreat)", group = "interaction(Temperature_level, TTtreat)")) +
    stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6)) +
    stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_grid(as.formula(paste(".~ ", "Precipitation_level"))) +
    scale_alpha_manual(legend.title.temp, values = c(0.5, 0.5, 0.5, 1, 1, 1)) +
    scale_color_manual(legend.title.temp, values = cbPalette[c(1, 1, 1, 9, 8, 7)]) +
    scale_linetype_manual(legend.title.temp, values = c("dashed", "dashed", "dashed", "solid", "solid", "solid")) +
    scale_shape_manual(legend.title.temp, values = c(0, 1, 2, 15, 16, 17)) +
    theme_classic() +
    theme(strip.background = element_blank()) +
    labs(y = if_else(ylab == TRUE,
                     paste("Δ", substr(response, 6, nchar(response))),
                     ""), colour = "") +
    #theme(legend.text = c("Control", "Treatment")) +
    axis.dim
  if(save == TRUE){ 
    ggsave(p, filename = paste0(response, "precip.jpg", sep = ""), height = 4, width = 8, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")
  }# defaults to save == FALSE, so you don't have to write that in every time
  return(p)
}

timedelta %>% filter(TTtreat == "RTC") %>% ggplot(aes(x = Year, y = deltasumcover, colour = as.factor(Precipitation_level), group = as.factor(Temperature_level))) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), aes(group = Precipitation_level)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", aes(group = Precipitation_level)) +
  facet_grid(. ~ Temperature_level) +
  scale_colour_manual(values = cbPalette[c(10, 4, 2, 7)]) +
  geom_hline(yintercept = 0, linetype = "dashed")


####### plot for temperature and precipitation interaction
timedelta %>% filter(deltasumcover > -75) %>% 
  ggplot(aes(x = Year, y = deltasumcover, colour = as.factor(Precipitation_level), alpha = TTtreat, linetype = TTtreat, group = interaction(Precipitation_level, TTtreat))) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line") +
  facet_grid(. ~ Temperature_level) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_colour_manual(values = cbPalette[c(7, 2, 4, 10)]) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  geom_hline(yintercept = 0, linetype = "dashed")


time.plots.facet <- function(dat, response) {
  p <- ggplot(dat, aes_string(x = "as.numeric(Year)", y = response, colour = "TTtreat")) +
    stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6)) +
    stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_grid(as.formula(paste("Temperature_level ~ Precipitation_level"))) +
    scale_color_manual(values = cbPalette) +
    theme(legend.text = c("Control", "Treatment")) +
    axis.dim
  return(p)
}

density.plots <- function(dat, response, explan) {
  p <- ggplot(dat, aes_string(x = response, fill = "funYear")) +
    theme_bw() +
    scale_color_manual(values = cbPalette) +
    geom_density(alpha = 0.5) +
    #geom_smooth(method = lm) +
    #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
    facet_grid(as.formula(. ~ explan)) +
    axis.dim
  return(p)
}

  

##########################
time.plots(timedelta, response = "deltatotalBryophytes", explan = "Temperature_level", save = FALSE)

time.plots.temp(timedelta, response = "deltatotalBryophytes", save = FALSE, ylab = TRUE)
time.plots.precip(timedelta, response = "deltatotalBryophytes", save = TRUE, ylab = TRUE)


#### COVER ####
# if we dig deeper, and look at the cover of the functional groups, things start to look interesting.

time.plots.temp(timedelta, response = "deltasumcover", save = TRUE, ylab = TRUE)
time.plots.facet(timedelta, response = "deltasumcover")


deltaevenness <- ggplot(timedelta, aes(x = Year, y = deltaevenness, colour = interaction(Precipitation_level, TTtreat), alpha = interaction(Precipitation_level, TTtreat), shape = interaction(Precipitation_level, TTtreat), linetype = interaction(Precipitation_level, TTtreat), group = interaction(Precipitation_level, TTtreat))) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_alpha_manual(legend.title.prec, values = c(0.5, 0.5, 0.5, 0.5, 1, 1, 1, 1)) +
  scale_color_manual(legend.title.prec, values = cbPalette[c(1, 1, 1, 1, 7, 2, 4, 3)]) +
  scale_linetype_manual(legend.title.prec, values = c("dashed", "dashed", "dashed", "dashed", "solid", "solid", "solid", "solid")) +
  scale_shape_manual(legend.title.prec, values = c(0, 1, 2, 8, 15, 16, 17, 8)) +
  theme_classic() +
  axis.dim +
  facet_wrap(~ Temperature_level) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) 
#theme(legend.text = c("Control", "Treatment")) +


delta <- plot_grid(deltasumcover, deltaevenness, labels = c('A', 'B'), ncol = 1, align = 'h')
ggsave(filename = paste0("fig7_deltaComm_gramRem.jpg"), width = 8, height = 7, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")


###################

deltaheight <- ggplot(timedelta, aes(x = Year, y = deltawmeanheight_local, colour = interaction(Precipitation_level, TTtreat), alpha = interaction(Precipitation_level, TTtreat), shape = interaction(Precipitation_level, TTtreat), linetype = interaction(Precipitation_level, TTtreat), group = interaction(Precipitation_level, TTtreat))) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_alpha_manual(legend.title.prec, values = c(0.5, 0.5, 0.5, 0.5, 1, 1, 1, 1)) +
  scale_color_manual(legend.title.prec, values = cbPalette[c(1, 1, 1, 1, 7, 2, 4, 3)]) +
  scale_linetype_manual(legend.title.prec, values = c("dashed", "dashed", "dashed", "dashed", "solid", "solid", "solid", "solid")) +
  scale_shape_manual(legend.title.prec, values = c(0, 1, 2, 8, 15, 16, 17, 8)) +
  theme_classic() +
  axis.dim +
  facet_wrap(~ Temperature_level) +
  theme(legend.position = "none",
        strip.background = element_blank())

even_temp.orig <- timedelta %>% 
  mutate(temp = if_else(grepl("6.5", temp), "alpine", if_else(grepl("8.5", temp), "sub-alpine", "boreal"))) %>% 
  mutate(temp = factor(temp, levels = c("alpine", "sub-alpine", "boreal"))) %>% 
  ggplot(aes(x = Year, y = deltaevenness, shape = TTtreat, alpha = TTtreat, group = TTtreat)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), size = 0.75, colour = "#02401B") +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", size = 0.75, colour = "#02401B") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ temp) +
  scale_alpha_manual(legend.title.climate, values = c(0.5,1), labels = c("Control", "Removal")) +
  scale_shape_manual(legend.title.climate, values = c(1, 16), labels = c("Control", "Removal")) +
  theme_classic() +
  labs(y = paste("Δ forb community evenness")) +
  theme(legend.direction = "horizontal",
        legend.justification=c(-0.1, 1.5), 
        legend.position=c(0, 1),
        strip.background = element_blank(),
        axis.text.x  = element_text(angle = 90)) +
  axis.dimLarge

ggsave(filename = paste0("fig2A_coverSLA_v2.jpg"), width = 11, height = 4.5, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")

cover_temp.orig <- timedelta %>% 
  mutate(temp = if_else(grepl("6.5", temp), "alpine", if_else(grepl("8.5", temp), "sub-alpine", "boreal"))) %>% 
  mutate(temp = factor(temp, levels = c("alpine", "sub-alpine", "boreal"))) %>% 
  ggplot(aes(x = Year, y = deltasumcover, shape = TTtreat, alpha = TTtreat, group = TTtreat)) +
    stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), size = 0.75, colour = "#02401B") +
    stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", size = 0.75, colour = "#02401B") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_wrap(~ temp) +
    scale_alpha_manual(legend.title.climate, values = c(0.5,1), labels = c("Control", "Removal")) +
  scale_shape_manual(legend.title.climate, values = c(1, 16), labels = c("Control", "Removal")) +
  theme_classic() +
    labs(y = paste("Δ forb cover (%)")) +
  theme(legend.direction = "horizontal",
        legend.justification=c(-0.1, 1.5), 
        legend.position=c(0, 1),
        strip.background = element_blank(),
        axis.text.x  = element_text(angle = 90)) +
  axis.dimLarge


sla_temp.orig <- timedelta %>% 
  mutate(temp = if_else(grepl("6.5", temp), "alpine", if_else(grepl("8.5", temp), "sub-alpine", "boreal"))) %>% 
  mutate(temp = factor(temp, levels = c("alpine", "sub-alpine", "boreal"))) %>% 
  ggplot(aes(x = Year, y = deltawmeanSLA_local, shape = TTtreat, alpha = TTtreat, group = TTtreat)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), size = 0.75, colour = "#972D15") +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line", size = 0.75, colour = "#972D15") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(. ~ temp) +
  scale_alpha_manual(legend.title.climate, values = c(0.5, 1), labels = c("Control", "Removal")) +
  scale_shape_manual(legend.title.climate, values = c(1, 16), labels = c("Control", "Removal")) +
  theme_classic() +
  labs(y = paste("Δ SLA")) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        axis.text.x  = element_text(angle = 90))  +
  axis.dimLarge


delta <- plot_grid(cover_temp.orig, sla_temp.orig, labels = c('A', 'B'), ncol = 2, align = 'h')
ggsave(filename = paste0("fig2A_coverSLA_v2.jpg"), width = 11, height = 4.5, dpi = 300, path = "/Users/fja062/Documents/seedclimComm/figures")



my.GR.data %>%
  mutate(mossHeight = if_else(Year == 2016, mossHeight/10, mossHeight)) %>% 
  ggplot(aes(x = Year, y = mossHeight, colour = TTtreat, shape = TTtreat)) +
  stat_summary(fun.data = "mean_cl_boot") +
  stat_summary(fun.data = "mean_cl_boot", geom = "line") +
  scale_colour_manual(legend.title.weat, values = cbPalette, labels = c("Control", "Removal")) +
  scale_shape_manual(legend.title.weat, values = c(1, 16), labels = c("Control", "Removal")) +
  theme_classic() +
  facet_grid(.~precip) +
  axis.dimLarge +
  labs(x = "", y = "Moss depth (cm)") +
  theme(axis.text.x  = element_text(angle = 90)) +
  ggsave(filename = "moss_depth_precip.jpg", path = "/Users/fja062/Documents/seedclimComm/figures", height = 4, width = 7.5)


my.GR.data %>% 
  ggplot(aes(x = Year, y = totalBryophytes, colour = TTtreat, shape = TTtreat)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.5)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.5), geom = "line") +
  facet_grid(. ~ precip) +
  scale_colour_manual(legend.title.weat, values = cbPalette, labels = c("Control", "Removal")) +
  scale_shape_manual(legend.title.weat, values = c(1, 16), labels = c("Control", "Removal")) +
  axis.dimLarge +
  theme_classic() +
  theme(axis.text.x  = element_text(angle = 90)) +
  ggsave(filename = "moss_coverPRECIP.jpg", width = 7.5, height = 4, path = "/Users/fja062/Documents/seedclimComm/figures")


my.GR.data %>% 
  filter(Year == 2016) %>% 
  ggplot(aes(x = seedMass, colour = TTtreat)) +
  geom_density()
