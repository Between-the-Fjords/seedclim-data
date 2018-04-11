#Package to make plots
library(ggplot2)
library(RColorBrewer)
library(cowplot)

source("/Users/fja062/Documents/seedclimComm/seedclimComm/inst/graminoidRemovals/multiplot_function.R")


#### palettes and labelling ####
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#1C9099", "#A6BDDB", "#ECE2F0", "orange3")

plot(1:length(cbPalette), col = cbPalette, pch = 16, cex = 5) #check colour and order

precip.lab <-   scale_x_discrete("Precipitation [mm y-1]",
                                 labels = c("0.6"="600", "1.2"="1200","2"="2000","2.7"="2700"))
temp.lab <- scale_x_discrete("Temperature [C]",
                             labels = c("0.6"="600", "1.2"="1200","2"="2000","2.7"="2700"))

axis.dim <- theme(axis.text=element_text(size=10),
                  axis.title=element_text(size=15),
                  axis.ticks = element_blank(),
                  legend.text = element_text(size=11),
                  legend.title = element_text(size=12),
                  strip.text.x = element_text(size = 11),
                  strip.text.y = element_text(size = 13),
                  axis.text.x  = element_text(angle = 90))

legend.title.prec <- "Treatment and \n precipitation"
legend.title.temp <- "Treatment \n and temperature"

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


#### COVER ####
# if we dig deeper, and look at the cover of the functional groups, things start to look interesting.

time.plots.temp(timedelta, response = "deltasumcover", save = TRUE, ylab = TRUE)
time.plots.facet(timedelta, response = "deltasumcover")


#### RICHNESS ####
#
time.plots.precip(timedelta, response = "deltarichness", save = TRUE, ylab = TRUE)
time.plots.facet(timedelta, response = "deltarichness")

#### EVENNESS ####
#
time.plots.temp(timedelta, response = "deltaevenness", save = TRUE, ylab = TRUE)
time.plots.facet(timedelta, response = "deltaevenness")


#### HEIGHT ####
# we'd expect the forb community at the warmest sites to increase in height after graminoid removal, but we do not see this.

time.plots.precip(timedelta, response = "deltawmeanheight_local", save = FALSE, ylab = TRUE)
time.plots.facet(timedelta, response = "deltawmean_height_local")

#### CN ####
# 
time.plots.precip(timedelta, response = "deltawmeanCN_local", save = FALSE, ylab = TRUE)
time.plots.facet(timedelta, response = "deltawmean_CN_local")

#### SLA ####
# lower SLA in 
time.plots.precip(timedelta, response = "deltawmeanSLA_local", save = FALSE, ylab = TRUE)
time.plots.facet(timedelta, response = "deltawmean_SLA_local")

#### SEEDMASS ####
#
time.plots.temp(timedelta, response = "deltawmean_seedMass", save = FALSE, ylab = TRUE)
time.plots.facet(timedelta, response = "deltawmean_seedMass")

#### LDMC ####
#
time.plots(timedelta, response = "deltawmeanLDMC_local", save = FALSE, ylab = TRUE)
time.plots.facet(timedelta, response = "deltawmean_LDMC_local")

#### LTH ####
#
time.plots.temp(timedelta, response = "deltawmeanLTH_local", save = FALSE, ylab = TRUE)
time.plots.precip(timedelta, response = "deltawmeanLTH_local", save = FALSE, ylab = TRUE)
time.plots.facet(timedelta, response = "deltawmean_LDMC_local")

#### LA ####
#
time.plots(timedelta, response = "deltawmeanLA_local", save = FALSE, ylab = TRUE)
time.plots.facet(timedelta, response = "deltawmean_LDMC_local")

#######################################
########## other plots ################
# whole community: diversity

div <- wholecom %>%
  filter(funYear %in% c("forb_2011", "forb_2016", "graminoid_2011"))
  #filter(!(functionalgroup == "woody" & Temperature_level == 10.5))


ggplot(div, aes(x = diversity, fill = funYear)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = cbPalette) +
  #geom_smooth(method = lm) +
  #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  facet_grid(as.formula(. ~ Temperature_level)) 



plots <- list()
nm <- c("Alrust", "Arhelleren", "Fauske", "Gudmedalen", "Hogsete", "Lavisdalen", "Ovstedal", "Rambera", "Skjellingahaugen", "Ulvhaugen", "Veskre", "Vikesland" )

# explanatory variables along gridded temperature gradients
plots <- list()
nm <- names(rtcforbs)[39:44]

for(i in seq_along(nm)) {
  p1 <- ggplot(rtcforbs, aes_string(x = "summer_temp", y = nm[i], colour = "Year")) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle(paste(nm[i], "along temp gradient")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = cbPalette) +
    theme_bw() +
    axis.dim
  plots[[i]] <- p1 #add each plot to the empty list
}

varplots <- plot_grid(plotlist = plots, cols = 2)
save_plot("plots1.jpg", varplots,
          ncol = 2,
          nrow = 3,
          base_aspect_ratio = 1.3)


ggplot(wholecom, aes(x = Year, y = wmean_seedMass, colour = functionalgroup, linetype = TTtreat)) +
  geom_point() +
  geom_smooth(method = lm) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  #geom_point() +
  #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  facet_grid(as.formula(. ~ prec)) +
  geom_density2d()



###########################
###### subplot maps #######
plots <- list()

subplot.maps <- function(data, site){
  f.dat <- filter(data, siteID == site)
  for(i in turfID) {
  p1 <- ggplot(f.dat, aes(x = (subTurf - 1) %/% 5, y = (subTurf - 1) %% 5, fill = cover)) +
      geom_tile(colour = "grey60") +
      facet_grid(as.formula(paste0(Year ~ species))) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      ggtitle(paste("Plot", f.dat$turfID, "at", f.dat$siteID)) +
      scale_fill_distiller(type = "seq", palette = "Greens", direction = 1) + 
      theme_bw() +
      theme(
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        strip.text.y = element_text(angle = 0)
      )
    
  cat("\n")
  plots[[1]] <- p1
  }
}



results <- subturf %>%
  filter(species != "graminoid") %>%
  group_by(siteID, turfID) %>%
  do(plot = ggplot(., aes(x = (subTurf - 1) %/% 5, y = (subTurf - 1) %% 5, fill = cover)) +
       geom_tile(colour = "grey60") +
       facet_grid(species ~ Year) +
       scale_x_continuous(expand = c(0, 0)) +
       scale_y_continuous(expand = c(0, 0)) +
       ggtitle(paste("Plot", .$turfID, "at", .$siteID)) +
       scale_fill_distiller(type = "seq", palette = "Greens", direction = 1) + 
       theme_bw() +
       theme(
         axis.text = element_blank(), 
         axis.title = element_blank(), 
         axis.ticks = element_blank(),
         strip.text.y = element_text(angle = 0)
       )
  )

pdf('all.pdf', height = 10, width = 7)
invisible(lapply(results$plot, print))
dev.off()


multiplot(plotlist = plots, cols = 3)  


my.GR.data %>%
  filter(TTtreat == "TTC") %>% 
  ggplot(aes(x = as.factor(Precipitation_level), y = mossHeight)) +
  stat_summary(fun.data = "mean_cl_boot") +
  stat_summary(fun.data = "mean_cl_boot", geom = "line") +
  theme_classic() +
  axis.dim +
  labs(x = "Annual rainfall (m)", y = "Moss depth (cm)") +
  ggsave(filename = "moss_depth_precip.jpg", path = "/Users/fja062/Documents/seedclimComm/figures")



my.GR.data %>% filter(TTtreat == "RTC") %>% ggplot(aes(x = Year, y = totalBryophytes, colour = as.factor(Temperature_level), group = as.factor(Temperature_level))) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6)) +
  stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line") +
  facet_grid(. ~ Precipitation_level) +
  scale_colour_manual(values = cbPalette)

######
# so far I've managed to create a tally of subplot frequency for all species, which reinstates all those species that are dropped because their cover is NA. But I either need to update the inital MySQL code, or find some way of implementing the below code to include site/plot/block info etc...
######

subturf.GR <- dbGetQuery(con, paste("SELECT sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, turfs.GRtreat, subTurfCommunity.subTurf, subTurfCommunity.Year, subTurfCommunity.species, sites.Temperature_level, sites.Precipitation_level
                                    FROM taxon INNER JOIN ((sites INNER JOIN ((blocks INNER JOIN plots ON blocks.blockID = plots.blockID) INNER JOIN turfs ON plots.plotID = turfs.originPlotID) ON sites.siteID = blocks.siteID) INNER JOIN subTurfCommunity ON turfs.turfID = subTurfCommunity.turfID) ON taxon.species = subTurfCommunity.species
                                    WHERE Not taxon.functionalGroup='graminoid'
                                    GROUP BY sites.siteID, blocks.blockID, plots.plotID, turfs.turfID, turfs.TTtreat, turfs.GRtreat, subTurfCommunity.subTurf, subTurfCommunity.Year, subTurfCommunity.species, sites.Temperature_level, sites.Precipitation_level
                                    HAVING subTurfCommunity.Year>2009 AND (turfs.TTtreat='ttc' OR (turfs.GRtreat)='rtc' OR (turfs.GRtreat)='ttc');"))


subturf.GR$TTtreat[subturf.GR$TTtreat == ""| is.na(subturf.GR$TTtreat)] <- subturf.GR$GRtreat[subturf.GR$TTtreat == ""| is.na(subturf.GR$TTtreat)] # merge the GRtreat and TTtreat into one column
subturf.GR$GRtreat <- NULL
subturf.GR$TTtreat <- factor(subturf.GR$TTtreat)
subturf.GR$Year <- factor(subturf.GR$Year)
subturf.GR <- subturf.GR[!(subturf.GR$blockID == "Gud5" & subturf.GR$Year == 2010), ]
subturf.GR$Year[subturf.GR$Year == 2010] <- 2011
subturf.GR$Year <- droplevels(subturf.GR$Year)
subturf.GR$turfID <- plyr::mapvalues(subturf.GR$turfID, from = "Ram4RTCx", to = "Ram4RTC")
subturf.GR$turfID <- plyr::mapvalues(subturf.GR$turfID, from = "Ram5RTCx", to = "Ram5RTC")

subturf.GR$ID <- as.factor(paste(subturf.GR$turfID, subturf.GR$Year, sep = "_"))
subturf.GR <- subturf.GR[!subturf.GR$blockID %in% remsites,] 

subturf.GR <- subturf.GR %>% count(species, ID)

my.GR.data <- as.data.frame(my.GR.data)
subturf <- full_join(subturf.GR, my.GR.data, by = c("species", "ID"))

subturf$problems[is.na(subturf$cover)] <- "no cover"
subturf$problems[is.na(subturf$subTurf)] <- "no subturf"
subturf$problems[is.na(subturf$problems)] <- "ok"
subturf$problems <- as.factor(subturf$problems)

###########################