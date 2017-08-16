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

axis.dim <- theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=15),
                  axis.ticks = element_blank(),
                  legend.text = element_text(size=12),
                  legend.title = element_text(size=14),
                  strip.text.x = element_text(size = 12),
                  strip.text.y = element_text(size = 15),
                  axis.text.x  = element_text(angle = 90))

#### FORBS ONLY ####
distributions <- my.GR.data %>%
  filter(TTtreat == "RTC")

forbcover <- xtabs(cover ~ paste(turfID, Year, sep = "_") + species, data = distributions)
forbcover <- as.data.frame(unclass(forbcover))
forbcover <- forbcover[,colSums(forbcover > 0) > 0] #remove empty spp

forbcom$funYear <- as.factor(paste(forbcom$functionalgroup, forbcom$Year, sep = "_"))



##### FUNCTIONS FOR PLOTTING #####
# per response variable
time.plots <- function(dat, response, explan, save) {
  p <- ggplot(dat, aes_string(x = "Year", y = response, colour = "TTtreat", group = "TTtreat")) +
    stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6)) +
    stat_summary(fun.data = "mean_cl_boot", position = position_dodge(width = 0.6), geom = "line") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_grid(as.formula(paste(".~ ", explan))) +
    scale_color_manual(values = cbPalette, labels = c("Control", "Treatment")) +
    theme_classic() +
    labs(y = paste("Δ", substr(response, 6, nchar(response))), colour = "") +
    axis.dim
  if(save == TRUE){ 
    ggsave(p, filename = paste0(response, explan, ".jpg", sep = ""), height = 3.5, width = 6.5, dpi = 300)
  }# defaults to save == FALSE, so you don't have to write that in every time
  return(p)
}


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
time.plots(timedelta, response = "deltadiversity", explan = "Temperature_level", save = FALSE)

time.plots(wholecom, response = "vegetationHeight", explan = "Temperature_level", save = FALSE)

time.plots(timedelta, response = "deltawmean_height_global", explan = "Temperature_level", save = FALSE)

#### Temperature and precipitation across grid ####

ggsave(ggplot(my.GR.data, aes(summer_temp, annPrecip)) +
  geom_point() +
  geom_density_2d(colour = "black") +
  axis.dim, filename = "temp_precip_gradient.jpg", height = 6, width = 6)

#### COVER ####
# if we dig deeper, and look at the cover of the functional groups, things start to look interesting.

time.plots(timedelta, response = "deltasumcover", explan = "Precipitation_level", save = FALSE)
time.plots(timedelta, response = "deltasumcover", explan = "Temperature_level", save = FALSE)
time.plots.facet(timedelta, response = "deltasumcover")

density.plots(wholecom, "sumcover", "Precipitation_level")

wholecom %>%
  filter(funYear %in% c("forb_2011", "forb_2016", "graminoid_2011")) %>%
  ggplot(aes(x = wmean_SLA_local, fill = funYear)) +
  theme_bw() +
  scale_color_manual(values = cbPalette) +
  geom_density(alpha = 0.5) +
  #geom_smooth(method = lm) +
  #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  facet_grid(as.formula(TTtreat ~ Temperature_level)) +
  axis.dim +
  ggsave(filename = "sla_distributions.pdf")


ggplot(wholecom, aes(wmean_SLA_local, fill = factor(Temperature_level))) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  axis.dim +
  facet_wrap( ~ functionalGroup) +
  ggsave(filename = "func_sla_distributions.pdf")

#### RICHNESS ####
#
time.plots(timedelta, response = "deltarichness", explan = "Temperature_level", save = TRUE)
time.plots(timedelta, response = "deltarichness", explan = "Precipitation_level", save = TRUE)
time.plots.facet(timedelta, response = "deltarichness")

#### EVENNESS ####
#
time.plots(timedelta, response = "deltaevenness", explan = "Temperature_level", save = TRUE)
time.plots(timedelta, response = "deltaevenness", explan = "Precipitation_level", save = TRUE)
time.plots.facet(timedelta, response = "deltaevenness")

#### DIVERSITY ####
#
time.plots(timedelta, response = "deltadiversity", explan = "Temperature_level")
time.plots(timedelta, response = "deltadiversity", explan = "Precipitation_level")
time.plots.facet(timedelta, response = "deltadiversity")

#### HEIGHT ####
# we'd expect the forb community at the warmest sites to increase in height after graminoid removal, but we do not see this.

time.plots(timedelta, response = "deltawmean_height_global", explan = "Temperature_level", save = TRUE)
time.plots(timedelta, response = "deltawmean_height_global", explan = "Precipitation_level", save = TRUE)
time.plots.facet(timedelta, response = "deltawmean_height_global")

#### CN ####
# 
time.plots(timedelta, response = "deltawmean_CN_global", explan = "Temperature_level", save = TRUE)
time.plots(timedelta, response = "deltawmean_CN_global", explan = "Precipitation_level", save = TRUE)
time.plots.facet(timedelta, response = "deltawmean_CN_local")

#### SLA ####
# lower SLA in 
time.plots(timedelta, response = "deltawmean_SLA_local", explan = "Temperature_level", save = FALSE)
time.plots(timedelta, response = "deltawmean_SLA_global", explan = "Precipitation_level", save = FALSE)
time.plots.facet(timedelta, response = "deltawmean_SLA_local")

#### SEEDMASS ####
#
time.plots(timedelta, response = "deltawmean_seedMass", explan = "Temperature_level", save = TRUE)
time.plots(timedelta, response = "deltawmean_seedMass", explan = "Precipitation_level", save = TRUE)
time.plots.facet(timedelta, response = "deltawmean_seedMass")

#### LDMC ####
#
time.plots(timedelta, response = "deltawmean_LDMC_global", explan = "Temperature_level", save = TRUE)
time.plots(timedelta, response = "deltawmean_LDMC_global", explan = "Precipitation_level", save = TRUE)
time.plots.facet(timedelta, response = "deltawmean_LDMC_global")

#### LA ####
#
time.plots(timedelta, response = "deltawmean_LA_global", explan = "Temperature_level", save = TRUE)
time.plots(timedelta, response = "deltawmean_LA_global", explan = "Precipitation_level", save = TRUE)
time.plots.facet(timedelta, response = "deltawmean_LA_local")


#######################################
########## other plots ################
# whole community: diversity

div <- wholecom %>%
  filter(funYear %in% c("forb_2011", "forb_2016", "graminoid_2011"))
  #filter(!(functionalgroup == "woody" & Temperature_level == 10.5))


ggplot(div, aes(x = wmean_SLA_global, fill = funYear)) +
  theme_bw() +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = cbPalette) +
  #geom_smooth(method = lm) +
  #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  facet_grid(as.formula(. ~ Temperature_level)) 

ggplot(my.GR.data, aes(x = as.numeric(Year), y = summer_temp, colour = as.factor(Temperature_level))) + geom_point() + geom_smooth(method = "lm", se= FALSE)

ggplot(my.GR.data, aes(x = as.numeric(Year), y = annPrecip, colour = as.factor(Precipitation_level))) + geom_point() + geom_smooth(method = "lm", se= FALSE)


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
save_plot("plots1.tiff", varplots,
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



#6. rtcmeta, aes(x = precip/temp, y = deltarichness:deltaseedMass)

plotdeltatprec <- function(df, na.rm = TRUE, ...) {
  nm <- names(df)[22:28]
  for(i in seq_along(nm)) {
    print(
      ggplot(df, aes_string(x = "prec", y = nm[i])) +
        geom_boxplot() +
        ggtitle(paste(nm[i], "across precipitation gradient")) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_colour_manual(values = cbPalette) +
        facet_grid(as.formula(.~ Year)) +
        theme_bw() +
        axis.dim + precip.lab
    )
  }
}
plotdeltatprec(rtcmeta)


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

############### SEEDLINGS ###############

#recruitment.data$temp.prec.comb<-paste(recruitment.data$Temperature_level, recruitment.data$Precipitation_level)

#recruitment.data.RTC<-recruitment.data[recruitment.data$TTtreat=="RTC",]
#recruitment.data.TTC<-recruitment.data[recruitment.data$TTtreat=="TTC",]
#These two should have the same dimensions
#...and the order of the plots should be the same
#recruitment.data.RTC$blockID 
#recruitment.data.TTC$blockID
#recruitment.data.TTC<-recruitment.data.TTC[order(recruitment.data.TTC$blockID),] #correcting the order 

#Calculating the difference in recruitment between RTCs and TTCs
#diff.recruitment<-cbind(recruitment.data.RTC,recruitment.data.TTC$seedlings)
#colnames(diff.recruitment)[12]<-"seedlings.RTC"
#colnames(diff.recruitment)[16]<-"seedlings.TTC"
#diff.recruitment$diff.seedling<-diff.recruitment$seedlings.RTC-diff.recruitment$seedlings.TTC

#lineplot.CI(temp.prec.comb, diff.seedling, group=TTtreat, err.width=0.25, lty=0, cex=2, cex.leg=1.5, xlab="", ylab="?? No. seedlings (removal-control)", legend=F, cex.lab=1.5,xaxt="n",data=diff.recruitment) #replace "???" with the lambda symbol in case it hasn't been saved properly  
#axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10,11,12), labels=F)
#abline(h=0,lty=2)
#abline(v=4.5, lty=2)
#abline(v=8.5, lty=2)
#mtext(at=2.5,"Alpine (dry to wet)", side=1, line=2.5, cex=1.5)
#mtext(at=6.5,"Intermediate (dry to wet)", side=1, line=2.5, cex=1.5)
#mtext(at=10.5,"Lowland (dry to wet)", side=1, line=2.5, cex=1.5)
