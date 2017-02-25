#Package to make plots
library(ggplot2)
library(RColorBrewer)
library(cowplot)

source("/Users/fja062/Documents/seedclimComm/seedclimComm/inst/graminoidRemovals/multiplot_function.R")


yrs <- rtcmeta %>%
  filter(funYear %in% c("forb_2011", "forb_2016", "graminoid_2011"))

rtcforbs <- rtcmeta %>%
  filter(functionalgroup == "forb")
  
diversity <- diversity %>%
  filter(year %in% c("2011", "2016"))

special <- special %>%
  filter(Year %in% c(2011, 2016)) %>%
  filter(specialism %in% c("alpine", "generalist")) %>%
  filter(temp == 6.5)

#palettes and labelling
cbPalette <- c("#009E73", "#999999", "#E69F00", "#56B4E9", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#1C9099", "#A6BDDB", "#ECE2F0", "orange3")

precip.lab <-   scale_x_discrete("Precipitation [mm y-1]",
                                 labels = c("0.6"="600", "1.2"="1200","2"="2000","2.7"="2700"))
temp.lab <- scale_x_discrete("Temperature [C]",
                             labels = c("0.6"="600", "1.2"="1200","2"="2000","2.7"="2700"))
axis.dim <- theme(axis.text=element_text(size=14),
                  axis.title=element_text(size=16),
                  axis.ticks = element_blank(),
                  legend.text = element_text(size=14),
                  legend.title = element_text(size=16, face="bold")) +
  theme(strip.text.x = element_text(size = 14))


ggplot(my.GR.data, aes(x = as.numeric(Year), y = summer_temp, colour = as.factor(Temperature_level))) + geom_point() + geom_smooth(method = "lm", se= FALSE)

ggplot(my.GR.data, aes(x = as.numeric(Year), y = annPrecip, colour = as.factor(Precipitation_level))) + geom_point() + geom_smooth(method = "lm", se= FALSE)


plots <- list()
nm <- c("Alrust", "Arhelleren", "Fauske", "Gudmedalen",
      "Hogsete", "Lavisdalen", "Ovstedal", "Rambera",
      "Skjellingahaugen", "Ulvhaugen", "Veskre", "Vikesland" )

for(i in seq_along(nm)) {
    p1 <- ggplot(my.GR.data, aes(x = (subTurf - 1) %/% 5, y = (subTurf - 1) %% 5, fill = cover)) +
      geom_tile(colour = "grey60") +
      facet_grid(species ~ Year) +
      ggtitle(paste("Plot", turfID, "at", siteID)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_distiller(type = "seq", palette = "Greens", direction = 1) + 
      theme_bw() +
      theme(
        axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(),
        strip.text.y = element_text(angle = 0)
      )
    plots[[i]] <- p1
}


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


# explanatory variables along gridded temperature gradients
plots <- list() # clear old list
nm <- names(rtcforbs)[45:50]

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
save_plot("plots2.tiff", varplots,
          ncol = 2,
          nrow = 3,
          base_aspect_ratio = 1.3)

# explanatory variables along gridded temperature gradients
plots <- list() # clear old list
nm <- names(rtcforbs)[51:53]

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
save_plot("plots3.tiff", varplots,
          ncol = 2,
          nrow = 2,
          base_aspect_ratio = 1.3)





# explanatory variables along gridded precipitation gradients
plots <- list()
nm <- names(rtcforbs)[39:44]

for(i in seq_along(nm)) {
  p1 <- ggplot(rtcforbs, aes_string(x = "annPrecip", y = nm[i], colour = "Year")) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle(paste(nm[i], "along annPrecip gradient")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = cbPalette) +
    theme_bw() +
    axis.dim
  plots[[i]] <- p1 #add each plot to the empty list
}

varplots <- plot_grid(plotlist = plots, cols = 2)
save_plot("plots4.tiff", varplots,
          ncol = 2,
          nrow = 3,
          base_aspect_ratio = 1.3)


# explanatory variables along gridded annPrecip gradients
plots <- list() # clear old list
nm <- names(rtcforbs)[45:50]

for(i in seq_along(nm)) {
  p1 <- ggplot(rtcforbs, aes_string(x = "annPrecip", y = nm[i], colour = "Year")) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle(paste(nm[i], "along annPrecip gradient")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = cbPalette) +
    theme_bw() +
    axis.dim
  plots[[i]] <- p1 #add each plot to the empty list
}

varplots <- plot_grid(plotlist = plots, cols = 2)
save_plot("plots5.tiff", varplots,
          ncol = 2,
          nrow = 3,
          base_aspect_ratio = 1.3)

# explanatory variables along gridded annPrecip gradients
plots <- list() # clear old list
nm <- names(rtcforbs)[51:53]

for(i in seq_along(nm)) {
  p1 <- ggplot(rtcforbs, aes_string(x = "annPrecip", y = nm[i], colour = "Year")) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ggtitle(paste(nm[i], "along annPrecip gradient")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = cbPalette) +
    theme_bw() +
    axis.dim
  plots[[i]] <- p1 #add each plot to the empty list
}

varplots <- plot_grid(plotlist = plots, cols = 2)
save_plot("plots6.tiff", varplots,
          ncol = 2,
          nrow = 2,
          base_aspect_ratio = 1.3)




plottraits <- traits %>%
  inner_join(traitdata, by = "species") %>%
  select(-specialism, - functionalgroup, - siteID) %>%
  group_by(species) %>%
  summarise_all(funs(mean))


my.GR.data %>%
  filter(!is.na(SLA) & !is.na(SLAR)) %>%
  ggplot(aes(SLAR, (SLA*10))) +
  geom_point() +
  scale_color_manual(values = cbPalette) +
  #geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, fullrange = FALSE) +
  geom_abline(intercept = 0, slope = 1)
  ggsave("trait_scales_SLA_comp.jpg") 

my.GR.data %>%
  filter(!is.na(height) & !is.na(heightR)) %>%
  ggplot(aes(heightR, (height*1000))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  ggsave("trait_scales_height_comp.jpg") 

my.GR.data %>%
  filter(!is.na(leafSize) & !is.na(Leaf_areaR) & Leaf_areaR < 50) %>%
  ggplot(aes(Leaf_areaR, (leafSize))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  ggsave("trait_scales_LA_comp.jpg") 

#are functional groups driving this difference?
my.GR.data %>%
  filter(!is.na(height) & !is.na(heightR)) %>%
  ggplot(aes(heightR, (height*1000), colour = functionalgroup)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  ggsave("trait_scales_height_comp_funct.jpg") 

# how do the different trait scales compare?
wholecom %>%
  mutate(wmean_SLA = wmean_SLA*10) %>%
  gather(sla, value, wmean_SLA, wmean_SLA_global, wmean_SLA_local) %>%
  ggplot(aes(siteID, value, colour = sla)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(as.formula(.~ Year)) +
  ggsave("trait_scales_SLA_yrFacet.jpg") 

wholecom %>%
  mutate(wmean_height = wmean_height*1000) %>%
  gather(HEIGHT, value, wmean_height, wmean_height_global, wmean_height_local) %>%
  ggplot(aes(siteID, value, colour = HEIGHT)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90)) +
  ggsave("trait_scales_height.jpg") 

wholecom %>%
  gather(leafArea, value, wmean_leafSize, wmean_LA_global, wmean_LA_local) %>%
  ggplot(aes(siteID, value, colour = leafArea)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90)) +
  ggsave("trait_scales_LA.jpg") 

ggplot(weather, aes(x = Year, y = summer_temp)) +
  geom_point() +
  geom_smooth(method = "lm", se= FALSE) +
  ggsave("summer_temp_2009_2016.jpg")

# whole community: diversity
div <- wholecom %>%
  filter(funYear %in% c("forb_2011", "forb_2016", "pteridophyte_2011", "pteridophyte_2016", "graminoid_2011"))

ggplot(div, aes(x = wmean_SLA, colour = funYear)) +
  theme_bw() +
  geom_density() +
  scale_color_brewer(palette = "Paired") +
  #geom_smooth(method = lm) +
  #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  facet_grid(as.formula(. ~ temp)) 

divF <- rtcmeta %>%
  filter(functionalgroup != "graminoid")

ggplot(rtcforbs, aes(x = Year, y = deltawmean_SLA_local)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(as.formula(. ~ temp)) +
  theme_bw()

# no apparent trend for diversity

#### COVER ####
# if we dig deeper, and look at the cover of the functional groups, things start to look interesting.


#### HEIGHT ####
# we'd expect the forb community at the warmest sites to increase in height after graminoid removal, but we do not see this.
ggplot(rtcmeta, aes(x = Year, y = deltawmean_height_local, colour = functionalgroup)) +
  theme_bw() +
  geom_boxplot() +
  scale_color_brewer(palette = "Paired") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_smooth(method = lm) +
  #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  facet_grid(as.formula(. ~ temp)) 



ggplot(wholecom, aes(y = temp, x = precip)) +
  geom_raster(aes(z = cover)) +
  #geom_smooth(method = lm) +
  scale_color_brewer(palette = "Dark2") +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() 
  #geom_point() +
  #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  #facet_grid(as.formula(. ~ temp))


ggplot(wholecom, aes(x = Year, y = wmean_seedMass, colour = functionalgroup, linetype = TTtreat)) +
  geom_point() +
  geom_smooth(method = lm) +
  scale_color_brewer(palette = "Dark2") +
  theme_bw() +
  #geom_point() +
  #geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  facet_grid(as.formula(. ~ prec)) +
  geom_density2d()


ggplot(special, aes(x = deltawmean_height, y = deltasumcover, colour = specYear)) +
  geom_jitter() +
  scale_color_brewer(palette = "Paired") +
  geom_smooth(method = lm, se = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.2) +
  #facet_grid(as.formula(. ~ temp), scales = "free_x") +
  theme_bw()

# 1. timedelta, aes(x = temp, y = richness:SLA)

plotrawtemp <- function(df, na.rm = TRUE, ...) {
  nm <- names(df)[14:17]
  for(i in seq_along(nm)) {
    print(
      ggplot(df, aes_string(x = "temp", y = nm[i], colour = "TTtreat")) +
        geom_boxplot() +
        ggtitle(paste(nm[i], "across temperature gradient")) +
        scale_colour_manual(values = cbPalette) +
        facet_grid(as.formula(. ~ Year)) +
        theme_bw() +
        axis.dim + temp.lab
    )
  }
}
plotrawtemp(my.GR.data)


# 2. timedelta, aes(x = precip, y = richness:SLA)

plotrawprec <- function(df, na.rm = TRUE, ...) {
  nm <- names(df)[8:15]
  for(i in seq_along(nm)) {
    print(
      ggplot(df, aes_string(x = "prec", y = nm[i], colour = "TTtreat")) +
        geom_boxplot() +
        ggtitle(paste(nm[i], "across precipitation gradient")) +
        scale_colour_manual(values = cbPalette) +
        facet_grid(as.formula(.~ Year)) +
        theme_bw() +
        axis.dim + precip.lab
    )
  }
}
plotrawprec(cover.meta)


#3. timedelta, aes(x = Year, y = deltarichness:deltaseedMass)

plotdeltatemp <- function(df, na.rm = TRUE, ...) {
  nm <- names(df)[19:26]
  for(i in seq_along(nm)) {
    print(
      ggplot(df, aes_string(x = "temp", y = nm[i], colour = "TTtreat")) +
        geom_boxplot() +
        ggtitle(paste(nm[i], "across temperature gradient")) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_colour_manual(values = cbPalette) +
        facet_grid(as.formula(. ~ Year)) +
        theme_bw() +
        axis.dim + temp.lab
    )
  }
}
plotdeltatemp(timedelta)

#4. timedelta, aes(x = Year, y = deltarichness:deltaseedMass)

plotdeltaprec <- function(df, na.rm = TRUE, ...) {
  nm <- names(df)[19:26]
  for(i in seq_along(nm)) {
    print(
      ggplot(df, aes_string(x = "prec", y = nm[i], colour = "TTtreat")) +
        geom_boxplot() +
        ggtitle(paste(nm[i], "across precipitation gradient")) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_colour_manual(values = cbPalette) +
        facet_grid(as.formula(. ~ Year)) +
        theme_bw() +
        axis.dim + precip.lab
    )
  }
}
plotdeltaprec(timedelta)



#5. rtcmeta, aes(x = precip/temp, y = deltarichness:deltaseedMass)

plotdeltatempt <- function(df, na.rm = TRUE, ...) {
  nm <- names(df)[22:28]
  for(i in seq_along(nm)) {
    print(
      ggplot(df, aes_string(x = "temp", y = nm[i])) +
        geom_boxplot() +
        ggtitle(paste(nm[i], "across temperature gradient")) +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_colour_manual(values = cbPalette) +
        facet_grid(as.formula(.~ Year)) +
        theme_bw() +
        axis.dim + temp.lab
    )
  }
}
plotdeltatempt(rtcmeta)

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