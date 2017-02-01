#Package to make plots
library(ggplot2)
library(RColorBrewer)


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
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#1C9099", "#A6BDDB", "#ECE2F0")

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
  



## ---- cover ----
# how does cover vary with temp after treatment?
ggplot(rtcforbs, aes(x = Year, y = deltasumcover)) +
  theme_bw() +
  geom_boxplot() +
  scale_color_brewer(palette = "Paired") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(as.formula(. ~ temp)) +
  ggtitle("Change in cover over time across temperature gradient") +
  axis.dim +
  ggsave(filename = "cover_forbs_temp.tiff", height = 8, width = 11, units = 'in', dpi = 600)

# how does cover vary with precip after treatment?
ggplot(rtcforbs, aes(x = Year, y = deltasumcover)) +
  theme_bw() +
  geom_boxplot() +
  scale_color_brewer(palette = "Paired") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(as.formula(. ~ prec)) +
  ggtitle("Change in cover over time across precipitation gradient") +
  axis.dim +
  ggsave(filename = "cover_forbs_prec.tiff", height = 8, width = 11, units = 'in', dpi = 600)

# and how do the forbs respond in comparison to the graminoid cover?
ggplot(rtcforbs, aes(x = Year, y = deltabiomass)) +
  theme_bw() +
  geom_boxplot() +
  scale_colour_manual(values = cbPalette) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(as.formula(. ~ temp)) +
  ggtitle("Change in biomass over time across temperature gradient") +
  axis.dim +
  ggsave(filename = "biomass_forbs_temp.tiff", height = 8, width = 11, units = 'in', dpi = 600)

# and how do the forbs respond in comparison to the graminoid cover?
ggplot(rtcforbs, aes(x = Year, y = deltabiomass)) +
  theme_bw() +
  geom_boxplot() +
  scale_colour_manual(values = cbPalette) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(as.formula(. ~ prec)) +
  ggtitle("Change in biomass over time across precipitation gradient") +
  axis.dim +
  ggsave(filename = "biomass_forbs_prec.tiff", height = 8, width = 11, units = 'in', dpi = 600)

# the delta of forb:graminoid responses, but not sure how much sense this makes.
ggplot(rtcmeta, aes(x = Year, y = sumcover, colour = functionalgroup)) +
  theme_bw() +
  geom_boxplot() +
  scale_colour_manual(values = cbPalette) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(as.formula(. ~ prec)) 




# making a proxy for biomass

ggplot(rtcforbs, aes(x = Year, y = deltawmean_SLA_local)) +
  theme_bw() +
  geom_boxplot() +
  scale_colour_manual(values = cbPalette) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = lm, se = FALSE)
  #facet_grid(as.formula(. ~ prec)) 

ggplot(rtcmeta, aes(x = sumcover, y = wmean_SLA_local, colour = functionalgroup)) +
  theme_bw() +
  geom_point() +
  scale_colour_manual(values = cbPalette) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = lm, se = FALSE) +
  facet_grid(as.formula(temp ~ Year)) 









#5. rtcmeta, aes(x = precip/temp, y = deltarichness:deltaseedMass)

plotdeltatempt <- function(df, na.rm = TRUE, ...) {
  nm <- names(df)[22:28]
  for(i in seq_along(nm)) {
    print(
      ggplot(df, aes_string(x = "Year", y = nm[i], colour = "functionalgroup")) +
        geom_point() +
        geom_smooth(method = "lm") +
        ggtitle(paste(nm[i], "across temperature gradient")) +
        scale_colour_manual(values = palette) +
        facet_grid(as.formula(. ~ prec)) +
        theme_bw() +
        axis.dim + temp.lab
    )
  }
}
plotdeltatempt(rtcmeta)

#
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