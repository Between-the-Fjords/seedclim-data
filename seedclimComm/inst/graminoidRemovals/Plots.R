#Package to make plots
library(sciplot)
library(ggplot2)

p.grey <- "#999999"
p.orange <- "#E69F00"
p.blue <- "#56B4E9"
p.green <- "#009E73"
p.yellow <- "#F0E442"
p.darkblue <- "#0072B2"
p.red <- "#D55E00"
p.pink <- "#CC79A7"

cbPalette <- c("#999999","#1C9099", "#A6BDDB", "#ECE2F0", "#56B4E9",
               "#E69F00", "#F0E442", "#D55E00", "#CC79A7")
weather <- c("prec","temp")

precip.lab <-   scale_x_discrete("Precipitation [mm y-1]",
                labels = c("0.6"="600", "1.2"="1200","2"="2000","2.7"="2700"))
temp.lab <- scale_x_discrete("Temperature [C]",
            labels = c("0.6"="600", "1.2"="1200","2"="2000","2.7"="2700"))
axis.dim <- theme(axis.text=element_text(size=8)) +
  theme(axis.title=element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(legend.title = element_text(size=10, face="bold"))

var_list <- combn(names(timedelta)[8:15], 8, simplify = FALSE)

# Make plots.
# Save plots to tiff. Makes a separate file for each plot.
for (i in 1:3) {
  file_name = paste("iris_plot_", i, ".tiff", sep="")
  tiff(file_name)
  print(plot_list[[i]])
  dev.off()
}

############### SPECIES RICHNESS ###############
rtcmeta$temp.prec.comb<-paste(rtcmeta$Temperature_level, rtcmeta$Precipitation_level)
i <-ggplot()


site <- unique(timedelta$siteID)
for (i in 1:7){
  Site.i <- site[i]
  site.name <- timedelta[timedelta$siteID == Site.i, ]
  Yourfilename <- paste(Site.i, ".jpg", sep = "")
  jpeg(file = Yourfilename,  width = 15, height = 8, units = "cm", pointsize = 9,res=300)
  plot(y = site.name$deltadiversity, x = site.name$Precipitation_level)
  dev.off()
}  

# drivers of delta richness
#lineplot.CI(temp, deltaSLA, group=Year, lty=0, err.width=0.25, cex=2, legend=F, cex.leg=1.5, xlab="", cex.lab=1.5,xaxt="n",data=rtcmeta, ylab="?? Species richness (removal-control)", cex.lab=1.5) #kun 2013-data 
#abline(h=0,lty=2)
#abline(v=4.5, lty=2)
#abline(v=8.5, lty=2)
#mtext(at=2.5,"Alpine (dry to wet)", side=1, line=2.5, cex=1.5)
#mtext(at=6.5,"Intermediate (dry to wet)", side=1, line=2.5, cex=1.5)
#mtext(at=10.5,"Lowland (dry to wet)", side=1, line=2.5, cex=1.5)


############ DIVERSITY #################
###### raw ######
jpeg(filename = "diversity_raw.jpg",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(timedelta, aes(x = factor(prec), y= diversity)) +
  geom_boxplot(aes(colour = TTtreat)) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + precip.lab

dev.off()

###### delta time ######
jpeg(filename = "diversity_timedelta.jpg",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(timedelta, aes(x = factor(Year), y= deltadiversity)) +
  geom_boxplot(aes(colour = TTtreat)) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  #facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + temp.lab

dev.off()

###### delta treatment ######

jpeg(filename = "diversity_timedelta_treatmentdelta.jpg",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(rtcmeta, aes(x = factor(prec), y= deltadiversity)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + precip.lab

dev.off()

############ RICHNESS #################
###### raw ######
jpeg(filename = "richness_raw.jpg",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(timedelta, aes(x = factor(temp), y = richness)) +
  geom_boxplot(aes(colour = TTtreat)) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + temp.lab

dev.off()

###### delta time ######
jpeg(filename = "richness_timedelta.jpg",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(timedelta, aes(x = factor(temp), y= deltarichness)) +
  geom_boxplot(aes(colour = TTtreat)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  #facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + temp.lab

dev.off()
###### delta treatment ######

jpeg(filename = "richness_timedelta_treatmentdelta.jpg",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(rtcmeta, aes(x = factor(temp), y= deltarichness)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + temp.lab
dev.off()

############ EVENNESS #################
###### raw ######
jpeg(filename = "evenness_raw.jpg",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(timedelta, aes(x = factor(temp), y = evenness)) +
  geom_boxplot(aes(colour = TTtreat)) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + temp.lab

dev.off()

###### delta time ######
jpeg(filename = "evenness_timedelta.jpg",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(timedelta, aes(x = factor(temp), y= deltaevenness)) +
  geom_boxplot(aes(colour = TTtreat)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  #facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + temp

dev.off()
###### delta treatment ######

jpeg(filename = "evenness_timedelta_treatmentdelta.jpg",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(rtcmeta, aes(x = factor(temp), y= deltaevenness)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + temp

dev.off()

############ SLA #################
###### raw ######
jpeg(filename = "SLA_raw.jpg",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(timedelta, aes(x = factor(temp), y = SLA)) +
  geom_boxplot(aes(colour = TTtreat)) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + temp.lab

dev.off()

###### delta time ######
jpeg(filename = "SLA_timedelta.jpg",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(timedelta, aes(x = factor(temp), y= deltaSLA)) +
  geom_boxplot(aes(colour = TTtreat)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  #facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + temp.lab

dev.off()
###### delta treatment ######

jpeg(filename = "SLA_timedelta_treatmentdelta.jpg",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(rtcmeta, aes(x = factor(temp), y= deltaSLA)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + temp.lab

dev.off()

##################### COVER #######################
jpeg(filename = "Desktop/deltacoverprec",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(timedelta, aes(x=factor(prec), y=deltaSLA))+
  scale_colour_manual(values=cbPalette) +
  geom_boxplot(aes (colour = factor(Year))) +
  geom_hline(yintercept = 0) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  ylab(parse(text = "Delta_cover")) +
  axis.dim + temp.lab

dev.off()

############ COVER #################
###### raw ######
jpeg(filename = "SLA_raw.jpg",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(timedelta, aes(x = factor(temp), y = sumcover)) +
  geom_boxplot(aes(colour = TTtreat)) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + temp.lab

dev.off()

###### delta time ######
jpeg(filename = "SLA_timedelta.jpg",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(timedelta, aes(x = factor(prec), y= deltasumcover)) +
  geom_boxplot(aes(colour = TTtreat)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  #facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + precip.lab

dev.off()
###### delta treatment ######

jpeg(filename = "SLA_timedelta_treatmentdelta.jpg",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(rtcmeta, aes(x = factor(temp), y= deltasumcover)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + temp.lab

dev.off()


##################### HEIGHT #######################

z<-ggplot(cover.meta[cover.meta$TTtreat == "RTC",], aes(x = fprec, y= Maxheight)) +
  geom_boxplot(aes(colour = fyear)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

y<-ggplot(cover.meta[cover.meta$TTtreat == "TTC",], aes(x = fprec, y= Maxheight)) +
  geom_boxplot(aes(colour = fyear)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

multiplot(z,y, cols = 1)

############ HEIGHT #################
###### raw ######
jpeg(filename = "SLA_raw.jpg",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(timedelta, aes(x = factor(temp), y = Maxheight)) +
  geom_boxplot(aes(colour = TTtreat)) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + temp.lab

dev.off()

###### delta time ######
jpeg(filename = "SLA_timedelta.jpg",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(timedelta, aes(x = factor(temp), y= leafSize)) +
  geom_boxplot(aes(colour = TTtreat)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + temp.lab

dev.off()
###### delta treatment ######

jpeg(filename = "SLA_timedelta_treatmentdelta.jpg",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(rtcmeta, aes(x = factor(temp), y= seedMass)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + temp.lab

dev.off()


##################### GROWTH #######################


jpeg(filename = "growth_raw.jpg",  width = 15, height = 8, units = "cm", pointsize = 9,res=300)

ggplot(timedelta, aes(x = factor(temp), y= sumcover)) +
  geom_boxplot(aes(colour = TTtreat)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values=cbPalette) +
  facet_grid(.~ Year) +
  #ylab(parse(text = "Delta_growth")) +
  axis.dim + precip.lab

##################### SEEDMASS #######################
z<-ggplot(cover.meta[cover.meta$TTtreat == "RTC",], aes(x = ftemp, y= sumcover)) +
  geom_boxplot(aes(colour = fyear)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

y<-ggplot(cover.meta[cover.meta$TTtreat == "TTC",], aes(x = ftemp, y= sumcover)) +
  geom_boxplot(aes(colour = fyear)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

multiplot(z,y, cols = 1)


##################### SEEDMASS #######################

z<-ggplot(cover.meta[cover.meta$TTtreat == "RTC",], aes(x = ftemp, y= Seedmass)) +
  geom_boxplot(aes(colour = fyear)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

y<-ggplot(cover.meta[cover.meta$TTtreat == "TTC",], aes(x = ftemp, y= Seedmass)) +
  geom_boxplot(aes(colour = fyear)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

multiplot(z,y, cols = 1)

#If you want to make nice high-resolution images for publishing, here's a nice piece of code that does that: 

setwd("C:/Users/siri.lie.olsen/Documents/Artikler - alle egne") #specify where you want the plot to be saved
#Then choose your prefered file type (tiff, jpeg etc.) and specify size of the plot (trial and error...) and the resolution (most journals want 300 dpi)
tiff(filename = "Sp.richness.tiff",  width = 13, height = 10, units = "cm", pointsize = 12,res=300) 

#Add the code drawing the plot here (you may need to make some adjustments compared to when you plot it in R)

#...and finish with
dev.off()



############### SEEDLINGS ###############

recruitment.data$temp.prec.comb<-paste(recruitment.data$Temperature_level, recruitment.data$Precipitation_level)

recruitment.data.RTC<-recruitment.data[recruitment.data$TTtreat=="RTC",]
recruitment.data.TTC<-recruitment.data[recruitment.data$TTtreat=="TTC",]
#These two should have the same dimensions
#...and the order of the plots should be the same
recruitment.data.RTC$blockID 
recruitment.data.TTC$blockID
recruitment.data.TTC<-recruitment.data.TTC[order(recruitment.data.TTC$blockID),] #correcting the order 

#Calculating the difference in recruitment between RTCs and TTCs
diff.recruitment<-cbind(recruitment.data.RTC,recruitment.data.TTC$seedlings)
colnames(diff.recruitment)[12]<-"seedlings.RTC"
colnames(diff.recruitment)[16]<-"seedlings.TTC"
diff.recruitment$diff.seedling<-diff.recruitment$seedlings.RTC-diff.recruitment$seedlings.TTC

lineplot.CI(temp.prec.comb, diff.seedling, group=TTtreat, err.width=0.25, lty=0, cex=2, cex.leg=1.5, xlab="", ylab="?? No. seedlings (removal-control)", legend=F, cex.lab=1.5,xaxt="n",data=diff.recruitment) #replace "???" with the lambda symbol in case it hasn't been saved properly  
axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10,11,12), labels=F)
abline(h=0,lty=2)
abline(v=4.5, lty=2)
abline(v=8.5, lty=2)
mtext(at=2.5,"Alpine (dry to wet)", side=1, line=2.5, cex=1.5)
mtext(at=6.5,"Intermediate (dry to wet)", side=1, line=2.5, cex=1.5)
mtext(at=10.5,"Lowland (dry to wet)", side=1, line=2.5, cex=1.5)
