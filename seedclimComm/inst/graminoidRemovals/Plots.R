#Package to make plots
library(ggplot2)


cbPalette <- c("#999999","#1C9099", "#A6BDDB", "#ECE2F0", "#56B4E9",
               "#E69F00", "#F0E442", "#D55E00", "#CC79A7")

precip.lab <-   scale_x_discrete("Precipitation [mm y-1]",
                labels = c("0.6"="600", "1.2"="1200","2"="2000","2.7"="2700"))
temp.lab <- scale_x_discrete("Temperature [C]",
            labels = c("0.6"="600", "1.2"="1200","2"="2000","2.7"="2700"))
axis.dim <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  axis.ticks = element_blank(),
                  legend.text = element_text(size=10),
                  legend.title = element_text(size=10, face="bold"))


# turn some variables into factors for plotting
cover.meta$temp <- as.factor(cover.meta$temp)
cover.meta$prec <- as.factor(cover.meta$prec)
cover.meta$Year <- as.factor(cover.meta$Year)

timedelta$temp <- as.factor(timedelta$temp)
timedelta$prec <- as.factor(timedelta$prec)
timedelta$Year <- as.factor(timedelta$Year)

rtcmeta$temp <- as.factor(rtcmeta$temp)
rtcmeta$prec <- as.factor(rtcmeta$prec)
rtcmeta$Year <- as.factor(rtcmeta$Year)


# 1. timedelta, aes(x = temp, y = richness:SLA)

plotrawtemp <- function(df, na.rm = TRUE, ...) {
  nm <- names(df)[8:15]
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
plotrawtemp(cover.meta)


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
  nm <- names(df)[19:27]
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
  nm <- names(df)[19:27]
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