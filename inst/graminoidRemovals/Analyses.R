#load packages

# site averages from 2009 to 2016 for temp and precip

library(lme4)
library(MuMIn)
library(GGally)
library(tibble)
library(ggplot2)
library(lmerTest)

forbcom %>%
  filter(TTtreat == "RTC") %>%
  select(Year, annPrecip, summer_temp, sumcover, wmean_SLA_global, wmean_height_global, wmean_LDMC_global, richness, diversity) %>%
  ggpairs()

resp.traits.delta <- rtcmeta %>%
  filter(TTtreat == "RTC") %>%
  select(Year, annPrecip, summer_temp, deltasumcover, deltawmean_SLA_global, deltawmean_height_global, deltawmean_LDMC_global, deltarichness, deltadiversity) %>%
  ggpairs(resp.traits.delta)

#### Scaling explanatory variables ####

forbcom$annPrecip <- as.numeric(scale(forbcom$annPrecip))
forbcom$summer_temp <- as.numeric(scale(forbcom$summer_temp))
forbcom$Year <- as.numeric(scale(forbcom$Year))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

########## COVER ##########

## ---- SumCover start ---- 
car::qqp(rtcmeta$deltasumcover, "norm")

#faceting by secondary explanatory variable
ggplot(rtcmeta, aes(x = as.factor(Temperature_level), y = deltasumcover, colour = Year)) + geom_boxplot()
ggplot(rtcmeta, aes(x = as.factor(Precipitation_level), y = deltasumcover, colour = Year)) + geom_boxplot()
ggplot(timedelta, aes(x = Year, y = deltasumcover, colour = TTtreat)) + geom_boxplot()

rtcmeta.sumcover <- filter(timedelta, deltasumcover != "NA")

model.dsc.ba <- lmer(sumcover ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = forbcom)

model.dsc.wp <- lmer(sumcover ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = forbcom)

model.dsc.wt <- lmer(sumcover ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = forbcom)

anova(model.dsc.ba, model.dsc.wt)
anova(model.dsc.ba, model.dsc.wp)

summary(model.dsc.ba)
qqnorm(residuals(model.dsc.wt)); qqline(residuals(model.dsc.wt))
plot(model.dsc.wt)

## ---- SumCover end ---- 

############### Diversity analysis ###############

## ---- diversity start ---- 

# treatment delta
ggplot(rtcmeta, aes(x = as.factor(Temperature_level), y = deltadiversity, colour = Year)) + geom_boxplot()
ggplot(rtcmeta, aes(x = as.factor(Precipitation_level), y = deltadiversity, colour = Year)) + geom_boxplot()


# remove nas
rtcmeta.diversity <- filter(timedelta, deltadiversity != "NA")
car::qqp(rtcmeta.diversity$deltadiversity, "norm")

model.div.tr <- lmer(diversity ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), REML = FALSE, data = wholecom)

model.div.wp <- lmer(diversity ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), REML = FALSE, data = wholecom)

model.div.wt <- lmer(diversity ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = forbcom)

anova(model.div.tr, model.div.wt)
anova(model.div.tr, model.div.wp, test = "Chisq")

summary(model.div.tr)
qqnorm(residuals(model.div.tr)); qqline(residuals(model.div.tr))
plot(model.div.tr)


## ---- diversity end ---- 


## ---- richness start ---- is interannual variability more important than treatment effect???
rtcmeta.richness <- filter(timedelta, deltarichness != "NA")

car::qqp(rtcmeta.richness$deltarichness, "norm")

ggplot(rtcmeta, aes(x = as.factor(Temperature_level), y = deltarichness, colour = Year)) + geom_boxplot()
ggplot(rtcmeta, aes(x = as.factor(Precipitation_level), y = deltarichness, colour = Year)) + geom_boxplot()


model.rich.tr <- lmer(richness ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip +  (1|siteID/blockID/turfID), REML = FALSE, na.action = "na.fail", data = forbcom)

model.rich.wp <- lmer(richness ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year +  (1|siteID/blockID/turfID), REML = FALSE, na.action = "na.fail", data = forbcom)

model.rich.wt <- lmer(richness ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip +  (1|siteID/blockID/turfID), REML = FALSE, na.action = "na.fail", data = forbcom)

anova(model.rich.tr, model.rich.wt)
anova(model.rich.tr, model.rich.wp)

summary(model.rich.tr)
qqnorm(residuals(model.div.tr)); qqline(residuals(model.div.tr))
plot(model.div.tr)

## ---- richness end ---- 


## ---- evenness start ---- 
poisson <- MASS::fitdistr(forbcomfilter$evenness, "Poisson")

ggplot(rtcmeta, aes(x = as.factor(Temperature_level), y = deltaevenness, colour = Year)) + geom_boxplot()
ggplot(rtcmeta, aes(x = as.factor(Precipitation_level), y = deltaevenness, colour = Year)) + geom_boxplot()

rtcmeta.evenness <- filter(timedelta, deltaevenness != "NA")
rtcmeta.evenness <- filter(timedelta, deltaevenness != "Inf") #removing infinite values from Ovs2RTC and Ovs3RTC

car::qqp(rtcmeta$deltaevenness, "norm")

model.eve.tr <- lmer(evenness ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), REML = FALSE, data = forbcom)

model.eve.wp <- lmer(evenness ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), REML = FALSE, data = forbcom)

model.eve.wt <- lmer(evenness ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), REML = FALSE, data = forbcom)

anova(model.eve.tr, model.eve.wt)
anova(model.eve.tr, model.eve.wp)

summary(model.eve.tr)
qqnorm(residuals(model.eve.tr)); qqline(residuals(model.eve.tr))
plot(model.eve.tr)

## ---- evenness end ---- 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############### Traits analysis ###############


## ---- SLA start ---- 
rtcmeta.sla <- filter(timedelta, deltawmean_SLA_local != "NA")
car::qqp(timedelta$deltawmean_SLA_global, "norm")

ggplot(rtcmeta, aes(x = as.factor(Temperature_level), y = deltawmean_SLA_global, colour = Year)) + geom_boxplot()
ggplot(rtcmeta, aes(x = as.factor(Precipitation_level), y = deltawmean_SLA_global, colour = Year)) + geom_boxplot()


model.sla.tr <- lmer(wmean_SLA_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = "na.fail", REML = FALSE, data = forbcom)

model.sla.wp <- lmer(wmean_SLA_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = "na.fail", REML = FALSE, data = forbcom)

model.sla.wt <- lmer(wmean_SLA_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = "na.fail", REML = FALSE, data = forbcom)

anova(model.sla.tr, model.sla.wt)
anova(model.sla.tr, model.sla.wp)

summary(model.sla.tr)
qqnorm(residuals(model.sla.tr)); qqline(residuals(model.sla.tr))
plot(model.sla.tr)


## ---- SLA end ---- 

## ---- CN start ---- 
rtcmeta.CN <- filter(timedelta, deltawmean_CN_local != "NA")
car::qqp(timedelta$deltawmean_CN_global, "norm")

ggplot(rtcmeta, aes(x = as.factor(Temperature_level), y = deltawmean_CN_global, colour = Year)) + geom_boxplot()
ggplot(rtcmeta, aes(x = as.factor(Precipitation_level), y = deltawmean_CN_global, colour = Year)) + geom_boxplot()


model.CN.tr <- lmer(wmean_CN_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = "na.fail", REML = FALSE, data = forbcom)

model.CN.wp <- lmer(wmean_CN_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = "na.fail", REML = FALSE, data = forbcom)

model.CN.wt <- lmer(wmean_CN_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = "na.fail", REML = FALSE, data = forbcom)

anova(model.CN.tr, model.CN.wt)
anova(model.CN.tr, model.CN.wp)

summary(model.CN.tr)
qqnorm(residuals(model.CN.tr)); qqline(residuals(model.CN.tr))
plot(model.CN.tr)


## ---- CN end ---- 

## ---- LDMC start ---- 
car::qqp(rtcmeta$deltawmean_LDMC_global, "lnorm")

ggplot(rtcmeta, aes(x = as.factor(Temperature_level), y = deltawmean_LDMC_global, colour = Year)) + geom_boxplot()
ggplot(rtcmeta, aes(x = as.factor(Precipitation_level), y = deltawmean_LDMC_global, colour = Year)) + geom_boxplot()

rtcmeta.LDMC <- filter(timedelta, deltawmean_LDMC_global != "NA")

model.LDMC.ba <- lmer(wmean_LDMC_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = forbcom)

model.LDMC.wp <- lmer(wmean_LDMC_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = forbcom)

model.LDMC.wt <- lmer(wmean_LDMC_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = forbcom)

anova(model.LDMC.ba, model.LDMC.wt)
anova(model.LDMC.ba, model.LDMC.wp)

summary(model.LDMC.ba)
qqnorm(residuals(model.LDMC.ba)); qqline(residuals(model.LDMC.ba))
plot(model.LDMC.ba)

## ---- LDMC end ---- 


## ---- HEIGHT start ---- 
rtcmeta.height <- filter(timedelta, deltawmean_height_global != "NA")

ggplot(rtcmeta, aes(x = as.factor(Temperature_level), y = deltawmean_height_global, colour = Year)) + geom_boxplot()
ggplot(rtcmeta, aes(x = as.factor(Precipitation_level), y = deltawmean_height_global, colour = Year)) + geom_boxplot()

model.height.ba <- lmer(wmean_height_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = forbcom) #factor contrast error - come back to this

model.height.wp <- lmer(wmean_height_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = forbcom) #factor contrast error - come back to this

model.height.wt <- lmer(wmean_height_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = forbcom) #factor contrast error - come back to this

anova(model.height.ba, model.height.wt)
anova(model.height.ba, model.height.wp)

summary(model.height.wt)
qqnorm(residuals(model.height.ba)); qqline(residuals(model.height.ba))
plot(model.height.ba)


## ---- HEIGHT end ---- 


## ---- SEEDMASS start ---- 
car::qqp(timedelta$deltawmean_seedMass, "lnorm")
ggplot(rtcmeta, aes(x = as.factor(Temperature_level), y = deltawmean_seedMass, colour = Year)) + geom_boxplot()
ggplot(rtcmeta, aes(x = as.factor(Precipitation_level), y = deltawmean_seedMass, colour = Year)) + geom_boxplot()

rtcmeta.seedmass <- filter(timedelta, deltawmean_seedMass != "NA")

model.seedmass.ba <- lmer(wmean_seedMass ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = forbcom)

model.seedmass.wp <- lmer(wmean_seedMass ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = forbcom)

model.seedmass.wt <- lmer(wmean_seedMass ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = forbcom)

anova(model.seedmass.ba, model.seedmass.wt)
anova(model.seedmass.ba, model.seedmass.wp)

summary(model.seedmass.ba)
qqnorm(residuals(model.seedmass.ba)); qqline(residuals(model.seedmass.ba))
plot(model.seedmass.ba)

## ---- SEEDMASS end ---- 


############### SEEDLINGS ###############

#Adding values for temperature and precipitation to the dataset (rather than just levels)


head(recruitment.data)

#Running the model
  #NB1: Poisson distribution for counts
  #NB2: No need to include year as a random factor or subtract data from different years, as seedling data was only collected in one year 

############### SPECIES COMPOSITION ###############

#I am honestly not sure if what I did, shown below, is the best way to do this. I don't know for sure if it takes the nested structure of the data into account in the right way.
#Another way of doing it would be to run an NMDS (due to the tongue effect in the DCA; see below) and then do mixed models on the resulting axis scores. 
#In the mixed models you can add proper random effects to fit the data structure 

#Anyway, this is what I did:
freqsubturf<-freqsubturf[,colSums(freqsubturf>0)>0]
names(freqsubturf)
#freq.forbs.ordination<-freqsubturf[,-c(43,56,75,130,136:137)] #remove some weird taxa before the ordination
#names(freq.forbs.ordination)

library(vegan) 

#Start with a DCA to determine axes lengths 
nmds.r<-metaMDS(freqsubturf) #you'll get a warning message; that's fine 
summary(nmds.r) #long axis = use CCA rather than RDA for testing 

# #Plot it all - if you want to
nmds1<-scores(nmds.r,display="sites",origin=FALSE)[,1]#extract axis scores
nmds2<-scores(nmds.r,display="sites",origin=FALSE)[,2]
plot(nmds1,nmds2,xlab="nmds1",ylab="nmds2",type="n",cex.axis=1.25,cex.lab=1.25, xlim=c(-2,2), ylim=c(-2,2)) 
text(nmds.r, display = "spec", cex=0.7, col="blue") #nasty tongue effect... 

nmdslmer<- lmer(nmds1 ~ nmds2 + (1|siteID/blockID/turfID), data = cover.meta)
nmds0 <- lmer(nmds1 ~ 1 + (1|siteID/blockID/turfID), data = cover.meta)


#CCA
mycca2 <- cca(freqsubturf ~ Height*Seedmass*SLA, data = cover.meta)
m02 <- cca(freqsubturf ~1, data = cover.meta)

add1(m02, scope=formula(mycca2), test = "perm")
m02 <- update(m02, .~. + Height:Seedmass)


#start with the full model
mycca <- cca(freqsubturf ~ TTtreat*Year*Temperature_level*Precipitation_level, data = cover.meta)
m0 <- cca(freqsubturf ~1, data = cover.meta)

add1(m0, scope=formula(mycca), test = "perm")
m0 <- update(m0, .~. + Year:Precipitation_level)

anova.cca(m0)

m0score1<-scores(m0,display="sites",origin=FALSE)[,1]#extract axis scores
m0score2<-scores(m0,display="sites",origin=FALSE)[,2]

plot(m0score1,m0score2,xlab="cca1",ylab="cca2",type="n",cex.axis=1.25,cex.lab=1.25, xlim=c(-2,2), ylim=c(-2,2)) 
text(m0, display = "spec", cex=0.7, col="blue")
# final model = m0<- cca(freqsubturf ~ Precipitation_level + Temperature_level + TTtreat + Year + Temperature_level:TTtreat + Temperature_level:Year + Precipitation_level:Temperature_level + Precipitation_level:TTtreat + Precipitation_level:Year + Precipitation_level:Temperature_level:TTtreat, data = cover.meta)

# Nice function for plotting 
mytext.cca<-function (x, display = "sites", labels, choices = c(1, 2), scaling = 2, 
                      arrow.mul, head.arrow = 0.1, select, const, ...) 
{
  formals(arrows) <- c(formals(arrows), alist(... = ))
  if (length(display) > 1) 
    stop("Only one 'display' item can be added in one command.")
  pts <- scores(x, choices = choices, display = display, scaling = scaling, 
                const)
  if (!missing(labels)) 
    rownames(pts) <- labels
  if (!missing(select)) 
    pts <- .checkSelect(select, pts)
  if (display == "cn") {
    cnam <- rownames(pts)
    text(pts, labels = cnam, ...)
    pts <- scores(x, choices = choices, display = "bp", scaling = scaling, 
                  const)
    bnam <- rownames(pts)
    pts <- pts[!(bnam %in% cnam), , drop = FALSE]
    if (nrow(pts) == 0) 
      return(invisible())
    else display <- "bp"
  }
  if (display == "bp") {
    if (missing(arrow.mul)) {
      arrow.mul <- ordiArrowMul(pts)
    }
    pts <- pts * arrow.mul
    arrows(0, 0, pts[, 1], pts[, 2], length = head.arrow, 
           ...)
    pts <- pts * 1.1
    
  }
  text(pts, labels = rownames(pts), ...)
  invisible()
}

plot(m0, dis="sp", type="n", xlim=c(-3,2), ylim=c(-2,4))
abundance <- colSums(freqsubturf)
orditorp(m0, "sp", cex=1, air=1, pch="", priority=abundance[abundance>0]) #"Priority" makes sure you don't plot all species at once... Adjust number to adjust number of species plotted
mytext.cca(m0, dis = "bp", arrow.mul = 3.4, adj=1, font=2, labels=c("Prec", "Rem","Year", "Temp","Prec:Year", "Rem:Temp", "Prec:Temp", "Rem:Prec","Year:Temp", "Rem:Temp:Prec"))

plot(m02, dis="sp", type="n", xlim=c(-3,2), ylim=c(-2,4))
abundance <- colSums(freqsubturf)
orditorp(m02, "sp", cex=1, air=1, pch="", priority=abundance[abundance>0]) #"Priority" makes sure you don't plot all species at once... Adjust number to adjust number of species plotted
mytext.cca(m02, dis = "bp", arrow.mul = 3.4, adj=1, font=2, labels=c("Height", "Seedmass", "SLA", "Height:SLA", "Height:Seedmass"))

###########################################################################

