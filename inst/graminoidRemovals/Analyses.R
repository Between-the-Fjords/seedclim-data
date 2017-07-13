#load packages

# site averages from 2009 to 2016 for temp and precip

library(lme4)
library(MuMIn)
library(GGally)
library(tibble)
library(ggplot2)
library(lmerTest)

resp.traits <- forbcom %>%
  filter(TTtreat == "RTC") %>%
  select(Year, annPrecip, summer_temp, sumcover, wmean_SLA_global, wmean_height_global, wmean_LDMC_global, richness, diversity) %>%
  ggpairs(resp.traits)

resp.traits.delta <- rtcmeta %>%
  filter(TTtreat == "RTC") %>%
  select(Year, annPrecip, summer_temp, deltasumcover, deltawmean_SLA_global, deltawmean_height_global, deltawmean_LDMC_global, deltarichness, deltadiversity) %>%
  ggpairs(resp.traits.delta)

#### Scaling explanatory variables ####

timedelta$annPrecip <- as.numeric(scale(timedelta$annPrecip))
timedelta$summer_temp <- as.numeric(scale(timedelta$summer_temp))
timedelta$Year <- as.numeric(scale(timedelta$Year))


#### Functions ####

# thanks to (find blog name) for vifmer function...
vif.mer <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)]
  }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}


# function to select and average mixed effects models when given a full model. Prints dredge table if print = TRUE. Outputs model estimates, standard errors, and confidence intervals.

model.average <- function(mod, percent.thresh, print) {
  
  mod.select <- dredge(mod, rank = "AICc")
  if(print == TRUE){
    print(mod.select)
    print(importance(mod.select))
  }
  avg.mod <- model.avg(mod.select, cumsum(weight) <= 0.95, fit = TRUE)
  summary.mod.avg <- data.frame(summary(avg.mod)$coefmat.full)
  summary.mod.avg <- summary.mod.avg %>%
    rownames_to_column(var = "explan.var") %>%
    select(explan.var, Estimate, St.error = Std..Error) %>%
    mutate(CI.upper = Estimate + (St.error * 1.96), CI.lower = Estimate - (St.error * 1.96))
  summary.mod.avg
  #write.csv(summary.mod.avg, file = paste0(mod, ".csv", sep = ","))

}

model.importance <- function(mod) {
  
  mod.select <- dredge(mod, extra = "adjR^2", rank = "AICc")

    i <- as.data.frame(importance(mod.select)) %>%
      rownames_to_column(var = "Explanatory_variables")
    return(i)
}

# function to plot the estimates of all variables returned by model averaging
plot.estimates <- function(output, title) {
  p <- ggplot(output, aes_string(x = "explan.var", y = "Estimate"))
  print(p + geom_boxplot() +
          geom_errorbar(aes_string(ymin = "CI.lower", ymax = "CI.upper")) +
          geom_hline(yintercept = 0, linetype = "dashed") +
          theme(axis.text = element_text(angle = 90)) +
          ggtitle(title)
  )
}

#function to extract model predictions
model.predict <- function(mod, print, plot) {
  
  if(print == TRUE){
    print()
  }
  if(plot == TRUE){
    print(ggplot())
  }
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

########## BIOMASS ##########

## ---- SumCover start ---- 
car::qqp(rtcmeta$deltasumcover, "norm")

#faceting by secondary explanatory variable
ggplot(rtcmeta, aes(x = as.factor(Temperature_level), y = deltasumcover, colour = Year)) + geom_boxplot()
ggplot(timedelta, aes(x = Year, y = deltasumcover, colour = TTtreat)) + geom_boxplot() + facet_grid(.~ as.factor(Temperature_level))

rtcmeta.sumcover <- filter(timedelta, deltasumcover != "NA")

model.dsc.ba <- lmer(deltasumcover ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = rtcmeta.sumcover)

model.dsc.wot <- lmer(deltasumcover ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = rtcmeta.sumcover)

model.dsc.wop <- lmer(deltasumcover ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = rtcmeta.sumcover)

anova(model.dsc.ba, model.dsc.wot)
anova(model.dsc.ba, model.dsc.wop)

summary(model.dsc.ba)
drop1(model.dsc.ba, test = "Chisq")
output.dsc <- model.average(model.dsc.ba, percent.thresh = 0.95, print = TRUE)

plot.estimates(output.dsc, title = "Sum of cover")

vif.mer(model.dsc.ba)

preds <- predict(avg.mod)

qqnorm(residuals(model.dsc.ba)); qqline(residuals(model.dsc.ba))
plot(model.dsc.ba)

## ---- SumCover end ---- 

############### Diversity analysis ###############

## ---- diversity start ---- 

# treatment delta
ggplot(rtcmeta, aes(x = summer_temp, y = deltadiversity, colour = Year)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(rtcmeta, aes(x = annPrecip, y = deltadiversity, colour = Year)) + geom_point() + geom_smooth(method = "lm", se = FALSE)


# remove nas
rtcmeta.diversity <- filter(timedelta, deltadiversity != "NA")
car::qqp(rtcmeta.diversity$deltadiversity, "norm")

model.div.tr <- lmer(deltadiversity ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = "na.fail", REML = FALSE, data = rtcmeta.diversity)

model.div.wot <- lmer(deltadiversity ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = "na.fail", REML = FALSE, data = rtcmeta.diversity)

model.div.wop <- lmer(deltadiversity ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = "na.fail", REML = FALSE, data = rtcmeta.diversity)

anova(model.div.tr, model.div.wot)
anova(model.div.tr, model.div.wop)

summary(model.div.tr)
output.div <- model.average(model.div.tr, 0.95, print = TRUE)
plot.estimates(output.div, title = "Diversity")
qqnorm(residuals(model.div.tr)); qqline(residuals(model.div.tr))
plot(model.div.tr)


## ---- diversity end ---- 


## ---- richness start ---- is interannual variability more important than treatment effect???
rtcmeta.richness <- filter(timedelta, deltarichness != "NA")

car::qqp(rtcmeta.richness$deltarichness, "norm")
poisson <- MASS::fitdistr(rtcmeta.richness$deltarichness, "Poisson")
car::qqp(rtcmeta.richness$deltarichness, "pois", poisson$estimate)

ggplot(rtcmeta, aes(x = summer_temp, y = deltarichness, colour = Year, linetype = TTtreat)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(rtcmeta, aes(x = annPrecip, y = richness, colour = Year, linetype = TTtreat)) + geom_point() + geom_smooth(method = "lm", se = FALSE)


model.rich.tr <- lmer(deltarichness ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year +  (1|siteID/blockID/turfID), REML = FALSE, na.action = "na.fail", data = rtcmeta.richness)

model.rich.wot <- lmer(deltarichness ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year +  (1|siteID/blockID/turfID), REML = FALSE, na.action = "na.fail", data = rtcmeta.richness)

model.rich.wop <- lmer(deltarichness ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip +  (1|siteID/blockID/turfID), REML = FALSE, na.action = "na.fail", data = rtcmeta.richness)

anova(model.rich.tr, model.rich.wot)
anova(model.rich.tr, model.rich.wop)

summary(model.rich.tr)
output.rich <- model.average(model.rich.tr, 0.95, print = TRUE)
plot.estimates(output.rich, title = "Richness")
qqnorm(residuals(model.div.tr)); qqline(residuals(model.div.tr))
plot(model.div.tr)

## ---- richness end ---- 


## ---- evenness start ---- 
poisson <- MASS::fitdistr(forbcomfilter$evenness, "Poisson")

ggplot(rtcmeta, aes(x = summer_temp, y = deltaevenness, colour = Year, linetype = TTtreat)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(rtcmeta, aes(x = annPrecip, y = richness, colour = Year, linetype = TTtreat)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

rtcmeta.evenness <- filter(timedelta, deltaevenness != "NA")
rtcmeta.evenness <- filter(timedelta, deltaevenness != "Inf") #removing infinite values from Ovs2RTC and Ovs3RTC

car::qqp(rtcmeta.evenness$deltaevenness, "norm")

model.eve.tr <- lmer(deltaevenness ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = rtcmeta.evenness)

model.eve.wot <- lmer(deltaevenness ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = rtcmeta.evenness)

model.eve.wop <- lmer(deltaevenness ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = rtcmeta.evenness)

anova(model.eve.tr, model.eve.wop)
anova(model.eve.tr, model.eve.wot)

summary(model.eve.tr)
output.eve <- model.average(model.eve.tr, 0.95, print = TRUE)
plot.estimates(output.eve, title = "Evenness")

#need to create the dataframe 'modave'...
ggplot(modave.eve, aes(x = explan.var, y = Estimate)) + geom_boxplot() + geom_errorbar(aes(ymin = CI.lower, ymax = CI.upper)) + geom_hline(yintercept = 0, linetype = "dashed") + theme(axis.text = element_text(angle = 90))

## ---- evenness end ---- 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############### Traits analysis ###############


## ---- SLA start ---- 
rtcmeta.sla <- filter(timedelta, deltawmean_SLA_global != "NA")
car::qqp(timedelta$deltawmean_SLA_global, "norm")

ggplot(rtcmeta, aes(x = as.factor(Temperature_level), y = deltawmean_SLA_global, colour = Year)) + geom_boxplot()
ggplot(rtcmeta, aes(x = Year, y = deltawmean_SLA_global)) + geom_boxplot() + facet_grid(Temperature_level ~ Precipitation_level) + geom_hline(yintercept = 0, linetype = "dashed")


model.sla.tr <- lmer(deltawmean_SLA_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = "na.fail", REML = FALSE, data = rtcmeta.sla)

model.sla.wot <- lmer(deltawmean_SLA_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = "na.fail", REML = FALSE, data = rtcmeta.sla)

model.sla.wop <- lmer(deltawmean_SLA_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = "na.fail", REML = FALSE, data = rtcmeta.sla)

anova(model.sla.tr, model.sla.wop)
anova(model.sla.tr, model.sla.wot)

summary(model.sla.tr)
vif.mer(model.sla.tr)
output.sla <- model.average(model.sla.tr, 0.95, print = TRUE)
plot.estimates(output.sla, title = "SLA")
qqnorm(residuals(model.sla.tr)); qqline(residuals(model.sla.tr))
plot(model.sla.tr)


ggplot(modave.sla, aes(x = explan.var, y = Estimate)) + geom_boxplot() + geom_errorbar(aes(ymin = CI.lower, ymax = CI.upper)) + geom_hline(yintercept = 0, linetype = "dashed") + theme(axis.text = element_text(angle = 90))

## ---- SLA end ---- 


## ---- LDMC start ---- 
qqp(rtcmeta$deltawmean_LDMC_global, "lnorm")
ggplot(rtcmeta, aes(x = Precipitation_level, y = deltawmean_LDMC_global, colour = Year)) + geom_boxplot() + facet_grid(. ~ Temperature_level)

rtcmeta.LDMC <- filter(timedelta, deltawmean_LDMC_global != "NA")

model.LDMC.ba <- lmer(deltawmean_LDMC_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = rtcmeta.LDMC)

model.LDMC.wot <- lmer(deltawmean_LDMC_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = rtcmeta.LDMC)

model.LDMC.wop <- lmer(deltawmean_LDMC_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = rtcmeta.LDMC)

anova(model.LDMC.ba, model.LDMC.wop)
anova(model.LDMC.ba, model.LDMC.wot)

summary(model.LDMC.ba)
output.ldmc <- model.average(model.LDMC.ba, 0.95, print = TRUE)
plot.estimates(output.ldmc, title = "LDMC")
qqnorm(residuals(model.LDMC.ba)); qqline(residuals(model.LDMC.ba))
plot(model.LDMC.ba)

## ---- LDMC end ---- 


## ---- HEIGHT start ---- 
rtcmeta.height <- filter(timedelta, deltawmean_height_global != "NA")

model.height.ba <- lmer(deltawmean_height_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = rtcmeta.height) #factor contrast error - come back to this

model.height.wot <- lmer(deltawmean_height_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = rtcmeta.height) #factor contrast error - come back to this

model.height.wop <- lmer(deltawmean_height_global ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = rtcmeta.height) #factor contrast error - come back to this

anova(model.height.ba, model.height.wop)
anova(model.height.ba, model.height.wot)

summary(model.height.ba)
output.height <- model.average(model.height.ba, 0.95, print = TRUE)
plot.estimates(output.height, title = "height")
qqnorm(residuals(model.height.ba)); qqline(residuals(model.height.ba))
plot(model.height.ba)


## ---- HEIGHT end ---- 


## ---- SEEDMASS start ---- 
car::qqp(timedelta$deltawmean_seedMass, "lnorm")
rtcmeta.seedmass <- filter(timedelta, deltawmean_seedMass != "NA")

model.seedmass.ba <- lmer(deltawmean_seedMass ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = rtcmeta.seedmass)

model.seedmass.wot <- lmer(deltawmean_seedMass ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + TTtreat:annPrecip:Year + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = rtcmeta.seedmass)

model.seedmass.wop <- lmer(deltawmean_seedMass ~ TTtreat + summer_temp + annPrecip + Year + TTtreat:summer_temp + TTtreat:Year + TTtreat:summer_temp:Year + summer_temp:Year + TTtreat:annPrecip + Year:annPrecip + (1|siteID/blockID/turfID), na.action = na.fail, REML = FALSE, data = rtcmeta.seedmass)

anova(model.seedmass.ba, model.seedmass.wop)
anova(model.seedmass.ba, model.seedmass.wot)

summary(model.seedmass)
output.sm <- model.average(model.seedmass, 0.95, print = TRUE)
plot.estimates(output.sm, title = "seed mass")
qqnorm(residuals(model.seedmass)); qqline(residuals(model.seedmass))
plot(model.seedmass)

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
# hogsete plot

