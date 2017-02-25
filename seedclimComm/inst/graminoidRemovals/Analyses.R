#For the analyses I have chosen to work with the subplot frequency data rather than cover data, as the cover data has observer bias



######################################################################################
#function for QAICc. NB, phi is the scaling parameter from the quasi-family model. If using e.g. a poisson family, phi=1 and QAICc returns AICc, or AIC if QAICc=FALSE.
QAICc <- function(mod, scale, QAICc = TRUE) {
  ll <- as.numeric(logLik(mod))
  df <- attr(logLik(mod), "df")
  n <- length(resid(mod))
  if (QAICc)
    qaic = as.numeric(-2 * ll/scale + 2 * df + 2 * df * (df + 1)/(n - df - 1))
  else qaic = as.numeric(-2 * ll/scale + 2 * df)
  qaic
}


## code for model selection. First fit mod01, then run this code.
modsel <- function(mods,x){	
  phi=1
  dd <- data.frame(Model=1:length(mods), K=1, QAIC=1)
  for(j in 1:length(mods)){
    dd$K[j] = attr(logLik(mods[[j]]),"df")
    dd$QAIC[j] = QAICc(mods[[j]],phi)
  }
  dd$delta.i <- dd$QAIC - min(dd$QAIC)
  dd <- subset(dd,dd$delta.i<x)
  dd$re.lik <- round(exp(-0.5*dd$delta.i),3)
  sum.aic <- sum(exp(-0.5*dd$delta.i))
  wi <- numeric(0)
  for (i in 1:length(dd$Model)){wi[i] <- round(exp(-0.5*dd$delta.i[i])/sum.aic,3)}; dd$wi<-wi
  print(dds <- dd[order(dd$QAIC), ])
  assign("mstable",dd,envir=.GlobalEnv)
}

modsel(list(mod01,mod02,mod03,mod04,mod05,mod06,mod07,mod08,mod09,mod10),1000)


#And finally - analyses!

library(lme4)
library(psych)
library(car)
library(broom)
library(hypervolume)


my.GR.data.clean <- my.GR.data %>%
  select(-Temperature_level, -Precipitation_level, -plotID)


summary(lm(summer_precip ~ summer_temp, data = my.GR.data))
ggplot(rtcforbs, aes(y = summer_temp, x = annPrecip, colour = Year)) + geom_point() + geom_smooth(method = "lm", se = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############### Hypervolume analysis ###############

## ---- hypervolume start ---- 

# checking out what the deal is with the hypervolume package
forbs <- forbs[complete.cases(forbs[,20:24]),]
forbs <- forbs %>%
  filter(TTtreat == "RTC")

hv1 <- hypervolume(subset(forbs, Year == 2011 & temp == 6.5)[,14:17], bandwidth = estimate_bandwidth(forbs[,14:15]), name = '2011_alpine')
hv2 <- hypervolume(subset(forbs, Year == 2016 & temp == 6.5)[,14:17], bandwidth = estimate_bandwidth(forbs[,14:15]), name = '2016_alpine')
hv3 <- hypervolume(subset(forbs, Year == 2011 & temp == 10.5)[,14:17], bandwidth = estimate_bandwidth(forbs[,14:15]), name = '2011_lowland')
hv4 <- hypervolume(subset(forbs, Year == 2016 & temp == 10.5)[,14:17], bandwidth = estimate_bandwidth(forbs[,14:15]), name = '2016_lowland')

hv2 <- hypervolume(subset(forbs, specialism == "alpine")[,14:17], bandwidth = 0.25, name = 'alpines')
hv3 <- hypervolume(subset(forbs, specialism == "lowland")[,14:17], bandwidth = 0.25, name = 'lowlands')

hv_all <- hypervolume_join(hv1, hv2)
plot(hv_all)

species_list = as.character(unique(forbs$Year))
num_species = length(species_list)  
trait_axes <- c("SLA_mean","Height_mean","LDMC_mean","LA_mean")

# compute hypervolumes for each species  
hv_specialism_list = new("HypervolumeList")
hv_specialism_list@HVList = vector(mode="list",length=num_species)
for (i in 1:num_species)
  {
 # keep the trait data 
data_this_specialism = forbs[forbs$Year==species_list[i],trait_axes]
# log-transform to rescale
# data_this_species_log <- log10(data_this_species)

# make a hypervolume using auto-bandwidth
hv_specialism_list@HVList[[i]] <- hypervolume(data_this_specialism, bandwidth = estimate_bandwidth(data_this_specialism), name = as.character(species_list[i]), warn = FALSE)
}

# compute all pairwise overlaps
overlap = matrix(NA, nrow = num_species, ncol = num_species)
dimnames(overlap) = list(species_list, species_list)
for (i in 1:num_species)
  {
  for (j in i:num_species)
    {
    if (i!=j)
      {
      # compute set operations on each pair
      this_set = hypervolume_set(hv_specialism_list@HVList[[i]], hv_specialism_list@HVList[[j]], check_memory = FALSE)
      # calculate a Sorensen overlap index (2 x shared volume / sum of |hv1| + |hv2|)
      overlap[i,j] = hypervolume_sorensen_overlap(this_set)
    }
  }   
  }

# show all hypervolumes
plot(hv_specialism_list)

# show pairwise overlaps - note that actually very few species overlap in four dimensions
  +   op <- par(mar=c(10,10,1,1))
+   image(x=1:nrow(overlap), y=1:nrow(overlap), z=overlap,axes=F,xlab='',ylab='',col=rainbow(5))
+   box()
+   axis(side=1, at=1:(length(dimnames(overlap)[[1]])),dimnames(overlap)[[1]],las=2,cex.axis=0.75)
+   axis(side=2, at=1:(length(dimnames(overlap)[[2]])),dimnames(overlap)[[2]],las=1,cex.axis=0.75)
+   par(op)


## ---- hypervolume end ---- 

#correlations

pairs.panels(wholecom)

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############### Diversity analysis ###############

## ---- diversity start ---- 

# base
model.div.ba <- lmer(diversity ~ TTtreat*temp*scale(prec)*scale(Year) + (1|siteID/blockID/turfID), na.action=na.omit, REML=F, data = wholecom)
drop1(model.div.ba, test = "Chisq")
model.div.ba <- update(model.div.ba, .~. - TTtreat:temp)
summary(model.div.ba); Anova(model.div.ba) #this may not be a great method to get variable significances
qqnorm(residuals(model.div.ba)); qqline(residuals(model.div.ba))
plot(model.div.ba)
#final model: diversity ~ TTtreat + temp + prec + sYear + (1 | siteID/blockID/turfID) +  temp:prec + TTtreat:sYear + prec:sYear

# treatment delta
qqp(rtcmeta$deltadiversity, "norm")
model.div.tr<-lmer(deltadiversity ~ annPrecip*Year*summer_temp + (1|siteID/blockID), na.action=na.omit, REML = TRUE, data = rtcforbs)
vif.mer(model.div.tr)
#model.div.tr <- update(model.div.tr, .~. - temp)
vif.mer(model.div.tr)
drop1(model.div.tr, test = "Chisq")
summary(model.div.tr); Anova(model.div.tr) #this may not be a great method to get variable significances.
qqnorm(residuals(model.div.tr)); qqline(residuals(model.div.tr))
plot(model.div.tr)
df <- augment(model.div.tr)


ggplot(df, aes(x = temp, y = deltadiversity)) +
  geom_point() +
  geom_raster(aes(fill = .mu))

  geom_boxplot() +
  ggtitle(paste("Delta diversity across precipitation gradient")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_colour_manual(values = cbPalette) +
  facet_grid(as.formula(.~ Year)) +
  theme_bw() +
  axis.dim + precip.lab
#final model: diversity ~ prec + sYear + (1 | siteID/blockID/turfID) +  temp:prec + TTtreat:sYear + prec:sYear

# time delta
model.div.ti<-lmer(deltadiversity ~ TTtreat*temp*scale(prec)*scale(Year) + (1|siteID/blockID/turfID), na.action=na.omit, REML=F, data=timedelta)
drop1(model.div.ti, test = "Chisq")
model.div.ti <- update(model.div.ti, .~. - TTtreat:scale(Year))
summary(model.div.ti); Anova(model.div.ti) #this may not be a great method to get variable significances..
qqnorm(residuals(model.div.ti)); qqline(residuals(model.div.ti))
plot(model.div.ti)
#final model: diversity ~ TTtreat + temp + prec + sYear + (1 | siteID/blockID/turfID) +  temp:prec + TTtreat:sYear + prec:sYear

## ---- diversity end ---- 


## ---- richness start ---- is interannual variability more important than treatment effect???
forbcomfilter <- forbcom %>%
  filter(richness != "NA")

poisson <- fitdistr(forbcomfilter$richness, "Poisson")
qqp(forbcomfilter$richness, "pois", poisson$estimate)

ggplot(forbcom, aes(x = summer_temp, y = richness, colour = Year, linetype = TTtreat)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(forbcom, aes(x = annPrecip, y = richness, colour = Year, linetype = TTtreat)) + geom_point() + geom_smooth(method = "lm", se = FALSE)


model.rich.tr <- glmer(richness ~ summer_temp + annPrecip + Year + TTtreat + (1|siteID/blockID), na.action = na.omit, data = forbcom, family = "poisson")

# base
# treatment delta
model.rich.tr <- glmer(deltarichness ~ summer_temp*annPrecip*Year + (1|siteID/blockID), na.action = na.omit, data = rtcforbs, family = "poisson")
drop1(model.rich.tr, test="Chisq")
model.rich.tr <- update(model.rich.tr, .~. - summer_temp:annPrecip:Year)
drop1(model.rich.tr, test="Chisq")
model.rich.tr <- update(model.rich.tr, .~. - summer_temp:Year)
drop1(model.rich.tr, test="Chisq")
model.rich.tr <- update(model.rich.tr, .~. - summer_temp:annPrecip)
drop1(model.rich.tr, test="Chisq")
model.rich.tr <- update(model.rich.tr, .~. - summer_temp)
drop1(model.rich.tr, test="Chisq")
model.rich.tr <- update(model.rich.tr, .~. - annPrecip:Year)
drop1(model.rich.tr, test="Chisq")
model.rich.tr <- update(model.rich.tr, .~. - annPrecip)
drop1(model.rich.tr, test="Chisq")
#final model = deltarichness ~ summer_temp + annPrecip + Year + (1 | siteID/blockID) + annPrecip:Year
summary(model.rich.tr); Anova(model.rich.tr) #this may not be a great method to get variable significance
qqnorm(residuals(model.rich.tr)); qqline(residuals(model.rich.tr))
plot(model.rich.tr)

# time delta
model.rich.ti<-lmer(deltarichness~TTtreat*temp*scale(prec)*scale(Year) + (1|siteID/blockID/turfID), na.action=na.omit, REML=F, data = timeforbs)
drop1(model.rich.ti, test="Chisq")
model.rich.ti <- update(model.rich.ti, .~. - TTtreat:scale(Year))
#final model = richness ~ TTtreat + temp + prec + TTtreat:temp + (1 | Year) + (1 | siteID/blockID/turfID)
summary(model.rich.ti); Anova(model.rich.ti) #this may not be a great method to get variable significance
qqnorm(residuals(model.rich.ti)); qqline(residuals(model.rich.ti))
plot(model.rich.ti)

## ---- richness end ---- 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############### Traits analysis ###############

## ---- SumCover start ---- 
qqp(rtcforbs$deltasumcover, "norm")
ggplot(rtcforbs, aes(x = summer_temp, y = deltasumcover, colour = Year)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(rtcforbs, aes(x = annPrecip, y = deltasumcover, colour = Year)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

# base
model.dsc.ba <- lmer(deltasumcover ~ summer_temp + annPrecip + Year + summer_temp:Year + annPrecip:Year + (1|siteID/blockID), na.action=na.omit, REML = TRUE, data=rtcforbs)
vif.mer(model.dsc.ba)
step(model.dsc.ba)

model.dsc.ba <- update(model.dsc.ba, .~. - annPrecip:Year)
model.dsc.ba <- update(model.dsc.ba, .~. - annPrecip)

anova(model.sla.ba, model.sla.ba.int)
anova(model.sla.ba, model.sla.ba.temp)
anova(model.sla.ba, model.sla.ba.yr)

qqnorm(residuals(model.dsc.ba)); qqline(residuals(model.dsc.ba))
plot(model.dsc.ba)

ggplot(sla.ba, aes(x = Year, y = summer_temp, z = deltasumcover)) +
  geom_raster(aes(fill = deltasumcover), interpolate = TRUE) +
  scale_fill_distiller(palette = "Spectral") +
  geom_point(aes(deltasumcover))

  


#treatment delta
model.sla.tr <- lmer(deltawmean_SLA_local ~ summer_precip*summer_temp*Year + (1|siteID/blockID), na.action=na.omit, REML = TRUE, data = rtcforbs)
vif.mer(model.sla.tr)
drop1(model.sla.tr, test="Chisq")
model.sla.tr <- update(model.sla.tr, .~. - summer_precip:summer_temp:Year)
drop1(model.sla.tr, test="Chisq")
model.sla.tr <- update(model.sla.tr, .~. - summer_temp:Year)
drop1(model.sla.tr, test="Chisq")
summary(model.sla.tr); Anova(model.sla.tr)
qqnorm(residuals(model.sla.tr)); qqline(residuals(model.sla.tr))
plot(model.sla.tr)
sla.tr <- augment(model.sla.tr)

ggplot(sla.tr, aes(x = .fitted, y = deltawmean_SLA_local)) +
  geom_point() +
  geom_raster(aes(fill = .mu))

#time delta
model.sla.ti <- lmer(deltawmean_LDMC_local ~ prec*Year + (1|siteID/blockID), na.action=na.omit, REML = TRUE, data = rtcforbs)
vif.mer(model.sla.ti)
drop1(model.sla.ti, test="Chisq")
model.sla.ti <- update(model.sla.ti, .~. - Year)
summary(model.sla.ti); Anova(model.sla.ti)
qqnorm(residuals(model.sla.ti)); qqline(residuals(model.sla.ti))
plot(model.sla.ti)



######### SLA ##########
# base
qqp(rtcforbs$deltawmean_SLA_global)
model.sla.ba <- lmer(deltawmean_SLA ~ summer_temp + annPrecip + Year + summer_temp:Year + annPrecip:Year + (1|siteID/blockID), na.action=na.omit, REML = TRUE, data = rtcforbs)

vif.mer(model.sla.ba)
step(model.sla.ba)

model.sla.ba <- update(model.sla.ba, .~. - summer_temp:Year)
model.sla.ba <- update(model.sla.ba, .~. - summer_temp)

summary(model.sla.ba); Anova(model.sla.ba) #this may not be a great method to get variable significances
qqnorm(residuals(model.sla.ba)); qqline(residuals(model.sla.ba))
plot(model.sla.ba)

#treatment delta
model.sum.tr<-lmer(deltasumcover~temp*prec*Year + (1|siteID/blockID), na.action=na.omit, REML=FALSE, data=rtcmeta)
drop1(model.sum.tr, test="Chisq")
model.sum.tr <- update(model.sum.tr, .~. - Year)
summary(model.sum.tr); Anova(model.sum.tr)
qqnorm(residuals(model.sum.tr)); qqline(residuals(model.sum.tr))
plot(model.sum.tr)

#time delta
model.sum.ti<-lmer(deltasumcover~TTtreat*temp*scale(prec)*scale(Year) + (1|siteID/blockID/turfID), na.action=na.omit, REML=FALSE, data=timedelta)
drop1(model.sum.ti, test="Chisq")
model.sum.ti <- update(model.sum.ti, .~. - temp:scale(Year))
summary(model.sum.ti); Anova(model.sum.ti)
qqnorm(residuals(model.sum.ti)); qqline(residuals(model.sum.ti))
plot(model.sum.ti)


######### Height ##########
# base
model.hei.ba<-lmer(Height ~ TTtreat*temp*scale(prec)*scale(Year) + (1|siteID/blockID/turfID), na.action=na.omit, REML=FALSE, data=timedelta)
drop1(model.hei.ba, test = "Chisq")
model.hei.ba <- update(model.hei.ba, .~. - temp:scale(prec))
summary(model.hei.ba); Anova(model.hei.ba) #this may not be a great method to get variable significances
qqnorm(residuals(model.hei.ba)); qqline(residuals(model.hei.ba))
plot(model.hei.ba)

#treatment delta
model.hei.tr<-lmer(deltasumcover~temp*prec*Year + (1|siteID/blockID), na.action=na.omit, REML=FALSE, data=rtcmeta)
drop1(model.hei.tr, test="Chisq")
model.hei.tr <- update(model.hei.tr, .~. - Year)
summary(model.hei.tr); Anova(model.hei.tr)
qqnorm(residuals(model.hei.tr)); qqline(residuals(model.hei.tr))
plot(model.hei.tr)

#time delta
model.hei.ti<-lmer(deltasumcover~TTtreat*temp*scale(prec)*scale(Year) + (1|siteID/blockID/turfID), na.action=na.omit, REML=FALSE, data=timedelta)
drop1(model.hei.ti, test="Chisq")
model.hei.ti <- update(model.hei.ti, .~. - temp:scale(Year))
summary(model.hei.ti); Anova(model.hei.ti)
qqnorm(residuals(model.hei.ti)); qqline(residuals(model.hei.ti))
plot(model.hei.ti)


######### Seed mass ##########
#seed mass model Seedmass ~ TTtreat + prec + sYear + (1 | siteID/blockID/turfID) + TTtreat:sYear + prec:sYear
model.seedmass<-lmer(seedMass~TTtreat*scale(Year)*temp*prec + (1|Year) + (1|siteID/blockID/turfID), na.action=na.omit, REML=FALSE, data=cover.meta)
drop1(model.seedmass, test="Chisq")
model.seedmass <- update(model.seedmass, .~. - TTtreat:scale(Year))

summary(model.height)
Anova(model.sla) #this may not be a great method to get variable significances... 

############### SEEDLINGS ###############

#Adding values for temperature and precipitation to the dataset (rather than just levels)

recruitment.data$temp<-1
for(i in 1:nrow(recruitment.data)){
  if(recruitment.data$Temperature_level[i]==1){
    recruitment.data$temp[i]<-6.5
  } else {
    if(recruitment.data$Temperature_level[i]==2){
      recruitment.data$temp[i]<-8.5
      
    } else {
      recruitment.data$temp[i]<-10.5
    }
  }
  
}

recruitment.data$prec<-1

for(i in 1:nrow(recruitment.data)){
  if(recruitment.data$Precipitation_level[i]==1){
    recruitment.data$prec[i]<-0.600
  } else {
    if(recruitment.data$Precipitation_level[i]==2){
      recruitment.data$prec[i]<-1.200
      
    } else {
      if(recruitment.data$Precipitation_level[i]==3){
        recruitment.data$prec[i]<-2.000
        
      } else {
        recruitment.data$prec[i]<-2.700
      }
    }
  }
  
}

head(recruitment.data)

#Running the model
  #NB1: Poisson distribution for counts
  #NB2: No need to include year as a random factor or subtract data from different years, as seedling data was only collected in one year 

library(lme4)

###########################
###########################
###   failed to converge!!!
model.seedl<-glmer(seedlings~TTtreat*sTemperature_level*sPrecipitation_level + (1|siteID), na.action=na.omit, family=poisson, data=recruitment.data)

ms0 <- glmer(seedlings ~ 1 + (1|siteID), na.action=na.omit, family=poisson, data=recruitment.data)
add1(ms0,scope = formula (model.seedl), test = "Chisq")

summary(model.seedl)

model2.juv<-glmer(juveniles~TTtreat*temp*prec+(1|siteID/blockID), na.action=na.omit, family=poisson, data=recruitment.data) 
summary(model2.juv)


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

