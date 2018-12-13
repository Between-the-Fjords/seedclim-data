#load packages

# site averages from 2009 to 2016 for temp and precip

library(lme4)
library(MuMIn)
library(GGally)
library(tibble)
library(lmerTest)
library(broom)

forbcom %>%
  filter(TTtreat == "RTC") %>%
  select(Year, annPrecip, summer_temp, sumcover, wmeanSLA_local, wmeanheight_local, wmeanLDMC_local, richness, diversity) %>%
  ggpairs()

resp.traits.delta <- rtcmeta %>%
  filter(TTtreat == "RTC") %>%
  select(Year, annPrecip, summer_temp, deltasumcover, deltawmean_SLA_local, deltawmean_height_local, deltawmean_LDMC_local, deltarichness, deltadiversity) %>%
  ggpairs(resp.traits.delta)

#### Scaling explanatory variables ####

forbcom <- forbcom %>% 
  filter(!is.infinite(cwvSLA_local)) %>% 
  mutate(SannPrecip = as.numeric(scale(annPrecip)),
         Ssummer_temp = as.numeric(scale(summer_temp)),
         SYear = as.numeric(scale(Year)),
         wmeanLDMC_local = as.numeric(scale(wmeanLDMC_local)),
         wmeanseedMass_local = as.numeric(scale(wmeanseedMass_local)),
         wmeanCN_local = as.numeric(scale(wmeanCN_local)),
         wmeanheight_local = as.numeric(scale(wmeanheight_local)),
         wmeanSLA_local = as.numeric(scale(wmeanSLA_local)),
         wmeanLA_local = as.numeric(scale(wmeanLA_local)),
         wmeanLTH_local = as.numeric(scale(wmeanLTH_local)),
         sumcover = as.numeric(scale(sumcover)),
         evenness = as.numeric(scale(evenness)),
         richness = as.numeric(scale(richness)),
         cwvLDMC_local = as.numeric(scale(cwvLDMC_local)),
         cwvseedMass_local = as.numeric(scale(cwvseedMass_local)),
         cwvCN_local = as.numeric(scale(cwvCN_local)),
         cwvheight_local = as.numeric(scale(cwvheight_local)),
         cwvSLA_local = as.numeric(scale(cwvSLA_local)),
         cwvLA_local = as.numeric(scale(cwvLA_local)),
         cwvLTH_local = as.numeric(scale(cwvLTH_local)))

                            


  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mod1temp <- forbcom %>% 
  gather(key = trait, value = measurement, c(richness, evenness, sumcover, wmeanLDMC_local:cwvseedMass_local)) %>% 
  filter(!is.na(measurement)) %>% 
  #filter(trait %in% c(tempTraits$trait)) %>% 
  group_by(trait) %>%
  do({
    mod <- lmer(measurement ~ TTtreat*Ssummer_temp*SannPrecip*SYear - TTtreat:Ssummer_temp:SannPrecip:SYear + (1|siteID/blockID), REML = FALSE, data = .)
    tidy(mod)}) %>% 
  #filter(term %in% c("TTtreatRTC","TTtreatRTC:Ssummer_temp:SYear", "TTtreatRTC:SannPrecip:SYear", "TTtreatRTC:SYear")) %>% 
  arrange(desc(trait)) %>% 
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96)) %>%
  as.data.frame()

mod1temp <- mod1temp %>% 
  mutate(test = case_when(
    grepl("wmean", trait) ~ "Mean",
    grepl("cwv", trait) ~ "Variance",
    grepl("^s|^r|^e", trait) ~ "Mean"),
    term = case_when(
    grepl("TTtreatRTC:Ssummer_temp:SYear", term) ~ "t x year x removal",
    grepl("TTtreatRTC:SannPrecip:SYear", term) ~ "P x year x removal", 
    grepl("TTtreatRTC:SYear", term) ~ "Year x removal",
    grepl("TTtreatRTC", term) ~ "removal")) %>% 
  mutate(trait = if_else(grepl("wmean", trait), substr(trait, 6, regexpr("_", trait)),
                         if_else(grepl("cwv", trait), substr(trait, 4, regexpr("_", trait)), trait))) %>% 
  mutate(trait = if_else(grepl("_", trait), substr(trait, 1, (nchar(trait) -1)), trait)) %>% 
  mutate(sign = recode(trait, sumcover = 1, evenness = 1, richness = 1, seedMass = 1, height = 0, LA = 0, LTH = 0, LDMC = 0, CN = 1, SLA = 1))


coefEst <- mod1temp %>%
  ggplot(aes(x = trait, y = estimate, ymin = lower, ymax = upper, fill = factor(term, levels = c("P x year x removal", "t x year x removal", "Year x removal", "removal")), shape = factor(term, levels = c("P x year x removal", "t x year x removal", "Year x removal", "removal")), alpha = as.factor(sign))) +
  geom_errorbar(width = 0, position = position_dodge(width = 0.5), aes(colour = factor(term, levels = c("P x year x removal", "t x year x removal", "Year x removal", "removal")))) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 2.9) +
  coord_flip() +
  geom_vline(xintercept =  c(1.5,2.5,3.5,5.5,6.5,8.5,9.5), colour = "grey90") +
  geom_vline(xintercept =  7.5, colour = "black") +
  geom_vline(xintercept =  4.5, colour = "grey50") +
  scale_alpha_manual(values = c(0.6, 1), guide = FALSE) +
  scale_fill_manual(legend.title.climate, values = c("#1C9099", "#E69F00", "grey90", "black")) +
  scale_colour_manual(legend.title.climate, values = c("black", "black", "black", "black")) +
  scale_shape_manual(legend.title.climate, values = c(25, 24, 23, 21)) +
  #scale_linetype_manual(legend.title.climate, values = c(1,1,3, 21,21,23, 25)) +
  scale_x_discrete(limits = c("SLA", "CN", "LDMC", "LTH", "LA", "height", "seedMass", "richness", "evenness", "sumcover"), labels = c("SLA", "C:N ratio", "Leaf dry \n matter content", "Leaf thickness", "Leaf area", "Height", "Seed mass", "Richness", "Evenness", "Cover")) +
  facet_wrap(~test, strip.position = "top", scales = "free_x") +
  labs(y = "Standardised coefficients", x = "Leaf economic traits                 Structural traits               Community structure") +
  theme_cowplot(font_family = "Helvetica") +
  ylim(c(-0.4, 0.6)) +
  theme(strip.background = element_rect(fill="white"),
        legend.position = "bottom",
        legend.justification = "centre",
        legend.background = element_rect(fill = "white"),
        strip.text = element_text(size = 14, hjust = 0.42),
        axis.ticks.y = element_blank()) +
  theme(axis.text.y = element_text(colour = c("black", "black", "grey40", "grey40", "grey40", "grey40", "black", "black", "black", "black")))
  
coefEst <- plot_grid(coefEst, labels = c("B                                                       C"), label_x = 0)

leg <- ""  
leg <- "Functional groups \n in 2011 and 2016"
# plot 2 for IAVS conference
slaVar <- traitScale %>% 
  mutate(temp = if_else(grepl("6.5", temp), "Alpine", if_else(grepl("8.5", temp), "Sub-alpine", "Boreal"))) %>% 
  mutate(temp = factor(temp, levels = c("Alpine", "Sub-alpine", "Boreal"))) %>% 
  filter(TTtreat == "RTC", trait == "wmeanseedMass") %>% 
  filter(funYear %in% c("forb_2011", "forb_2016", "graminoid_2011")) %>% 
  ggplot(aes(measurement, fill = factor(funYear), colour = factor(funYear), group = factor(funYear))) +
  scale_fill_manual(leg, values = c("#F0E442", "#E69F00", "#81A88D"), labels = c("forbs 2011", "forbs 2016", "graminoids 2011")) +
  scale_colour_manual(leg, values = c("#F0E442", "#E69F00", "#81A88D"), labels = c("forbs 2011", "forbs 2016", "graminoids 2011")) +
  geom_density(alpha = 0.5, trim = FALSE, size = 0.9) +
  geom_rug(aes(colour = as.factor(funYear)), size = 0.8) +
  theme_cowplot(font_family = "Helvetica") +
  axis.dim +
  facet_grid( ~ temp) +
  theme(legend.position = c(0.8, 0.73),
        strip.background = element_blank(),
        plot.margin = unit(c(0,0,1,0), "cm")) +
  labs(x = expression("Community weighted mean SLA "(cm^2/g))) +
  xlim(0,3)

slaVar <- plot_grid(slaVar, labels = "A")

TEST <- plot_grid(slaVar, coefEst, nrow = 2, rel_heights = c(0.35,1))

ggsave(TEST, filename = "fig16coefEst_v4TEST.jpg", height = 12.5, width = 8, dpi = 300)




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
