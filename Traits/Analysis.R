#### Raw data ####

library(gridExtra)
library(multcomp)


##Height

hist(traitdata$Height)
hist(log(traitdata$Height))

ggplot(traitdata, aes(x = Site, y = log(Height), fill = factor(P_level))) +
  geom_boxplot()+
  scale_fill_brewer(palette="Blues",
                    name="Percipitation (mm/yr)",
                    labels=c("650", "1300", "1950", "2900"))+ #Change the numbers
  facet_wrap(~T_level, scales = "free_x")

   
##Lth

hist(traitdata$Lth_ave)
hist(log(traitdata$Lth_ave))

ggplot(traitdata, aes(x = Site, y = log(Lth_ave), fill = factor(P_level))) +
     geom_boxplot()+
  scale_fill_brewer(palette="Blues",
                    name="Percipitation (mm/yr)",
                    breaks=c("1", "2", "3", "4"),
                    labels=c("650", "1300", "1950", "2900"))+
  facet_wrap(~T_level, scales = "free_x")



##LDMC

hist(traitdata$LDMC)
hist(log(traitdata$LDMC))

ggplot(traitdata, aes(x = Site, y = log(LDMC), fill = factor(P_level))) +
  geom_boxplot()+
  scale_fill_brewer(palette="Blues",
                    name="Percipitation (mm/yr)",
                    breaks=c("1", "2", "3", "4"),
                    labels=c("650", "1300", "1950", "2900"))+
  facet_wrap(~T_level, scales = "free_x")



##Leaf area

hist(traitdata$Leaf_area)
hist(log(traitdata$Leaf_area))

ggplot(traitdata, aes(x = Site, y = log(Leaf_area), fill = factor(P_level))) +
  geom_boxplot()+
  scale_fill_brewer(palette="Blues",
                    name="Percipitation (mm/yr)",
                    breaks=c("1", "2", "3", "4"),
                    labels=c("650", "1300", "1950", "2900"))+
  facet_wrap(~T_level, scales = "free_x")



##SLA

hist(traitdata$SLA)
hist(log(traitdata$SLA))

ggplot(traitdata, aes(x = Site, y = log(SLA), fill = factor(P_level))) +
  geom_boxplot()+
  scale_fill_brewer(palette="Blues",
                    name="Percipitation (mm/yr)",
                    breaks=c("1", "2", "3", "4"),
                    labels=c("650", "1300", "1950", "2900"))+
  facet_wrap(~T_level, scales = "free_x")



##CN ratio

hist(traitdata_t$CN_ratio_men)
hist(log(traitdata_t$CN_ratio_men))

ggplot(traitdata, aes(x = Site, y = log(CN_ratio_mean), fill = factor(P_level))) +
  geom_boxplot()+
  scale_fill_brewer(palette="Blues",
                    name="Percipitation (mm/yr)",
                    breaks=c("1", "2", "3", "4"),
                    labels=c("650", "1300", "1950", "2900"))+
  facet_wrap(~T_level, scales = "free_x")

plot(traitdata$Site, traitdata$CN_ratio_mean)
plot(traitdata$T_level, traitdata$CN_ratio_mean)
plot(traitdata$P_level, traitdata$CN_ratio_mean)


## Functional Groups
par(mfrow = c(2, 3))
plot(traitdata$functionalGroup, traitdata$CN_ratio_mean, main = "C:N ratio")
plot(traitdata$functionalGroup, traitdata$LA_mean, main = "Leaf area")
plot(traitdata$functionalGroup, traitdata$SLA_mean, main = "SLA")
plot(traitdata$functionalGroup, traitdata$LDMC_mean, main = "LDMC")
plot(traitdata$functionalGroup, traitdata$Lth_mean, main = "Leaf thickness")
plot(traitdata$functionalGroup, traitdata$Height_mean, main = "Height")
par(mfrow = c(1, 1))



par(mfrow = c(2, 3))
plot(wcommunity_df$functionalGroup, wcommunity_df$Wmean_CN, main = "C:N ratio")
plot(wcommunity_df$functionalGroup, wcommunity_df$Wmean_LA, main = "Leaf area")
plot(wcommunity_df$functionalGroup, wcommunity_df$Wmean_SLA, main = "SLA")
plot(wcommunity_df$functionalGroup, wcommunity_df$Wmean_LDMC, main = "LDMC")
plot(wcommunity_df$functionalGroup, wcommunity_df$Wmean_Lth, main = "Leaf thickness")
plot(wcommunity_df$functionalGroup, wcommunity_df$Wmean_Height, main = "Height")
par(mfrow = c(1, 1))


## Lifespan ##

par(mfrow = c(2, 3))
plot(traitdata$lifeSpan, traitdata$CN_ratio_mean, main = "C:N ratio")
plot(traitdata$lifeSpan, traitdata$LA_mean, main = "Leaf area")
plot(traitdata$lifeSpan, traitdata$SLA_mean, main = "SLA")
plot(traitdata$lifeSpan, traitdata$LDMC_mean, main = "LDMC")
plot(traitdata$lifeSpan, traitdata$Lth_mean, main = "Leaf thickness")
plot(traitdata$lifeSpan, traitdata$Height_mean, main = "Height")
par(mfrow = c(1, 1))



par(mfrow = c(2, 3))
plot(wcommunity_df$lifeSpan, wcommunity_df$Wmean_CN, main = "C:N ratio")
plot(wcommunity_df$lifeSpan, wcommunity_df$Wmean_LA, main = "Leaf area")
plot(wcommunity_df$lifeSpan, wcommunity_df$Wmean_SLA, main = "SLA")
plot(wcommunity_df$lifeSpan, wcommunity_df$Wmean_LDMC, main = "LDMC")
plot(wcommunity_df$lifeSpan, wcommunity_df$Wmean_Lth, main = "Leaf thickness")
plot(wcommunity_df$lifeSpan, wcommunity_df$Wmean_Height, main = "Height")
par(mfrow = c(1, 1))



## Specialist or generalist ##

par(mfrow = c(2, 3))
plot(traitdata$occurrence, traitdata$CN_ratio_mean, main = "C:N ratio")
plot(traitdata$occurrence, traitdata$LA_mean, main = "Leaf area")
plot(traitdata$occurrence, traitdata$SLA_mean, main = "SLA")
plot(traitdata$occurrence, traitdata$LDMC_mean, main = "LDMC")
plot(traitdata$occurrence, traitdata$Lth_mean, main = "Leaf thickness")
plot(traitdata$occurrence, traitdata$Height_mean, main = "Height")
par(mfrow = c(1, 1))



## Precipitation ##


plot1 <- ggplot(traitdata, aes(x = Precip, y = CN.ratio)) +
  geom_jitter()+
  geom_smooth(aes(group = 1), method='lm')

CNP.lm<-lm(CN.ratio~Precip, data=traitdata)
anova(CNP.lm)
summary(CNP.lm)



plot2 <- ggplot(traitdata, aes(x = Precip, y = log(Leaf_area))) +
  geom_point()+
  geom_smooth(method='lm')

LAP.lm<-lm(Leaf_area~Precip, data=traitdata)
anova(LAP.lm)
summary(LAP.lm)




plot3 <- ggplot(traitdata, aes(x = Precip, y = SLA)) +
  geom_point()+
  geom_smooth(method='lm')

SLAP.lm<-lm(SLA~Precip, data=traitdata)
anova(SLAP.lm)
summary(SLAP.lm)



plot4 <- ggplot(traitdata, aes(x = Precip, y = LDMC)) +
  geom_point()+
  geom_smooth(method='lm')

LDMCP.lm<-lm(LDMC~Precip, data=traitdata)
anova(LDMCP.lm)
summary(LDMCP.lm)



plot5<- ggplot(traitdata, aes(x = Precip, y = Lth_ave)) +
  geom_point()+
  geom_smooth(method='lm')

LthP.lm<-lm(Lth_ave~Precip, data=traitdata)
anova(LthP.lm)
summary(LthP.lm)



plot6 <- ggplot(traitdata, aes(x = Precip, y = log(Height))) +
  geom_point()+
  geom_smooth(method='lm')

HeightP.lm<-lm(Height~Precip, data=traitdata)
anova(HeightP.lm)
summary(HeightP.lm)



grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=2)



## Temperature ##

T_plot1<- ggplot(traitdata, aes(x = Temp, y = CN.ratio)) +
  geom_point()+
  geom_smooth(method='lm')

CN.lm<-lm(CN.ratio~Temp, data=traitdata)
#anova(CN.lm)
summary(CN.lm)



T_plot2<- ggplot(traitdata, aes(x = Temp, y = log(Leaf_area))) +
  geom_point()+
  geom_smooth(method='lm')


LA.lm <- lm(Leaf_area ~ Temp, data = traitdata)
#anova(LA.lm)
summary(LA.lm)




T_plot3<- ggplot(traitdata, aes(x = Temp, y = SLA)) +
  geom_point()+
  geom_smooth(method='lm')

SLA.lm<-lm(log(SLA)~Temp, data=traitdata)
#anova(SLA.lm)
summary(SLA.lm)




T_plot4<- ggplot(traitdata, aes(x = Temp, y = LDMC)) +
  geom_point()+
  geom_smooth(method='lm')

LDMC.lm<-lm(LDMC~Temp, data=traitdata)
#anova(LDMC.lm)
summary(LDMC.lm)




T_plot5<- ggplot(traitdata, aes(x = Temp, y = Lth_ave)) +
  geom_point()+
  geom_smooth(method='lm')

Lth.lm<-lm(Lth_ave~Temp, data=traitdata)
#anova(Lthlm)
summary(Lth.lm)



T_plot6<- ggplot(traitdata, aes(x = Temp, y = log(Height))) +
  geom_point()+
  geom_smooth(method='lm')

Height.lm<-lm(Height~Temp, data=traitdata)
#anova(Height.lm)
summary(Height.lm)



grid.arrange(T_plot1, T_plot2, T_plot3, T_plot4, T_plot5, T_plot6, ncol=2)



#### Linear test ####


CN.lm<-lm(CN.ratio~Temp, data=traitdata)
anova(CN.lm)
summary(CN.lm)

TukeyCN<- glht(CN.lm, linfct=mcp(T_level="Tukey"), data=traitdata)
summary(TukeyCN)



#Linear regression


CN.lm <- lm(CN.ratio~Temp, data=traitdata)
summary(CN.lm)



CNP.lm <- lm(CN.ratio~Precip, data=traitdata)
summary(CNP.lm)



#### WEIGHTED MEANS ####

#### SLA ####

#hist(wcommunity_df$Wmean_SLA) #To check if it was normally distributed, and it was
#hist(log(wcommunity_df$Wmean_SLA))
#shapiro.test(wcommunity_df$Wmean_SLA)
#shapiro.test(log(wcommunity_df$Wmean_SLA)) #Didn't work until I changed the Inf values to NA, can I do that?

ggplot(wcommunity_df, aes(x = Site, y = log(Wmean_SLA), fill = factor(P_level))) +
  geom_boxplot()+
  scale_fill_brewer(palette="Blues",
                    name="Precipitation
  (mm/yr)",
                    breaks=c("1", "2", "3", "4"),
                    labels=c("650", "1300", "1950", "2900"))+
  facet_wrap(~T_level, scales = "free_x")+
  ylab("CWM of SLA(log)")+
  ggtitle("Specific Leaf Area")


#### LA ####

#hist(wcommunity_df$Wmean_LA) #Is defenetly skewed
#hist(log(wcommunity_df$Wmean_LA))
#shapiro.test(wcommunity_df$Wmean_LA)
#shapiro.test(log(wcommunity_df$Wmean_LA))

ggplot(wcommunity_df, aes(x = Site, y = log(Wmean_LA), fill = factor(P_level))) +
  geom_boxplot()+
  scale_fill_brewer(palette="Blues",
                    name="Percipitation (mm/yr)",
                    breaks=c("1", "2", "3", "4"),
                    labels=c("662", "1279", "1939", "2892"))+
  facet_wrap(~T_level, scales = "free_x")+
  ylab("CWM of Leaf Area (log)")

#### Lth ####
  
#hist(wcommunity_df$Wmean_Lth) #It looks a little bit skewed
#hist(log(wcommunity_df$Wmean_Lth)) #I don't think that this looks any better..
#shapiro.test(wcommunity_df$Wmean_Lth)
#shapiro.test(log(wcommunity_df$Wmean_Lth)) #The log transformation was chosen because of the shapiro test.

#hist(sqrt(wcommunity_df$Wmean_Lth)) #Also tried the sqrt transformation, but that was still skewed.
#shapiro.test(sqrt(wcommunity_df$Wmean_Lth))


ggplot(wcommunity_df, aes(x = Site, y = log(Wmean_Lth), fill=factor(P_level))) +
  geom_boxplot()+
  scale_fill_brewer(palette="Blues",
                    name="Percipitation (mm/yr)",
                    breaks=c("1", "2", "3", "4"),
                    labels=c("662", "1279", "1939", "2892"))+
  facet_wrap(~T_level, scales = "free_x")+
  ylab("CWM of Leaf Thickness (log)")


#### LDMC ####

#hist(wcommunity_df$Wmean_LDMC) #Has a few outliers all the way to the right
#hist(log(wcommunity_df$Wmean_LDMC))
#hist(sqrt(wcommunity_df$Wmean_LDMC)) #Doesnt get rid of the outliers
#shapiro.test(wcommunity_df$Wmean_LDMC)
#shapiro.test(log(wcommunity_df$Wmean_LDMC))
#shapiro.test(sqrt(wcommunity_df$Wmean_LDMC))


ggplot(wcommunity_df, aes(x = Site, y = log(Wmean_LDMC), fill=factor(P_level))) +
  geom_boxplot()+
  scale_fill_brewer(palette="Blues",
                    name="Percipitation (mm/yr)",
                    breaks=c("1", "2", "3", "4"),
                    labels=c("662", "1279", "1939", "2892"))+
  facet_wrap(~T_level, scales = "free_x")+
  ylab("CWM of LDMC (log)")



#### Height ####


#hist(wcommunity_df$Wmean_Height) #Definetly skewed
#hist(log(wcommunity_df$Wmean_Height)) #Looks much better
#shapiro.test(wcommunity_df$Wmean_Height)
#shapiro.test(log(wcommunity_df$Wmean_Height))
#hist(sqrt(wcommunity_df$Wmean_Height)) #Nope

ggplot(wcommunity_df, aes(x = Site, y = log(Wmean_Height), fill=factor(P_level))) +
  geom_boxplot()+
  scale_fill_brewer(palette="Blues",
                    name="Percipitation (mm/yr)",
                    breaks=c("1", "2", "3", "4"),
                    labels=c("662", "1279", "1939", "2892"))+
  facet_wrap(~T_level, scales = "free_x")+
  ylab("CWM of Height (log)")


#Make a code for one graph and call it something.
#Make a new graph by writing what you called it + and the part of the code you want to change.. In this example it would be + aes(x=Site, y= new trait)

#### ANOVA analysis ####


#### SLA ####


anova_SLA_P<-aov(Wmean_SLA~P_level, wcommunity_df)
summary(anova_SLA_P)
TukeyHSD(anova_SLA_P)



anova_SLA_T<-aov(Wmean_SLA~T_level, wcommunity_df)
summary(anova_SLA_T)
TukeyHSD(anova_SLA_T)



#### ANOVA Height ####


anova_H_T<-aov(Wmean_Height~T_level, wcommunity_df)
summary(anova_H_T)
TukeyHSD(anova_H_T)


anova_H_P<-aov(Wmean_Height~P_level, wcommunity_df)
summary(anova_H_P)
TukeyHSD(anova_H_P)



#### ANOVA LDMC ####


anova_LDMC_T<-aov(Wmean_LDMC~T_level, wcommunity_df)
summary(anova_LDMC_T)
TukeyHSD(anova_LDMC_T)


anova_LDMC_P<-aov(Wmean_LDMC~P_level, wcommunity_df)
summary(anova_LDMC_P)
TukeyHSD(anova_LDMC_P)


#### ANOVA Lth ####


anova_Lth_T<-aov(Wmean_Lth~T_level, wcommunity_df)
summary(anova_Lth_T)
TukeyHSD(anova_Lth_T)


anova_Lth_P<-aov(Wmean_Lth~P_level, wcommunity_df)
summary(anova_Lth_P)
TukeyHSD(anova_Lth_P)


#### ANOVA LA ####


anova_LA_T<-aov(Wmean_LA~T_level, wcommunity_df)
summary(anova_LA_T)
TukeyHSD(anova_LA_T)



anova_LA_P<-aov(Wmean_LA~P_level, wcommunity_df)
summary(anova_LA_P)
TukeyHSD(anova_LA_P)

#### Mixed effect model ####

library(lme4)

#### Temperature ####

## SLA ##

MEMSLA1x<-lmer(SLA~Temp+scale(Precip)+Temp:scale(Precip)+(1|Site), data=traitdata)
MEMSLA0<-lmer(SLA~1+(1|Site), data=traitdata)
summary(MEMSLA1)

AIC(MEMSLA1x, MEMSLA0)


## Height ##

MEMHeight1<-lmer(log(Height)~Temp+(1|Site), data=traitdata)
MEMHeight0<-lmer(log(Height)~1+(1|Site), data=traitdata)
summary(MEMHeight1)

AIC(MEMHeight1, MEMHeight0)


## Leaf Area ##

MEM_LA_1<-lmer(log(Leaf_area)~Temp+(1|Site), data=traitdata)
MEM_LA_0<-lmer(log(Leaf_area)~1+(1|Site), data=traitdata)
summary(MEM_LA_1)

AIC(MEM_LA_1, MEM_LA_0)


## LDMC ##

MEM_LDMC_1<-lmer(LDMC~Temp+(1|Site)+(1|Species), data=traitdata)
MEM_LDMC_0<-lmer(LDMC~1+(1|Site)+(1|Species), data=traitdata)
summary(MEM_LDMC_1)

AIC(MEM_LDMC_1, MEM_LDMC_0)


## CN ratio ##

MEM_CN_1<-lmer(CN.ratio~Temp+(1|Site), data=traitdata)
MEM_CN_0<-lmer(CN.ratio~1+(1|Site), data=traitdata)
summary(MEM_CN_1)

AIC(MEM_CN_1, MEM_CN_0)


## Leaf thickness ##

MEM_Lth_1<-lmer(Lth_ave~Temp+(1|Site), data=traitdata)
MEM_Lth_0<-lmer(Lth_ave~1+(1|Site), data=traitdata)
summary(MEM_Lth_1)

AIC(MEM_Lth_1, MEM_Lth_0)



#### Precipitation ####



## SLA ##

MEMSLA1_P<-lmer(SLA~scale(Precip)+Temp+Temp:scale(Precip)+(1|Site)+(1|Species), data=traitdata)
MEMSLA0<-lmer(SLA~1+(1|Site)+(1|Species), data=traitdata)

summary(MEMSLA1_P)

AIC(MEMSLA1_P, MEMSLA0)


## Height ##

MEMHeight1_P<-lmer(log(Height)~Precip+(1|Site), data=traitdata)
MEMHeight0<-lmer(log(Height)~1+(1|Site), data=traitdata)
summary(MEMHeight1_P)

AIC(MEMHeight1_P, MEMHeight0)


## Leaf Area ##

MEM_LA_1_P<-lmer(log(Leaf_area)~Precip+(1|Site), data=traitdata)
MEM_LA_0<-lmer(log(Leaf_area)~1+(1|Site), data=traitdata)
summary(MEM_LA_1_P)

AIC(MEM_LA_1_P, MEM_LA_0)


## LDMC ##

MEM_LDMC_1_P<-lmer(LDMC~Precip+(1|Site), data=traitdata)
MEM_LDMC_0<-lmer(LDMC~1+(1|Site), data=traitdata)
summary(MEM_LDMC_1_P)

AIC(MEM_LDMC_1_P, MEM_LDMC_0)


## CN ratio ##

MEM_CN_1_P<-lmer(CN.ratio~Precip+(1|Site), data=traitdata)
MEM_CN_0<-lmer(CN.ratio~1+(1|Site), data=traitdata)
summary(MEM_CN_1)

AIC(MEM_CN_1_P, MEM_CN_0)


## Leaf thickness ##

MEM_Lth_1_P<-lmer(Lth_ave~Precip+(1|Site), data=traitdata)
MEM_Lth_0<-lmer(Lth_ave~1+(1|Site), data=traitdata)
summary(MEM_Lth_1)

AIC(MEM_Lth_1_P, MEM_Lth_0)




#### Plotte traits mot hverandre ####


pairs_traits<-wcommunity_df%>%
  ungroup()%>%
  select(-Site, -Site_sp, -species, -mean_cover, -T_level, -P_level, -Wmean_LDMC, -Wmean_Lth, -Wmean_LA, -Wmean_SLA)


#Traits without weighting them

pairs_traits1<-traitdata%>%
  select(Height, Wet_mass, Dry_mass, LDMC, Lth_ave, Leaf_area, SLA, CN.ratio)


pairs(log(pairs_traits1), gap=0)

GGally::ggpairs(pairs_traits1)


#Weighted traits


pairs_traits2<-wcommunity_df%>%
  select(Wmean_LDMC, Wmean_Lth, Wmean_LA, Wmean_SLA, Wmean_Height, Wmean_CN)


pairs(log(pairs_traits2), gap=0)


GGally::ggpairs(pairs_traits2)



## Realtionship between SLA and LDMC ##


#SLA and LDMC are supposed to have a negative correlation

ggplot(traitdata, aes(x = log(SLA), y = log(LDMC))) +
  geom_point()+
  geom_smooth(method='lm')


## Realtionship between LDMC and Lth ##

#LDMC and Lth are supposed to say something about the same thing, should be a similar trend

ggplot(traitdata, aes(x = LDMC, y = Lth_ave)) +
  geom_point()

ggplot(traitdata, aes(x = log(LDMC), y = log(Lth_ave))) +
  geom_point()



## SLA, LDMC and Lth relationship, found from an article ##

ggplot(traitdata, aes(x = ((SLA_mean*LDMC_mean)^-1), y = Lth_mean)) +
  geom_point()+
  geom_smooth(method='lm')

ggplot(traitdata, aes(x = ((SLA*LDMC)^-1), y = Lth_ave)) +
  geom_point()+
  geom_smooth(method='lm')


## Comparing the rest of the traits ##

ggplot(traitdata, aes(x = log(SLA), y = log(Lth_ave))) +
  geom_point()+
  geom_smooth(method='lm')

ggplot(traitdata, aes(x = log(Height), y = log(Leaf_area ))) +
  geom_point()+
  geom_smooth(method='lm')

ggplot(traitdata, aes(x = log(LDMC), y = log(CN.ratio))) +
  geom_point()+
  geom_smooth(method='lm')

ggplot(traitdata, aes(x = log(SLA), y = log(CN.ratio))) +
  geom_point()+
  geom_smooth(method='lm')

ggplot(traitdata, aes(x = log(Lth_ave), y = log(CN.ratio))) +
  geom_point()+
  geom_smooth(method='lm')



#### Ordination ####

Env_var<-wcommunity_df %>%
  distinct(turfID, .keep_all=TRUE)%>%
  arrange(turfID)%>%
  filter(!is.na(SLA_mean))%>%
  select(SLA_mean, Lth_mean, Height_mean, LDMC_mean, LA_mean, CN.ratio)


cover <- wcommunity_df%>%
  filter(!is.na(SLA_mean))
cover <- xtabs(cover ~ turfID + species, data = wcommunity_df)
cover <- as.data.frame(unclass(cover))
cover <- cover[,colSums(cover > 0) > 0] #remove empty spp

library(vegan)

x<- rda(cover~ SLA_mean, Lth_mean, Height_mean, LDMC_mean, data = Env_var)
