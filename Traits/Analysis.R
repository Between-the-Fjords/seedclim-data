#### Raw data ####

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


#### Functional Groups ####
par(mfrow = c(2, 3))
plot(traitdata$functionalGroup, traitdata$CN_ratio_mean, main = "C:N ratio")
plot(traitdata$functionalGroup, traitdata$LA_mean, main = "Leaf area")
plot(traitdata$functionalGroup, traitdata$SLA_mean, main = "SLA")
plot(traitdata$functionalGroup, traitdata$LDMC_mean, main = "LDMC")
plot(traitdata$functionalGroup, traitdata$Lth_mean, main = "Leaf thickness")
plot(traitdata$functionalGroup, traitdata$Height_mean, main = "Height")


#### Lifespan ####

par(mfrow = c(2, 3))
plot(traitdata$lifeSpan, traitdata$CN_ratio_mean, main = "C:N ratio")
plot(traitdata$lifeSpan, traitdata$LA_mean, main = "Leaf area")
plot(traitdata$lifeSpan, traitdata$SLA_mean, main = "SLA")
plot(traitdata$lifeSpan, traitdata$LDMC_mean, main = "LDMC")
plot(traitdata$lifeSpan, traitdata$Lth_mean, main = "Leaf thickness")
plot(traitdata$lifeSpan, traitdata$Height_mean, main = "Height")

#### Precipitation ####

par(mfrow = c(2, 3))
ggplot(traitdata, aes(x = Precip, y = log(CN.ratio))) +
  geom_point()+
  geom_smooth(method='lm')

ggplot(traitdata, aes(x = Precip, y = log(Leaf_area))) +
  geom_point()+
  geom_smooth(method='lm')

ggplot(traitdata, aes(x = Precip, y = log(SLA))) +
  geom_point()+
  geom_smooth(method='lm')

ggplot(traitdata, aes(x = Precip, y = log(LDMC))) +
  geom_point()+
  geom_smooth(method='lm')

ggplot(traitdata, aes(x = Precip, y = log(Lth_ave))) +
  geom_point()+
  geom_smooth(method='lm')

ggplot(traitdata, aes(x = Precip, y = log(Height))) +
  geom_point()+
  geom_smooth(method='lm')

#### Temperature ####

ggplot(traitdata, aes(x = Temp, y = log(CN.ratio))) +
  geom_point()+
  geom_smooth(method='lm')

ggplot(traitdata, aes(x = Temp, y = log(Leaf_area))) +
  geom_point()+
  geom_smooth(method='lm')

ggplot(traitdata, aes(x = Temp, y = log(SLA))) +
  geom_point()+
  geom_smooth(method='lm')

ggplot(traitdata, aes(x = Temp, y = log(LDMC))) +
  geom_point()+
  geom_smooth(method='lm')

ggplot(traitdata, aes(x = Temp, y = log(Lth_ave))) +
  geom_point()+
  geom_smooth(method='lm')

ggplot(traitdata, aes(x = Temp, y = log(Height))) +
  geom_point()+
  geom_smooth(method='lm')


#Linear test

CN.lm<-lm(CN.ratio~T_level, data=traitdata)
anova(CN.lm)
summary(CN.lm)

install.packages("multcomp")
library(multcomp)

TukeyCN<- glht(CN.lm, linfct=mcp(T_level="Tukey"), data=traitdata)
summary(TukeyCN)

#Linear regression

CN.lm <- lm(CN.ratio~Temp, data=traitdata)
summary(CN.lm)


#### WEIGHTED MEANS ####

####SLA

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


####LA

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

####Lth
  
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


####LDMC

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



####Height


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


#ANOVA SLA 
anova_SLA_P<-aov(Wmean_SLA~P_level, wcommunity_df)
summary(anova_SLA_P)
TukeyHSD(anova_SLA_P)

anova_SLA_T<-aov(Wmean_SLA~T_level, wcommunity_df)
summary(anova_SLA_T)
TukeyHSD(anova_SLA_T)

#ANOVA Height
anova_H_T<-aov(Wmean_Height~T_level, wcommunity_df)
summary(anova_H_T)
TukeyHSD(anova_H_T)

anova_H_P<-aov(Wmean_Height~P_level, wcommunity_df)
summary(anova_H_P)
TukeyHSD(anova_H_P)

#ANOVA LDMC
anova_LDMC_T<-aov(Wmean_LDMC~T_level, wcommunity_df)
summary(anova_LDMC_T)
TukeyHSD(anova_LDMC_T)

anova_LDMC_P<-aov(Wmean_LDMC~P_level, wcommunity_df)
summary(anova_LDMC_P)
TukeyHSD(anova_LDMC_P)

#ANOVA Lth
anova_Lth_T<-aov(Wmean_Lth~T_level, wcommunity_df)
summary(anova_Lth_T)
TukeyHSD(anova_Lth_T)

anova_Lth_P<-aov(Wmean_Lth~P_level, wcommunity_df)
summary(anova_Lth_P)
TukeyHSD(anova_Lth_P)

#ANOVA LA
anova_LA_T<-aov(Wmean_LA~T_level, wcommunity_df)
summary(anova_LA_T)
TukeyHSD(anova_LA_T)

anova_LA_P<-aov(Wmean_LA~P_level, wcommunity_df)
summary(anova_LA_P)
TukeyHSD(anova_LA_P)

#### Plotte traits mot hverandre ####


pairs_traits<-wcommunity_df%>%
  ungroup()%>%
  select(-Site, -Site_sp, -Species, -mean_cover, -T_level, -P_level, -Wmean_LDMC, -Wmean_Lth, -Wmean_LA, -Wmean_SLA)

pairs_traits1<-traitdata%>%
  select(Height, Wet_mass, Dry_mass, LDMC, Lth_ave, Leaf_area, SLA, N.., C.., CN.ratio, Temp, Precip)

pairs(log(pairs_traits1), gap=0)

GGally::ggpairs(pairs_traits1)
