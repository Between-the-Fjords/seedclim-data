##### Traits with abiotic factors #####

### SLA ###

mean(wcommunity_df$SLA, na.rm=TRUE)
mean(wcommunity_df$SLA_mean, na.rm=TRUE)
mean(wcommunity_df$SLA_mean_global, na.rm=TRUE)
mean(wcommunity_df$Wmean_SLA, na.rm=TRUE)
mean(wcommunity_df$Wmean_global_SLA, na.rm=TRUE)


SLA_raw_Temp <- lm(SLA ~Temp, data= wcommunity_df)
summary(SLA_raw_Temp)

newdata_Temp<-expand.grid( Temp=seq(5.5, 11, length=1000))

newdata_Temp$fit <- predict(SLA_raw_Temp, re.form=NA, newdata=newdata_Temp)


pLot1<-wcommunity_df%>%
  select(Site, Species, Temp, SLA)%>%
  unique()%>%
  ggplot(aes(x=Temp, y=SLA))+
  geom_jitter(width=0.1)+
  geom_line(aes( x = Temp, y = fit), data=newdata_Temp, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#FF6666")+
  labs( y="SLA (cm^2/g", x="")+
  theme_minimal()


SLA_raw_precip <- lm(SLA ~Precip, data= wcommunity_df)
summary(SLA_raw_precip)

newdata_Precip<-expand.grid(Precip=seq(500, 3100, length=1000))

newdata_Precip$fit <- predict(SLA_raw_precip, re.form=NA, newdata=newdata_Precip)

pLot2<-wcommunity_df%>%
  select(Site, Species, Precip, SLA)%>%
  unique()%>%
  ggplot(aes(x=Precip, y=SLA))+
  geom_jitter(width=30)+
  geom_line(aes( x = Precip, y = fit), data=newdata_Precip, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#99CCFF")+
  labs(x="", y="")+
  theme_minimal()

### LDMC ###

LDMC_raw_Temp <- lm(LDMC ~Temp, data= wcommunity_df)
summary(LDMC_raw_Temp)

newdata_Temp$fit_LDMC <- predict(LDMC_raw_Temp, re.form=NA, newdata=newdata_Temp)

pLot3<-wcommunity_df%>%
  select(Site, Species, Temp, LDMC)%>%
  unique()%>%
  filter(!LDMC>1)%>%
  ggplot(aes(x=Temp, y=LDMC))+
  geom_jitter(width=0.1)+
  geom_line(aes( x = Temp, y = fit_LDMC), data=newdata_Temp, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#FF6666", linetype = 2)+
  labs(y="LDMC", x="")+
  theme_minimal()

LDMC_raw_precip <- lm(LDMC ~Precip, data= wcommunity_df)
summary(LDMC_raw_precip)

newdata_Precip$fit_LDMC <- predict(LDMC_raw_precip, re.form=NA, newdata=newdata_Precip)

pLot4<-wcommunity_df%>%
  select(Site, Species, Precip, LDMC)%>%
  unique()%>%
  filter(!LDMC>1)%>%
  ggplot(aes(x=Precip, y=LDMC))+
  geom_jitter(width=30)+
  geom_line(aes( x = Precip, y = fit_LDMC), data=newdata_Precip, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#99CCFF", linetype = 2)+
  labs(x="", y="")+
  theme_minimal()

#### Leaf thickness ####

Lth_raw_Temp <- lm(Lth_ave ~Temp, data= wcommunity_df)
summary(Lth_raw_Temp)

newdata_Temp$fit_Lth <- predict(Lth_raw_Temp, re.form=NA, newdata=newdata_Temp)

pLot5<-wcommunity_df%>%
  select(Site, Species, Temp, Lth_ave)%>%
  unique()%>%
  ggplot(aes(x=Temp, y=Lth_ave))+
  geom_jitter(width=0.1)+
  geom_line(aes( x = Temp, y = fit_Lth), data=newdata_Temp, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#FF6666", linetype = 1)+
  labs(y="Leaf thickness (mm)", x="")+
  theme_minimal()


Lth_raw_precip <- lm(Lth_ave ~Precip, data= wcommunity_df)
summary(Lth_raw_precip)

newdata_Precip$fit_Lth <- predict(LDMC_raw_precip, re.form=NA, newdata=newdata_Precip)

pLot6<-wcommunity_df%>%
  select(Site, Species, Precip, Lth_ave)%>%
  unique()%>%
  ggplot(aes(x=Precip, y=Lth_ave))+
  geom_jitter(width=30)+
  geom_line(aes( x = Precip, y = fit_Lth), data=newdata_Precip, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#99CCFF", linetype = 1)+
  labs(x="", y="")+
  theme_minimal()

#### CN ratio ####

CN_raw_Temp <- lm(CN.ratio ~Temp, data= wcommunity_df)
summary(CN_raw_Temp)

newdata_Temp$fit_CN <- predict(CN_raw_Temp, re.form=NA, newdata=newdata_Temp)

pLot7<-wcommunity_df%>%
  select(Site, Species, Temp, CN.ratio)%>%
  unique()%>%
  ggplot(aes(x=Temp, y=CN.ratio))+
  geom_jitter(width=0.1)+
  geom_line(aes( x = Temp, y = fit_CN), data=newdata_Temp, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#FF6666", linetype = 1)+
  labs(y="C/N ratio", x="")+
  theme_minimal()

CN_raw_precip <- lm(CN.ratio ~Precip, data= wcommunity_df)
summary(CN_raw_precip)

newdata_Precip$fit_CN <- predict(CN_raw_precip, re.form=NA, newdata=newdata_Precip)

pLot8<-wcommunity_df%>%
  select(Site, Species, Precip, CN.ratio)%>%
  unique()%>%
  ggplot(aes(x=Precip, y=CN.ratio))+
  geom_jitter(width=30)+
  geom_line(aes( x = Precip, y = fit_CN), data=newdata_Precip, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#99CCFF", linetype = 1)+
  labs(x="", y="")+
  theme_minimal()

#### Height ####

Forbs<-wcommunity_df%>%
  filter(functionalGroup=="forb")

H_Forb_raw_Temp <- lm(log(Height) ~Temp, data= Forbs)
summary(H_Forb_raw_Temp)

newdata_Temp$fit_H_Forb <- predict(H_Forb_raw_Temp, re.form=NA, newdata=newdata_Temp)

pLot9<-wcommunity_df%>%
  filter(functionalGroup=="forb")%>%
  select(Site, Species, Temp, Height)%>%
  unique()%>%
  ggplot(aes(x=Temp, y=log(Height)))+
  geom_jitter(width=0.1)+
  geom_line(aes( x = Temp, y = fit_H_Forb), data=newdata_Temp, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#FF6666", linetype = 1)+
  labs(y="Height (f) (log)", x="")+
  theme_minimal()

H_Forb_raw_Precip <- lm(log(Height) ~Precip, data= Forbs)
summary(H_Forb_raw_Precip)

newdata_Precip$fit_H_Forb <- predict(H_Forb_raw_Precip, re.form=NA, newdata=newdata_Precip)

pLot10<-wcommunity_df%>%
  filter(functionalGroup=="forb")%>%
  select(Site, Species, Precip, Height)%>%
  unique()%>%
  ggplot(aes(x=Precip, y=log(Height)))+
  geom_jitter(width=30)+
  geom_line(aes( x = Precip, y = fit_H_Forb), data=newdata_Precip, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#99CCFF", linetype = 1)+
  labs(x="", y="")+
  theme_minimal()

Graminoids<-wcommunity_df%>%
  filter(functionalGroup=="graminoid")

H_Gram_raw_Temp <- lm(log(Height) ~Temp, data= Graminoids)
summary(H_Gram_raw_Temp)

newdata_Temp$fit_H_Gram <- predict(H_Gram_raw_Temp, re.form=NA, newdata=newdata_Temp)

pLot11<-wcommunity_df%>%
  filter(functionalGroup=="graminoid")%>%
  select(Site, Species, Temp, Height)%>%
  unique()%>%
  ggplot(aes(x=Temp, y=log(Height)))+
  geom_jitter(width=0.1)+
  geom_line(aes( x = Temp, y = fit_H_Gram), data=newdata_Temp, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#FF6666", linetype = 1)+
  labs(x="Temperature (C)", y="Height (g) (log)")+
  theme_minimal()

H_Gram_raw_Precip <- lm(log(Height) ~Precip, data= Graminoids)
summary(H_Gram_raw_Precip)

newdata_Precip$fit_H_Gram <- predict(H_Gram_raw_Precip, re.form=NA, newdata=newdata_Precip)

pLot12<-wcommunity_df%>%
  filter(functionalGroup=="graminoid")%>%
  select(Site, Species, Precip, Height)%>%
  unique()%>%
  ggplot(aes(x=Precip, y=log(Height)))+
  geom_jitter(width=30)+
  geom_line(aes( x = Precip, y = fit_H_Gram), data=newdata_Precip, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#99CCFF", linetype = 2)+
  labs(y="", x="Precipitation (mm/year)")+
  theme_minimal()

library(gridExtra)

grid.arrange(pLot1, pLot2, pLot3, pLot4, pLot5, pLot6, pLot7, pLot8, pLot9, pLot10, pLot11, pLot12, ncol=2)

