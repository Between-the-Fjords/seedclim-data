##### Traits with abiotic factors #####
library("lme4")
library("broom")
library("lmerTest")
library("gridExtra")
### SLA ###

mean(wcommunity_df$SLA, na.rm=TRUE)
mean(wcommunity_df$SLA_mean, na.rm=TRUE)
mean(wcommunity_df$SLA_mean_global, na.rm=TRUE)
mean(wcommunity_df$Wmean_SLA, na.rm=TRUE)
mean(wcommunity_df$Wmean_global_SLA, na.rm=TRUE)

mean(wcommunity_df$Lth_ave, na.rm=TRUE)
mean(wcommunity_df$Lth_mean, na.rm=TRUE)
mean(wcommunity_df$Lth_mean_global, na.rm=TRUE)
mean(wcommunity_df$Wmean_Lth, na.rm=TRUE)
mean(wcommunity_df$Wmean_global_Lth, na.rm=TRUE)

mean(wcommunity_df$LDMC, na.rm=TRUE)
mean(wcommunity_df$LDMC_mean, na.rm=TRUE)
mean(wcommunity_df$LDMC_mean_global, na.rm=TRUE)
mean(wcommunity_df$Wmean_LDMC, na.rm=TRUE)
mean(wcommunity_df$Wmean_global_LDMC, na.rm=TRUE)



SLA_raw_Temp <- lmer(SLA ~ Temp + (1|Site), data= wcommunity_df)
summary(SLA_raw_Temp)

SLA_raw_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

newdata_Temp<-expand.grid( Temp=seq(5.5, 11, length=1000))

newdata_Temp$fit <- predict(SLA_raw_Temp, re.form=NA, newdata=newdata_Temp)


pLot1<-wcommunity_df%>%
  select(Site, Species, Temp, SLA)%>%
  unique()%>%
  ggplot(aes(x=Temp, y=SLA))+
  geom_jitter(width=0.1)+
  geom_line(aes( x = Temp, y = fit), data=newdata_Temp, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#FF6666")+
  labs( y="SLA (cm2/g)", x="")+
  theme_minimal()


var_com<-wcommunity_df%>%
  group_by(Site, Species)%>%
  mutate(var_SLA = var(SLA))%>%
  mutate(var_height = var(Height))%>%
  select(Site, Species, Temp, var_SLA, var_height)%>%
  unique()

try <- lmer(var_SLA ~ Temp + (1|Site), data= var_com)
summary(try)

try%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

newdata_try<-expand.grid( Temp=seq(5.5, 11, length=1000))

newdata_try$fit <- predict(try, re.form=NA, newdata=newdata_try)

ggplot(aes(x=Temp, y=var_SLA), data=var_com)+
  geom_jitter(width=0.1)+
  geom_line(aes( x = Temp, y = fit), data=newdata_try, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#FF6666")+
  labs( y="SLA (cm2/g)", x="")+
  theme_minimal()

try2 <- lmer(var_height ~ Temp + (1|Site), data= var_com)
summary(try2)

try2%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

newdata_try<-expand.grid( Temp=seq(5.5, 11, length=1000))

newdata_try$fit2 <- predict(try2, re.form=NA, newdata=newdata_try)

ggplot(aes(x=Temp, y=var_height), data=var_com)+
  geom_jitter(width=0.1)+
  geom_line(aes( x = Temp, y = fit2), data=newdata_try, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#FF6666")+
  labs( y="SLA (cm2/g)", x="")+
  theme_minimal()



SLA_raw_precip <- lmer(SLA ~Precip + (1|Site), data= wcommunity_df)
summary(SLA_raw_precip)

SLA_raw_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

newdata_Precip<-expand.grid(Precip=seq(500, 3100, length=1000))

newdata_Precip$fit <- predict(SLA_raw_precip, re.form=NA, newdata=newdata_Precip)

pLot2<-wcommunity_df%>%
  select(Site, Species, Precip, SLA)%>%
  unique()%>%
  ggplot(aes(x=Precip, y=SLA))+
  geom_jitter(width=30)+
  geom_line(aes( x = Precip, y = fit), data=newdata_Precip, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#99CCFF", linetype=2)+
  labs(x="", y="")+
  theme_minimal()

### LDMC ###

LDMC_raw_Temp <- lmer(LDMC ~Temp + (1|Site), data= wcommunity_df)
summary(LDMC_raw_Temp)

LDMC_raw_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))


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

LDMC_raw_precip <- lmer(LDMC ~Precip + (1|Site), data= wcommunity_df)
summary(LDMC_raw_precip)

LDMC_raw_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

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

Lth_raw_Temp <- lmer(Lth_ave ~Temp + (1|Site), data= wcommunity_df)
summary(Lth_raw_Temp)

Lth_raw_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

newdata_Temp$fit_Lth <- predict(Lth_raw_Temp, re.form=NA, newdata=newdata_Temp)

pLot5<-wcommunity_df%>%
  select(Site, Species, Temp, Lth_ave)%>%
  unique()%>%
  ggplot(aes(x=Temp, y=Lth_ave))+
  geom_jitter(width=0.1)+
  geom_line(aes( x = Temp, y = fit_Lth), data=newdata_Temp, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#FF6666", linetype = 2)+
  labs(y="Leaf thickness (mm)", x="")+
  theme_minimal()


Lth_raw_precip <- lmer(Lth_ave ~Precip + (1|Site), data= wcommunity_df)
summary(Lth_raw_precip)

Lth_raw_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

newdata_Precip$fit_Lth <- predict(Lth_raw_precip, re.form=NA, newdata=newdata_Precip)

pLot6<-wcommunity_df%>%
  select(Site, Species, Precip, Lth_ave)%>%
  unique()%>%
  ggplot(aes(x=Precip, y=Lth_ave))+
  geom_jitter(width=30)+
  geom_line(aes( x = Precip, y = fit_Lth), data=newdata_Precip, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#99CCFF", linetype = 2)+
  labs(x="", y="")+
  theme_minimal()

#### CN ratio ####

CN_raw_Temp <- lmer(log(CN.ratio) ~Temp + (1|Site), data= wcommunity_df)
summary(CN_raw_Temp)
plot(CN_raw_Temp)

CN_raw_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

newdata_Temp$fit_CN <- predict(CN_raw_Temp, re.form=NA, newdata=newdata_Temp)

pLot7<-wcommunity_df%>%
  select(Site, Species, Temp, CN.ratio)%>%
  unique()%>%
  ggplot(aes(x=Temp, y=log(CN.ratio)))+
  geom_jitter(width=0.1)+
  geom_line(aes( x = Temp, y = fit_CN), data=newdata_Temp, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#FF6666", linetype = 2)+
  labs(y="C/N ratio", x="")+
  theme_minimal()

CN_raw_precip <- lmer(log(CN.ratio) ~Precip + (1|Site), data= wcommunity_df)
summary(CN_raw_precip)
plot(CN_raw_precip)

CN_raw_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

newdata_Precip$fit_CN <- predict(CN_raw_precip, re.form=NA, newdata=newdata_Precip)

pLot8<-wcommunity_df%>%
  select(Site, Species, Precip, CN.ratio)%>%
  unique()%>%
  ggplot(aes(x=Precip, y=log(CN.ratio)))+
  geom_jitter(width=30)+
  geom_line(aes( x = Precip, y = fit_CN), data=newdata_Precip, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#99CCFF", linetype = 2)+
  labs(x="", y="")+
  theme_minimal()

#### Height ####

Forbs<-wcommunity_df%>%
  filter(functionalGroup=="forb")

H_Forb_raw_Temp <- lmer(log(Height) ~Temp + (1|Site), data= Forbs)
summary(H_Forb_raw_Temp)

H_Forb_raw_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))


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

H_Forb_raw_Precip <- lmer(log(Height) ~Precip + (1|Site), data= Forbs)
summary(H_Forb_raw_Precip)

H_Forb_raw_Precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

newdata_Precip$fit_H_Forb <- predict(H_Forb_raw_Precip, re.form=NA, newdata=newdata_Precip)

pLot10<-wcommunity_df%>%
  filter(functionalGroup=="forb")%>%
  select(Site, Species, Precip, Height)%>%
  unique()%>%
  ggplot(aes(x=Precip, y=log(Height)))+
  geom_jitter(width=30)+
  geom_line(aes( x = Precip, y = fit_H_Forb), data=newdata_Precip, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#99CCFF", linetype = 2)+
  labs(x="", y="")+
  theme_minimal()

Graminoids<-wcommunity_df%>%
  filter(functionalGroup=="graminoid")

H_Gram_raw_Temp <- lmer(log(Height) ~Temp + (1|Site), data= Graminoids)
summary(H_Gram_raw_Temp)

H_Gram_raw_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

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

H_Gram_raw_Precip <- lmer(log(Height) ~Precip + (1|Site), data= Graminoids)
summary(H_Gram_raw_Precip)

H_Gram_raw_Precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

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


grid.arrange(pLot1, pLot2, pLot3, pLot4, pLot5, pLot6, pLot7, pLot8, pLot9, pLot10, pLot11, pLot12, ncol=2)

######################################### CWM ########################################

#### SLA ####

SLA_CWM_Temp <- lmer(Wmean_global_SLA ~Temp + (1|Site), data= wcommunity_df)
summary(SLA_CWM_Temp)

SLA_CWM_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

newdata_Temp_CWM<-expand.grid( Temp=seq(5.5, 11, length=1000))

newdata_Temp_CWM$fit_SLA <- predict(SLA_CWM_Temp, re.form=NA, newdata=newdata_Temp_CWM)


CWM_1<-wcommunity_df%>%
  select(Site, Species, Temp, Wmean_global_SLA)%>%
  unique()%>%
  ggplot(aes(x=Temp, y=Wmean_global_SLA))+
  geom_point()+
  geom_line(aes( x = Temp, y = fit_SLA), data=newdata_Temp_CWM, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#FF6666",linetype=1)+
  labs( y="SLA (cm2/g)", x="")+
  theme_minimal()


SLA_CWM_precip <- lmer(Wmean_global_SLA ~Precip + (1|Site), data= wcommunity_df)
summary(SLA_CWM_precip)

SLA_CWM_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

newdata_Precip_CWM<-expand.grid(Precip=seq(500, 3100, length=1000))

newdata_Precip_CWM$fit_SLA <- predict(SLA_CWM_precip, re.form=NA, newdata=newdata_Precip_CWM)

CWM_2<-wcommunity_df%>%
  select(Site, Species, Precip, Wmean_global_SLA)%>%
  unique()%>%
  ggplot(aes(x=Precip, y=Wmean_global_SLA))+
  geom_point()+
  geom_line(aes( x = Precip, y = fit_SLA), data=newdata_Precip_CWM, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#99CCFF", linetype = 2)+
  labs(x="", y="")+
  theme_minimal()

#### LDMC ####

LDMC_CWM_Temp <- lmer(Wmean_global_LDMC ~Temp + (1|Site), data= wcommunity_df)
summary(LDMC_CWM_Temp)

LDMC_CWM_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

newdata_Temp_CWM$fit_LDMC <- predict(LDMC_CWM_Temp, re.form=NA, newdata=newdata_Temp_CWM)


CWM_3<-wcommunity_df%>%
  select(Site, Species, Temp, Wmean_global_LDMC)%>%
  unique()%>%
  ggplot(aes(x=Temp, y=Wmean_global_LDMC))+
  geom_point()+
  geom_line(aes( x = Temp, y = fit_LDMC), data=newdata_Temp_CWM, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#FF6666", linetype = 2)+
  labs( y="LDMC", x="")+
  theme_minimal()


LDMC_CWM_precip <- lmer(Wmean_global_LDMC ~Precip + (1|Site), data= wcommunity_df)
summary(LDMC_CWM_precip)

LDMC_CWM_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

newdata_Precip_CWM$fit_LDMC <- predict(LDMC_CWM_precip, re.form=NA, newdata=newdata_Precip_CWM)

CWM_4<-wcommunity_df%>%
  select(Site, Species, Precip, Wmean_global_LDMC)%>%
  unique()%>%
  ggplot(aes(x=Precip, y=Wmean_global_LDMC))+
  geom_point()+
  geom_line(aes( x = Precip, y = fit_LDMC), data=newdata_Precip_CWM, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#99CCFF", linetype = 2)+
  labs(x="", y="")+
  theme_minimal()


#### Leaf thickness

Lth_CWM_Temp <- lmer(Wmean_global_Lth ~Temp + (1|Site), data= wcommunity_df)
summary(Lth_CWM_Temp)

Lth_CWM_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

newdata_Temp_CWM$fit_Lth <- predict(Lth_CWM_Temp, re.form=NA, newdata=newdata_Temp_CWM)


CWM_5<-wcommunity_df%>%
  select(Site, Species, Temp, Wmean_global_Lth)%>%
  unique()%>%
  ggplot(aes(x=Temp, y=Wmean_global_Lth))+
  geom_point()+
  geom_line(aes( x = Temp, y = fit_Lth), data=newdata_Temp_CWM, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#FF6666", linetype = 2)+
  labs( y="Leaf thickness (mm)", x="")+
  theme_minimal()


Lth_CWM_precip <- lmer(Wmean_global_Lth ~Precip + (1|Site), data= wcommunity_df)
summary(Lth_CWM_precip)

Lth_CWM_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

newdata_Precip_CWM$fit_Lth <- predict(Lth_CWM_precip, re.form=NA, newdata=newdata_Precip_CWM)

CWM_6<-wcommunity_df%>%
  select(Site, Species, Precip, Wmean_global_Lth)%>%
  unique()%>%
  ggplot(aes(x=Precip, y=Wmean_global_Lth))+
  geom_point()+
  geom_line(aes( x = Precip, y = fit_Lth), data=newdata_Precip_CWM, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#99CCFF", linetype = 2)+
  labs(x="", y="")+
  theme_minimal()

#### CN ratio ####

CN_CWM_Temp <- lmer(log(Wmean_global_CN) ~Temp + (1|Site), data= wcommunity_df)
summary(CN_CWM_Temp)
plot(CN_CWM_Temp)

CN_CWM_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))


newdata_Temp_CWM$fit_CN <- predict(CN_CWM_Temp, re.form=NA, newdata=newdata_Temp_CWM)


CWM_7<-wcommunity_df%>%
  select(Site, Species, Temp, Wmean_global_CN)%>%
  unique()%>%
  ggplot(aes(x=Temp, y=log(Wmean_global_CN)))+
  geom_point()+
  geom_line(aes( x = Temp, y = fit_CN), data=newdata_Temp_CWM, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#FF6666", linetype = 2)+
  labs( y="C/N ratio", x="")+
  theme_minimal()


CN_CWM_precip <- lmer(log(Wmean_global_CN) ~Precip + (1|Site), data= wcommunity_df)
summary(CN_CWM_precip)

CN_CWM_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

newdata_Precip_CWM$fit_CN <- predict(CN_CWM_precip, re.form=NA, newdata=newdata_Precip_CWM)

CWM_8<-wcommunity_df%>%
  select(Site, Species, Precip, Wmean_global_CN)%>%
  unique()%>%
  ggplot(aes(x=Precip, y=log(Wmean_global_CN)))+
  geom_point()+
  geom_line(aes( x = Precip, y = fit_CN), data=newdata_Precip_CWM, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#99CCFF", linetype = 2)+
  labs(x="", y="")+
  theme_minimal()

#### Height ####

H_Forb_CWM_Temp <- lmer(log(Wmean_global_Height) ~Temp + (1|Site), data= Forbs)
summary(H_Forb_CWM_Temp)

H_Forb_CWM_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

newdata_Temp_CWM$fit_H_Forb <- predict(H_Forb_CWM_Temp, re.form=NA, newdata=newdata_Temp_CWM)


CWM_9<-Forbs%>%
  select(Site, Species, Temp, Wmean_global_Height)%>%
  unique()%>%
  ggplot(aes(x=Temp, y=log(Wmean_global_Height)))+
  geom_point()+
  geom_line(aes( x = Temp, y = fit_H_Forb), data=newdata_Temp_CWM, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#FF6666")+
  labs( y="Height f (log)", x="")+
  theme_minimal()

H_Forb_CWM_precip <- lmer(log(Wmean_global_Height) ~Precip + (1|Site), data= Forbs)
summary(H_Forb_CWM_precip)

H_Forb_CWM_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

newdata_Precip_CWM$fit_H_Forb <- predict(H_Forb_CWM_precip, re.form=NA, newdata=newdata_Precip_CWM)

CWM_10<-Forbs%>%
  select(Site, Species, Precip, Wmean_global_Height)%>%
  unique()%>%
  ggplot(aes(x=Precip, y=log(Wmean_global_Height)))+
  geom_point()+
  geom_line(aes( x = Precip, y = fit_H_Forb), data=newdata_Precip_CWM, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#99CCFF", linetype = 2)+
  labs(x="", y="")+
  theme_minimal()


H_Gram_CWM_Temp <- lmer(log(Wmean_global_Height) ~Temp + (1|Site), data= Graminoids)
summary(H_Gram_CWM_Temp)

H_Gram_CWM_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))


newdata_Temp_CWM$fit_H_Gram <- predict(H_Gram_CWM_Temp, re.form=NA, newdata=newdata_Temp_CWM)


CWM_11<-Graminoids%>%
  select(Site, Species, Temp, Wmean_global_Height)%>%
  unique()%>%
  ggplot(aes(x=Temp, y=log(Wmean_global_Height)))+
  geom_point()+
  geom_line(aes( x = Temp, y = fit_H_Gram), data=newdata_Temp_CWM, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#FF6666")+
  labs( y="Height f (log)", x="")+
  theme_minimal()



H_Gram_CWM_precip <- lmer(log(Wmean_global_Height) ~Precip + (1|Site), data= Graminoids)
summary(H_Gram_CWM_precip)

H_Gram_CWM_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

newdata_Precip_CWM$fit_H_Gram <- predict(H_Gram_CWM_precip, re.form=NA, newdata=newdata_Precip_CWM)

CWM_12<-Graminoids%>%
  select(Site, Species, Precip, Wmean_global_Height)%>%
  unique()%>%
  ggplot(aes(x=Precip, y=log(Wmean_global_Height)))+
  geom_point()+
  geom_line(aes( x = Precip, y = fit_H_Gram), data=newdata_Precip_CWM, size = 1, inherit.aes = FALSE, show.legend = FALSE, color="#99CCFF", linetype = 2)+
  labs(x="", y="")+
  theme_minimal()

grid.arrange(CWM_1, CWM_2, CWM_3, CWM_4, CWM_5, CWM_6, CWM_7, CWM_8, CWM_9, CWM_10, CWM_11, CWM_12, ncol=2)


########## Specific mean, fixed mean and spcific CWM #########

### SLA ###

SLA_specific_mean_Temp <- lmer(SLA_mean ~ Temp + (1|Site), data= wcommunity_df)
summary(SLA_specific_mean_Temp)

SLA_specific_mean_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

SLA_specific_mean_precip <- lmer(SLA_mean ~Precip + (1|Site), data= wcommunity_df)
summary(SLA_specific_mean_precip)

SLA_specific_mean_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

### LDMC ###

LDMC_specific_mean_Temp <- lmer(LDMC_mean ~ Temp + (1|Site), data= wcommunity_df)
summary(LDMC_specific_mean_Temp)

LDMC_specific_mean_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

LDMC_specific_mean_precip <- lmer(LDMC_mean ~Precip + (1|Site), data= wcommunity_df)
summary(LDMC_specific_mean_precip)

LDMC_specific_mean_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

### Leaf thickness ###

Lth_specific_mean_Temp <- lmer(Lth_mean ~ Temp + (1|Site), data= wcommunity_df)
summary(Lth_specific_mean_Temp)

Lth_specific_mean_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

Lth_specific_mean_precip <- lmer(Lth_mean ~Precip + (1|Site), data= wcommunity_df)
summary(Lth_specific_mean_precip)

Lth_specific_mean_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

### C/N ratio ###

CN_specific_mean_Temp <- lmer(log(CN_ratio_mean) ~ Temp + (1|Site), data= wcommunity_df)
summary(CN_specific_mean_Temp)

CN_specific_mean_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

CN_specific_mean_precip <- lmer(log(CN_ratio_mean) ~Precip + (1|Site), data= wcommunity_df)
summary(CN_specific_mean_precip)

CN_specific_mean_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

### Height in forbs ###

H_Forb_specific_mean_temp <- lmer(log(Height_mean) ~Temp + (1|Site), data= Forbs)
summary(H_Forb_specific_mean_temp)

H_Forb_specific_mean_temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

H_Forb_specific_mean_precip <- lmer(log(Height_mean) ~Precip + (1|Site), data= Forbs)
summary(H_Forb_specific_mean_precip)

H_Forb_specific_mean_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

### Height i graiminois ###

H_Gram_specific_mean_Temp <- lmer(log(Height_mean) ~Temp + (1|Site), data= Graminoids)
summary(H_Gram_specific_mean_Temp)

H_Gram_specific_mean_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

H_Gram_specific_mean_precip <- lmer(log(Height_mean) ~Precip + (1|Site), data= Graminoids)
summary(H_Gram_specific_mean_precip)

H_Gram_specific_mean_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))


########## Fixed mean #########

### SLA ###

SLA_fixed_mean_Temp <- lmer(SLA_mean_global ~ Temp + (1|Site), data= wcommunity_df)
summary(SLA_fixed_mean_Temp)

SLA_fixed_mean_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

SLA_fixed_mean_precip <- lmer(SLA_mean_global ~Precip + (1|Site), data= wcommunity_df)
summary(SLA_fixed_mean_precip)

SLA_fixed_mean_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

### LDMC ###

LDMC_fixed_mean_Temp <- lmer(LDMC_mean_global ~ Temp + (1|Site), data= wcommunity_df)
summary(LDMC_fixed_mean_Temp)

LDMC_fixed_mean_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

LDMC_fixed_mean_precip <- lmer(LDMC_mean_global ~Precip + (1|Site), data= wcommunity_df)
summary(LDMC_fixed_mean_precip)

LDMC_fixed_mean_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

### Leaf thickness ###

Lth_fixed_mean_Temp <- lmer(Lth_mean_global ~ Temp + (1|Site), data= wcommunity_df)
summary(Lth_fixed_mean_Temp)

Lth_fixed_mean_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

Lth_fixed_mean_precip <- lmer(Lth_mean_global ~Precip + (1|Site), data= wcommunity_df)
summary(Lth_fixed_mean_precip)

Lth_fixed_mean_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

### C/N ratio ###

CN_fixed_mean_Temp <- lmer(log(CN_ratio_mean_global) ~ Temp + (1|Site), data= wcommunity_df)
summary(CN_fixed_mean_Temp)

CN_fixed_mean_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

CN_fixed_mean_precip <- lmer(log(CN_ratio_mean_global) ~Precip + (1|Site), data= wcommunity_df)
summary(CN_fixed_mean_precip)

CN_fixed_mean_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

### Height in forbs ###

H_Forb_fixed_mean_temp <- lmer(log(Height_mean_global) ~Temp + (1|Site), data= Forbs)
summary(H_Forb_fixed_mean_temp)

H_Forb_fixed_mean_temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

H_Forb_fixed_mean_precip <- lmer(log(Height_mean_global) ~Precip + (1|Site), data= Forbs)
summary(H_Forb_fixed_mean_precip)

H_Forb_fixed_mean_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

### Height i graiminois ###

H_Gram_fixed_mean_Temp <- lmer(log(Height_mean_global) ~Temp + (1|Site), data= Graminoids)
summary(H_Gram_fixed_mean_Temp)

H_Gram_fixed_mean_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

H_Gram_fixed_mean_precip <- lmer(log(Height_mean_global) ~Precip + (1|Site), data= Graminoids)
summary(H_Gram_fixed_mean_precip)

H_Gram_fixed_mean_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))


########## Specific community weighted means #########

### SLA ###

SLA_specific_CWM_Temp <- lmer(Wmean_SLA ~ Temp + (1|Site), data= wcommunity_df)
summary(SLA_specific_CWM_Temp)

SLA_specific_CWM_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

SLA_specific_CWM_precip <- lmer(Wmean_SLA ~Precip + (1|Site), data= wcommunity_df)
summary(SLA_specific_CWM_precip)

SLA_specific_CWM_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

### LDMC ###

LDMC_specific_CWM_Temp <- lmer(Wmean_LDMC ~ Temp + (1|Site), data= wcommunity_df)
summary(LDMC_specific_CWM_Temp)

LDMC_specific_CWM_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

LDMC_specific_CWM_precip <- lmer(Wmean_LDMC ~Precip + (1|Site), data= wcommunity_df)
summary(LDMC_specific_CWM_precip)

LDMC_specific_CWM_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

### Leaf thickness ###

Lth_specific_CWM_Temp <- lmer(Wmean_Lth ~ Temp + (1|Site), data= wcommunity_df)
summary(Lth_specific_CWM_Temp)

Lth_specific_CWM_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

Lth_specific_CWM_precip <- lmer(Wmean_Lth ~Precip + (1|Site), data= wcommunity_df)
summary(Lth_specific_CWM_precip)

Lth_specific_CWM_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

### C/N ratio ###

CN_specific_CWM_Temp <- lmer(log(Wmean_CN) ~ Temp + (1|Site), data= wcommunity_df)
summary(CN_specific_CWM_Temp)

CN_specific_CWM_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

CN_specific_CWM_precip <- lmer(log(Wmean_CN) ~Precip + (1|Site), data= wcommunity_df)
summary(CN_specific_CWM_precip)

CN_specific_CWM_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

### Height in forbs ###

H_Forb_specific_CWM_temp <- lmer(log(Wmean_Height) ~Temp + (1|Site), data= Forbs)
summary(H_Forb_specific_CWM_temp)

H_Forb_specific_CWM_temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

H_Forb_specific_CWM_precip <- lmer(log(Wmean_Height) ~Precip + (1|Site), data= Forbs)
summary(H_Forb_specific_CWM_precip)

H_Forb_specific_CWM_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

### Height i graiminois ###

H_Gram_specific_CWM_Temp <- lmer(log(Wmean_Height) ~Temp + (1|Site), data= Graminoids)
summary(H_Gram_specific_CWM_Temp)

H_Gram_specific_CWM_Temp%>%
  tidy()%>%
  filter(term == "Temp")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

H_Gram_specific_CWM_precip <- lmer(log(Wmean_Height) ~Precip + (1|Site), data= Graminoids)
summary(H_Gram_specific_CWM_precip)

H_Gram_specific_CWM_precip%>%
  tidy()%>%
  filter(term == "Precip")%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))
