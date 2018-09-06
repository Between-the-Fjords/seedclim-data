# Lorah Patterson - Variance partitioning of 2016 China leaf trait data

### lme and varcomp analysis ###

library("ape")
library("nlme")


### Variance partitioning by taxonomic level: order, family, genus, species, within species ###

#varSLA <- lmer(SLA~1 + (1|Order/Family/Genus/species), data=wcommunity_df, na.action = na.omit, REML = TRUE)
#varcomp.mer(varSLA)

varSLA <- lme(SLA~1, random= ~1|Order/Family/Genus/Species, data=wcommunity_df, na.action = na.omit)

SLA_sys<-varcomp(varSLA, scale=TRUE)


LDMC_df<-wcommunity_df%>%
  filter(!LDMC>1)

varLDMC <- lme(LDMC~1, random= ~1|Order/Family/Genus/Species, data=LDMC_df, na.action = na.omit)

LDMC_sys<-varcomp(varLDMC, scale=TRUE)


varLth <- lme(Lth_ave~1, random= ~1|Order/Family/Genus/Species, data=wcommunity_df, na.action = na.omit)

Lth_sys <- varcomp(varLth, scale=TRUE)


## Do I want to log transform here aswell?
varCN <- lme(CN.ratio~1, random= ~1|Order/Family/Genus/Species, data=wcommunity_df, na.action = na.omit)

CN_sys <- varcomp(varCN, scale=TRUE)
#plot(CN_sys)


Height_graminoid<-wcommunity_df%>%
  filter(Order=="Poales")

Height_forbs<-wcommunity_df%>%
  filter(!Order=="Poales")

varHeight_gra <- lme(Height~1, random= ~1|Order/Family/Genus/Species, data=Height_graminoid, na.action = na.omit)

varHeight_forb <- lme(Height~1, random= ~1|Order/Family/Genus/Species, data=Height_forbs, na.action = na.omit)

Height_gra_sys <- varcomp(varHeight_gra, scale=TRUE)
Height_forb_sys <- varcomp(varHeight_forb, scale=TRUE)

#plot(Height_gra_sys)
#plot(Height_forb_sys)


SLA_vec<-as.vector(SLA_sys)
LDMC_vec<-as.vector(LDMC_sys)
Lth_vec<-as.vector(Lth_sys)
CN_vec<-as.vector(CN_sys)
Height_gra_vec<-as.vector(Height_gra_sys)
Height_forb_vec<-as.vector(Height_forb_sys)

Variance<-c(SLA_vec, LDMC_vec, Lth_vec, CN_vec, Height_gra_vec, Height_forb_vec)

Traits<-c("SLA","SLA","SLA","SLA","SLA", "LDMC","LDMC","LDMC","LDMC", "LDMC", "Lth","Lth","Lth","Lth", "Lth", "CN","CN","CN","CN","CN", "Height_gra","Height_gra","Height_gra","Height_gra","Height_gra", "Height_forb", "Height_forb","Height_forb", "Height_forb","Height_forb")

Systematics<-c("aOrder", "bFamily", "cGenus", "dSpecies", "eWithin","aOrder", "bFamily", "cGenus", "dSpecies", "eWithin", "aOrder", "bFamily", "cGenus", "dSpecies", "eWithin", "aOrder", "bFamily", "cGenus", "dSpecies","eWithin","aOrder", "bFamily", "cGenus", "dSpecies", "eWithin" ,"aOrder", "bFamily", "cGenus", "dSpecies", "eWithin")


Col_vec<-(c("Traits", "Variance", "Systematics"))


df<- data.frame(matrix(vector(), 30, 3,
       dimnames=list(c(), Col_vec)),stringsAsFactors=F)


df[,2]<-Variance
df[,1]<-Traits
df[,3]<-Systematics

ggplot(data=df, aes(x=Traits, y=Variance, fill=Systematics))+
  geom_bar(stat="identity")+
  theme_minimal()+
  scale_fill_manual(labels= c("Order", "Family", "Genus", "Species", "Within" ), values=rev(c("#FF6666", "#FFCC33", "#FFFF99", "#99FF99","#99CCFF")))+
  scale_x_discrete(labels=c("C/N ratio", "Height forbs", "Height graminoids", "LDMC", "Leaf thickness", "SLA"))+
  theme(axis.text.x= element_text(angle=90))+
  labs(x="", y="Proportion of variance", fill="")


#### Variance partitioning on spacial level ####


varSLA_spa <- lme(SLA~T_level, random= ~1|Site, data=wcommunity_df, na.action = na.omit)
summary(varSLA_spa)

SLA_Fixed_var <- var(predict(varSLA_spa, level = 0))

#library("piecewiseSEM")
#sem.model.fits(varSLA_spa)
#Checking that this works and gives the same numbers, and it did.


SLA_spa <- varcomp(varSLA_spa)
SLA_spa

SLA_total_var <- (sum(as.numeric(SLA_spa)))+SLA_Fixed_var

# Variation within site
SLA_var_1<-as.numeric(SLA_spa["Within"])/SLA_total_var

#Variation between sites
SLA_var_2<-as.numeric(SLA_spa["Site"])/SLA_total_var

#Variation between temperature level
SLA_var_3<-SLA_Fixed_var/SLA_total_var

# Total
SLA_var_1+SLA_var_2+SLA_var_3

#### LDMC ####

varLDMC_spa <- lme(LDMC~T_level, random= ~1|Site, data=wcommunity_df, na.action = na.omit)
summary(varLDMC_spa)


LDMC_Fixed_var <- var(predict(varLDMC_spa, level = 0))

LDMC_spa <- varcomp(varLDMC_spa)
LDMC_spa


LDMC_total_var<-(sum(as.numeric(LDMC_spa)))+LDMC_Fixed_var

# Variation within site
LDMC_var_1<-as.numeric(LDMC_spa["Within"])/LDMC_total_var

#Variation between sites
LDMC_var_2<-as.numeric(LDMC_spa["Site"])/LDMC_total_var

#Variation between temperature level
LDMC_var_3<-LDMC_Fixed_var/LDMC_total_var

# Total
LDMC_var_1+LDMC_var_2+LDMC_var_3


### Leaf thickness ###

varLth_spa <- lme(Lth_ave~T_level, random= ~1|Site, data=wcommunity_df, na.action = na.omit)
summary(varLth_spa)

Lth_Fixed_var<-var(predict(varLth_spa, level = 0))
#sem.model.fits(varLth_spa)

Lth_spa <- varcomp(varLth_spa)
Lth_spa


Lth_total_var<-(sum(as.numeric(Lth_spa)))+Lth_Fixed_var

# Variation within site
Lth_var_1<-as.numeric(Lth_spa["Within"])/Lth_total_var

#Variation between sites
Lth_var_2<-as.numeric(Lth_spa["Site"])/Lth_total_var

#Variation between temperature level
Lth_var_3<-Lth_Fixed_var/Lth_total_var

# Total
Lth_var_1+Lth_var_2+Lth_var_3


### C/N ratio ###

varCN_spa <- lme(CN.ratio~T_level, random= ~1|Site, data=wcommunity_df, na.action = na.omit)
summary(varCN_spa)

CN_Fixed_var<-var(predict(varCN_spa, level = 0))
sem.model.fits(varCN_spa)

CN_spa <- varcomp(varCN_spa)
CN_spa

CN_total_var<-(sum(as.numeric(CN_spa)))+CN_Fixed_var

# Variation within site
CN_var_1<-as.numeric(CN_spa["Within"])/CN_total_var

#Variation between sites
CN_var_2<-as.numeric(CN_spa["Site"])/CN_total_var

#Variation between temperature level
CN_var_3<-CN_Fixed_var/CN_total_var

# Total
CN_var_1+CN_var_2+CN_var_3

### Height graminoid ###




Height_graminoid<-wcommunity_df%>%
  filter(Order=="Poales")

Height_forbs<-wcommunity_df%>%
  filter(!Order=="Poales")



varHeight_gra_spa <- lme(log(Height)~T_level, random= ~1|Site, data=Height_graminoid, na.action = na.omit)
summary(varHeight_gra_spa)

HG_Fixed_var<-var(predict(varHeight_gra_spa, level = 0))

H_gram_spa <- varcomp(varHeight_gra_spa)
H_gram_spa

HG_total_var<-(sum(as.numeric(H_gram_spa)))+HG_Fixed_var

# Variation within site
HG_var_1<-as.numeric(H_gram_spa["Within"]/HG_total_var)

#Variation between sites
HG_var_2<-as.numeric(H_gram_spa["Site"]/HG_total_var)

#Variation between temperature level
HG_var_3<-HG_Fixed_var/HG_total_var

# Total
HG_var_1+HG_var_2+HG_var_3


### Height forbs ###

varHeight_forb_spa <- lme(log(Height)~T_level, random= ~1|Site, data=Height_forbs, na.action = na.omit)
summary(varHeight_forb_spa)

HF_Fixed_var<-var(predict(varHeight_forb_spa, level = 0))

H_forb_spa <- varcomp(varHeight_forb_spa)
H_forb_spa

HF_total_var<-(sum(as.numeric(H_forb_spa)))+HF_Fixed_var

# Variation within site
HF_var_1<-as.numeric(H_forb_spa["Within"]/HF_total_var)

#Variation between sites
HF_var_2<-as.numeric(H_forb_spa["Site"]/HF_total_var)

#Variation between temperature level
HF_var_3<-HF_Fixed_var/HF_total_var

# Total
HF_var_1+HF_var_2+HF_var_3


Variance2<-c(SLA_var_3, SLA_var_2, SLA_var_1, LDMC_var_3, LDMC_var_2, LDMC_var_1, Lth_var_3, Lth_var_2, Lth_var_1, CN_var_3, CN_var_2, CN_var_1, HG_var_3, HG_var_2, HG_var_1, HF_var_3, HF_var_2, HF_var_1)

Traits2<-c("SLA","SLA","SLA", "LDMC","LDMC","LDMC", "Lth","Lth","Lth", "CN","CN","CN", "Height gra","Height gra","Height gra", "Height forb", "Height forb", "Height forb")

Spacial<-c("aTemp_level", "bSite", "cWithin", "aTemp_level", "bSite", "cWithin", "aTemp_level", "bSite", "cWithin", "aTemp_level", "bSite", "cWithin","aTemp_level", "bSite", "cWithin", "aTemp_level", "bSite", "cWithin")

Col_vec<-(c("Traits", "Variance", "Spacial scale"))


spatial<- data.frame(matrix(vector(), 18, 3,
                       dimnames=list(c(), Col_vec)),stringsAsFactors=F)


spatial[,2]<-Variance2
spatial[,1]<-Traits2
spatial[,3]<-Spacial

ggplot(data=spatial, aes(x=Traits, y=Variance, fill=Spacial))+
  geom_bar(stat="identity")+
  theme_minimal()+
  scale_fill_manual(labels = c("Temperature level", "Site", "Within"), values=rev(c("#FF6666","#FFCC33","#99CCFF")))+
  scale_x_discrete(labels=c("C/N ratio", "Height forbs", "Height graminoids", "LDMC", "Leaf thickness", "SLA"))+
  theme(axis.text.x= element_text(angle=90))+
  labs(x="", y="Proportion of variance", fill="")


#### Variance partitioning on spacial level PRECIPITATION ####


varSLA_spa_P <- lme(SLA~P_level, random= ~1|Site, data=wcommunity_df, na.action = na.omit)

SLA_Fixed_var_P <- var(predict(varSLA_spa_P, level = 0))

SLA_spa_P <- varcomp(varSLA_spa_P)

SLA_total_var_P <- (sum(as.numeric(SLA_spa_P)))+SLA_Fixed_var_P

# Variation within site
SLA_var_1_P<-as.numeric(SLA_spa_P["Within"])/SLA_total_var_P

#Variation between sites
SLA_var_2_P<-as.numeric(SLA_spa_P["Site"])/SLA_total_var_P

#Variation between temperature level
SLA_var_3_P<-SLA_Fixed_var_P/SLA_total_var_P

# Total
SLA_var_1_P+SLA_var_2_P+SLA_var_3_P

#### LDMC ####

varLDMC_spa_P <- lme(LDMC~P_level, random= ~1|Site, data=wcommunity_df, na.action = na.omit)
summary(varLDMC_spa_P)


LDMC_Fixed_var_P <- var(predict(varLDMC_spa_P, level = 0))

LDMC_spa_P <- varcomp(varLDMC_spa_P)

LDMC_total_var_P<-(sum(as.numeric(LDMC_spa_P)))+LDMC_Fixed_var_P

# Variation within site
LDMC_var_1_P<-as.numeric(LDMC_spa_P["Within"])/LDMC_total_var_P

#Variation between sites
LDMC_var_2_P<-as.numeric(LDMC_spa_P["Site"])/LDMC_total_var_P

#Variation between temperature level
LDMC_var_3_P<-LDMC_Fixed_var_P/LDMC_total_var_P


### Leaf thickness ###

varLth_spa_P <- lme(Lth_ave~P_level, random= ~1|Site, data=wcommunity_df, na.action = na.omit)
summary(varLth_spa_P)

Lth_Fixed_var_P<-var(predict(varLth_spa_P, level = 0))
#sem.model.fits(varLth_spa)

Lth_spa_P <- varcomp(varLth_spa_P)


Lth_total_var_P<-(sum(as.numeric(Lth_spa_P)))+Lth_Fixed_var_P

# Variation within site
Lth_var_1_P<-as.numeric(Lth_spa_P["Within"])/Lth_total_var_P

#Variation between sites
Lth_var_2_P<-as.numeric(Lth_spa_P["Site"])/Lth_total_var_P

#Variation between temperature level
Lth_var_3_P<-Lth_Fixed_var_P/Lth_total_var_P


### C/N ratio ###

varCN_spa_P <- lme(CN.ratio~P_level, random= ~1|Site, data=wcommunity_df, na.action = na.omit)
summary(varCN_spa_P)

CN_Fixed_var_P<-var(predict(varCN_spa_P, level = 0))
sem.model.fits(varCN_spa_P)

CN_spa_P <- varcomp(varCN_spa_P)

CN_total_var_P<-(sum(as.numeric(CN_spa_P)))+CN_Fixed_var_P

# Variation within site
CN_var_1_P<-as.numeric(CN_spa_P["Within"])/CN_total_var_P

#Variation between sites
CN_var_2_P<-as.numeric(CN_spa_P["Site"])/CN_total_var_P

#Variation between temperature level
CN_var_3_P<-CN_Fixed_var_P/CN_total_var_P


### Height graminoid ###

Height_graminoid<-wcommunity_df%>%
  filter(Order=="Poales")

Height_forbs<-wcommunity_df%>%
  filter(!Order=="Poales")

#varHeight_gra_spa <- lme(Height~T_level, random= ~1|Site, data=Height_graminoid, na.action = na.omit)
#varHeight_forb_spa <- lme(Height~T_level, random= ~1|Site, data=Height_forbs, na.action = na.omit)
#Height_gra_spa <- varcomp(varHeight_gra_spa, scale=TRUE)
#Height_forb_spa <- varcomp(varHeight_forb_spa, scale=TRUE)



varHeight_gra_spa_P <- lme(log(Height)~P_level, random= ~1|Site, data=Height_graminoid, na.action = na.omit)

HG_Fixed_var_P<-var(predict(varHeight_gra_spa_P, level = 0))

H_gram_spa_P <- varcomp(varHeight_gra_spa_P)

HG_total_var_P<-(sum(as.numeric(H_gram_spa_P)))+HG_Fixed_var_P

# Variation within site
HG_var_1_P<-as.numeric(H_gram_spa_P["Within"]/HG_total_var_P)

#Variation between sites
HG_var_2_P<-as.numeric(H_gram_spa_P["Site"]/HG_total_var_P)

#Variation between temperature level
HG_var_3_P<-HG_Fixed_var_P/HG_total_var_P


### Height forbs ###

varHeight_forb_spa_P <- lme(log(Height)~P_level, random= ~1|Site, data=Height_forbs, na.action = na.omit)

HF_Fixed_var_P<-var(predict(varHeight_forb_spa_P, level = 0))

H_forb_spa_P <- varcomp(varHeight_forb_spa_P)
H_forb_spa

HF_total_var_P<-(sum(as.numeric(H_forb_spa_P)))+HF_Fixed_var_P

# Variation within site
HF_var_1_P<-as.numeric(H_forb_spa_P["Within"]/HF_total_var_P)

#Variation between sites
HF_var_2_P<-as.numeric(H_forb_spa_P["Site"]/HF_total_var_P)

#Variation between temperature level
HF_var_3_P<-HF_Fixed_var_P/HF_total_var_P



Variance3<-c(SLA_var_3_P, SLA_var_2_P, SLA_var_1_P, LDMC_var_3_P, LDMC_var_2_P, LDMC_var_1_P, Lth_var_3_P, Lth_var_2_P, Lth_var_1_P, CN_var_3_P, CN_var_2_P, CN_var_1_P, HG_var_3_P, HG_var_2_P, HG_var_1_P, HF_var_3_P, HF_var_2_P, HF_var_1_P)

Traits3<-c("SLA","SLA","SLA", "LDMC","LDMC","LDMC", "Lth","Lth","Lth", "CN","CN","CN", "Height gra","Height gra","Height gra", "Height forb", "Height forb", "Height forb")

Spacial_P<-c("aPrecip_level", "bSite", "cWithin", "aPrecip_level", "bSite", "cWithin", "aPrecip_level", "bSite", "cWithin", "aPrecip_level", "bSite", "cWithin","aPrecip_level", "bSite", "cWithin", "aPrecip_level", "bSite", "cWithin")

Col_vec<-(c("Traits", "Variance", "Spacial scale"))


spatial_P<- data.frame(matrix(vector(), 18, 3,
                            dimnames=list(c(), Col_vec)),stringsAsFactors=F)


spatial_P[,2]<-Variance3
spatial_P[,1]<-Traits3
spatial_P[,3]<-Spacial_P

ggplot(data=spatial_P, aes(x=Traits, y=Variance, fill=Spacial_P))+
  geom_bar(stat="identity")+
  theme_minimal()+
  scale_fill_manual(labels = c("Precipitation level", "Site", "Within"), values=rev(c("#FF6666","#FFCC33","#99CCFF")))+
  scale_x_discrete(labels=c("C/N ratio", "Height forbs", "Height graminoids", "LDMC", "Leaf thickness", "SLA"))+
  theme(axis.text.x= element_text(angle=90))+
  labs(x="", y="Proportion of variance", fill="")


#### Variance analysis from Fran ####

wcommunity_df %>% 
  gather(key=SLA, value=measurement, SLA, SLA_mean, SLA_mean_global)%>% 
  #gather(key=Height, value=measurement, Height, Height_mean, Wmean_Height, Height_mean_global, Wmean_global_Height)%>%
  #gather(key=Leaf_thickness, value=measurement, Lth_ave, Lth_mean, Wmean_Lth, Lth_mean_global, Wmean_global_Lth)%>%
  #gather(key=Leaf_area, value=measurement, Leaf_area, LA_mean, Wmean_LA, LA_mean_global, Wmean_global_LA)%>%
  #gather(key=CN_ratio, value=measurement, CN.ratio, CN_ratio_mean, Wmean_CN, CN_ratio_mean_global, Wmean_global_CN)%>%
  ggplot(aes(measurement, fill = SLA)) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  scale_fill_manual(values=c("#FF0033","#FFCC33", "#FFFFCC"))+
  facet_wrap(~ T_level, scales = "free") +
  labs(x = "SLA", fill = "SLA measurements")
  #scale_x_continuous(limits=c(0,700))+
  #scale_y_continuous(limits=c(0.0000, 0.04))

wcommunity_df %>% 
  gather(key=SLA_com, value=measurement, Wmean_global_SLA, Wmean_SLA)%>% 
  #gather(key=Height, value=measurement, Height, Height_mean, Wmean_Height, Height_mean_global, Wmean_global_Height)%>%
  #gather(key=Leaf_thickness, value=measurement, Lth_ave, Lth_mean, Wmean_Lth, Lth_mean_global, Wmean_global_Lth)%>%
  #gather(key=Leaf_area, value=measurement, Leaf_area, LA_mean, Wmean_LA, LA_mean_global, Wmean_global_LA)%>%
  #gather(key=CN_ratio, value=measurement, CN.ratio, CN_ratio_mean, Wmean_CN, CN_ratio_mean_global, Wmean_global_CN)%>%
  filter(Order=="Poales")%>%
  ggplot(aes(measurement, fill= SLA_com)) +
  geom_density(alpha = 0.5) +
  theme_classic() +
  scale_fill_manual(values=c("#99FF99","#99CCFF"))+
  facet_wrap(~ T_level, scales = "free") +
  labs(x = "C/N ratio", fill = "SLA measurements")+
  scale_x_continuous(limits=c(0,700))+
  scale_y_continuous(limits=c(0.0000, 0.035))

SpecificLeafArea<-wcommunity_df %>%
  rename(Wmean_SLA2_global=Wmean_global_SLA, Wmean_SLA1=Wmean_SLA)%>%
  gather(key=SLA, value=measurement, SLA, SLA_mean, SLA_mean_global, Wmean_SLA1, Wmean_SLA2_global)


ggplot(data=SpecificLeafArea, aes(SLA, measurement)) +
  geom_violin(aes(fill=as.factor(SLA)))+
  geom_boxplot(width=0.1)+
  coord_flip()+
  guides(fill=FALSE)+
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c("#FF6666","#FFCC33", "#FFFF99", "#99FF99","#99CCFF"))+
  stat_summary(fun.y=mean, colour="black", geom="point", 
               shape=42, size=7,show_guide = FALSE)+
  labs(title = "Variation in SLA", y = "Specific leaf area (cm2/g)", x = "")+
  scale_x_discrete(labels=c("Wmean_SLA2_global" = "Fixed CWM", "Wmean_SLA1" = "Specific CWM", "SLA_mean_global" = "Fixed mean", "SLA_mean" = "Specifc mean", "SLA" = "All observaions"))
  #+facet_wrap(~ Site, scales = "free")


LeafDryMatterContent<-wcommunity_df %>%
  filter(!LDMC>1)%>%
  rename(Wmean_LDMC2_global=Wmean_global_LDMC, Wmean_LDMC1=Wmean_LDMC)%>%
  gather(key=LDMC, value=measurement, LDMC, LDMC_mean, LDMC_mean_global, Wmean_LDMC1, Wmean_LDMC2_global)


ggplot(data=LeafDryMatterContent, aes(LDMC, measurement)) +
  geom_violin(aes(fill=as.factor(LDMC)))+
  geom_boxplot(width=0.1)+
  coord_flip()+
  guides(fill=FALSE)+
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c("#FF6666","#FFCC33", "#FFFF99", "#99FF99","#99CCFF"))+
  stat_summary(fun.y=mean, colour="black", geom="point", 
               shape=42, size=7,show_guide = FALSE)+
  labs(title = "Variation in LDMC", y = "Leaf dry matter content", x = "")+
  scale_x_discrete(labels=c("Wmean_LDMC2_global" = "Fixed CWM", "Wmean_LDMC1" = "Specific CWM", "LDMC_mean_global" = "Fixed mean", "LDMC_mean" = "Specific mean", "LDMC" = "All observations"))



CarbonNitrogenRatio<-wcommunity_df %>%
  rename(Wmean_CN2_global=Wmean_global_CN, Wmean_CN1=Wmean_CN)%>%
  gather(key=CN_ratio, value=measurement, CN.ratio, CN_ratio_mean, CN_ratio_mean_global, Wmean_CN1, Wmean_CN2_global)


ggplot(data=CarbonNitrogenRatio, aes(CN_ratio, measurement)) +
  geom_violin(aes(fill=as.factor(CN_ratio)))+
  geom_boxplot(width=0.1)+
  coord_flip()+
  guides(fill=FALSE)+
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c("#FF6666","#FFCC33", "#FFFF99", "#99FF99","#99CCFF"))+
  stat_summary(fun.y=mean, colour="black", geom="point", 
               shape=42, size=7,show_guide = FALSE)+
  labs(title = "Variation in C/N ratio", y = "C/N ratio", x = "")+
  scale_x_discrete(labels=c("Wmean_CN2_global" = "Fixed CWM", "Wmean_CN1" = "Specific CWM", "CN_ratio_mean_global" = "Fixed mean", "CN_ratio_mean" = "Specific mean", "CN.ratio" = "All observations"))

LeafThickness<-wcommunity_df %>%
  rename(Wmean_Lth2_global=Wmean_global_Lth, Wmean_Lth1=Wmean_Lth)%>%
  gather(key=Lth, value=measurement, Lth_ave, Lth_mean, Lth_mean_global,Wmean_Lth1, Wmean_Lth2_global)


ggplot(data=LeafThickness, aes(Lth, measurement)) +
  geom_violin(aes(fill=as.factor(Lth)))+
  geom_boxplot(width=0.1)+
  coord_flip()+
  guides(fill=FALSE)+
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c("#FF6666","#FFCC33", "#FFFF99", "#99FF99","#99CCFF"))+
  stat_summary(fun.y=mean, colour="black", geom="point", 
               shape=42, size=7,show_guide = FALSE)+
  labs(title = "Variation in leaf thickness", y = "Leaf thickness (mm)", x = "")+
  scale_x_discrete(labels=c("Wmean_Lth2_global" = "Fixed CWM", "Wmean_Lth1" = "Specific CWM", "Lth_mean_global" = "Fixed mean", "Lth_mean" = "Specific mean", "Lth_ave" = "All observations"))



HeightForbs<-Forbs %>%
  rename(Wmean_Height2_global_F=Wmean_global_Height_F, Wmean_Height1_F=Wmean_Height_F)%>%
  gather(key=Height, value=measurement, Height, Height_mean, Height_mean_global, Wmean_Height2_global_F, Wmean_Height1_F)


ggplot(data=HeightForbs, aes(Height, measurement)) +
  geom_violin(aes(fill=as.factor(Height)))+
  geom_boxplot(width=0.1)+
  coord_flip()+
  guides(fill=FALSE)+
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c("#FF6666","#FFCC33", "#FFFF99", "#99FF99","#99CCFF"))+
  stat_summary(fun.y=mean, colour="black", geom="point", 
               shape=42, size=7,show_guide = FALSE)+
  labs(title = "Variation in height of forbs", y = "Vegetative height (mm)", x = "")+
  scale_x_discrete(labels=c("Wmean_Height2_global_F" = "Fixed CWM", "Wmean_Height1_F" = "Specific CWM", "Height_mean_global" = "Fixed mean", "Height_mean" = "Specific mean", "Height" = "All observations"))


HeightGraminoids<-Graminoids %>%
  rename(Wmean_Height2_global_G=Wmean_global_Height_G, Wmean_Height1_G=Wmean_Height_G)%>%
  gather(key=Height, value=measurement, Height, Height_mean, Height_mean_global, Wmean_Height2_global_G, Wmean_Height1_G)


ggplot(data=HeightGraminoids, aes(Height, measurement)) +
  geom_violin(aes(fill=as.factor(Height)))+
  geom_boxplot(width=0.1)+
  coord_flip()+
  guides(fill=FALSE)+
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c("#FF6666","#FFCC33", "#FFFF99", "#99FF99","#99CCFF"))+
  stat_summary(fun.y=mean, colour="black", geom="point", 
               shape=42, size=7,show_guide = FALSE)+
  labs(title = "Variation in height of graminoids", y = "Vegetative height (mm)", x = "")+
  scale_x_discrete(labels=c("Wmean_Height2_global_G" = "Fixed CWM", "Wmean_Height1_G" = "Specific CWM", "Height_mean_global" = "Fixed mean", "Height_mean" = "Specific mean", "Height" = "All observations"))
