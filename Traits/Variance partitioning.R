# Lorah Patterson - Variance partitioning of 2016 China leaf trait data

### lme and varcomp analysis ###

#library("BIEN")
#library("varComp")
library("ape")
library("nlme")
#library("lme4")
#library("HLMdiag")


### Variance partitioning by taxonomic level: order, family, genus, species, within species ###

#varSLA <- lmer(SLA~1 + (1|Order/Family/Genus/species), data=wcommunity_df, na.action = na.omit, REML = TRUE)
#varcomp.mer(varSLA)

varSLA <- lme(SLA~1, random= ~1|Order/Family/Genus/Species, data=wcommunity_df, na.action = na.omit)

SLA_sys<-varcomp(varSLA, scale=TRUE)
#plot(SLA_sys)

#Order: 0.0%
#Family: 13.5%
#Genus: 7.7%
#Species: 32.6%
#Within species: 45.9%

LDMC_df<-wcommunity_df%>%
  filter(!LDMC>1)

varLDMC <- lme(LDMC~1, random= ~1|Order/Family/Genus/Species, data=LDMC_df, na.action = na.omit)

LDMC_sys<-varcomp(varLDMC, scale=TRUE)
#plot(LDMC_sys)

#Order: 20.0
#Family: 15.5
#Genus: 12.7
#Species: 16.2
#Within species: 35.6


varLth <- lme(Lth_ave~1, random= ~1|Order/Family/Genus/Species, data=wcommunity_df, na.action = na.omit)

Lth_sys <- varcomp(varLth, scale=TRUE)
#plot(Lth_sys)


# 8.7% på Order level
# 0.0 % på Family level
# 11.0% på Genus level
# 50.5 % på Species level
# 29.9% Unexplained


varCN <- lme(CN.ratio~1, random= ~1|Order/Family/Genus/Species, data=wcommunity_df, na.action = na.omit)

CN_sys <- varcomp(varCN, scale=TRUE)
#plot(CN_sys)


# 19.9 % Order level
# 7.1 % Family level
# 8.4 % Genus
# 23.9 % Species level
# 40.6 % Within

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


# 2.2 % på Order
# 2.6 % på Family
# 59.4% på Genus
# 13.3 % Species
# 22.5% Within

SLA_vec<-as.vector(SLA_sys)
LDMC_vec<-as.vector(LDMC_sys)
Lth_vec<-as.vector(Lth_sys)
CN_vec<-as.vector(CN_sys)
Height_gra_vec<-as.vector(Height_gra_sys)
Height_forb_vec<-as.vector(Height_forb_sys)

Variance<-c(SLA_vec, LDMC_vec, Lth_vec, CN_vec, Height_gra_vec, Height_forb_vec)

Traits<-c("SLA","SLA","SLA","SLA","SLA", "LDMC","LDMC","LDMC","LDMC","LDMC", "Lth","Lth","Lth","Lth","Lth", "CN","CN","CN","CN","CN", "Height_gra","Height_gra","Height_gra","Height_gra","Height_gra", "Height_forb", "Height_forb","Height_forb", "Height_forb","Height_forb")

Systematics<-c("aOrder", "bFamily", "cGenus", "dSpecies", "eWithin","aOrder", "bFamily", "cGenus", "dSpecies", "eWithin","aOrder", "bFamily", "cGenus", "dSpecies", "eWithin","aOrder", "bFamily", "cGenus", "dSpecies", "eWithin","aOrder", "bFamily", "cGenus", "dSpecies", "eWithin","aOrder", "bFamily", "cGenus", "dSpecies", "eWithin")


Col_vec<-(c("Traits", "Variance", "Systematics"))


df<- data.frame(matrix(vector(), 30, 3,
       dimnames=list(c(), Col_vec)),stringsAsFactors=F)


df[,2]<-Variance
df[,1]<-Traits
df[,3]<-Systematics

ggplot(data=df, aes(x=Traits, y=Variance, fill=Systematics))+
  geom_bar(stat="identity")+
  theme_minimal()+
  scale_fill_manual(labels= c("Order", "Family", "Genus", "Species", "Within" ), values=rev(c("#FF6666","#FFCC33", "#FFFF99", "#99FF99","#99CCFF")))+
  scale_x_discrete(labels=c("C/N ratio", "Height forbs", "Height graminoids", "LDMC", "Leaf thickness", "SLA"))+
  theme(axis.text.x= element_text(angle=90))+
  labs(x="", y="Proportion of variance", fill="")


#### Variance partitioning on spacial level ####

varSLA_spa <- lme(SLA~1, random= ~1|T_level/Site, data=wcommunity_df, na.action = na.omit)

SLA_spa <- varcomp(varSLA_spa, scale= TRUE)



LDMC_df<-wcommunity_df%>%
  filter(!LDMC>1)

varLDMC_spa <- lme(LDMC~1, random= ~1|T_level/Site, data=LDMC_df, na.action = na.omit)

LDMC_spa <- varcomp(varLDMC_spa, scale = TRUE)



varLth_spa <- lme(Lth_ave~1, random= ~1|T_level/Site, data=wcommunity_df, na.action = na.omit)

Lth_spa <- varcomp(varLth_spa, scale = TRUE)




varCN_spa <- lme(CN.ratio~1, random= ~1|T_level/Site, data=wcommunity_df, na.action = na.omit)

CN_spa <- varcomp(varCN_spa, scale = TRUE)




Height_graminoid<-wcommunity_df%>%
  filter(Order=="Poales")

Height_forbs<-wcommunity_df%>%
  filter(!Order=="Poales")

varHeight_gra_spa <- lme(Height~1, random= ~1|T_level/Site, data=Height_graminoid, na.action = na.omit)

varHeight_forb_spa <- lme(Height~1, random= ~1|T_level/Site, data=Height_forbs, na.action = na.omit)

Height_gra_spa <- varcomp(varHeight_gra_spa, scale=TRUE)
Height_forb_spa <- varcomp(varHeight_forb_spa, scale=TRUE)



SLA_vec2 <- as.vector(SLA_spa)
LDMC_vec2 <- as.vector(LDMC_spa)
Lth_vec2 <- as.vector(Lth_spa)
CN_vec2 <- as.vector(CN_spa)
Height_vec_gra <- as.vector(Height_gra_spa)
Height_vec_forb <- as.vector(Height_forb_spa)

Variance2<-c(SLA_vec2, LDMC_vec2, Lth_vec2, CN_vec2, Height_vec_gra, Height_vec_forb)

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



#### Variance partitioning species/within species with temp and precip ####

var_temp_SLA <- lme(SLA~T_level, random= ~1|Species, data=wcommunity_df, na.action = na.omit)

SLA_temp<-varcomp(var_temp_SLA, scale=TRUE)


var_precip_SLA <- lme(SLA~P_level, random= ~1|Species, data=wcommunity_df, na.action = na.omit)

SLA_precip<-varcomp(var_precip_SLA, scale=TRUE)


var_temp_LDMC <- lme(LDMC~T_level, random= ~1|Species, data=LDMC_df, na.action = na.omit)

LDMC_temp<-varcomp(var_temp_LDMC, scale=TRUE)


var_precip_LDMC <- lme(LDMC~P_level, random= ~1|Species, data=LDMC_df, na.action = na.omit)

LDMC_precip<-varcomp(var_precip_LDMC, scale=TRUE)



var_temp_Lth <- lme(Lth_ave~T_level, random= ~1|Species, data=wcommunity_df, na.action = na.omit)

Lth_temp<-varcomp(var_temp_Lth, scale=TRUE)


var_precip_Lth <- lme(Lth_ave~P_level, random= ~1|Species, data=wcommunity_df, na.action = na.omit)

Lth_precip<-varcomp(var_precip_Lth, scale=TRUE)



var_temp_CN <- lme(CN.ratio~T_level, random= ~1|Species, data=wcommunity_df, na.action = na.omit)

CN_temp<-varcomp(var_temp_CN, scale=TRUE)


var_precip_CN <- lme(CN.ratio~P_level, random= ~1|Species, data=wcommunity_df, na.action = na.omit)

CN_precip<-varcomp(var_precip_CN, scale=TRUE)


var_temp_Height <- lme(Height~T_level, random= ~1|Species, data=wcommunity_df, na.action = na.omit)

Height_temp<-varcomp(var_temp_Height, scale=TRUE)


var_precip_Height <- lme(Height~P_level, random= ~1|Species, data=wcommunity_df, na.action = na.omit)

Height_precip<-varcomp(var_precip_Height, scale=TRUE)



SLA_temp2 <- as.vector(SLA_temp)
LDMC_temp2 <- as.vector(LDMC_temp)
Lth_temp2 <- as.vector(Lth_temp)
CN_temp2 <- as.vector(CN_temp)
Height_temp2 <- as.vector(Height_temp)

Variance2<-c(SLA_temp2, LDMC_temp2, Lth_temp2, CN_temp2, Height_temp2)

Traits2<-c("SLA","SLA", "LDMC","LDMC", "Lth","Lth", "CN","CN","Height","Height")

Intra_inter<-c("Species", "Within", "Species", "Within", "Species", "Within", "Species", "Within", "Species", "Within")

Col_vec<-(c("Traits", "Variance", "Intra_inter"))


df_temp<- data.frame(matrix(vector(), 10, 3,
                       dimnames=list(c(), Col_vec)),stringsAsFactors=F)


df_temp[,2]<-Variance2
df_temp[,1]<-Traits2
df_temp[,3]<-Intra_inter

ggplot(data=df_temp, aes(x=Traits, y=Variance, fill=Intra_inter))+
  geom_bar(stat="identity")+
  theme_classic()+
  scale_fill_brewer(palette = "YlGn")+
  ggtitle("Inter- and intraspecific variation with Temperature")


SLA_precip2 <- as.vector(SLA_precip)
LDMC_precip2 <- as.vector(LDMC_precip)
Lth_precip2 <- as.vector(Lth_precip)
CN_precip2 <- as.vector(CN_precip)
Height_precip2 <- as.vector(Height_precip)

Variance3<-c(SLA_precip2, LDMC_precip2, Lth_precip2, CN_precip2, Height_precip2)

Traits3<-c("SLA","SLA", "LDMC","LDMC", "Lth","Lth", "CN","CN","Height","Height")

Intra_inter<-c("Species", "Within", "Species", "Within", "Species", "Within", "Species", "Within", "Species", "Within")

Col_vec<-(c("Traits", "Variance", "Intra_inter"))


df_precip<- data.frame(matrix(vector(), 10, 3,
                            dimnames=list(c(), Col_vec)),stringsAsFactors=F)


df_precip[,2]<-Variance3
df_precip[,1]<-Traits3
df_precip[,3]<-Intra_inter

ggplot(data=df_precip, aes(x=Traits, y=Variance, fill=Intra_inter))+
  geom_bar(stat="identity")+
  theme_classic()+
  scale_fill_brewer(palette = "YlGn")+
  ggtitle("Inter- and intraspecific variation with Precipitation")




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

blabla<-wcommunity_df %>%
  rename(Wmean_SLA2_global=Wmean_global_SLA, Wmean_SLA1=Wmean_SLA)%>%
  gather(key=SLA, value=measurement, SLA, SLA_mean, SLA_mean_global, Wmean_SLA1, Wmean_SLA2_global)


ggplot(data=blabla, aes(SLA, measurement)) +
  geom_violin(aes(fill=as.factor(SLA)))+
  geom_boxplot(width=0.1)+
  coord_flip()+
  guides(fill=FALSE)+
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c("#FF6666","#FFCC33", "#FFFF99", "#99FF99","#99CCFF"))+
  stat_summary(fun.y=mean, colour="black", geom="point", 
               shape=42, size=7,show_guide = FALSE)+
  labs(title = "Variation in SLA means", y = "Specific leaf area", x = "")+
  scale_x_discrete(labels=c("Wmean_SLA2_global" = "Global CWM", "Wmean_SLA1" = "Local CWM", "SLA_mean_global" = "Global mean", "SLA_mean" = "Local mean", "SLA" = "Raw data"))
