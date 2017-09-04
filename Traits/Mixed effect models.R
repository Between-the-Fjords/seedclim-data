##### Mixed effect models #####
library(lme4)
library(lmerTest)
#source("Cleaning.R")

devtools::source_gist("https://gist.github.com/phipsgabler/91a81883a82a54bb6a92", filename="qqline.r")


#### Temperature ####





## SLA ##

model_SLA_1<-lmer(SLA~scale(Precip)+Temp+Temp:scale(Precip)+(1|Site/Species), data=traitdata)

model_SLA_1p<-lmer(SLA~scale(Precip)+Temp+(1|Site/Species), data=traitdata)

model_SLA_x<-lmer(SLA~Temp+(1|Site/Species), data=traitdata)

model_SLA_0<-lmer(SLA~1+(1|Site/Species), data=traitdata)

AIC(model_SLA_1, model_SLA_1p, model_SLA_x, model_SLA_0)

qqnorm(resid(model_SLA_1))

#The mixed effect model with the temperature and the precip, and the interaction between them is the better model


#The 15 species that have been collected at the most sites, SLA

TheLucky15<-traitdata %>%
  filter(Species %in% c("Agr_cap", "Ant_odo", "Cam_rot", "Des_ces", "Ver_off", "Ave_fle", "Luz_mul", "Bis_viv", "Pot_ere", "Alc_alp", "Tri_rep", "Tha_alp", "Ach_mil", "Nar_str", "Rum_ace"))

scalevalues<- scale(TheLucky15$Temp) #Finding the values to scale the temperature back with
#attributes(scalevalues)


#SLA

SLA_mod_pred<-TheLucky15%>%
  mutate(scale_Temp=scale(Temp))%>%
  group_by(Species)%>%
  do({model<-lmer(SLA ~scale_Temp + (1|Site), data= .)
     NewData <- expand.grid(temp = seq(min(.$scale_Temp)*1.1,max(.$scale_Temp)*1.1, length=100))
     X <- model.matrix(~ temp,data = NewData)
     NewData$fit <- X %*% fixef(model)
     NewData$SE <- sqrt(diag(X %*% vcov(model) %*% t(X)))
     NewData$lo <- NewData$fit - (1.96 * NewData$SE )
     NewData$up <- NewData$fit + (1.96 * NewData$SE )
     NewData
     })%>%
  mutate(temp=temp*attr(scalevalues, which = "scaled:scale")+attr(scalevalues, which = "scaled:center"))


TheLucky15%>%  
  ggplot(aes(x=Temp, y=SLA, color=P_level))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=SLA_mod_pred, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=temp, y=fit), data=SLA_mod_pred, inherit.aes = FALSE)

SLA_precip<-TheLucky15%>%
  group_by(Species)%>%
  do({model<-lmer(SLA ~Precip + (1|Site), data= .)
  NewData <- expand.grid(Precip = seq(min(.$Precip)*1.1,max(.$Precip)*1.1, length=7000))
  X <- model.matrix(~ Precip,data = NewData)
  NewData$fit <- X %*% fixef(model)
  NewData$SE <- sqrt(diag(X %*% vcov(model) %*% t(X)))
  NewData$lo <- NewData$fit - (1.96 * NewData$SE )
  NewData$up <- NewData$fit + (1.96 * NewData$SE )
  NewData
  })

TheLucky15%>%  
  ggplot(aes(x=Precip, y=SLA, color=P_level))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_ribbon(aes(x=Precip, ymax=up, ymin=lo), data=SLA_precip, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=Precip, y=fit), data=SLA_precip, inherit.aes = FALSE)


#Leaf dry matter content

LDMC_mod_pred<-TheLucky15%>%
  mutate(scale_Temp=scale(Temp))%>%
  group_by(Species)%>%
  do({model<-lmer(LDMC ~scale_Temp + (1|Site), data= .)
  NewData <- expand.grid(temp = seq(min(.$scale_Temp)*1.1,max(.$scale_Temp)*1.1, length=100))
  X <- model.matrix(~ temp,data = NewData)
  NewData$fit <- X %*% fixef(model)
  NewData$SE <- sqrt(diag(X %*% vcov(model) %*% t(X)))
  NewData$lo <- NewData$fit - (1.96 * NewData$SE )
  NewData$up <- NewData$fit + (1.96 * NewData$SE )
  NewData
  })%>%
  mutate(temp=temp*attr(scalevalues, which = "scaled:scale")+attr(scalevalues, which = "scaled:center"))

  
TheLucky15%>%  
  ggplot(aes(x=Temp, y=LDMC, color=P_level))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=LDMC_mod_pred, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=temp, y=fit), data=LDMC_mod_pred, inherit.aes = FALSE)

#Leaf thickness

Lth_mod_pred<-TheLucky15%>%
  mutate(scale_Temp=scale(Temp))%>%
  group_by(Species)%>%
  do({model<-lmer(Lth_ave ~scale_Temp + (1|Site), data= .)
  NewData <- expand.grid(temp = seq(min(.$scale_Temp)*1.1,max(.$scale_Temp)*1.1, length=100))
  X <- model.matrix(~ temp,data = NewData)
  NewData$fit <- X %*% fixef(model)
  NewData$SE <- sqrt(diag(X %*% vcov(model) %*% t(X)))
  NewData$lo <- NewData$fit - (1.96 * NewData$SE )
  NewData$up <- NewData$fit + (1.96 * NewData$SE )
  NewData
  })%>%
  mutate(temp=temp*attr(scalevalues, which = "scaled:scale")+attr(scalevalues, which = "scaled:center"))


TheLucky15%>%  
  ggplot(aes(x=Temp, y=Lth_ave, color=P_level))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=Lth_mod_pred, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=temp, y=fit), data=Lth_mod_pred, inherit.aes = FALSE)


#Height

Height_mod_pred<-TheLucky15%>%
  mutate(scale_Temp=scale(Temp))%>%
  group_by(Species)%>%
  do({model<-lmer(Height ~scale_Temp + (1|Site), data= .)
  NewData <- expand.grid(temp = seq(min(.$scale_Temp)*1.1,max(.$scale_Temp)*1.1, length=100))
  X <- model.matrix(~ temp,data = NewData)
  NewData$fit <- X %*% fixef(model)
  NewData$SE <- sqrt(diag(X %*% vcov(model) %*% t(X)))
  NewData$lo <- NewData$fit - (1.96 * NewData$SE )
  NewData$up <- NewData$fit + (1.96 * NewData$SE )
  NewData
  })%>%
  mutate(temp=temp*attr(scalevalues, which = "scaled:scale")+attr(scalevalues, which = "scaled:center"))


TheLucky15%>%  
  ggplot(aes(x=Temp, y=Height, color=P_level))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=Height_mod_pred, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=temp, y=fit), data=Height_mod_pred, inherit.aes = FALSE)
  
#Leaf area

LA_mod_pred<-TheLucky15%>%
  mutate(scale_Temp=scale(Temp))%>%
  group_by(Species)%>%
  do({model<-lmer(Leaf_area ~scale_Temp + (1|Site), data= .)
  NewData <- expand.grid(temp = seq(min(.$scale_Temp)*1.1,max(.$scale_Temp)*1.1, length=100))
  X <- model.matrix(~ temp,data = NewData)
  NewData$fit <- X %*% fixef(model)
  NewData$SE <- sqrt(diag(X %*% vcov(model) %*% t(X)))
  NewData$lo <- NewData$fit - (1.96 * NewData$SE )
  NewData$up <- NewData$fit + (1.96 * NewData$SE )
  NewData
  })%>%
  mutate(temp=temp*attr(scalevalues, which = "scaled:scale")+attr(scalevalues, which = "scaled:center"))


TheLucky15%>%  
  ggplot(aes(x=Temp, y=Leaf_area, color=P_level))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=LA_mod_pred, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=temp, y=fit), data=LA_mod_pred, inherit.aes = FALSE)

# CN ratio

CN_mod_pred<-TheLucky15%>%
  mutate(scale_Temp=scale(Temp))%>%
  group_by(Species)%>%
  do({model<-lmer(CN.ratio ~scale_Temp + (1|Site), data= .)
  NewData <- expand.grid(temp = seq(min(.$scale_Temp)*1.1,max(.$scale_Temp)*1.1, length=100))
  X <- model.matrix(~ temp,data = NewData)
  NewData$fit <- X %*% fixef(model)
  NewData$SE <- sqrt(diag(X %*% vcov(model) %*% t(X)))
  NewData$lo <- NewData$fit - (1.96 * NewData$SE )
  NewData$up <- NewData$fit + (1.96 * NewData$SE )
  NewData
  })%>%
  mutate(temp=temp*attr(scalevalues, which = "scaled:scale")+attr(scalevalues, which = "scaled:center"))


TheLucky15%>%  
  ggplot(aes(x=Temp, y=CN.ratio, color=P_level))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=CN_mod_pred, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=temp, y=fit), data=CN_mod_pred, inherit.aes = FALSE)

## All the species, to figure out how the different species SLAs are reacting to temperature change

scalevalues1<- scale(traitdata$Temp)

FransOrder<-traitdata%>%
  mutate(scale_Temp=scale(Temp))%>%
  group_by(Species, functionalGroup)%>%
  filter(!is.na(SLA))%>%
  filter(n_distinct(T_level)>=2, n_distinct(Site)>2)%>%
  do({broom::tidy(lmer(SLA ~scale_Temp + (1|Site), data= .))})%>%
  filter(term=="scale_Temp")%>%
  mutate(estimate=estimate/attr(scalevalues1, which = "scaled:scale"), std.error=std.error/attr(scalevalues1, which = "scaled:scale"))
  
ggplot(FransOrder, aes(x=estimate, fill=functionalGroup))+
  geom_histogram()



## All the traits ##

MixTrait<-traitdata%>%
  mutate(scale_Temp=scale(Temp), Site=as.factor(Site))%>%
  select(Site, Species, SLA, Lth_ave, Height, LDMC, CN.ratio, functionalGroup, scale_Temp, T_level)%>%
  gather(key= collected_traits, value = measurement, c(SLA, Lth_ave, Height, LDMC, CN.ratio))%>%
  group_by(collected_traits, Species)%>%
  filter(!is.na(measurement))%>%
  filter(n_distinct(Site)>2, n_distinct(T_level)>=2)%>%
  do({model<-lmer(measurement~scale_Temp + (1|Site), data= .)
  NewData <- expand.grid(temp = seq(min(.$scale_Temp)*1.1,max(.$scale_Temp)*1.1, length=100))
  X <- model.matrix(~ temp,data = NewData)
  NewData$fit <- X %*% fixef(model)
  NewData$SE <- sqrt(diag(X %*% vcov(model) %*% t(X)))
  NewData$lo <- NewData$fit - (1.96 * NewData$SE )
  NewData$up <- NewData$fit + (1.96 * NewData$SE )
  NewData
  })%>%
  mutate(temp=temp*attr(scalevalues1, which = "scaled:scale")+attr(scalevalues1, which = "scaled:center"))


MixTrait_est<-traitdata%>%
  mutate(scale_Temp=scale(Temp), Site=as.factor(Site))%>%
  select(Site, Species, SLA, Lth_ave, Height, LDMC, CN.ratio, functionalGroup, scale_Temp, T_level)%>%
  gather(key= collected_traits, value = measurement, c(SLA, Lth_ave, Height, LDMC, CN.ratio))%>%
  group_by(collected_traits, Species, functionalGroup)%>%
  filter(!is.na(measurement))%>%
  filter(n_distinct(Site)>2, n_distinct(T_level)>=2)%>%
  do({broom::tidy(lmer(measurement~scale_Temp + (1|Site), data= .))})%>%
  filter(term=="scale_Temp")%>%
  mutate(estimate=estimate/attr(scalevalues1, which = "scaled:scale"))



ggplot(MixTrait_est, aes(x=estimate, fill=functionalGroup))+
  geom_histogram()+
  facet_wrap(~collected_traits, scales="free")


#### Testing different species ####

## Height ##

## Hypericum macilatum ##

Hyp<-subset(traitdata, Species=="Hyp_mac")
Mix_Hyp<-subset(MixTrait, Species=="Hyp_mac")
Mix_Hyp<-subset(Mix_Hyp, collected_traits=="Height")

Hyp%>%  
  ggplot(aes(x=Temp, y=Height))+
  geom_jitter(height=0)+
  #facet_wrap( ~ collected_traits, ncol=5)
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=Mix_Hyp, inherit.aes= FALSE, alpha=0.5)+
  geom_line(aes(x=temp, y=fit), data=Mix_Hyp, inherit.aes = FALSE)

# Decided te remove Hypericum macilatum from Alrust



## CN ratio ##

CN.ratio<-subset(MixTrait_est, collected_traits=="CN.ratio")
CN.ratio%>%
  filter(estimate<0)


## Rumex acetosella ##

Mix_Rum_acl<-filter(MixTrait, Species=="Rum_acl", collected_traits=="CN.ratio")

filter(traitdata, Species=="Rum_acl", !is.na(CN.ratio))%>%  
  ggplot(aes(x=Temp, y=CN.ratio))+
  geom_jitter(height=0)+
  #facet_wrap( ~ collected_traits, ncol=5)
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=Mix_Rum_acl, inherit.aes= FALSE, alpha=0.5)+
  geom_line(aes(x=temp, y=fit), data=Mix_Rum_acl, inherit.aes = FALSE)


## Veronica offisinalis ##

Mix_Ver_off<-filter(MixTrait, Species=="Ver_off", collected_traits=="CN.ratio")

filter(traitdata, Species=="Ver_off", !is.na(CN.ratio))%>%  
  ggplot(aes(x=Temp, y=CN.ratio, color=Site))+
  geom_jitter(height=0)+
  #facet_wrap( ~ collected_traits, ncol=5)
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=Mix_Ver_off, inherit.aes= FALSE, alpha=0.5)+
  geom_line(aes(x=temp, y=fit), data=Mix_Ver_off, inherit.aes = FALSE)


## Carex pallescens ##

Mix_Car_pal<-filter(MixTrait, Species=="Car_pal", collected_traits=="CN.ratio")

filter(traitdata, Species=="Car_pal", !is.na(CN.ratio))%>%  
  ggplot(aes(x=Temp, y=CN.ratio, color=Site))+
  geom_jitter(height=0)+
  #facet_wrap( ~ collected_traits, ncol=5)
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=Mix_Car_pal, inherit.aes= FALSE, alpha=0.5)+
  geom_line(aes(x=temp, y=fit), data=Mix_Car_pal, inherit.aes = FALSE)

## LDMC ##

LDMC_mem<-subset(MixTrait_est, collected_traits=="LDMC")
LDMC_mem%>%
  filter(estimate < -0.05)

## Trifolium pratense ##

Mix_Tri_pra<-filter(MixTrait, Species=="Tri_pra", collected_traits=="LDMC")

filter(traitdata, Species=="Tri_pra", !is.na(LDMC))%>%  
  ggplot(aes(x=Temp, y=LDMC, color=Site))+
  geom_jitter(height=0)+
  #facet_wrap( ~ collected_traits, ncol=5)
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=Mix_Tri_pra, inherit.aes= FALSE, alpha=0.5)+
  geom_line(aes(x=temp, y=fit), data=Mix_Tri_pra, inherit.aes = FALSE)

# This must be one of the LDMC mistakes - check after fixing those mistakes

## Alchemilla alpina ##

Mix_Alc_alp<-filter(MixTrait, Species=="Alc_alp", collected_traits=="LDMC")

filter(traitdata, Species=="Alc_alp", !is.na(LDMC))%>%  
  ggplot(aes(x=Temp, y=LDMC, color=Site))+
  geom_jitter(height=0)+
  #facet_wrap( ~ collected_traits, ncol=5)
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=Mix_Alc_alp, inherit.aes= FALSE, alpha=0.5)+
  geom_line(aes(x=temp, y=fit), data=Mix_Alc_alp, inherit.aes = FALSE)

# I was checking one that was on the 0 to see how it looks like

## Leaf thickness ##

Lth_mem<-subset(MixTrait_est, collected_traits=="Lth_ave")
Lth_mem%>%
  filter(estimate > 0.0250)

## Rumex acetosella ##

Mix_Rum_acl_Lth<-filter(MixTrait, Species=="Rum_acl", collected_traits=="Lth_ave")

filter(traitdata, Species=="Rum_acl", !is.na(Lth_ave))%>%  
  ggplot(aes(x=Temp, y=Lth_ave, color=Site))+
  geom_jitter(height=0)+
  #facet_wrap( ~ collected_traits, ncol=5)
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=Mix_Rum_acl_Lth, inherit.aes= FALSE, alpha=0.5)+
  geom_line(aes(x=temp, y=fit), data=Mix_Rum_acl_Lth, inherit.aes = FALSE)


## Knautia arvensis ##

Mix_Kna_arv<-filter(MixTrait, Species=="Kna_arv", collected_traits=="Lth_ave")

filter(traitdata, Species=="Kna_arv", !is.na(Lth_ave))%>%  
  ggplot(aes(x=Temp, y=Lth_ave, color=Site))+
  geom_jitter(height=0)+
  #facet_wrap( ~ collected_traits, ncol=5)
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=Mix_Kna_arv, inherit.aes= FALSE, alpha=0.5)+
  geom_line(aes(x=temp, y=fit), data=Mix_Kna_arv, inherit.aes = FALSE)

# A wierd trend, but it looks ok


## Hypericum macilatum ##

Mix_Hyp_mac<-filter(MixTrait, Species=="Hyp_mac", collected_traits=="Lth_ave")

filter(traitdata, Species=="Hyp_mac", !is.na(Lth_ave))%>%  
  ggplot(aes(x=Temp, y=Lth_ave, color=Site))+
  geom_jitter(height=0)+
  #facet_wrap( ~ collected_traits, ncol=5)
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=Mix_Hyp_mac, inherit.aes= FALSE, alpha=0.5)+
  geom_line(aes(x=temp, y=fit), data=Mix_Hyp_mac, inherit.aes = FALSE)

# A wierd trend, but it looks ok


## Viola riviniana ##

Mix_Vio_riv<-filter(MixTrait, Species=="Vio_riv", collected_traits=="Lth_ave")

filter(traitdata, Species=="Vio_riv", !is.na(Lth_ave))%>%  
  ggplot(aes(x=Temp, y=Lth_ave, color=Site))+
  geom_jitter(height=0)+
  #facet_wrap( ~ collected_traits, ncol=5)
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=Mix_Vio_riv, inherit.aes= FALSE, alpha=0.5)+
  geom_line(aes(x=temp, y=fit), data=Mix_Vio_riv, inherit.aes = FALSE)

# A wierd trend, but it looks ok


## SLA ##

SLA_mem<-subset(MixTrait_est, collected_traits=="SLA")
SLA_mem%>%
  filter(estimate > 40)

## Rumex acetosella ##

Mix_Rum_acl<-filter(MixTrait, Species=="Rum_acl", collected_traits=="SLA")

filter(traitdata, Species=="Rum_acl", !is.na(SLA))%>%  
  ggplot(aes(x=Temp, y=SLA, color=Site))+
  geom_jitter(height=0)+
  #facet_wrap( ~ collected_traits, ncol=5)
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=Mix_Rum_acl, inherit.aes= FALSE, alpha=0.5)+
  geom_line(aes(x=temp, y=fit), data=Mix_Rum_acl, inherit.aes = FALSE)

#One point is kind of wierd, but it is not wierd enough to change