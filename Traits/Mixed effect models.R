##### Mixed effect models #####
library(lme4)
library(lmerTest)

#### Precipitation ####



## SLA ##

model_SLA_1<-glmer(SLA~scale(Precip)+Temp+Temp:scale(Precip)+(1|Site/Species), data=traitdata, family="gamma")
model_SLA_0<-glmer(SLA~1+(1|Site/Species), data=traitdata, family="gamma")

summary(MEMSLA1_P)

AIC(model_SLA_1, model_SLA_0)

qqnorm(resid(model_SLA_1))


## SLA ##

SLA_model<- glmer(SLA ~scale(Temp)+scale(Precip)+scale(Temp):scale(Precip) + (1|Site), data= traitdata, fam=gaussian(link=log))

summary(SLA_model)


traitdata%>%
  filter(Species %in% c("Agr_cap", "Ant_odo", "Cam_rot", "Des_ces", "Ver_off", "Ave_fle", "Luz_mul", "Bis_viv", "Pot_ere", "Alc_alp", "Tri_rep", "Tha_alp", "Ach_mil", "Nar_str", "Rum_ace"))%>%
  ggplot(aes(x=Temp, y=SLA, color=T_level))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_smooth(method="lme", aes(group=1), method.args=list(random=~1|Site))



SLA_model_Agr<- lmer(SLA ~scale(Temp)+scale(Precip)+scale(Temp):scale(Precip) + (1|Site), data= traitdata, subset=Species=="Agr_cap")

summary(SLA_model_Agr)


hm<-traitdata%>%
  select(Species)%>%
  group_by(Species)%>%
  summarise(number=n())%>%
  arrange(-number)
