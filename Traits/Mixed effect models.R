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

summary(MEMSLA1_P)

AIC(model_SLA_1, model_SLA_1p)

qqnorm(resid(model_SLA_1))

#The mixed effect model with the temperature and the precip, and the interaction betweent hem is the better model



## SLA ##

#Two different models tryed out

#SLA_model<- glmer(SLA ~scale(Temp)+scale(Precip)+scale(Temp):scale(Precip) + (1|Site), data= traitdata, fam=gaussian(link=log))

#SLA_model<-lmer(SLA ~ scale(Temp) + scale(Precip) + scale(Temp):scale(Precip) +  (1 | Site) + (1 | Species), data = traitdata)

#summary(SLA_model)

#The gaussion log model was not good. Using just normal gaussian distribution therefore using lmer instead of glmer.


## Making something to see which species I have collected from at most sites ##

#hm<-traitdata%>%
#  select(Species)%>%
#  group_by(Species)%>%
#  summarise(number=n())%>%
#  arrange(-number)


#The 15 species that have been collected at the most sites, SLA

TheLucky15<-traitdata %>%
  filter(Species %in% c("Agr_cap", "Ant_odo", "Cam_rot", "Des_ces", "Ver_off", "Ave_fle", "Luz_mul", "Bis_viv", "Pot_ere", "Alc_alp", "Tri_rep", "Tha_alp", "Ach_mil", "Nar_str", "Rum_ace"))

scalevalues<- scale(TheLucky15$Temp) #Finding the values to scale the temperature back with
#attributes(scalevalues)


klm<-TheLucky15%>%
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
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=klm, inherit.aes= FALSE, alpha=0.5)+
  geom_line(aes(x=temp, y=fit), data=klm, inherit.aes = FALSE)
  


## All the species, to figure out how the different species SLAs are reacting to temperature change

scalevalues1<- scale(traitdata$Temp)

FransOrder<-traitdata%>%
  mutate(scale_Temp=scale(Temp))%>%
  group_by(Species, functionalGroup)%>%
  filter(n_distinct(T_level)>=2, n_distinct(Site)>2)%>%
  do({print(.$Species[1])
    tidy(lmer(SLA ~scale_Temp + (1|Site), data= .))})%>%
  filter(term=="scale_Temp")%>%
  mutate(estimate=estimate*attr(scalevalues1, which = "scaled:scale"), std.error=std.error/attr(scalevalues1, which = "scaled:scale"))
  
ggplot(FransOrder, aes(x=estimate, fill=functionalGroup))+
  geom_histogram()

