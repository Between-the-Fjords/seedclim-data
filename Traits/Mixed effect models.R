##### Mixed effect models #####
library(lme4)
library(lmerTest)

#### Precipitation ####



## SLA ##

model_SLA_1<-lmer(SLA~scale(Precip)+Temp+Temp:scale(Precip)+(1|Site/Species), data=traitdata)

model_SLA_1p<-lmer(SLA~scale(Precip)+Temp+(1|Site/Species), data=traitdata)

model_SLA_x<-lmer(SLA~Temp+(1|Site/Species), data=traitdata)

model_SLA_0<-lmer(SLA~1+(1|Site/Species), data=traitdata)

summary(MEMSLA1_P)

AIC(model_SLA_1, model_SLA_1p)

qqnorm(resid(model_SLA_1))




## SLA ##

#Two different models tryed out

#SLA_model<- glmer(SLA ~scale(Temp)+scale(Precip)+scale(Temp):scale(Precip) + (1|Site), data= traitdata, fam=gaussian(link=log))

#SLA_model<-lmer(SLA ~ scale(Temp) + scale(Precip) + scale(Temp):scale(Precip) +  (1 | Site) + (1 | Species), data = traitdata)

#summary(SLA_model)


## Making something to see which species I have collected from at most sites ##

#hm<-traitdata%>%
#  select(Species)%>%
#  group_by(Species)%>%
#  summarise(number=n())%>%
#  arrange(-number)


TheLucky15<-traitdata %>%
  filter(Species %in% c("Agr_cap", "Ant_odo", "Cam_rot", "Des_ces", "Ver_off", "Ave_fle", "Luz_mul", "Bis_viv", "Pot_ere", "Alc_alp", "Tri_rep", "Tha_alp", "Ach_mil", "Nar_str", "Rum_ace"))

klm<-TheLucky15%>%
  mutate(scale_Temp=scale(Temp))%>%
  group_by(Species)%>%
  do(augment(lmer(SLA ~scale_Temp + (1|Site), data= .)))

#scale(TheLucky15$Temp) #Used this to figure out which numbers to use to get the right temperature back from the scaled version


klm<-klm%>%
  mutate(Temp=scale_Temp*1.774107+8.433835) #Getting the real temperature back
  
TheLucky15%>%  
  ggplot(aes(x=Temp, y=SLA, color=P_level))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_line(aes(x=Temp, y=.fixed), data=klm, inherit.aes = FALSE)
  

model<-lmer(SLA ~Temp + (1|Site), data=TheLucky15[TheLucky15$Species=="Ach_mil",])

NewData <- expand.grid(temp = seq(5,11, 0.1))
X <- model.matrix(~ temp,data = NewData)
NewData$fit <- X %*% fixef(model)
#head(NewData,15)     # checking
NewData$SE <- sqrt(diag(X %*% vcov(model) %*% t(X)))
NewData$lo <- NewData$fit - (1.96 * NewData$SE )
NewData$up <- NewData$fit + (1.96 * NewData$SE )
#head(NewData,15)    # checking


TheLucky15%>%  
  ggplot(aes(x=Temp, y=SLA, color=P_level))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_line(aes(x=temp, y=fit), data=NewData, inherit.aes = FALSE)+
  geom_line(aes(x=temp, y=lo), data=NewData, inherit.aes = FALSE)+
  geom_line(aes(x=temp, y=up), data=NewData, inherit.aes = FALSE)



##Making all the models one by one ##
library("broom")

predict(SLA_model_Ant, interval="confidence", level=0.95)
SLA_model_Ant<- lmer(SLA ~scale(Temp)+scale(Precip)+scale(Temp):scale(Precip) + (1|Site), data= traitdata, subset=Species=="Ant_odo")


SLA_model_Cam<- lmer(SLA ~scale(Temp)+scale(Precip)+scale(Temp):scale(Precip) + (1|Site), data= traitdata, subset=Species=="Cam_rot")


SLA_model_Des<- lmer(SLA ~scale(Temp)+scale(Precip)+scale(Temp):scale(Precip) + (1|Site), data= traitdata, subset=Species=="Des_ces")


SLA_model_Ver<- lmer(SLA ~scale(Temp)+scale(Precip)+scale(Temp):scale(Precip) + (1|Site), data= traitdata, subset=Species=="Ver_off")



traitdata$MEM_SLA <- predict(SLA_model, na.action = na.exclude)


