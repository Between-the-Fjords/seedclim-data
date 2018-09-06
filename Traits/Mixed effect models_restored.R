##### Mixed effect models #####
library(lme4)
#library(lmerTest)
library(broom)
#source("Cleaning.R")

#devtools::source_gist("https://gist.github.com/phipsgabler/91a81883a82a54bb6a92", filename="qqline.r")


#### Temperature ####

## SLA ##

#model_SLA_1<-lmer(SLA~scale(Precip)+Temp+Temp:scale(Precip)+(1|Site/Species), data=traitdata)

#model_SLA_1p<-lmer(SLA~scale(Precip)+Temp+(1|Site/Species), data=traitdata)

#model_SLA_x<-lmer(SLA~Temp+(1|Site/Species), data=traitdata)

#model_SLA_0<-lmer(SLA~1+(1|Site/Species), data=traitdata)

#AIC(model_SLA_1, model_SLA_1p, model_SLA_x, model_SLA_0)

#qqnorm(resid(model_SLA_1))

#The mixed effect model with the temperature and the precip, and the interaction between them is the better model


#The 15 species that have been collected at the most sites, SLA

TheLucky15<-traitdata %>%
  filter(Species %in% c("Agr_cap", "Ant_odo", "Cam_rot", "Des_ces", "Ver_off", "Ave_fle", "Luz_mul", "Bis_viv", "Pot_ere", "Alc_alp", "Tri_rep", "Tha_alp", "Ach_mil", "Nar_str", "Rum_ace"))

scalevalues<- scale(TheLucky15$Temp) #Finding the values to scale the temperature back with
#attributes(scalevalues)


#SLA

SLA_temp_95 <- TheLucky15 %>%
  mutate(scale_Precip = scale(Precip),
         scale_Temp = scale(Temp)) %>%
  group_by(Species) %>%
  do({
    model <- lmer(SLA ~ scale_Temp*scale_Precip + (1 | Site), data = .)
    tidy(model)
  }) %>%
  filter(term == "scale_Temp"| term =="scale_Precip" | term == "scale_Temp:scale_Precip")

SLA_temp_95 <- SLA_temp_95%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))

SLA_temp_95 %>%
  group_by(term) %>%
  mutate(Average = mean(estimate)) %>%
  ggplot(aes(
    x = Species,
    y = estimate,
    ymin = lower,
    ymax = upper,
    color = term
  )) +
  geom_errorbar(width = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5)) +
  coord_flip()+
  scale_color_manual(labels = c("Precipitation", "Temperature", "Interaction"), values=rev(c("#FF6666","#FFCC33","#99CCFF")))+
  labs(y="Change in SLA per unit precipitation and/or temperature", x = "", color="")+
  theme_minimal()+
  geom_hline(
    yintercept = 0,
    color = "darkgrey")

#ggsave("Model_output.jpg")



#### SLA Agrostis capillaris ####

Agr_cap<-TheLucky15%>%
  filter(Species=="Agr_cap")%>%
  filter(!is.na(SLA))

model_Agr <- lmer(SLA ~scale(Temp)*scale(Precip) + (1|Site), data= Agr_cap)

newdata_Agr<-expand.grid(Precip=seq(500,3200, length=500), Temp=c(6.5, 8.5, 10.5), Site=NA)

newdata_Agr$fit <- predict(model_Agr, re.form=NA, newdata=newdata_Agr)

summary(model_Agr)  

Agr_cap_plot <- ggplot(Agr_cap, aes(Precip, SLA,lty = as.factor(T_level),colour = as.factor(T_level))) +
  geom_jitter(show.legend = FALSE) +
  theme_minimal(base_size = 11) +
  geom_line(aes(
    x = Precip,
    y = fit, color=factor(Temp)), data=newdata_Agr, size = 1, inherit.aes = FALSE,
    show.legend = FALSE) +
  labs(
    title = "Agrostis capillaris",
    x = "",
    y = "cm2/g",
    colour = "Temperature (ºC)",
    lty = "Temperature (ºC)"
  ) +
  scale_color_manual(values = c("#99CCFF", "#FFCC33", "#FF6666")) +
  theme(plot.title = element_text(hjust = 0.5, face="italic"))+
  expand_limits(y=c(20,700))


#### Anthoxantum odoratum ####

Ant_odo <- TheLucky15 %>%
  filter(Species == "Ant_odo") %>%
  filter(!is.na(SLA))


model_Ant_odo <- lmer(SLA ~ scale(Temp) * scale(Precip) + (1 | Site), data = Ant_odo)

newdata_Ant<-expand.grid(Precip=seq(500,3200, length=3000), Temp=c(6.5, 8.5, 10.5), Site=NA)

newdata_Ant$fit <- predict(model_Ant_odo, re.form=NA, newdata=newdata_Ant)

#summary(model_Ant_odo)  


Ant_odo_plot <- ggplot(Ant_odo, aes(Precip, SLA, lty=as.factor(T_level), colour=as.factor(T_level))) +
  geom_jitter(show.legend = FALSE) +
  theme_minimal(base_size = 11) +
  geom_line(aes( x = Precip,
    y = fit, color = factor(Temp)), data=newdata_Ant,
    size = 1, inherit.aes = FALSE, show.legend = FALSE) +
  labs(
    title = "Anthoxanthum odoratum",
    x = "",
    y = "cm2/g"
  ) +
  scale_color_manual(values=c("#99CCFF", "#FFCC33", "#FF6666")) +
  theme(plot.title = element_text(hjust = 0.5, face="italic"))+
  expand_limits(y=c(20,700))


#### Campanula rotundifolia ####

Cam_rot<-TheLucky15%>%
  filter(Species=="Cam_rot")%>%
  filter(!is.na(SLA))

model_Cam_rot <- lmer(SLA ~scale(Temp)*scale(Precip) + (1|Site), data= Cam_rot)

newdata_Cam<-expand.grid(Precip=seq(500,3200, length=3000), Temp=c(6.5, 8.5, 10.5), Site=NA)

newdata_Cam$fit <- predict(model_Cam_rot, re.form=NA, newdata=newdata_Cam)

Cam_rot_plot <- ggplot(Cam_rot, aes(Precip, SLA,lty = as.factor(T_level), color = as.factor(T_level))) +
  geom_jitter(aes(color = as.factor(T_level))) +
  theme_minimal(base_size = 11) +
  geom_line(aes(
    y = fit, x=Precip, color=factor(Temp)), data = newdata_Cam, size = 1, inherit.aes = FALSE) +
  labs(
    title = "(c) Campanula rotundifolia",
    x = "Precipitation (mm/year)",
    y = "Specific leaf area (cm2/g)",
    colour = "Temperature (ºC)",
    lty = "Temperature (ºC)"
  ) +
  scale_color_manual(values = c("#99CCFF", "#FFCC33", "#FF6666")) +
  theme(plot.title = element_text(hjust = 0.5, face="italic"))+
  expand_limits(y=c(20,700))


#### Deschampsia cespitosa ####

Des_ces<-TheLucky15%>%
  filter(Species=="Des_ces")%>%
  filter(!is.na(SLA))

model_Des_ces <- lmer(SLA ~scale(Temp)*scale(Precip) + (1|Site), data= Des_ces)

newdata_Des<-expand.grid(Precip=seq(500,3200, length=3000), Temp=c(6.5, 8.5, 10.5), Site=NA)

newdata_Des$fit <- predict(model_Des_ces, re.form=NA, newdata=newdata_Des)

newdata_Des<-newdata_Des%>%
  filter(!Temp==10.5)

Des_ces_plot <- ggplot(Des_ces, aes(Precip, SLA, lty = as.factor(T_level), color = as.factor(T_level))) +
  geom_jitter(show.legend = FALSE) +
  theme_minimal(base_size = 11) +
  geom_line(aes( x = Precip,
    y = fit, color= factor(Temp)), data=newdata_Des, size = 1, inherit.aes = FALSE, show.legend = FALSE) +
  labs(
    title = "(d) Deschampsia cespitosa",
    x = "Precipitation (mm/year)",
    y = "cm2/g"
  ) +
  scale_color_manual(values = c("#99CCFF", "#FFCC33", "#FF6666")) +
  theme(plot.title = element_text(hjust = 0.5, face="italic"))+
  expand_limits(y=c(20,700))



library(gridExtra)
grid.arrange(Agr_cap_plot, Ant_odo_plot, Cam_rot_plot, Des_ces_plot, widths=c(0.6, 0.4), ncol=2)


png("Species_trends_SLA.png", width = 1668, height = 2532, res = 400)
grid.arrange(Ant_odo_plot, Agr_cap_plot, Des_ces_plot, ncol=1)
dev.off()


#################################### Old code #########################################

#scale_values<- scale(TheLucky15$Precip)

##### One model of each #####

SLA_temp_95 <- TheLucky15 %>%
  #mutate(scale_Temp = scale(Temp)) %>%
  group_by(Species) %>%
  do({
    model <- lmer(SLA ~ Temp + (1 | Site), data = .)
    tidy(model)
  }) %>%
  filter(term == "Temp")

SLA_temp_95 <- SLA_temp_95%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))




SLA_precip_95 <- TheLucky15 %>%
  mutate(scale_Precip = scale(Precip)) %>%
  group_by(Species) %>%
  do({
    model <- lmer(SLA ~ scale_Precip + (1 | Site), data = .)
    tidy(model)
  }) %>%
  filter(term == "scale_Precip")

SLA_precip_95 <- SLA_precip_95%>%
  mutate(lower = (estimate - std.error*1.96),
         upper = (estimate + std.error*1.96))



SLA_95<-rbind(SLA_temp_95, SLA_precip_95)



SLA_95 %>%
  group_by(term) %>%
  mutate(Average = mean(estimate)) %>%
  ungroup() %>%
  ggplot(aes(
    x = Species,
    y = estimate,
    ymin = lower,
    ymax = upper,
    col = term,
    group = term
  )) +
  geom_hline(
    yintercept = c(0, 2.301869, 7.031479),
    color = c("red", "#0033CC", "#33CC00")
  ) +
  geom_errorbar(width = 0.5, position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5)) +
  coord_flip() +
  scale_color_manual("term",
                     breaks = c(1, 2),
                     values = c("#0033CC", "#33CC00"))


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



############

scalevalues<- scale(TheLucky15$Temp)

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


SLA_mod_pred<-SLA_mod_pred%>%
  filter(Species=="Agr_cap")


TheLucky15%>%  
  filter(Species=="Agr_cap")%>%
  ggplot(aes(x=Temp, y=SLA, color=as.factor(P_level)))+
  geom_jitter(height=0)+
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=SLA_mod_pred, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=temp, y=fit), data=SLA_mod_pred, inherit.aes = FALSE)+
  labs(color = "Precipitation (mm)")


SLA_mod_temp<-SLA_mod_temp%>%
  filter(Species=="Agr_cap")


TheLucky15%>%  
  filter(Species=="Agr_cap")%>%
  ggplot(aes(x=Temp, y=SLA, color=as.factor(P_level)))+
  geom_jitter(height=0)+
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=SLA_mod_pred, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=temp, y=fit), data=SLA_mod_pred, inherit.aes = FALSE)+
  labs(color = "Precipitation (mm)", title="Agrostis capillaris")


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

TheLucky15$Species=="Agr_cap" %>%  
  ggplot(aes(x=Precip, y=SLA, color=as.factor(T_level)))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_ribbon(aes(x=Precip, ymax=up, ymin=lo), data=SLA_precip, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=Precip, y=fit), data=SLA_precip, inherit.aes = FALSE)+
  labs(color = "Temperature (C)")


Agr_cap<-TheLucky15%>%
  filter(Species=="Agr_cap")

Agr_cap%>%
  ggplot(aes(x=Precip, y=SLA, color=as.factor(T_level)))+
  geom_jitter()+
  facet_grid(~ T_level)+
  geom_ribbon(aes(x=Precip, ymax=up, ymin=lo), data=SLA_precip, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=Precip, y=fit), data=SLA_precip, inherit.aes = FALSE)+
  labs(color = "Temperature (C)")

#Leaf dry matter content

TheLucky15_LDMC<-traitdata %>%
  filter(Species %in% c("Agr_cap", "Ant_odo", "Cam_rot", "Des_ces", "Ver_off", "Ave_fle", "Luz_mul", "Bis_viv", "Pot_ere", "Alc_alp", "Tri_rep", "Tha_alp", "Ach_mil", "Nar_str", "Rum_ace"))%>%
  filter(!LDMC>1)

scalevalues_LDMC<- scale(TheLucky15_LDMC$Temp)

LDMC_mod_pred<-TheLucky15_LDMC%>%
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
  mutate(temp=temp*attr(scalevalues_LDMC, which = "scaled:scale")+attr(scalevalues, which = "scaled:center"))

  
TheLucky15_LDMC%>%  
  ggplot(aes(x=Temp, y=LDMC, color=as.factor(P_level)))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=LDMC_mod_pred, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=temp, y=fit), data=LDMC_mod_pred, inherit.aes = FALSE)+
  labs(color = "Precipitation (mm)")

LDMC_precip<-TheLucky15_LDMC%>%
  group_by(Species)%>%
  do({model<-lmer(LDMC ~Precip + (1|Site), data= .)
  NewData <- expand.grid(Precip = seq(min(.$Precip)*1.1,max(.$Precip)*1.1, length=7000))
  X <- model.matrix(~ Precip,data = NewData)
  NewData$fit <- X %*% fixef(model)
  NewData$SE <- sqrt(diag(X %*% vcov(model) %*% t(X)))
  NewData$lo <- NewData$fit - (1.96 * NewData$SE )
  NewData$up <- NewData$fit + (1.96 * NewData$SE )
  NewData
  })

TheLucky15_LDMC%>%  
  ggplot(aes(x=Precip, y=LDMC, color=as.factor(T_level)))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_ribbon(aes(x=Precip, ymax=up, ymin=lo), data=LDMC_precip, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=Precip, y=fit), data=LDMC_precip, inherit.aes = FALSE)+
  labs(color = "Temperature (C)")

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
  ggplot(aes(x=Temp, y=Lth_ave, color=as.factor(P_level)))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=Lth_mod_pred, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=temp, y=fit), data=Lth_mod_pred, inherit.aes = FALSE)+
  labs(color = "Precipitation (mm)", x= "Temperature (C)", y="Leaf thickness (mm)")

Lth_precip<-TheLucky15%>%
  group_by(Species)%>%
  do({model<-lmer(Lth_ave ~Precip + (1|Site), data= .)
  NewData <- expand.grid(Precip = seq(min(.$Precip)*1.1,max(.$Precip)*1.1, length=7000))
  X <- model.matrix(~ Precip,data = NewData)
  NewData$fit <- X %*% fixef(model)
  NewData$SE <- sqrt(diag(X %*% vcov(model) %*% t(X)))
  NewData$lo <- NewData$fit - (1.96 * NewData$SE )
  NewData$up <- NewData$fit + (1.96 * NewData$SE )
  NewData
  })

TheLucky15%>%  
  ggplot(aes(x=Precip, y=Lth_ave, color=as.factor(T_level)))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_ribbon(aes(x=Precip, ymax=up, ymin=lo), data=Lth_precip, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=Precip, y=fit), data=Lth_precip, inherit.aes = FALSE)+
  labs(x= "Precipitation (mm)", color= "Temperature (C)", y = "Leaf thickness (mm)")


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
  ggplot(aes(x=Temp, y=Height, color=as.factor(P_level)))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=Height_mod_pred, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=temp, y=fit), data=Height_mod_pred, inherit.aes = FALSE)+
  labs(color = "Precipitation (mm)", x= "Temperature (C)", y="Height (mm)")

Height_precip<-TheLucky15%>%
  group_by(Species)%>%
  do({model<-lmer(Height ~Precip + (1|Site), data= .)
  NewData <- expand.grid(Precip = seq(min(.$Precip)*1.1,max(.$Precip)*1.1, length=7000))
  X <- model.matrix(~ Precip,data = NewData)
  NewData$fit <- X %*% fixef(model)
  NewData$SE <- sqrt(diag(X %*% vcov(model) %*% t(X)))
  NewData$lo <- NewData$fit - (1.96 * NewData$SE )
  NewData$up <- NewData$fit + (1.96 * NewData$SE )
  NewData
  })

TheLucky15%>%  
  ggplot(aes(x=Precip, y=Height, color=as.factor(T_level)))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_ribbon(aes(x=Precip, ymax=up, ymin=lo), data=Height_precip, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=Precip, y=fit), data=Height_precip, inherit.aes = FALSE)+
  labs(x = "Precipitation (mm)", color= "Temperature (C)", y="Height (mm)")
  
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
  ggplot(aes(x=Temp, y=Leaf_area, color=as.factor(P_level)))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=LA_mod_pred, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=temp, y=fit), data=LA_mod_pred, inherit.aes = FALSE)+
  labs(color = "Precipitation (mm)", x= "Temperature (C)", y="Leaf area (x)")



LA_precip<-TheLucky15%>%
  group_by(Species)%>%
  do({model<-lmer(Leaf_area ~Precip + (1|Site), data= .)
  NewData <- expand.grid(Precip = seq(min(.$Precip)*1.1,max(.$Precip)*1.1, length=7000))
  X <- model.matrix(~ Precip,data = NewData)
  NewData$fit <- X %*% fixef(model)
  NewData$SE <- sqrt(diag(X %*% vcov(model) %*% t(X)))
  NewData$lo <- NewData$fit - (1.96 * NewData$SE )
  NewData$up <- NewData$fit + (1.96 * NewData$SE )
  NewData
  })

TheLucky15%>%  
  ggplot(aes(x=Precip, y=Leaf_area, color=as.factor(T_level)))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_ribbon(aes(x=Precip, ymax=up, ymin=lo), data=LA_precip, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=Precip, y=fit), data=LA_precip, inherit.aes = FALSE)+
  labs(x = "Precipitation (mm)", color= "Temperature (C)", y="Leaf area (x)")

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
  ggplot(aes(x=Temp, y=CN.ratio, color=as.factor(P_level)))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_ribbon(aes(x=temp, ymax=up, ymin=lo), data=CN_mod_pred, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=temp, y=fit), data=CN_mod_pred, inherit.aes = FALSE)+
  labs(color = "Precipitation (mm)", x= "Temperature (C)", y="C/N ratio")

CN_precip<-TheLucky15%>%
  group_by(Species)%>%
  do({model<-lmer(CN.ratio ~Precip + (1|Site), data= .)
  NewData <- expand.grid(Precip = seq(min(.$Precip)*1.1,max(.$Precip)*1.1, length=7000))
  X <- model.matrix(~ Precip,data = NewData)
  NewData$fit <- X %*% fixef(model)
  NewData$SE <- sqrt(diag(X %*% vcov(model) %*% t(X)))
  NewData$lo <- NewData$fit - (1.96 * NewData$SE )
  NewData$up <- NewData$fit + (1.96 * NewData$SE )
  NewData
  })

TheLucky15%>%  
  ggplot(aes(x=Precip, y=CN.ratio, color=as.factor(T_level)))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)+
  geom_ribbon(aes(x=Precip, ymax=up, ymin=lo), data=CN_precip, inherit.aes= FALSE, alpha=0.4)+
  geom_line(aes(x=Precip, y=fit), data=CN_precip, inherit.aes = FALSE)+
  labs(x= "Precipitation (mm)", color= "Temperature (C)", y="C/N ratio")

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

traitdata%>%
  filter(Order=="Poales")%>%
  ggplot(aes(x=Temp, y=SLA))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=5)

traitdata%>%
  filter(!Order=="Poales")%>%
  ggplot(aes(x=Temp, y=SLA))+
  geom_jitter(height=0)+
  facet_wrap( ~ Species, ncol=10)



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
