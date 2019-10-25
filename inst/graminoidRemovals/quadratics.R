# analyses
# traits

# height
# PRECIP: 2nd order polynomial
forbcomAnalysis %>% filter(trait == "wmeanheight") %>% ggplot(aes(x = Sprecip0916, y = log(value), shape = TTtreat)) + geom_point() + 
  geom_smooth(method = "lm", colour = "darkgreen") + 
  geom_smooth(method = "lm",formula = 'y~poly(x, 2)', colour = "red")  +
  geom_smooth(method = "lm",formula = 'y~poly(x, 3)', colour = "grey50")

# TEMP: linear
forbcomAnalysis %>% filter(trait == "wmeanheight") %>% ggplot(aes(x = Stemp0916, y = log(value), shape = TTtreat)) + geom_point() + 
  geom_smooth(method = "lm", colour = "darkgreen") + 
  geom_smooth(method = "lm",formula = 'y~poly(x, 2)', colour = "red")  +
  geom_smooth(method = "lm",formula = 'y~poly(x, 3)', colour = "grey50")


forbcomAnalysis %>% filter(trait == "wmeanheight") %>% 
  lmer(log(value) ~ TTtreat*Stemp0916*Sprecip0916*SYear + TTtreat*Stemp0916*SYear*poly(Sprecip0916, 2) - TTtreat:Stemp0916:Sprecip0916:SYear - TTtreat:Stemp0916:SYear:poly(Sprecip0916, 2) + (1|siteID/blockID), REML = FALSE, data = .) %>% 
  tidy() %>% 
  View()


# SLA
# PRECIP: linear model
forbcomAnalysis %>% filter(trait == "wmeanSLA") %>% 
  ggplot(aes(x = Sprecip0916, y = value, shape = TTtreat)) + geom_point() + 
  geom_smooth(method = "lm", colour = "darkgreen") + 
  geom_smooth(method = "lm",formula = 'y~poly(x, 2)', colour = "red")  +
  geom_smooth(method = "lm",formula = 'y~poly(x, 3)', colour = "grey50")

# TEMP: linear model
forbcomAnalysis %>% filter(trait == "wmeanSLA") %>% 
  ggplot(aes(x = temp0916, y = value, shape = TTtreat)) + geom_point() + 
  geom_smooth(method = "lm", colour = "darkgreen") + 
  geom_smooth(method = "lm",formula = 'y~poly(x, 2)', colour = "red")  +
  geom_smooth(method = "lm",formula = 'y~poly(x, 3)', colour = "grey50")

forbcomAnalysis %>% filter(trait == "wmeanSLA") %>% 
  lmer(value ~ TTtreat*Stemp0916*Sprecip0916*SYear - TTtreat:Stemp0916:Sprecip0916:SYear + (1|siteID/blockID), REML = FALSE, data = .) %>% 
  tidy() %>% 
  View()

# LTH 
# PRECIP: linear model
forbcomAnalysis %>% filter(trait == "wmeanLTH") %>% 
  ggplot(aes(x = precip0916, y = value, shape = TTtreat)) + geom_point() + 
  geom_smooth(method = "lm", colour = "darkgreen") + 
  geom_smooth(method = "lm",formula = 'y~poly(x, 2)', colour = "red")  +
  geom_smooth(method = "lm",formula = 'y~poly(x, 3)', colour = "grey50")

# TEMP: 2nd order polynomial
forbcomAnalysis %>% filter(trait == "wmeanLTH") %>% 
  ggplot(aes(x = temp0916, y = value, shape = TTtreat)) + geom_point() + 
  geom_smooth(method = "lm", colour = "darkgreen") + 
  geom_smooth(method = "lm",formula = 'y~poly(x, 2)', colour = "red")  +
  geom_smooth(method = "lm",formula = 'y~poly(x, 3)', colour = "grey50")

forbcomAnalysis %>% filter(trait == "wmeanLTH") %>% 
  lmer(log(value) ~ TTtreat*Stemp0916*Sprecip0916*SYear + TTtreat*Sprecip0916*SYear*I(Stemp0916^2) - TTtreat:Stemp0916:Sprecip0916:SYear - TTtreat:Sprecip0916:SYear:I(Stemp0916^2) + (1|siteID/blockID), REML = FALSE, data = .) %>% 
  tidy() %>% 
  View()


# LDMC
# PRECIP: 2nd order polynomial
forbcomAnalysis %>% filter(trait == "wmeanLDMC") %>% 
  ggplot(aes(x = precip0916, y = value, shape = TTtreat)) + geom_point() + 
  geom_smooth(method = "lm", colour = "darkgreen") + 
  geom_smooth(method = "lm",formula = 'y~poly(x, 2)', colour = "red")  +
  geom_smooth(method = "lm",formula = 'y~poly(x, 3)', colour = "grey50")

# TEMP: linear
forbcomAnalysis %>% filter(trait == "wmeanLDMC") %>% 
  ggplot(aes(x = temp0916, y = value, shape = TTtreat)) + geom_point() + 
  geom_smooth(method = "lm", colour = "darkgreen") + 
  geom_smooth(method = "lm",formula = 'y~poly(x, 2)', colour = "red")  +
  geom_smooth(method = "lm",formula = 'y~poly(x, 3)', colour = "grey50")

forbcomAnalysis %>% filter(trait == "wmeanLDMC") %>% 
  lmer(value ~ TTtreat*Stemp0916*Sprecip0916*SYear + TTtreat*Stemp0916*SYear*I(Sprecip0916^2) - TTtreat:Stemp0916:Sprecip0916:SYear - TTtreat:Stemp0916:SYear:I(Sprecip0916^2) + (1|siteID/blockID), REML = FALSE, data = .) %>% 
  tidy() %>% 
  View()


# LA
# PRECIP: 2nd order polynomial
forbcomAnalysis %>% filter(trait == "wmeanLA") %>% 
  ggplot(aes(x = precip0916, y = value, shape = TTtreat)) + geom_point() + 
  geom_smooth(method = "lm", colour = "darkgreen") + 
  geom_smooth(method = "lm",formula = 'y~poly(x, 2)', colour = "red")  +
  geom_smooth(method = "lm",formula = 'y~poly(x, 3)', colour = "grey50")

# TEMP: 2nd order polynomial
forbcomAnalysis %>% filter(trait == "wmeanLA") %>% 
  ggplot(aes(x = temp0916, y = value, shape = TTtreat)) + geom_point() + 
  geom_smooth(method = "lm", colour = "darkgreen") + 
  geom_smooth(method = "lm",formula = 'y~poly(x, 2)', colour = "red")  +
  geom_smooth(method = "lm",formula = 'y~poly(x, 3)', colour = "grey50")

forbcomAnalysis %>% filter(trait == "wmeanLA") %>% 
  lmer(log(value) ~ TTtreat*Stemp0916*Sprecip0916*SYear + TTtreat*I(Stemp0916^2)*SYear*I(Sprecip0916^2) - TTtreat:Stemp0916:Sprecip0916:SYear - TTtreat:I(Stemp0916^2):SYear:I(Sprecip0916^2) + (1|siteID/blockID), REML = FALSE, data = .) %>% 
  tidy() %>% 
  View()

# CN
# PRECIP: 2nd order polynomial
forbcomAnalysis %>% filter(trait == "wmeanCN") %>% 
  ggplot(aes(x = precip0916, y = value, shape = TTtreat)) + geom_point() + 
  geom_smooth(method = "lm", colour = "darkgreen") + 
  geom_smooth(method = "lm",formula = 'y~poly(x, 2)', colour = "red")  +
  geom_smooth(method = "lm",formula = 'y~poly(x, 3)', colour = "grey50")

# TEMP: 2nd order polynomial
forbcomAnalysis %>% filter(trait == "wmeanCN") %>% 
  ggplot(aes(x = temp0916, y = value, shape = TTtreat)) + geom_point() + 
  geom_smooth(method = "lm", colour = "darkgreen") + 
  geom_smooth(method = "lm",formula = 'y~poly(x, 2)', colour = "red")  +
  geom_smooth(method = "lm",formula = 'y~poly(x, 3)', colour = "grey50")

forbcomAnalysis %>% filter(trait == "wmeanCN") %>% 
  lmer(log(value) ~ TTtreat*Stemp0916*Sprecip0916*SYear + TTtreat*I(Stemp0916^2)*SYear*I(Sprecip0916^2) - TTtreat:Stemp0916:Sprecip0916:SYear - TTtreat:Stemp0916:SYear:I(Sprecip0916^2) + (1|siteID/blockID), REML = FALSE, data = .) %>% 
  tidy() %>% 
  View()
