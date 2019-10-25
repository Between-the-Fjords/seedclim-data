#### Run analysis ####
source("Traits/Bootstraping.R")
source("Traits/Cleaning.R")
library(lme4)

set.seed(47)

Bootstrap_Traits <- CWM_Bootstrapping(community_cover, traitdata_1)

Bootstrap_Traits1 <- Bootstrap_Traits %>% 
  mutate(T_level = recode(Site, Ulv = 6.5, Lav = 6.5,  Gud = 6.5, Skj = 6.5, Alr = 8.5, Hog = 8.5, Ram = 8.5, Ves = 8.5, Fau = 10.5, Vik = 10.5, Arh = 10.5, Ovs = 10.5)) %>%
  mutate(Temp = recode(Site, Ulv=6.17, Lav=6.45, Gud=5.87, Skj=6.58, Alr=9.14, Hog=9.17, Ram=8.77, Ves=8.67, Fau=10.3, Vik=10.55, Arh=10.60, Ovs=10.78))%>%
  mutate(Precip= recode(Site, Ulv=596, Lav=1321, Gud=1925, Skj=2725, Alr=789, Hog=1356, Ram=1848, Ves=3029, Fau=600, Vik=1161, Arh=2044, Ovs=2923))%>%
  mutate(P_level = recode(Site, Ulv = 600, Alr = 600, Fau = 600, Lav = 1200, Hog = 1200, Vik = 1200, Gud = 2000, Ram = 2000, Arh = 2000, Skj = 2700, Ves = 2700, Ovs = 2700)) %>% 
  mutate(P_cat = recode(Site, Ulv = "Dry", Alr = "Dry", Fau = "Dry", Lav = "Dry_Intermediate", Hog = "Dry_Intermediate", Vik = "Dry_Intermediate", Gud = "Wet_Intermediate", Ram = "Wet_Intermediate", Arh = "Wet_Intermediate", Skj = "Wet", Ves = "Wet", Ovs = "Wet")) %>% 
  mutate(T_cat = recode(Site, Ulv = "Alpine", Lav = "Alpine",  Gud = "Alpine", Skj = "Alpine", Alr = "Boreal", Hog = "Boreal", Ram = "Boreal", Ves = "Boreal", Fau = "Lowland", Vik = "Lowland", Arh = "Lowland", Ovs = "Lowland"))
#gather(key = "Moment", value = "Value", Mean, Variance, Kurtosis, Skewness)


## Mean of SLA ##

Bootstrap_Traits1 %>% 
  filter(Trait == "SLA_cm2_g") %>% 
  ggplot(aes(Mean, fill = T_cat))+
  geom_density(alpha = 0.5)+
  #facet_wrap(~P_cat, nrow = 1) +
  labs(x = "Community weighted means - SLA (cm2/g)", fill = "Temperature category", title = "Porbability distribution of the community weighted mean of SLA")+
  theme_bw()+
  scale_fill_manual(values = c("#99CCFF", "#9999FF", "#FF9999"))

ggsave("Bootstrap_SLA_mean.jpg", width = 20 , height = 15, units = "cm")

## Variance, Kurtosis, Skewness ##

Bootstrap_Traits1 %>% 
  filter(Trait == "SLA_cm2_g") %>% 
  ggplot(aes(Variance, fill = T_cat))+
  geom_density(alpha = 0.5)+
  #facet_wrap(~P_cat, nrow = 1) +
  labs(x = "Community weighted variance - SLA (cm2/g)", fill = "Temperature category")+
  theme_bw()

Bootstrap_Traits1 %>% 
  filter(Trait == "SLA_cm2_g") %>% 
  ggplot(aes(Skewness, fill = P_cat))+
  geom_density(alpha = 0.5)+
  #facet_wrap(~P_cat, nrow = 1) +
  labs(x = "Community weighted skewness - SLA (cm2/g)", fill = "Temperature category")+
  theme_bw()+
  scale_fill_manual(values = c("#FFDB6D", "#C3D7A4", "#4E84C4", "#3333CC"))

Bootstrap_Traits1 %>% 
  filter(Trait == "SLA_cm2_g") %>% 
  ggplot(aes(Kurtosis, fill = T_cat))+
  geom_density(alpha = 0.5)+
  #facet_wrap(~P_cat, nrow = 1) +
  labs(x = "Community weighted kurtosis - SLA (cm2/g)", fill = "Temperature category")+
  theme_bw()+
  geom_vline(xintercept = 1.2)


## Mean of all traits ##

Bootstrap_Traits1 %>% 
  filter(Trait %in% c("CN_ratio", "LDMC",  "Plant_Height_mm", "SLA_cm2_g")) %>% 
  ggplot(aes(Mean, fill = T_cat))+
  geom_density(alpha = 0.5)+
  facet_wrap(~Trait, scales = "free") +
  labs(x = "Community weighted means", fill = "Temperature category")+
  theme_bw()+ 
  scale_fill_manual(values = c("#99CCFF", "#9999FF", "#FF9999"))


ggsave("Bootstrap_all_traits_mean.jpg", width = 20 , height = 15, units = "cm")


### Kurtosis vs. skewness ##

Bootstrap_Traits1 %>% 
  filter(Trait == "SLA_cm2_g") %>% 
  ggplot(aes(x = Skewness^2, y = Kurtosis, col = T_cat))+
  geom_point()


### Testing ###
Bootstrap_Traits1 %>% 
  filter(Trait == "SLA_cm2_g") %>% 
  lmer(Mean ~ Temp + (Temp|)
       