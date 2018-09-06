#require(data.table)
#library(lme4)
#library(lmerTest)

# Reading in data and dictionaries #

TRY <- fread("Traits/data/TRY_data.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)

dict_TRY <- read.csv2("Traits/data/Dict_TRY.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)

dict_units <- read.csv2("Traits/data/Dict_units.csv", header = TRUE, sep=";", stringsAsFactors = FALSE)

# Cleaning data, changing units to match units used in our study and making means for all species #

TRY <- transform(TRY, OrigValueStr = as.numeric(OrigValueStr))

TRY <- TRY%>%
  select(AccSpeciesName, TraitName, OriglName, OrigValueStr, OrigUnitStr, OrigObsDataID, ValueKindName)%>%
  filter(!TraitName=="")%>%
  mutate(OrigUnitStr = plyr::mapvalues(OrigUnitStr, from = dict_units$Old_names, to = dict_units$New_names))%>%
  filter(TraitName == "Leaf area per leaf dry mass (SLA or 1/LMA): petiole and rachis included")%>%
  filter(!OrigUnitStr == "g/m")%>%
  mutate(SLA = ifelse(OrigUnitStr=="mm2/mg",OrigValueStr*10,
                      ifelse(OrigUnitStr == "m2/kg", OrigValueStr*10,
                             ifelse(OrigUnitStr == "mm2/g", OrigValueStr*0.01,
                                    ifelse(OrigUnitStr == "cm2/g", OrigValueStr*1,
                                           OrigValueStr*1)))))%>%
  filter(ValueKindName == "Single")%>%
  group_by(AccSpeciesName)%>%
  mutate(SLA_TRY = mean(SLA))%>%
  ungroup()%>%
  select(AccSpeciesName, SLA_TRY)%>%
  unique()%>%
  mutate(AccSpeciesName = plyr::mapvalues(AccSpeciesName, from = dict_TRY$Old_names, to = dict_TRY$New_names))
  
# Merging TRY data with the local trait data #

Local_Global_traits <- full_join(wcommunity_df, TRY, by=c("Species"="AccSpeciesName"))

# Making community weighted means #

Local_Global_traits <- Local_Global_traits%>%
  group_by(turfID, Site)%>%
  mutate(Wmean_TRY= weighted.mean(SLA_TRY, cover, na.rm=TRUE))




#### SLA~Temperature models ####
#library("lme4")


Temp_local_SLA <- lmer(Wmean_SLA ~scale(Temp) + (1|Site), data= Local_Global_traits)

Temp_newdata<-expand.grid(Temp=seq(5.5,11, length=500), Site=NA)

Temp_newdata$fit_local_SLA <- predict(Temp_local_SLA, re.form=NA, newdata=Temp_newdata)

#summary(Temp_local_SLA)  
anova(Temp_local_SLA)


Temp_regional_SLA <- lmer(Wmean_global_SLA ~scale(Temp) + (1|Site), data= Local_Global_traits)

Temp_newdata$fit_regional_SLA <- predict(Temp_regional_SLA, re.form=NA, newdata=Temp_newdata)

#summary(Temp_regional_SLA) 
anova(Temp_regional_SLA)


Temp_global_SLA <- lmer(Wmean_TRY ~scale(Temp) + (1|Site), data= Local_Global_traits)

Temp_newdata$fit_global_SLA <- predict(Temp_global_SLA, re.form=NA, newdata=Temp_newdata)

summary(Temp_global_SLA)  
anova(Temp_global_SLA)



Traits_SLA <- Temp_newdata %>%
  gather(key = Predicted, value = value, fit_local_SLA, fit_regional_SLA, fit_global_SLA)


# Plotting models #

ggplot(Traits_SLA, aes(x = Temp, y = value, color=Predicted, inherit.aes = FALSE, show.legend = FALSE)) +
  theme_minimal(base_size = 11) +
  geom_line(size = 1)+
  labs(
    title = "Specific leaf area",
    x = "Temp",
    y = "cm2/g",
    color = "Trait calculation"
  )

#ggsave("Local_global_traits", device="jpg")

