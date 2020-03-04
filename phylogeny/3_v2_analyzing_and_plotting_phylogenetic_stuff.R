#Seedclim: analyzing and plotting phylogenetic patterns
#Brian Maitner.  BMaitner at gmail
#####################
#Metadata:

#Treaments (TTtreat)
#Levels: TTC TT1 TT2 TT3 TT4
#warmer (TT2) 
#wetter (TT3)
#warmer and wetter (TT4) climates, 

#transplanting within blocks (to control for the transplanting itself)(TT1), 
#untouched control plot (TTC)

#####################

#load packages
library(lmerTest)
library(lme4)
library(nlme)

#read in data, re-scale predictors
cover.meta<-readRDS(file = "phylogeny/cover_phylo_trait_trait_scaled.rds")
cover.meta$summerTemperature<-scale(cover.meta$summerTemperature)
cover.meta$annualPrecipitation<-scale(cover.meta$annualPrecipitation)
cover.meta$year<-scale(cover.meta$year)

###################################################################

#Fit models

model_outputs<-NULL

for(i in 14:ncol(cover.meta)){
  
  var_i<-colnames(cover.meta)[i]
  

  model_i<- eval(parse(text = paste0("lme(",var_i," ~ annualPrecipitation +summerTemperature+year+TTtreat+year*TTtreat,
                 random=~1|blockID,data=cover.meta)")))
  
sum_i<-summary(model_i)$tTable
sum_i<-as.data.frame(sum_i)
sum_i$parm<-row.names(sum_i)


r2<-r2glmm::r2beta(model = model_i)#temp 0.05
merged_out_i<-merge(x = r2,y = sum_i,by.x = "Effect",by.y = "parm",all.x = T,all.y = T)
merged_out_i<-merged_out_i[c("Effect","Rsq","upper.CL","lower.CL","Value","Std.Error","DF","t-value","p-value")]
  
model_outputs<- rbind(model_outputs,cbind(var_i,merged_out_i))

}

#cleanup
rm(merged_out_i,sum_i,r2,i,var_i,model_i)


###################################################################

#Add metadata
model_outputs$moment<-NA
model_outputs$metric<-NA
model_outputs$weighting<-NA

unique(model_outputs$var_i)

model_outputs$weighting[grep(pattern = "abd_std",x = model_outputs$var_i)]<-"abundance_and_standardized"
model_outputs$weighting[grep(pattern = "abd$",x = model_outputs$var_i)]<-"abundance"
model_outputs$weighting[grep(pattern = "(?<!abd)_std",x = model_outputs$var_i,perl = T)]<-"standardized"

model_outputs$metric[grep(pattern = "^pd",x = model_outputs$var_i)]<-"phydiv"
model_outputs$metric[grep(pattern = "[[:alpha:]]pd",x = model_outputs$var_i)]<-"phydist"
model_outputs$metric[grep(pattern = "[[:alpha:]]ntd",x = model_outputs$var_i)]<-"nearest_taxon_dist"
model_outputs$metric[grep(pattern = "[[:alpha:]]ntraitd",x = model_outputs$var_i)]<-"nearest_trait_dist"
model_outputs$metric[grep(pattern = "[mvsk]traitd",x = model_outputs$var_i)]<-"func_dist"


model_outputs$metric[grep(pattern = "trait_.ntd",x = model_outputs$var_i)]<-"trait_scaled_ntd"
model_outputs$metric[grep(pattern = "trait_.pd",x = model_outputs$var_i)]<-"trait_scaled_phy_dist"
model_outputs$metric[grep(pattern = "trait_pd",x = model_outputs$var_i)]<-"trait_phydist"
model_outputs$metric[grep(pattern = "trait_rate_pd",x = model_outputs$var_i)]<-"trait_rate_phydist"


model_outputs$metric[grep(pattern = "trait_rate_.ntd",x = model_outputs$var_i)]<-"trait_rate_scaled_ntd"
model_outputs$metric[grep(pattern = "trait_rate_.pd",x = model_outputs$var_i)]<-"trait_rate_scaled_phy_dist"



model_outputs$moment[grep(pattern = "^m",x = model_outputs$var_i)]<-"mean"
model_outputs$moment[grep(pattern = "^v",x = model_outputs$var_i)]<-"variance"
model_outputs$moment[grep(pattern = "trait_m",x = model_outputs$var_i)]<-"mean"
model_outputs$moment[grep(pattern = "trait_v",x = model_outputs$var_i)]<-"variance"
model_outputs$moment[grep(pattern = "trait_rate_m",x = model_outputs$var_i)]<-"mean"
model_outputs$moment[grep(pattern = "trait_rate_v",x = model_outputs$var_i)]<-"variance"

model_outputs$Effect<-as.character(model_outputs$Effect)


###############################################
###############################################


library(ggplot2)

ggplot(data=model_outputs, aes(x=dose, y=Rsq)) +
  geom_bar(stat="identity")




model_outputs$Rsq



unique(model_outputs$weighting)

#subset to diversity + whole model

ggplot(data=subset(model_outputs, Effect == "Model" & !is.na(moment)), aes(x=metric, y=Rsq,fill=weighting)) +
  geom_bar(stat="identity", position=position_dodge())+facet_wrap(facets = "moment")+ coord_flip()

ggplot(data=subset(model_outputs, Effect == "year:TTtreatTT1" & !is.na(moment)), aes(x=metric, y=Rsq,fill=weighting)) +
  geom_bar(stat="identity", position=position_dodge())+facet_wrap(facets = "moment")+ coord_flip()

ggplot(data=subset(model_outputs, Effect == "year:TTtreatTT2" & !is.na(moment)), aes(x=metric, y=Rsq,fill=weighting)) +
  geom_bar(stat="identity", position=position_dodge())+facet_wrap(facets = "moment")+ coord_flip()

ggplot(data=subset(model_outputs, Effect == "year:TTtreatTT3" & !is.na(moment)), aes(x=metric, y=Rsq,fill=weighting)) +
  geom_bar(stat="identity", position=position_dodge())+facet_wrap(facets = "moment")+ coord_flip()

ggplot(data=subset(model_outputs, Effect == "year:TTtreatTT4" & !is.na(moment)), aes(x=metric, y=Rsq,fill=weighting)) +
  geom_bar(stat="identity", position=position_dodge())+facet_wrap(facets = "moment")+ coord_flip()


unique(model_outputs$Effect)


model_outputs[which(model_outputs$Effect == "Model"),]
unique(model_outputs$Effect)

#subset to mean metrics + whole model
#subset to nearest metrics + whole model


#Effect == model


