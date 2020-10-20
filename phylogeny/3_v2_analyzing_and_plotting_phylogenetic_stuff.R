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
library(ggplot2)

#read in data, re-scale predictors
cover.meta_trait_scaled <- readRDS(file = "phylogeny/cover_phylo_trait_trait_scaled.rds")
cover.meta_trait <- readRDS(file = "phylogeny/cover_phylo_trait.rds")
cover.meta <- cbind(cover.meta_trait,cover.meta_trait_scaled[14:ncol(cover.meta_trait_scaled)])
rm(cover.meta_trait,cover.meta_trait_scaled)


cover.meta$summerTemperature<-scale(cover.meta$summerTemperature)
cover.meta$annualPrecipitation<-scale(cover.meta$annualPrecipitation)
#cover.meta$year<-scale(cover.meta$year)

#Fix varibale names where needed

#using new naming scheme: type_moment_dist_abd
##type {func,phylo,trait,rate}
##moment {rich,mean,var,skew,kurt}
##dist {all,near}
##abund {abd,nabd}



colnames(cover.meta)[which(colnames(cover.meta)=="trait_pd_abd_std")]<- "trait_rich_all_abd"
colnames(cover.meta)[which(colnames(cover.meta)=="trait_mpd_abd_std")]<- "trait_mean_all_abd"
colnames(cover.meta)[which(colnames(cover.meta)=="trait_mntd_abd_std")]<- "trait_mean_near_abd"
colnames(cover.meta)[which(colnames(cover.meta)=="trait_vpd_abd_std")]<- "trait_var_all_abd"
colnames(cover.meta)[which(colnames(cover.meta)=="trait_vntd_abd_std")]<- "trait_var_near_abd"

##################################################################

#Drop non-abundance-weighted stuff from futher analyses. Abundance weighting gives more power, has larger effect sizes, and generally makes more biological sense.

cover.meta <- cover.meta[,grep(pattern = "nabd",x = colnames(cover.meta),invert = T)]

#drop the richness values for now

cover.meta <- cover.meta[,grep(pattern = "rich",x = colnames(cover.meta),invert = T)]



##################################################################
#Do the control plots for year zero show differences in community structure vs temp/precip/interaction?


#helper fx for model pvals
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

model_outputs_control <- NULL

cover.meta_null <- cover.meta[which(cover.meta$year==min(cover.meta$year)),]

for(i in 14:ncol(cover.meta_null)){
  
  var_i<-colnames(cover.meta_null)[i]
  
  model_i<- eval(parse(text = paste0("lm(",var_i," ~ annualPrecipitation +summerTemperature + annualPrecipitation:summerTemperature, data=cover.meta_null)")))
  
  sum_i<-summary(model_i)$coefficients
  sum_i<-as.data.frame(sum_i)
  sum_i$parm<-row.names(sum_i)
  r2<-r2glmm::r2beta(model = model_i)#temp 0.05
  
  summary(model_i)
  merged_out_i<-merge(x = r2,y = sum_i,by.x = "Effect",by.y = "parm",all.x = T,all.y = T)
  #merged_out_i<-merged_out_i[c("Effect","Rsq","upper.CL","lower.CL","Value","Std.Error","DF","t-value","p-value")]
  merged_out_i[which(merged_out_i$Effect=="Model"),'Pr(>|t|)'] <- lmp(model_i)
  model_outputs_control<- rbind(model_outputs_control,cbind(var_i,merged_out_i))
  
}

#cleanup
rm(merged_out_i,sum_i,r2,i,var_i,model_i)
model_outputs_control$significant <- NA
model_outputs_control$significant
colnames(model_outputs_control)[which(colnames(model_outputs_control)=="Pr(>|t|)")] <- "p-value"
  


model_outputs_control$significant[which(model_outputs_control$`p-value`<=0.05)] <- "Yes"
model_outputs_control$significant[which(model_outputs_control$`p-value`>0.05)] <- "No"




ggplot(data=subset(model_outputs_control, Effect %in% c('annualPrecipitation',"summerTemperature","annualPrecipitation:summerTemperature") ), 
       aes(x=Effect, y=Estimate,alpha=significant)) +  geom_bar(stat="identity")+geom_hline(yintercept = 0,col="red")+facet_wrap('var_i')+ 
  theme(axis.text.x = element_text(angle=270))+ggtitle(label = "Control plots at year zero")


#summarize data in a mode convenient table


control_output_small <- matrix(nrow = length(unique(model_outputs_control$var_i)),ncol = length(setdiff(x = as.character(unique(model_outputs_control$Effect)),y = "Model"))+3)
control_output_small <- as.data.frame(control_output_small)
colnames(control_output_small) <- c("metric",setdiff(x = as.character(unique(model_outputs_control$Effect)),y = "Model"),"R^2","p-value")


for(i in 1:length(unique(model_outputs_control$var_i))){
  control_output_small$metric[i] <- as.character(unique(model_outputs_control$var_i)[i])
  
  #
  control_output_small$annualPrecipitation[i] <- round(model_outputs_control[which(model_outputs_control$Effect=="annualPrecipitation" & model_outputs_control$var_i==control_output_small$metric[i]),"Estimate"],digits = 3)
  
    if(model_outputs_control[which(model_outputs_control$Effect=="annualPrecipitation" & model_outputs_control$var_i==control_output_small$metric[i]),"p-value"] < 0.05){ 
      control_output_small$annualPrecipitation[i] <- paste(control_output_small$annualPrecipitation[i],"*")}
  #
  control_output_small$`annualPrecipitation:summerTemperature`[i] <- round(model_outputs_control[which(model_outputs_control$Effect=='annualPrecipitation:summerTemperature' & model_outputs_control$var_i==control_output_small$metric[i]),"Estimate"],digits = 3)
  
  if(model_outputs_control[which(model_outputs_control$Effect=="annualPrecipitation:summerTemperature" & model_outputs_control$var_i==control_output_small$metric[i]),"p-value"] < 0.05){ 
    control_output_small$`annualPrecipitation:summerTemperature`[i] <- paste(control_output_small$`annualPrecipitation:summerTemperature`[i],"*")}
  
  #
  control_output_small$summerTemperature[i] <- round(model_outputs_control[which(model_outputs_control$Effect=="summerTemperature" & model_outputs_control$var_i==control_output_small$metric[i]),"Estimate"],digits = 3)
  
  if(model_outputs_control[which(model_outputs_control$Effect=="summerTemperature" & model_outputs_control$var_i==control_output_small$metric[i]),"p-value"] < 0.05){ 
    control_output_small$summerTemperature[i] <- paste(control_output_small$summerTemperature[i],"*")}
  
  
  #
  control_output_small$`(Intercept)`[i] <- round(model_outputs_control[which(model_outputs_control$Effect=="(Intercept)" & model_outputs_control$var_i==control_output_small$metric[i]),"Estimate"],digits = 3)
  
  if(model_outputs_control[which(model_outputs_control$Effect=="(Intercept)" & model_outputs_control$var_i==control_output_small$metric[i]),"p-value"] < 0.05){ 
    control_output_small$`(Intercept)`[i] <- paste(control_output_small$`(Intercept)`[i],"*")}
  
  
  #
  control_output_small$`R^2`[i] <- round(model_outputs_control$Rsq[which(model_outputs_control$Effect=="Model" & model_outputs_control$var_i==control_output_small$metric[i])],digits = 3)
  
  #
  #control_output_small$`p-value`   <- round(model_outputs_control$`p-value`[which(model_outputs_control$Effect=="Model" & model_outputs_control$var_i==control_output_small$metric[i])],digits = 3)
  control_output_small$`p-value`   <- model_outputs_control$`p-value`[which(model_outputs_control$Effect=="Model" & model_outputs_control$var_i==control_output_small$metric[i])]
    
}


##################################################################






##################################################################
#remove precip and temp from models below?






###################################################################

#Fit models
library(afex)
model_outputs<-NULL

for(i in 14:ncol(cover.meta)){
  
  var_i<-colnames(cover.meta)[i]
  
  model_i <- eval(parse(text = paste0("lme(",var_i," ~ TTtreat+ TTtreat*year,
                 random=~1|blockID,data=cover.meta)")))
  
  #model_i<- eval(parse(text = paste0("lme(",var_i," ~ annualPrecipitation +summerTemperature+year+TTtreat+year*TTtreat,
  #               random=~1|blockID,data=cover.meta)")))
  
  
  sum_i<-summary(model_i)$tTable
  sum_i<-as.data.frame(sum_i)
  sum_i$parm<-row.names(sum_i)

  r2<-r2glmm::r2beta(model = model_i)#temp 0.05
  merged_out_i<-merge(x = r2,y = sum_i,by.x = "Effect",by.y = "parm",all.x = T,all.y = T)
  merged_out_i<-merged_out_i[c("Effect","Rsq","upper.CL","lower.CL","Value","Std.Error","DF","t-value","p-value")]
  #summary(model_i)

  model_outputs<- rbind(model_outputs,cbind(var_i,merged_out_i))




}

#cleanup
rm(merged_out_i,sum_i,r2,i,var_i,model_i)


model_outputs$significant <- NA
model_outputs$significant

model_outputs$significant[which(model_outputs$`p-value`<=0.05)] <- "Yes"
model_outputs$significant[which(model_outputs$`p-value`>0.05)] <- "No"



############


#summarize data in a more convenient table


model_output_small <- matrix(nrow = length(unique(model_outputs$var_i)),ncol = length(setdiff(x = as.character(unique(model_outputs$Effect)),y = "Model"))+3)
model_output_small <- as.data.frame(model_output_small)
colnames(model_output_small) <- c("metric",setdiff(x = as.character(unique(model_outputs$Effect)),y = "Model"),"R^2","p-value")


for(i in 1:length(unique(model_outputs$var_i))){
  model_output_small$metric[i] <- as.character(unique(model_outputs$var_i)[i])
  
  #TTtreatTT1
  model_output_small$TTtreatTT1[i] <- round(model_outputs[which(model_outputs$Effect=="TTtreatTT1" & model_outputs$var_i==model_output_small$metric[i]),"Value"],digits = 3)

  if(model_outputs[which(model_outputs$Effect=="TTtreatTT1" & model_outputs$var_i==model_output_small$metric[i]),"p-value"] < 0.05){ 
    model_output_small$TTtreatTT1[i] <- paste(model_output_small$TTtreatTT1[i],"*")}

  
  #TTtreatTT2
  model_output_small$TTtreatTT2[i] <- round(model_outputs[which(model_outputs$Effect=="TTtreatTT2" & model_outputs$var_i==model_output_small$metric[i]),"Value"],digits = 3)
  
  if(model_outputs[which(model_outputs$Effect=="TTtreatTT2" & model_outputs$var_i==model_output_small$metric[i]),"p-value"] < 0.05){ 
    model_output_small$TTtreatTT2[i] <- paste(model_output_small$TTtreatTT2[i],"*")}
  
  
  #TTtreatTT3
  model_output_small$TTtreatTT3[i] <- round(model_outputs[which(model_outputs$Effect=="TTtreatTT3" & model_outputs$var_i==model_output_small$metric[i]),"Value"],digits = 3)
  
  if(model_outputs[which(model_outputs$Effect=="TTtreatTT3" & model_outputs$var_i==model_output_small$metric[i]),"p-value"] < 0.05){ 
    model_output_small$TTtreatTT3[i] <- paste(model_output_small$TTtreatTT3[i],"*")}
  
  
  #TTtreatTT4
  model_output_small$TTtreatTT4[i] <- round(model_outputs[which(model_outputs$Effect=="TTtreatTT4" & model_outputs$var_i==model_output_small$metric[i]),"Value"],digits = 3)
  
  if(model_outputs[which(model_outputs$Effect=="TTtreatTT4" & model_outputs$var_i==model_output_small$metric[i]),"p-value"] < 0.05){ 
    model_output_small$TTtreatTT4[i] <- paste(model_output_small$TTtreatTT3[i],"*")}
  
  #TTtreatTT1:year
  model_output_small$`TTtreatTT1:year`[i] <- round(model_outputs[which(model_outputs$Effect=="TTtreatTT1:year" & model_outputs$var_i==model_output_small$metric[i]),"Value"],digits = 3)
  
  if(model_outputs[which(model_outputs$Effect=="TTtreatTT1:year" & model_outputs$var_i==model_output_small$metric[i]),"p-value"] < 0.05){ 
    model_output_small$`TTtreatTT1:year`[i] <- paste(model_output_small$`TTtreatTT1:year`[i],"*")}
  
  
  #TTtreatTT2:year
  model_output_small$`TTtreatTT2:year`[i] <- round(model_outputs[which(model_outputs$Effect=="TTtreatTT2:year" & model_outputs$var_i==model_output_small$metric[i]),"Value"],digits = 3)
  
  if(model_outputs[which(model_outputs$Effect=="TTtreatTT2:year" & model_outputs$var_i==model_output_small$metric[i]),"p-value"] < 0.05){ 
    model_output_small$`TTtreatTT2:year`[i] <- paste(model_output_small$`TTtreatTT2:year`[i],"*")}
  
  
  #TTtreatTT3:year
  model_output_small$`TTtreatTT3:year`[i] <- round(model_outputs[which(model_outputs$Effect=="TTtreatTT3:year" & model_outputs$var_i==model_output_small$metric[i]),"Value"],digits = 3)
  
  if(model_outputs[which(model_outputs$Effect=="TTtreatTT3:year" & model_outputs$var_i==model_output_small$metric[i]),"p-value"] < 0.05){ 
    model_output_small$`TTtreatTT3:year`[i] <- paste(model_output_small$`TTtreatTT3:year`[i],"*")}
  
  
  #TTtreatTT4:year
  model_output_small$`TTtreatTT4:year`[i] <- round(model_outputs[which(model_outputs$Effect=="TTtreatTT4:year" & model_outputs$var_i==model_output_small$metric[i]),"Value"],digits = 3)
  
  if(model_outputs[which(model_outputs$Effect=="TTtreatTT4:year" & model_outputs$var_i==model_output_small$metric[i]),"p-value"] < 0.05){ 
    model_output_small$`TTtreatTT4:year`[i] <- paste(model_output_small$`TTtreatTT4:year`[i],"*")}
  
  #year
  model_output_small$year[i] <- round(model_outputs[which(model_outputs$Effect=="year" & model_outputs$var_i==model_output_small$metric[i]),"Value"],digits = 3)
  
  if(model_outputs[which(model_outputs$Effect=="year" & model_outputs$var_i==model_output_small$metric[i]),"p-value"] < 0.05){ 
    model_output_small$year[i] <- paste(model_output_small$year[i],"*")}
  
  
  #(Intercept)   
  model_output_small$`(Intercept)`[i] <- round(model_outputs[which(model_outputs$Effect=="(Intercept)" & model_outputs$var_i==model_output_small$metric[i]),"Value"],digits = 3)
  
  if(model_outputs[which(model_outputs$Effect=="(Intercept)" & model_outputs$var_i==model_output_small$metric[i]),"p-value"] < 0.05){ 
    model_output_small$`(Intercept)`[i] <- paste(model_output_small$`(Intercept)`[i],"*")}
  
  

  #
  model_output_small$`R^2`[i] <- round(model_outputs$Rsq[which(model_outputs$Effect=="Model" & model_outputs$var_i==model_output_small$metric[i])],digits = 3)
  
  #
  model_output_small$`p-value`   <- round(model_outputs$`p-value`[which(model_outputs$Effect=="Model" & model_outputs$var_i==model_output_small$metric[i])],digits = 3)
  
  
  
  
  
  
}

#Toss p-vale column since this isn't generated by the model or summary functions
model_output_small<- model_output_small[which(colnames(model_output_small)!="p-value")]



###################################################################
###################################################################

#Plots

# for each interaction term, show the effect (alpha to show p-value)
library(ggplot2)

unique(model_outputs$Effect)


ggplot(data=subset(model_outputs, Effect %in% c('TTtreatTT1:year','TTtreatTT2:year','TTtreatTT3:year','TTtreatTT4:year') ), 
       aes(x=Effect, y=Value,alpha=significant)) +  geom_bar(stat="identity")+facet_wrap('var_i')+ 
        theme(axis.text.x = element_text(angle=270))

#ggplot(data=subset(model_outputs, Effect %in% c('year:TTtreatTT1','year:TTtreatTT2','year:TTtreatTT3','year:TTtreatTT4') ), 
#       aes(x=Effect, y=Value,alpha=significant)) +  geom_bar(stat="identity")+facet_wrap('var_i')+ 
#  theme(axis.text.x = element_text(angle=270))




###################################################################


#plot control metrics (year 0) vs temp, vs precip

cover.meta$phylo_mean_all_abd
min(cover.meta$year)
cover.meta$annualPrecipitation
cover.meta$blockID

cover.meta.skinny <- tidyr::gather(cover.meta,"metric","value",
                            "phylo_mean_all_abd","phylo_mean_near_abd","phylo_var_all_abd","phylo_var_near_abd",
                            "func_mean_all_abd","func_mean_near_abd","func_var_all_abd","func_var_near_abd",
                            "trait_mean_all_abd","trait_mean_near_abd","trait_var_all_abd","trait_var_near_abd" )

cover.meta.skinny$metric_long <- cover.meta.skinny$metric


cover.meta.skinny$metric_long<- qdap::multigsub(pattern = c("phylo_mean_all_abd","phylo_mean_near_abd","phylo_var_all_abd","phylo_var_near_abd",
                            "func_mean_all_abd","func_mean_near_abd","func_var_all_abd","func_var_near_abd",
                            "trait_mean_all_abd","trait_mean_near_abd","trait_var_all_abd","trait_var_near_abd"),
                replacement = c("Mean Phylogenetic (time) Distance","Minimum Phylogenetic (time) Distance","Variance in Phylogenetic (time) Distance","Variance in Minimum Phylogenetic (time) Distance",
                                "Mean Functional Distance","Minimum Functional Distance","Variance in Functional Distance","Variance in Minimum Functional Distance",
                                "Mean Phylogenetic (trait) Distance","Minimum Phylogenetic (trait) Distance","Variance in Phylogenetic (trait) Distance","Variance in Minimum Phylogenetic (trait) Distance"),
                text.var = cover.meta.skinny$metric_long
                )

ggplot(data = subset(cover.meta.skinny, year %in% c(2009)),aes(x=summerTemperature,y=value))+
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap('metric_long',scales = "free",nrow = 4)+
  xlab("Mean Summer Temperature") + ylab("SES")+
  theme( axis.text = element_text( size = 20 ),
         axis.text.x = element_text( size = 20 ),
         axis.title = element_text( size = 20 ),
         legend.position="none",
         # The new stuff
         strip.text = element_text(size = 18))

ggplot(data = subset(cover.meta.skinny, year %in% c(2009)),aes(x=annualPrecipitation,y=value))+
  geom_point()+
  geom_smooth(method=lm)+
  facet_wrap('metric_long',scales="free",nrow=4)+
  xlab("Mean Annual Precipitation") + ylab("SES")+
  theme( axis.text = element_text( size = 20 ),
         axis.text.x = element_text( size = 20 ),
         axis.title = element_text( size = 20 ),
         legend.position="none",
         # The new stuff
         strip.text = element_text(size = 18))


####################################################################

# make relative values

  #metrics relative to destination site (per Guittar 2016)
  #skipping this due to high amount of variation in sites

#cover.meta.relative <- cover.meta


#for( i in 1:nrow(unique(cover.meta[c("destSiteID","year")]))){

#  site_i  <- unique(cover.meta[c("destSiteID","year")])[i,1]
#  year_i  <- unique(cover.meta[c("destSiteID","year")])[i,2]

  

#  control_data_i <- cover.meta[which(cover.meta$destSiteID==site_i & cover.meta$year==year_i & cover.meta$TTtreat %in% c("TTC","TT1")),]
  
  
  
  
  
#}


  #metrics relative to time zero? ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cover.meta.delta <- cover.meta


  for( i in 1:length(unique(cover.meta$turfID)) ){
  
    turf_i <- unique(cover.meta$turfID)[i]
    
    data_0 <- cover.meta[which(cover.meta$turfID==turf_i & cover.meta$year==min(cover.meta$year)),]
  
        for(j in 14:ncol(data_0) ){
        
          cover.meta.delta[which(cover.meta$turfID==turf_i),j] <- (cover.meta.delta[which(cover.meta$turfID==turf_i),j]) -( data_0[1,j] )
  
        }
  
  }
colnames(cover.meta.delta)
delta_data<-cover.meta.delta


cover.meta.delta  <- tidyr::gather(data = cover.meta.delta, "metric","value",
                            "phylo_mean_all_abd","phylo_mean_near_abd","phylo_var_all_abd","phylo_var_near_abd",
                            "func_mean_all_abd","func_mean_near_abd","func_var_all_abd","func_var_near_abd",
                            "trait_mean_all_abd","trait_mean_near_abd","trait_var_all_abd","trait_var_near_abd"  )

cover.meta.delta$metric_long<- qdap::multigsub(pattern = c("phylo_mean_all_abd","phylo_mean_near_abd","phylo_var_all_abd","phylo_var_near_abd",
                                                            "func_mean_all_abd","func_mean_near_abd","func_var_all_abd","func_var_near_abd",
                                                            "trait_mean_all_abd","trait_mean_near_abd","trait_var_all_abd","trait_var_near_abd"),
                                                replacement = c("Mean Phylogenetic (time) Distance","Minimum Phylogenetic (time) Distance","Variance in Phylogenetic (time) Distance","Variance in Minimum Phylogenetic (time) Distance",
                                                                "Mean Functional Distance","Minimum Functional Distance","Variance in Functional Distance","Variance in Minimum Functional Distance",
                                                                "Mean Phylogenetic (trait) Distance","Minimum Phylogenetic (trait) Distance","Variance in Phylogenetic (trait) Distance","Variance in Minimum Phylogenetic (trait) Distance"),
                                                text.var = cover.meta.delta$metric
)

cover.meta.delta$Treatment <- qdap::multigsub(pattern = c("TTC","TT1", "TT2", "TT3" ,"TT4"),
                                               replacement = c("Control","Procedural Control","Warmer","Wetter","Warmer and Wetter"),
                                               text.var = cover.meta.delta$TTtreat)


ggplot(data=subset(cover.meta.delta,TTtreat %in% c( "TT2","TT3","TT4")), 
       aes(x=year, y=value , color= Treatment, group_by(TurfID))) + geom_point() +geom_smooth(method =  "lm")+
  facet_wrap('metric_long',scales = "free")+
  xlab("Year") + ylab("Change in SES")+
  theme( axis.text = element_text( size = 20 ),
         axis.text.x = element_text( size = 20 ),
         axis.title = element_text( size = 20 ),
         strip.text = element_text(size = 18))+
  scale_x_continuous(breaks = c(2009,2013,2017))


ggplot(data=subset(cover.meta.delta,TTtreat %in% c( "TT2","TT3","TT4")), 
       aes(x=as.integer(year), y=value , color= TTtreat, group_by(TurfID))) + geom_point() +geom_smooth(method =  "lm")+
  facet_wrap('metric_long')+ylim(c(-10,10)) 



length(unique(cover.meta.delta$metric))

colnames(cover.meta.delta)


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




#correlations in response variables
pairs(cover.meta[14:25])

metric_correlations <- Hmisc::rcorr(as.matrix(cover.meta[14:25]),type = "pearson")



colnames(metric_correlations$r)<- qdap::multigsub(pattern = c("phylo_mean_all_abd","phylo_mean_near_abd","phylo_var_all_abd","phylo_var_near_abd",
                                                           "func_mean_all_abd","func_mean_near_abd","func_var_all_abd","func_var_near_abd",
                                                           "trait_mean_all_abd","trait_mean_near_abd","trait_var_all_abd","trait_var_near_abd"),
                                               replacement = c("Mean Phylogenetic (time) Distance","Minimum Phylogenetic (time) Distance","Variance in Phylogenetic (time) Distance","Variance in Minimum Phylogenetic (time) Distance",
                                                               "Mean Functional Distance","Minimum Functional Distance","Variance in Functional Distance","Variance in Minimum Functional Distance",
                                                               "Mean Phylogenetic (trait) Distance","Minimum Phylogenetic (trait) Distance","Variance in Phylogenetic (trait) Distance","Variance in Minimum Phylogenetic (trait) Distance"),
                                               text.var = colnames(metric_correlations$r) )

colnames(metric_correlations$P)<- qdap::multigsub(pattern = c("phylo_mean_all_abd","phylo_mean_near_abd","phylo_var_all_abd","phylo_var_near_abd",
                                                              "func_mean_all_abd","func_mean_near_abd","func_var_all_abd","func_var_near_abd",
                                                              "trait_mean_all_abd","trait_mean_near_abd","trait_var_all_abd","trait_var_near_abd"),
                                                  replacement = c("Mean Phylogenetic (time) Distance","Minimum Phylogenetic (time) Distance","Variance in Phylogenetic (time) Distance","Variance in Minimum Phylogenetic (time) Distance",
                                                                  "Mean Functional Distance","Minimum Functional Distance","Variance in Functional Distance","Variance in Minimum Functional Distance",
                                                                  "Mean Phylogenetic (trait) Distance","Minimum Phylogenetic (trait) Distance","Variance in Phylogenetic (trait) Distance","Variance in Minimum Phylogenetic (trait) Distance"),
                                                  text.var = colnames(metric_correlations$P) )




rownames(metric_correlations$r)<-colnames(metric_correlations$r)
rownames(metric_correlations$P)<-colnames(metric_correlations$P)
metric_correlations$r <- round(x = metric_correlations$r,digits = 2)
metric_correlations$P <- round(x = metric_correlations$P,digits = 2)
metric_correlations$r[which(metric_correlations$P<0.05)]<- paste(metric_correlations$r[which(metric_correlations$P<0.05)],"*",sep = "")
metric_correlations$r[lower.tri(x = metric_correlations$r,diag = T)]<-NA


?Hmisc::rcorr


library(reshape2)
metric_r <- reshape2::melt(metric_correlations$r,na.rm=T)
metric_p <- reshape2::melt(metric_correlations$P)
metrics_corr_tidy <- merge(x = metric_r,y = metric_p,by.x =  c("Var1" ,"Var2"  ),by.y = c("Var1" ,"Var2"  ))
unique(metrics_corr_tidy)
write.csv(metrics_corr_tidy,file = "phylogeny/plots_figures/metric_correlations_skinny.csv")




library(GGally)



mean_plot<- ggpairs(data = cover.meta,
        columns = grep(x = colnames(cover.meta),pattern = "mean_all"),
        upper = list(continuous = wrap(ggally_cor, displayGrid = FALSE)),title = "Mean Distance",
        columnLabels = c("Phylogenetic(time)","Functional","Phylogenetic(trait)") )
        
        
near_plot <- ggpairs(data = cover.meta,
        columns = grep(x = colnames(cover.meta),pattern = "mean_near"),
        upper = list(continuous = wrap(ggally_cor, displayGrid = FALSE)),
        title = "Mean Minimum Distance",
        columnLabels = c("Phylogenetic(time)","Functional","Phylogenetic(trait)") )

var_plot <- ggpairs(data = cover.meta,
        columns = grep(x = colnames(cover.meta),pattern = "var_all"),
        upper = list(continuous = wrap(ggally_cor, displayGrid = FALSE)), title= "Variance in Distances",
        columnLabels = c("Phylogenetic(time)","Functional","Phylogenetic(trait)") )

var_near_plot <- ggpairs(data = cover.meta,
        columns = grep(x = colnames(cover.meta),pattern = "var_near"),
        upper = list(continuous = wrap(ggally_cor, displayGrid = FALSE)), 
        title= "Variance in Minimum Distances",
        columnLabels = c("Phylogenetic(time)","Functional","Phylogenetic(trait)") )



cowplot::plot_grid(
  ggmatrix_gtable(mean_plot),
  ggmatrix_gtable(near_plot),
  ggmatrix_gtable(var_plot),
  ggmatrix_gtable(var_near_plot),
  nrow = 2,ncol = 2
)



###############################################

#Use delta values

delta_data<-delta_data[which(delta_data$year=="2017"),]
delta_data <- na.omit(delta_data)


#Fit models
library(afex)
model_outputs_delta<-NULL

for(i in 14:ncol(cover.meta)){
  
  var_i<-colnames(cover.meta)[i]
  
  #lme()
  
  #lme(phylo_mean_all_abd ~ TTtreat, random = ~1|blockID, data = delta_data)
  
  
  model_i<- eval(parse(text = paste0("lme(",var_i," ~ TTtreat,
                 random=~1|blockID,data=delta_data)")))
  
  #model_i<- eval(parse(text = paste0("lme(",var_i," ~ annualPrecipitation +summerTemperature+year+TTtreat+year*TTtreat,
  #               random=~1|blockID,data=cover.meta)")))
  
  
  sum_i<-summary(model_i)$tTable
  sum_i<-as.data.frame(sum_i)
  sum_i$parm<-row.names(sum_i)
  
  r2<-r2glmm::r2beta(model = model_i)#temp 0.05
  merged_out_i<-merge(x = r2,y = sum_i,by.x = "Effect",by.y = "parm",all.x = T,all.y = T)
  merged_out_i<-merged_out_i[c("Effect","Rsq","upper.CL","lower.CL","Value","Std.Error","DF","t-value","p-value")]
  #summary(model_i)
  
  model_outputs_delta<- rbind(model_outputs_delta,cbind(var_i,merged_out_i))
  
  
  
  
}

#cleanup
rm(merged_out_i,sum_i,r2,i,var_i,model_i)


model_outputs_delta$significant <- NA
model_outputs_delta$significant

model_outputs_delta$significant[which(model_outputs_delta$`p-value`<=0.05)] <- "Yes"
model_outputs_delta$significant[which(model_outputs_delta$`p-value`>0.05)] <- "No"



#The only thing significant with this approach is (sometimes) the intercept







#################################################
#Statistical tests

model_i<- eval(parse(text = paste0("lm(",var_i," ~ annualPrecipitation +summerTemperature + annualPrecipitation:summerTemperature, data=cover.meta_null)")))


