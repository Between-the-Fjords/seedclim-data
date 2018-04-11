
# Loading in the trait data for 

# 2. John's
# 3. Marta's, and
# 4. LEDA's

# datasets, and performing some cleaning and merging.

#########################################################################

# code from John
#A function that finds and replaces a string (using regex) in a dataframe

replace_all <- function(df, pattern, replacement) {
  char <- vapply(df, function(x) is.factor(x) || is.character(x), logical(1))
  df[char] <- lapply(df[char], str_replace_all, pattern, replacement)  
  df
}

#A function that takes a dataframe, and a list of problems and resolutions, and runs them through replace_all() 
probfixes=function(df, old, new){
  for(i in 1:length(old)){
    df=replace_all(df,old[i],new[i])
  }
  return(df)
}

#a function if we want to look only (up to) the top 5 maximum values (e.g. for max potential height)
top5=function(df){
  list=unique(df$Species)
  df=df[order(df$Species, -df$vals),]
  df2=df[FALSE,]
  for (i in 1:length(list)){
    df3=df[df$Species==list[i],][1:5,]
    df2=rbind(df2,df3[!is.na(df3$vals),])
  }
  return(df2)
}

#A function that returns the number of replicates, means, and SE for (x=factors, y=data)
means<-function(x, y){
  
  #calculate mean, SD, number of replicates, SE.
  xx<-tapply(y,x,mean)
  yy<-tapply(y,x,sd)
  zz<-tapply(y,x,length)
  er<-yy/sqrt(zz)
  return(data.frame(Species=rownames(xx), N=zz, mean=xx, SE=er, row.names=NULL))
  
}

#a function to clean up raw trait data and calculate means

cleanup=function(df){
  
  #remove empty entries
  df = df[!is.na(df$Species),][df$vals!=0,]
  
  #correct naming inconsistencies
  df=probfixes(df,as.character(probs$old),as.character(probs$new))
  
  #if we want to use only the top 5 values...
  #if(top5==TRUE){
  # df=top5(df)  
  #}
  
  #calculate species means
  #df.means = means(df$binomial, df$vals)
}

##SPECIES FIXES

#list of species names that haven't aligned well with the SEEDclim List, and their resolutions
probs = as.data.frame(matrix(c(
  "Anemona nemorosa","Anemone nemorosa",
  "Deschampsia flexuosa","Avenella flexuosa",
  "Empetrum nigrum","Empetrum hermaphroditum",
  "Euphrasia frigida","Euphrasia sp",
  "Euphrasia stricta","Euphrasia sp",
  "Noccia sp","Noccaea caerulescens",
  "Polygonum viviparum","Bistorta vivipara",
  "Pyrola norvegica","Pyrola rotundifolia",
  "Sagina saginoides","Sagina sp",
  "Taraxacum","Taraxacum sp",
  "Carex ovina","Carex leporina",
  "Hypochoeris radicata","Hypochaeris radicata",
  "Thlaspi caerulescens","Noccaea caerulescens",
  "Saxifraga media", "Saxifraga aizoides"#,
  #"Anthoxanthum odoratum","Anthoxanthum alpinum" #Dunno if i should change this one -- LEDA leaf size only exists for A. alpinum
), byrow=TRUE, ncol=2, dimnames=list(c(),c("old","new"))))

# 2. JOHN

#SLA
john.sla = read.csv("Plant traits/RawTraitData_SLA.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE)

john.sla <- john.sla %>%
  select(Site, Species, vals = SLA..m2.kg.1.) %>%
  probfixes(as.character(probs$old),as.character(probs$new)) %>%
  separate(Species, into = c("genus", "species")) %>%
  mutate(species = ifelse(is.na(species), "sp", species)) %>%
  mutate(genus = substr(genus, 1,3), species = substr(species, 1,3)) %>%
  unite(species, genus, species, sep = ".") %>%
  as.data.frame()

john.sla <- john.sla %>%
  #mutate(Site_sp = paste0(Site,"_", species)) %>%
  group_by(species) %>%
  mutate(john.SLA_mean_global = mean(vals, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Site, species) %>%
  mutate(john.SLA_mean_local = mean(vals, na.rm = TRUE)*10) %>%
  ungroup() %>%
  distinct(Site, species, .keep_all = TRUE) %>%
  select( - vals, siteID = Site)

#check to see how the global vs local traits compare
#john.sla %>% gather(SLAgather, scale, john.SLA_mean_global:john.SLA_mean_local) %>% ggplot(aes(x = siteID, y = scale, colour = SLAgather)) + geom_boxplot() + theme_bw() + ggsave("john_sla.jpg")


#adding to other traits
traitdata <- traitdata %>%
  inner_join(john.sla, by = c("species", "siteID"))


#HEIGHT
john.height = read.csv("Plant traits/RawTraitData_height.csv", header=TRUE, sep = ";", stringsAsFactors = FALSE)

john.height <- john.height %>%
  select(Site = SITE, Species = SPECIES, vals = HEIGHT) %>%
  probfixes(as.character(probs$old),as.character(probs$new)) %>%
  separate(Species, into = c("genus", "species")) %>%
  mutate(species = ifelse(is.na(species), "sp", species)) %>%
  mutate(genus = substr(genus, 1,3), species = substr(species, 1,3)) %>%
  unite(species, genus, species, sep = ".") %>%
  as.data.frame()

john.height <- john.height %>%
  #mutate(Site_sp = paste0(Site,"_", species)) %>%
  group_by(species) %>%
  mutate(john.height_mean_global = mean(vals, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(Site, species) %>%
  mutate(john.height_mean_local = mean(vals, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(Site, species, .keep_all = TRUE) %>%
  select(- vals, siteID = Site)

#check to see how the global vs local traits compare
john.height %>% gather(heightGather, scale, john.height_mean_global:john.height_mean_local) %>% ggplot(aes(x = siteID, y = scale, colour = heightGather)) +geom_boxplot() + theme_bw() #+ ggsave("john_height.jpg")

traitdata %>% gather(heightGather, scale, c(Height_mean_global, Height_mean)) %>% ggplot(aes(x = siteID, y = scale, colour = heightGather)) +geom_boxplot() + theme_bw() #+ ggsave("ragnhild_height.jpg")

#adding to other traits
traitdata <- traitdata %>%
  left_join(john.height, by = c("species", "siteID"))


# 4. LEDA

#SLA
leda.sla = read.csv("Plant traits/LEDA_SLA_RawData.csv", header=TRUE, sep = ",", stringsAsFactors = FALSE)

leda.sla <- leda.sla %>%
  filter(plant.stage == "adult") %>%
  select(species = SBS.name, vals = mean.SLA..mm.2.mg.) %>%
  probfixes(as.character(probs$old), as.character(probs$new)) %>%
  group_by(species) %>%
  mutate(leda.SLA_global = mean(vals)) %>%
  ungroup() %>%
  distinct(species, .keep_all = TRUE) %>%
  separate(species, into = c("genus", "species")) %>%
  mutate(species = ifelse(is.na(species), "sp", species)) %>%
  mutate(genus = substr(genus, 1,3), species = substr(species, 1,3)) %>%
  unite(species, genus, species, sep = ".") %>%
  as.data.frame()


traitdata <- traitdata %>%
  inner_join(leda.sla, by = "species")


# comparing ragnhild's and john's trait variability

#HEIGHT
traitdata %>% 
  gather(recorder, scale, c(john.height_mean_local, Height_mean)) %>%
  ggplot(aes(x = siteID, y = scale, colour = recorder)) + geom_boxplot()


#SLA
traitdata %>% 
  mutate(SLA_mean_global = SLA_mean_global/10) %>%
  gather(recorder, scale, c(john.SLA_mean_global, SLA_mean_global, leda.SLA_global)) %>%
  ggplot(aes(x = siteID, y = scale, colour = recorder)) + geom_boxplot()


traitdata%>%
  mutate(SLA_mean_global = SLA_mean_global/10) %>%
  ggplot(aes(x = john.SLA_mean_global, y = SLA_mean_global)) +
  geom_point() +
  facet_wrap(~siteID) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed")

groupedTraits <- traitdata %>%
  gather(SLArecorder, SLAglobal, c(john.SLA_mean_global, SLA_mean_global, leda.SLA_global)) %>%
  gather(heightrecorder, heightglobal, c(Height_mean_global, john.height_mean_global))


ggplot(groupedTraits, aes(x = siteID, y = SLAglobal, colour = SLArecorder)) + geom_boxplot() + theme_bw() #+ ggsave("grouped_sla_global.jpg")

ggplot(groupedTraits, aes(x = siteID, y = heightglobal, colour = heightrecorder)) + geom_boxplot() + theme_bw() #+ ggsave("grouped_sla_global.jpg")

groupedTraits %>%
  mutate(species =as.factor(species)) %>%
  mutate(species = reorder(species, scale, na.rm = TRUE)) %>%
  ggplot(aes(x = species, y = scale, colour = recorder)) + geom_point()

mod.grouped.tr <- aov(scale ~ recorder, groupedTraits)
summary(mod.grouped.tr)
TukeyHSD(mod.grouped.tr)


#traitdata %>%
#filter(!is.na(SLA) & !is.na(SLAR)) %>%
ggplot(aes(SLA_mean_global, leda.SLA_global)) +
  geom_point() +
  scale_color_manual(values = cbPalette) +
  #geom_smooth(method = "lm", formula = y ~ log(x), se = FALSE, fullrange = FALSE) +
  geom_abline(intercept = 0, slope = 1)
