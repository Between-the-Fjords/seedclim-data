#Code to calculate phylogenetic structure measurements over time at various plots


#Load in needed stuff
library(ape)
library(PhyloMeasures)
source("start_here.R")
#get taxonomy table
dbDisconnect(con)
rm(con, alltaxa,noNIDseedlings,propertaxa)
name_codes<-read.csv("phylogeny/tree_names_to_code_lookup.csv")
tree_names<-read.csv(file = "phylogeny/tree_names_to_code_lookup.csv")#Contains standardized names used in phylogenies and names used in cover.thin


#Append species names to cover.thin, drop species that aren't in phylogeny
cover.thin<-merge(x = cover.thin,y = tree_names,by.x = "species",by.y = "species")

#Convert cover (species x turf matrix) to match names on phylogeny, probably smarter way to do this, but meh....
cover_names<-as.data.frame(colnames(cover))
colnames(cover_names)<-"covernames"
cover_names<-merge(x = cover_names,y = tree_names,by.x = "covernames", by.y = "species",all.x = T)
colnames(cover)<-cover_names$speciesName
cover<-cover[which(!is.na(cover_names$speciesName))]
rm(cover_names)
colnames(cover)<-gsub(pattern = " ",replacement = "_",x = colnames(cover))
tree_1<-read.tree("phylogeny/phylogenies/gbotb_base_rep_1.tre")
colnames(cover)[which(!colnames(cover)%in%tree_1$tip.label)]#need to drop a few species from the cover set
cover<-cover[which(colnames(cover)%in%tree_1$tip.label)]
cover_binary<-cover
class(cover_binary)
cover_binary<-as.matrix(cover_binary)
cover_binary[which(is.na(cover_binary))]<-0
cover_binary[which(cover_binary>0)]<-1


#Need to iterate through cover.meta, and for each row (e.g. turf x year), calculated phylogenetic metrics
#abundance weighted measures?
#Maybe use NRI, NTI as these are most common.
?PhyloMeasures
?pd.query#faiths pd
?mntd.query# MNTD, and standardized measure is 1-NTI
?mpd.query #mean pairwise distance, and standardized measure is NRI



#input: species x site matrix


cover.meta$pd<-pd.query(tree = tree_1,matrix = cover_binary)
cover.meta$pd_std<-pd.query(tree = tree_1,matrix = cover_binary,standardize = T)
cover.meta$mntd<-mntd.query(tree = tree_1,matrix = cover_binary)
cover.meta$mntd_std<-mntd.query(tree = tree_1,matrix = cover_binary,standardize = T)
cover.meta$mpd<-mpd.query(tree = tree_1,matrix = cover_binary)
cover.meta$mpd_std<-mpd.query(tree = tree_1,matrix = cover_binary,standardize = T)


#Quick looks at non-abundance-weighted metrics
summary(lm(formula = cover.meta$mpd_std~cover.meta$TTtreat + cover.meta$precipitation_level +cover.meta$temperature_level+cover.meta$year))
summary(lm(formula = cover.meta$pd_std~cover.meta$TTtreat + cover.meta$precipitation_level +cover.meta$temperature_level+cover.meta$year))
library(lmerTest)
library(lme4)
library(nlme)
mntd.out<- lme(mntd_std~TTtreat + precipitation_level +temperature_level+year +year*TTtreat,
              random=~1|turfID,data=cover.meta)

pd.out<- lme(pd_std~TTtreat + precipitation_level +temperature_level+year +year*TTtreat,
               random=~1|turfID,data=cover.meta)

mpd.out<- lme(mpd_std~TTtreat + precipitation_level +temperature_level+year +year*TTtreat,
             random=~1|turfID,data=cover.meta)


summary(mntd.out)
summary(pd.out)
summary(mpd.out)


#Need to look at abundance-weighted metrics, could be low turnover but high abundance variation masking effects
  #maybe include abundance-weighted PD?

library(picante)
?picante

source("phylogeny/r_functions/comm_phy_fxs.R")

mpd_out<-replicated_mpd(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/")
cover.meta$mpd_abd<-rowMeans(mpd_out)

mntd_out<-replicated_mntd(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/")
cover.meta$mntd_abd<-rowMeans(mntd_out)

mpd.abd.out<- lme(mpd_abd~TTtreat + precipitation_level +temperature_level+year +year*TTtreat,
                   random=~1|turfID,data=cover.meta)



mntd.abd.out<- lme(mntd_abd~TTtreat + precipitation_level +temperature_level+year +year*TTtreat,
               random=~1|turfID,data=cover.meta)
summary(mpd.abd.out)
summary(mntd.abd.out)

summary(lm(formula = cover.meta$mpd_abd~cover.meta$TTtreat + cover.meta$precipitation_level +cover.meta$temperature_level+cover.meta$year))
summary(lm(formula = cover.meta$mntd_abd~cover.meta$TTtreat + cover.meta$precipitation_level +cover.meta$temperature_level+cover.meta$year))


######################
