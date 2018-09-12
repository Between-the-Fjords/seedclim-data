#Code to calculate phylogenetic structure measurements over time at various plots


#Load in needed stuff
source("start_here.R")
#get taxonomy table
dbDisconnect(con)
rm(con, alltaxa,noNIDseedlings,propertaxa)
library(ape)
library(PhyloMeasures)
library(picante)
source("phylogeny/r_functions/comm_phy_fxs.R")
#library(devtools)
#install_github("NGSwenson/lefse_0.5")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Prepare community data as needed for phylo stuff

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Calculating various phylo metrics

cover.meta$pd<-pd.query(tree = tree_1,matrix = cover_binary)
cover.meta$pd_std<-pd.query(tree = tree_1,matrix = cover_binary,standardize = T)
cover.meta$mntd<-mntd.query(tree = tree_1,matrix = cover_binary)
cover.meta$mntd_std<-mntd.query(tree = tree_1,matrix = cover_binary,standardize = T)
cover.meta$mpd<-mpd.query(tree = tree_1,matrix = cover_binary)
cover.meta$mpd_std<-mpd.query(tree = tree_1,matrix = cover_binary,standardize = T)

mpd_out<-replicated_mpd(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/")
cover.meta$mpd_abd<-rowMeans(mpd_out)

mntd_out<-replicated_mntd(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/")
cover.meta$mntd_abd<-rowMeans(mntd_out)

pd_out<-replicated_pd_abd(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/")
cover.meta$pd_abd<-rowMeans(pd_out)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Write needed outputs

write_rds(x = cover.meta,path = "phylogeny/cover_phylo.rds")
