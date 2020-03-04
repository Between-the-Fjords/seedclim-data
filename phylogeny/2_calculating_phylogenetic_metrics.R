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
library(lefse)
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
colnames(cover)[which(!colnames(cover)%in%tree_1$tip.label)]#need to drop a few species from the cover set (the 5 non seed-plants)
cover<-cover[,which(colnames(cover)%in%tree_1$tip.label)]
cover_binary<-cover
class(cover_binary)
cover_binary<-as.matrix(cover_binary)
cover_binary[which(is.na(cover_binary))]<-0
cover_binary[which(cover_binary>0)]<-1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Calculating various phylo metrics: non-abundance weighted~~~~~~~~~~~~~~~~~~~~~~~~~~

#richness metrics
pd_out<-replicated_pd(comm_matrix = cover_binary,phylogeny_directory = "phylogeny/phylogenies/",n_reps = 1000)
cover.meta$pd<-rowMeans(pd_out)

pd_out_std<-replicated_pd_std(comm_matrix = cover_binary,phylogeny_directory = "phylogeny/phylogenies/",n_reps = 1000)
cover.meta$pd_std<-rowMeans(pd_out_std)

#divergence metrics
mpd_out<-replicated_mpd(comm_matrix = cover_binary,phylogeny_directory = "phylogeny/phylogenies/",n_reps = 1000)
cover.meta$mpd<-rowMeans(mpd_out)

mpd_out_std<-replicated_mpd_std(comm_matrix = cover_binary,phylogeny_directory = "phylogeny/phylogenies/",n_reps = 1000)
cover.meta$mpd_std<-rowMeans(mpd_out)

mntd_out<-replicated_mntd(comm_matrix = cover_binary,phylogeny_directory = "phylogeny/phylogenies/",n_reps = 1000)
cover.meta$mntd<-rowMeans(mntd_out)

mntd_out_std<-replicated_mntd_std(comm_matrix = cover_binary,phylogeny_directory = "phylogeny/phylogenies/",n_reps = 1000)
cover.meta$mntd_std<-rowMeans(mntd_out)

#regularity metrics

#vpd
vpd_out<-replicated_vpd(comm_matrix = cover_binary,phylogeny_directory = "phylogeny/phylogenies/",n_reps = 1000)
cover.meta$vpd<-rowMeans(vpd_out)

vpd_out_std <-replicated_vpd_std(comm_matrix = cover_binary,phylogeny_directory = "phylogeny/phylogenies/",n_reps = 1000)
cover.meta$vpd_std<-rowMeans(vpd_out_std)


#vntd
vntd_out<-replicated_vntd(comm_matrix = cover_binary,phylogeny_directory = "phylogeny/phylogenies/",n_reps = 1000)
cover.meta$vntd<-rowMeans(vntd_out)

vntd_out_std<-replicated_vntd_std(comm_matrix = cover_binary,phylogeny_directory = "phylogeny/phylogenies/",n_reps = 1000)
cover.meta$vntd_std<-rowMeans(vntd_out_std)

#calculating phylo metrics: abundance weighted~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#richness
pd_out_abd<-replicated_pd_abd(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/")
cover.meta$pd_abd<-rowMeans(pd_out_abd)

#divergence
mpd_out_abd<-replicated_mpd_abd(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/")
cover.meta$mpd_abd<-rowMeans(mpd_out_abd)

mntd_out_abd<-replicated_mntd_abd(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/")
cover.meta$mntd_abd<-rowMeans(mntd_out_abd)

#variance
vpd_out_abd<-replicated_vpd(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/",n_reps = 1000)
cover.meta$vpd_abd<-rowMeans(vpd_out_abd)

#vntd
vntd_out_abd<-replicated_vntd_abd(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/",n_reps = 1000)
cover.meta$vntd_abd<-rowMeans(vntd_out_abd)


#need to add standardized versions of abundance weighted metrics

#richness
pd_abd_std <- replicated_pd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/",n_reps_phylo = 100,nreps_null = 100)
cover.meta$pd_abd_std<-rowMeans(pd_abd_std)

#divergence
mpd_abd_std<-replicated_mpd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/",n_reps_phylo = 100,nreps_null = 100)
cover.meta$mpd_abd_std<-rowMeans(mpd_abd_std)

mntd_abd_std<-replicated_mntd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/",n_reps_phylo = 100,nreps_null = 100)
cover.meta$mntd_abd_std<-rowMeans(mntd_abd_std)


#variance
vpd_abd_std<-replicated_vpd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/",n_reps_phylo = 100,nreps_null = 100)
cover.meta$vpd_abd_std<-rowMeans(vpd_abd_std)

vntd_abd_std<-replicated_vntd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/",n_reps_phylo = 100,nreps_null = 100)
cover.meta$vntd_abd_std<-rowMeans(vntd_abd_std)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Write needed outputs

saveRDS(object = cover.meta,file = "phylogeny/cover_phylo.rds")
