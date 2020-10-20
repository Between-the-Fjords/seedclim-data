#Code to calculate phylogenetic structure measurements over time at various plots
#v2- simplified,renamed

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

#using new naming scheme: type_moment_dist_abd
##type {func,phylo,trait,rate}
##moment {rich,mean,var,skew,kurt}
##dist {all,near}
##abund {abd,nabd}

#Calculating various phylo metrics: non-abundance weighted~~~~~~~~~~~~~~~~~~~~~~~~~~

#richness metrics
pd_out_std<-replicated_pd_std(comm_matrix = cover_binary,phylogeny_directory = "phylogeny/phylogenies/",n_reps = 1000)
cover.meta$phylo_rich_all_nabd<-rowMeans(pd_out_std)

#divergence metrics
mpd_out_std<-replicated_mpd_std(comm_matrix = cover_binary,phylogeny_directory = "phylogeny/phylogenies/",n_reps = 1000)
cover.meta$phylo_mean_all_nabd<-rowMeans(mpd_out_std)

mntd_out_std<-replicated_mntd_std(comm_matrix = cover_binary,phylogeny_directory = "phylogeny/phylogenies/",n_reps = 1000)
cover.meta$phylo_mean_near_nabd<-rowMeans(mntd_out_std)


#regularity metrics

#vpd
vpd_out_std <-replicated_vpd_std(comm_matrix = cover_binary,phylogeny_directory = "phylogeny/phylogenies/",n_reps_phylo = 100,nreps_null = 100)
cover.meta$phylo_var_all_nabd<-rowMeans(vpd_out_std)

#vntd
vntd_out_std<-replicated_vntd_std(comm_matrix = cover_binary,phylogeny_directory = "phylogeny/phylogenies/",n_reps_phylo = 100,nreps_null = 100)
cover.meta$phylo_var_near_nabd <- rowMeans(vntd_out_std)


#Calculating various phylo metrics: abundance weighted~~~~~~~~~~~~~~~~~~~~~~~~~~

#richness
pd_abd_std <- replicated_pd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/",n_reps_phylo = 100,nreps_null = 100)
cover.meta$phylo_rich_all_abd<-rowMeans(pd_abd_std)

#divergence
mpd_abd_std<-replicated_mpd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/",n_reps_phylo = 100,nreps_null = 100)
cover.meta$phylo_mean_all_abd<-rowMeans(mpd_abd_std)

mntd_abd_std<-replicated_mntd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/",n_reps_phylo = 100,nreps_null = 100)
cover.meta$phylo_mean_near_abd<-rowMeans(mntd_abd_std)


#variance
vpd_abd_std<-replicated_vpd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/",n_reps_phylo = 100,nreps_null = 100)
cover.meta$phylo_var_all_abd<-rowMeans(vpd_abd_std)

vntd_abd_std<-replicated_vntd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/phylogenies/",n_reps_phylo = 100,nreps_null = 100)
cover.meta$phylo_var_near_abd<-rowMeans(vntd_abd_std)

saveRDS(object = cover.meta,file = "phylogeny/cover_phylo.rds")
