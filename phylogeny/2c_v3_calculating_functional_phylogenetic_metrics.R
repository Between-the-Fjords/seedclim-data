#Code to calculate phylogenetic structure measurements over time at various plots


#Load in needed stuff
source("start_here.R")
#get taxonomy table
dbDisconnect(con)
rm(con, alltaxa,noNIDseedlings,propertaxa)
library(ape)
library(PhyloMeasures)
library(picante)
library(lefse)
source("phylogeny/r_functions/comm_phy_fxs.R")
#library(devtools)
#install_github("NGSwenson/lefse_0.5")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Prepare community data as needed for phylo stuff

tree_names <- read.csv(file = "phylogeny/tree_names_to_code_lookup.csv")#Contains standardized names used in phylogenies and names used in cover.thin

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
cover_binary<-as.matrix(cover_binary)
cover_binary[which(is.na(cover_binary))]<-0
cover_binary[which(cover_binary>0)]<-1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Load in trait data here.  Add code once this has stabilized.

  #For now, just manually run code in 2b
traits<-unitraits
traits<-scale(traits)
traits<-as.data.frame(traits)
traits$species<-rownames(traits)
traits<-traits[c("species","Height","LDMC","SLA")]
rownames(traits)<-1:nrow(traits)




#generate trait-scaled phylogenies
source("phylogeny/r_functions/scale_branches_multidimensional_distance_with_variance.R")
phylos <- list.files("phylogeny/phylogenies/",full.names = T)
directory_trait_branches <- "phylogeny/trait_scaled_phylogenies/"
directory_trait_rate_branches <- "phylogeny/trait_rate_scaled_phylogenies/"

#calculate phy signal while we're at this
source("phylogeny/r_functions/replicated_phy_signal.R")
phy_signal <- replicated_signal(phylogeny_list = phylos,traits = traits)
mean(phy_signal$K[which(phy_signal$`colnames(traits_i[j])`=="Height")])#.19
mean(phy_signal$PIC.variance.P[which(phy_signal$`colnames(traits_i[j])`=="Height")])#<0.05

mean(phy_signal$K[which(phy_signal$`colnames(traits_i[j])`=="LDMC")])#.49
mean(phy_signal$PIC.variance.P[which(phy_signal$`colnames(traits_i[j])`=="LDMC")])#< 0.05

mean(phy_signal$K[which(phy_signal$`colnames(traits_i[j])`=="SLA")])#.23
mean(phy_signal$PIC.variance.P[which(phy_signal$`colnames(traits_i[j])`=="LDMC")])# < 0.05


#remove all files in directories (remove comments to run.  This is to keep me from accidentally deleting everything (again....))

  #file.remove(list.files(directory_trait_branches,full.names = T))
  #file.remove(list.files(directory_trait_rate_branches,full.names = T))

#nreps_trait_branches<-10
#for(i in 1:length(phylos)){
#  
#  phy_i<-read.tree(file = phylos[i])
#  splitname<-unlist(strsplit(phylos[i],split = "/"))
#  splitname<-splitname[length(splitname)]
#  splitname<-unlist(strsplit(x = splitname,split = ".tre"))
#  
#  for(n in 1:nreps_trait_branches){
#  tf_phy_i<-scale_branches_multidimensional_with_variation(tree = phy_i,traits = traits,rate = F)  
#  write.tree(phy = tf_phy_i,file = paste(directory_trait_branches,splitname,"_scaled_rep_",n,".tre",sep = ""))
#  
#  tf_phy_rate_i<-scale_branches_multidimensional_with_variation(tree = phy_i,traits = traits,rate = T) 
#  write.tree(phy = tf_phy_rate_i,file = paste(directory_trait_rate_branches,splitname,"_rate_scaled_rep_",n,".tre",sep = ""))
#  
#  }#reps
#  
#  print(paste(i/length(phylos)*100," percent done",sep = ""))
#  
#}


#using new naming scheme: type_moment_dist_abd
##type {func,phylo,trait,rate} #trait=trait-scaled
##moment {rich,mean,var,skew,kurt}
##dist {all,near}
##abund {abd,nabd}

#non-standardized versions of abundance weighted metrics


#Using trait-scaled branches
#richness
  #trait_pd_abd <- replicated_pd_abd(comm_matrix = cover,phylogeny_directory = "phylogeny/trait_scaled_phylogenies/",n_reps = 100)
  #cover.meta$trait_pd_abd <- rowMeans(trait_pd_abd)

  #save.image("C:/Users/Brian/Desktop/current_projects/seedclimComm/temp.RData")


#divergence
  trait_mpd_abd <- replicated_mpd_abd(comm_matrix = cover,phylogeny_directory = "phylogeny/trait_scaled_phylogenies/",n_reps = 100)
  cover.meta$trait_mpd_abd <- rowMeans(trait_mpd_abd)

  trait_mntd_abd <- replicated_mntd_abd(comm_matrix = cover,phylogeny_directory = "phylogeny/trait_scaled_phylogenies/",n_reps = 100)
  cover.meta$trait_mntd_abd <- rowMeans(trait_mntd_abd)

  #save.image("C:/Users/Brian/Desktop/current_projects/seedclimComm/temp.RData")


#variance
  trait_vpd_abd <- replicated_vpd_abd(comm_matrix = cover,phylogeny_directory = "phylogeny/trait_scaled_phylogenies/",n_reps = 100)
  cover.meta$trait_vpd_abd <- rowMeans(trait_vpd_abd)

  trait_vntd_abd <- replicated_vntd_abd(comm_matrix = cover,phylogeny_directory = "phylogeny/trait_scaled_phylogenies/",n_reps = 100)
  cover.meta$trait_vntd_abd <- rowMeans(trait_vntd_abd)

  #save.image("C:/Users/Brian/Desktop/current_projects/seedclimComm/temp.RData")


#####################################################old code V

#standardized versions of abundance weighted metrics


#Using trait-scaled branches
#richness
  #trait_pd_abd_std <- replicated_pd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/trait_scaled_phylogenies/",n_reps_phylo = 100,nreps_null = 100)
  #cover.meta$trait_pd_abd_std<-rowMeans(trait_pd_abd_std)

save.image("C:/Users/Brian/Desktop/current_projects/seedclimComm/temp.RData")


#divergence
  trait_mpd_abd_std <- replicated_mpd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/trait_scaled_phylogenies/",n_reps_phylo = 100,nreps_null = 100)
  cover.meta$trait_mpd_abd_std <- rowMeans(trait_mpd_abd_std)

  trait_mntd_abd_std <- replicated_mntd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/trait_scaled_phylogenies/",n_reps_phylo = 100,nreps_null = 100)
  cover.meta$trait_mntd_abd_std <- rowMeans(trait_mntd_abd_std)

save.image("C:/Users/Brian/Desktop/current_projects/seedclimComm/temp.RData")


#variance
  trait_vpd_abd_std <- replicated_vpd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/trait_scaled_phylogenies/",n_reps_phylo = 100,nreps_null = 100)
  cover.meta$trait_vpd_abd_std <- rowMeans(trait_vpd_abd_std)

  trait_vntd_abd_std <- replicated_vntd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/trait_scaled_phylogenies/",n_reps_phylo = 100,nreps_null = 100)
  cover.meta$trait_vntd_abd_std <- rowMeans(trait_vntd_abd_std)

save.image("C:/Users/Brian/Desktop/current_projects/seedclimComm/temp.RData")


#Using trait-rate-scaled branches

#richness
#trait_rate_pd_abd_std <- replicated_pd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/trait_rate_scaled_phylogenies/",n_reps_phylo = 100,nreps_null = 100)
#cover.meta$trait_rate_pd_abd_std<-rowMeans(trait_rate_pd_abd_std)

#divergence
#trait_rate_mpd_abd_std<-replicated_mpd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/trait_rate_scaled_phylogenies/",n_reps_phylo = 100,nreps_null = 100)
#cover.meta$trait_rate_mpd_abd_std<-rowMeans(trait_rate_mpd_abd_std)

#trait_rate_mntd_abd_std<-replicated_mntd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/trait_rate_scaled_phylogenies/",n_reps_phylo = 100,nreps_null = 100)
#cover.meta$trait_rate_mntd_abd_std<-rowMeans(trait_rate_mntd_abd_std)


#variance
#trait_rate_vpd_abd_std<-replicated_vpd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/trait_rate_scaled_phylogenies/",n_reps_phylo = 100,nreps_null = 100)
#cover.meta$trait_rate_vpd_abd_std<-rowMeans(trait_rate_vpd_abd_std)

#trait_rate_vntd_abd_std<-replicated_vntd_abd_std(comm_matrix = cover,phylogeny_directory = "phylogeny/trait_rate_scaled_phylogenies/",n_reps_phylo = 100,nreps_null = 100)
#cover.meta$trait_rate_vntd_abd_std<-rowMeans(trait_rate_vntd_abd_std)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Write needed outputs

saveRDS(object = cover.meta,file = "phylogeny/cover_phylo_trait_trait_scaled.rds")
