
#replicated mpd
#inputs: 
  #community data matrix
  #directory containing phylogenies
  #n replicates (default = all)

library(picante)

replicated_mpd<-function(comm_matrix,phylogeny_directory,n_reps=NULL){

phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
if(is.null(n_reps)){n_reps<-length(phylogenies)}  
if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
mpd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)


for(i in 1:n_reps){

  
phy_i<-read.tree(phylogenies[i])  
phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
dist_i<-cophenetic(phy_i)
comm_matrix<-as.matrix(comm_matrix)
comm_matrix[which(is.na(comm_matrix))]<-0
mpd_out[,i]<-mpd(samp = comm_matrix,dis = dist_i,abundance.weighted = T)

print(paste(i/n_reps*100," percent done",sep = ""))  


}#i loop  
return(mpd_out)
  
}

###################

replicated_mntd<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  mntd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    dist_i<-cophenetic(phy_i)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    mntd_out[,i]<-mntd(samp = comm_matrix,dis = dist_i,abundance.weighted = T)
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
  }#i loop  
  return(mntd_out)
  
}

############################

library(lefse)

replicated_pd_abd<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  pd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    pd_out[,i]<-weighted.faith(my.phylo = phy_i,my.sample = comm_matrix)
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
  }#i loop  
  return(pd_out)
  
}
