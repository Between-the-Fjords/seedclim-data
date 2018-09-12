
#replicated mpd
#inputs: 
  #community data matrix
  #directory containing phylogenies
  #n replicates (default = all)

library(picante)

replicated_mpd_abd<-function(comm_matrix,phylogeny_directory,n_reps=NULL){

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

replicated_mntd_abd<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
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

###################


replicated_pd<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  pd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    pd_out[,i]<-PhyloMeasures::pd.query(tree = phy_i,matrix = comm_matrix,standardize = F,abundance.weights = F)
    
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
  }#i loop  
  return(pd_out)
  
}

###################


replicated_pd_std<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  pd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    pd_out[,i]<-PhyloMeasures::pd.query(tree = phy_i,matrix = comm_matrix,standardize = T,abundance.weights = F)
    
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
  }#i loop  
  return(pd_out)
  
}

###################


replicated_mpd<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  mpd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    mpd_out[,i]<-PhyloMeasures::mpd.query(tree = phy_i,matrix = comm_matrix,standardize = F,abundance.weights = F)
    
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
  }#i loop  
  return(mpd_out)
  
}

###################


replicated_mpd_std<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  mpd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    mpd_out[,i]<-PhyloMeasures::mpd.query(tree = phy_i,matrix = comm_matrix,standardize = T,abundance.weights = F)
    
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
  }#i loop  
  return(mpd_out)
  
}

###################


replicated_mntd()<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  mntd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    mntd_out[,i]<-PhyloMeasures::mntd.query(tree = phy_i,matrix = comm_matrix,standardize = F,abundance.weights = F)
    
    
    
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
  }#i loop  
  return(mntd_out)
  
}

###################


replicated_mntd_std()<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  mntd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    mntd_out[,i]<-PhyloMeasures::mntd.query(tree = phy_i,matrix = comm_matrix,standardize = T,abundance.weights = F)
    
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

###############################

replicated_vpd<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  vpd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    dist_i<-cophenetic(phy_i)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    vpd_out[,i]<-vpd(samp = comm_matrix,dis = dist_i,abundance.weighted = F)
    
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
    
  }#i loop  
  return(vpd_out)
  
}

################################################

replicated_vpd_abd<-function(comm_matrix,phylogeny_directory,n_reps=NULL){
  
  phylogenies<-list.files(path = phylogeny_directory,pattern = ".tre",full.names = T)  
  if(is.null(n_reps)){n_reps<-length(phylogenies)}  
  if(n_reps>length(phylogenies)){stop("More replications specified than phylogenies available")}  
  
  vpd_out<-matrix(data = NA,nrow = nrow(comm_matrix),ncol = n_reps)
  
  
  for(i in 1:n_reps){
    
    
    phy_i<-read.tree(phylogenies[i])  
    phy_i<-drop.tip(phy = phy_i,tip = phy_i$tip.label[which(!phy_i$tip.label%in%colnames(comm_matrix))],trim.internal = T,collapse.singles = T)
    dist_i<-cophenetic(phy_i)
    comm_matrix<-as.matrix(comm_matrix)
    comm_matrix[which(is.na(comm_matrix))]<-0
    vpd_out[,i]<-vpd(samp = comm_matrix,dis = dist_i,abundance.weighted = T)
    
    print(paste(i/n_reps*100," percent done",sep = ""))  
    
    
  }#i loop  
  return(vpd_out)
  
}


################



vpd<-function (samp, dis, abundance.weighted = FALSE) 
{
  N <- dim(samp)[1]
  vpd <- numeric(N)
  for (i in 1:N) {
    sppInSample <- names(samp[i, samp[i, ] > 0])
    if (length(sppInSample) > 1) {
      sample.dis <- dis[sppInSample, sppInSample]
      if (abundance.weighted) {
        sample.weights <- t(as.matrix(samp[i, sppInSample, 
                                           drop = FALSE])) %*% as.matrix(samp[i, sppInSample, 
                                                                              drop = FALSE])
        vpd[i] <- weighted.var(sample.dis, sample.weights)
      }
      else {
        vpd[i] <- var(sample.dis[lower.tri(sample.dis)])
      }
    }
    else {
      vpd[i] <- NA
    }
  }
  vpd
}
#######################################

weighted.var <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    w <- w[i <- !is.na(x)]
    x <- x[i]
  }
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w <- sum(x * w) / sum(w)
  (sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2, na.rm =
                                       na.rm)
}


